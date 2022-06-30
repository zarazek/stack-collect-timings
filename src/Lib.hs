{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( Stats(..)
  , ModuleStats(..)
  , readStats
  ) where

import Control.Monad.State.Lazy (State, evalState)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
import Optics.State (assign, modifying, use)
import Optics.TH (makeLenses)
import System.Directory.Tree (AnchoredDirTree(..), DirTree(..), FileName, readDirectoryWithL)
import qualified System.FilePath as FP
import System.FilePath ((</>))
import Text.Read (readEither)
import Data.Csv (ToNamedRecord(..), DefaultOrdered(..), namedRecord, (.=), header)

data Stats
  = Stats
  { bytesAllocated   :: !Natural
  , milisecondsSpent :: !Double
  }
  deriving Show

instance Semigroup Stats where
  (Stats lba lms) <> (Stats rba rms) = Stats (lba + rba) (lms + rms)

instance Monoid Stats where
  mempty = Stats 0 0.0

parseStatsFile :: String -> Either (Int, String) Stats
parseStatsFile = fmap mconcat
               . traverse (uncurry parseLine)
               . zip [1..]
               . lines
  where
    parseLine :: Int -> String -> Either (Int, String) Stats
    parseLine lineNum = first (lineNum,)
                      . parseStats
                      . words

    parseStats :: [String] -> Either String Stats
    parseStats strs = do
      (allocPair, timePair) <- toEither "not enough words" $ takeLast2 strs
      allocStr <- toEither "'alloc=' prefix not found" $ expectPrefix "alloc=" allocPair
      alloc <- replaceLeft ("invalid alloc '" <> allocStr <> "'") $ readEither allocStr
      timeStr <- toEither "'time=' prefix not found" $ expectPrefix "time=" timePair
      time <- replaceLeft ("invalid time '" <> timeStr <> "'") $ readEither timeStr
      pure $ Stats alloc time

takeLast2 :: [a] -> Maybe (a, a)
takeLast2 = toTuple . take 2 . reverse
  where
    toTuple [x, y] = Just (y, x)
    toTuple _      = Nothing

expectPrefix :: Eq a => [a] -> [a] -> Maybe [a]
expectPrefix []     ys = Just ys
expectPrefix (_:_)  [] = Nothing
expectPrefix (x:xs) (y:ys)
  | x == y    = expectPrefix xs ys
  | otherwise = Nothing

toEither :: a -> Maybe b -> Either a b
toEither a = maybe (Left a) Right

replaceLeft :: a -> Either b c -> Either a c
replaceLeft a = first (const a)

readStatsFile :: FilePath -> IO Stats
readStatsFile path =  parseStatsFile <$> readFile path >>= \case
  Left (lineNum, errMsg) -> fail $ "Error parsing " <> path <> ":" <> show lineNum <> ":" <> errMsg
  Right stats -> pure stats

data ModuleStats
  = ModuleStats
  { packageDir :: FilePath
  , moduleName :: FilePath
  , stats      :: Stats
  }
  deriving Show

instance ToNamedRecord ModuleStats where
  toNamedRecord ModuleStats{ packageDir
                           , moduleName
                           , stats = Stats
                             { bytesAllocated
                             , milisecondsSpent
                             }
                           } =
    namedRecord
    [ "package" .= packageDir
    , "file" .= moduleName
    , "time" .= milisecondsSpent
    , "memory" .= bytesAllocated
    ]

instance DefaultOrdered ModuleStats where
  headerOrder _ = header ["package", "file", "time", "memory"]

data TraverserState
  = TraverserState
  { _currentPath    :: [FileName]
  , _currentPackage :: Maybe [FileName]
  , _currentModule  :: Maybe [FileName]
  }

makeLenses ''TraverserState


type TreeTraverser a = State TraverserState a

data DirectoryType
  = PackageRoot
  | ModuleRoot
  | WithinModule
  | Other

foldTree :: AnchoredDirTree (Maybe Stats) -> [ModuleStats]
foldTree (:/){ dirTree } = evalState (fmap concat $ traverse go $ dropTopLevel dirTree) initState
  where
    dropTopLevel :: DirTree a -> [DirTree a]
    dropTopLevel = \case
      Failed{ name, err } -> signalError name err
      File{}              -> []
      Dir{ contents }     -> contents
    
    initState :: TraverserState
    initState = TraverserState
      { _currentPath = []
      , _currentPackage = Nothing
      , _currentModule  = Nothing
      }

    go :: DirTree (Maybe Stats) -> TreeTraverser [ModuleStats]
    go = \case
      Failed{ name, err } -> signalError name err
      File{ name, file } -> processFile name file
      Dir{ name, contents } -> do
        dirType <- determineDirectoryType name
        enterDirectory name dirType
        result <- concat <$> traverse go contents
        leaveDirectory dirType
        pure result

    signalError :: FileName -> a -> b
    signalError = undefined

    processFile :: FileName -> Maybe Stats -> TreeTraverser [ModuleStats]
    processFile name = \case
      Just stats -> do
        maybePackage <- use currentPackage
        let packageDir = renderPath
                       $ fromMaybe (error "stats file not within package") maybePackage
        maybeModule <- use currentModule
        let modulePath = renderPath
                       $ fromMaybe (error "stats file not within module") maybeModule
            moduleFileName = flip FP.addExtension "hs" $ FP.dropExtensions name
            moduleName = modulePath </> moduleFileName
            moduleStats = ModuleStats{ packageDir, moduleName, stats }
        pure [moduleStats]
      Nothing -> pure []

    determineDirectoryType :: FileName -> TreeTraverser DirectoryType
    determineDirectoryType name = do
      maybePackage <- use currentPackage
      maybeModule <- use currentModule
      pure $ case (maybePackage, maybeModule, name) of
        (Nothing, Nothing, ".stack-work") -> PackageRoot
        (Nothing, Nothing, _)             -> Other
        (Just _, Nothing, "build")        -> ModuleRoot
        (Just _, Nothing, _)              -> Other
        (Just _, Just _, _)               -> WithinModule
        (Nothing, Just _, _)              -> error "module without package?"

    enterDirectory :: FileName -> DirectoryType -> TreeTraverser ()
    enterDirectory name = \case
      PackageRoot  -> setCurrentPackage *> pushDir name
      ModuleRoot   -> setModuleRoot *> pushDir name
      WithinModule -> pushModuleDir name *> pushDir name
      Other        -> pushDir name

    leaveDirectory :: DirectoryType -> TreeTraverser ()
    leaveDirectory = \case
      PackageRoot  -> popDir *> unsetCurrentPackage
      ModuleRoot   -> popDir *> unsetModuleRoot
      WithinModule -> popDir *> popModuleDir
      Other        -> popDir

    pushDir :: FileName -> TreeTraverser ()
    pushDir name = modifying currentPath (pushPath name) 

    popDir :: TreeTraverser ()
    popDir = modifying currentPath popPath

    setCurrentPackage :: TreeTraverser ()
    setCurrentPackage = do
      path <- use currentPath
      assign currentPackage (Just path)

    unsetCurrentPackage :: TreeTraverser ()
    unsetCurrentPackage = assign currentPackage Nothing

    setModuleRoot :: TreeTraverser ()
    setModuleRoot = assign currentModule (Just [])

    pushModuleDir :: FileName -> TreeTraverser ()
    pushModuleDir name = modifying currentModule (fmap (pushPath name))

    popModuleDir :: TreeTraverser ()
    popModuleDir = modifying currentModule (fmap popPath)

    unsetModuleRoot :: TreeTraverser ()
    unsetModuleRoot = assign currentModule Nothing

    pushPath :: FileName -> [FileName] -> [FileName]
    pushPath = (:)

    popPath :: [FileName] -> [FileName]
    popPath = tail

    renderPath :: [FileName] -> FilePath
    renderPath = intercalate "/" . reverse

readStats :: FilePath -> IO [ModuleStats] 
readStats = fmap foldTree . readDirectoryWithL processFile
  where
    processFile path
      | FP.takeExtension path == ".dump-timings" = Just <$> readStatsFile path
      | otherwise                                = pure Nothing
