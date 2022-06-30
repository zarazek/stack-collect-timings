# stack-collect-timings

This is a tool for collecting compilation time staticstic for stack (Haskell) based projects

## Building

```
$ stack install
```

## Usage

1. Build cleanly your project with `stack` with appropriate parameters

  ```
  $ stack clean
  $ stack build --ghc-options="-ddump-timings -ddump-to-file"
  ```

  If you want to include test code in your report, add `--test --no-run-tests` options to your `stack`
  invocation. If you want to include benchmarking code in your report add `--bench --no-run-benchmarks`.

2. Run the tool

  ```
  $ stack-collect-timings <path-to-project-directory> <name-of-output-file>
  ```

## Output format

Output is in CSV format, so it can be easily imported to Excell or in other way grinded further.
Columns are:
- package - directory where .cabal file resides
- file - path to .hs file within the package
- time - compilation time of this file in milliseconds
- memory - bytes allocated during the compilation of this file

