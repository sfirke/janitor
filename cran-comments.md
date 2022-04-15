# Submission
2020-12-28

## Submission summary
An accumulation of small enhancements and bug fixes.  No breaking changes.

### Test environments

#### Windows
* Windows 10 with R-release 4.0.3 (local)
* Windows 10 with R Under development (unstable) (2020-12-19 r79650) via win-builder, checked 2020-12-28

#### Linux
* ubuntu 20.04 R-release 4.0.3 (Github CI)
* ubuntu 20.04 R-devel R Under development (unstable) (2020-12-28) (Github CI)

#### Mac
* Mac OS with R-release (Github CI)

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
This does not negatively affect downstream dependencies.

I ran a revdepcheck, it succeeded for 30 packages and I manually investigated the others to verify that errors were the result of time-outs and that the janitor changes do not affect those packages.
