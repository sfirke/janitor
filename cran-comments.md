# Submission
2020-04-11

## Submission summary
Patch version 2.0.1 to fix failing tests in 2.0.0, problems on some platforms and installations of dependency package "stringi".

### Test environments

#### Windows
* Windows 10 with R-release 3.6.3 (Rhub)
* Windows 10 with R-devel via win-builder, 2020-04-11

#### Linux
* ubuntu 14.04.5 R-release 3.6.2 (Travis CI)
* ubuntu 14.04.5 R-devel R Under development (unstable) (2020-03-13 r77948) (Travis CI)

#### Mac
* Mac OS with R-oldrel and R-release (Travis CI)

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
This does not negatively affect downstream dependencies and in fact should resolve any janitor-related failures.
