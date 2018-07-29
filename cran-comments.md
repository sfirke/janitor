# Submission
2018-07-29

## Submission summary
Fixes a bug introduced 11 days ago in the v1.1.0 release of janitor, whereby a function began inappropriately failing if provided NA values.

### Test environments

#### Windows
* win-builder with R Under development (unstable) (2018-07-23 r75001)

#### Linux
* ubuntu 16.04.3, R-oldrel 3.4.4 (local)
* ubuntu 14.04.5, R-oldrel 3.4.4 (Travis CI)
* ubuntu 14.04.5 R-release 3.5.0 (Travis CI)
* ubuntu 14.04.5 R-devel R Under development (unstable) (2018-06-20 r74923) (Travis CI)

#### Mac
* OS X Sierra 10.12.6, R-oldrel 3.4.4 (Travis CI)
* OS X Sierra 10.12.6, R-release 3.5.0 (Travis CI)

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
This bugfix release fixes one specific bug introduced 11 days ago; it does not affect downstream dependencies.

I checked 7 reverse dependencies from CRAN: ballr, bomrang, driftR, fivethirtyeight, moderndive, postal, questionr.  I get some unrelated warnings and notes but nothing having to do with the janitor package or this update.