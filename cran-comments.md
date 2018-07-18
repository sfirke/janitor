# Submission
2018-07-17

## Submission summary
Per CRAN request (Kurt Hornik on July 3rd 2018), I am submitting a patched version that addresses two dependency-based warnings currently being thrown under R-devel.  There are a few minor changes to the package functionality as well.

### Test environments

#### Windows
* win-builder with R Under development (unstable) (2018-07-15 r74966)

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
The only breaking change in this release that could affect another package is quite obscure and does not affect the downstream dependencies.

I checked 6 reverse dependencies from CRAN: ballr, bomrang, driftR, fivethirtyeight, moderndive, questionr.  I get some unrelated warnings and notes but nothing having to do with the janitor package or this update.