# Submission
2020-04-07

## Submission summary
Release version 2.0.0 of janitor, making many bug fixes and improvements.  There are minor breaking changes but they don't appear to affect downstream dependencies.

Timing is a factor: this was sped up to address errors caused by breaking changes to tibble 3.0.0 (now on CRAN, caused serious breakage, so I must release ASAP), dplyr 1.0.0, and R 4.0.0 (caused trivial test failures due to stringsAsFactors default changing).

### Test environments

#### Windows
* Windows 10 with R-release 3.6.3 (local)

#### Linux
* ubuntu 18.04.2, R-release 3.6.3  (local)
* ubuntu 14.04.5 R-release 3.6.2 (Travis CI)
* ubuntu 14.04.5 R-devel R Under development (unstable) (2020-03-13 r77948) (Travis CI)

#### Mac
I can't currently test this on Mac OS due to lack of access to a Mac OS device
and problems on Travis CI outside of my control.  But a very recent build passed
on Travis CI.

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
I ran revdepchk::revdep_check() and was able to check 19 of the 20 downstream dependency packages (I was unable to check DCPO).  I found no errors in downstream dependencies caused by changes to janitor 2.0.0 compared to the current CRAN version.
