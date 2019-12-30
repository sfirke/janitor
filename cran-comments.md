# Submission
2019-04-20

## Submission summary
I fixed a test that was failing as a result of the change to `base::sample()` and am submitting this update per CRAN request.  I've also incorporated new functionality & bug fixes accumulated over the last 9 months since 1.1.1 went to CRAN.

### Test environments

#### Windows
* win-builder with R-release 3.5.3 (2019-03-11)
* Windows 10 with R-oldrel 3.5.0 (local)

#### Linux
* ubuntu 18.04.2, R-oldrel 3.4.4  (local)
* ubuntu 14.04.5 R-release 3.5.3 (Travis CI)
* ubuntu 14.04.5 R-devel R  Under development (unstable) (2019-04-20 r76407) (Travis CI)

#### Mac
* OS X High Sierra 10.13.3, R-oldrel 3.4.4 (Travis CI)
* OS X High Sierra 10.13.3, R-release 3.5.3 (Travis CI)

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
The nature of changes to janitor in 1.2.0 should not introduce breakage for downstream dependencies.

I checked the 8 reverse dependencies from CRAN: ballr, bomrang, CGPfunctions, congressbr, driftR, fivethirtyeight, moderndive, questionr.  There was an error in congressbr, but it is unrelated to janitor (that package currently has errors in its CRAN check results).