# Submission
2020-01-22

## Submission summary
I fixed a test that will be failing under package tidyselect 1.0.0, coming to CRAN shortly.  The only changes are to tests that were generating warnings or failures due to changes in dependencies.

### Test environments

#### Windows
* win-builder with R-release 3.6.2 (2020-01-22)
* Windows 10 with R-oldrel 3.6.1 (local)

#### Linux
* ubuntu 14.04.5 R-release 3.6.2 (Travis CI)
* ubuntu 14.04.5 R-devel R  Under development (unstable) (2020-01-22 r77695) (Travis CI)

#### Mac
* OS X 10.13.6, R-oldrel 3.5.3 (Travis CI)
* OS X 10.13.6, R-release 3.6.2 (Travis CI)

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
No user-facing code has changed from janitor 1.2.0 to 1.2.1, only handling of messages that have changed because of tidyselect 1.0.0.  This update does not affect downstream dependencies.