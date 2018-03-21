# Submission
2018-03-21

### Test environments

#### Windows
* local Windows 10 install, R 3.4.3
* local Windows 10 install, R 3.4.4
* win-builder with R-devel 2018-03-16 74418

#### Linux
* ubuntu 16.04.3, R-release 3.4.3 (local)
* ubuntu 16.04.3, R-release 3.4.4 (local)
* ubuntu 14.04.5, R-oldrel 3.3.3 (Travis CI)
* ubuntu 14.04.5 R-release 3.4.4 (Travis CI)
* ubuntu 14.04.5 R-devel R 3.5.0 74418 (Travis CI)

#### Mac
* OS X Sierra 10.12.6, R-oldrel 3.3.3 (Travis CI)
* OS X Sierra 10.12.6, R-release 3.4.4 (Travis CI)

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
The janitor v1.0 update involves breaking changes that affect some downstream packages.  I advised all downstream package maintainers of these changes on February 21, including providing example code that would keep their packages compatible with all versions of janitor.

I checked 4 reverse dependencies from CRAN.

#### New problems
These CRAN packages depending on janitor each have 1 warning:

* ballr, bomrang: failures due to change in janitor::clean_names. I proposed a fix to package maintainers on Feb 21 and emailed them notice of failing R CMD check on Mar 17.  Maintainers of both packages have since confirmed to me that they've updated their packages accordingly.  Both packages have been submitted to CRAN or will be shortly following janitor 1.0.0's acceptance.
* fivethirtyeight: my check yielded a warning `checking for unstated dependencies in example ... WARNING warning in parse(file = files, n = -1L) : invalid input found on input connection 'fivethirtyeight-Ex.R'.`   I discussed this warning with the maintainer; he tested janitor 1.0.0 with his package and confirmed that any WARNING or NOTE is not the result of changes to janitor.
