# Submission
2018-03-17

### Test environments
* local Windows 10 install, R 3.4.3
* local Windows 10 install, R 3.4.4
* win-builder with R-devel 2018-03-16 74418
* ubuntu 16.04.3, R-release 3.4.3
* ubuntu 16.04.3, R-release 3.4.4


On Travis CI:
* ubuntu 14.04.5, R-oldrel 3.3.3
* ubuntu 14.04.5 R-release 3.4.4
* ubuntu 14.04.5 R-devel R 3.5.0 74418
* OS X Sierra 10.12.6, R-oldrel 3.3.3
* OS X Sierra 10.12.6, R-release 3.4.4

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
This update to janitor v1.0 involves breaking changes that affect some downstream packages.  I advised all downstream package maintainers of these changes on February 21, including providing code that would keep their packages compatible with all versions of janitor.

I checked 4 reverse dependencies from CRAN.

#### New problems
These CRAN packages depending on janitor each have 1 warning:

* ballr: vignette fails due to change in janitor::clean_names, I proposed a fix to package maintainer
* 


# Submission
2018-01-04

This is a tiny fix (I deleted six characters) to address a bug exposed by an updated dependency package and identified by CRAN automated testing.

### Test environments
* local Windows 10 install, R 3.4.1
* win-builder with R-devel 2018-01-03 r74042

On Travis CI:
* ubuntu 14.04.5, R-oldrel 3.3.3
* ubuntu 14.04.5 R-release 3.4.2
* ubuntu 14.04.5 R-devel R 3.5.0 74052
* OS X Sierra 10.12.6, R-oldrel 3.3.3
* OS X Sierra 10.12.6, R-release 3.4.3

### R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

### Downstream dependencies
Both CRAN packages depending on janitor (ballr, bomrang) fully passed R CMD check.
