# Submission
2023-02-01

## Submission summary

*Resubmitting as 2.2.0 again with fixes to URLs in documentation*

### janitor version 2.2.0
An accumulation of enhancements and bug fixes.  Breaking changes only for edge cases.

Notably, this fixes the current test failures on CRAN for this package, resulting from
changes introduced in the latest version of the dplyr package.

### Test environments

#### Windows
* Windows 10 with R-release 4.2.2 (local)
* Windows Server 2022 x64 (build 20348) with R Under development (unstable) (2023-01-31 r83741 ucrt) via win-builder, checked 2023-02-01

#### Linux
* ubuntu 22.04 R-release 4.2.2 (Github CI)
* ubuntu 22.04 R-devel R Under development (unstable) (2023-02-01) (Github CI)
* ubuntu 22.04 R-oldrel 4.1.3 (Github CI)

#### Mac
* Mac OS 12.6.2 with R-release 4.2.2 (Github CI)

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
This does not negatively affect downstream dependencies.

revdepcheck passed for 101 of 101 packages (98 from CRAN, 3 from bioconductor).