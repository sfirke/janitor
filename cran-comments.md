# Submission
2024-12-22

## Submission summary

### janitor version 2.2.1
Contains only trivial changes needed to address failing test on CRAN, specific to how timezones are handled in Debian. 

### Test environments

#### Windows
* Windows Server 2022 x64 (build 20348) with R Under development (unstable) (2024-12-20 r87452 ucrt) via win-builder, checked 2024-12-21

#### Linux
* Ubuntu 24.04 R-version 4.3.3 (2024-02-29) (local)

### R CMD check results
0 errors | 0 warnings | 0 notes

### Downstream dependencies
This does not negatively affect downstream dependencies.

revdepcheck passed for 124 of 125 packages. I investigated package BFS and the erroring function and found no use of janitor's functions so believe this to be a false positive.