
# Submission
2017-05-07


### Test environments
* local Windows 10 install, R 3.3.3

On Travis CI:
* ubuntu 12.04.5, R-oldrel 3.3.3
* ubuntu 12.04.5 R-release 3.4.0
* ubuntu 12.04.5 R-devel R 3.5.0
* OS X El Capitan 10.11.6, R-oldrel 3.3.3
* OS X El Capitan 10.11.6, R-release 3.4.0

### R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

### Downstream dependencies
There are currently no downstream dependencies for this package.

---

# Resubmission
2016-10-30

Per feedback from Duncan, I updated a url in README.md to begin https:// to fit the canonical format.  The rest of the package resubmission is the same as submitted earlier today, described as follows.

---
# Submission
2016-10-30

This is a patch release, fixing a test that was failing on R 3.2.5 for Windows.  Uwe Ligges notified me of this on October 23rd:

> *"find that your package faild under R-oldrelease (R-3.2.5) with the packages available there. Please fix or declare proper version dependencies and resubmit."*

I have also fixed three other small bugs, described in NEWS.md.  No other package behavior has changed.

---

# Submission
2016-10-30

### Test environments
* local Windows 10 install, R 3.3.1
* ubuntu 12.04.5 (on travis-ci), R 3.3.1
* Debian Linux, R-devel, GCC (debian-gcc-devel)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (windows-x86_64-devel)
* Windows Server 2008 R2 SP1, R-oldrel (3.2.5), 32/64 bit (windows-x86_64-oldrel)

### R CMD check results
There were no ERRORs or WARNINGs.  There was 1 NOTE:

Maintainer: 'Sam Firke <samuel.firke@gmail.com>'

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2016
  COPYRIGHT HOLDER: Sam Firke

### Downstream dependencies
There are currently no downstream dependencies for this package.
