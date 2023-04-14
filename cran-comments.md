# Resubmission
This is a resubmission. In this version I have:

* Identified the relevant reference in the DESCRIPTION files as indicated (authors (year) <https:...>)

* Added the missing value fields in the corresponding .Rd files

* Replace dontrun by donttest (these examples require more then 5 seconds)

* Added a on.exit statement in the 'fitBeeGUTS' function

* restricted the examples to two cores

## Test environments
* local R installation, R 4.1.2
* Windows Server 2022, R-devel, 64 bit
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 3 note

* This is a new release.

* checking installed package size
    installed size is 10.0Mb
    sub-directories of 1Mb or more:
      data   4.1Mb
      libs   5.5Mb

* checking for GNU extensions in Makefiles
  GNU make is a SystemRequirements.

## Downstream dependencies
There are no downstream dependencies
