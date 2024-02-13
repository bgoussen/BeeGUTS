# BeeGUTS 1.2
* Introduce calculations for Time Reinforced Toxicity test and time of effect

# BeeGUTS 1.1.3
* Update to rstan version 2.26

# BeeGUTS 1.1.2
* Reduce the size of the package to fulfill CRAN requirements

# Version 1.1.1
* Correction of the default value for parameter k_sr for _Honey Bee_.
* Update of the Tutorial


# Version 1.1.0
## New features
* Added new species. It is now possible to use the package for _Honey Bee_, 
_Bumble Bee_,  _Osmia bicornis_, and a _User defined bee_

## Update
* Update of the package syntax for compatibility with the upcoming rstan 2.26 

## Bug fixes
* Correction of a bug in the LCx summary function (object LCx$dfLCx is a list and was not printed in the previous version)
* Correction of the documentation
*  Increase the time sequence resolution when recalculating the concentrations. A too low resolution can lead to errors when the experiments run for partial days (e.g. 2.25 days in the acute oral). 


# Version 1.0.1
* Correct bug in the summary function where the summary of the `beeSurvFit` class would
use the wrong function.

# Version 1.0.0
* Add possibility to use more than one dataset at a time. One file should be loaded
per datasets. The concentrations should be expressed in the same unit.
* Add possibility to have NAs in the datasets

# Version 0.0.0.9000 (Development release)

## Known limitations
* Only one dataset at a time can be used for the calibration
* LPx calculations are not currently implemented
* PPC plots and EFSA criteria are not available for the calibration. They are available for the validation
* Only the _honey bee_ species is implemented
