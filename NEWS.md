rgeospatialquality 0.3.2
========================

### BUG FIXES

* Fixed function break when API returns 500 error

### IMPROVEMENTS

* Listed `plyr` dependency in README

rgeospatialquality 0.3.1
========================

### IMPROVEMENTS

* Implemented `add_flags` and `format_gq` as S3 methods
* Improved tests for this new implementation
* On `.travis.yml`, added option to cache packages so builds are faster

### BUG FIXES

* Fixed issue with Travis not being able to install `rgbif`, `rinat` and `rvertnet`

rgeospatialquality 0.3.0
========================

### IMPROVEMENTS

* Changed to MIT license
* Custom arguments can be passed to `httr` calls via the `...` argument
* Improved usability of `format_gq` function
* General documentation improvements

### NEW FEATURES

* Added Code of Conduct
* Added integration with Appveyor
* Added `quiet` option to functions, to supress warnings
* Added a plethora of new examples and tests
* Added ability to "guess" correct field names in `add_flags` when non-standard names are present

rgeospatialquality 0.2.0
========================

### NEW FEATURES

* Improved integration with some other data access packages:
    * `rgbif`
    * `rvertnet`
    * `rinat`
* `add_flags` shows summary output
* Added option to supress warnings and logging output in `add_flags`
* Added `format_gq` function
* Strong update of documentation
* Created vignette with Bioinformatics paper
* Added Travis-CI integration
* Added Codecov integration

### BUG FIXES

* Connection with `rvertnet` now works

rgeospatialquality 0.1.0
========================

Initial release
