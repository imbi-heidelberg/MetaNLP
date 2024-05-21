## Resubmission
This is a resubmission. In this version I have:

* Removed all linebreaks in the Authors-section and removed unnecessary spaces in the Description-section in the DESCRIPTION-file.
* Removed one reference to a web-address in the documentation select_features.
* Removed all \dontrun{} statements. Added an inst/extdata folder containing data which is used in the examples via system.file().
* Removed a default path of the function write_csv.
* In the tests and the examples to write_csv, all files are written into a temporary directory and are removed afterwards.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
