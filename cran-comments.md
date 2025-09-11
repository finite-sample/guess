## Resubmission (v0.2.0)
This version includes significant refactoring, comprehensive testing improvements, and bug fixes.

## Test environments
* local macOS Sequoia 15.6.1, R 4.5.1
* GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
* GitHub Actions (windows-latest): R-release  
* GitHub Actions (macOS-latest): R-release

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:
1. CRAN incoming feasibility check flagged HTTP URLs in vignette that redirect to HTTPS (all URLs work correctly)
2. HTML manual check skipped due to missing recent HTML Tidy (non-critical)

## Changes in this version
* Major refactoring with utility modules for better maintainability
* 29x increase in test coverage (from 7 to 206 tests)
* Unified fit function consolidating fit_dk/fit_nodk
* Updated all URLs to use correct GitHub organization
* Fixed deprecated lintr configuration
* Added comprehensive GitHub Actions CI/CD workflows
* Improved input validation and error handling
* Maintained full backward compatibility

