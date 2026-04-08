## Submission (v0.4.0)
This version fixes DK model parameter names and formulas, and improves code maintainability with named parameter access.

## Test environments
* local macOS 15.4 (Darwin 25.3.0), R 4.5.1
* GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macOS-latest): R-release

## R CMD check results
0 errors | 0 warnings | 0 notes

## Changes since v0.3.0

### Breaking Changes
* DK model parameter names now match the likelihood function:
  - Old: lgg, lgk, lgc, lkk, lcg, lck, lcc, gamma
  - New: lgg, lgk, lgd, lkg, lkk, lkd, ldd, gamma

### Improvements
* All internal code uses named row access instead of numeric indices
* GOF functions (fit_model, fit_dk, fit_nodk) use formulas consistent with likelihood
* lca_adj correctly handles both DK and non-DK models
* Model criticism functions use correct DK model formulas
* Improved README with clearer user documentation

### Bug Fixes
* Fixed DK model cell probabilities in cell_probs()
* Fixed expected values calculation in calculate_expected_values()

### Testing
* 678 tests passing
* R CMD check: 0 errors, 0 warnings, 0 notes
