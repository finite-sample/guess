# version 0.2.2 2025-12-15

## Validation System Modernization
* **Complete migration to checkmate**: Replaced all manual input validation with robust checkmate assertions
* **Enhanced validation utilities**: Added comprehensive validation helper functions in `utils-validation.R`
* **Standardized error messages**: All validation errors now use consistent checkmate format
* **Improved code quality**: Eliminated all manual `stop()` calls and `::` namespace patterns
* **Dependency optimization**: 
  - Added `checkmate` dependency for robust input validation
  - Removed `goji` dependency by implementing internal `zero1` function
* **Test suite updates**: Updated all test expectations to match new validation patterns
* **Documentation improvements**: Enhanced validation function documentation with proper `@importFrom` declarations

## Development Workflow Improvements
* **Local code coverage**: Replaced Codecov.io with local coverage reporting
  - Added `tools/check_coverage.R` for local coverage analysis
  - Created `Makefile` for common development tasks
  - Removed external Codecov dependency and badge
* **Fixed CRAN URL**: Updated to canonical CRAN package URL format

# version 0.2.1 2024-12-15

## Infrastructure & Modernization
* Added comprehensive GitHub Actions CI/CD workflows for R CMD check, test coverage, and pkgdown
* Updated minimum R version requirement to 4.0.0
* Modernized code patterns:
  - Replaced `T`/`F` with `TRUE`/`FALSE` throughout
  - Updated logical operators to use `||` for scalar comparisons
  - Removed deprecated `stringsAsFactors` arguments
  - Added explicit parameters to `mapply()` calls
* Enhanced package metadata in DESCRIPTION
* Improved .gitignore and .Rbuildignore patterns
* Re-enabled and configured lintr for code quality checks
* Updated pkgdown configuration URLs

# version 0.2.0 2017-05-XX

* Consistent support for input data format (with potential for d for 'don't know').
* Person level adjustments for LCA and standard correction
* Explain logic for Rsolnp priors and allow people to pass different priors
* Standard output and nomenclature for stnd_cor and lca_cor, including option for s.e.
* Extensive linting, expect_lint_free passes