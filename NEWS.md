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