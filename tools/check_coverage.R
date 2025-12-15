#!/usr/bin/env Rscript

# Local Code Coverage Report for guess Package
# Usage: Rscript tools/check_coverage.R

# Ensure covr is installed
if (!requireNamespace("covr", quietly = TRUE)) {
  message("Installing 'covr' package...")
  install.packages("covr")
}

# Run coverage analysis
message("\n=== Running Code Coverage Analysis ===\n")
cov <- covr::package_coverage(quiet = FALSE)

# Print summary
message("\n=== Coverage Summary ===")
print(cov)

# Calculate and print percentage
coverage_pct <- covr::percent_coverage(cov)
message(sprintf("\nTotal Coverage: %.1f%%\n", coverage_pct))

# Show file-by-file breakdown
file_coverage <- covr::coverage_to_list(cov)
if (length(file_coverage) > 0) {
  message("=== File Coverage ===")
  for (file in names(file_coverage)) {
    file_lines <- file_coverage[[file]]
    covered <- sum(file_lines > 0, na.rm = TRUE)
    total <- length(file_lines[!is.na(file_lines)])
    if (total > 0) {
      pct <- (covered / total) * 100
      message(sprintf("  %s: %.1f%% (%d/%d lines)", 
                      basename(file), pct, covered, total))
    }
  }
}

# Generate HTML report (optional)
if (interactive()) {
  message("\n=== Generating Interactive HTML Report ===")
  covr::report(cov)
} else {
  message("\nRun interactively to generate HTML coverage report: covr::report(cov)")
}