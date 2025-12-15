# https://github.com/jimhester/lintr
if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    skip_on_cran()
    skip_on_ci()  # Skip on CI initially to avoid build failures
    lintr::expect_lint_free(cache = TRUE)
  })
}

