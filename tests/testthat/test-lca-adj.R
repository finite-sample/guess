context("Test LCA Adjustment Function")

test_that("lca_adj works with basic data", {
  # Create test data with DK responses 
  pre_test <- data.frame(
    item1 = c(1, 0, 0, 1, "d", 1, 0, "d"),
    item2 = c(1, 0, 1, "d", 0, "d", 1, 0)
  )
  pst_test <- data.frame(
    item1 = c(1, 1, 0, "d", 1, 0, "d", 1),
    item2 = c(1, 0, 1, 0, "d", 1, "d", 0)
  )
  
  # Test basic functionality
  result <- lca_adj(pre_test, pst_test)
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("pre", "pst"))
  
  # Check that something was returned (basic smoke test)
  expect_true(!is.null(result$pre))
  expect_true(!is.null(result$pst))
})

test_that("lca_adj handles NA values", {
  # Create test data with NAs and DK
  pre_test <- data.frame(
    item1 = c(1, 0, NA, 1, "d", 0, 1, "d"),
    item2 = c(1, NA, 1, "d", 0, "d", 1, 0)
  )
  pst_test <- data.frame(
    item1 = c(1, 1, 0, "d", NA, 1, "d", 0),
    item2 = c(NA, 0, 1, 0, "d", 0, "d", 1)
  )
  
  # Should produce a warning about NA conversion
  expect_warning(
    result <- lca_adj(pre_test, pst_test),
    "NAs will be converted to 0"
  )
  
  # Check that function still works
  expect_type(result, "list")
  expect_named(result, c("pre", "pst"))
})

test_that("lca_adj validates basic inputs", {
  # Test with NULL inputs (should error)
  expect_error(lca_adj(NULL, NULL))
  
  # Test basic functionality doesn't crash
  pre_test <- data.frame(item1 = c(1, 0, "d", 1, 0))
  pst_test <- data.frame(item1 = c(1, "d", 1, 0, 1))
  
  expect_no_error(result <- lca_adj(pre_test, pst_test))
})