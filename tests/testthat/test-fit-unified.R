context("Test Unified Fit Functions")

# Helper function to create test data
create_test_data <- function(with_dk = FALSE) {
  if (with_dk) {
    pre_test <- data.frame(
      item1 = c("1", "0", "d", "1", "0", "1", "d"),
      item2 = c("0", "1", "0", "d", "1", "0", "1")
    )
    pst_test <- data.frame(
      item1 = c("1", "d", "1", "0", "0", "1", "1"),
      item2 = c("0", "1", "1", "1", "1", "d", "0")
    )
  } else {
    pre_test <- data.frame(
      item1 = c(1, 0, 0, 1, 0, 1, 0),
      item2 = c(0, 1, 0, 1, 1, 0, 1)
    )
    pst_test <- data.frame(
      item1 = c(1, 1, 0, 1, 0, 1, 1),
      item2 = c(0, 1, 1, 1, 1, 0, 0)
    )
  }
  list(pre_test = pre_test, pst_test = pst_test)
}

test_that("fit_model validates inputs correctly", {
  test_data <- create_test_data()
  
  # Test NULL inputs
  expect_error(fit_model(NULL, test_data$pst_test, c(0.25, 0.25), matrix(0.1, 3, 2)), 
               "Specify pre_test data.frame.")
  expect_error(fit_model(test_data$pre_test, NULL, c(0.25, 0.25), matrix(0.1, 3, 2)), 
               "Specify pst_test data.frame.")
  expect_error(fit_model(test_data$pre_test, test_data$pst_test, NULL, matrix(0.1, 3, 2)), 
               "Both g and est.param must be provided.")
  expect_error(fit_model(test_data$pre_test, test_data$pst_test, c(0.25, 0.25), NULL), 
               "Both g and est.param must be provided.")
  
  # Test mismatched data frames
  wrong_pst <- data.frame(item1 = 1:3)
  expect_error(fit_model(test_data$pre_test, wrong_pst, c(0.25, 0.25), matrix(0.1, 3, 2)), 
               "Lengths of pre_test and pst_test must be the same.")
})

test_that("fit_model works with no-DK data", {
  # Use simple, well-defined test case
  pre_test <- data.frame(item1 = c(1, 0, 1, 0, 1))
  pst_test <- data.frame(item1 = c(1, 0, 0, 1, 1))
  
  # Use simple parameters that should work
  gamma <- list(0.25)  # gamma as list (like lca_cor returns)
  params <- matrix(c(0.4, 0.3, 0.3), nrow = 3, ncol = 1)  # lgg, lgk, lkk for item1
  
  # Should not crash
  expect_silent({
    result <- fit_model(pre_test, pst_test, gamma, params)
  })
  
  result <- fit_model(pre_test, pst_test, gamma, params)
  
  # Check structure
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 1)
  expect_equal(rownames(result), c("chi-square", "p-value"))
  expect_equal(colnames(result), "item1")
  
  # Check that values are reasonable
  expect_true(all(result["chi-square", ] >= 0))
  expect_true(all(result["p-value", ] >= 0))
  expect_true(all(result["p-value", ] <= 1))
})

test_that("fit_model basic functionality works", {
  # Simple test that the function can be called without errors
  pre_test <- data.frame(item1 = rep(c(1, 0), 5))
  pst_test <- data.frame(item1 = rep(c(1, 0), 5))
  
  gamma <- list(0.25)
  params <- matrix(c(0.4, 0.3, 0.3), nrow = 3, ncol = 1)
  
  expect_silent({
    result <- fit_model(pre_test, pst_test, gamma, params)
  })
  
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 1)
})

test_that("fit_model handles list-format gamma", {
  test_data <- create_test_data(with_dk = FALSE)
  
  # Gamma as list (as returned by lca_cor)
  gamma_list <- list(0.25, 0.30)
  params <- matrix(c(0.6, 0.2, 0.2,
                     0.7, 0.1, 0.2),
                   nrow = 3, ncol = 2)
  
  result <- fit_model(test_data$pre_test, test_data$pst_test, gamma_list, params)
  
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2, 2))
})

test_that("backward compatibility functions work", {
  # Simple test data
  pre_test <- data.frame(item1 = c(1, 0, 1, 0))
  pst_test <- data.frame(item1 = c(1, 1, 0, 1))
  
  # Simple parameters
  gamma <- list(0.25)
  params <- matrix(c(0.4, 0.3, 0.3), nrow = 3, ncol = 1)
  
  # Test that functions don't crash - actual values may vary
  expect_silent({
    result_nodk <- fit_nodk(pre_test, pst_test, gamma, params)
  })
  expect_true(is.matrix(result_nodk))
  expect_equal(dim(result_nodk), c(2, 1))
  
  expect_silent({
    result_dk <- fit_dk(pre_test, pst_test, gamma, params, force9 = FALSE)
  })
  expect_true(is.matrix(result_dk))
  expect_equal(dim(result_dk), c(2, 1))
})

test_that("fit_model handles edge cases", {
  # Single item case
  pre_single <- data.frame(item1 = c(1, 0, 1, 0))
  pst_single <- data.frame(item1 = c(1, 1, 0, 0))
  gamma_single <- 0.25
  params_single <- c(0.5, 0.2, 0.3)
  
  result_single <- fit_model(pre_single, pst_single, gamma_single, params_single)
  expect_equal(ncol(result_single), 1)
  expect_equal(colnames(result_single), "item1")
  
  # Very small data set
  pre_tiny <- data.frame(item1 = c(1, 0))
  pst_tiny <- data.frame(item1 = c(0, 1))
  
  # This might produce warnings due to small sample size, but shouldn't crash
  expect_silent({
    result_tiny <- fit_model(pre_tiny, pst_tiny, 0.25, c(0.5, 0.2, 0.3))
  })
  
  expect_true(is.matrix(result_tiny))
})

test_that("fit_model removes aggregate rows correctly", {
  test_data <- create_test_data(with_dk = FALSE)
  
  # Add aggregate manually to test removal
  transmat <- multi_transmat(test_data$pre_test, test_data$pst_test, agg = TRUE)
  expect_true("agg" %in% rownames(transmat))
  
  gamma <- c(0.25, 0.25)
  params <- matrix(c(0.5, 0.2, 0.3, 0.6, 0.1, 0.3), nrow = 3, ncol = 2)
  
  # fit_model should handle this correctly by removing the aggregate row
  result <- fit_model(test_data$pre_test, test_data$pst_test, gamma, params)
  expect_equal(ncol(result), 2)  # Should only have 2 items, not 3 (with agg)
  expect_false("agg" %in% colnames(result))
})
