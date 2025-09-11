context("Test Utility Functions")

test_that("validate_dataframe works correctly", {
  # Valid data frame
  df <- data.frame(x = 1:3, y = 4:6)
  expect_true(validate_dataframe(df, "test_df"))
  
  # Invalid inputs
  expect_error(validate_dataframe(list(x = 1:3), "test_df"), "Specify test_df data.frame.")
  expect_error(validate_dataframe(1:3, "test_df"), "Specify test_df data.frame.")
  expect_error(validate_dataframe(data.frame(), "test_df"), "test_df cannot be empty.")
})

test_that("validate_compatible_dataframes works correctly", {
  df1 <- data.frame(x = 1:3, y = 4:6)
  df2 <- data.frame(a = 7:9, b = 10:12)
  df3 <- data.frame(a = 7:9)  # different number of columns
  df4 <- data.frame(a = 7:8, b = 10:11)  # different number of rows
  
  expect_true(validate_compatible_dataframes(df1, df2))
  expect_error(validate_compatible_dataframes(df1, df3), 
               "Lengths of pre_test and pst_test must be the same.")
  expect_error(validate_compatible_dataframes(df1, df4), 
               "Number of rows in pre_test and pst_test must be the same.")
})

test_that("validate_lucky_vector works correctly", {
  # Valid lucky vector
  expect_true(validate_lucky_vector(c(0.25, 0.33, 0.5), 3))
  
  # Invalid inputs
  expect_error(validate_lucky_vector(NULL, 3), "Specify lucky vector.")
  expect_error(validate_lucky_vector(c(0.25, 0.33), 3), "Length of input varies.")
  expect_error(validate_lucky_vector(c(0, 0.25, 0.5), 3), 
               "All lucky values must be between 0 and 1")
  expect_error(validate_lucky_vector(c(0.25, 1, 0.5), 3), 
               "All lucky values must be between 0 and 1")
  expect_error(validate_lucky_vector(c(0.25, -0.1, 0.5), 3), 
               "All lucky values must be between 0 and 1")
})

test_that("validate_transition_values works correctly", {
  # Valid inputs
  pre_valid <- c("1", "0", "1", "d", NA)
  pst_valid <- c("0", "1", "1", "1", "0")
  expect_true(validate_transition_values(pre_valid, pst_valid))
  
  # Invalid inputs
  pre_invalid <- c("1", "0", "2")  # Invalid value "2"
  pst_invalid <- c("0", "1", "0")
  expect_error(validate_transition_values(pre_invalid, pst_invalid), 
               "The input vectors can only contain: 0, 1, NA, d")
})

test_that("validate_gamma works correctly", {
  # Valid gamma values
  expect_true(validate_gamma(0.5))
  expect_true(validate_gamma(c(0.25, 0.33, 0.5)))
  
  # Invalid inputs
  expect_error(validate_gamma(NULL), "Gamma parameter cannot be NULL.")
  expect_error(validate_gamma(c(0.25, NA, 0.5)), "Gamma parameter cannot contain NA values.")
  expect_error(validate_gamma(c(0.25, -0.1, 0.5)), "Gamma parameter must be between 0 and 1.")
  expect_error(validate_gamma(c(0.25, 1.1, 0.5)), "Gamma parameter must be between 0 and 1.")
})

test_that("validate_priors works correctly", {
  # Valid priors
  expect_true(validate_priors(c(0.3, 0.1, 0.1, 0.25), 4, "nodk_priors"))
  
  # Invalid inputs
  expect_error(validate_priors(c("0.3", "0.1"), 2, "test_priors"), 
               "test_priors must be numeric.")
  expect_error(validate_priors(c(0.3, 0.1), 3, "test_priors"), 
               "test_priors must have length 3")
  expect_error(validate_priors(c(0.3, -0.1, 0.1), 3, "test_priors"), 
               "All test_priors values must be between 0 and 1.")
  expect_error(validate_priors(c(0.3, 1.1, 0.1), 3, "test_priors"), 
               "All test_priors values must be between 0 and 1.")
})

test_that("count_transitions works correctly", {
  # Test without DK responses
  pre_simple <- c("1", "0", "0", "1")
  pst_simple <- c("1", "0", "1", "1")
  result_simple <- count_transitions(pre_simple, pst_simple)
  
  expect_equal(length(result_simple), 4)
  expect_equal(names(result_simple), c("x00", "x01", "x10", "x11"))
  expect_equal(as.numeric(result_simple["x00"]), 1)
  expect_equal(as.numeric(result_simple["x01"]), 1)
  expect_equal(as.numeric(result_simple["x10"]), 0)
  expect_equal(as.numeric(result_simple["x11"]), 2)
  
  # Test with DK responses
  pre_dk <- c("1", "0", "d", "1")
  pst_dk <- c("1", "d", "1", "0")
  result_dk <- count_transitions(pre_dk, pst_dk)
  
  expect_equal(length(result_dk), 9)
  expect_equal(names(result_dk), c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd"))
  expect_equal(as.numeric(result_dk["x0d"]), 1)
  expect_equal(as.numeric(result_dk["xd1"]), 1)
  expect_equal(as.numeric(result_dk["x10"]), 1)
  expect_equal(as.numeric(result_dk["x11"]), 1)
})

test_that("format_transition_matrix works correctly", {
  # Create test data
  transitions1 <- c(x00 = 2, x01 = 1, x10 = 0, x11 = 2)
  transitions2 <- c(x00 = 1, x01 = 2, x10 = 1, x11 = 1)
  transition_list <- list(transitions1, transitions2)
  
  result <- format_transition_matrix(transition_list, 2, add_aggregate = FALSE)
  
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 4)
  expect_equal(rownames(result), c("item1", "item2"))
  expect_equal(colnames(result), c("x00", "x01", "x10", "x11"))
  expect_equal(result[1, ], transitions1)
  expect_equal(result[2, ], transitions2)
  
  # Test with aggregate
  result_agg <- format_transition_matrix(transition_list, 2, add_aggregate = TRUE)
  expect_equal(nrow(result_agg), 3)
  expect_equal(rownames(result_agg)[3], "agg")
  expect_equal(result_agg[3, ], colSums(result))
})

test_that("calculate_expected_values works correctly for nodk model", {
  gamma_i <- 0.25
  params <- c(0.3, 0.1, 0.1)  # lgg, lgk, lkk
  total_obs <- 100
  
  expected <- calculate_expected_values(gamma_i, params, total_obs, "nodk")
  
  expect_equal(length(expected), 4)
  expect_true(all(expected >= 0))
  expect_true(sum(expected) <= total_obs * 1.1)  # Allow tolerance
})

test_that("calculate_expected_values works correctly for dk model", {
  gamma_i <- 0.25
  params <- c(0.3, 0.1, 0.2, 0.05, 0.1, 0.1, 0.05)  # 7 parameters for DK model
  total_obs <- 100
  
  expected <- calculate_expected_values(gamma_i, params, total_obs, "dk")
  
  expect_equal(length(expected), 9)
  expect_true(all(expected >= 0))
  expect_true(sum(expected) <= total_obs * 1.1)  # Allow tolerance
})
