context("Test Multi Transition Matrix Function")

test_that("multi_transmat validates inputs correctly", {
  # Test NULL inputs
  expect_error(multi_transmat(NULL, data.frame(x = 1:3)), "Specify pre_test data.frame.")
  expect_error(multi_transmat(data.frame(x = 1:3), NULL), "Specify pst_test data.frame.")
  
  # Test non-dataframe inputs
  expect_error(multi_transmat(c(1, 2, 3), data.frame(x = 1:3)), "Specify pre_test data.frame.")
  expect_error(multi_transmat(data.frame(x = 1:3), c(1, 2, 3)), "Specify pst_test data.frame.")
  
  # Test mismatched dimensions
  pre_test_diff <- data.frame(x = 1:3, y = 4:6)  # 2 columns
  pst_test_diff <- data.frame(z = 7:9)           # 1 column
  expect_error(multi_transmat(pre_test_diff, pst_test_diff), 
               "Lengths of pre_test and pst_test must be the same.")
  
  # Test mismatched rows
  pre_test_rows <- data.frame(x = 1:3, y = 4:6)  # 3 rows
  pst_test_rows <- data.frame(a = 1:2, b = 3:4)  # 2 rows
  expect_error(multi_transmat(pre_test_rows, pst_test_rows), 
               "Number of rows in pre_test and pst_test must be the same.")
  
  # Test invalid subgroup
  pre_test <- data.frame(x = 1:3)
  pst_test <- data.frame(y = 1:3)
  expect_error(multi_transmat(pre_test, pst_test, subgroup = c(TRUE, FALSE)), 
               "subgroup must have the same length as number of rows")
  expect_error(multi_transmat(pre_test, pst_test, subgroup = c(1, 0, 1)), 
               "subgroup must be a logical vector.")
})

test_that("multi_transmat works correctly without DK", {
  # Create test data without DK responses
  pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0), 
                         item2 = c(1, NA, 0, 1, 0))
  pst_test <- data.frame(item1 = c(1, 0, 1, 1, 0), 
                         item2 = c(1, 1, 0, 1, 1))
  
  result <- multi_transmat(pre_test, pst_test)
  
  # Check structure
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 2)  # 2 items
  expect_equal(ncol(result), 4)  # No DK, so 4 columns
  expect_equal(rownames(result), c("item1", "item2"))
  expect_equal(colnames(result), c("x00", "x01", "x10", "x11"))
  
  # Check that values are reasonable
  expect_true(all(result >= 0))
  expect_true(all(rowSums(result) > 0))  # Each item should have some responses
})

test_that("multi_transmat works correctly with DK", {
  # Create test data with DK responses
  pre_test <- data.frame(item1 = c("1", "0", "d", "1", "0"), 
                         item2 = c("1", "d", "0", "1", "d"))
  pst_test <- data.frame(item1 = c("1", "d", "1", "0", "0"), 
                         item2 = c("0", "1", "0", "1", "1"))
  
  result <- multi_transmat(pre_test, pst_test)
  
  # Check structure
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 2)  # 2 items
  expect_equal(ncol(result), 9)  # DK present, so 9 columns
  expect_equal(rownames(result), c("item1", "item2"))
  expect_equal(colnames(result), c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd"))
  
  # Check that values are reasonable
  expect_true(all(result >= 0))
  expect_true(all(rowSums(result) > 0))
})

test_that("multi_transmat handles subgroup correctly", {
  # Create test data
  pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0))
  pst_test <- data.frame(item1 = c(1, 0, 1, 1, 0))
  subgroup <- c(TRUE, TRUE, FALSE, TRUE, FALSE)  # Select rows 1, 2, 4
  
  result <- multi_transmat(pre_test, pst_test, subgroup = subgroup)
  
  # Should only use 3 observations (rows 1, 2, 4)
  expect_equal(sum(result), 3)
  
  # Manual calculation: pre[1,2,4] = c(1,0,1), pst[1,2,4] = c(1,0,1)
  # Transitions: 1->1, 0->0, 1->1
  # So x00=1, x01=0, x10=0, x11=2
  expected_transitions <- c(1, 0, 0, 2)
  expect_equal(as.numeric(result), expected_transitions)
})

test_that("multi_transmat handles aggregate option", {
  pre_test <- data.frame(item1 = c(1, 0, 0, 1), 
                         item2 = c(0, 1, 0, 1))
  pst_test <- data.frame(item1 = c(1, 1, 0, 0), 
                         item2 = c(0, 1, 1, 1))
  
  # Without aggregate
  result_no_agg <- multi_transmat(pre_test, pst_test, agg = FALSE)
  expect_equal(nrow(result_no_agg), 2)
  expect_false("agg" %in% rownames(result_no_agg))
  
  # With aggregate
  result_with_agg <- multi_transmat(pre_test, pst_test, agg = TRUE)
  expect_equal(nrow(result_with_agg), 3)
  expect_true("agg" %in% rownames(result_with_agg))
  
  # Aggregate row should be sum of individual items
  manual_agg <- colSums(result_no_agg)
  expect_equal(as.numeric(result_with_agg["agg", ]), as.numeric(manual_agg))
})

test_that("multi_transmat handles force9 option", {
  # Create data without DK
  pre_test <- data.frame(item1 = c(1, 0, 0, 1))
  pst_test <- data.frame(item1 = c(1, 1, 0, 0))
  
  # Without force9
  result_4col <- multi_transmat(pre_test, pst_test, force9 = FALSE)
  expect_equal(ncol(result_4col), 4)
  
  # With force9
  result_9col <- multi_transmat(pre_test, pst_test, force9 = TRUE)
  expect_equal(ncol(result_9col), 9)
  
  # DK columns should be zero
  dk_cols <- c("x0d", "x1d", "xd0", "xd1", "xdd")
  expect_true(all(result_9col[, dk_cols] == 0))
})

test_that("multi_transmat handles edge cases", {
  # Single row
  pre_single <- data.frame(item1 = 1, item2 = 0)
  pst_single <- data.frame(item1 = 0, item2 = 1)
  
  result_single <- multi_transmat(pre_single, pst_single)
  expect_equal(nrow(result_single), 2)
  expect_equal(sum(result_single), 2)  # Only 1 observation per item
  
  # All same responses
  pre_same <- data.frame(item1 = rep(1, 5))
  pst_same <- data.frame(item1 = rep(1, 5))
  
  result_same <- multi_transmat(pre_same, pst_same)
  expect_equal(result_same[1, "x11"], 5)
  expect_equal(sum(result_same[1, ]), 5)
  
  # Mixed NA and valid responses
  pre_mixed <- data.frame(item1 = c(1, NA, 0, 1))
  pst_mixed <- data.frame(item1 = c(NA, 1, 0, 1))
  
  result_mixed <- multi_transmat(pre_mixed, pst_mixed)
  # NAs should be converted to 0 by nona() function
  expect_true(all(result_mixed >= 0))
  expect_equal(sum(result_mixed), 4)
})
