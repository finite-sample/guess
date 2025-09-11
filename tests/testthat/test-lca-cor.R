context("Test LCA Correction Function")

test_that("lca_cor validates inputs correctly", {
  # Test NULL input
  expect_error(lca_cor(NULL), "transmatrix cannot be NULL.")
  
  # Test non-matrix input
  expect_error(lca_cor(data.frame(x = 1:4)), "transmatrix must be a matrix.")
  
  # Test empty matrix
  expect_error(lca_cor(matrix(nrow = 0, ncol = 4)), "transmatrix cannot be empty.")
  
  # Test wrong dimensions
  expect_error(lca_cor(matrix(1:6, nrow = 2, ncol = 3)), 
               "transmatrix must have either 4 or 9 columns.")
  
  # Test invalid priors
  expect_error(lca_cor(matrix(1:4, nrow = 1), nodk_priors = c(0.3, 0.1, -0.1, 0.25)), 
               "All nodk_priors values must be between 0 and 1.")
  
  expect_error(lca_cor(matrix(1:4, nrow = 1), nodk_priors = c(0.3, 0.1)), 
               "nodk_priors must have length 4")
})

test_that("lca_cor works with no-DK data", {
  # Create simple transition matrix (no DK)
  transmat_nodk <- matrix(c(2, 1, 0, 2, 
                            1, 2, 1, 1), 
                          nrow = 2, byrow = TRUE)
  colnames(transmat_nodk) <- c("x00", "x01", "x10", "x11")
  rownames(transmat_nodk) <- c("item1", "item2")
  
  # This should work (may produce convergence messages)
  result <- lca_cor(transmat_nodk)
  
  result <- lca_cor(transmat_nodk)
  
  # Check output structure
  expect_true(is.list(result))
  expect_true("param.lca" %in% names(result))
  expect_true("est.learning" %in% names(result))
  
  # Check parameter matrix structure
  expect_true(is.matrix(result$param.lca))
  expect_equal(nrow(result$param.lca), 4)
  expect_equal(ncol(result$param.lca), 2)
  expect_equal(rownames(result$param.lca), c("lgg", "lgk", "lkk", "gamma"))
  
  # Check learning estimates
  expect_true(is.matrix(result$est.learning))
  expect_equal(ncol(result$est.learning), 2)
})

test_that("lca_cor works with DK data", {
  # Create transition matrix with DK responses
  transmat_dk <- matrix(c(1, 1, 0, 1, 1, 0, 1, 1, 0,
                          2, 0, 1, 0, 2, 0, 1, 0, 1), 
                        nrow = 2, byrow = TRUE)
  colnames(transmat_dk) <- c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd")
  rownames(transmat_dk) <- c("item1", "item2")
  
  result <- lca_cor(transmat_dk)
  
  # Check output structure
  expect_true(is.list(result))
  expect_true("param.lca" %in% names(result))
  expect_true("est.learning" %in% names(result))
  
  # Check parameter matrix structure for DK model
  expect_true(is.matrix(result$param.lca))
  expect_equal(nrow(result$param.lca), 8)
  expect_equal(ncol(result$param.lca), 2)
  expect_equal(rownames(result$param.lca), c("lgg", "lgk", "lgc", "lkk", "lcg", "lck", "lcc", "gamma"))
})

test_that("lca_cor handles edge cases", {
  # Single item case
  transmat_single <- matrix(c(3, 1, 1, 2), nrow = 1)
  colnames(transmat_single) <- c("x00", "x01", "x10", "x11")
  rownames(transmat_single) <- "item1"
  
  result <- lca_cor(transmat_single)
  expect_equal(ncol(result$param.lca), 1)
  expect_equal(ncol(result$est.learning), 1)
  
  # All zeros case (should handle gracefully)
  transmat_zeros <- matrix(rep(0, 8), nrow = 2)
  colnames(transmat_zeros) <- c("x00", "x01", "x10", "x11")
  rownames(transmat_zeros) <- c("item1", "item2")
  
  # This might fail optimization but shouldn't crash
  result_zeros <- lca_cor(transmat_zeros)
})

test_that("lca_cor parameter bounds are respected", {
  # Create valid transition matrix
  transmat <- matrix(c(10, 5, 2, 8, 
                       12, 3, 4, 6), 
                     nrow = 2, byrow = TRUE)
  colnames(transmat) <- c("x00", "x01", "x10", "x11")
  rownames(transmat) <- c("item1", "item2")
  
  result <- lca_cor(transmat)
  
  # All parameters should be between 0 and 1
  expect_true(all(result$param.lca >= 0, na.rm = TRUE))
  expect_true(all(result$param.lca <= 1, na.rm = TRUE))
  
  # Learning estimates should be reasonable
  expect_true(all(result$est.learning >= -1, na.rm = TRUE))  # Allow some negative learning
  expect_true(all(result$est.learning <= 1, na.rm = TRUE))
})
