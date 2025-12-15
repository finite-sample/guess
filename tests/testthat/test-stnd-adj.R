context("Test Standard Adjustment")

test_that("stnd adj works correctly", {

  # Transmat
  pre_test_var <- c(1, 0, 0, 1, 0, 1, 0)
  pst_test_var <- c(1, 0, 1, 1, 0, 1, 1)

  res <- transmat(pre_test_var, pst_test_var)
  cor_ans <- c(x00 = 2, x01 = 2, x10 = 0, x11 = 3)

  expect_equal(sapply(res, as.numeric), cor_ans)

  # Multi-transmat
  pre_test <- data.frame(pre_item1 = c(1, 0, 0, 1, 0),
                         pre_item2 = c(1, NA, 0, 1, 0))
  pst_test <- data.frame(pst_item1 = pre_test[, 1] + c(0, 1, 1, 0, 0),
                         pst_item2 = pre_test[, 2] + c(0, 1, 0, 0, 1))

  res <- multi_transmat(pre_test, pst_test)
  cor_ans <-  matrix(c(1, 2, 0, 2, 2, 1, 0, 2), byrow = TRUE, nrow = 2)
  rownames(cor_ans) <- c("item1", "item2")
  colnames(cor_ans) <- c("x00", "x01", "x10", "x11")
  expect_equal(res, cor_ans)
})

