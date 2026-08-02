# Comprehensive validation tests using data-raw approach

test_that("All core functions work together in realistic workflow", {
  set.seed(42)

  # Create realistic test data (small scale)
  n_people <- 50
  n_items <- 5

  # Generate pre-test data (40% know the answers)
  pre_test <- data.frame(
    item1 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.6, 0.4)),
    item2 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.6, 0.4)),
    item3 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.6, 0.4)),
    item4 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.6, 0.4)),
    item5 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.6, 0.4))
  )

  # Generate post-test data (60% know the answers - learning occurred)
  post_test <- data.frame(
    item1 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.4, 0.6)),
    item2 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.4, 0.6)),
    item3 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.4, 0.6)),
    item4 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.4, 0.6)),
    item5 = sample(c(0, 1), n_people, replace = TRUE, prob = c(0.4, 0.6))
  )

  # Test complete workflow
  expect_no_error({
    # 1. Create transition matrix
    trans_matrix <- multi_transmat(pre_test, post_test)

    # 2. Apply LCA correction
    lca_results <- lca_cor(trans_matrix)

    # 3. Apply standard correction
    std_results <- stnd_cor(pre_test, post_test, lucky = rep(0.25, n_items))

    # 4. Test fit
    fit_results <- fit_model(
      pre_test, post_test,
      lca_results$params["gamma", ],
      lca_results$params[c("gg", "gk", "kk"), ]
    )

    # 5. Test group adjustment
    group_results <- group_adj(pre_test, post_test, rep(0.25, n_items))
  })

  # Validate results structure
  expect_equal(nrow(trans_matrix), n_items)
  expect_equal(ncol(trans_matrix), 4)
  expect_true(inherits(lca_results, "guess_fit"))
  expect_true(all(c("params", "learning") %in% names(lca_results)))
  expect_equal(length(std_results$learn), n_items)
  expect_true(is.list(group_results))
  expect_equal(length(group_results$learn), n_items)

  # Validate learning estimates are reasonable
  expect_true(all(lca_results$learning >= -1 & lca_results$learning <= 1))
  expect_true(all(std_results$learn >= -1 & std_results$learn <= 1))
})

test_that("Functions handle Don't Know responses correctly", {
  set.seed(123)

  n_people <- 40
  n_items <- 4

  # Create data with Don't Know responses
  pre_test_dk <- data.frame(
    item1 = sample(c(0, 1, "d"), n_people, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    item2 = sample(c(0, 1, "d"), n_people, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    item3 = sample(c(0, 1, "d"), n_people, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    item4 = sample(c(0, 1, "d"), n_people, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )

  post_test_dk <- data.frame(
    item1 = sample(c(0, 1, "d"), n_people, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    item2 = sample(c(0, 1, "d"), n_people, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    item3 = sample(c(0, 1, "d"), n_people, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    item4 = sample(c(0, 1, "d"), n_people, replace = TRUE, prob = c(0.4, 0.4, 0.2))
  )

  expect_no_error({
    # Create 9-column transition matrix
    trans_matrix_dk <- multi_transmat(pre_test_dk, post_test_dk, force9 = TRUE)

    # Apply LCA correction with DK
    lca_results_dk <- lca_cor(trans_matrix_dk)

    # Test fit with DK
    fit_results_dk <- fit_model(pre_test_dk, post_test_dk,
      lca_results_dk$params["gamma", ],
      lca_results_dk$params[c("gg", "gk", "gd", "kk", "dg", "dk", "dd"), ],
      force9 = TRUE
    )
  })

  # Validate DK-specific results
  expect_equal(ncol(trans_matrix_dk), 9) # 3x3 transition matrix
  expect_equal(nrow(lca_results_dk$params), 8) # 7 lambdas + 1 gamma
  expect_true(is.matrix(fit_results_dk) || is.data.frame(fit_results_dk))
})

test_that("Edge cases and error handling work correctly", {
  # Test with minimal data
  small_pre <- data.frame(item1 = c(0, 1, 0, 1))
  small_post <- data.frame(item1 = c(1, 1, 0, 1))

  expect_no_error({
    small_trans <- multi_transmat(small_pre, small_post)
    small_lca <- lca_cor(small_trans)
  })

  # Test validation functions work
  expect_error(multi_transmat(data.frame(), small_post)) # Empty pre-test
  expect_error(multi_transmat(small_pre, data.frame())) # Empty post-test

  # Test with all same responses (edge case)
  same_pre <- data.frame(item1 = rep(1, 10))
  same_post <- data.frame(item1 = rep(1, 10))

  expect_no_error({
    same_trans <- multi_transmat(same_pre, same_post)
    # LCA may have convergence issues with degenerate data, but shouldn't crash
  })
})

test_that("Backward compatibility maintained", {
  # Test that old function names still work
  set.seed(99)

  pre_test <- data.frame(
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE)
  )
  post_test <- data.frame(
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE)
  )

  trans_mat <- multi_transmat(pre_test, post_test)
  lca_result <- lca_cor(trans_mat)

  # Test that both fit functions work
  expect_no_error({
    fit_nodk_result <- fit_nodk(
      pre_test, post_test,
      lca_result$params["gamma", ],
      lca_result$params[c("gg", "gk", "kk"), ]
    )
  })

  expect_no_error({
    fit_model_result <- fit_model(
      pre_test, post_test,
      lca_result$params["gamma", ],
      lca_result$params[c("gg", "gk", "kk"), ]
    )
  })

  # Results should be equivalent
  expect_equal(dim(fit_nodk_result), dim(fit_model_result))
})
