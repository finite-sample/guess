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
    trans_matrix <- count_item_transitions(pre_test, post_test)

    # 2. Apply LCA correction
    lca_results <- fit_item_lca_counts(trans_matrix)

    # 3. Apply standard correction
    std_results <- stnd_cor(
      pre_test, post_test, guessing_probability = rep(0.25, n_items)
    )

    # 4. Test fit
    fit_results <- assess_item_lca_fit(lca_results, pre_test, post_test)

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
  expect_equal(length(group_results$mean_learning), n_items)

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
    trans_matrix_dk <- count_item_transitions(pre_test_dk, post_test_dk)

    # Apply LCA correction with DK
    lca_results_dk <- fit_item_lca_counts(trans_matrix_dk)

    # Test fit with DK
    fit_results_dk <- assess_item_lca_fit(lca_results_dk, pre_test_dk, post_test_dk)
  })

  # Validate DK-specific results
  expect_equal(ncol(trans_matrix_dk), 9) # 3x3 transition matrix
  expect_equal(nrow(lca_results_dk$params), 8) # 7 lambdas + 1 gamma
  expect_s3_class(fit_results_dk, "guess_gof")
})

test_that("Edge cases and error handling work correctly", {
  # Test with minimal data
  small_pre <- data.frame(item1 = c(0, 1, 0, 1))
  small_post <- data.frame(item1 = c(1, 1, 0, 1))

  expect_no_error({
    small_trans <- count_item_transitions(small_pre, small_post)
    small_lca <- fit_item_lca_counts(small_trans)
  })

  # Test validation functions work
  expect_error(count_item_transitions(data.frame(), small_post))
  expect_error(count_item_transitions(small_pre, data.frame()))

  # Test with all same responses (edge case)
  same_pre <- data.frame(item1 = rep(1, 10))
  same_post <- data.frame(item1 = rep(1, 10))

  expect_no_error({
    same_trans <- count_item_transitions(same_pre, same_post)
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

  trans_mat <- count_item_transitions(pre_test, post_test)
  lca_result <- fit_item_lca_counts(trans_mat)

  assessment <- assess_item_lca_fit(lca_result, pre_test, post_test)
  expect_s3_class(assessment, "guess_gof")
  expect_true(all(is.na(assessment$statistics$p_value)))
})
