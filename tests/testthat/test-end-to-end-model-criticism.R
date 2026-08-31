test_that("binary raw and aggregated model criticism agree end to end", {
  sim <- simulate_lca(
    n = 1200, n_items = 3,
    gg = 0.40, gk = 0.30, kk = 0.30,
    gamma = 0.25, seed = 901
  )
  transitions <- count_item_transitions(sim$pre, sim$post)
  fit <- fit_item_lca(sim$pre, sim$post)

  raw_ll <- score_individual_lca(fit, sim$pre, sim$post)$total_log_likelihood
  item_score <- score_item_lca(fit, transitions)
  item_ll <- item_score$total_log_likelihood

  expect_equal(raw_ll, item_ll, tolerance = 1e-8)
  expect_equal(
    score_individual_lca(fit, sim$pre, sim$post)$perplexity,
    item_score$perplexity,
    tolerance = 1e-8
  )
  expect_equal(unname(fit$learning), rep(0.30, 3), tolerance = 0.06)

  individual_cv <- cv_individual_lca(
    sim$pre, sim$post,
    k = 3, seed = 902
  )
  expect_true(is.finite(individual_cv$perplexity))
})

test_that("NA-coded DK data remain DK through model criticism", {
  sim <- simulate_lca_dk(
    n = 1800, n_items = 2,
    gg = 0.25, gk = 0.15, gd = 0.10, kk = 0.15,
    dg = 0.10, dk = 0.10, dd = 0.15,
    gamma = 0.25, seed = 903
  )
  pre_na <- sim$pre
  post_na <- sim$post
  pre_na[pre_na == "d"] <- NA
  post_na[post_na == "d"] <- NA

  transitions <- count_item_transitions(pre_na, post_na)
  fit <- fit_item_lca(pre_na, post_na)
  raw_ll <- score_individual_lca(fit, pre_na, post_na)$total_log_likelihood
  item_score <- score_item_lca(fit, transitions)
  item_ll <- item_score$total_log_likelihood

  expect_equal(ncol(transitions), 9L)
  expect_equal(unname(rowSums(transitions)), rep(1800, 2))
  expect_equal(raw_ll, item_ll, tolerance = 1e-8)
  expect_equal(
    score_individual_lca(fit, pre_na, post_na)$perplexity,
    item_score$perplexity,
    tolerance = 1e-8
  )
  expect_equal(unname(fit$learning), rep(0.25, 2), tolerance = 0.07)
})

test_that("structural omission agrees at every model-criticism boundary", {
  sim <- simulate_lca(
    n = 1400, n_items = 3,
    gg = 0.40, gk = 0.30, kk = 0.30,
    gamma = 0.25, seed = 904
  )
  set.seed(905)
  pre_matrix <- as.matrix(sim$pre)
  post_matrix <- as.matrix(sim$post)
  pre_matrix[sample(length(pre_matrix), 300)] <- NA
  post_matrix[sample(length(post_matrix), 450)] <- NA
  pre <- as.data.frame(pre_matrix)
  post <- as.data.frame(post_matrix)
  names(pre) <- names(sim$pre)
  names(post) <- names(sim$post)

  complete_counts <- vapply(
    seq_len(ncol(pre)),
    function(j) sum(!is.na(pre[[j]]) & !is.na(post[[j]])),
    integer(1)
  )
  transitions <- count_item_transitions(pre, post, na_as = "missing")
  fit <- fit_item_lca(pre, post, na_as = "missing")

  raw_ll <- score_individual_lca(
    fit, pre, post,
    na_as = "missing"
  )$total_log_likelihood
  item_score <- score_item_lca(fit, transitions)
  item_ll <- item_score$total_log_likelihood

  expect_equal(unname(rowSums(transitions)), unname(complete_counts))
  expect_equal(raw_ll, item_ll, tolerance = 1e-8)
  expect_equal(
    score_individual_lca(fit, pre, post, na_as = "missing")$perplexity,
    item_score$perplexity,
    tolerance = 1e-8
  )

  cv <- cv_individual_lca(
    pre, post,
    k = 3, seed = 906, na_as = "missing"
  )
  expect_true(is.finite(cv$perplexity))
  expect_equal(sum(cv$fold_results$test_obs), sum(complete_counts))
})
