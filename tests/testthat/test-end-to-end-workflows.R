test_that("public item and person workflows recover no-DK simulations", {
  truth <- c(gg = 0.40, gk = 0.30, kk = 0.30)
  gamma <- c(item1 = 0.15, item2 = 0.25, item3 = 0.35, item4 = 0.45)
  sim <- simulate_lca(
    n = 3000, n_items = 4,
    gg = truth[["gg"]], gk = truth[["gk"]], kk = truth[["kk"]],
    gamma = gamma, seed = 951, return_classes = TRUE
  )

  item_fit <- item_lca_fit(sim$pre, sim$post)
  person_fit <- person_item_lca_fit(sim$pre, sim$post, item_fit = item_fit)
  posterior <- posterior_class_probs(person_fit)

  expect_s3_class(item_fit, "guess_fit")
  expect_equal(rowMeans(item_fit$params[names(truth), ]), truth, tolerance = 0.05)
  expect_equal(item_fit$params["gamma", ], gamma, tolerance = 0.05)
  expect_s3_class(person_fit, "guess_person_fit")
  expect_true(person_fit$converged)
  expect_equal(person_fit$class_priors, truth, tolerance = 0.04)
  expect_equal(person_fit$gamma, gamma, tolerance = 0.04)
  expect_equal(rowSums(posterior), rep(1, nrow(sim$pre)), tolerance = 1e-10)
  expect_equal(posterior_learned(person_fit), posterior$P_gk)
  expect_gt(cor(posterior$P_gk, as.numeric(sim$learned)), 0.8)
})

test_that("public DK workflow preserves explicit and NA-coded DK responses", {
  sim <- simulate_lca_dk(
    n = 2200, n_items = 3,
    gg = 0.25, gk = 0.15, gd = 0.10, kk = 0.15,
    dg = 0.10, dk = 0.10, dd = 0.15,
    gamma = 0.25, seed = 952
  )
  pre_na <- sim$pre
  post_na <- sim$post
  pre_na[pre_na == "d"] <- NA
  post_na[post_na == "d"] <- NA

  explicit_transitions <- multi_transmat(sim$pre, sim$post)
  na_transitions <- multi_transmat(pre_na, post_na)
  explicit_fit <- item_lca_fit(sim$pre, sim$post)
  na_fit <- item_lca_fit(pre_na, post_na)

  expect_equal(na_transitions, explicit_transitions)
  expect_equal(na_fit$params, explicit_fit$params, tolerance = 1e-8)
  expect_equal(na_fit$learning, explicit_fit$learning, tolerance = 1e-8)
  expect_equal(
    rowSums(na_transitions),
    stats::setNames(rep(2200, 3), names(sim$pre))
  )

  fit_stats <- fit_model(
    pre_na, post_na,
    g = na_fit$params["gamma", ],
    est_param = na_fit$params[-nrow(na_fit$params), ],
    force9 = TRUE
  )
  expect_equal(dim(fit_stats), c(2L, 3L))
  expect_true(all(is.finite(fit_stats)))
})

test_that("public structural-missingness workflow omits only incomplete pairs", {
  sim <- simulate_lca(n = 1600, n_items = 3, seed = 953)
  pre <- sim$pre
  post <- sim$post
  pre[1:200, 1] <- NA
  post[101:300, 2] <- NA
  complete <- !is.na(as.matrix(pre)) & !is.na(as.matrix(post))

  transitions <- multi_transmat(pre, post, na_as = "missing")
  item_fit <- item_lca_fit(pre, post, na_as = "missing")
  person_fit <- person_item_lca_fit(pre, post, na_as = "missing")

  expect_equal(rowSums(transitions), colSums(complete))
  expect_equal(item_fit$n_obs, sum(complete))
  expect_equal(person_fit$n_obs, sum(complete))
  expect_error(
    item_lca_fit(
      pre, post,
      na_as = "missing", missing_action = "error"
    ),
    "Structural missing responses"
  )
})

test_that("public difficulty and score workflows satisfy their definitions", {
  difficulty <- c(item1 = 1, item2 = 0, item3 = -1)
  sim <- simulate_lca(
    n = 1800, n_items = 3,
    difficulty = difficulty, base_rate = 0.25, seed = 954
  )
  transitions <- multi_transmat(sim$pre, sim$post)
  difficulty_fit <- lca_difficulty(transitions, base_rate = 0.25)

  expected_gamma <- 0.25 + 0.75 * plogis(-difficulty_fit$params["difficulty", ])
  expect_s3_class(difficulty_fit, "guess_difficulty_fit")
  expect_equal(difficulty_fit$gamma, expected_gamma, tolerance = 1e-8)

  pre_score <- estimate_logit_score(sim$pre)
  post_score <- estimate_logit_score(sim$post)
  learning <- cross_sectional_learning(sim$pre, sim$post)
  bounded <- cross_sectional_learning_score(sim$pre, sim$post)
  expect_equal(learning, post_score - pre_score)
  expect_equal(bounded, plogis(learning))
})
