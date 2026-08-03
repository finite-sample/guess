test_that("item and person/item models expose distinct fitted structures", {
  sim <- simulate_lca(
    n = 2500, n_items = 4,
    gg = 0.40, gk = 0.30, kk = 0.30,
    gamma = c(0.15, 0.25, 0.35, 0.45),
    seed = 941, return_classes = TRUE
  )

  item_fit <- item_lca_fit(sim$pre, sim$post)
  person_fit <- person_item_lca_fit(
    sim$pre, sim$post,
    item_fit = item_fit
  )

  expect_s3_class(item_fit, "guess_fit")
  expect_equal(dim(item_fit$params), c(4L, 4L))
  expect_s3_class(person_fit, "guess_person_fit")
  expect_named(
    person_fit,
    c(
      "class_priors", "gamma", "posterior", "log_likelihood",
      "n_items", "n_obs", "iterations", "converged", "call"
    )
  )
  expect_equal(nrow(person_fit$posterior), 2500L)
  expect_equal(rowSums(person_fit$posterior), rep(1, 2500), tolerance = 1e-10)
})

test_that("person/item EM recovers shared classes and item gamma", {
  truth <- c(gg = 0.40, gk = 0.30, kk = 0.30)
  gamma <- c(item1 = 0.15, item2 = 0.25, item3 = 0.35, item4 = 0.45)
  sim <- simulate_lca(
    n = 6000, n_items = 4,
    gg = truth["gg"], gk = truth["gk"], kk = truth["kk"],
    gamma = gamma, seed = 942
  )

  fit <- person_item_lca_fit(sim$pre, sim$post)

  expect_true(fit$converged)
  expect_equal(fit$class_priors, truth, tolerance = 0.035)
  expect_equal(fit$gamma, gamma, tolerance = 0.035)
  expect_true(is.finite(fit$log_likelihood))
})

test_that("posterior wrappers use the explicit person/item model", {
  sim <- simulate_lca(
    n = 1500, n_items = 5,
    gg = 0.35, gk = 0.35, kk = 0.30,
    gamma = 0.25, seed = 943, return_classes = TRUE
  )
  item_fit <- item_lca_fit(sim$pre, sim$post)
  person_fit <- person_item_lca_fit(
    sim$pre, sim$post,
    item_fit = item_fit
  )

  posterior <- posterior_class_probs(person_fit)
  learned <- posterior_learned(person_fit)

  expect_equal(posterior, person_fit$posterior)
  expect_equal(learned, person_fit$posterior$P_gk)
  expect_gt(cor(learned, as.numeric(sim$learned)), 0.8)
})

test_that("person/item model omits structural pairs and rejects DK", {
  sim <- simulate_lca(n = 800, n_items = 3, seed = 944)
  pre <- sim$pre
  post <- sim$post
  pre[1:100, 1] <- NA
  post[51:150, 2] <- NA

  fit <- person_item_lca_fit(
    pre, post,
    na_as = "missing"
  )
  expected_pairs <- sum(!is.na(pre) & !is.na(post))

  expect_equal(fit$n_obs, expected_pairs)
  expect_true(fit$converged)
  expect_error(
    person_item_lca_fit(
      data.frame(i = c("d", "1")),
      data.frame(i = c("1", "1"))
    ),
    "only the no-DK model"
  )
})

test_that("person/item fit has useful print and coefficient methods", {
  sim <- simulate_lca(n = 400, n_items = 2, seed = 945)
  fit <- person_item_lca_fit(sim$pre, sim$post)

  expect_output(print(fit), "Joint Person-Level LCA Fit")
  expect_equal(
    names(coef(fit)),
    c("gg", "gk", "kk", "gamma_item1", "gamma_item2")
  )
})

test_that("person/item fit preserves a single item name", {
  sim <- simulate_lca(n = 500, n_items = 1, seed = 947)
  fit <- person_item_lca_fit(sim$pre, sim$post)

  expect_named(fit$gamma, "item1")
  expect_equal(nrow(fit$posterior), 500L)
})

test_that("person/item fit aligns item parameters by name", {
  sim <- simulate_lca(
    n = 1500, n_items = 4,
    gamma = c(0.15, 0.25, 0.35, 0.45), seed = 946
  )
  original <- person_item_lca_fit(sim$pre, sim$post)
  order <- c(4L, 2L, 1L, 3L)
  permuted <- person_item_lca_fit(
    sim$pre[order], sim$post[order]
  )

  expect_equal(
    original$gamma,
    permuted$gamma[names(original$gamma)],
    tolerance = 1e-8
  )
  expect_equal(
    original$class_priors,
    permuted$class_priors,
    tolerance = 1e-8
  )

  item_fit <- item_lca_fit(sim$pre, sim$post)
  colnames(item_fit$params) <- paste0("wrong", seq_len(ncol(item_fit$params)))
  expect_error(
    person_item_lca_fit(sim$pre, sim$post, item_fit = item_fit),
    "item names must match"
  )
})
