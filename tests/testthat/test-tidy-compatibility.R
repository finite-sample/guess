test_that("tibbles produce the same transitions and fits as data frames", {
  skip_if_not_installed("tibble")

  sim <- simulate_lca(n = 600, n_items = 2, seed = 921)
  pre_tbl <- tibble::as_tibble(sim$pre)
  post_tbl <- tibble::as_tibble(sim$post)

  expect_equal(
    count_item_transitions(pre_tbl, post_tbl),
    count_item_transitions(sim$pre, sim$post)
  )
  expect_equal(
    fit_item_lca(pre_tbl, post_tbl)$params,
    fit_item_lca(sim$pre, sim$post)$params
  )
  expect_equal(
    stnd_cor(pre_tbl, post_tbl, guessing_probability = rep(0.25, 2)),
    stnd_cor(sim$pre, sim$post, guessing_probability = rep(0.25, 2))
  )
})

test_that("dplyr-selected tibbles work in a native pipe workflow", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")

  sim <- simulate_lca(n = 700, n_items = 2, seed = 922)
  wide <- tibble::tibble(
    id = seq_len(nrow(sim$pre)),
    pre_item1 = sim$pre$item1,
    pre_item2 = sim$pre$item2,
    post_item1 = sim$post$item1,
    post_item2 = sim$post$item2
  )

  pre <- wide |>
    dplyr::select(dplyr::starts_with("pre_")) |>
    dplyr::rename_with(function(x) sub("^pre_", "", x))
  post <- wide |>
    dplyr::select(dplyr::starts_with("post_")) |>
    dplyr::rename_with(function(x) sub("^post_", "", x))

  fit <- pre |>
    fit_item_lca(post_test = post)

  expect_s3_class(pre, "tbl_df")
  expect_s3_class(fit, "guess_fit")
  expect_equal(ncol(fit$params), 2L)
  expect_true(all(is.finite(fit$learning)))
})

test_that("tibbles preserve the missing-response contract", {
  skip_if_not_installed("tibble")

  pre <- tibble::tibble(i1 = c(1, NA, 0), i2 = c("d", "1", "0"))
  post <- tibble::tibble(i1 = c(1, 1, 0), i2 = c("1", "1", "0"))

  default <- count_item_transitions(pre, post)
  omitted <- count_item_transitions(pre, post, na_as = "missing")

  expect_equal(ncol(default), 9L)
  expect_equal(rowSums(default), c(i1 = 3, i2 = 3))
  expect_equal(rowSums(omitted), c(i1 = 2, i2 = 3))
})
