## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(guess)
set.seed(42)

## ----workflow_data------------------------------------------------------------
pre_test <- data.frame(
  item1 = c(1, 0, 0, 1, 0, 1, 0, 0, 1, 0),
  item2 = c(0, 0, 1, 1, 0, 0, 1, 0, 1, 0),
  item3 = c(1, 1, 0, 1, 0, 0, 0, 1, 1, 0)
)

post_test <- data.frame(
  item1 = c(1, 1, 0, 1, 1, 1, 0, 1, 1, 0),
  item2 = c(1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
  item3 = c(1, 1, 1, 1, 0, 1, 0, 1, 1, 0)
)

## ----workflow_naive-----------------------------------------------------------
naive_learning <- colMeans(post_test) - colMeans(pre_test)
cat("Naive learning estimates (biased downward):\n")
print(round(naive_learning, 3))
cat(sprintf("\nMean naive learning: %.3f\n", mean(naive_learning)))

## ----workflow_fit-------------------------------------------------------------
fit <- lca_fit(pre_test, post_test)
print(fit)

## ----workflow_interpret-------------------------------------------------------
summary(fit)

cat("\nComparison: Naive vs. LCA-adjusted learning:\n")
comparison <- data.frame(
  Item = colnames(pre_test),
  Naive = naive_learning,
  LCA_Adjusted = fit$learning,
  Difference = fit$learning - naive_learning
)
print(comparison, row.names = FALSE)

## ----workflow_se, eval = FALSE------------------------------------------------
# se_results <- lca_se(pre_test, post_test, n_boot = 100)

## ----workflow_diagnostics-----------------------------------------------------
fit_stats <- fit_model(pre_test, post_test,
                       fit$params["gamma", ],
                       fit$params[c("gg", "gk", "kk"), ])
print(fit_stats)

## ----workflow_groups----------------------------------------------------------
group <- c(rep("treatment", 5), rep("control", 5))

pre_treat <- pre_test[group == "treatment", ]
post_treat <- post_test[group == "treatment", ]
pre_ctrl <- pre_test[group == "control", ]
post_ctrl <- post_test[group == "control", ]

fit_treat <- lca_fit(pre_treat, post_treat)
fit_ctrl <- lca_fit(pre_ctrl, post_ctrl)

cat("Treatment group learning:", round(mean(fit_treat$learning), 3), "\n")
cat("Control group learning:", round(mean(fit_ctrl$learning), 3), "\n")
cat("Difference:", round(mean(fit_treat$learning) - mean(fit_ctrl$learning), 3), "\n")

## ----worked_example-----------------------------------------------------------
gg <- 0.35
gk <- 0.30
kk <- 0.35
gamma <- 0.25

p00 <- (1 - gamma)^2 * gg
p01 <- (1 - gamma) * gamma * gg + (1 - gamma) * gk
p10 <- (1 - gamma) * gamma * gg
p11 <- gamma^2 * gg + gamma * gk + kk

cat("Cell probabilities:\n")
cat(sprintf("  P(0→0) = %.4f\n", p00))
cat(sprintf("  P(0→1) = %.4f\n", p01))
cat(sprintf("  P(1→0) = %.4f\n", p10))
cat(sprintf("  P(1→1) = %.4f\n", p11))
cat(sprintf("  Sum    = %.4f\n", p00 + p01 + p10 + p11))

## ----verify_implementation----------------------------------------------------
guess_lik_manual <- function(gg, gk, kk, gamma, data) {
  vec <- numeric(4)
  vec[1] <- (1 - gamma) * (1 - gamma) * gg        # P(0→0)
  vec[2] <- (1 - gamma) * gamma * gg + (1 - gamma) * gk  # P(0→1)
  vec[3] <- (1 - gamma) * gamma * gg              # P(1→0)
  vec[4] <- gamma * gamma * gg + gamma * gk + kk  # P(1→1)

  -sum(data * log(vec))
}

test_data <- c(100, 150, 50, 200)

ll_manual <- guess_lik_manual(0.35, 0.30, 0.35, 0.25, test_data)

cat(sprintf("Manual implementation: %.4f\n", ll_manual))

## ----param_recovery_demo------------------------------------------------------
true_params <- c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25)

sim <- simulate_lca(
  n = 1000,
  n_items = 5,
  gg = true_params["gg"],
  gk = true_params["gk"],
  kk = true_params["kk"],
  gamma = true_params["gamma"],
  seed = 123
)

fit <- lca_fit(sim$pre, sim$post)

estimated <- c(
  gg = mean(fit$params["gg", ]),
  gk = mean(fit$params["gk", ]),
  kk = mean(fit$params["kk", ]),
  gamma = mean(fit$params["gamma", ])
)

comparison <- rbind(
  true = true_params,
  estimated = estimated,
  difference = estimated - true_params
)

knitr::kable(comparison, digits = 3,
             caption = "Parameter Recovery: True vs. Estimated")

## ----monte_carlo_setup--------------------------------------------------------
n_sims <- 100
n <- 500
n_items <- 2
true_params <- c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25)

set.seed(789)
estimates <- matrix(NA, nrow = n_sims, ncol = 4)
colnames(estimates) <- names(true_params)

for (sim in seq_len(n_sims)) {
  sim_data <- simulate_lca(
    n = n, n_items = n_items,
    gg = true_params["gg"], gk = true_params["gk"],
    kk = true_params["kk"], gamma = true_params["gamma"]
  )

  tryCatch({
    fit <- lca_fit(sim_data$pre, sim_data$post)
    estimates[sim, ] <- c(
      mean(fit$params["gg", ]),
      mean(fit$params["gk", ]),
      mean(fit$params["kk", ]),
      mean(fit$params["gamma", ])
    )
  }, error = function(e) NULL)
}

## ----bias_assessment----------------------------------------------------------
mean_estimates <- colMeans(estimates, na.rm = TRUE)
bias <- mean_estimates - true_params
rel_bias <- 100 * bias / true_params

bias_table <- data.frame(
  Parameter = names(true_params),
  True = true_params,
  Mean_Estimate = mean_estimates,
  Bias = bias,
  Relative_Bias_Pct = rel_bias
)

knitr::kable(bias_table, digits = 4, row.names = FALSE,
             caption = "Bias Assessment from Monte Carlo Simulation")

## ----se_assessment------------------------------------------------------------
se_estimates <- apply(estimates, 2, sd, na.rm = TRUE)
rmse <- sqrt(colMeans((estimates - matrix(true_params, nrow = n_sims,
                                           ncol = 4, byrow = TRUE))^2,
                       na.rm = TRUE))

se_table <- data.frame(
  Parameter = names(true_params),
  SE = se_estimates,
  RMSE = rmse
)

knitr::kable(se_table, digits = 4, row.names = FALSE,
             caption = "Standard Errors from Monte Carlo Simulation")

## ----coverage_assessment------------------------------------------------------
coverage <- numeric(4)
for (j in 1:4) {
  ci_lower <- estimates[, j] - 1.96 * se_estimates[j]
  ci_upper <- estimates[, j] + 1.96 * se_estimates[j]
  coverage[j] <- mean(true_params[j] >= ci_lower &
                        true_params[j] <= ci_upper, na.rm = TRUE)
}

coverage_table <- data.frame(
  Parameter = names(true_params),
  Coverage_95 = coverage
)

knitr::kable(coverage_table, digits = 3, row.names = FALSE,
             caption = "95% CI Coverage from Monte Carlo Simulation")

## ----visualization, fig.cap = "Distribution of gk (learning) estimates across simulations"----
hist(estimates[, "gk"], breaks = 20,
     main = "Distribution of Learning (gk) Estimates",
     xlab = "Estimated gk", col = "lightblue", border = "white")
abline(v = true_params["gk"], col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("True value"), col = "red", lty = 2, lwd = 2)

## ----sample_size_effects------------------------------------------------------
sample_sizes <- c(100, 250, 500, 1000)
true_params <- c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25)
n_sims_quick <- 50

set.seed(456)
rmse_by_n <- matrix(NA, nrow = length(sample_sizes), ncol = 4)
colnames(rmse_by_n) <- names(true_params)

for (s in seq_along(sample_sizes)) {
  n <- sample_sizes[s]
  estimates_n <- matrix(NA, nrow = n_sims_quick, ncol = 4)

  for (sim in seq_len(n_sims_quick)) {
    sim_data <- simulate_lca(
      n = n, n_items = 2,
      gg = true_params["gg"], gk = true_params["gk"],
      kk = true_params["kk"], gamma = true_params["gamma"]
    )

    tryCatch({
      fit <- lca_fit(sim_data$pre, sim_data$post)
      estimates_n[sim, ] <- c(
        mean(fit$params["gg", ]),
        mean(fit$params["gk", ]),
        mean(fit$params["kk", ]),
        mean(fit$params["gamma", ])
      )
    }, error = function(e) NULL)
  }

  rmse_by_n[s, ] <- sqrt(colMeans((estimates_n - matrix(true_params,
                                                         nrow = n_sims_quick,
                                                         ncol = 4,
                                                         byrow = TRUE))^2,
                                   na.rm = TRUE))
}

sample_size_table <- data.frame(
  n = sample_sizes,
  RMSE_gk = rmse_by_n[, "gk"],
  RMSE_ratio = c(NA, rmse_by_n[-nrow(rmse_by_n), "gk"] / rmse_by_n[-1, "gk"])
)

knitr::kable(sample_size_table, digits = 3, row.names = FALSE,
             caption = "RMSE of gk by Sample Size (ratio should be ~sqrt(2) for doubling n)")

