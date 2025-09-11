# Deprecated - use fit_model instead
# This function is provided for backward compatibility only

fit_dk <- function(pre_test, pst_test, g, est.param, force9 = FALSE) {

  data   <- multi_transmat(pre_test, pst_test, force9 = force9)
  data   <- data[(1:nrow(data) - 1), ] # remove the agg.
  expec  <- matrix(ncol = nrow(data), nrow = 9)
  fit    <- matrix(ncol = nrow(data), nrow = 2)
  colnames(fit) <- rownames(data)
  rownames(fit) <- c("chi-square", "p-value")

  for (i in 1:nrow(data)) {

    gi      <- g[[i]]
    expec[1, i]  <- (1 - gi) * (1 - gi) * est.param[1, i] * sum(data[i, ])
    expec[2, i]  <- ( (1 - gi) * gi * est.param[1, i] +
                    (1 - gi) * est.param[2, i]) * sum(data[i, ])
    expec[3, i]  <- ( (1 - gi) * est.param[3, i]) * sum(data[i, ])
    expec[4, i]  <- ( (1 - gi) * gi * est.param[1, i]) * sum(data[i, ])
    expec[5, i]  <- (gi * gi * est.param[1, i] + gi * est.param[2, i] +
                    est.param[4, i]) * sum(data[i, ])
    expec[6, i]  <- (gi * est.param[3, i]) * sum(data[i, ])
    expec[7, i]  <- ( (1 - gi) * est.param[5, i]) * sum(data[i, ])
    expec[8, i]  <- (gi * est.param[5, i] + est.param[6, i]) * sum(data[i, ])
    expec[9, i]  <- est.param[7, i] * sum(data[i, ])
    test     <- suppressWarnings(chisq.test(expec[, i],
                                            p = data[i, ] / sum(data[i, ])))
    fit[1:2, i]  <- round(unlist(test[c(1, 3)]), 3)
  }
  fit
}
