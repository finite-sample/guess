# Deprecated - use fit_model instead  
# This function is provided for backward compatibility only

fit_nodk <- function(pre_test, pst_test, g, est_param) {

  data    <- multi_transmat(pre_test, pst_test)
  data    <- data[seq_len(nrow(data) - 1), , drop = FALSE]
  expec  <- matrix(ncol = nrow(data), nrow = 4)
  fit    <- matrix(ncol = nrow(data), nrow = 2)
  colnames(fit) <- rownames(data)
  rownames(fit) <- c("chi-square", "p-value")

  for (i in seq_len(nrow(data))) {

    gi      <- g[[i]]
    expec[1, i]  <- (1 - gi) * (1 - gi) * est_param[1, i] * sum(data[i, ])
    expec[2, i]  <- ((1 - gi) * gi * est_param[1, i] +
                       (1 - gi) * est_param[2, i]) * sum(data[i, ])
    expec[3, i]  <- ((1 - gi) * est_param[3, i] * est_param[1, i] *
                       sum(data[i, ]))
    expec[4, i]  <- (gi * gi * est_param[1, i] + gi * est_param[2, i] +
                       est_param[3, i]) * sum(data[i, ])
    test     <- suppressWarnings(chisq.test(expec[, i],
                                            p = data[i, ] / sum(data[i, ])))
    fit[1:2, i]  <- unlist(test[c(1, 3)])
  }

  fit
}
