#' Sensitivity Analysis for MCD Coverage Parameter
#'
#' @param X_train Training data matrix.
#' @param X_test Test data matrix.
#' @param alpha_values Vector of coverage parameters to test.
#' @param n_replications Monte Carlo replications.
#' @return Data frame with results for each alpha.
#'
#' @export
sensitivity_mcd_alpha <- function(
    X_train,
    X_test,
    alpha_values = c(0.5, 0.6, 0.75, 0.85, 0.9),
    n_replications = 200
) {
  
  results <- data.frame()
  
  for (alpha in alpha_values) {
    cat("Testing alpha =", alpha, "\n")
    
    mdas_res <- robust_mdas(X_train, X_test, alpha = alpha, return_components = TRUE)
    
    X_full <- rbind(X_train, X_test)
    n_total <- nrow(X_full)
    n_tr <- nrow(X_train)
    
    ref_values <- numeric(n_replications)
    for (i in 1:n_replications) {
      idx_train <- sample(n_total, n_tr)
      ref_mdas <- robust_mdas(X_full[idx_train, ], X_full[-idx_train, ], alpha = alpha)
      ref_values[i] <- ref_mdas$lambda
    }
    
    p_value <- empirical_p_value(mdas_res$lambda, ref_values)
    
    results <- rbind(results, data.frame(
      alpha = alpha,
      breakdown = 1 - alpha,
      observed_mdas = mdas_res$lambda,
      mean_ref = mean(ref_values),
      sd_ref = sd(ref_values),
      p_value = p_value,
      reject = p_value < 0.05,
      mean_component = mdas_res$mean_component,
      cov_component = mdas_res$cov_component
    ))
  }
  
  results
}
