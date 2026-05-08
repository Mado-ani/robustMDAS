#' Bonferroni-Adjusted Kolmogorov-Smirnov Battery
#'
#' @param X_train Training data
#' @param X_test Test data
#' @param feature_names Optional feature names
#' @return List with KS statistics and p-values
#' @export
ks_battery <- function(X_train, X_test, feature_names = NULL) {
  X_train <- as.matrix(X_train)
  X_test <- as.matrix(X_test)
  p <- ncol(X_train)
  
  if (is.null(feature_names)) {
    feature_names <- paste0("Feature_", 1:p)
  }
  
  ks_stats <- numeric(p)
  p_values <- numeric(p)
  
  for (j in 1:p) {
    test <- suppressWarnings(ks.test(X_train[, j], X_test[, j], exact = FALSE))
    ks_stats[j] <- test$statistic
    p_values[j] <- test$p.value
  }
  
  p_adj <- pmin(1, p_values * p)
  names(ks_stats) <- names(p_values) <- names(p_adj) <- feature_names
  
  list(
    ks_stats = ks_stats,
    p_values = p_values,
    p_adj = p_adj,
    max_stat = max(ks_stats),
    feature_max = feature_names[which.max(ks_stats)],
    reject_null = any(p_adj < 0.05)
  )
}
