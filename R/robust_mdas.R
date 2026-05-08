#' Robust Mahalanobis Distribution Alignment Score (MDAS)
#'
#' Computes the robust version of MDAS using MCD-based covariance estimation.
#'
#' @param X_train Training data matrix (n_tr x p).
#' @param X_test Test data matrix (n_te x p).
#' @param alpha MCD coverage parameter (default 0.75).
#' @param return_components Logical, return decomposed components.
#' @return List containing lambda and optional components.
#'
#' @export
#'
#' @examples
#' n_tr <- 200; n_te <- 80; p <- 5
#' X_train <- matrix(rnorm(n_tr * p), ncol = p)
#' X_test <- matrix(rnorm(n_te * p), ncol = p)
#' result <- robust_mdas(X_train, X_test)
#' print(result$lambda)
robust_mdas <- function(X_train, X_test, alpha = 0.75, return_components = FALSE) {
  
  X_train <- as.matrix(X_train)
  X_test <- as.matrix(X_test)
  
  n_tr <- nrow(X_train)
  n_te <- nrow(X_test)
  p <- ncol(X_train)
  
  if (ncol(X_test) != p) {
    stop("Training and test data must have same number of features")
  }
  
  # MCD estimates
  mcd_tr <- mcd_estimator(X_train, alpha = alpha)
  mcd_te <- mcd_estimator(X_test, alpha = alpha)
  
  mu_tr_rob <- mcd_tr$center
  mu_te_rob <- mcd_te$center
  Sigma_tr_rob <- mcd_tr$cov
  Sigma_te_rob <- mcd_te$cov
  
  # Robust pooled covariance
  Sigma_pooled_rob <- ((n_tr - 1) * Sigma_tr_rob + 
                        (n_te - 1) * Sigma_te_rob) / (n_tr + n_te - 2)
  
  # Regularize if necessary
  eigenvals <- eigen(Sigma_pooled_rob, symmetric = TRUE, only.values = TRUE)$values
  if (min(eigenvals) < 1e-8) {
    Sigma_pooled_rob <- Sigma_pooled_rob + diag(1e-6, p)
  }
  
  Sigma_inv <- solve(Sigma_pooled_rob)
  
  # Delta XY
  centered_tr <- sweep(X_train, 2, mu_te_rob)
  mahal_dist_tr <- apply(centered_tr, 1, function(x) as.numeric(t(x) %*% Sigma_inv %*% x))
  delta_XY <- mean(mahal_dist_tr)
  
  # Delta YX
  centered_te <- sweep(X_test, 2, mu_tr_rob)
  mahal_dist_te <- apply(centered_te, 1, function(x) as.numeric(t(x) %*% Sigma_inv %*% x))
  delta_YX <- mean(mahal_dist_te)
  
  lambda <- (delta_XY + delta_YX) / 2
  
  if (return_components) {
    mean_diff <- mu_tr_rob - mu_te_rob
    mean_component <- as.numeric(t(mean_diff) %*% Sigma_inv %*% mean_diff)
    
    trace_tr <- sum(diag(Sigma_inv %*% Sigma_tr_rob))
    trace_te <- sum(diag(Sigma_inv %*% Sigma_te_rob))
    cov_component <- 0.5 * (((n_tr - 1) / n_tr) * trace_tr + 
                              ((n_te - 1) / n_te) * trace_te)
    
    return(list(
      lambda = lambda,
      mean_component = mean_component,
      cov_component = cov_component,
      mcd_alpha = alpha,
      breakdown = 1 - alpha
    ))
  }
  
  list(lambda = lambda, mcd_alpha = alpha, breakdown = 1 - alpha)
}
