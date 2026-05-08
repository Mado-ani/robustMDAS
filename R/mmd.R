#' Maximum Mean Discrepancy (MMD)
#'
#' Computes the unbiased empirical MMD with Gaussian kernel.
#'
#' @param X_train Training data matrix (n_tr x p).
#' @param X_test Test data matrix (n_te x p).
#' @param sigma Bandwidth parameter (NULL for median heuristic).
#' @param subsample_size Integer for subsampling large datasets.
#' @return MMD^2 value.
#'
#' @export
#' @importFrom stats dist median
#'
#' @examples
#' X_train <- matrix(rnorm(1000), ncol = 5)
#' X_test <- matrix(rnorm(500), ncol = 5)
#' mmd_val <- mmd(X_train, X_test)
#' print(mmd_val)
mmd <- function(X_train, X_test, sigma = NULL, subsample_size = NULL) {
  
  X_train <- as.matrix(X_train)
  X_test <- as.matrix(X_test)
  
  n_tr <- nrow(X_train)
  n_te <- nrow(X_test)
  
  if (!is.null(subsample_size) && subsample_size < min(n_tr, n_te)) {
    if (subsample_size < n_tr) {
      idx_tr <- sample(n_tr, subsample_size)
      X_train <- X_train[idx_tr, ]
      n_tr <- subsample_size
    }
    if (subsample_size < n_te) {
      idx_te <- sample(n_te, subsample_size)
      X_test <- X_test[idx_te, ]
      n_te <- subsample_size
    }
  }
  
  X_all <- rbind(X_train, X_test)
  dist_mat <- as.matrix(dist(X_all))
  
  if (is.null(sigma)) {
    upper_tri <- dist_mat[upper.tri(dist_mat)]
    sigma <- median(upper_tri, na.rm = TRUE)
    if (sigma == 0) sigma <- 1
  }
  
  kernel_mat <- exp(-dist_mat^2 / (2 * sigma^2))
  
  term1 <- (sum(kernel_mat[1:n_tr, 1:n_tr]) - sum(diag(kernel_mat[1:n_tr, 1:n_tr]))) / 
           (n_tr * (n_tr - 1))
  term2 <- 2 * sum(kernel_mat[1:n_tr, (n_tr + 1):(n_tr + n_te)]) / (n_tr * n_te)
  term3 <- (sum(kernel_mat[(n_tr + 1):(n_tr + n_te), (n_tr + 1):(n_tr + n_te)]) - 
            sum(diag(kernel_mat[(n_tr + 1):(n_tr + n_te), (n_tr + 1):(n_tr + n_te)]))) /
           (n_te * (n_te - 1))
  
  return(max(0, term1 - term2 + term3))
}
