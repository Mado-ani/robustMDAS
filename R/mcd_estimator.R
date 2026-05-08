#' Minimum Covariance Determinant (MCD) Estimator
#'
#' Computes the MCD estimator using the FAST-MCD algorithm.
#'
#' @param X Data matrix (n x p).
#' @param alpha Coverage parameter (0.5 to 1). Controls the breakdown point.
#' @return List containing center, cov, weights, and breakdown point.
#'
#' @export
#' @import robustbase
#'
#' @examples
#' X <- matrix(rnorm(1000), ncol = 5)
#' result <- mcd_estimator(X, alpha = 0.75)
#' print(result$center)
mcd_estimator <- function(X, alpha = 0.75) {
  
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  if (alpha <= 0.5 || alpha >= 1) {
    stop("alpha must be between 0.5 and 1")
  }
  
  h <- floor(alpha * n)
  
  mcd_result <- robustbase::covMcd(
    X,
    alpha = alpha,
    nsamp = "best",
    maxcsteps = 200
  )
  
  list(
    center = as.vector(mcd_result$center),
    cov = mcd_result$cov,
    weights = mcd_result$mcd.wt,
    best = mcd_result$best,
    alpha = alpha,
    h = h,
    breakdown = 1 - alpha
  )
}
