#' Energy Distance
#'
#' Computes the energy distance between two samples.
#'
#' @param X_train Training data matrix (n_tr x p).
#' @param X_test Test data matrix (n_te x p).
#' @param subsample_size Integer for subsampling large datasets.
#' @return Energy distance value.
#'
#' @export
#' @import energy
#'
#' @examples
#' X_train <- matrix(rnorm(1000), ncol = 5)
#' X_test <- matrix(rnorm(500), ncol = 5)
#' edist <- energy_distance(X_train, X_test)
#' print(edist)
energy_distance <- function(X_train, X_test, subsample_size = NULL) {
  
  X_train <- as.matrix(X_train)
  X_test <- as.matrix(X_test)
  
  n_tr <- nrow(X_train)
  n_te <- nrow(X_test)
  
  if (!is.null(subsample_size) && subsample_size < min(n_tr, n_te)) {
    if (subsample_size < n_tr) {
      X_train <- X_train[sample(n_tr, subsample_size), ]
      n_tr <- subsample_size
    }
    if (subsample_size < n_te) {
      X_test <- X_test[sample(n_te, subsample_size), ]
      n_te <- subsample_size
    }
  }
  
  energy::eqdist.etest(
    rbind(X_train, X_test),
    sizes = c(n_tr, n_te),
    R = 0
  )$statistic
}
