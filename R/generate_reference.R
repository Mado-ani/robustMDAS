#' Generate Monte Carlo Reference Distribution
#'
#' @param X_full Full data matrix
#' @param split_ratio Training proportion
#' @param n_replications Number of replications
#' @param alpha_mcd MCD coverage parameter
#' @param n_cores Number of cores
#' @param verbose Print progress
#' @return Data frame with reference values
#' @export
generate_reference <- function(
    X_full,
    split_ratio = 0.8,
    n_replications = 500,
    alpha_mcd = 0.75,
    n_cores = 1,
    verbose = TRUE
) {
  X_full <- as.matrix(X_full)
  n_total <- nrow(X_full)
  n_train <- floor(n_total * split_ratio)
  measures <- c("mdas", "energy", "mmd")
  
  if (n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    `%dopar%` <- foreach::`%dopar%`
  } else {
    `%dopar%` <- foreach::`%do%`
  }
  
  results <- foreach::foreach(
    rep = 1:n_replications,
    .combine = rbind,
    .packages = c("robustbase", "energy", "MASS")
  ) %dopar% {
    idx_train <- sample(n_total, n_train)
    X_tr <- X_full[idx_train, ]
    X_te <- X_full[-idx_train, ]
    c(
      mdas = robust_mdas(X_tr, X_te, alpha = alpha_mcd)$lambda,
      energy = energy_distance(X_tr, X_te),
      mmd = mmd(X_tr, X_te)
    )
  }
  
  if (n_cores > 1) parallel::stopCluster(cl)
  
  results_df <- as.data.frame(results)
  colnames(results_df) <- measures
  na.omit(results_df)
}
