#' Robust Multi-Measure Split Diagnostic (Algorithm 1)
#'
#' Assesses the quality of a train-test split by comparing the observed
#' distributional discrepancy to a Monte Carlo reference distribution.
#'
#' @param X_train Training data matrix (n_tr x p).
#' @param X_test Test data matrix (n_te x p).
#' @param alpha Significance level (default 0.05).
#' @param n_replications Number of Monte Carlo replications (default 500).
#' @param alpha_mcd MCD coverage parameter (default 0.75).
#' @param n_cores Number of cores for parallel computation.
#' @param return_details Logical, return full reference distribution.
#' @param verbose Logical, print progress messages.
#' @return List containing diagnostic results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' X_train <- matrix(rnorm(500), ncol = 5)
#' X_test <- matrix(rnorm(200), ncol = 5)
#' result <- robust_diagnostic(X_train, X_test)
#' print(result$p_combined)
#' }
robust_diagnostic <- function(
    X_train,
    X_test,
    alpha = 0.05,
    n_replications = 500,
    alpha_mcd = 0.75,
    n_cores = 1,
    return_details = FALSE,
    verbose = TRUE
) {
  
  # Input validation
  X_train <- as.matrix(X_train)
  X_test <- as.matrix(X_test)
  
  if (ncol(X_train) != ncol(X_test)) {
    stop("Training and test data must have same number of features")
  }
  
  n_tr <- nrow(X_train)
  n_te <- nrow(X_test)
  split_ratio <- n_tr / (n_tr + n_te)
  
  if (verbose) cat("Step 1: Computing observed statistics...\n")
  
  # Observed statistics
  mdas_res <- robust_mdas(X_train, X_test, alpha = alpha_mcd, return_components = TRUE)
  energy_obs <- energy_distance(X_train, X_test)
  mmd_obs <- mmd(X_train, X_test)
  ks_res <- ks_battery(X_train, X_test)
  
  observed_stats <- list(
    mdas = mdas_res$lambda,
    energy = energy_obs,
    mmd = mmd_obs,
    mean_component = mdas_res$mean_component,
    cov_component = mdas_res$cov_component
  )
  
  if (verbose) cat("Step 2: Reconstructing full dataset...\n")
  X_full <- rbind(X_train, X_test)
  
  if (verbose) cat("Step 3: Generating Monte Carlo reference distribution (", 
                   n_replications, " replications)...\n")
  ref_dist <- generate_reference(
    X_full = X_full,
    split_ratio = split_ratio,
    n_replications = n_replications,
    alpha_mcd = alpha_mcd,
    n_cores = n_cores,
    verbose = verbose
  )
  
  if (verbose) cat("Step 4: Computing empirical p-values...\n")
  
  p_values <- c(
    mdas = empirical_p_value(observed_stats$mdas, ref_dist$mdas),
    energy = empirical_p_value(observed_stats$energy, ref_dist$energy),
    mmd = empirical_p_value(observed_stats$mmd, ref_dist$mmd),
    ks = min(ks_res$p_adj)
  )
  
  if (verbose) cat("Step 5: Cauchy combination test...\n")
  p_combined <- cauchy_combination(p_values, continuity_correction = TRUE)
  
  reject_null <- p_combined <= alpha
  
  results <- list(
    reject_null = reject_null,
    p_combined = p_combined,
    p_values = p_values,
    observed_stats = observed_stats,
    ks_diagnosis = list(
      feature_max = ks_res$feature_max,
      ks_stat = ks_res$max_stat,
      p_adj = ks_res$p_adj[ks_res$feature_max],
      all_p_adj = ks_res$p_adj
    ),
    n_replications = n_replications,
    mcd_alpha = alpha_mcd,
    breakdown = 1 - alpha_mcd
  )
  
  if (return_details) {
    results$reference_distribution <- ref_dist
  }
  
  if (verbose) {
    cat("\n========== Diagnostic Summary ==========\n")
    cat("Combined p-value:", round(p_combined, 4), "\n")
    cat("Decision:", ifelse(reject_null, "REJECT H0 - Split INADEQUATE", 
                            "ACCEPT H0 - Split ADEQUATE"), "\n")
    cat("\nIndividual p-values:\n")
    for (name in names(p_values)) {
      cat("  ", name, ":", round(p_values[name], 4), "\n")
    }
    if (reject_null && !is.null(ks_res$feature_max)) {
      cat("\nProblematic feature:", ks_res$feature_max,
          "(adj. p =", sprintf("%.2e", min(ks_res$p_adj)), ")\n")
    }
    cat("========================================\n")
  }
  
  results
}
