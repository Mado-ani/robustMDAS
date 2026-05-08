#' Benchmark Computational Performance
#'
#' @param sample_sizes Vector of sample sizes to test
#' @param n_replications Monte Carlo replications
#' @param n_simulations Number of simulations per size
#' @return Data frame with timing results
#' @export
benchmark_performance <- function(
    sample_sizes = c(100, 200, 500, 1000),
    n_replications = 100,
    n_simulations = 3
) {
  results <- data.frame()
  
  for (n in sample_sizes) {
    cat("Benchmarking n =", n, "\n")
    
    for (sim in 1:n_simulations) {
      data <- simulate_clinical_data(n = n, task = "sepsis", seed = sim)
      data <- preprocess_clinical_data(data)
      
      feature_cols <- names(data)[!names(data) %in% "outcome"]
      X <- as.matrix(data[, feature_cols])
      
      n_total <- nrow(X)
      n_train <- floor(0.8 * n_total)
      train_idx <- sample(n_total, n_train)
      
      start_time <- Sys.time()
      result <- robust_diagnostic(
        X[train_idx, ],
        X[-train_idx, ],
        n_replications = n_replications,
        verbose = FALSE
      )
      end_time <- Sys.time()
      
      results <- rbind(results, data.frame(
        n_patients = n,
        simulation = sim,
        time_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
        p_value = result$p_combined,
        mdas = result$observed_stats$mdas
      ))
    }
  }
  
  results
}
