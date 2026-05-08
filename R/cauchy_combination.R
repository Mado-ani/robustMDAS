
#' Empirical P-value from Monte Carlo Reference Distribution
#'
#' Computes the empirical p-value by comparing an observed statistic
#' against a reference distribution.
#'
#' @param obs_value Numeric, the observed test statistic.
#' @param ref_values Numeric vector, the reference distribution values.
#' @return Empirical p-value.
#'
#' @examples
#' obs <- 2.5
#' ref <- rnorm(100)
#' p <- empirical_p_value(obs, ref)
#' print(p)
empirical_p_value <- function(obs_value, ref_values) {
  N <- length(ref_values)
  (1 + sum(ref_values >= obs_value, na.rm = TRUE)) / (N + 1)
}

#' Cauchy Combination Test
#'
#' Combines multiple p-values using the Cauchy combination test,
#' valid under arbitrary dependence.
#'
#' @param p_values Vector of p-values to combine.
#' @param weights Optional weights for each component.
#' @param continuity_correction Logical, apply continuity correction.
#' @return Combined p-value.
#'
#' @export
#'
#' @examples
#' p_vals <- c(0.03, 0.12, 0.08, 0.01)
#' combined <- cauchy_combination(p_vals)
#' print(combined)
cauchy_combination <- function(p_values, weights = NULL, continuity_correction = TRUE) {
  
  # Clip extremes to avoid numerical issues
  p_values <- pmin(pmax(p_values, 1e-16), 1 - 1e-16)
  
  k <- length(p_values)
  
  if (is.null(weights)) {
    weights <- rep(1/k, k)
  } else {
    weights <- weights / sum(weights)
  }
  
  # Apply continuity correction if requested
  if (continuity_correction) {
    p_values <- p_values - 1e-8
    p_values <- pmax(p_values, 1e-16)
  }
  
  tan_values <- tan((0.5 - p_values) * pi)
  T_cct <- sum(weights * tan_values)
  p_combined <- 0.5 - atan(T_cct) / pi
  p_combined <- min(max(p_combined, 0), 1)
  
  p_combined
}

