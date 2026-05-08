#' Preprocess Clinical Data
#'
#' Applies log transformation to skewed features and standardizes.
#'
#' @param data Raw clinical data frame.
#' @param features Character vector of feature names.
#' @return Preprocessed data frame.
#'
#' @export
#'
#' @examples
#' data <- simulate_clinical_data(n = 200)
#' processed <- preprocess_clinical_data(data)
#' head(processed)
preprocess_clinical_data <- function(data, features = NULL) {
  
  if (is.null(features)) {
    features <- c("age", "creatinine", "lactate", "wbc", "bilirubin",
                  "heart_rate", "map", "resp_rate", "temperature", "spo2")
  }
  
  features <- features[features %in% names(data)]
  
  X <- data[, features, drop = FALSE]
  
  skewed_vars <- c("creatinine", "lactate", "wbc", "bilirubin")
  for (var in intersect(skewed_vars, features)) {
    X[[var]] <- log1p(X[[var]])
  }
  
  X_mat <- as.matrix(X)
  X_scaled <- scale(X_mat, center = TRUE, scale = TRUE)
  
  result <- as.data.frame(X_scaled)
  result$outcome <- data$outcome
  
  result
}
