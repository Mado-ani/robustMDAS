#' Simulate Clinical Data
#'
#' Generates synthetic clinical data mimicking MIMIC-IV statistical properties.
#'
#' @param n Number of patients.
#' @param task Clinical task: "sepsis", "aki", "los".
#' @param contamination_rate Proportion of outliers (0 to 0.25).
#' @param seed Random seed.
#' @return Data frame with clinical features and outcome.
#'
#' @export
#' @import MASS
#'
#' @examples
#' data <- simulate_clinical_data(n = 500, task = "sepsis")
#' head(data)
simulate_clinical_data <- function(
    n = 1000,
    task = "sepsis",
    contamination_rate = 0.05,
    seed = NULL
) {
  
  if (!is.null(seed)) set.seed(seed)
  
  age <- round(pmax(pmin(rnorm(n, 67, 16), 95), 18))
  gender <- rbinom(n, 1, 0.45)
  
  creatinine <- pmax(rlnorm(n, log(1.2), 0.5), 0.3)
  lactate <- pmax(rlnorm(n, log(2.2), 0.6), 0.5)
  wbc <- pmax(rlnorm(n, log(9.5), 0.5), 1)
  bilirubin <- pmax(rlnorm(n, log(0.8), 0.6), 0.2)
  
  heart_rate <- pmax(pmin(rnorm(n, 86, 18), 160), 40)
  map <- pmax(pmin(rnorm(n, 85, 15), 150), 40)
  resp_rate <- pmax(pmin(rnorm(n, 18, 5), 40), 8)
  temperature <- pmax(pmin(rnorm(n, 37.0, 0.8), 39.5), 35)
  spo2 <- pmax(pmin(rnorm(n, 96, 3), 100), 85)
  
  logit <- -3.5 + 
    0.03 * scale(age) + 
    0.2 * gender + 
    0.5 * scale(log(creatinine)) + 
    0.8 * scale(log(lactate)) + 
    0.1 * scale(log(wbc))
  outcome <- rbinom(n, 1, plogis(logit))
  
  data <- data.frame(
    patient_id = 1:n,
    age = age,
    gender = gender,
    creatinine = creatinine,
    lactate = lactate,
    wbc = wbc,
    bilirubin = bilirubin,
    heart_rate = heart_rate,
    map = map,
    resp_rate = resp_rate,
    temperature = temperature,
    spo2 = spo2,
    outcome = outcome
  )
  
  if (contamination_rate > 0) {
    n_contam <- floor(n * contamination_rate)
    contam_idx <- sample(n, n_contam)
    mult <- runif(n_contam, 5, 15)
    data$creatinine[contam_idx] <- data$creatinine[contam_idx] * mult
  }
  
  attr(data, "task") <- task
  data
}
