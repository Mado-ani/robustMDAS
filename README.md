# robustMDAS

<!-- badges: start -->
[![R-CMD-check](https://github.com/Mado-ani/robustMDAS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Mado-ani/robustMDAS/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R Version](https://img.shields.io/badge/R-%3E%3D%203.5.0-blue.svg)](https://www.r-project.org/)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![GitHub issues](https://img.shields.io/github/issues/Mado-ani/robustMDAS)](https://github.com/Mado-ani/robustMDAS/issues)
[![GitHub stars](https://img.shields.io/github/stars/Mado-ani/robustMDAS)](https://github.com/Mado-ani/robustMDAS/stargazers)
<!-- badges: end -->

## Overview

**robustMDAS** implements the **R**obust **M**ahalanobis **D**istribution **A**lignment **S**core for assessing train-test split quality in clinical machine learning. The package addresses the fundamental vulnerability of classical split diagnostics to outlying observations endemic in electronic health records (EHRs).

### Key Features

- **Robust covariance estimation** using the Minimum Covariance Determinant (MCD) estimator with 25% breakdown point
- **Multi-measure battery** including energy distance, maximum mean discrepancy (MMD), and Kolmogorov-Smirnov (KS) statistics
- **Cauchy combination test** for combining dependent p-values under arbitrary dependence
- **Feature-level diagnosis** identifying which specific variables drive distributional imbalance
- **Synthetic data generation** mimicking MIMIC-IV clinical data for testing and validation
- **Interactive Shiny application** for point-and-click analysis

## Installation

### From GitHub (Recommended)

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install robustMDAS
devtools::install_github("Mado-ani/robustMDAS")

# Load the package
library(robustMDAS)

```
# From Source

```r
# Download the package
git clone https://github.com/Mado-ani/robustMDAS.git
cd robustMDAS

# Install in R
R CMD INSTALL .
```

# Quick Start
# Basic Usage

```r
library(robustMDAS)

# Generate synthetic clinical data
set.seed(456)
data <- simulate_clinical_data(n = 500, task = "sepsis")

# Preprocess (log transform + standardization)
data <- preprocess_clinical_data(data)

# Create train-test split (80/20)
n_total <- nrow(data)
n_train <- floor(0.8 * n_total)
train_idx <- sample(n_total, n_train)

X_train <- data[train_idx, -ncol(data)]  # Features
X_test <- data[-train_idx, -ncol(data)]
y_train <- data$outcome[train_idx]
y_test <- data$outcome[-train_idx]

# Run the robust split diagnostic
result <- robust_diagnostic(X_train, X_test, n_replications = 200)

# View results
print(result$p_combined)      # Combined p-value
print(result$reject_null)     # TRUE = split inadequate
print(result$ks_diagnosis$feature_max)  # Most imbalanced feature
```


# Example Output
```r
========== Diagnostic Summary ==========
Combined p-value: 0.575 
Decision: ACCEPT H0 - Split ADEQUATE 

Individual p-values:
   mdas : 0.4118 
   energy : 0.4118 
   mmd : 1 
   ks : 1 
========================================
```

# Detailed Function Documentation
# Core Diagnostic Function
```r
robust_diagnostic()
```
Main function implementing Algorithm 1. Assesses train-test split quality.

```r
robust_diagnostic(
  X_train,           # Training data matrix (n_tr x p)
  X_test,            # Test data matrix (n_te x p)
  alpha = 0.05,      # Significance level
  n_replications = 500,  # Monte Carlo replications
  alpha_mcd = 0.75,  # MCD coverage parameter (breakdown = 25%)
  n_cores = 1,       # Parallel cores
  return_details = FALSE,  # Return full reference distribution
  verbose = TRUE     # Print progress
)
```








