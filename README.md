# robustMDAS

<!-- badges: start -->
[![R-CMD-check](https://github.com/Mado-ani/robustMDAS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Mado-ani/robustMDAS/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/robustMDAS)](https://CRAN.R-project.org/package=robustMDAS)
[![Downloads](https://cranlogs.r-pkg.org/badges/robustMDAS)](https://CRAN.R-project.org/package=robustMDAS)
[![R Version](https://img.shields.io/badge/R-%3E%3D%203.5.0-blue.svg)](https://www.r-project.org/)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

## Overview

**robustMDAS** implements the Robust Mahalanobis Distribution Alignment Score for assessing train-test split quality in clinical machine learning. The package provides robust covariance estimation via the Minimum Covariance Determinant (MCD) estimator, a complementary battery of discrepancy measures (energy distance, maximum mean discrepancy, Kolmogorov-Smirnov battery), and a Cauchy combination test for dependent p-values.

[Rest of your README content...]
```r
devtools::install_github("mohamad-91/robustMDAS")
