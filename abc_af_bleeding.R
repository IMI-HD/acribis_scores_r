# Load necessary libraries
library(dplyr)

# Define parameters validation function
check_ranges <- function(parameters) {
  if (parameters$`Age` < 22 || parameters$`Age` > 95) {
    stop("'Age' must be between 22 and 95")
  }
  if (parameters$`Troponin T in ng/L` < 3.0 || parameters$`Troponin T in ng/L` > 200.0) {
    stop("'Troponin T in ng/L' must be between 3.0 and 200.0")
  }
  if (parameters$`GDF-15 in ng/L` < 400.0 || parameters$`GDF-15 in ng/L` > 20000.0) {
    stop("'GDF-15 in ng/L' must be between 400.0 and 20000.0")
  }
  if (parameters$`Hemoglobin in g/dL` < 9.0 || parameters$`Hemoglobin in g/dL` > 20.0) {
    stop("'Hemoglobin in g/dL' must be between 9.0 and 20.0")
  }
}

# Define weights
weights <- list(
  `Prior Bleeding` = 0.2611,
  `Age` = 0.02168,
  `log(Troponin T in ng/L)` = 0.4095,
  `log(GDF-15 in ng/L)` = 0.4134,
  `Hemoglobin in g/dL` = -0.08541
)

calc_abc_af_bleeding_score <- function(parameters) {
  # Check the ranges
  check_ranges(parameters)
  
  if (!(parameters$DOAC || parameters$Aspirin)) {
    stop("Either 'DOAC' or 'Aspirin' must be true!")
  }
  
  new_parameters <- parameters
  new_parameters$`log(Troponin T in ng/L)` <- log(parameters$`Troponin T in ng/L`)
  new_parameters$`log(GDF-15 in ng/L)` <- log(parameters$`GDF-15 in ng/L`)
  
  linear_predictor <- sum(unlist(new_parameters[names(weights)]) * unlist(weights)) - 4.667
  
  baseline_survival <- 0.9766
  
  if (parameters$Aspirin) {
    linear_predictor <- 0.19965 + 1.2579 * linear_predictor
    baseline_survival <- 0.9914
  }
  
  one_year_risk <- (1 - (baseline_survival ^ exp(linear_predictor)))
  return (one_year_risk * 100)
}

# Example usage
parameters <- list(
  `Prior Bleeding` = TRUE,
  `Age` = 65,
  `Troponin T in ng/L` = 10.0,
  `GDF-15 in ng/L` = 500.0,
  `Hemoglobin in g/dL` = 13.5,
  `DOAC` = TRUE,
  `Aspirin` = FALSE
)

calc_abc_af_bleeding_score(parameters)

