library(dplyr)

check_ranges <- function(parameters) {
  if (parameters$`Age` < 22 || parameters$`Age` > 95) {
    stop("'Age' must be between 22 and 95")
  }
  if (parameters$`Troponin T in ng/L` < 3.0 || parameters$`Troponin T in ng/L` > 200.0) {
    stop("'Troponin T in ng/L' must be between 3.0 and 200.0")
  }
  if (parameters$`NT-proBNP in ng/L` < 5 || parameters$`NT-proBNP in ng/L` > 21000) {
    stop("'NT-proBNP in ng/L' must be between 5 and 21000")
  }
}

weights <- list(
  `Prior Stroke/TIA` = 0.8331,
  `Age` = 0.007488,
  `log(Troponin T in ng/L)` = 0.2139,
  `log(NT-proBNP in ng/L)` = 0.2879
)

calc_abc_af_stroke_score <- function(parameters) {
  check_ranges(parameters)
  
  if (!(parameters$DOAC || parameters$Aspirin)) {
    stop("Either 'DOAC' or 'Aspirin' must be true!")
  }
  
  new_parameters <- parameters
  new_parameters$`log(Troponin T in ng/L)` <- log(parameters$`Troponin T in ng/L`)
  new_parameters$`log(NT-proBNP in ng/L)` <- log(parameters$`NT-proBNP in ng/L`)
  
  linear_predictor <- sum(unlist(new_parameters[names(weights)]) * unlist(weights)) - 3.286
  
  baseline_survival <- 0.9863
  
  if (parameters$Aspirin) {
    linear_predictor <- 0.25627 + 1.0426 * linear_predictor
    baseline_survival <- 0.9673
  }
  
  one_year_risk <- (1 - (baseline_survival ^ exp(linear_predictor)))
  return (one_year_risk * 100)
}

parameters <- list(
  `Prior Stroke/TIA` = TRUE,
  `Age` = 70,
  `Troponin T in ng/L` = 15.0,
  `NT-proBNP in ng/L` = 1000,
  `DOAC` = TRUE,
  `Aspirin` = FALSE
)

calc_abc_af_stroke_score(parameters)
