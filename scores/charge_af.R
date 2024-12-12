library(dplyr)

charge_af_check_ranges <- function(parameters) {
  if (parameters$`Age` < 46 || parameters$`Age` > 94) {
    stop("'Age' must be between 46 and 94")
  }
}

charge_af_weights <- list(
  `Age` = 0.508,
  `Race (white)` = 0.465,
  `Height` = 0.248,
  `Weight` = 0.115,
  `Systolic Blood Pressure` = 0.197,
  `Diastolic Blood Pressure` = -0.101,
  `Smoking (current)` = 0.359,
  `Antihypertensive Medication Use (Yes)` = 0.349,
  `Diabetes (Yes)` = 0.237,
  `Heart failure (Yes)` = 0.701,
  `Myocardial infarction (Yes)` = 0.496
)

scales <- list(
  `Age` = 5,
  `Race (white)` = 1,
  `Height` = 10,
  `Weight` = 15,
  `Systolic Blood Pressure` = 20,
  `Diastolic Blood Pressure` = 10,
  `Smoking (current)` = 1,
  `Antihypertensive Medication Use (Yes)` = 1,
  `Diabetes (Yes)` = 1,
  `Heart failure (Yes)` = 1,
  `Myocardial infarction (Yes)` = 1
)

calc_charge_af_score <- function(parameters) {
  # Check the ranges
  charge_af_check_ranges(parameters)

  x <- sum(sapply(names(parameters), function(param) (parameters[[param]] / scales[[param]]) * charge_af_weights[[param]]))
  one_year_risk <- (1 - (0.9718412736 ^ exp(x + -12.58156))) * 100
  return (one_year_risk)
}
# 
# parameters <- list(
#   `Age` = 60,
#   `Race (white)` = TRUE,
#   `Height` = 170,
#   `Weight` = 70,
#   `Systolic Blood Pressure` = 120,
#   `Diastolic Blood Pressure` = 80,
#   `Smoking (current)` = FALSE,
#   `Antihypertensive Medication Use (Yes)` = TRUE,
#   `Diabetes (Yes)` = FALSE,
#   `Heart failure (Yes)` = FALSE,
#   `Myocardial infarction (Yes)` = TRUE
# )
# 
# print(calc_charge_af_score(parameters))
# 
