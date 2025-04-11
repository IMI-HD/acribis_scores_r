library(dplyr)

param_names <- c(
  "Age",
  "Race (white)",
  "Height",
  "Weight",
  "Systolic Blood Pressure",
  "Diastolic Blood Pressure",
  "Smoking (current)",
  "Antihypertensive Medication Use (Yes)",
  "Diabetes (Yes)",
  "Heart failure (Yes)",
  "Myocardial infarction (Yes)"
)

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


calc_charge_af_score_from_df <- function(patients_df) {
  # Set new column names to expected.
  colnames(patients_df) <- c("PatientID", param_names)

  # Extract PatientIDs from the data frame (assumes the column is named "PatientID")
  patient_ids <- patients_df$PatientID

  # Preallocate a numeric vector to store scores
  scores <- numeric(nrow(patients_df))

  for (i in seq_len(nrow(patients_df))) {
    # cat("Processing Patient", patient_ids[i], "...\n")

    # Convert the i-th row (excluding PatientID) to a named list
    patient_params <- as.list(patients_df[i, param_names])

    # Note: calc_charge_af_score uses each parameters name to retrieve weights and scales.
    # Booleans (e.g., for "Race (white)", "Smoking (current)") work fine since R converts
    # TRUE and FALSE to 1 and 0 automatically when used in arithmetic.
    score <- tryCatch({
      calc_charge_af_score(parameters)
    }, error = function(e) {
      cat("Error for Patient", patient_ids[i], ":", e$message, "\n")
      NA
    })

    scores[i] <- score
  }

  # Return a data frame with PatientIDs and their corresponding Scores
  return(data.frame(patient_ids, scores, stringsAsFactors = FALSE, check.names = FALSE))
}

# example_charge_af_df <- data.frame(
#   PatientID = c("P1", "P2", "P3"),
#   Age = c(60, 50, 95),  # 95 will trigger the range check error
#   `Race (white)` = c(TRUE, FALSE, TRUE),
#   Height = c(170, 165, 180),
#   Weight = c(70, 80, 75),
#   `Systolic Blood Pressure` = c(120, 110, 140),
#   `Diastolic Blood Pressure` = c(80, 70, 90),
#   `Smoking (current)` = c(FALSE, TRUE, FALSE),
#   `Antihypertensive Medication Use (Yes)` = c(TRUE, FALSE, TRUE),
#   `Diabetes (Yes)` = c(FALSE, TRUE, FALSE),
#   `Heart failure (Yes)` = c(FALSE, FALSE, TRUE),
#   `Myocardial infarction (Yes)` = c(TRUE, FALSE, TRUE),
#   stringsAsFactors = FALSE
# )
# 
# # Optionally, verify that the column names match
# print(names(example_charge_af_df))
# 
# # calc the Charge AF score function on the example data frame
# result_charge_af <- calc_charge_af_score_from_df(example_charge_af_df)
# print(result_charge_af)
