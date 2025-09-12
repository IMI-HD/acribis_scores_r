library(dplyr)

param_names <- c(
  "Prior Bleeding",
  "Age",
  "Troponin T in ng/L",
  "GDF-15 in ng/L",
  "Hemoglobin in g/dL",
  "DOAC",
  "Aspirin"
)

bool_params <- c(
  "Prior Bleeding",
  "DOAC",
  "Aspirin"
)

# Define parameters validation function
abc_af_bleeding_check_ranges <- function(parameters) {
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
  if (!(parameters$`DOAC` || parameters$`Aspirin`)) {
    stop("Either 'DOAC' or 'Aspirin' must be true!")
  }
  if ((parameters$`DOAC` && parameters$`Aspirin`)) {
    stop("'DOAC' and 'Aspirin' cannot both be true!")
  }
}

# Define weights
abc_af_bleeding_weights <- list(
  `Prior Bleeding` = 0.2611,
  `Age` = 0.02168,
  `log(Troponin T in ng/L)` = 0.4095,
  `log(GDF-15 in ng/L)` = 0.4134,
  `Hemoglobin in g/dL` = -0.08541
)

calc_abc_af_bleeding_score <- function(parameters) {
  # Check the ranges
  abc_af_bleeding_check_ranges(parameters)
  
  new_parameters <- parameters
  new_parameters$`log(Troponin T in ng/L)` <- log(parameters$`Troponin T in ng/L`)
  new_parameters$`log(GDF-15 in ng/L)` <- log(parameters$`GDF-15 in ng/L`)
  
  linear_predictor <- sum(unlist(new_parameters[names(abc_af_bleeding_weights)]) * unlist(abc_af_bleeding_weights)) - 4.667
  
  baseline_survival <- 0.9766
  
  if (parameters$Aspirin) {
    linear_predictor <- 0.19965 + 1.2579 * linear_predictor
    baseline_survival <- 0.9914
  }
  
  one_year_risk <- (1 - (baseline_survival ^ exp(linear_predictor)))
  return (one_year_risk * 100)
}

# parameters <- list(
#   `Prior Bleeding` = TRUE,
#   `Age` = 65,
#   `Troponin T in ng/L` = 10.0,
#   `GDF-15 in ng/L` = 500.0,
#   `Hemoglobin in g/dL` = 13.5,
#   `DOAC` = TRUE,
#   `Aspirin` = FALSE
# )
# 
# calc_abc_af_bleeding_score(parameters)

# Function to process a data frame and calculate the ABC‐AF Bleeding Score.
calc_abc_af_bleeding_score_from_df <- function(patients_df) {
  # Set new column names: the first column must be "PatientID", then the expected parameters.
  colnames(patients_df) <- c("PatientID", param_names)
  
  # Extract Patient IDs.
  patient_ids <- patients_df$PatientID
  
  # Preallocate a numeric vector to store the scores.
  scores <- numeric(nrow(patients_df))
  
  # Loop through each row.
  for (i in seq_len(nrow(patients_df))) {
    # Convert the row (excluding PatientID) into a named list.
    patient_params <- as.list(patients_df[i, param_names])
    
    # Convert specified boolean fields (if provided as 0/1) to logical values.
    for (field in bool_params) {
      patient_params[[field]] <- as.logical(as.integer(patient_params[[field]]))
    }
    
    # Calculate the score using calc_abc_af_bleeding_score (from abc_af_bleeding.R).
    score <- tryCatch({
      calc_abc_af_bleeding_score(patient_params)
    }, error = function(e) {
      cat("Error for Patient", patient_ids[i], ":", e$message, "\n")
      NA
    })
    
    scores[i] <- score
  }
  
  # Return a data frame with PatientIDs and their corresponding Bleeding Scores.
  return(data.frame(PatientID = patient_ids, Score = scores,
                    stringsAsFactors = FALSE, check.names = FALSE))
}

# example_abc_bleeding_df <- data.frame(
#   PatientID = c("P1", "´P2", "P3"),
#   `Prior Bleeding` = c(1, 0, 1),
#   Age = c(65, 70, 55),
#   `Troponin T in ng/L` = c(10.0, 15.0, 12.0),
#   `GDF-15 in ng/L` = c(500.0, 800.0, 600.0),
#   `Hemoglobin in g/dL` = c(13.5, 12.0, 14.0),
#   DOAC = c(1, 1, 0),
#   Aspirin = c(0, 0, 0),
#   stringsAsFactors = FALSE
# )
# 
# # Test the ABC‐AF Bleeding Score function.
# result_abc_bleeding <- calc_abc_af_bleeding_score_from_df(example_abc_bleeding_df)
# print(result_abc_bleeding)

