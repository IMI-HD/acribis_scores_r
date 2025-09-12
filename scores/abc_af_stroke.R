library(dplyr)

param_names <- c(
  "Prior Stroke/TIA",
  "Age",
  "Troponin T in ng/L",
  "NT-proBNP in ng/L",
  "DOAC",
  "Aspirin"
)

bool_params <- c(
  "Prior Stroke/TIA",
  "DOAC",
  "Aspirin"
)

abc_af_stroke_check_ranges <- function(parameters) {
  if (parameters$`Age` < 22 || parameters$`Age` > 95) {
    stop("'Age' must be between 22 and 95")
  }
  if (parameters$`Troponin T in ng/L` < 3.0 || parameters$`Troponin T in ng/L` > 200.0) {
    stop("'Troponin T in ng/L' must be between 3.0 and 200.0")
  }
  if (parameters$`NT-proBNP in ng/L` < 5 || parameters$`NT-proBNP in ng/L` > 21000) {
    stop("'NT-proBNP in ng/L' must be between 5 and 21000")
  }
  if (!(parameters$`DOAC` || parameters$`Aspirin`)) {
    stop("Either 'DOAC' or 'Aspirin' must be true!")
  }
  if ((parameters$`DOAC` && parameters$`Aspirin`)) {
    stop("'DOAC' and 'Aspirin' cannot both be true!")
  }
}

abc_af_stroke_weights <- list(
  `Prior Stroke/TIA` = 0.8331,
  `Age` = 0.007488,
  `log(Troponin T in ng/L)` = 0.2139,
  `log(NT-proBNP in ng/L)` = 0.2879
)

calc_abc_af_stroke_score <- function(parameters) {
  abc_af_stroke_check_ranges(parameters)
  
  new_parameters <- parameters
  new_parameters$`log(Troponin T in ng/L)` <- log(parameters$`Troponin T in ng/L`)
  new_parameters$`log(NT-proBNP in ng/L)` <- log(parameters$`NT-proBNP in ng/L`)
  
  linear_predictor <- sum(unlist(new_parameters[names(abc_af_stroke_weights)]) * unlist(abc_af_stroke_weights)) - 3.286
  
  baseline_survival <- 0.9863
  
  if (parameters$Aspirin) {
    linear_predictor <- 0.25627 + 1.0426 * linear_predictor
    baseline_survival <- 0.9673
  }
  
  one_year_risk <- (1 - (baseline_survival ^ exp(linear_predictor)))
  return (one_year_risk * 100)
}

# parameters <- list(
#   `Prior Stroke/TIA` = TRUE,
#   `Age` = 70,
#   `Troponin T in ng/L` = 15.0,
#   `NT-proBNP in ng/L` = 1000,
#   `DOAC` = TRUE,
#   `Aspirin` = FALSE
# )
# 
# calc_abc_af_stroke_score(parameters)

# Function to process a data frame and calculate the ABC‐AF Stroke Score.
calc_abc_af_stroke_score_from_df <- function(patients_df) {
  # Set new column names: first column "PatientID", then the expected parameters.
  colnames(patients_df) <- c("PatientID", param_names)
  
  # Extract Patient IDs.
  patient_ids <- patients_df$PatientID
  
  # Preallocate a numeric vector to store stroke scores.
  scores <- numeric(nrow(patients_df))
  
  # Loop through each row.
  for (i in seq_len(nrow(patients_df))) {
    # Convert the current row (excluding PatientID) to a named list.
    patient_params <- as.list(patients_df[i, param_names])
    
    # Convert specified boolean fields to logical.
    for (field in bool_params) {
      patient_params[[field]] <- as.logical(as.integer(patient_params[[field]]))
    }
    
    # Calculate the stroke score using calc_abc_af_stroke_score (from abc_af_stroke.R).
    score <- tryCatch({
      calc_abc_af_stroke_score(patient_params)
    }, error = function(e) {
      cat("Error for Patient", patient_ids[i], ":", e$message, "\n")
      NA
    })
    
    scores[i] <- score
  }
  
  # Return a data frame with PatientIDs and their corresponding Stroke Scores.
  return(data.frame(PatientID = patient_ids, Score = scores,
                    stringsAsFactors = FALSE, check.names = FALSE))
}

# example_abc_stroke_df <- data.frame(
#   PatientID = c("P1", "P2", "P3"),
#   `Prior Stroke/TIA` = c(1, 0, 1),
#   Age = c(70, 65, 75),
#   `Troponin T in ng/L` = c(15.0, 12.0, 18.0),
#   `NT-proBNP in ng/L` = c(1000, 950, 1100),
#   DOAC = c(1, 1, 0),
#   Aspirin = c(0, 0, 1),
#   stringsAsFactors = FALSE
# )
# 
# # Test the ABC‐AF Stroke Score function.
# result_abc_stroke <- calc_abc_af_stroke_score_from_df(example_abc_stroke_df)
# print(result_abc_stroke)
