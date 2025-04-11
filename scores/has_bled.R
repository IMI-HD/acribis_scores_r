param_names <- c(
  "Uncontrolled hypertension",
  "Abnormal Renal Function",
  "Abnormal Liver Function",
  "Stroke",
  "Bleeding history or predisposition",
  "Labile international normalized ratio (INR)",
  "Elderly",
  "Drugs",
  "Alcohol"
)

calc_has_bled_score <- function(parameters) {
  score <- as.integer(sum(unlist(parameters)))
  
  return(score)
}
# 
# parameters <- list(
#   `Uncontrolled hypertension` = TRUE,
#   `Abnormal Renal Function` = FALSE,
#   `Abnormal Liver Function` = TRUE,
#   `Stroke` = FALSE,
#   `Bleeding history or predisposition` = TRUE,
#   `Labile international normalized ratio (INR)` = FALSE,
#   `Elderly` = TRUE,
#   `Drugs` = TRUE,
#   `Alcohol` = FALSE
# )
# 
# calc_has_bled_score(parameters)


calc_has_bled_score_from_df <- function(patients_df) {
  # Set new column names to expected.
  colnames(patients_df) <- c("PatientID", param_names)
  
  # Extract Patient IDs (assumes the column is named "PatientID")
  patient_ids <- patients_df$PatientID
  
  # Preallocate a numeric vector to store HAS‐BLED scores
  scores <- numeric(nrow(patients_df))
  
  # Loop through each row and compute the score
  for (i in seq_len(nrow(patients_df))) {
    # cat("Processing Patient", patient_ids[i], "...\n")
    
    # Convert the current row (excluding PatientID) to a named list of parameters
    patient_params <- as.list(patients_df[i, param_names])
    
    # Convert parameter values to logical if necessary. 
    # (This ensures that numeric values like 0 or 1 are treated as FALSE/TRUE.)
    for (field in names(parameters)) {
      # Use as.integer() in case the value is numeric, then convert to logical.
      parameters[[field]] <- as.logical(as.integer(parameters[[field]]))
    }
    
    # Calculate the HAS-BLED score using calc_has_bled_score()
    score <- tryCatch({
      calc_has_bled_score(parameters)
    }, error = function(e) {
      cat("Error for Patient", patient_ids[i], ":", e$message, "\n")
      NA
    })
    
    scores[i] <- score
  }
  
  # Return a data frame with PatientIDs and their corresponding Scores
  return(data.frame(patient_ids, scores, stringsAsFactors = FALSE, check.names = FALSE))
}

# example_has_bled_df <- data.frame(
#   PatientID = c("P1", "P2", "P3"),
#   `Uncontrolled hypertension` = c(1, 0, 1),
#   `Abnormal Renal Function` = c(0, 1, 0),
#   `Abnormal Liver Function` = c(1, 0, 1),
#   `Stroke` = c(0, 1, 0),
#   `Bleeding history or predisposition` = c(1, 1, 0),
#   `Labile international normalized ratio (INR)` = c(0, 0, 1),
#   `Elderly` = c(1, 0, 1),
#   `Drugs` = c(1, 1, 0),
#   `Alcohol` = c(0, 0, 1),
#   stringsAsFactors = FALSE
# )
# 
# # Optionally, verify that the data frame has the required column names:
# print(names(example_has_bled_df))
# 
# # calc the HAS-BLED score function on the example data frame
# result_has_bled <- calc_has_bled_score_from_df(example_has_bled_df)
# print(result_has_bled)

