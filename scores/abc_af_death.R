param_names <- c(
  "Heart Failure",
  "Age",
  "NT-proBNP in ng/L",
  "GDF-15 in ng/L",
  "Troponin T in ng/L"
)

bool_params <- c("Heart Failure")


MODEL_A_WEIGHTS <- list(
  'Heart Failure' = 0.3416,
  'Age' = -0.01305,
  '(Age - 66) ^ 3' = 0.0001723,
  '(Age - 74) ^ 3' = -0.0003446,
  '(Age - 82) ^ 3' = 0.0001723,
  'NT-proBNP in ng/L' = 0.04248,
  '(NT-proBNP in ng/L - 5.303) ^ 3' = 0.04728,
  '(NT-proBNP in ng/L - 6.708) ^ 3' = -0.1139,
  '(NT-proBNP in ng/L - 7.705) ^ 3' = 0.0666,
  'GDF-15 in ng/L' = 0.7963,
  '(GDF-15 in ng/L - 6.608) ^ 3' = -0.1923,
  '(GDF-15 in ng/L - 7.231) ^ 3' = 0.341,
  '(GDF-15 in ng/L - 8.037) ^ 3' = -0.1487,
  'Troponin T in ng/L' = 0.6875,
  '(Troponin T in ng/L - 1.705) ^ 3' = -0.07336,
  '(Troponin T in ng/L - 2.389) ^ 3' = 0.1344,
  '(Troponin T in ng/L - 3.211) ^ 3' = -0.06104
)

MODEL_A_SPLINE_TERMS <- list(
  'Age' = c(66, 74, 82),
  'NT-proBNP in ng/L' = c(5.303, 6.708, 7.705),
  'GDF-15 in ng/L' = c(6.608, 7.231, 8.037),
  'Troponin T in ng/L' = c(1.705, 2.389, 3.211)
)

MODEL_B_WEIGHTS <- list(
  'Heart Failure' = 0.4635,
  'Age' = -0.01244,
  '(Age - 71) ^ 3' = 0.0003442,
  '(Age - 77) ^ 3' = -0.0006393,
  '(Age - 84) ^ 3' = 0.0002951,
  'NT-proBNP in ng/L' = 0.05166,
  '(NT-proBNP in ng/L - 5.303) ^ 3' = 0.05677,
  '(NT-proBNP in ng/L - 6.708) ^ 3' = -0.1367,
  '(NT-proBNP in ng/L - 7.705) ^ 3' = 0.07998,
  'GDF-15 in ng/L' = 0.4796,
  '(GDF-15 in ng/L - 6.608) ^ 3' = -0.1769,
  '(GDF-15 in ng/L - 7.231) ^ 3' = 0.3137,
  '(GDF-15 in ng/L - 8.037) ^ 3' = -0.1368,
  'Troponin T in ng/L' = 1.026,
  '(Troponin T in ng/L - 1.705) ^ 3' = -0.1508,
  '(Troponin T in ng/L - 2.389) ^ 3' = 0.2763,
  '(Troponin T in ng/L - 3.211) ^ 3' = -0.1255
)

MODEL_B_SPLINE_TERMS <- list(
  'Age' = c(71, 77, 84),
  'NT-proBNP in ng/L' = c(5.303, 6.708, 7.705),
  'GDF-15 in ng/L' = c(6.608, 7.231, 8.037),
  'Troponin T in ng/L' = c(1.705, 2.389, 3.211)
)

add_spline_terms <- function(parameters, spline_terms) {
  for (parameter in names(spline_terms)) {
    for (x in spline_terms[[parameter]]) {
      spline_term_name <- sprintf("(%s - %s) ^ 3", parameter, x)
      parameters[[spline_term_name]] <- pmax(0, parameters[[parameter]] - x) ^ 3
    }
  }
  return(parameters)
}

calc_score <- function(parameters, weights, spline_terms, base, exponent) {
  parameters <- add_spline_terms(parameters, spline_terms)
  weighted_sum <- sum(unlist(sapply(names(weights), function(name) {
    if (name %in% names(parameters)) {
      return(parameters[[name]] * weights[[name]])
    } else {
      return(0)
    }
  })))
  
  one_year_risk <- (1 - base ^ exp(weighted_sum - exponent)) * 100
  
  return(one_year_risk)
}


calc_abc_af_death_score <- function(parameters) {
  model_a_parameters <- list('Heart Failure' = parameters[['Heart Failure']],
                             'NT-proBNP in ng/L' = log(pmax(200, parameters[['NT-proBNP in ng/L']])),
                             'GDF-15 in ng/L' = log(parameters[['GDF-15 in ng/L']]),
                             'Troponin T in ng/L' = log(parameters[['Troponin T in ng/L']]))
  model_b_parameters <- model_a_parameters
  
  model_a_parameters[['Age']] <- pmax(65, parameters[['Age']])
  model_b_parameters[['Age']] <- pmax(70, parameters[['Age']])
  
  model_a <- calc_score(model_a_parameters, MODEL_A_WEIGHTS, MODEL_A_SPLINE_TERMS, 0.9763, 7.218)
  model_b <- calc_score(model_b_parameters, MODEL_B_WEIGHTS, MODEL_B_SPLINE_TERMS, 0.9876, 5.952)
  
  return(list(model_a, model_b))
}


# parameters <- list(
#   `Heart Failure` = TRUE,
#   `Age` = 72,
#   `NT-proBNP in ng/L` = 600.0,
#   `GDF-15 in ng/L` = 700.0,
#   `Troponin T in ng/L` = 10.0
# )
# 
# calc_abc_af_death_score(parameters)

# Function to process a data frame and calculate the ABC‐AF Death Score.
# This function returns a data frame with PatientIDs and two columns: Model_A and Model_B.
calc_abc_af_death_score_from_df <- function(patients_df) {
  # Set new column names: first column "PatientID", then the expected parameters.
  colnames(patients_df) <- c("PatientID", param_names)
  
  # Extract Patient IDs.
  patient_ids <- patients_df$PatientID
  
  # Preallocate numeric vectors for the two models.
  model_a_scores <- numeric(nrow(patients_df))
  model_b_scores <- numeric(nrow(patients_df))
  
  # Loop through each row.
  for (i in seq_len(nrow(patients_df))) {
    # Convert the current row (excluding PatientID) to a named list.
    patient_params <- as.list(patients_df[i, param_names])
    
    # Convert boolean fields.
    for (field in bool_params) {
      patient_params[[field]] <- as.logical(as.integer(patient_params[[field]]))
    }
    
    # Calculate the death score using calc_abc_af_death_score (from abc_af_death.R).
    result <- tryCatch({
      calc_abc_af_death_score(patient_params)
    }, error = function(e) {
      cat("Error for Patient", patient_ids[i], ":", e$message, "\n")
      list(NA, NA)
    })
    
    model_a_scores[i] <- result[[1]]
    model_b_scores[i] <- result[[2]]
  }
  
  # Return a data frame with PatientIDs, Model_A, and Model_B scores.
  return(data.frame(PatientID = patient_ids, Model_A = model_a_scores, Model_B = model_b_scores,
                    stringsAsFactors = FALSE, check.names = FALSE))
}

# example_abc_death_df <- data.frame(
#   PatientID = c("P1", "P2", "P3"),
#   `Heart Failure` = c(1, 0, 1),
#   Age = c(72, 68, 75),
#   `NT-proBNP in ng/L` = c(600.0, 550.0, 800.0),
#   `GDF-15 in ng/L` = c(700.0, 650.0, 900.0),
#   `Troponin T in ng/L` = c(10.0, 8.0, 12.0),
#   stringsAsFactors = FALSE
# )
# 
# # Test the ABC‐AF Death Score function.
# result_abc_death <- calc_abc_af_death_score_from_df(example_abc_death_df)
# print(result_abc_death)
