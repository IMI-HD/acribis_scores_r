library(dplyr)

weights <- list(
  `Age in years` = -0.085,
  `Squared Age in years` = 0.00105,
  `Male` = 0.156,
  `Current smoker` = 0.262,
  `Systolic blood pressure in mmHg` = 0.00429,
  `Diabetic` = 0.223,
  `History of coronary artery disease` = 0.14,
  `History of cerebrovascular disease` = 0.406,
  `Abdominal aortic aneurysm` = 0.558,
  `Peripheral artery disease` = 0.283,
  `Years since first diagnosis of vascular disease` = 0.0229,
  `HDL-cholesterol in mmol/L` = -0.426,
  `Total cholesterol in mmol/L` = 0.0959,
  `eGFR in mL/min/1.73m²` = -0.0532,
  `Squared eGFR in mL/min/1.73m²` = 0.000306,
  `log(hs-CRP in mg/dL)` = 0.139
)

# parameters <- list(
#   `Age in years` = as.integer(NA),
#   `Male` = as.logical(NA),
#   `Current smoker` = as.logical(NA),
#   `Systolic blood pressure in mmHg` = as.integer(NA),
#   `Diabetic` = as.logical(NA),
#   `History of coronary artery disease` = as.logical(NA),
#   `History of cerebrovascular disease` = as.logical(NA),
#   `Abdominal aortic aneurysm` = as.logical(NA),
#   `Peripheral artery disease` = as.logical(NA),
#   `Years since first diagnosis of vascular disease` = as.integer(NA),
#   `HDL-cholesterol in mmol/L` = as.numeric(NA),
#   `Total cholesterol in mmol/L` = as.numeric(NA),
#   `eGFR in mL/min/1.73m²` = as.numeric(NA),
#   `hs-CRP in mg/dL` = as.numeric(NA),
#   `Antithrombotic treatment` = as.logical(NA)
# )

# Define the validation functions for each parameter type
validate_range <- function(value, min, max, param_name) {
  if (!is.numeric(value)) {
    return(paste(param_name, "must be numeric."))
  } else if (value < min || value > max) {
    return(paste(param_name, "must be between", min, "and", max, "."))
  }
  return(TRUE)
}

validate_bool <- function(value, param_name) {
  if (!is.logical(value) || length(value) != 1) {
    return(paste(param_name, "must be a single boolean (TRUE or FALSE)."))
  }
  return(TRUE)
}

# Define a main validation function for the parameters
validate_parameters <- function(params) {
  validation_results <- list(
    `Age in years` = validate_range(params$`Age in years`, 30, 90, "Age in years"),
    `Male` = validate_bool(params$Male, "Male"),
    `Current smoker` = validate_bool(params$`Current smoker`, "Current smoker"),
    `Systolic blood pressure in mmHg` = validate_range(params$`Systolic blood pressure in mmHg`, 70, 200, "Systolic blood pressure in mmHg"),
    `Diabetic` = validate_bool(params$Diabetic, "Diabetic"),
    `History of coronary artery disease` = validate_bool(params$`History of coronary artery disease`, "History of coronary artery disease"),
    `History of cerebrovascular disease` = validate_bool(params$`History of cerebrovascular disease`, "History of cerebrovascular disease"),
    `Abdominal aortic aneurysm` = validate_bool(params$`Abdominal aortic aneurysm`, "Abdominal aortic aneurysm"),
    `Peripheral artery disease` = validate_bool(params$`Peripheral artery disease`, "Peripheral artery disease"),
    `Years since first diagnosis of vascular disease` = validate_range(params$`Years since first diagnosis of vascular disease`, 0, 30, "Years since first diagnosis of vascular disease"),
    `HDL-cholesterol in mmol/L` = validate_range(params$`HDL-cholesterol in mmol/L`, 0.6, 2.5, "HDL-cholesterol in mmol/L"),
    `Total cholesterol in mmol/L` = validate_range(params$`Total cholesterol in mmol/L`, 2.5, 8.0, "Total cholesterol in mmol/L"),
    `eGFR in mL/min/1.73m²` = validate_range(params$`eGFR in mL/min/1.73m²`, 21.60551, 178.39297, "eGFR in mL/min/1.73m²"),
    `hs-CRP in mg/dL` = validate_range(params$`hs-CRP in mg/dL`, 0.1, 15.0, "hs-CRP in mg/dL"),
    `Antithrombotic treatment` = validate_bool(params$`Antithrombotic treatment`, "Antithrombotic treatment")
  )
  
  # Collect issues as non-TRUE results
  issues <- Filter(function(x) x != TRUE, validation_results)
  if (length(issues) == 0) {
    return(TRUE)  # No issues, validation passed
  } else {
    return(issues)  # Return validation errors
  }
}

calc_smart_score <- function(parameters) {
  parameters[['Squared Age in years']] <- parameters[['Age in years']]^2
  parameters[['Squared eGFR in mL/min/1.73m²']] <- parameters[['eGFR in mL/min/1.73m²']]^2
  parameters[['log(hs-CRP in mg/dL)']] <- log(parameters[['hs-CRP in mg/dL']])
  
  x <- sum(sapply(names(weights), function(name) parameters[[name]] * weights[[name]]))
  
  ten_year_risk <- (1 - 0.81066^exp(x + 2.099))
  if (parameters[['History of cerebrovascular disease']]) {
    cvd_ten_year_risk <- (1 - 0.7184^exp(x + 1.933))
    ten_year_risk <- max(cvd_ten_year_risk, ten_year_risk)
  }
  if (parameters[['Peripheral artery disease']]) {
    pad_ten_year_risk <- (1 - 0.70594^exp(x + 1.4))
    ten_year_risk <- max(pad_ten_year_risk, ten_year_risk)
  }
  if (!parameters[['Antithrombotic treatment']]) {
    ten_year_risk <- 1.0 - (1.0 - ten_year_risk)^(1.0 / 0.81)
  }
  return(ten_year_risk * 100)
}

# Function to validate and run the score calculation if validation passes
run_smart_score_if_valid <- function(params) {
  validation_results <- validate_parameters(params)
  
  if (isTRUE(validation_results)) {
    # Validation passed, run score function
    score_value <- calc_smart_score(params)
    cat("Score:", score_value, "\n")
  } else {
    # Validation failed, print errors
    cat("Validation Errors:\n")
    print(validation_results)
  }
}
# 
# parameters <- list(
#   `Age in years` = 40,
#   `Male` = FALSE,
#   `Current smoker` = FALSE,
#   `Systolic blood pressure in mmHg` = 110,
#   `Diabetic` = FALSE,
#   `History of coronary artery disease` = FALSE,
#   `History of cerebrovascular disease` = FALSE,
#   `Abdominal aortic aneurysm` = FALSE,
#   `Peripheral artery disease` = FALSE,
#   `Years since first diagnosis of vascular disease` = 8,
#   `HDL-cholesterol in mmol/L` = 1,
#   `Total cholesterol in mmol/L` = 3,
#   `eGFR in mL/min/1.73m²` = 177,
#   `hs-CRP in mg/dL` = 6,
#   `Antithrombotic treatment` = FALSE
# )
# 
# run_smart_score_if_valid(parameters)
