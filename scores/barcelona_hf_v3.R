library(dplyr)
library(purrr)
library(tibble)
library(readr)

Model <- list(
  MODEL_1 = "MODEL_1",
  MODEL_2 = "MODEL_2",
  MODEL_3 = "MODEL_3",
  MODEL_4 = "MODEL_4",
  MODEL_5 = "MODEL_5",
  MODEL_6 = "MODEL_6",
  MODEL_7 = "MODEL_7",
  MODEL_8 = "MODEL_8"
)

MIN_MAX_MEDIAN <- tibble(
  variable = c("Age (years)", "Ejection fraction (%)", "Sodium (mmol/L)", "eGFR in mL/min/1.73m²",
               "Hemoglobin (g/dL)", "NT-proBNP in pg/mL", "hs-cTnT in ng/L", "ST2 (ng/mL)",
               "HF Duration in months", "Hospitalisation Prev. Year"),
  lower_limit = c(35, 11, 130, 6.4, 8.8, 21.5017, 3, 18.089, 0.5, 0),
  upper_limit = c(88, 71, 147, 109.7, 17.1, 34800.59, 265.45, 157.074, 246.948, 8),
  median_impute = c(70.3, 34, 139, 51.2, 12.9, 1361.5, 22.6, 38.1, 27, 0)
)

check_values <- function(parameter_value, parameter_name, min_max_median) {
  row <- filter(min_max_median, variable == parameter_name)
  lower_limit <- row$lower_limit
  upper_limit <- row$upper_limit
  median_impute <- row$median_impute
  
  if (is.na(parameter_value)) {
    parameter_value <- ifelse(!is.na(median_impute), median_impute, NA)
  } else if (parameter_value < lower_limit) {
    parameter_value <- lower_limit
  } else if (parameter_value > upper_limit) {
    parameter_value <- upper_limit
  }
  return(parameter_value)
}

get_model <- function(parameters) {
  if (!is.na(parameters$`NT-proBNP in pg/mL`) &&
      is.na(parameters$`hs-cTnT in ng/L`) && is.na(parameters$`ST2 (ng/mL)`)) {
    return(Model$MODEL_2)
  } else if (!is.na(parameters$`hs-cTnT in ng/L`) &&
             is.na(parameters$`NT-proBNP in pg/mL`) && is.na(parameters$`ST2 (ng/mL)`)) {
    return(Model$MODEL_3)
  } else if (!is.na(parameters$`ST2 (ng/mL)`) &&
             is.na(parameters$`NT-proBNP in pg/mL`) && is.na(parameters$`hs-cTnT in ng/L`)) {
    return(Model$MODEL_4)
  } else if (!is.na(parameters$`NT-proBNP in pg/mL`) && !is.na(parameters$`ST2 (ng/mL)`) &&
             is.na(parameters$`hs-cTnT in ng/L`)) {
    return(Model$MODEL_5)
  } else if (!is.na(parameters$`NT-proBNP in pg/mL`) && !is.na(parameters$`hs-cTnT in ng/L`) &&
             is.na(parameters$`ST2 (ng/mL)`)) {
    return(Model$MODEL_6)
  } else if (!is.na(parameters$`hs-cTnT in ng/L`) && !is.na(parameters$`ST2 (ng/mL)`) &&
             is.na(parameters$`NT-proBNP in pg/mL`)) {
    return(Model$MODEL_7)
  } else if (!is.na(parameters$`NT-proBNP in pg/mL`) && !is.na(parameters$`hs-cTnT in ng/L`) &&
             !is.na(parameters$`ST2 (ng/mL)`)) {
    return(Model$MODEL_8)
  } else {
    return(Model$MODEL_1)
  }
}

get_coefficients <- function(model, model_coefficients) {
  # Excluding the last six rows from model_coefficients
  coefficients <- model_coefficients %>% 
    slice(1:(n() - 6)) %>% 
    select(Variables, !!sym(model)) %>% 
    deframe()
  
  # Extracting the sum_product value
  sum_product <- model_coefficients %>% 
    filter(Variables == "Sum_Product") %>% 
    pull(!!sym(model))
  
  # Returning the list containing coefficients and sum_product
  return(list(coefficients = coefficients, sum_product = sum_product))
}


get_survival_estimate <- function(model, survival_year, model_coefficients) {
  survival_estimate <- switch(as.character(survival_year),
                              "1" = model_coefficients %>% filter(Variables == "One_year_survival") %>% pull(!!sym(model)),
                              "2" = model_coefficients %>% filter(Variables == "Two_year_survival") %>% pull(!!sym(model)),
                              "3" = model_coefficients %>% filter(Variables == "Three_year_survival") %>% pull(!!sym(model)),
                              "4" = model_coefficients %>% filter(Variables == "Four_year_survival") %>% pull(!!sym(model)),
                              "5" = model_coefficients %>% filter(Variables == "Five_year_survival") %>% pull(!!sym(model)),
                              stop("'survival_year' must be between 1 and 6!"))
  return(survival_estimate)
}


get_new_parameters <- function(parameters) {
  new_parameters <- parameters
  new_parameters$`NYHA Class` <- ifelse(parameters$`NYHA Class` %in% c(1, 2), 0, 1)
  new_parameters$`Ejection fraction (%)` <- ifelse(parameters$`Ejection fraction (%)` <= 45, 0, 1)
  new_parameters$`log(HF Duration in months)` <- log(parameters$`HF Duration in months`)
  new_parameters$`Furosemide Dose 1` <- ifelse(parameters$`Loop Diuretic Furosemide Dose` > 0 & parameters$`Loop Diuretic Furosemide Dose` <= 40, 1, 0)
  new_parameters$`Furosemide Dose 2` <- ifelse(parameters$`Loop Diuretic Furosemide Dose` > 40 & parameters$`Loop Diuretic Furosemide Dose` <= 80, 1, 0)
  new_parameters$`Furosemide Dose 3` <- ifelse(parameters$`Loop Diuretic Furosemide Dose` > 80, 1, 0)
  if (!is.na(parameters$`NT-proBNP in pg/mL`)) {
    new_parameters$`log(NT-proBNP in pg/mL)` <- ifelse(parameters$`NT-proBNP in pg/mL` == 0, 0, log(parameters$`NT-proBNP in pg/mL`))
  }
  if (!is.na(parameters$`hs-cTnT in ng/L`)) {
    new_parameters$`log(hs-cTnT in ng/L)` <- ifelse(parameters$`hs-cTnT in ng/L` == 0, 0, log(parameters$`hs-cTnT in ng/L`))
    new_parameters$`Squared log(hs-cTnT in ng/L)` <- (new_parameters$`log(hs-cTnT in ng/L)`)^2
  }
  if (!is.na(parameters$`ST2 (ng/mL)`)) {
    new_parameters$`ST2_div_10` <- parameters$`ST2 (ng/mL)` / 10
    new_parameters$`Squared ST2_div_10` <- (new_parameters$`ST2_div_10`)^2
  }
  return(new_parameters)
}

get_scores <- function(file, model, new_parameters) {
  scores <- c()
  file_path <- file
  if (!file.exists(file_path)) {
    stop(paste("Error: File does not exist -", file_path))
  }
  model_beta_coefficients <- read_csv(file_path)
  coefficients_list <- get_coefficients(model, model_beta_coefficients)
  coefficients <- coefficients_list$coefficients
  sum_product <- coefficients_list$sum_product
  if (basename(file) == 'barcelona_hf_v3_hosp_coefficients.csv') {
    sum_product_all_parameters <- sum(map_dbl(names(coefficients), function(parameter) {
      if (parameter %in% names(new_parameters)) {
        new_parameters[[parameter]] * coefficients[[parameter]]
      } else {
        0
      }
    }))
  } else {
  new_parameters_copy <- new_parameters
  new_parameters_copy$`Hospitalisation Prev. Year` <- as.logical(new_parameters_copy$`Hospitalisation Prev. Year`)
  sum_product_all_parameters <- sum(map_dbl(names(coefficients), function(parameter) {
    if (parameter %in% names(new_parameters_copy)) {
      new_parameters_copy[[parameter]] * coefficients[[parameter]]
    } else {
      0
    }
  }))
  }
  for (year in 1:5) {
    survival_estimate <- get_survival_estimate(model, year, model_beta_coefficients)
    score <- (1 - (survival_estimate ^ exp(sum_product_all_parameters - sum_product))) * 100
    scores <- c(scores, round(score, 1))
  }
  return(scores)
}

calc_life_expectancy <- function(model, new_parameters) {
  coefficients_life_expectancy <- '../resources/barcelona_hf_v3_life_expectancy_coefficients.csv'
  life_expectancy_limits <- '../resources/life_expectancy_limits.csv'
  if (!file.exists(coefficients_life_expectancy) || !file.exists(life_expectancy_limits)) {
    stop("Error: Life expectancy coefficient files do not exist in the resources folder.")
  }
  le_coefficients <- read_csv(coefficients_life_expectancy)
  le_limits <- read_csv(life_expectancy_limits)
  coefficients <- le_coefficients %>% filter(Variables != "Intercept" & Variables != "Gamma Value") %>% select(Variables, !!sym(model)) %>% deframe()
  new_parameters_copy <- new_parameters
  new_parameters_copy$`Hospitalisation Prev. Year` <- as.logical(new_parameters_copy$`Hospitalisation Prev. Year`)
  sum_product_all_parameters <- sum(map_dbl(names(coefficients), function(parameter) {
    if (parameter %in% names(new_parameters_copy)) {
      new_parameters_copy[[parameter]] * coefficients[[parameter]]
    } else {
      0
    }
  }))
  intercept <- le_coefficients %>% filter(Variables == "Intercept") %>% pull(!!sym(model))
  gamma_value <- le_coefficients %>% filter(Variables == "Gamma Value") %>% pull(!!sym(model))
  le <- exp(intercept + sum_product_all_parameters) * gamma_value
  key <- ifelse(new_parameters$Female, 'Women', 'Men')
  age <- as.integer(new_parameters$`Age (years)`)
  if (age %in% le_limits$Age) {
    if ((key == 'Men' && age > 63) || (key == 'Women' && age > 67)) {
      upper_limit <- le_limits %>% filter(Age == age) %>% pull(!!sym(key))
      if (le > as.numeric(upper_limit)) {
        le <- upper_limit
      }
    }
  }
  if (!is.na(le) && le > 20) {
    if (key == 'Men' && age <= 63) {
      le <- '>20'
    } else if (key == 'Women' && age <= 67) {
      le <- '>20'
    }
  }
  return(le)
}

round_life_expectancy <- function(model, parameters) {
  life_expectancy <- calc_life_expectancy(model, parameters)
  tryCatch({
    life_expectancy <- round(as.numeric(life_expectancy), 1)
  }, warning = function(w) {
    life_expectancy <- life_expectancy
  }, error = function(e) {
    life_expectancy <- life_expectancy
  })
  return(life_expectancy)
}

calc_barcelona_hf_score <- function(parameters) {
  all_scores <- list()
  coefficients_death_file <- '../resources/barcelona_hf_v3_death_coefficients.csv'
  coefficients_hosp_file <- '../resources/barcelona_hf_v3_hosp_coefficients.csv'
  coefficients_hosp_death_file <- '../resources/barcelona_hf_v3_hosp_death_coefficients.csv'
  model <- get_model(parameters)
  
  for (param in MIN_MAX_MEDIAN$variable) {
    if (param %in% names(parameters)) {
      parameters[[param]] <- check_values(parameters[[param]], param, MIN_MAX_MEDIAN)
    }
  }
  
  new_parameters <- get_new_parameters(parameters)
  endpoints_without_biomarkers <- list()
  endpoints_with_biomarkers <- list()
  for (file in c(coefficients_death_file, coefficients_hosp_file, coefficients_hosp_death_file)) {
    file_path <- file
    if (!file.exists(file_path)) {
      stop(paste("Error: File does not exist -", file_path))
    }
    suffix <- sub('barcelona_hf_v3_', '', sub('_coefficients.csv', '', basename(file)))
    scores_without_biomarkers <- get_scores(file_path, Model$MODEL_1, new_parameters)
    endpoints_without_biomarkers[[suffix]] <- scores_without_biomarkers
    
    if (model != Model$MODEL_1) {
      scores_with_biomarkers <- get_scores(file_path, model, new_parameters)
      endpoints_with_biomarkers[[suffix]] <- scores_with_biomarkers
    }
    
    if (basename(file) == 'barcelona_hf_v3_death_coefficients.csv') {
      le_without_biomarkers <- round_life_expectancy(Model$MODEL_1, new_parameters)
      endpoints_without_biomarkers[['life_expectancy']] <- as.character(le_without_biomarkers)
      
      if (model != Model$MODEL_1) {
        le_with_biomarkers <- round_life_expectancy(model, new_parameters)
        endpoints_with_biomarkers[['life_expectancy']] <- as.character(le_with_biomarkers)
      }
    }
  }
  
  all_scores[['without_biomarkers']] <- endpoints_without_biomarkers
  if (model != Model$MODEL_1) {
    all_scores[['with_biomarkers']] <- endpoints_with_biomarkers
  }
  
  return(all_scores)
}
# 
# parameters2 <- list(
#   `Age (years)` = 40,
#   `Female` = TRUE,
#   `NYHA Class` = 11,
#   `Sodium (mmol/L)` = 10,
#   `eGFR in mL/min/1.73m²` = 6,
#   `Hemoglobin (g/dL)` = 12,
#   `Loop Diuretic Furosemide Dose` = 20,
#   `Statin` = TRUE,
#   `ACEi/ARB` = FALSE,
#   `Betablocker` = FALSE,
#   `HF Duration in months` = 1,
#   `Diabetes Mellitus` = FALSE,
#   `Hospitalisation Prev. Year` = 0,
#   `MRA` = FALSE,
#   `ICD` = FALSE,
#   `CRT` = FALSE,
#   `ARNI` = FALSE,
#   `NT-proBNP in pg/mL` = NA,
#   `hs-cTnT in ng/L` = 1.112,
#   `ST2 (ng/mL)` = 4,
#   `SGLT2i` = FALSE
# )
# 
# example_scores <- calc_barcelona_hf_score(parameters2)
