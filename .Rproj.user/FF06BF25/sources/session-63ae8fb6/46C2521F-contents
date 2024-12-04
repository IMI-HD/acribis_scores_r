POINTS <- list(
  `Congestive heart failure/LV dysfunction` = 1,
  `Hypertension` = 1,
  `Age ≥75y` = 2,
  `Diabetes mellitus` = 1,
  `Stroke/TIA/TE` = 2,
  `Vascular diseases` = 1,
  `Age 65-74y` = 1,
  `Sex category` = 1
)

calc_chads_vasc_score <- function(parameters) {
  if (parameters$`Age ≥75y` && parameters$`Age 65-74y`) {
    stop("Not both age parameters can be true!")
  }
  
  score <- sum(sapply(names(parameters), function(param) parameters[[param]] * POINTS[[param]]))
  return (score)
}

parameters <- list(
  `Congestive heart failure/LV dysfunction` = TRUE,
  `Hypertension` = TRUE,
  `Age ≥75y` = FALSE,
  `Diabetes mellitus` = TRUE,
  `Stroke/TIA/TE` = FALSE,
  `Vascular diseases` = TRUE,
  `Age 65-74y` = TRUE,
  `Sex category` = TRUE
)

calc_chads_vasc_score(parameters)
