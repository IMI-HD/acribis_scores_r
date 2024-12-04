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
