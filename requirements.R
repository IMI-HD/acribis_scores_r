packages <- c("shiny", "dplyr", "purrr", "readr", "tibble")

install_if_missing <- function(pack) {
  if (!require(pack, character.only = TRUE)) {
    install.packages(pack, dependencies = TRUE)
  }
}

invisible(sapply(packages, install_if_missing))