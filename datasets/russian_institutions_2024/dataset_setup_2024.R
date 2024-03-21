library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("castarter")

knitr::opts_chunk$set(echo = FALSE,
                      fig.width = 8,
                      fig.height = 4.5)

ggplot2::theme_set(
  new = theme_minimal(
    base_family = "Roboto Condensed")
)

end_date <- as.Date("2023-12-31")
