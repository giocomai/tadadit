library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("castarter")

description_string <- paste(
  description_string_01,
  description_string_02,
  collapse = " "
)

if (website_name == "zavtra.ru_ru") {
  cas_set_options(
    base_folder = fs::path(fs::path_home_r(), "R", "castarter_2025"),
    project = "Russian media",
    website = website_name
  )
  base_explore_link <- "https://explore.tadadit.xyz/2025/"
} else {
  cas_set_options(
    base_folder = fs::path(fs::path_home_r(), "R", "castarter_2025"),
    project = "Russian institutions",
    website = website_name
  )
  base_explore_link <- "https://explore.tadadit.xyz/2025/ru_institutions_2025/"
}


corpus_name <- stringr::str_c(website_name, "_2025")

knitr::opts_chunk$set(echo = FALSE, fig.width = 8, fig.height = 4.5)

ggplot2::theme_set(
  new = theme_minimal(
    base_family = "Roboto Condensed"
  )
)

end_date <- as.Date("2024-12-31")

library("piggyback")
options(piggyback.verbose = FALSE)
base_folder <- fs::dir_create(fs::path(
  fs::path_home_r(),
  "R",
  "ru_institutions_2025"
))

get_corpus <- function(corpus_name) {
  pb_download(
    dest = base_folder,
    repo = "giocomai/tadadit",
    tag = corpus_name,
    file = stringr::str_c(corpus_name, ".csv.gz", collapse = "")
  )
  invisible(fs::path(
    base_folder,
    stringr::str_c(corpus_name, ".csv.gz", collapse = "")
  ))
}

corpus_df <- readr::read_csv(file = get_corpus(corpus_name = corpus_name))
