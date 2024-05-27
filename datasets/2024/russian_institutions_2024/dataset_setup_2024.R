library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("castarter")


if (website_name == "zavtra.ru_ru") {
  cas_set_options(
    base_folder = fs::path(fs::path_home_r(), 
                           "R",
                           "castarter_2024"),
    project = "Russian media",
    website = website_name
  )
  base_explore_link <- "https://explore.tadadit.xyz/2024/"
} else {
  cas_set_options(
    base_folder = fs::path(fs::path_home_r(), 
                           "R",
                           "castarter_2024"),
    project = "Russian institutions",
    website = website_name 
  )
  base_explore_link <- "https://explore.tadadit.xyz/2024/ru_institutions_2024/"
}


corpus_name <- stringr::str_c(website_name, "_2024")

knitr::opts_chunk$set(echo = FALSE,
                      fig.width = 8,
                      fig.height = 4.5)

ggplot2::theme_set(
  new = theme_minimal(
    base_family = "Roboto Condensed")
)

end_date <- as.Date("2023-12-31")

library("piggyback")
options(piggyback.verbose = FALSE)
base_folder <- fs::dir_create(fs::path(fs::path_home_r(), "R", "ru_institutions_2024"))

get_corpus <- function(corpus_name) {
  pb_download(dest = base_folder, 
              repo = "giocomai/tadadit",
              tag = corpus_name, 
              file = stringr::str_c(corpus_name, ".csv.gz", collapse = ""))
  invisible(fs::path(base_folder, stringr::str_c(corpus_name, ".csv.gz", collapse = "")))
}

corpus_df <- readr::read_csv(file = get_corpus(corpus_name = corpus_name))

description_string <- paste(description_string_01, description_string_02, collapse = " ")

summary_stats_text <- function() {
  cat(
    stringr::str_c(
      paste("**Dataset name**:", corpus_name),
      paste("**Dataset description**:", description_string),
      paste("**Start date**:", min(corpus_df$date)),
      paste("**End date**:", max(corpus_df$date)),
      paste("**Total items**:", scales::number(nrow(corpus_df))),
      paste("**Available columns**:",  paste(colnames(corpus_df), collapse = "; ")),
      paste("**License**:", license_string),
      stringr::str_c("**Link for download**: [", corpus_name, "](https://github.com/giocomai/tadadit/releases/tag/", corpus_name, ")"),
      sep = "\n\n")
  )
  }

items_per_year <- function() {
  corpus_year_df <- corpus_df |>
    mutate(year = lubridate::year(date)) |> 
    count(year) 
  
  n_breaks <- min(max(corpus_year_df[["year"]])-min(corpus_year_df[["year"]]), 10)
  
corpus_year_df |> 
    ggplot(mapping = aes(x = year, y = n)) +
    geom_col() +
    scale_y_continuous(name = "", labels = scales::number) +
    scale_x_continuous(name = "", breaks = scales::pretty_breaks(n = n_breaks)) +
    labs(
      title = paste("Number of items per year published on", description_string_02, collapse = " "),
      subtitle = stringr::str_c(
        "Based on ",
        scales::number(nrow(corpus_df)),
        " items published between ",
        format.Date(x = min(corpus_df$date), "%e %B %Y"), 
        " and ",
        format.Date(x = max(corpus_df$date), "%e %B %Y")) |> 
        stringr::str_squish(),
      caption = stringr::str_c("Source: Giorgio Comai / tadadit.xyz / ", corpus_name)
    ) 
}

words_per_year <- function() {
  words_per_day_df <- corpus_df |> 
    cas_count_total_words() |> 
    mutate(date = lubridate::as_date(date),
           pattern = "total words")
  
  corpus_year_df <-   words_per_day_df |> 
    cas_summarise(period = "year", auto_convert = TRUE) |>
    rename(year = date) 
  
  n_breaks <- min(max(corpus_year_df[["year"]], na.rm = TRUE)-min(corpus_year_df[["year"]], na.rm = TRUE), 10)
  
  corpus_year_df |> 
    ggplot(mapping = aes(x = year, y = n)) +
    geom_col() +
    scale_y_continuous(name = "", labels = scales::number) +
    scale_x_continuous(name = "", breaks = n_breaks) +
    labs(title = paste("Number of words per year published on", description_string_02, collapse = " "),
         subtitle = stringr::str_c("Based on ",
                                   scales::number(nrow(corpus_df)),
                                   " items published between ",
                                   format.Date(x = min(corpus_df$date), "%e %B %Y"), 
                                   " and ",
                                   format.Date(x = max(corpus_df$date), "%e %B %Y")) |> 
           stringr::str_squish(),
         caption = stringr::str_c("Source: Giorgio Comai / tadadit.xyz / ", corpus_name)
         )
}


missing_table <- function() {
  
  missing_long_df <- corpus_df |> 
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) {is.na(x)|(as.character(x)=="")})) |> 
    tidyr::pivot_longer(dplyr::everything(), names_to = "field", values_to = "missing") |> 
    dplyr::group_by(field, missing) |> 
    dplyr::count() |> 
    dplyr::mutate(missing = dplyr::if_else(missing, "missing", "present")) |> 
    dplyr::ungroup()
  
  missing_df <- missing_long_df |> 
    tidyr::pivot_wider(names_from = missing, values_from = n) |> 
    tidyr::replace_na(list(present = 0, missing = 0)) |> 
    dplyr::mutate(missing_share = scales::percent(missing/(present+missing), trim = FALSE,accuracy = 0.1)) 
  
  tibble::tibble(field = colnames(corpus_df)) |> 
   # dplyr::filter(!(field %in% c("doc_id", "date"))) |> 
    dplyr::left_join(y = missing_df, by = "field") |> 
    knitr::kable(format.args = list(big.mark = " "))
}

missing_graph <- function() {
  corpus_df |> 
  dplyr::mutate(dplyr::across(-date, \(x) {is.na(x)|(as.character(x)=="")})) |> 
  tidyr::pivot_longer(-date, names_to = "field", values_to = "missing") |> 
  dplyr::mutate(field = factor(field, rev(colnames(corpus_df)))) |>
  dplyr::filter(missing) |> 
  ggplot2::ggplot(mapping = ggplot2::aes(x = date, y = field)) +
  ggplot2::geom_point(shape = "|", size = 0.8, alpha = 0.2) +
  ggplot2::scale_y_discrete(name = NULL, drop = FALSE) +
  ggplot2::scale_x_date(name = NULL) +
  ggplot2::labs(
    title = stringr::str_c("Missing metadata on ",
                           description_string_02,
                           collapse = " "),
    subtitle = "A thin line represents an empty field",
    caption = stringr::str_c("Source: Giorgio Comai / tadadit.xyz / ", corpus_name))
}

download_callout <- function() {
  cat(
    stringr::str_c(
      "::: {.callout-tip title='Explore or download this dataset'}",
      stringr::str_c("**[Explore in an interactive web interface](", base_explore_link, corpus_name, ")**", collapse = ""),
      stringr::str_c("**Links for download**: ", "[compressed csv](https://github.com/giocomai/tadadit/releases/download/", corpus_name, "/", corpus_name, ".csv.gz) / ", "[ods](https://github.com/giocomai/tadadit/releases/download/", corpus_name, "/", corpus_name, ".ods)", collapse = ""),
     ":::",
      sep = "\n\n"
    )
  )
}

