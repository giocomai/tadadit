summary_stats_text <- function() {
  cat(
    stringr::str_c(
      paste("**Dataset name**:", corpus_name),
      paste("**Dataset description**:", description_string),
      paste("**Start date**:", min(corpus_df$date)),
      paste("**End date**:", max(corpus_df$date)),
      paste("**Total items**:", scales::number(nrow(corpus_df))),
      paste(
        "**Available columns**:",
        paste(colnames(corpus_df), collapse = "; ")
      ),
      paste("**License**:", license_string),
      stringr::str_c(
        "**Link for download**: [",
        corpus_name,
        "](https://github.com/giocomai/tadadit/releases/tag/",
        corpus_name,
        ")"
      ),
      sep = "\n\n"
    )
  )
}

items_per_year <- function() {
  corpus_year_df <- corpus_df |>
    mutate(year = lubridate::year(date)) |>
    count(year)

  n_breaks <- min(
    max(corpus_year_df[["year"]]) - min(corpus_year_df[["year"]]),
    10
  )

  corpus_year_df |>
    ggplot(mapping = aes(x = year, y = n)) +
    geom_col() +
    scale_y_continuous(name = NULL, labels = scales::number) +
    scale_x_continuous(
      name = NULL,
      breaks = scales::pretty_breaks(n = n_breaks)
    ) +
    labs(
      title = paste(
        "Number of items per year published on",
        description_string_02,
        collapse = " "
      ),
      subtitle = stringr::str_c(
        "Based on ",
        scales::number(nrow(corpus_df)),
        " items published between ",
        format.Date(x = min(corpus_df$date), "%e %B %Y"),
        " and ",
        format.Date(x = max(corpus_df$date), "%e %B %Y")
      ) |>
        stringr::str_squish(),
      caption = stringr::str_c(
        "Source: Giorgio Comai / tadadit.xyz / ",
        corpus_name
      )
    )
}

words_per_year <- function() {
  words_per_day_df <- corpus_df |>
    cas_count_total_words() |>
    mutate(date = lubridate::as_date(date), pattern = "total words")

  corpus_year_df <- words_per_day_df |>
    cas_summarise(period = "year", auto_convert = TRUE) |>
    rename(year = date)

  n_breaks <- min(
    max(corpus_year_df[["year"]], na.rm = TRUE) -
      min(corpus_year_df[["year"]], na.rm = TRUE),
    10
  )

  corpus_year_df |>
    ggplot(mapping = aes(x = year, y = n)) +
    geom_col() +
    scale_y_continuous(name = NULL, labels = scales::number) +
    scale_x_continuous(
      name = NULL,
      breaks = scales::pretty_breaks(n = n_breaks)
    ) +
    labs(
      title = paste(
        "Number of words per year published on",
        description_string_02,
        collapse = " "
      ),
      subtitle = stringr::str_c(
        "Based on ",
        scales::number(nrow(corpus_df)),
        " items published between ",
        format.Date(x = min(corpus_df$date), "%e %B %Y"),
        " and ",
        format.Date(x = max(corpus_df$date), "%e %B %Y")
      ) |>
        stringr::str_squish(),
      caption = stringr::str_c(
        "Source: Giorgio Comai / tadadit.xyz / ",
        corpus_name
      )
    )
}


missing_table <- function() {
  missing_long_df <- corpus_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) {
      is.na(x) | (as.character(x) == "")
    })) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "field",
      values_to = "missing"
    ) |>
    dplyr::group_by(field, missing) |>
    dplyr::count() |>
    dplyr::mutate(missing = dplyr::if_else(missing, "missing", "present")) |>
    dplyr::ungroup()

  missing_wide_df <- missing_long_df |>
    tidyr::pivot_wider(names_from = missing, values_from = n)

  if (is.element("missing", colnames(missing_wide_df)) == FALSE) {
    missing_wide_df <- missing_wide_df |>
      dplyr::mutate(missing = 0L)
  }

  missing_df <- missing_wide_df |>
    tidyr::replace_na(list(present = 0, missing = 0)) |>
    dplyr::mutate(
      missing_share = scales::percent(
        missing / (present + missing),
        trim = FALSE,
        accuracy = 0.1
      )
    )

  tibble::tibble(field = colnames(corpus_df)) |>
    # dplyr::filter(!(field %in% c("doc_id", "date"))) |>
    dplyr::left_join(y = missing_df, by = "field") |>
    knitr::kable(format.args = list(big.mark = " "))
}

missing_graph <- function() {
  missing_df <- corpus_df |>
    dplyr::mutate(dplyr::across(-date, \(x) {
      is.na(x) | (as.character(x) == "")
    })) |>
    tidyr::pivot_longer(-date, names_to = "field", values_to = "missing") |>
    dplyr::mutate(field = factor(field, rev(colnames(corpus_df)))) |>
    dplyr::filter(missing)

  if (nrow(missing_df) == 0) {
    fields_v <- corpus_df |>
      dplyr::select(-doc_id, -date) |>
      colnames()

    missing_df <- corpus_df |>
      dplyr::distinct(date) |>
      dplyr::mutate(field = list(fields_v)) |>
      tidyr::unnest(field) |>
      dplyr::mutate(missing = NA)

    missing_gg <- missing_df |>
      ggplot2::ggplot(mapping = ggplot2::aes(x = date, y = field)) +
      #ggplot2::geom_point(shape = "|", size = 0.8, alpha = 0.2) +
      ggplot2::scale_y_discrete(name = NULL, drop = FALSE) +
      ggplot2::scale_x_date(name = NULL) +
      ggplot2::labs(
        title = stringr::str_c(
          "Missing metadata on ",
          description_string_02,
          collapse = " "
        ),
        subtitle = "A thin line represents an empty field",
        caption = stringr::str_c(
          "Source: Giorgio Comai / tadadit.xyz / ",
          corpus_name
        )
      )
  } else {
    missing_gg <- missing_df |>
      ggplot2::ggplot(mapping = ggplot2::aes(x = date, y = field)) +
      ggplot2::geom_point(shape = "|", size = 0.8, alpha = 0.2) +
      ggplot2::scale_y_discrete(name = NULL, drop = FALSE) +
      ggplot2::scale_x_date(name = NULL) +
      ggplot2::labs(
        title = stringr::str_c(
          "Missing metadata on ",
          description_string_02,
          collapse = " "
        ),
        subtitle = "A thin line represents an empty field",
        caption = stringr::str_c(
          "Source: Giorgio Comai / tadadit.xyz / ",
          corpus_name
        )
      )
  }

  missing_gg
}

download_callout <- function() {
  cat(
    stringr::str_c(
      "::: {.callout-tip title='Explore or download this dataset'}",
      stringr::str_c(
        "**[Explore in an interactive web interface](",
        base_explore_link,
        corpus_name,
        ")**",
        collapse = ""
      ),
      stringr::str_c(
        "**Links for download**: ",
        "[compressed csv](https://github.com/giocomai/tadadit/releases/download/",
        corpus_name,
        "/",
        corpus_name,
        ".csv.gz) / ",
        "[ods](https://github.com/giocomai/tadadit/releases/download/",
        corpus_name,
        "/",
        corpus_name,
        ".ods)",
        collapse = ""
      ),
      stringr::str_c("[See other datasets that are part of this release](..)"),
      ":::",
      sep = "\n\n"
    )
  )
}
