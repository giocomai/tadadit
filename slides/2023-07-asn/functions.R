integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    unique(breaks)
  }
  return(fxn)
}

create_barchart <- function(corpus,
                            pattern,
                            website,
                            language,
                            metadata,
                            collect = TRUE,
                            period = "year") {

  if (collect == TRUE) {
    count_df <-  corpus |> 
      dplyr::collect() |> 
      cas_count(pattern = pattern)
  } else {
    count_df <-  corpus |> 
      cas_count(pattern = pattern)
  }

  pattern_text <- stringr::str_replace_all(string = pattern, pattern = stringr::fixed("[[:alpha:]]+"), replacement = "*")
  
  if (website ==   "Komsomolskaya Pravda") {
    title_text <- glue::glue("Number of references per year to {sQuote(pattern_text)} on the politics section of {website}")
  } else {
    title_text <- glue::glue("Number of references per year to {sQuote(pattern_text)} on {website}")
  }

  
  count_df |> 
    cas_summarise(period = period,
                  auto_convert = TRUE) %>% 
    dplyr::mutate(alpha = dplyr::if_else(date==2023, 0.9, 1)) |> 
    ggplot2::ggplot(mapping = ggplot2::aes(x = date, y = n, alpha = alpha)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(name = "",
                                breaks = integer_breaks(n = 10)) + #https://stackoverflow.com/questions/15622001/how-to-display-only-integer-values-on-an-axis-using-ggplot2
    ggplot2::scale_y_continuous(name = "", labels = scales::number) +
    ggplot2::scale_alpha_continuous(range = c(0.6, 1), guide = NULL) +
    ggplot2::labs(
      title = title_text,
      subtitle = stringr::str_c(
        "Based on ",
        scales::number(metadata$total_items),
        " items published in ",
        language, 
        " between ",
        format.Date(x = metadata$start_date, "%d %B %Y"), 
        " and ",
        format.Date(x = metadata$end_date, "%d %B %Y"),
        "\nQuery: ",
        sQuote(pattern_text)),
      caption = "Data processing: Giorgio Comai (OBCT/CCI) / tadadit.xyz") +
    ggplot2::theme_minimal(base_family = "Roboto Condensed") +
    ggplot2::theme(legend.position = "none",
                   text = ggplot2::element_text(size = 14))
}


### Kwic ####


kwic_table <- function(corpus,
                       pattern) {
  kwic_df <- cas_kwic(corpus = corpus,
           pattern = pattern,
           same_sentence = TRUE,
           regex = TRUE) |>  
    dplyr::select(date, title, before, pattern, after, url) %>% 
    dplyr::mutate(title = stringr::str_c("<a href='", url, "'>", title, "</a>")) %>% 
    dplyr::select(-url) %>% 
    dplyr::mutate(date = as.Date(date)) |> 
    dplyr::arrange(date) 
    # dplyr::mutate(pattern = stringr::str_c(pattern, " ", stringr::str_extract(string = after, pattern = "[[:graph:]]+")), 
    #        after = stringr::str_remove(string = after, pattern = "[[:graph:]]+"))
  
  
  kwic_df %>% 
    reactable::reactable(filterable = TRUE,
                         highlight = TRUE, 
                         resizable = TRUE,
                         defaultPageSize = 5,
                         columns = list(
                           date = reactable::colDef(maxWidth = 130),
                           title = reactable::colDef(html = TRUE),
                           before = reactable::colDef(align = "right",
                                                      minWidth = 100),
                           pattern = reactable::colDef(maxWidth = 180),
                           after = reactable::colDef(minWidth = 100)
                         ))
  
}
