create_values_summary_wide <- function(all_df, 
                                       values_df) {
  
  dplyr::bind_rows(
    all_df |>
      dplyr::mutate(year = lubridate::year(date)) |> 
      dplyr::group_by(year) |> 
      dplyr::count() |>
      dplyr::ungroup() |> 
      dplyr::arrange(year) |> 
      dplyr::collect() |> 
      dplyr::mutate(type = "all"),
    values_df |>
      dplyr::mutate(year = lubridate::year(date)) |> 
      dplyr::group_by(year) |> 
      dplyr::count() |>
      dplyr::ungroup() |> 
      dplyr::arrange(year) |> 
      dplyr::collect() |> 
      dplyr::mutate(type = "values")
  ) |> 
    tidyr::pivot_wider(names_from = type,
                       values_from = n,
                       values_fill = 0) 
}


cas_highlight <- function(corpus, 
                          pattern,
                          text = text, 
                          colour = "#FFEC8B") {
  h_text_v <- purrr::reduce2(
    .x = pattern,
    .y = colour,
    .init = corpus |> 
      dplyr::pull( {{text}} , as_vector = TRUE),
    .f = function(original_text, current_pattern, current_colour) {
      #cli::cli_progress_message("Processing: {current_pattern}", clear = FALSE)
      purrr::map_chr(
        .progress = stringr::str_c("Highlighting: ", current_pattern), 
        .x =  original_text,
        .f = function(current_text) {
          all_matches <- current_text |> 
            stringr::str_extract_all(
              pattern = stringr::regex(current_pattern,
                                       ignore_case = TRUE),
              simplify = TRUE) |> 
            as.character()
          
          if (length(all_matches)==0) {
            return(current_text)
          }
          
          if (current_pattern!="values") {
            replacement_v <- stringr::str_c(
              "<span style='background-color:", 
              current_colour,
              "'>", 
              all_matches, 
              "</span>")
          } else {
            replacement_v <- stringr::str_c(
              "<span style='background-color:", 
              current_colour,
              "'><strong>", 
              all_matches, 
              "</strong></span>")
          }
          
          
          names(replacement_v) <- all_matches
          
          stringr::str_replace_all(string = current_text,
                                   pattern = replacement_v)
        })
    }
    
  )
  
  
  corpus |>
    dplyr::mutate({{ text }} := h_text_v)
  
}



create_values_type_count <- function(values_df, 
                                     language = "ru") {
  
  pattern <- stringr::str_c("pattern", "_", language)
  
  values_df |> 
    dplyr::collect() |> 
    dplyr::mutate(
      Western = stringr::str_count(string = text,
                                   pattern = matches_df |> 
                                     dplyr::filter(type == "Western") |> 
                                     dplyr::pull(pattern) |> 
                                     stringr::str_c(collapse = "|") |> 
                                     stringr::regex(ignore_case = TRUE)),
      traditional = stringr::str_count(string = text,
                                       pattern = matches_df |> 
                                         dplyr::filter(type == "traditional") |> 
                                         dplyr::pull(pattern) |> 
                                         stringr::str_c(collapse = "|") |> 
                                         stringr::regex(ignore_case = TRUE)),
      universal  = stringr::str_count(string = text,
                                      pattern = matches_df |> 
                                        dplyr::filter(type == "universal") |> 
                                        dplyr::pull(pattern) |> 
                                        stringr::str_c(collapse = "|") |> 
                                        stringr::regex(ignore_case = TRUE)),
      common  = stringr::str_count(string = text,
                                   pattern = matches_df |> 
                                     dplyr::filter(type == "common") |> 
                                     dplyr::pull(pattern) |> 
                                     stringr::str_c(collapse = "|") |> 
                                     stringr::regex(ignore_case = TRUE)),
      negative  = stringr::str_count(string = text,
                                     pattern = matches_df |> 
                                       dplyr::filter(type == "negative") |> 
                                       dplyr::pull(pattern) |> 
                                       stringr::str_c(collapse = "|") |> 
                                       stringr::regex(ignore_case = TRUE))
    ) |> 
    dplyr::mutate(year = lubridate::year(date)) |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(dplyr::across(is.numeric, sum))
}


create_values_type_count_long <- function(df) {
  
  values_type_count_long_df <- df |> 
    tidyr::pivot_longer(cols = -1,
                        names_to = "type",
                        values_to = "n") 
  
  ordered_types_v <- values_type_count_long_df |> 
    dplyr::filter(year == 2022) |> 
    dplyr::arrange(n) |> 
    dplyr::pull(type) 
  
  values_type_count_long_df <- values_type_count_long_df |> 
    dplyr::mutate(type = factor(type,
                                levels = ordered_types_v))
}




create_values_last_word <- function(values_df, 
                                    values_type_count_long_df,
                                    language = "ru") {
  
  pattern <- stringr::str_c("pattern", "_", language)
  
  if (language == "en") {
    value_string <- "values"
  } else if (language == "ru") {
    value_string <- "ценност"
  }
  
  ordered_types_v <- values_type_count_long_df |> 
    dplyr::filter(year == 2022) |> 
    dplyr::arrange(n) |> 
    dplyr::pull(type) 
  
  values_df |> 
    dplyr::collect() |> 
    dplyr::mutate(
      text = stringr::str_extract(string = text,
                                  pattern = stringr::regex(
                                    pattern = stringr::str_c("[[:alpha:]]+ ", value_string),
                                    ignore_case = TRUE))) |> 
    dplyr::select(date, text) |> 
    dplyr::mutate(id = dplyr::row_number()) |> 
    dplyr::mutate(text = stringr::str_remove(string = text,
                                             pattern = stringr::regex(stringr::str_c(" ",
                                                                                     value_string),
                                                                      ignore_case = TRUE))) |> 
    fuzzyjoin::regex_left_join(y = matches_df |> 
                                 dplyr::select("type", pattern),
                               by=c('text' = pattern),
                               ignore_case = TRUE) |> 
    dplyr::filter(is.na(type)==FALSE) |> 
    dplyr::distinct(id, .keep_all = TRUE) |> 
    dplyr::mutate(year = lubridate::year(date)) |> 
    dplyr::group_by(year) |> 
    dplyr::count(type) |>
    dplyr::mutate(total = sum(n)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(ratio = n/total) |> 
    dplyr::group_by(year) |> 
    dplyr::mutate(total_ratio = sum(ratio)) |> 
    dplyr::ungroup() |> 
    dplyr::filter(year<2023) |> 
    tidyr::complete(year, type, fill = list(ratio = 0)) |> 
    dplyr::mutate(type = factor(type, levels = ordered_types_v))
  
}