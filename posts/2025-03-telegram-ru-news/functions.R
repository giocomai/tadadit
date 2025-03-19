read_telegram_json <- function(path) {
  channel_l <- yyjsonr::read_json_file(filename = path)
  
  channel_full_df <- channel_l[["messages"]] |> 
    tibble::as_tibble()
  
  channel_info_df <- tibble::tibble(
    channel_name = channel_l[["name"]],
    channel_type = channel_l[["type"]],
    channel_id = channel_l[["id"]]
    ) 
  
  dplyr::bind_cols(channel_info_df,
                   channel_full_df)
}


read_telegram_messages <- function(telegram_df) {
  
  telegram_df |> 
    dplyr::filter(type == "message") |> 
    dplyr::mutate(text =  purrr::map_chr(
      .x = text_entities,
      .f = function(x) {
        if (length(x)==0) {
          NA_character_
        } else if (nrow(x)==0) {
          NA_character_
        } else {
          x[["text"]] |> 
            stringr::str_flatten(collapse = " ", na.rm = TRUE) 
        }
      }
    )
    ) |> 
    dplyr::mutate(datetime = lubridate::as_datetime(as.numeric(date_unixtime))) |> 
    dplyr::mutate(date = lubridate::as_date(datetime)) |> 
    tidyr::unite(col = "doc_id", channel_name, id, sep = "_", remove = FALSE, na.rm = FALSE) |>
    dplyr::select(doc_id,
                  text,
                  date, 
                  datetime,
                  id,
                  reply_to_message_id,
                  channel_name,
                  channel_id
                  ) 
}


