
### functions ####

read_telegram <- function(path) {
  
  all_messages_l <- jsonlite::fromJSON(path)
  
  messages_df <- all_messages_l$messages |> 
    tibble::as_tibble() |> 
    dplyr::mutate(text =  purrr::map_chr(
      .x = text_entities,
      .f = function(x) {
        if (nrow(x)==0) {
          ""
        } else {
          x |> 
            tibble::as_tibble() |> 
            dplyr::pull(text) |> 
            stringr::str_c(collapse = "\n") 
        }
      }
    )
    ) |> 
    dplyr::mutate(datetime = lubridate::as_datetime(as.numeric(date_unixtime))) |> 
    dplyr::mutate(date = lubridate::as_date(datetime)) |> 
    dplyr::select(c("id",
                    "date_unixtime",
                    "datetime",
                    "date",
                    "text", 
                    "photo", 
                    "file", 
                    "media_type",
                    "mime_type",
                    "duration_seconds",
                    "reply_to_message_id"
    )) 
  messages_df
}

oll_annotate_telegram <- function(messages, 
                                  db) {
  purrr::map(
    .progress = TRUE, 
    .x = purrr::list_transpose(x = as.list(messages), simplify = FALSE),
    .f = function(current_item) {
      Sys.sleep(5) # probably ridiculous, but just in case, some pause to slightly reduce overheating
      cli::cat_rule()
      
      cat(current_item[["text"]])
      
      user_text <- stringr::str_c("Text: ", current_item[["text"]])
      
      q <- tibble::tribble(
        ~role,    ~content,
        "system", current_item[["instruction"]],
        "user",   user_text
      )
      
      output <- query(q = q,
                      model =current_item[["model"]])
      
      result_df <- tibble::tibble(message_id = current_item[["message_id"]],
                                  instruction = current_item[["instruction"]],
                                  text = user_text, 
                                  response = purrr::pluck(output, "message", "content"),
                                  model = purrr::pluck(output, "model"),
                                  created_at = purrr::pluck(output, "created_at"),
                                  total_duration = purrr::pluck(output, "total_duration")) 
      
      DBI::dbWriteTable(conn = db,
                        name = "telegram",
                        value = result_df,
                        append = TRUE)
      result_df
    }
  ) |> 
    purrr::list_rbind()
}