process_telegram_json <- function(path) {
  result_l <- jsonlite::fromJSON(txt = fs::path(base_folder, "result.json"))
  
  
  messages_df <- result_l$messages |> 
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
                    "reply_to_message_id", 
                    "forwarded_from"
    )) 
  messages_df
}


#### Transcribe audio #####



transcribe_audio <- function(messages_df,
                             base_folder,
                             destination_folder,
                             model) {
  model_whisper <- audio.whisper::whisper(x = model, model_dir = "/home/g/bin/whisper_model")
  
  destination_folder_by_model <- stringr::str_c(destination_folder, "_", model)
  
  to_transcribe_l <- messages_df |> 
    dplyr::arrange(dplyr::desc(date_unixtime)) |> 
    dplyr::select(c("id", "file")) |> 
    dplyr::filter(is.na(file)==FALSE) |> 
    dplyr::mutate(extension = fs::path_ext(file)) |> 
    # dplyr::distinct(extension)
    dplyr::filter(extension %in% c("mp4", "MP4", "wav", "ogg", "mp3", "MOV")) |> 
    purrr::transpose()
  
  fs::dir_create(destination_folder_by_model)
  
  purrr::walk(
    .x = to_transcribe_l,
    .progress = TRUE,
    .f = function(x) {
      expected_file <- fs::path(destination_folder_by_model,
                                fs::path_ext_set(path = as.character(x$id), ext = "rds"))
      
      if (fs::file_exists(expected_file)==FALSE) {
        
        current_wav <- fs::path_ext_set(fs::file_temp(), "wav")
        
        av::av_audio_convert(audio = fs::path(base_folder, x$file),
                             output = current_wav,
                             format = "wav",
                             sample_rate = 16000)
        
        transcript_original <- audio.whisper:::predict.whisper(object = model_whisper,
                                                               newdata = current_wav,
                                                               #language = "ru",
                                                               trim = FALSE,
                                                               translate = FALSE)
        
        saveRDS(object = transcript_original, file = expected_file)
      }
      
    })
  
}


#### OCR images #####


#destination_folder = "prigozhin_photo_transcribed"

ocr_images <- function(messages_df,
                       base_folder,
                       destination_folder,
                       engine = "rus") {
  
  engine <- tesseract::tesseract("rus")
  
  
  to_transcribe_l <- messages_df |> 
    dplyr::arrange(dplyr::desc(date_unixtime)) |> 
    dplyr::select(c("id", "photo")) |> 
    dplyr::filter(is.na(photo)==FALSE) |> 
    purrr::transpose()
  
  fs::dir_create(destination_folder)
  
  purrr::walk(
    .x = to_transcribe_l,
    .progress = TRUE,
    .f = function(x) {
      expected_file <- fs::path(destination_folder,
                                fs::path_ext_set(path = as.character(x$id), ext = "rds"))
      
      if (fs::file_exists(expected_file)==FALSE) {
        
        current_pic <- fs::path(base_folder, x$photo)
        
        transcript_original <-  tesseract::ocr(image = current_pic, engine = engine)
        
        saveRDS(object = transcript_original, file = expected_file)
      }
      
    })
  
}

#### Barchart functions - copy/pasted #####


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
                      regex = TRUE,
                      ignore_case = FALSE) |>  
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
