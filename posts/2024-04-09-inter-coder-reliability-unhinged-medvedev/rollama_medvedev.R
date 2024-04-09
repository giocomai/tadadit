library("rollama")
library("RSQLite")

source("functions.R")

fs::dir_create("medvedev")
path_db <- fs::path("medvedev", "medvedev.sqlite")
db <- RSQLite::dbConnect(drv = RSQLite::SQLite(), path_db)

path_telegram <- "medvedev/result.json"

messages_df <- read_telegram(path_telegram)

models_v <- c("mistral:7b-instruct",
              "llama2",
              "gemma:2b-instruct")

instruction_v <- c(
  `end with rating` = 
  "You rate texts. The original language of texts is Russian. You must rate on a scale from 1 to 10 the degree to which the given text is unhinged. Your rating of the unhinged level must be expressed in digits at the very end of your response. Each response must end with 'the rating is:' and then the rating in digits. There must be no mistake, your response must finish with the rating.",
  `rating only` = "You rate texts. The original language of texts is Russian. You must rate on a scale from 1 to 10 the degree to which the given text is unhinged. Your rating of the unhinged level must be expressed in the following format: 'the unhinged level is: <digit>'. This is all that must be in your response, there must be no mistake: 'the unhinged level is: <digit>'"
)



previous_df <- DBI::dbReadTable(conn = db, name = "telegram") |> 
  dplyr::collect() |> 
  tibble::as_tibble() |> 
  dplyr::filter(model %in% models_v)


combo_df <- tidyr::expand_grid(model = models_v, 
                               instruction = instruction_v)

to_do_df <- previous_df |> 
  dplyr::group_by(model, instruction, message_id) |> 
  dplyr::count() |> 
  dplyr::ungroup() |> 
  dplyr::filter(n<3) |> 
  dplyr::arrange(n, dplyr::desc(model), instruction, message_id) |> 
  dplyr::left_join(y = messages_df |> 
                     dplyr::rename(message_id = id),
                   by = "message_id")

### annotate #####

if (nrow(to_do_df)>0) {
  oll_annotate_telegram(messages = to_do_df,
                      db = db)
}

### end of annotation ####

previous_df <- DBI::dbReadTable(conn = db, name = "telegram") |> 
  dplyr::collect() |> 
  tibble::as_tibble() |> 
  dplyr::filter(model %in% models_v)

rating_premise_v <- c(
  "rating",
  "rating is",
  "unhinged level is",
  "rating of the unhinged level in this text is",
  "I would rate it as",
  "I would suggest a rating of",
  "I would rate the unhinged level",
  "receives a rating of",
  "unhinged level of this text is",
  "would rate this text a",
  "rates as",
  "rate the text as",
  "rating for this text is",
  "a low degree of unhingedness",
  "has a rating of",
  "would rate this text as",
  "The text's unhinged level",
  "unhinged level of the given text is",
  "my suggestion for the rating would be",
  "is rated",
  "The unhinged level",
  "would rate the unhinged level as",
  "unhinged level of the text is",
  "text is unhinged to a degree of",
  "I would rate this text as an",
  "unhinged to a degree of",
  "with the rating being",
  "rating for the given text is",
  "The text is a",
  "Рейтинг",
  "Рating",
  "rates as an unhinged level of",
  "I would rate its unhinged level as a ",
  "degree of unhinged level of ",
  "Unhinged level:",
  "degree of unhingedness as a ",
  "is:")

extract_pattern <- stringr::str_c(
  "(",
  stringr::str_c(
    rating_premise_v,
    collapse = "|"),
  ")",
  "[[:print:]]?[[:space:]]?([[:digit:]]+|[[:digit:]][[:punct:]][[:digit:]])",
  collapse = ""  
)

ratings_df <- previous_df |> 
  dplyr::mutate(
    rating = response |> 
      stringr::str_squish() |> 
      stringr::str_extract(
      pattern = stringr::regex(
        extract_pattern,
        ignore_case = TRUE)
      ) |>
      stringr::str_extract(pattern = "[[:digit:]]+|[[:digit:]][[:punct:]][[:digit:]]") |> 
      as.numeric()) |> 
  dplyr::mutate(rating = dplyr::if_else(is.na(rating)&stringr::str_starts(response, "[[:digit:]]"),
                                        as.numeric(stringr::str_extract(response, "(^[[:digit:]]+)|(^[[:digit:]][[:punct:]][[:digit:]])")), 
                                        rating)) |> 
  dplyr::mutate(rating = dplyr::if_else(rating>11,
                                        NA_real_, 
                                        rating)) |> 
  dplyr::arrange(model, instruction, message_id) |> 
  dplyr::group_by(model, instruction, message_id) |> 
  dplyr::mutate(iteration = dplyr::row_number()) |> 
  dplyr::group_by(model, instruction, iteration) |> 
  #  dplyr::slice_sample(prop = 1) |> 
  dplyr::mutate(coder_numeric = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(coder_id = stringr::str_c("coder", coder_numeric |> stringr::str_pad(width = 2, pad = "0"), sep = "_")) |> 
  dplyr::filter(iteration<4)



# ratings_df |>
#   dplyr::filter(is.na(rating)) 


mistral_ratings_df <- ratings_df |> 
  dplyr::filter(model == "mistral:7b-instruct") 
  
  

mistral_ratings_df |> 
  dplyr::distinct(message_id, .keep_all = TRUE) |> 
  dplyr::group_by(rating) |> 
  dplyr::count() |> 
  ggplot2::ggplot(mapping = ggplot2::aes(x = rating, y = n)) +
  ggplot2::geom_col()



ratings_only_df <- mistral_ratings_df |> 
  dplyr::group_by(message_id) |> 
#  dplyr::slice_sample(prop = 1) |> 
  dplyr::mutate(rater = dplyr::row_number()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(coder_id = stringr::str_c("coder", rater, sep = "_")) |> 
  dplyr::select(message_id, rating, coder_id)

ratings_only_df |> 
  dplyr::filter(coder_id == "coder_3") |> 
  dplyr::group_by(rating) |> 
  dplyr::count() |> 
  dplyr::ungroup() |> 
  ggplot2::ggplot(mapping = ggplot2::aes(x = rating, y = n)) +
  ggplot2::geom_col()

  # dplyr::mutate(rating = dplyr::case_when(
  #   stringr::str_detect(string = response, pattern = stringr::regex("The rating is: [[:digit:]]+|The rating is: [[:digit:]][[:punct:]][[:digit:]]|the unhinged level is: ", ignore_case = TRUE)) ~ stringr::str_extract(string = response, pattern = stringr::regex("The (rating|unhinged level|rating of the unhinged level in this text) is: [[:digit:]]+|The (rating|unhinged level|rating of the unhinged level in this text|rating of the unhinged level in this text) is: [[:digit:]][[:punct:]][[:digit:]]", ignore_case = TRUE)) |>
  #     stringr::str_extract(pattern = "[[:digit:]]+|[[:digit:]][[:punct:]][[:digit:]]") |> 
  #     as.numeric()
  # )) |> View()


ratings_only_df |> 
  tidyr::pivot_wider(names_from = coder_id, values_from = rating) |> 
  dplyr::mutate(difference = coder_1-coder_2) |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = difference)) +
  ggplot2::geom_histogram(binwidth = 1)



library("tidycomm")

ratings_only_df |> # Facebook post codings sample data
  dplyr::filter(coder_id == "coder_3"|coder_id == "coder_4"|coder_id == "coder_5") |>
  test_icr(message_id, coder_id, levels = c(rating = "ordinal"))

ratings_only_df |>
  dplyr::mutate(
    rating_nominal = dplyr::case_when(rating < 6 ~ "Not unhinged", 
                                      rating >= 6 ~ "Unhinged"),
    rating_ordinal = dplyr::case_when(rating < 3 ~ 1, 
                                      rating < 6 ~ 2, 
                                      rating < 8 ~ 3, 
                                      rating >= 8 ~ 4)) |> 
  test_icr(message_id, coder_id, levels = c(rating = "ordinal",
                                            rating_ordinal = "ordinal"))


ratings_only_df |>
  dplyr::filter(coder_id == "coder_3"|coder_id == "coder_4"|coder_id == "coder_5") |> 
  test_icr(message_id, coder_id, levels = c(rating = "ordinal"))


# readr::write_csv(x = ratings_only_df |>
#                    dplyr::filter(coder_id == "coder_1"|coder_id == "coder_2") |> 
#                    tidyr::pivot_wider(names_from = coder_id, values_from = rating) |> 
#                    dplyr::select(-1),
#                  file = "ratings_only.csv",
#                  col_names = FALSE)
