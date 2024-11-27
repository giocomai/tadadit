osset_subs_df <- pp_ru_subs_df |> 
  tidyr::drop_na(text) |> 
  dplyr::filter(stringr::str_detect(string = stringr::str_to_lower(text),
                                    pattern = "\\bосети")) 

osset_id_v <- osset_subs_df |> 
    dplyr::pull(yt_id)


osset_info_path_df <- yt_get(yt_id = osset_id_v, 
                        info_json = TRUE,
                        min_sleep_interval = 4)

osset_info_df <- yt_read_info_json(path = osset_info_path_df)


osset_subs_info_df <- osset_info_df |> 
  dplyr::select(yt_id, upload_date, title)  |> 
  dplyr::left_join(y = osset_subs_df,
                   by = "yt_id")

osset_subs_info_df |> 
  dplyr::filter(lubridate::year(upload_date)==2023) |>
  dplyr::arrange(upload_date) |> 
  View()
