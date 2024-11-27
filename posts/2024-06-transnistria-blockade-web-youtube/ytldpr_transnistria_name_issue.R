library("ytdlpr")

yt_set_base_folder(path = "/run/media/g/tadadit_2TB/ytdlpr")

yt_get_playlist_id("https://www.youtube.com/@PervyPridnestrovsky",
                   update = FALSE)

yt_get(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
       info_json = TRUE,
       min_sleep_interval = 8)

yt_get(yt_id = "hVGNsyGB4mA", info_json = TRUE)

# yt_get(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
#        auto_subs = TRUE,
#        sub_lang = "ru",
#        info_json = TRUE) 

if (fs::file_exists("PervyPridnestrovsky_subs_ru.rds")==FALSE){
  pp_ru_df <- yt_get(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
                               auto_subs = TRUE,
                               sub_lang = "ru") |>
    yt_read_vtt() 
  
  saveRDS(pp_ru_df, file = "PervyPridnestrovsky_subs_ru.rds")
} else {
  pp_ru_df <- readr::read_rds("PervyPridnestrovsky_subs_ru.rds")
}


if (fs::file_exists("transnistria_ru_pp.rds")==FALSE){
  transnistria_ru_df <- pp_ru_df |>
    yt_filter(pattern = "транснистр")
  
  saveRDS(transnistria_ru_df, file = "transnistria_ru_pp.rds")
}

transnistria_ru_df <- readRDS(file = "transnistria_ru_pp.rds")
