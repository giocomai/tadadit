library("ytdlpr")

yt_set_base_folder(path = fs::path(
  fs::path_home_r(),
  "R",
  "ytdlpr" # you'd probably set something meaningful here, relevant to what you're downloading
))


yt_set_base_folder(path = "/run/media/g/tadadit_2TB/ytdlpr")


yt_get_playlist_id("https://www.youtube.com/@PervyPridnestrovsky",
                   update = FALSE)


### All ru subs ####

if (fs::file_exists("pp_ru_subs.csv.gz")==FALSE) {
  
  pp_ru_subs_df  <- yt_get_local_subtitles(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
                                           sub_lang = "ru") |>
    yt_read_vtt()
  
  readr::write_csv(x = pp_ru_subs_df, file = "pp_ru_subs.csv.gz")
}


pp_ru_subs_df <- readr::read_csv(file = "pp_ru_subs.csv.gz")

### Blockade subs ####

if (fs::file_exists("blokad_ru_pp.rds")==FALSE){
  blokad_ru_df <- yt_get(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
                         auto_subs = TRUE,
                         sub_lang = "ru") |>
    yt_read_vtt() |>
    yt_filter(pattern = "блокад")
  
  saveRDS(blokad_ru_df, file = "blokad_ru_pp.rds")
}

blokad_ru_df <- readRDS(file = "blokad_ru_pp.rds")


### Getting blockade video ###

blokad_ru_df |>
  yt_get(video = TRUE)

blokad_ru_df |>
  yt_trim_with_text(only_local = TRUE, 
                    destination_folder = "transnistria_blokad",
                    duration = 4)


# yt_get(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
#        auto_subs = TRUE,
#        sub_lang = "en") 

blokad_ru_df |>
  yt_trim_with_text(destination_folder = "transnistria_blokad",
                    duration = 4)

# blokad_ru_df |>
#   yt_trim_with_text(destination_folder = "transnistria_blokad",
#                     duration = 5)



### concatenate ####
# https://trac.ffmpeg.org/wiki/Concatenate


blokad_ru_df |>
  yt_trim_with_text(destination_folder = "transnistria_blokad",
                    duration = 4) |>
  yt_concatenate(destination_folder = "transnistria_blokad_concatenated",
                 sort_by_date = TRUE)

info_blokad_df <- yt_read_info_json(yt_get(yt_id = blokad_ru_df[["yt_id"]], 
                                           info_json = TRUE))

#readr::write_csv(x = info_blokad_df, file = "blokad_ru_pp_info.csv")

#saveRDS(object = info_blokad_df,file = "blokad_ru_pp_info.rds" )
# get only files that have already been trimmed, and put them in dated order

info_blokad_df <- readRDS(file = "blokad_ru_pp_info.rds")

available_trimmed_df <- info_blokad_df |> 
  dplyr::arrange(upload_date) |> 
  dplyr::left_join(tibble::tibble(path = fs::dir_ls(fs::path(yt_get_base_folder(), "transnistria_blokad"))) |> 
                     dplyr::mutate(yt_id = path |> fs::path_file() |> stringr::str_extract(pattern = "[[:print:]]{11}"))) |>
  dplyr::filter(is.na(path)==FALSE) 


years <- 2014:2024

base_folder <- "/home/g/Videos/ytdplr_transnistria_blokade"


purrr::walk(.x = years, .f = function(current_year) {
  files_v <- available_trimmed_df |> 
    dplyr::filter(upload_date>=as.Date(paste0(current_year, "-01-01")), upload_date <= as.Date(paste0(current_year, "-12-31"))) |> 
    dplyr::pull(path)
  
  current_txt_file <- fs::path(base_folder, paste0("blokad_trimmed_", current_year, ".txt"))
  
  stringr::str_c("file ", shQuote(files_v)) |> 
    readr::write_lines(current_txt_file)
  
  system(
    paste0(
      'ffmpeg -f concat -safe 0 -c:a aac -i ',
      current_txt_file,
      ' ', 
      fs::path(base_folder, paste0("blokad_combo_", current_year, ".mp4"))
    )
    )
  
  # system(
  #   paste0(
  #     'ffmpeg -i ',
  #     current_txt_file,
  #     ' ', 
  #     '-filter_complex "[0:v]scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2[v0]; [v0][0:a][1:v][1:a]concat=n=2:v=1:a=1[v][a]" -map "[v]" -map "[a]" -c:v libx264 -c:a aac -movflags +faststart ',
  #     fs::path(base_folder, paste0("blokad_combo_v2_", current_year, ".mp4"))
  #   )
  # )  
  
})


# write list of files to be merged









########################################





### yt_rename ####

source_folder <- "transnistria_blokad"
destination_folder <- "transnistria_blokad_trimmed"

destination_path <- "/home/g/R/ytdlpr/transnistria_blokad_trimmed"

yt_rename <- function(source_folder = NULL,
                      source_path = NULL,
                      destination_folder = NULL,
                      destination_path = NULL,
                      yt_base_folder = NULL) {
  
  if (is.null(source_path)) {
    source_path <- fs::dir_create(
      yt_get_base_folder(path = yt_base_folder),
      source_folder
    )
  }
  
  if (is.null(destination_path)&is.null(destination_folder)) {
    destination_path <- source_path
  } else if (is.null(destination_path)) {
    destination_path <- fs::dir_create(
      yt_get_base_folder(path = yt_base_folder),
      destination_folder
    )
  }
  
  files_with_id <- tibble::tibble(source_file_path = fs::dir_ls(source_path, type = "file", glob = "*.mp4")) |>
    dplyr::mutate(yt_id = fs::path_file(.data[["source_file_path"]]) |>
                    stringr::str_extract(pattern = "[[:print:]]{11}_[[:digit:]]") |>
                    stringr::str_remove(pattern = "_[[:digit:]]$")) |>
    dplyr::filter(is.na(yt_id)==FALSE)
  
  info_json_df <- yt_get(yt_id = files_with_id[["yt_id"]],
                         info_json = TRUE) |>
    yt_read_info_json() |>
    dplyr::select("yt_id", "upload_date")
  
  files_combo_df <- files_with_id |>
    dplyr::left_join(y = info_json_df,
                     by = "yt_id") |>
    dplyr::mutate(source_file_name = fs::path_file(.data[["source_file_path"]])) |>
    dplyr::rowwise() |>
    dplyr::mutate(destination_file_name = stringr::str_c(.data[["upload_date"]],
                                                         "_",
                                                         .data[["source_file_name"]])) |>
    dplyr::mutate(destination_file_path = fs::path(destination_path,
                                                   .data[["destination_file_name"]]))
  
  files_combo_df
  
  fs::file_copy(path = files_combo_df[["source_file_path"]],
                new_path = files_combo_df[["destination_file_path"]])
}


yt_get_playlist_id("https://www.youtube.com/@PervyPridnestrovsky",
                   update = FALSE)

yt_get_local_subtitles(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
                       sub_lang = "ru")
# 23,766

yt_get(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
       sub_lang = "ru",
       auto_subs = TRUE)

yt_get(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
       info_json = TRUE)

### All ru subs ####

# pp_ru_subs_df  <- yt_get_local_subtitles(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
#                                          sub_lang = "ru") |>
#   yt_read_vtt()

# readr::write_csv(x = pp_ru_subs_df, file = "pp_ru_subs.csv.gz")

pp_ru_subs_df <- readr::read_csv(file = "pp_ru_subs.csv.gz")

blokad_ru_subs_df <- pp_ru_subs_df |>
  yt_filter(pattern = "блокад")


###########

# https://www.youtube.com/@protvchisinau5284
# https://www.youtube.com/@TRMMDChannel

source_path <- fs::dir_ls(path = fs::path(yt_get_base_folder(), "@PervyPridnestrovsky"),
                          glob = "*.ru.vtt")
destination_folder <- fs::dir_create(fs::path(yt_get_base_folder(), "@PervyPridnestrovsky", "ru_vtt"))
file_names <- source_path |> fs::path_file()

fs::file_move(source_path,
              new_path = fs::path(destination_folder, file_names))

# info_json_df <- yt_get_local_id(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
#                 info_json = TRUE)

info_json_path_df <- yt_get_local(playlist = "@PervyPridnestrovsky",
                                  file_extension = ".info.json")

info_json_df <- yt_read_info_json(path = info_json_path_df)

# saveRDS(object = info_json_df,
#         file = fs::path(yt_get_base_folder(), "PervyPridnestrovsky_info_json.rds"))


info_json_df <- readr::read_rds(file = fs::path(yt_get_base_folder(), "PervyPridnestrovsky_info_json.rds"))

info_json_df |>
  dplyr::group_by(language) |>
  dplyr::count()

## get missing json ####

yt_get_playlist_id(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
                   update = TRUE)


## check missing subtitles ####
playlist <- "https://www.youtube.com/@PervyPridnestrovsky"
update <- FALSE
sub_lang <- "en"
# playlist_id_df <- yt_get_playlist_id(playlist = playlist,
#                                      update = update)

yt_get_available_subtitles <- function(info_json_df = NULL,
                                       sub_lang = "en",
                                       automatic_captions = TRUE,
                                       subtitles = TRUE,
                                       yt_id = NULL,
                                       playlist = NULL) {
  
  
  if (is.null(info_json_df)) {
    info_json_df <- yt_get(yt_id = yt_id,
                           playlist = playlist,
                           info_json = TRUE
    ) |>
      yt_read_info_json()
  }
  
  if (subtitles) {
    subs_df <- info_json_df |>
      dplyr::select("yt_id", "subtitles") |>
      dplyr::mutate(subtitles = purrr::map(subtitles, \(x) x[x==sub_lang])) |>
      tidyr::unnest(subtitles) |>
      dplyr::filter(subtitles %in% sub_lang,
                    is.na(subtitles)==FALSE) |>
      dplyr::mutate(sub_type = "subtitles") |>
      dplyr::rename(sub_lang = subtitles)
  } else {
    subs_df <- NULL
  }
  
  
  if (automatic_captions) {
    ac_df <- info_json_df |>
      dplyr::select("yt_id", "automatic_captions") |>
      dplyr::mutate(automatic_captions=purrr::map(automatic_captions, \(x) x[x==sub_lang])) |>
      tidyr::unnest(automatic_captions) |>
      dplyr::filter(automatic_captions %in% sub_lang,
                    is.na(automatic_captions)==FALSE) |>
      dplyr::mutate(sub_type = "automatic_captions") |>
      dplyr::rename(sub_lang = automatic_captions)
  } else {
    ac_df <- NULL
  }
  
  dplyr::bind_rows(subs_df,
                   ac_df)
}



ru_subs_local_df <- yt_get_local_subtitles(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
                                           sub_lang = "ru")

info_json_df <- readRDS(fs::path(yt_get_base_folder(), "PervyPridnestrovsky_info_json.rds"))

ru_subs_available_online_df <- info_json_df |>
  dplyr::select(yt_id, title, upload_date, language, automatic_captions) |>
  #dplyr::mutate(automatic_captions=purrr::map(automatic_captions, \(x) x[x=="ru"])) |>
  tidyr::unnest(automatic_captions) |>
  dplyr::filter(automatic_captions == "ru",
                is.na(automatic_captions)==FALSE)

ru_subs_to_download_df <- ru_subs_available_online_df |>
  dplyr::anti_join(y = ru_subs_local_df, by = "yt_id")

ru_subs_to_download_df |>
  yt_get(auto_subs = TRUE,
         sub_lang = "ru",
         playlist = "https://www.youtube.com/@PervyPridnestrovsky")

### filter blockade subtitles ####

blockade_subs_df <- ru_subs_local_df |>
  head(500) |>
  yt_read_vtt() |>
  yt_filter(pattern = "блокад")


blockade_info_json_path_df <- blockade_subs_df |>
  dplyr::distinct(yt_id) |>
  dplyr::left_join(y = yt_get_local_id(file_extension = ".info.json"),
                   by = "yt_id")

blockade_info_json_df <- blockade_info_json_path_df |>
  dplyr::filter(is.na(path)==FALSE) |>
  yt_read_info_json()


### get blockade video and export #####

blokad_ru_df <- yt_get_local(playlist = "PLeKla9V-UdF_l6xCnsc4TlYHi-1JoPBmL",
                             file_extension = ".ru.vtt") |>
  #dplyr::slice_sample(n = 200) |>
  yt_read_vtt() |>
  yt_filter(pattern = "блокад")

blokad_ru_df |>
  yt_get(video = TRUE)

trim_df <- blokad_ru_df |>
  yt_trim(simulate = TRUE)

trim_df[["ffmpeg_command"]]

current_row_df <- trim_df |> dplyr::slice(1)

current_json_df <- yt_get_local(yt_id = current_row_df[["yt_id"]],
                                file_extension = ".info.json") |>
  yt_read_info_json()

text_v <- c(stringr::str_c("Date:  ", current_json_df[["upload_date"]]),
            stringr::str_c("Title: ", current_json_df[["title"]]),
            stringr::str_c("ID:    ", current_row_df[["yt_id"]]),
            stringr::str_c("Start: ", current_row_df[["start_time"]] |>
                             stringr::str_remove(pattern = ".[[:digit:]]{3}$")))
text_v
text_temp_path <- fs::file_temp(ext = "txt")
readr::write_lines(x = text_v,
                   text_temp_path)


# ffmpeg -y -i '/home/g/R/ytdlpr/«Молдова нарушает мирные соглашения 1992 г.»： о пошлинах на ОКК [JHVLYjP6wvs].webm' -vf "drawtext=font='Mono':textfile='/tmp/Rtmp0c7dfm/file118ff6fa8ebad.txt':fontcolor=white:fontsize=32:box=1:boxcolor=black@0.5:boxborderw=5:x=10:y=10" -ss 00:01:12.109 -to 00:01:17.109 '/home/g/R/ytdlpr/0_trimmed_video/JHVLYjP6wvs_72_5.mp4'


### actual trimmed video with text ####


library("ytdlpr")

yt_set_base_folder(path = fs::path(
  fs::path_home_r(),
  "R",
  "ytdlpr" # you'd probably set something meaningful here, relevant to what you're downloading
))

ru_subs_local_df <- yt_get_local_subtitles(playlist = "https://www.youtube.com/@PervyPridnestrovsky",
                                           sub_lang = "ru") |>
  #dplyr::slice_sample(n = 100) |>
  yt_read_vtt()

blokad_subs_df <- ru_subs_local_df |>
  yt_filter(pattern = "блокад")

# yt_get(yt_id = blokad_subs_df[["yt_id"]],
#        video = TRUE)

yt_trim_with_text(subtitles_df = blokad_ru_subs_df, only_local = TRUE)
