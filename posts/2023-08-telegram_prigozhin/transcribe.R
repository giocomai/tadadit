#Sys.sleep(25*60)
setwd(fs::path(here::here(), "posts", "2023-08-telegram_prigozhin"))

base_folder <- readLines("base_folder.txt")
source("functions.R")

messages_df <- process_telegram_json(fs::path(base_folder, "result.json"))

transcribe_audio(messages_df = messages_df,
                 base_folder = base_folder,
                 destination_folder = "telegram_concordgroup_official_audio_transcribed",
                 model = "large")


transcribe_audio(messages_df = messages_df,
                 base_folder = base_folder,
                 destination_folder = "telegram_concordgroup_official_audio_transcribed_translated",
                 model = "large",
                 translate = TRUE)
