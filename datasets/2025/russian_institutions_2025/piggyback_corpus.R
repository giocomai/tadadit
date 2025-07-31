cas_read

corpus_path <- fs::path(fs::path_home_r(), "R", "castarter_2025", "corpora")

fs::dir_create(corpus_path)

release_file <- fs::path(corpus_path, stringr::str_c(corpus_name, ".csv.gz"))

corpus_df |>
  readr::write_csv(file = release_file)


piggyback::pb_new_release(
  repo = "giocomai/tadadit",
  tag = corpus_name,
  body = summary_stats_text()
)

piggyback::pb_upload(
  file = release_file,
  repo = "giocomai/tadadit",
  tag = corpus_name
)

ods_file <- fs::path(corpus_path, stringr::str_c(path = corpus_name, ".ods"))

readODS::write_ods(x = corpus_df, path = ods_file)

piggyback::pb_upload(
  file = ods_file,
  repo = "giocomai/tadadit",
  tag = corpus_name
)

cas_write_corpus(
  corpus = corpus_df,
  partition = "year",
  tif_compliant = FALSE,
  path = fs::path(corpus_path, corpus_name)
)
