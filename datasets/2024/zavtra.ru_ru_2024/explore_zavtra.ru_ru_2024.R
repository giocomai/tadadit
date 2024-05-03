# remotes::install_github("giocomai/castarter")
library("castarter")

corpus_name <- "zavtra.ru_ru_2024"
corpus_file <- paste0(corpus_name, ".csv.gz", collapse = "")

## Step 1: Download the corpus ####

# download `zavtra.ru_ru_2024.csv.gz` from the main repository,
# or rely on the following to download the latest version from GitHub

library("piggyback")
pb_download(repo = "giocomai/tadadit",
            tag = corpus_name,
            file = corpus_file)

## Step 2: Explore the corpus in an interactive web interface ####

corpus_df <- readr::read_csv(file = corpus_file)

castarter::cas_explorer(corpus = corpus_df,
                        default_pattern = "коллективн[а-я]+ Запад",
                        title = corpus_name)

## Want a speed boost, and use less RAM on your local device?
## Convert corpus to the parquet format, and read via arrow:

cas_write_corpus(corpus = corpus_df |> 
                   dplyr::arrange(date) |> 
                   dplyr::collect(),
                 partition = "year",
                 path = corpus_name)

castarter::cas_explorer(corpus = arrow::open_dataset("zavtra.ru_ru_2024"),
                        default_pattern = "коллективн[а-я]+ Запад",
                        title = corpus_name)


## Count matches in R
cas_count(corpus = corpus_df,
          pattern = "коллективн[а-я]+ Запад")  

cas_count(corpus = corpus_df,
          pattern = "коллективн[а-я]+ Запад") |> 
  cas_summarise(period = "year") 

## Or process this corpus with your favourite tools!
