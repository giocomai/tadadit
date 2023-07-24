library("castarter")
cas_set_options(
  base_folder = fs::path(fs::path_home_r(), 
                         "R",
                         "castarter_tadadit")
)

## Russian institutions

kremlin.ru_ru_a <- cas_read_corpus(
  project = "Russian institutions",
  website = "kremlin.ru_ru",
  partition = "year",
  update = TRUE
)


kremlin.ru_ru_metadata <- cas_generate_metadata(corpus = kremlin.ru_ru_a)


`1tv.ru_ru_a` <- cas_read_corpus(
  project = "Russian media",
  website = "1tv.ru_ru",
  partition = "year",
  update = TRUE
)

`1tv.ru_ru_metadata` <- cas_generate_metadata(corpus = `1tv.ru_ru_a`)

`rg.ru_ru_a` <- cas_read_corpus(
  project = "Russian media",
  website = "rg.ru_ru",
  partition = "year",
  update = TRUE
)

`rg.ru_ru_metadata` <- cas_generate_metadata(corpus = `rg.ru_ru_a`)

`rg.ru_ru_metadata`

kwic_table(corpus = `rg.ru_ru_a` |> dplyr::collect(),
           pattern = "Стрелков")
