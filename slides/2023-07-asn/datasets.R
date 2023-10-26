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


mid.ru_ru_a <- cas_read_corpus(
  project = "Russian institutions",
  website = "mid.ru_ru",
  partition = "year",
  update = TRUE
)

mid.ru_ru_metadata <- cas_generate_metadata(corpus = mid.ru_ru_a)



duma.gov.ru_ru_a <- cas_read_corpus(
  project = "Russian institutions",
  website = "duma.gov.ru_ru",
  partition = "year",
  update = TRUE
)

duma.gov.ru_ru_metadata <- cas_generate_metadata(corpus = duma.gov.ru_ru_a)


transcript.duma.gov.ru_ru_a <- cas_read_corpus(
  project = "Russian institutions",
  website = "transcript.duma.gov.ru_ru",
  partition = "year",
  update = TRUE
)

transcript.duma.gov.ru_ru_metadata <- cas_generate_metadata(corpus = transcript.duma.gov.ru_ru_a)

## Russian media

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

`ng.ru_ru_a` <- cas_read_corpus(
  project = "Russian media",
  website = "ng.ru_ru",
  partition = "year",
  update = TRUE
)

`ng.ru_ru_metadata` <- cas_generate_metadata(corpus = `ng.ru_ru_a`)


`kp.ru_ru_a` <- cas_read_corpus(
  project = "Russian media",
  website = "kp.ru_ru",
  partition = "year",
  update = TRUE
)

`kp.ru_ru_metadata` <- cas_generate_metadata(corpus = `kp.ru_ru_a`)


zavtra.ru_ru_a <- cas_read_corpus(
  project = "Russian media",
  website = "zavtra.ru_ru",
  partition = "year",
  update = TRUE
)



zavtra.ru_ru_metadata <- cas_generate_metadata(corpus = zavtra.ru_ru_a)


tsargrad.tv_ru_a <- cas_read_corpus(
  project = "Russian media",
  website = "tsargrad.tv_ru",
  partition = "year",
  update = TRUE
)

tsargrad.tv_ru_metadata <- cas_generate_metadata(corpus = tsargrad.tv_ru_a)