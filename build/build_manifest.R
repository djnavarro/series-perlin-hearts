library(magrittr)

post_date <- "2021-07-16"

paths <- here::here("docs") %>%
  list.files(recursive = TRUE) %>%
  stringr::str_subset("jpg$|png$|gif$")

manifest <- tibble::tibble(path = paths) %>%
  tidyr::separate(
    col = path,
    into = c("resolution", "filename"),
    sep = "/",
    remove = FALSE
  ) %>%
  tidyr::separate(
    col = filename,
    into = c("filespec", "format"),
    sep = "\\."
  ) %>%
  tidyr::separate(
    col = filespec,
    into = c("series", "sys_id", "img_id"),
    sep = "_"
  ) %>%
  dplyr::mutate(
    #title = title %>%
    #  stringr::str_replace_all("-", " ") %>%
    #  stringr::str_to_title(),
    resolution = as.integer(resolution),
    date = post_date
  ) %>%
  dplyr::arrange(date, img_id)

readr::write_csv(manifest, here::here("docs", "manifest.csv"))
