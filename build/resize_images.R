
convert_file <- function(path, input_size, output_size) {

  img <- magick::image_read(here::here("docs", input_size, path))
  img <- magick::image_resize(img, paste0(output_size, "x", output_size))
  magick::image_write(img,
    path = here::here("docs", output_size, path)
  )
  rm(img)
  gc()
  cat(path, "\n")
}


images <- list.files(here::here("docs", "3000"))
purrr::walk(images, ~convert_file(.x, 3000, 800))
