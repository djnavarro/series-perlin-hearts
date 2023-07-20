
# load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(ambient)
library(here)
library(ggthemes)


# typical helper functions ------------------------------------------------

sample_shades <- function(n) {
  sample(colours(distinct = FALSE), n)
  #shades <- sample(canva_palettes,1)[[1]]
  #shades <- sample(shades)
  #return(shades)
}

blend_shades <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255, green = z[2, ]/255, blue = z[3, ]/255)
  return(z)
}

save_path <- function(sys_id, sys_version, seed, fmt = ".png") {
  sys_version <- sys_version %>% str_pad(width = 2, pad = "0")
  seed <- seed %>% str_pad(width = 3, pad = "0")
  base <- paste(sys_id, sys_version, seed, sep = "_")
  file <- paste0(base, fmt)
  path <- here("image", file)
  return(path)
}


# the thing I want to play with -------------------------------------------

heart_x <- function(angle) {
  x <- (16 * sin(angle)^3)/17
  return(x - mean(x))
}

heart_y <- function(angle) {
  y <- (13 * cos(angle) - 5 * cos(2 * angle) - 2 * cos(3 * angle) -
          cos(4 * angle))/17
  return(y - mean(y))
}

perlin_heart <- function(cx = 0, cy = 0, n = 1000, noise_max = 0.5,
                         octaves = 2, r_min = 0.5, r_max = 1, rot = 0,
                         w_min = 0, w_max = 1) {
  tibble(
    angle = seq(0, 2*pi, length.out = n),
    xoff = cos(angle) %>% rescale(from = c(-1, 1), to = c(0, noise_max)),
    yoff = sin(angle) %>% rescale(from = c(-1, 1), to = c(0, noise_max)),
    r = gen_simplex %>%
      fracture(fractal = fbm, x = xoff, y = yoff, octaves = octaves) %>%
      rescale(from = c(-0.5, 0.5), to = c(r_min, r_max)),
    x0 = r * heart_x(angle),
    y0 = r * heart_y(angle),
    x = x0 * cos(rot) - y0 * sin(rot) + cx,
    y = x0 * sin(rot) + y0 * cos(rot) + cy,
    width = gen_simplex %>%
      fracture(fractal = fbm, x = xoff, y = yoff, octaves = octaves) %>%
      rescale(to = c(w_min, w_max))
  )
}


# generate image ----------------------------------------------------------

generate_image <- function(seed, sys_version) {

  grain <- 6
  xlim <- c(.25, grain + .75)
  ylim <- c(.25, grain + .75)
  sys_id <- "perlin-heart"

  set.seed(seed)

  shades <- sample_shades(5)
  bg <- "black" #blend_shades(sample(shades, 1), "black", .2)
  #shades <- map_chr(shades, ~blend_shades(.x, "white"))

  perlin_heart_l <- lift_dl(perlin_heart)

  dat <- expand_grid(
    cx = 1:grain, cy = 1:grain,
    r_min = .35, r_max = .4,
    w_min = 0, w_max = 2) %>%
    mutate(
      cx = cx + runif(n(), -.1, .1),
      cy = cy + runif(n(), -.1, .1),
      rot = runif(n(), -.1, .1)
    ) %>%
    #sample_frac(.7) %>%
    transpose() %>%
    imap_dfr(~ perlin_heart_l(.x) %>% mutate(id = .y))

  pic <- dat %>%
    ggplot(aes(x, y, group = id, size = width)) +
    geom_path(
      colour = "white",
      show.legend = FALSE
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = bg)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = shades) +
    scale_size_identity() +
    coord_fixed(xlim = xlim, ylim = ylim) +
    NULL

  ggsave(
    filename = save_path(sys_id, sys_version, seed),
    plot = pic,
    width = 10,
    height = 10,
    dpi = 300
  )
}


# plot parameters ---------------------------------------------------------

seed <- 136
sys_version <- 5

for(s in seed) {
  cat(s, "\n")
  generate_image(s, sys_version)
}


