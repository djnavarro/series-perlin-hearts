
# load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(ambient)
library(here)
library(ggthemes)
library(animation)


# typical helper functions ------------------------------------------------

sample_shades <- function(n) {
  sample(colours(distinct = FALSE), n)
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

  bg <- "black"
  grain <- 8
  xlim <- c(0, grain+1)
  ylim <- c(0, grain+1)
  sys_id <- "perlin-heart"

  set.seed(seed)

  perlin_heart_l <- lift_dl(perlin_heart)

  make_dat <- function() {
    expand_grid(
      cx = 1:grain, cy = 1:grain,
      r_min = .35, r_max = .4,
      w_min = 0, w_max = 2
    ) %>%
      mutate(
        cx = cx + runif(n(), -.1, .1),
        cy = cy + runif(n(), -.1, .1),
        rot = runif(n(), -.1, .1)
      ) %>%
      transpose() %>%
      imap_dfr(~ perlin_heart_l(.x) %>% mutate(id = .y))
  }

  dat_1 <- make_dat()
  dat_2 <- make_dat()

  shades <- sample_shades(4)
  dat_1 <- dat_1 %>% mutate(shade = "white")
  dat_2 <- dat_2 %>% group_by(id) %>% mutate(shade = sample(shades, 1))

  ord <- sample(grain^2)

  generate_frames <- function(dat_1, dat_2, shades, ord, grain) {
    for(i in 1:30) {
      cat(".")

      ind <- sample(2, grain^2, replace = TRUE, prob = c(.8, .2))
      dat <- rbind(
        dat_1 %>% filter(id %in% which(ind == 1)),
        dat_2 %>% filter(id %in% which(ind == 2))
      )

      pic <- dat %>%
        ggplot(aes(x, y, group = id, size = width, colour = shade)) +
        geom_path(show.legend = FALSE) +
        theme_void() +
        theme(plot.background = element_rect(fill = bg)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_colour_identity() +
        scale_size_identity() +
        coord_fixed(xlim = xlim, ylim = ylim) +
        NULL

      print(pic)

    }
    cat("\n")
  }

  saveGIF(
    expr = generate_frames(dat_1, dat_2, shades, ord, grain),
    movie.name = save_path(sys_id, sys_version, seed, fmt = ".gif"),
    ani.height = 1000,
    ani.width = 1000,
    interval = .2
  )

}


# plot parameters ---------------------------------------------------------

seed <- 148
sys_version <- 7

for(s in seed) {
  cat(s, "\n")
  generate_image(s, sys_version)
}


