# pkgs ----
library(tidyverse)
library(scales)
library(knitr)
library(kableExtra)
library(colorblindr)
library(downlit)
library(xml2)
library(reactable)
library(palmerpenguins)
library(ggplot2movies)
library(ggalluvial)
library(fivethirtyeight)

# options ----
options(scipen = 9999999)


# ggplot2 theme ----
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 18))
# funs ----
ds538 <- readr::read_rds("data/ds538.rds")
# movies_data ----
movies_data <- readr::read_rds("data/movies_data.rds")
# penguins ----
penguins <- palmerpenguins::penguins
# movies ----
movies <- ggplot2movies::movies


# PIE-CHARTS --------------------------------------------------------------

movie_pie <- movies_data |> 
  dplyr::group_by(mpaa) |> 
  summarise(cnt = n()) |> 
  mutate(
    perc = round(cnt / sum(cnt), 3),
    mpaa = factor(mpaa, 
          levels = c("PG", "PG-13", "R")))

labs <- paste0(movie_pie$mpaa, " (", 
               (100*movie_pie$perc), "%)")
labs_pie <- labs(
  title = "Percent MPAA ratings for IMDB movies",
  x = "Percent MPAA rating")

ggp2_pie <- ggpubr::ggpie(movie_pie, 
          x = "perc", label = labs, 
          lab.pos = "in", lab.font = "white",
          fill = "mpaa", color = "white") + 
    theme(legend.position = "none")  
    
ggp2_pie + 
  labs_pie


# DIVERGING BAR GRAPHS ----------------------------------------------------

# If you have two proportions that contain positive and negative values, 
# consider using diverging bars with geom_bar()

## DATA --------------------------------------------------------------------
## Create trump_approval_diverg from the fivethirtyeight::trump_approval_trend
## dataset.

fivethirtyeight::trump_approval_trend |>
  filter(subgroup == "All polls") |>
  mutate(
    month = lubridate::month(modeldate,
      label = TRUE, abbr = TRUE),
    approve = approve_estimate * 0.01,
    disapprove = disapprove_estimate * 0.01,
    disapprove = disapprove * -1
  ) |>
  pivot_longer(
    cols = c(approve, disapprove),
    names_to = "poll",
    values_to = "values"
  ) |>
  group_by(month, poll) |>
  summarise(month_avg = mean(values, na.rm = TRUE)) |>
  ungroup() -> trump_approval_diverg

## CODE --------------------------------------------------------------------

labs_geom_bar_diverg <- labs(
  title = "Trump Approval Ratings",
  subtitle = "From 'How Popular is Donald Trump'",
  x = "Month", y = "Monthly average percent",
  fill = "Estimate")
ggp2_bars_diverg <- ggplot(trump_approval_diverg, 
    aes(x = month, y = month_avg)) +
  geom_bar(aes(fill = poll),
    stat = "identity", width = .5) + 
  scale_y_continuous(limits = c(-1, 1), 
    labels = scales::percent)

## GRAPH --------------------------------------------------------------------
ggp2_bars_diverg + 
  labs_geom_bar_diverg


# MOSAIC ------------------------------------------------------------------
library(ggmosaic)
#> Loading required package: ggplot2
# ggplot(data = fly) +
#   geom_mosaic(aes(x = product(rude_to_recline), 
#                   fill = do_you_recline)) +
#   theme_mosaic()
## DATA --------------------------------------------------------------------
flying <- fivethirtyeight::flying
fly_mosaic <- filter(flying, 
              !is.na(baby) & !is.na(unruly_child)) |> 
              select(baby, unruly_child)
glimpse(fly_mosaic)

## CODE --------------------------------------------------------------------
labs_mosaic <- labs(
      title = "In general...", 
      subtitle = "...is it rude to...",
      x = "... bring a baby on a plane?",
      y = "..,knowingly bring unruly children on a plane?",
      fill = "Responses") 
ggp2_mosaic <- ggplot(data = fly_mosaic) +
  geom_mosaic(aes(x = product(unruly_child, baby), 
      fill = baby)) +
  theme_mosaic()
## GRAPH --------------------------------------------------------------------
ggp2_mosaic + 
    labs_mosaic
