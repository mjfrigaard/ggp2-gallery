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

options(scipen = 9999999)


# ggplot2 theme ----
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 18))
# funs ----
ds538 <- readr::read_rds("slides/data/ds538.rds")
# movies_data ----
movies_data <- readr::read_rds("slides/data/movies_data.rds")
# penguins ----
penguins <- palmerpenguins::penguins
# movies ----
movies <- ggplot2movies::movies


# SCATTER -----------------------------------------------------------------

labs_scatter <- labs(title = "Bill Length vs. Flipper Length",
                      x = "Bill Length (mm)", y = "Flipper length (mm)")

ggp2_scatter <- penguins |> 
  ggplot(aes(x = bill_length_mm, y = flipper_length_mm)) + 
  geom_point() 

ggp2_scatter + 
  labs_scatter


# GROUPED SCATTER ---------------------------------------------------------

labs_grp_scatter <- labs(title = "Bill Length vs. Flipper Length",
                      x = "Bill Length (mm)", y = "Flipper length (mm)", 
                      color = "Penguin species")

ggp2_grp_scatter <- penguins |> 
  ggplot(aes(x = bill_length_mm, y = flipper_length_mm)) + 
  geom_point(aes(color = species)) 

ggp2_grp_scatter + 
  labs_grp_scatter



# BUBBLE GRAPH ------------------------------------------------------------

labs_bubble <- labs(title = "Bill Length vs. Flipper Length",
                      x = "Bill Length (mm)", y = "Flipper length (mm)", 
                      size = "Body Mass (g)")

ggp2_bubble <- penguins |> 
  ggplot(
    aes(x = bill_length_mm, 
        y = flipper_length_mm)) + 
  geom_point(
    aes(size = body_mass_g), 
      alpha = 1/3) + 
  scale_size(
    range = c(.1, 10), 
    name = "Body Mass (g)")

ggp2_bubble + 
  labs_bubble

# GROUPED BUBBLE GRAPH ------------------------------------------------------------

labs_grp_bubble <- labs(
  title = "Bill Length vs. Flipper Length",
  x = "Bill Length (mm)", y = "Flipper length (mm)", 
  size = "Body Mass (g)")


ggp2_grp_bubble <- penguins |> 
  ggplot(aes(
    x = bill_length_mm, 
    y = flipper_length_mm)) + 
  geom_point(
    aes(size = body_mass_g, 
        fill = species), 
          alpha = 2/3,
          shape = 21, 
          color = "black") + 
  scale_size(range = c(.04, 8), 
             name = "Body Mass (g)") + 
  labs_grp_bubble

ggp2_grp_bubble + 
  labs_grp_bubble


# ALLUVIAL WIDE --------------------------------------------------

# convert to wide
peng_wide <- penguins |> 
  tidyr::drop_na() |> 
  count(year, island, sex, species) |> 
  dplyr::mutate(year = factor(year)) |> 
  rename(freq = n)

# labels
labs_alluvial <- ggtitle(label = "Palmer Penguins", 
    subtitle = "Stratified by year, island and species")
labs_alluvial_y <- ylab("Frequency") 
labs_alluvial_fill <- labs(fill = "Sex")

# plot with wide data
ggp2_alluvial_w <- ggplot(data = peng_wide,
  aes(axis1 = year,
      axis2 = island,
      axis3 = species,
      y = freq)) +
  scale_x_discrete(
    limits = c("Year", "Island", "Species"),
    expand = c(0.1, .07)) +
  geom_alluvium(aes(fill = sex)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
    aes(label = after_stat(stratum)))

ggp2_alluvial_w + 
  labs_alluvial + 
  labs_alluvial_y + 
  labs_alluvial_fill

# ALLUVIAL LODES --------------------------------------------------
peng_lodes <- penguins |> 
  select(Year = year, Island = island, 
         Species = species, Sex = sex) |> 
  tidyr::drop_na() |> 
  dplyr::count(Year, Island, Species, Sex) |> 
  dplyr::mutate(Year = factor(Year)) |> 
  dplyr::rename(Freqency = n) |> 
  ggalluvial::to_lodes_form(key = "Measure", axes = 1:3)  
  
ggp2_alluvial_lf <- ggplot(data = peng_lodes,
  aes(x = Measure,
      y = Freqency,
      stratum = stratum,
      alluvium = alluvium,
      label = stratum)) +
  ggalluvial::geom_alluvium(aes(fill = Sex)) +
  ggalluvial::geom_stratum() +
  geom_stratum(width = 0.45) +
  geom_text(stat = "stratum")

ggp2_alluvial_lf + 
  labs_alluvial


# BUMP CHART --------------------------------------------------

# devtools::install_github("davidsjoberg/ggbump")
# install.packages("pacman")
pacman::p_load(padr, hablar, jsonlite, ggbump, 
               httr, xml2, lubridate, tidyverse)
library(ggbump)
pacman::p_load(tidyverse, cowplot, wesanderson)

df <- tibble(
    country = c("India", "India", "India",
      "Sweden", "Sweden", "Sweden",
      "Germany", "Germany", "Germany",
      "Finland", "Finland", "Finland"),
    year = c(2011, 2012, 2013, 2011, 2012,
      2013, 2011, 2012, 2013, 2011, 2012, 2013),
    value = c(492, 246, 246, 369, 123, 
      492, 246, 369, 123, 123, 492, 369))

fivethirtyeight::tv_hurricanes |> 
  filter(date > as_date("2017-09-15")) |> 
  pivot_longer(cols = -date, names_to = 'hurricane', 
    values_to = 'value') |> 
  group_by(date) |>
  mutate(rank = rank(value, ties.method = "random")) |> 
  ungroup() -> tidy_hurricanes

labs_bump <- labs(title = "TV News Hurricane Mentions",   
  subtitle = "Between Sep 15-25th, 2017", 
  x = "Date", y = "Rank", 
  color = "Hurricanes")

ggp2_bump <- ggplot(tidy_hurricanes, 
    aes(date, rank, color = hurricane)) +
    ggbump::geom_bump()

ggp2_bump + 
  labs_bump



