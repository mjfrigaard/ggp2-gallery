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
ds538 <- readr::read_rds("slides/data/ds538.rds")
# movies_data ----
movies_data <- readr::read_rds("slides/data/movies_data.rds")
# penguins ----
penguins <- palmerpenguins::penguins
# movies ----
movies <- ggplot2movies::movies



# BASIC BAR-GRAPHS -----
labs_bar <- labs(title = "IMDB movie information/user ratings",
  x = "MPAA rating",
  y = "Count",
  fill = "MPAA")
ggp2_bar <- ggplot(data = movies_data,
  aes(x = mpaa)) +
  geom_bar(aes(fill = mpaa),
    show.legend = FALSE)
ggp2_bar +
  labs_bar

# GROUPED COLUMN-GRAPHS -----
# Total graph
# Create labels
labs_col <- labs(title = "IMDB movie information/user ratings",
  x = "MPAA rating",
  y = "Total budget in US dollars")
# create plot
ggp2_col <- ggplot(data = movies_data,
  aes(x = mpaa,
    y = budget)) +
  geom_col(aes(fill = mpaa),
    show.legend = FALSE) +
  scale_y_continuous(labels = scales::dollar)
ggp2_col +
  labs_col

# total (sum) table
movies_sum_budget <- movies_data |>
  group_by(mpaa) |>
  summarise(tot_bud = sum(budget, na.rm = TRUE),
    tot_bud = scales::dollar(tot_bud)) |>
  ungroup() |>
  select(MPAA = mpaa,
    `Total budget` = tot_bud)
movies_sum_budget

# GROUPED COLUMN-GRAPHS 2 --------------------
# average bar-graph data
movies_col <- movies_data |>
  group_by(mpaa) |>
  summarise(avg_budget = mean(budget, na.rm = TRUE)) |>
  ungroup()
# Create labels
labs_col <- labs(title = "IMDB movie information/user ratings",
  x = "MPAA rating",
  y = "Average budget in US dollars")
# create plot
ggp2_col <- ggplot(data = movies_col,
  aes(x = mpaa,
    y = avg_budget)) +
  geom_col(aes(fill = mpaa),
    show.legend = FALSE) +
  scale_y_continuous(labels = scales::dollar)
ggp2_col +
  labs_col

# OVERLAPPING (STACKED) BAR GRAPHS --------------------
penguins_ovrlp <- filter(penguins,!is.na(sex) & species == "Adelie")
glimpse(penguins_ovrlp)

labs_bar_ovrlp <- labs(title = "Adelie adult foraging penguins",
  x = "Flipper length (millimeters)",
  fill = "Sex")
ggp2_bar_ovrlp <- ggplot(data = penguins_ovrlp,
  aes(x = flipper_length_mm,
    fill = sex)) +
  geom_bar()
ggp2_bar_ovrlp +
  labs_bar_ovrlp

# DODGE BAR GRAPHS --------------------
penguins_dodge <- filter(penguins, !is.na(sex))
glimpse(penguins_dodge)

labs_bar_dodge <- labs(
  title = "Adult foraging penguins",
  subtitle = "position = 'dodge'",
  x = "Penguin Species",
  fill = "Penguin Sex")

ggp2_bar_dodge <- ggplot(data = penguins_dodge,
                    aes(x = species,
                      fill = sex)) +
                    geom_bar(position = "dodge")
ggp2_bar_dodge +
  labs_bar_dodge

# DODGE2 BAR GRAPHS --------------------
# dodge2 preserves the total width of the elements.
penguins_dodge2 <- filter(penguins,
                    !is.na(sex) & !is.na(species))

labs_bar_dodge2 <- labs(
  title = "Adelie adult foraging penguins",
  subtitle = "position = 'dodge2'",
  x = "Species",
  fill = "Penguin Sex")
ggp2_bar_dodge2 <- ggplot(data = penguins_dodge2,
                    aes(x = species,
                      fill = sex)) +
                    geom_bar(position = "dodge2")
ggp2_bar_dodge2 +
  labs_bar_dodge2


# WAFFLE CHART ------------------------
penguins <- palmerpenguins::penguins
penguins <- dplyr::mutate(penguins, 
              species = as.character(species))
waffle_peng <- ggwaffle::waffle_iron(penguins,
                ggwaffle::aes_d(group = species))

glimpse(waffle_peng)
labs_waffle <- labs(
  title = "Waffle chart of palmer penguin species",
  x = "", y = "", fill = "Species")
ggp2_waffle <- ggplot(data = waffle_peng, 
       aes(x = x, 
           y = y, 
           fill = group)) + 
  ggwaffle::geom_waffle() 
ggp2_waffle + 
  labs_waffle +
  ggwaffle::theme_waffle()

# DIVERGING BAR GRAPHS ------------------
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

labs_geom_bar_diverg <- labs(
  title = "Trump Approval Ratings",
  subtitle = "From 'How Popular is Donald Trump'",
  x = "Month",
  y = "Monthly average percent",
  fill = "Estimate"
)


ggp2_bars_diverg <- ggplot(data = trump_approval_diverg,
  aes(x = month, y = month_avg)) +
  geom_bar(aes(fill = poll),
    stat = "identity", width = .5) +
  scale_y_continuous(limits = c(-1, 1),
    labels = scales::percent)

ggp2_bars_diverg +
  labs_geom_bar_diverg


# DIVERGING BAR GRAPHS (VERTICAL) -------

ggp2_bar_diverg_vert <- ggplot(data = trump_approval_diverg,
  aes(x = month_avg,
    y =  month)) +
  geom_bar(aes(fill = poll),
    stat = "identity",
    width = .5) +
  scale_x_continuous(limits = c(-1, 1),
    labels = scales::percent)

ggp2_bar_diverg_vert +
  labs_geom_bar_diverg_vert

# HEATMAPS ------------------------------
glimpse(movies_data)

heatmap_ross <- fivethirtyeight::bob_ross |>
  pivot_longer(-c(episode, season,
    episode_num, title),
    names_to = "object",
    values_to = "present") |>
  mutate(present = as.logical(present),
    object = str_replace_all(object, "_", " ")) |>
  arrange(episode, object) |>
  filter(
    object %in% c(
      'conifer',
      'trees',
      'tree',
      'snow',
      'palm trees',
      'grass',
      'flowers',
      'cactus',
      'bushes',
      'cirrus',
      'cumulus',
      'deciduous',
      'clouds',
      'fog'
    )
  ) |>
  group_by(season, object) |>
  summarise(count = sum(present)) |>
  ungroup()

# create labels
labs_heatmap_raster <- labs(title = "Bob Ross' plants & clouds",
  x = "Season",
  y = "Plant/Cloud Object",
  fill = "Occurances")
# create plot
ggp2_heatmap_raster <- ggplot(data = heatmap_ross,
  aes(x = season,
    y = object,
    fill = count)) +
  geom_raster()

ggp2_heatmap_raster +
  ggplot2::theme_minimal(base_size = 12) +
  labs_heatmap_raster

# create labels
labs_heatmap_tile <- labs(title = "Bob Ross' plants & clouds",
  x = "Season",
  y = "Plant/Cloud Object",
  fill = "Occurances")
# create plot
ggp2_heatmap_tile <- ggplot(data = heatmap_ross,
  aes(x = season,
    y = object,
    fill = count)) +
  geom_tile()

ggp2_heatmap_tile +
  ggplot2::theme_minimal(base_size = 12) +
  labs_heatmap_tile
