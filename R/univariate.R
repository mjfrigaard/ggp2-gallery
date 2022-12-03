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


# BAR-GRAPHS --------------------------------------------------------------

## DATA ----
penguins |> glimpse()
## CODE ----
labs_bar <- labs(
  title = "IMDB movie information/user ratings",
  x = "MPAA rating", y = "Count", 
  fill = "MPAA")
ggp2_bar <- ggplot(data = movies_data, 
       aes(x = mpaa)) + 
    geom_bar(aes(fill = mpaa)) 
## GRAPH ----
ggp2_bar + 
  labs_bar


# HISTOGRAMS --------------------------------------------------------------

## DATA ----
penguins |> glimpse()
## CODE ----
# Create labels 
labs_histogram <- labs(
  title = "Adult foraging penguins",
  x = "Flipper length (millimeters)")
# create plot 
ggp2_hist <- ggplot(data = penguins,
     aes(x = flipper_length_mm)) + 
     geom_histogram()

## GRAPH ----
ggp2_hist + 
  labs_histogram


# FREQUENCY POLYGONS ------------------------------------------------------

## DATA ----
penguins |> glimpse()
## CODE ----
labs_freqpoly <- labs(
  title = "Adult foraging penguins",
  x = "Flipper length (millimeters)")
ggp2_freqpoly <- ggplot(data = penguins,
      aes(x = flipper_length_mm)) + 
    geom_freqpoly()
## GRAPH ----
ggp2_freqpoly + 
  labs_freqpoly



# DOT-PLOTS ---------------------------------------------------------------

## DATA ----
penguins |> glimpse()

## CODE ----
labs_dotplot <- labs(
  title = "Adult foraging penguins",
  x = "Flipper length (millimeters)")
ggp2_dotplot <- ggplot(data = penguins,
      aes(x = flipper_length_mm)) + 
    geom_dotplot(dotsize = 0.5) 

## GRAPH ----
ggp2_dotplot + 
  labs_dotplot

# DENSITY PLOTS -----------------------------------------------------------

## DATA ----
penguins |> glimpse()
## CODE ----
labs_density <- labs(
  title = "Adult foraging penguins",
  x = "Flipper length (millimeters)")
ggp2_density <- ggplot(data = penguins, 
        aes(x = flipper_length_mm)) + 
      geom_density() 
## GRAPH ----
ggp2_density +
  labs_density

# VIOLIN PLOTS ------------------------------------------------------------

## DATA ----
penguins |> glimpse()
## CODE ----
labs_violin <- labs(
  title = "Adult foraging penguins",
  subtitle = "Distribution of bill length",
  x = "",
  y = "Bill length (millimeters)")
ggp2_violin <- ggplot(data = penguins,
       aes(x = '', 
           y = bill_length_mm)) +
       geom_violin() 
## GRAPH ----
ggp2_violin + 
  labs_violin

# BOX-PLOTS ---------------------------------------------------------------

## DATA ----
movies_data |> glimpse()
## CODE ----
labs_boxplot <- labs(
  title = "IMDB Movie information and user ratings",
  y = "length", x = "")
ggp2_boxplot <- ggplot(data = movies_data, 
           aes(x = " ", 
               y = length)) +
        geom_boxplot() 

## GRAPH ----
ggp2_boxplot + 
  labs_boxplot


