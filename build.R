library(tidyverse)
library(fs)
library(stringr)
library(purrr)

# fonts ----
library(extrafont)
library(sysfonts)
# import font
extrafont::font_import(
    paths = "assets/Ubuntu/",
    prompt = FALSE)
# add font
sysfonts::font_add(
    family =  "Ubuntu", 
    regular = "assets/Ubuntu/Ubuntu-Regular.ttf")
# use font
showtext::showtext_auto()
# add theme
source("R/theme_ggp2g.R")
# set theme
ggplot2::theme_set(theme_ggp2g(
    base_size = 16))

source("R/new_graph.R")
source("R/new_geom.R")

# create geom scripts
ggp2_funs <- lsf.str("package:ggplot2")
geom_funs <- ggp2_funs[str_detect(ggp2_funs, "^geom_")]
geom_files <- paste0("R/", geom_funs, ".R")
geom_files
# map(.x = geom_files, .f = fs::file_create)
# add headers
script_header <- "# pkgs ----
install.packages(c('palmerpenguins', 'ggplot2movies', 
       'ggplot2', 'dplyr', 'tidyr'))
library(palmerpenguins)
library(ggplot2movies)
library(ggplot2)
library(dplyr)
library(tidyr)

# data ----

# code ----

# graph ----"

# write_lines(x = script_header, file = "R/geom_abline.R", append = TRUE)
# map2(.x = script_header, .y = geom_files, .f = write_lines, append = TRUE)

new_graph(name = "cleveland dot plots", section = "amt")
