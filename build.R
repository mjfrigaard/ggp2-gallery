library(tidyverse)
library(fs)
library(stringr)
library(purrr)

source("R/new_graph.R")

# create geom scripts
ggp2_funs <- lsf.str("package:ggplot2")
geom_funs <- ggp2_funs[str_detect(ggp2_funs, "^geom_")]
geom_files <- paste0("R/", geom_funs, ".R")
map(.x = geom_files, .f = fs::file_create)
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

new_graph(name = "Correlograms", section = "rela")
