# create a smaller version of movie data
library(ggplot2movies)
library(tidyverse)
library(readr)
movies <- ggplot2movies::movies
movies_data <- movies |> 
  dplyr::filter(year > 2000 & mpaa != "") |> 
  tidyr::pivot_longer(
    cols = c(Action:Short),
    names_to = "genre_key",
    values_to = "genre_value"
  ) |> 
  dplyr::select(title:rating, mpaa, genre_key, genre_value) |> 
  dplyr::filter(genre_value == 1) |> 
  dplyr::mutate(
    genre_value = case_when(
      genre_key == 'Action' ~ "action",
      genre_key == 'Animation' ~ "animation",
      genre_key == 'Comedy' ~ "comedy",
      genre_key == 'Drama' ~ "drama",
      genre_key == 'Documentary' ~ "documentary",
      genre_key == 'Romance' ~ "romance",
      genre_key == 'Short' ~ "short",
      TRUE ~ NA_character_
    )
  ) |> 
  tidyr::pivot_wider(names_from = genre_key,
    values_from = genre_value) |> 
  tidyr::unite(col = "genres",
    Action:Short,
    sep = ", ") |> 
  dplyr::mutate(
    genres = stringr::str_remove_all(string = genres, pattern = "NA, "),
    genres = stringr::str_remove_all(string = genres, pattern = ", NA")
  ) |>
  dplyr::filter(!is.na(budget)) |>
  dplyr::mutate(mpaa = factor(mpaa,
    levels = c("PG", "PG-13", "R")))

readr::write_rds(x = movies_data, file = "slides/data/movies_data.rds")
