# pkgs --------------------------------------------------------------------
library(purrr)
library(fs)
library(glue)
library(stringr)


# qmd files ---------------------------------------------------------------
qmds <- c("uni", "amt", "prp", "dist", "rela", "stat") |> purrr::set_names()
graph_files <- purrr::map_df(.x = qmds, 
    .f = fs::dir_info, 
    regexp = ".qmd$", type = "file") |> 
    dplyr::select(qmd_path = path, contains("time")) |> 
    dplyr::mutate(
        qmd_path = as.character(qmd_path),
        qmd_file = base::basename(qmd_path),
        r_file = stringr::str_replace_all(
                        qmd_file, ".qmd", ".R"),
        r_path = stringr::str_replace_all(
                        qmd_path, "uni|amt|prp|dist|rela|stat", "R"),
        r_path = stringr::str_replace_all(
                        r_path, ".qmd", ".R")) |> 
    dplyr::select(contains("time"), 
                  contains("path"), 
                  contains("file"))

glimpse(graph_files)


