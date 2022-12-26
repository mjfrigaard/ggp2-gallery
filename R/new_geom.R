library(fs)
library(janitor)
library(stringr)

new_geom <- function(geom_name) {
    # all geom functions
    ggp2_funs <- lsf.str("package:ggplot2")
    geom_funs <- ggp2_funs[stringr::str_detect(ggp2_funs, "^geom_")]
    
    geom_nm <- str_to_lower(geom_name)
    # create path
    grph_pth <- paste0("./", "geoms", "/", geom_nm, ".qmd")
    if (section %in% sections) {
        fs::file_copy(path = "./_drafts/_new_geom.qmd", 
            new_path = grph_pth)
        fs::file_show(grph_pth)
    } else {
        cli::cli_abort("this is not a geom_ function!")
    }
}
