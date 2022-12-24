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
library(patchwork)
options(scipen = 9999999)
# ggplot2 theme ----
# ggplot2::theme_set(
#     ggplot2::theme_minimal(base_size = 16))
theme_ggp2g <- function(base_size = 11, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  half_line <- base_size / 2

  # most of this is borrowed from theme_minimal()/void(), but with some
  # adjustments to panel, margins, and legend
  # https://github.com/tidyverse/ggplot2/blob/main/R/theme-defaults.r
  t <- theme(
    
    rect =               element_blank(),
      
    text =               element_text(
                            family = base_family,
                            # family = "sans",
                            face = "plain",
                            colour = "black", 
                            size = base_size,
                            lineheight = 0.9, 
                            hjust = 0.5, vjust = 0.5, 
                            angle = 0,
                            margin = margin(), 
                            debug = FALSE),
    ## AXES ----
    axis.text =          element_text(
                            family = base_family,
                            # family = "sans",
                            size = rel(0.65), 
                            face = "plain"),
      
    axis.title =         element_text(
                            size = rel(0.8), 
                            face = "plain"),
    ## TICKS ----
    axis.ticks.length =  unit(0, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    ## STRIP ----
    strip.clip =         "inherit",
    strip.text =         element_text(size = rel(0.7)),
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),
    strip.background = element_rect(fill = "#d0d0d0"),
    ## PANEL ----
    panel.ontop =        FALSE,
    panel.spacing =      unit(half_line, "pt"),
    panel.grid.major.x = element_line(color = "#d0d0d0", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "#d0d0d0", linewidth = 0.3),
    panel.grid.minor.x = element_line(color = "#d0d0d0", linewidth = 0.1),
    panel.grid.minor.y = element_line(color = "#d0d0d0", linewidth = 0.1),
    ## TITLES ----
    plot.title =         element_text(
                           size = rel(1.1),
                           hjust = 0, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.title.position = "panel",
    plot.subtitle =      element_text(
                           size = rel(0.9),
                           face = "italic",
                           hjust = 0, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.caption =       element_text(
                           size = rel(0.8),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.caption.position = "panel",
    plot.tag =           element_text(
                           size = rel(1.2),
                           hjust = 0.5, vjust = 0.5
                         ),
    plot.tag.position =  'topleft',
    ## LEGEND ----
    legend.box =         NULL,
    legend.key.size =    unit(1.2, "lines"),
    legend.position =    c(0.95, 0.8),
    legend.justification = c(-0.7, 0.5), # c(horizontal, vertical)
    legend.text =        element_text(
                            face = "italic",
                            size = rel(0.7)),
    legend.title =       element_text(
                            size = rel(0.8), 
                            hjust = 0),
    legend.margin =      margin(t = 1, r = 1, b = 1, l = 1),
    ## PLOT MARGIN ----
    plot.margin =        unit(c(1.5, 8, 1.5, 2), "lines"),

    complete = TRUE
 )
  return(t)
}
# set ----
# ggplot2::theme_set(
#     theme_ggp2g(base_size = 16))
# test ----
# toy <- data.frame(
#   const = 1, 
#   up = 1:4,
#   txt = letters[1:4], 
#   big = (1:4)*1000,
#   log = c(2, 5, 10, 2000)
# )
# 
# base <- ggplot(toy, aes(up, up)) + 
#   geom_point(aes(colour = txt), size = 3) + 
#   xlab(NULL) + 
#   ylab(NULL)
# 
# base