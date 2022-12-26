require(ggplot2)
require(extrafont)
require(showtext)
require(showtextdb)
require(sysfonts)
library(extrafont)
# import font like the function in hrbrthemes 
# https://github.com/hrbrmstr/hrbrthemes/blob/3e8d9494a9e0026a3127f6a0df88208511cd0715/R/roboto-condensed.r#L208
# the call the font_import() is called inside each setup chunk 
# extrafont::font_import(
#     paths = "assets/Ubuntu/", 
#     prompt = FALSE)
# we also add the font using font_add()
# sysfonts::font_add(
#     family =  "Ubuntu", 
#     regular = "assets/Ubuntu/Ubuntu-Regular.ttf")
# then we use the showtext_auto() to use the text.
# showtext::showtext_auto()
# "If turned on, any newly opened graphics devices will use 
# showtext to draw text"
# theme_ggp2g ----
# this is a custom theme for the ggplot2 gallery
theme_ggp2g <- function(base_size = 11, base_family = "Ubuntu", 
                        base_line_size = base_size / 22, 
                        base_rect_size = base_size / 22) {
    
  half_line <- base_size / 2

  # most of this is borrowed from theme_minimal()/void(), but with some
  # adjustments to panel, margins, and legend see the orignal here:
  # https://github.com/tidyverse/ggplot2/blob/d9f179b038f020158773fac54af9a84cf961b54b/R/theme-defaults.r#L459
  thm <- theme(

    # RECTANGLE ----------------------------------------------------
    rect =               element_blank(),
    # TEXT ---------------------------------------------------------
    text =               element_text(
                            family = "Ubuntu",
                            # face = "plain",
                            colour = "black", 
                            size = base_size,
                            lineheight = 0.9, 
                            hjust = 0.5, vjust = 0.5, 
                            angle = 0,
                            margin = margin(), 
                            debug = FALSE),
    ## AXES --------------------------------------------------------------
    axis.text =          element_text(
                            # face = "plain",
                            family = base_family,
                            size = rel(0.65)),
      
    axis.title =         element_text(
                            # face = "plain",
                            size = rel(0.8), 
                            ),
    ### AXIS TICKS ----
    ## remove all axis ticks
    ## https://ggplot2-book.org/polishing.html#theme-axis
    axis.ticks.length =  unit(0, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    ## STRIP ----
    ## 
    strip.clip =         "inherit",
    strip.text.x =         element_text(size = rel(0.7)),
    # strip.text.x affects both facet_wrap() or facet_grid()
    strip.text.y =         element_text(size = rel(0.7)),
    # strip.text.y only affects facet_grid()
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),
    strip.background = element_rect(fill = "#d0d0d0"),
    ## PANEL ----
    ## here we introduce light gray lines for the graph area (with vertical 
    ## lines slightly larger than horizontal lines)
    ## https://ggplot2-book.org/polishing.html#panel-elements
    panel.ontop =        FALSE,
    panel.spacing =      unit(half_line, "pt"),
    panel.grid.major.x = element_line(color = "#d0d0d0", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "#d0d0d0", linewidth = 0.3),
    panel.grid.minor.x = element_line(color = "#d0d0d0", linewidth = 0.1),
    panel.grid.minor.y = element_line(color = "#d0d0d0", linewidth = 0.1),
    ## TITLES ----
    ## slightly larger text
    plot.title =         element_text(
                           size = rel(1.1),
                           hjust = 0, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    # location of title
    plot.title.position = "panel",
    # slightly smaller text, italic 
    plot.subtitle =      element_text(
                           size = rel(0.9),
                           # face = "italic",
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
    ## https://ggplot2-book.org/polishing.html#legend-elements
    legend.box =         NULL,
    legend.key.size =    unit(1.2, "lines"),
    legend.position =    c(0.95, 0.8),
    legend.justification = c(-0.7, 0.5), # c(horizontal, vertical)
    legend.text =        element_text(
                            # face = "italic",
                            size = rel(0.7)),
    legend.title =       element_text(
                            size = rel(0.8), 
                            hjust = 0),
    legend.margin =      margin(t = 1, r = 1, b = 1, l = 1),
    ## PLOT MARGIN ----
    ## https://ggplot2-book.org/polishing.html#plot-elements
    ## 
    plot.margin =        unit(c(1.5, 8, 1.5, 2), "lines"),

    complete = TRUE
 )
  return(thm)
}
# set ----
ggplot2::theme_set(
    theme_ggp2g(base_size = 16))
# test ----
toy <- data.frame(
  const = 1,
  up = 1:4,
  txt = letters[1:4],
  big = (1:4)*1000,
  log = c(2, 5, 10, 2000)
)

base <- ggplot(toy, aes(up, up)) +
  geom_point(aes(colour = txt), size = 3) +
    geom_line() +
  xlab(NULL) +
  ylab(NULL) + 
    labs(title = "Fira Sans", subtitle = "Red+Hat+Mono")

base