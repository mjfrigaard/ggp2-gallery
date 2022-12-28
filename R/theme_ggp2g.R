# pkgs --------------------------------------------------------------------
require(ggplot2)
require(extrafont)
require(showtext)
require(showtextdb)
require(sysfonts)
library(extrafont)
library(palmerpenguins)

# theme_ggp2g -------------------------------------------------------------
# this is a custom theme for the ggplot2 gallery
# import fonts like the function in hrbrthemes 
# https://github.com/hrbrmstr/hrbrthemes/blob/3e8d9494a9e0026a3127f6a0df88208511cd0715/R/roboto-condensed.r#L208
# First call the font_import() is called inside each setup chunk 
# extrafont::font_import(
#     paths = "assets/Ubuntu/", 
#     prompt = FALSE)
# we then add the font using font_add()
# sysfonts::font_add(
#     family =  "Ubuntu", 
#     regular = "assets/Ubuntu/Ubuntu-Regular.ttf")
# finally we use showtext_auto() to use the font in the graph
# showtext::showtext_auto()
# "If turned on, any newly opened graphics devices will use showtext to 
# draw text"
theme_ggp2g <- function(base_size = 11, base_family = "Ubuntu", 
                        base_line_size = base_size / 22, 
                        base_rect_size = base_size / 22) {
    
  half_line <- base_size / 2
  # most of this is borrowed from theme_minimal()/void(), but with some
  # adjustments to panel, margins, and legend see the original here:
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
                            family = base_family,
                            size = rel(0.65)
                            ),
    axis.title.x =       element_text(vjust = -4.75),
    axis.title.y =       element_text(vjust = 4.25, angle = 90),
    axis.title =         element_text(
                            size = rel(0.8)
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
    ### Title -----
    # location of title
    plot.title.position = "panel",
    ## slightly larger text
    plot.title =         element_text(
                           size = rel(1.1),
                           hjust = 0, vjust = 2,
                           margin = margin(b = half_line)
                         ),
    ### subtitle -----
    # slightly smaller text, italic 
    plot.subtitle =      element_text(
                           size = rel(0.9),
                           hjust = 0, vjust = 2,
                           margin = margin(b = half_line)
                         ),
    ### caption -----
    plot.caption =       element_text(
                           size = rel(0.8),
                           hjust = 1, vjust = 1,
                           margin = margin(b = half_line)
                         ),
      
    plot.caption.position = "panel",
    ### tag -----
    plot.tag =           element_text(
                           size = rel(1.2),
                           hjust = 0.5, vjust = 0.5
                         ),
    plot.tag.position =  'topleft',
    ## LEGEND ----
    ## https://ggplot2-book.org/polishing.html#legend-elements
    legend.box =         NULL,
    legend.key.size =    unit(1.2, "lines"),
        # the positioning is = c(horizontal, vertical)
        # c(1, 0) = bottom right
        # c(0.5, 0.5) = dead center
        # c(0, 1) = top left
    legend.position =    c(1, 0.5), # c(horizontal, vertical)
    legend.justification = c(-0.075, 0.0),

    legend.text =        element_text(
                            # face = "italic",
                            size = rel(0.7)),
    legend.title =       element_text(
                            size = rel(0.8), 
                            hjust = 0),
    legend.margin =      margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5),
    ## PLOT MARGIN ----
    ## https://ggplot2-book.org/polishing.html#plot-elements
    ## 
    plot.margin =        unit(x = c(1.5, 5.0, 1.5, 1.5), "lines"),
    complete = TRUE
 )
  return(thm)
}
# set ----
ggplot2::theme_set(
    theme_ggp2g(base_size = 16))

labs_grp_bubble <- labs(
  title = "Bill Length vs. Flipper Length",
  x = "Bill Length (mm)", 
  y = "Flipper length (mm)", 
  size = "Body Mass", 
  fill = "Species")

ggp2_grp_bubble <- penguins |> 
  ggplot(aes(
    x = bill_length_mm, y = flipper_length_mm)) + 
  geom_point(
    aes(size = body_mass_g, fill = species), 
        alpha = 2/3, shape = 21, color = "#000000") + 
  scale_size(range = c(.04, 8)) 

ggp2_grp_bubble + 
  labs_grp_bubble

# # test ----
# toy <- data.frame(
#   const = 1,
#   up = 1:4,
#   txt = letters[1:4],
#   big = (1:4)*1000,
#   log = c(2, 5, 10, 2000)
# )
# 
# base <- ggplot(data = toy, 
#         aes(x = up, y = up)) +
#     geom_point(aes(color = txt), 
#       size = 3) +
#     geom_line() +
#     labs(title = "Ubuntu", 
#          subtitle = "Ubuntu-Regular.ttf")
# 
# base