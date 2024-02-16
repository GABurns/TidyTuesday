# Title:  Loveheart
# Author: Gareth Burns
# Creation Date: 16/02/2024
# Description: Dumbbell plot styled in Love Hearts theme for valentines day
# Link: Based off https://r-graph-gallery.com/web-dumbbell-chart-with-a-gap-column.html

# Load Libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(camcorder)
library(ggtext)


# Local Variables ---------------------------------------------------------

LoveHeartRed <- "#DD211C" # Used Colour dropper tool to grab
LoveHeartYellow <- "#FEF488"
LoveHeartBlue <- "#B3E3F9"
LoveHeartPurple <- "#BE8EBE"

# From:
# https://gist.github.com/EmilHvitfeldt/a680703215c430bd1f6e2aa6831f3846
geom_heart <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                        ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`",
           call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomText,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(parse = parse, check_overlap = check_overlap,
                      na.rm = na.rm, label = sprintf('\u2661'), ...))
}


sysfonts::font_add_google("Love Ya Like A Sister", "Love Heart")
showtext::showtext_auto()

# Load Data ---------------------------------------------------------------

tuesdata <- tt_load('2024-02-13')

# Data Wrangling ----------------------------------------------------------
data <- tuesdata$gifts_gender |>
  pivot_longer(!Gender)


# Plot --------------------------------------------------------------------
# Plot --------------------------------------------------------------------
# For Camcorder using vignette arguments
gg_record(
  dir = file.path("data", "recording"),
  # where to save the recording
  device = "png",
  # device to use to save images
  width = 6,
  # width of saved image
  height = 4,
  # height of saved image
  units = "in",
  # units for width and height
  dpi = 300,       # dpi to use when saving image,
  bg = "white"
)


plot <- ggplot(data, aes(x = value, y = name)) +
  geom_line(linewidth = 5, colour = LoveHeartRed, alpha = 0.6) +
  geom_point(aes(color=Gender), size = 14) +
  geom_point(color = LoveHeartRed, pch = 21, size = 14, alpha = 0.5) +
  geom_heart(size = 11, colour = LoveHeartRed, nudge_y = -0.04) +
  geom_text(mapping = aes(label = value), size = 4, colour = LoveHeartRed, nudge_y = 0.04) +
  scale_colour_manual(values = c(LoveHeartBlue, LoveHeartYellow)) +
  scale_y_discrete(limits = c("Candy", "Flowers", "GreetingCards", "EveningOut", "Jewelry",  "Clothing", "GiftCards"), labels = c("Candy", "Flowers", "Cards", "Evening Out","Jewelry",  "Clothing", "Gift Cards"))+
  theme_minimal(base_family = "Love Heart") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        # Use gray text for the region names
        axis.text.y = element_text(color = LoveHeartPurple, size = 12),
        plot.caption = element_text(size = 12, colour = LoveHeartPurple),
        plot.title = element_markdown(size = 20, colour = LoveHeartRed),
        plot.title.position = "plot") +
  labs(title = "The percentage of <span style = 'color: #B3E3F9;'>men</span> and <span style = 'color: #FEF488;'>women</span> that spend on different gift <br> categories
       for their significant other on valentines day",
    caption = "Plot: Gareth Burns | #TidyTuesday")


gg_stop_recording()

# For Camcorder
gg_playback(
  name = file.path("data", "recording", "test.gif"),
  first_image_duration = 1,
  last_image_duration = 10,
  frame_duration = .1,
  image_resize = 800,
  bg = white
)
