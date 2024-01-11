# Title: BYOD TidyTuesday
# Author: Gareth Burns
# Creation Date: 09/01/2024
# Description: Circular barplot styled to look like hockey puck
# Link: Based off https://r-graph-gallery.com/web-circular-barplot-with-R-and-ggplot2.html

# Load Libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(ggimage)
library(cowplot)
library(camcorder)


# Load Data ---------------------------------------------------------------

tuesdata <- tt_load('2024-01-09')

# Data Wrangling ----------------------------------------------------------
data <- tuesdata$nhl_rosters |>
  filter(season == 20232024, !(birth_country %in% c("CAN", "USA"))) |>
  group_by(birth_country) |>
  summarise(height = mean(height_in_centimeters),
            weight = mean(weight_in_kilograms),
            count = n())

background <- data.frame(country = unique(data$birth_country), n = max(data$count), count = data$count)

# Source https://1000logos.net/nhl-logo/ accessed on 11th Jan 2024
image <- image_read("https://1000logos.net/wp-content/uploads/2017/05/NHL-Logo.png")



# Plot --------------------------------------------------------------------

plot <- ggplot(data) +
  geom_col(
    data = background,
    aes(
      x = reorder(str_wrap(country, 5), count),
      y = n,
      fill = n,
    ),
    colour = "black",
    fill = "black",
    show.legend = FALSE,
    width = 1
  ) +

  # Make custom panel grid
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0, 25, 50, 75)),
    color = "lightgrey"
  ) +
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(birth_country, 5), count),
      y = count,
      fill = count,
    ),colour = "white",
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  scale_fill_distiller(type = "seq",
                        direction = -1,
                        palette = "Greys") +
  # Make it circular!
  coord_polar() +
  # Annotate custom scale inside plot
  annotate(
    x = 11.7,
    y = 18,
    label = "25",
    geom = "text",
    color = "white",
  ) +
  annotate(
    x = 11.7,
    y = 43,
    label = "50",
    geom = "text",
    color = "white",
  ) +
  annotate(
    x = 11.7,
    y = 68,
    label = "75",
    geom = "text",
    color = "white",
  ) +
  # Scale y axis so bars don't start in the center
  scale_y_continuous(
    limits = c(-40, 75),
    expand = c(0, 0),
    breaks = c(0, 25, 50, 75)
  ) +
  # Customize general theme
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "none",

    # Set default color and font family for the text
    text = element_text(color = "gray12"),

    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 15),
    plot.caption = element_text(size = 12),

    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Nationality of Foreign Players in NHL",
    subtitle =
      "\nIn the latest NHL season there were 798 players born outside of North America. These\ncame from 19 different countries with 50% of players coming from Nordic countries ",
    caption = "Plot: Gareth Burns | #TidyTuesday")

# Adding Logo
ggdraw(plot) + draw_image(image, scale = 0.2, vjust = 0.06)



