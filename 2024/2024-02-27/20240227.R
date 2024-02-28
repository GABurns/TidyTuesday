# Title:  Are you more likely to die on your birthday?
# Author: Gareth Burns
# Creation Date: 26/02/2024
# Description: Dumbbell plot styled in Love Hearts theme for valentines day
# Link: Inspiration https://r-graph-gallery.com/web-stacked-area-chart-inline-labels.html

# Load Libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(camcorder)library(ggtext)


# Local Variables ---------------------------------------------------------

BgColour <- "black"
PrimaryColour <- "white"
SecondaryColour <- "darkred"

sysfonts::font_add_google("Love Ya Like A Sister", "Love Heart")
showtext::showtext_auto()

# Load Data ---------------------------------------------------------------

tuesdata <- tt_load("2024-02-27")

# MOCK FOR TEAMS MESSAGE

deaths <- ceiling(sort(rexp(365, rate = 0.05)))

data <- data.frame(DayOfYear = rep(1:365, 2),
                   Deaths = c(rep("Non-Birthday", 365), rep("Birthday", 365)),
                   Value = c(deaths* 10, deaths))

library(ggstream)

ggplot(data, aes(x = DayOfYear, y = Value, fill = Deaths, label = Deaths, color = Deaths)) +
  geom_stream(type = "ridge", bw = 1 ) +
  scale_fill_manual(values= c("red", "black"))+
  scale_colour_manual(values = c("white", "white")) +
  ggtitle("Are people more likely to die on their birthday?",
          subtitle = "Cumlative Deaths throughout the year") +
  ylab("Deaths") +
  theme(
    axis.line.x = element_line(linewidth = .75),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(color="black", size=10,margin = margin(5,0,0,0)),
    plot.margin = margin(20,120,20,20),
  )

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
