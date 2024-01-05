# Title: BYOD TidyTuesday
# Author: Gareth Burns
# Creation Date: 05/01/2024
# Description: Comparison of Google Search Trends for "Hangover Cure" &
# "Gym Membership" over New Year period.
# Disclaimer: This plot is in no form affiliated with Google but was inspired by
# their styling


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(camcorder)
library(lubridate)
library(showtext)
library(ggtext)
library(here)

font_add_google("Noto sans", "Noto")
showtext_auto()


# Local Variables ---------------------------------------------------------
# From https://www.color-hex.com/color-palette/1872 accessed on 05/01/2024

GoogleGreen <- "#008744"
GoogleBlue <- "#0057e7"
GoogleRed <- "#d62d20"
GoogleOrange <-"#ffa700"


# Load Data ---------------------------------------------------------------
# Data obtained from Google Search Trends:
# https://trends.google.com/trends/explore?date=now%207-d&geo=GB&q=Gym%20Membership,Hangover%20cure&hl=en-GB
# accessed on 05/01/2024

data <- read_csv(file = "data/hangover_v_gym.csv", skip = 2)

# Data Wrangling ----------------------------------------------------------
data <-  data |>
  rename(
    c(`Gym Membership` = `Gym Membership: (United Kingdom)`,
      `Hangover Cure` = `Hangover cure: (United Kingdom)`)
  ) |>
  pivot_longer(!Time)

# Data for annotation
AnnotationData <-  data |>
  group_by(name)  |>
  filter(value == max(value)) |>
  mutate(desc = c("Searches for Hangover Cure peak at 9am on NYD", "Searches for Gym Membership peak at 10pm on NYD"))

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
  dpi = 300       # dpi to use when saving image
)

ggplot(data, aes(
  x = Time,
  y = value,
  colour = as.factor(name)
)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_ma(n = 5, linetype = "solid", size = 1.5) +
  scale_color_manual(values = c(GoogleBlue, GoogleGreen)) +
  scale_y_continuous(
    labels = c("Less Searches", "More Searches"),
    breaks = c(25, 75),
    limits = c(0, 100)
  ) +
  scale_x_datetime(date_breaks = "day", date_labels = "%d %b") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      margin = margin(0, 0, 5.5, 0)
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(
      size = 20,
      family = "Noto",
      color = "red"
    ),
    axis.title.x = element_text(
      color = GoogleRed,
      face = "bold",
      size = 25,
      family = "Noto",
      margin = margin(t = 10)
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      color = GoogleRed,
      size = 20,
      family = "Noto"
    ),
    axis.text.y = element_text(
      color = GoogleRed,
      face = "bold",
      size = 20,
      family = "Noto"
    ),
    axis.line.x = element_line(color = GoogleRed),
    axis.ticks.x = element_line(color = GoogleRed),
    legend.position = "none",
    plot.caption.position = "plot",
    plot.caption = element_text(family = "Noto", colour = GoogleOrange, size = 18),
    plot.margin = margin(rep(20, 4))
  ) +
  ggtitle(label = "<span style = 'font-size:36pt'><b>Google Search Trends</b><br></span>
          <span style = 'font-size:24pt'>Search Trends for <span style = 'color:#008744;'>'Hangover Cure'</span> and <span style = 'color:#0057e7;'>'Gym Membership'</span> over the new year period</span>") +
  xlab("Search Time") +
  labs(caption = "Gareth Burns | Data: Google Trend")


# For Camcorder
gg_playback(
  name = file.path("data", "recording", "202040101.gif"),
  first_image_duration = 5,
  last_image_duration = 15,
  frame_duration = .4,
  image_resize = 800
)