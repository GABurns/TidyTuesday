# Title: BYOD TidyTuesday
# Author: Gareth Burns
# Creation Date: 05/01/2024
# Description: Comparison of Google Search Trends for "Hangover Cure" &
# "Gym Membership" over New Year period.
# Disclaimer: This plot is in no form affiliated with Google but was inspired by
# their styling


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggforce)
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
GoogleOrange <- "#ffa700"


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
  ungroup() |>
  mutate(
    label = c("Hangover Cure", "Gym Membership"),
    desc = c("Searches peak at 9am on NYD", "Searches peak at 10pm on NYD")
  )

# Data for Arrows
ArrowData <- tibble(x = rep(min(data$Time) - (60 * 60 * 5), 2),
                    y = c(60, 40),
                    xend = rep(min(data$Time) - (60 * 60 * 5), 2),
                    yend = c(90, 10))

# Decided to manually lower the points for annotation so they're aligned with moving average lines
# rather than curves
AnnotationData$value <- AnnotationData$value - c(16, 10)

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
  geom_ma(n = 5, linetype = "solid") +
  geom_mark_ellipse(
    data = AnnotationData,
    aes(
      fill = name,
      label = label,
      description = desc
    ),
    n = 1,
    expand = 0,
    label.family = "Noto",
    label.fontsize = 16,
    con.colour = c(GoogleBlue, GoogleGreen),
    label.buffer = unit(25, 'mm')
  ) +
  geom_segment(data = ArrowData, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(2, "mm")), colour = GoogleRed) +
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
    axis.title.x = element_text(
      color = GoogleRed,
      face = "bold",
      size = 18,
      family = "Noto",
      margin = margin(t = 10)
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      color = GoogleRed,
      size = 16,
      family = "Noto"
    ),
    axis.text.y = element_text(
      color = GoogleRed,
      face = "bold",
      size = 16,
      family = "Noto"
    ),
    axis.line.x = element_line(color = GoogleRed),
    axis.ticks.x = element_line(color = GoogleRed),
    legend.position = "none",
    plot.caption.position = "plot",
    plot.caption = element_text(
      family = "Noto",
      colour = GoogleOrange,
      size = 14
    ),
    plot.margin = margin(rep(20, 4))
  ) +
  ggtitle(
    label = "<span style = 'font-size:18pt'><b>Google Search Trends</b><br></span>
          <span style = 'font-size:16pt'>Search Trends for <span style = 'color:#008744;'>'Hangover Cure'</span> and <span style = 'color:#0057e7;'>'Gym Membership'</span> over the new year period</span>"
  ) +
  xlab("Search Time") +
  labs(caption = "Gareth Burns | Data: Google Trend")

gg_stop_recording()

# For Camcorder
gg_playback(
  name = file.path("data", "recording", "202040103.gif"),
  first_image_duration = 1,
  last_image_duration = 10,
  frame_duration = .1,
  image_resize = 800
)
