# Title: BYOD TidyTuesday
# Author: Gareth Burns
# Creation Date: 05/01/2024
# Description: Comparison of Google Search Trends for "Hangover Cure" &
# "Gym Membership" over New Year period.


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(showtext)
library(here)

font_add_google("Roboto Slab", "slab")
showtext_auto()


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

# Plot --------------------------------------------------------------------
