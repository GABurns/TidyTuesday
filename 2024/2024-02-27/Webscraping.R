# Webscraping

# Modified from https://github.com/rfordatascience/tidytuesday/tree/master/data/2024/2024-02-27
# accessed on 28th February 2024


# Load Libraries ----------------------------------------------------------

library(rvest)
library(tidyverse)
library(rlang)

dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by="days")

scrap_data <- function(date, ...) {
  month <- as.character(format(date, format = "%B"))
  day <- as.character(format(date, format = "%d"))

  webLink <- sprintf("https://en.wikipedia.org/wiki/%s_%s", month, day)

  # Read the HTML once so we don't have to keep hitting it.
  htmlSite <- rvest::read_html(webLink)

  # Find the headers. We'll use these to figure out which bullets are "inside"
  # each header, since nothing "contains" them to make it easy.
  h2s <- htmlSite |>
    rvest::html_elements("h2") |>
    rvest::html_text2() |>
    stringr::str_remove("\\[edit\\]")

  # We'll get all bullets that are after each header. We can then subtract out
  # later lists to figure out what's under a particular header.
  bullets_after_headers <- purrr::map(
    h2s,
    \(this_header) {
      this_selector <- glue::glue("h2:contains('{this_header}') ~ ul > li")
      htmlSite |>
        rvest::html_elements(this_selector) |>
        rvest::html_text2() |>
        # Remove footnotes.
        stringr::str_remove_all("\\[\\d+\\]")
    }
  ) |>
    rlang::set_names(h2s)

  # Subtract subsequent bullets from each set.
  bullets_in_headers <- purrr::map2(
    bullets_after_headers[-length(h2s)],
    bullets_after_headers[-1],
    setdiff
  )

  # The three sets we care about (Events, Births, Deaths) each have their own
  # format.
  events <- tibble::tibble(events = bullets_in_headers[["Events"]]) |>
    tidyr::separate_wider_regex(
      "events",
      patterns = c(
        year = "^\\d+",
        " – ",
        event = ".*"
      ),
      too_few = "align_start"
    )
  births <- tibble::tibble(births = bullets_in_headers[["Births"]]) |>
    tidyr::separate_wider_regex(
      "births",
      patterns = c(
        year_birth = "^\\d+",
        " – ",
        person = ".*"
      ),
      too_few = "align_start"
    ) |>
    tidyr::separate_wider_regex(
      "person",
      patterns = c(
        person = "[^(]*",
        "\\(d\\. ",
        sprintf("(?:%s %s, )*", month, date),
        year_death = "\\d+",
        "\\)\\.?"
      ),
      too_few = "align_start"
    ) |>
    tidyr::separate_wider_regex(
      "person",
      patterns = c(
        person = "[^,]*",
        ", ",
        description = ".*"
      ),
      too_few = "align_start"
    )

  deaths <- tibble::tibble(deaths = bullets_in_headers[["Deaths"]]) |>
    tidyr::separate_wider_regex(
      "deaths",
      patterns = c(
        year_death = "^\\d+",
        " – ",
        person = ".*"
      ), too_few = "align_start"
    ) |>
    tidyr::separate_wider_regex(
      "person",
      patterns = c(
        person = "[^(]*",
        "\\(b\\. ",
        sprintf("(?:%s %s, )*", month, date),
        year_birth = "\\d+",
        "\\)\\.?"
      ),
      too_few = "align_start"
    ) |>
    tidyr::separate_wider_regex(
      "person",
      patterns = c(
        person = "[^,]*",
        ", ",
        description = ".*"
      ),
      too_few = "align_start"
    )

  deaths$date <- date
  births$date <- date
  events$date <- date

  return(list(deaths = deaths,
              births = births,
              events = events))

}

data <- lapply(dates, scrap_data)

deaths <- lapply(test, function(list) {
  list[[1]]
})

deaths <- do.call("rbind", deaths)

births <- lapply(test, function(list) {
  list[[2]]
})

births <- do.call("rbind", births)
