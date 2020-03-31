library(ggplot2)
library(dplyr)
library(tidyr)


get_corona_data <- function() {

  raw_conf <- read.csv(
    file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
    stringsAsFactors = FALSE
  )

  # Fixing typo
  raw_conf$X2.6.20[which(raw_conf$Country.Region == "Japan")] <- 25

  df_confirmed <- raw_conf %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = "date_temp",
      values_to = "cases"
    )

  # Parsing the date
  df_confirmed$month <- sub("X", "", strsplit(df_confirmed$date_temp, split = "\\.") %>% purrr::map_chr(~.x[1]) )
  df_confirmed$day <- strsplit(df_confirmed$date_temp, split = "\\.") %>% purrr::map_chr(~.x[2])
  df_confirmed$date <- as.Date(paste("2020", df_confirmed$month, df_confirmed$day, sep = "-"))

  df_confirmed <- df_confirmed %>%
    dplyr::rename(country = Country.Region, province = Province.State) %>%
    group_by(country, date) %>%
    summarise(cases = sum(cases)) %>%
    ungroup() %>%
    mutate(type = "confirmed")


  #----------------------------------------------------
  # Pulling death cases

  raw_death <- read.csv(
    file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
    stringsAsFactors = FALSE
  )


  df_death <- raw_death %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = "date_temp",
      values_to = "cases"
    )

  # Parsing the date
  df_death$month <- sub("X", "", strsplit(df_death$date_temp, split = "\\.") %>% purrr::map_chr(~.x[1]) )
  df_death$day <- strsplit(df_death$date_temp, split = "\\.") %>% purrr::map_chr(~.x[2])
  df_death$date <- as.Date(paste("2020", df_death$month, df_death$day, sep = "-"))

  df_death <- df_death %>%
    dplyr::rename(country = Country.Region, province = Province.State) %>%
    group_by(country, date) %>%
    summarise(cases = sum(cases)) %>%
    ungroup() %>%
    mutate(type = "death")



  #----------------------------------------------------
  # Pulling recovered cases

  raw_recovered <- read.csv(
    file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
    stringsAsFactors = FALSE
  )

  df_recovered <- raw_recovered %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = "date_temp",
      values_to = "cases"
    )

  # Parsing the date
  df_recovered$month <- sub("X", "", strsplit(df_recovered$date_temp, split = "\\.") %>% purrr::map_chr(~.x[1]) )
  df_recovered$day <- strsplit(df_recovered$date_temp, split = "\\.") %>% purrr::map_chr(~.x[2])
  df_recovered$date <- as.Date(paste("2020", df_recovered$month, df_recovered$day, sep = "-"))

  df_recovered <- df_recovered %>%
    dplyr::rename(country = Country.Region, province = Province.State) %>%
    group_by(country, date) %>%
    summarise(cases = sum(cases)) %>%
    ungroup() %>%
    mutate(type = "recovered")



  dplyr::bind_rows(df_confirmed, df_death, df_recovered) %>%
    dplyr::arrange(country, date) %>%
    dplyr::ungroup()
}


coronavirus <- get_corona_data()
