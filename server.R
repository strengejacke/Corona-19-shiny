library(rlang)
library(ggplot2)
library(ggrepel)
library(shiny)

get_country_data <- function(country, include_death = FALSE, separate_death = FALSE, only_new) {
  x <- coronavirus

  if (isTRUE(only_new)) {
    x <- x %>%
      ungroup() %>%
      filter(country == {{country}}, type == "confirmed") %>%
      mutate(daily_new = c(0, diff(cases)), region = {{country}}) %>%
      select(-country)
    x$count <- NA
    x$count[nrow(x)] <- x$daily_new[nrow(x)]
  } else if (isTRUE(include_death) && isFALSE(separate_death)) {
    x$type[x$type == "death"] <- "recovered"
    x <- x %>%
      filter(country == {{country}}) %>%
      group_by(type, date) %>%
      summarise(cases = sum(cases)) %>%
      mutate(region = {{country}}) %>%
      ungroup()
    x <- x[c("date", "cases", "type", "region")]
    x$count <- NA
    x$count[tail(which(x$type == "confirmed"), 1)] <- x$cases[tail(which(x$type == "confirmed"), 1)]
    x$count[tail(which(x$type == "recovered"), 1)] <- x$cases[tail(which(x$type == "recovered"), 1)]
  } else if (isTRUE(include_death) && isTRUE(separate_death)) {
    x <- x %>%
      ungroup() %>%
      filter(country == {{country}}) %>%
      mutate(region = {{country}}) %>%
      select(-country)

    x$count <- NA
    x$count[nrow(x) - 2] <- x$cases[nrow(x) - 2]
    x$count[nrow(x) - 1] <- x$cases[nrow(x) - 1]
    x$count[nrow(x)] <- x$cases[nrow(x)]
  } else {
    x <- x %>%
      ungroup() %>%
      filter(type != "death", country == {{country}}) %>%
      mutate(region = {{country}}) %>%
      select(-country)

    x$count <- NA
    x$count[nrow(x) - 1] <- x$cases[nrow(x) - 1]
    x$count[nrow(x)] <- x$cases[nrow(x)]
  }

  x
}




shinyServer(function(input, output) {

  country_data <- reactive({

    dat <- data.frame()

    if (input$cWorld) {
      x <- coronavirus
      if (input$cIncludeDeath) {
        x$type[x$type == "death"] <- "recovered"
      }
      x <- x %>%
        filter(type != "death") %>%
        group_by(date, type) %>%
        summarise(cases = sum(cases), region = "World", count = NA) %>%
        ungroup()

      x$count[nrow(x) - 1] <- x$cases[nrow(x) - 1]
      x$count[nrow(x)] <- x$cases[nrow(x)]

      dat <- rbind(dat, x)
    }

    if (input$cWorldNoChina) {
      x <- coronavirus
      if (input$cIncludeDeath) {
        x$type[x$type == "death"] <- "recovered"
      }
      x <- x %>%
        filter(type != "death", country != "China") %>%
        group_by(date, type) %>%
        summarise(cases = sum(cases), region = "World except China", count = NA) %>%
        ungroup()

      x$count[nrow(x) - 1] <- x$cases[nrow(x) - 1]
      x$count[nrow(x)] <- x$cases[nrow(x)]

      dat <- rbind(dat, x)
    }

    if (input$cBrazil) {
      dat <- rbind(dat, get_country_data("Brazil", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cChina) {
      dat <- rbind(dat, get_country_data("China", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cGermany) {
      dat <- rbind(dat, get_country_data("Germany", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cItaly) {
      dat <- rbind(dat, get_country_data("Italy", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cFrance) {
      dat <- rbind(dat, get_country_data("France", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cFinland) {
      dat <- rbind(dat, get_country_data("Finland", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cSweden) {
      dat <- rbind(dat, get_country_data("Sweden", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cRussia) {
      dat <- rbind(dat, get_country_data("Russia", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cUS) {
      dat <- rbind(dat, get_country_data("US", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cSpain) {
      dat <- rbind(dat, get_country_data("Spain", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cAustria) {
      dat <- rbind(dat, get_country_data("Austria", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cAustralia) {
      dat <- rbind(dat, get_country_data("Australia", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cSingapore) {
      dat <- rbind(dat, get_country_data("Singapore", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cDenmark) {
      dat <- rbind(dat, get_country_data("Denmark", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cIsrael) {
      dat <- rbind(dat, get_country_data("Israel", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cIran) {
      dat <- rbind(dat, get_country_data("Iran", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cUK) {
      dat <- rbind(dat, get_country_data("United Kingdom", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cPoland) {
      dat <- rbind(dat, get_country_data("Poland", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cNetherlands) {
      dat <- rbind(dat, get_country_data("Netherlands", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    if (input$cSouthKorea) {
      dat <- rbind(dat, get_country_data("Korea, South", include_death = input$cIncludeDeath, separate_death = input$cSeparateDeath, only_new = input$cOnlyNew))
    }

    dat
  })


  plotHeight <- reactive({
    x <- 0
    if (input$cAustria) x <- x + 1
    if (input$cAustralia) x <- x + 1
    if (input$cBrazil) x <- x + 1
    if (input$cChina) x <- x + 1
    if (input$cDenmark) x <- x + 1
    if (input$cFinland) x <- x + 1
    if (input$cFrance) x <- x + 1
    if (input$cGermany) x <- x + 1
    if (input$cIran) x <- x + 1
    if (input$cIsrael) x <- x + 1
    if (input$cItaly) x <- x + 1
    if (input$cNetherlands) x <- x + 1
    if (input$cPoland) x <- x + 1
    if (input$cRussia) x <- x + 1
    if (input$cSingapore) x <- x + 1
    if (input$cSpain) x <- x + 1
    if (input$cSouthKorea) x <- x + 1
    if (input$cSweden) x <- x + 1
    if (input$cUK) x <- x + 1
    if (input$cUS) x <- x + 1
    if (input$cWorld) x <- x + 1
    if (input$cWorldNoChina) x <- x + 1
    x
  })


  output$corona_plot <- renderPlot(
    {
      d <- country_data()
      n_col <- ceiling(sqrt(length(unique(d$region))))
      if (n_col > 3) n_col <- 3

      legend_labels <- if (input$cIncludeDeath && !input$cSeparateDeath) {
        c(confirmed = "confirmed", recovered = "recovered and casualties")
      } else if (input$cIncludeDeath && input$cSeparateDeath) {
        c(confirmed = "confirmed", recovered = "recovered", death = "casualties")
      } else {
        c(confirmed = "confirmed", recovered = "recovered")
      }

      color_values <- if (input$cIncludeDeath && !input$cSeparateDeath) {
        c("#e74c3c", "#27ae60")
      } else if (input$cIncludeDeath && input$cSeparateDeath) {
        c("#e74c3c", "#2980b9", "#27ae60")
      } else {
        c("#e74c3c", "#27ae60")
      }

      if (input$cOnlyNew) {
        p <- ggplot(d, aes(x = date, y = daily_new)) +
          geom_line(alpha = .5, colour = color_values[1]) +
          stat_smooth(method = "loess", size = .8, colour = color_values[1], se = FALSE) +
          scale_x_date(
            date_breaks = "1 week",
            date_labels = "%d.%m.",
            guide = guide_axis(n.dodge = 2)
          ) +
          labs(y = NULL, x = NULL, title = "Daily Confirmed Covid-19 by Region") +
          theme_linedraw(base_size = 14) +
          facet_wrap(~region, scale = "free_y", ncol = n_col)
      } else if (input$cOnePlot) {
        d <- d %>% filter(type == "confirmed", !(region %in% c("World except China", "World")))
        p <- ggplot(d, aes(x = date, y = cases, colour = region)) +
          geom_line() +
          scale_x_date(
            date_breaks = "1 week",
            date_labels = "%d.%m.",
            # date_minor_breaks = "1 day",
            guide = guide_axis(n.dodge = 2)
          ) +
          labs(y = NULL, colour = "Confirmed Cases", x = NULL, title = "Confirmed Covid-19 by Region") +
          theme_linedraw(base_size = 14)
      } else {
        p <- ggplot(d, aes(x = date, y = cases, colour = type)) +
          geom_line() +
          # see::theme_abyss() +
          scale_color_manual(values = color_values, labels = legend_labels) +
          scale_x_date(
            date_breaks = "1 week",
            date_labels = "%d.%m.",
            # date_minor_breaks = "1 day",
            guide = guide_axis(n.dodge = 2)
          ) +
          facet_wrap(~region, scale = "free_y", ncol = n_col) +
          labs(y = NULL, colour = "Cases", x = NULL, title = "Covid-19") +
          theme_linedraw(base_size = 14) +
          theme(
            strip.text = element_text(size = 12),
            legend.position = "bottom"
          )
      }


      if (input$cShowNumber) {
        p <- p + ggrepel::geom_label_repel(aes(label = count), show.legend = FALSE)
      }

      if (input$cScaleLog) {
        p <- p + scale_y_log10()
      }

      p
    }
  )

  output$plot.ui <- renderUI({
    # plotOutput("corona_plot", width = "100%", height = "500px")
    if (input$cOnePlot) {
      plotOutput("corona_plot", width = "100%", height = "700px")
    } else if (plotHeight() > 9) {
      plotOutput("corona_plot", width = "100%", height = "1000px")
    } else if (plotHeight() > 6) {
      plotOutput("corona_plot", width = "100%", height = "850px")
    } else {
      plotOutput("corona_plot", width = "100%", height = "600px")
    }
  })
})
