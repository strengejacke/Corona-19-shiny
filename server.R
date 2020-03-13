library(rlang)
library(ggplot2)
library(shiny)

get_country_data <- function(country) {
  coronavirus %>%
    filter(type != "death", country == {{country}}) %>%
    mutate(region = {{country}}) %>%
    ungroup()
}




shinyServer(function(input, output) {

  country_data <- reactive({

    dat <- data.frame()

    if (input$cWorld) {
      dat <- rbind(
        dat,
        coronavirus %>%
          filter(type != "death") %>%
          group_by(date, type) %>%
          mutate(cases = sum(cases), region = "World") %>%
          ungroup()
      )
    }

    if (input$cWorldNoChina) {
      dat <- rbind(
        dat,
        coronavirus %>%
          filter(type != "death", country != "China") %>%
          group_by(date, type) %>%
          mutate(cases = sum(cases), region = "World except China") %>%
          ungroup()
      )
    }

    if (input$cChina) {
      dat <- rbind(dat, get_country_data("China"))
    }

    if (input$cGermany) {
      dat <- rbind(dat, get_country_data("Germany"))
    }

    if (input$cItaly) {
      dat <- rbind(dat, get_country_data("Italy"))
    }

    if (input$cFrance) {
      dat <- rbind(dat, get_country_data("France"))
    }

    if (input$cUS) {
      dat <- rbind(dat, get_country_data("US"))
    }

    if (input$cSpain) {
      dat <- rbind(dat, get_country_data("Spain"))
    }

    if (input$cAustria) {
      dat <- rbind(dat, get_country_data("Austria"))
    }

    if (input$cAustralia) {
      dat <- rbind(dat, get_country_data("Australia"))
    }

    if (input$cSingapore) {
      dat <- rbind(dat, get_country_data("Singapore"))
    }

    if (input$cDenmark) {
      dat <- rbind(dat, get_country_data("Denmark"))
    }

    if (input$cIsrael) {
      dat <- rbind(dat, get_country_data("Israel"))
    }

    if (input$cIran) {
      dat <- rbind(dat, get_country_data("Iran"))
    }

    dat
  })


  plotHeight <- reactive({
    x <- 0
    if (input$cAustria) x <- x + 1
    if (input$cAustralia) x <- x + 1
    if (input$cChina) x <- x + 1
    if (input$cDenmark) x <- x + 1
    if (input$cGermany) x <- x + 1
    if (input$cIran) x <- x + 1
    if (input$cIsrael) x <- x + 1
    if (input$cItaly) x <- x + 1
    if (input$cFrance) x <- x + 1
    if (input$cSingapore) x <- x + 1
    if (input$cSpain) x <- x + 1
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

      p <- ggplot(d, aes(x = date, y = cases, colour = type)) +
        geom_line() +
        # see::theme_abyss() +
        scale_color_manual(values = c("#e74c3c", "#2980b9")) +
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

      if (input$cScaleLog) {
        p <- p + scale_y_log10()
      }

      p
    }
  )

  output$plot.ui <- renderUI({
    # plotOutput("corona_plot", width = "100%", height = "500px")
    if (plotHeight() > 9) {
      plotOutput("corona_plot", width = "100%", height = "1000px")
    } else if (plotHeight() > 6) {
      plotOutput("corona_plot", width = "100%", height = "850px")
    } else {
      plotOutput("corona_plot", width = "100%", height = "600px")
    }
  })
})
