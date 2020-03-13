library(shiny)
library(shinydashboard)


shinyUI(fluidPage(

  titlePanel("Covid-19 Outbreak"),

  sidebarLayout(

    sidebarPanel(
      width = 2,
      checkboxInput("cAustralia", "Australia", FALSE),
      checkboxInput("cAustria", "Austria", FALSE),
      checkboxInput("cChina", "China", TRUE),
      checkboxInput("cDenmark", "Denmark", FALSE),
      checkboxInput("cFrance", "France", FALSE),
      checkboxInput("cGermany", "Germany", TRUE),
      checkboxInput("cIran", "Iran", FALSE),
      checkboxInput("cIsrael", "Israel", FALSE),
      checkboxInput("cItaly", "Italy", FALSE),
      checkboxInput("cSingapore", "Singapore", FALSE),
      checkboxInput("cSpain", "Spain", FALSE),
      checkboxInput("cUS", "US", FALSE),
      checkboxInput("cWorld", "World", TRUE),
      checkboxInput("cWorldNoChina", "World (except China)", TRUE),
      checkboxInput("cScaleLog", "Logarithmic y-Axis", TRUE),
      checkboxInput("cShowNumber", "Show Count", TRUE)
    ),

    mainPanel(
      uiOutput("plot.ui")
    )

  )
))
