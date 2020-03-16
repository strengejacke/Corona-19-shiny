library(shiny)
library(shinydashboard)


shinyUI(fluidPage(

  titlePanel("Covid-19 Outbreak"),

  sidebarLayout(

    sidebarPanel(
      width = 3,
      checkboxInput("cAustralia", "Australia", FALSE),
      checkboxInput("cAustria", "Austria", FALSE),
      checkboxInput("cChina", "China", TRUE),
      checkboxInput("cDenmark", "Denmark", FALSE),
      checkboxInput("cFrance", "France", FALSE),
      checkboxInput("cGermany", "Germany", TRUE),
      checkboxInput("cIran", "Iran", FALSE),
      checkboxInput("cIsrael", "Israel", FALSE),
      checkboxInput("cItaly", "Italy", FALSE),
      checkboxInput("cNetherlands", "Netherlands", FALSE),
      checkboxInput("cPoland", "Poland", FALSE),
      checkboxInput("cSingapore", "Singapore", FALSE),
      checkboxInput("cSpain", "Spain", FALSE),
      checkboxInput("cUK", "United Kingdom", FALSE),
      checkboxInput("cUS", "US", FALSE),
      checkboxInput("cWorld", "World", TRUE),
      checkboxInput("cWorldNoChina", "World (except China)", TRUE),
      tags$div(tags$p(tags$em("--- Options:"))),
      checkboxInput("cScaleLog", "Logarithmic y-Axis", TRUE),
      checkboxInput("cShowNumber", "Show count", TRUE),
      checkboxInput("cIncludeDeath", "Include casualties (adds number of casualties to recovered cases)", FALSE),
      checkboxInput("cOnePlot", "All in one Plot", FALSE)
      # checkboxInput("cSelectAll", "Select all countries", FALSE)
    ),

    mainPanel(
      uiOutput("plot.ui")
    )

  )
))
