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
      tags$div(
        style = "border-top:1px solid #555555; font-size:0.9em; padding-top: 1em",
        tags$p(tags$em("Options:")),
        checkboxInput("cScaleLog", "Logarithmic y-Axis", TRUE),
        checkboxInput("cShowNumber", "Show count", TRUE),
        checkboxInput("cIncludeDeath", "Include casualties (adds number of casualties to recovered cases)", FALSE),
        checkboxInput("cOnePlot", "All in one Plot", FALSE)
      ),
      # checkboxInput("cSelectAll", "Select all countries", FALSE),
      tags$div(style = "border-top:1px solid #999999; font-size:0.8em; color: #666666; padding-top: 1em", tags$p(
        "Data: ", tags$a(href = "https://github.com/CSSEGISandData/COVID-19", "CCSE / John Hopkins University"),
        tags$br(),
        "Source code on ", tags$a(href = "https://github.com/strengejacke/Corona-19-shiny", "GitHub")
      ))

    ),

    mainPanel(
      uiOutput("plot.ui")
    )

  )
))
