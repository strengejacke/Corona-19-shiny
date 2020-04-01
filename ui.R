library(shiny)
library(shinydashboard)


shinyUI(fluidPage(

  titlePanel("Covid-19 Outbreak"),

  sidebarLayout(

    sidebarPanel(
      width = 3,
      checkboxInput("cAustralia", "Australia", FALSE),
      checkboxInput("cAustria", "Austria", FALSE),
      checkboxInput("cBrazil", "Brazil", FALSE),
      checkboxInput("cChina", "China", TRUE),
      checkboxInput("cDenmark", "Denmark", FALSE),
      checkboxInput("cFinland", "Finland", FALSE),
      checkboxInput("cFrance", "France", FALSE),
      checkboxInput("cGermany", "Germany", TRUE),
      checkboxInput("cIran", "Iran", FALSE),
      checkboxInput("cIsrael", "Israel", FALSE),
      checkboxInput("cItaly", "Italy", FALSE),
      checkboxInput("cNetherlands", "Netherlands", FALSE),
      checkboxInput("cPoland", "Poland", FALSE),
      checkboxInput("cRussia", "Russia", FALSE),
      checkboxInput("cSingapore", "Singapore", FALSE),
      checkboxInput("cSouthKorea", "South Korea", TRUE),
      checkboxInput("cSpain", "Spain", FALSE),
      checkboxInput("cSweden", "Sweden", FALSE),
      checkboxInput("cUK", "United Kingdom", FALSE),
      checkboxInput("cUS", "US", TRUE),
      checkboxInput("cWorld", "World", FALSE),
      checkboxInput("cWorldNoChina", "World (except China)", FALSE),
      tags$div(
        style = "border-top:1px solid #555555; font-size:0.9em; padding-top: 1em",
        tags$p(tags$em("Options:")),
        checkboxInput("cScaleLog", "Logarithmic y-Axis", TRUE),
        checkboxInput("cShowNumber", "Show count", TRUE),
        checkboxInput("cIncludeDeath", "Include casualties (adds number of casualties to recovered cases)", FALSE),
        checkboxInput("cSeparateDeath", "Separate casualties and recovered", FALSE),
        checkboxInput("cOnePlot", "All in one Plot", FALSE)
      ),
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
