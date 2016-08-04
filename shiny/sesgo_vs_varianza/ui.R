#-------------------------------------------------------#
#   Árboles de Decisión, Bagging y Bosques Aleatorios   #
#   Preparado por:      Kael Huerta y Enrique Bonilla   #
#                                 06 de Julio de 2016   #
#-------------------------------------------------------#

library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
    menuItem("Simulaciones", tabName = "plot", icon = icon("line-chart"), selected =T),
    menuItem("Teoría", tabName = "readme", icon = icon("mortar-board")),
    menuItem("Datos", tabName = "table", icon = icon("table")),
    menuItem("Códigos",  icon = icon("file-text-o"),
      menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
      menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
    ),
    menuItem("Acerca de...", tabName = "about", icon = icon("question"))
  ),
  hr(),
  conditionalPanel("input.tabs=='plot'",
    fluidRow(
      column(1),
      column(10,
        checkboxInput("real", "Modelo real", T),
        checkboxInput("simul", "Datos simulados", F),
        checkboxInput("polinomio", "Aproximación polinomial", F),
        checkboxInput("arbol", "Árboles", F)
      )
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "readme",
      withMathJax(),
      includeMarkdown("../../presentacion/clase_arboles_tufte.Rmd")
    ),
    tabItem(tabName = "plot",
      fluidRow(
        column(width = 4,
          tabBox( width = NULL,
            tabPanel(h5("Parámetros"),
              sliderInput("n", "Número de Simulaciones:", value = 11, min = 1, max = 25, step = 5),
              sliderInput("k", "Complejidad:", value = 1, min = 1, max = 10, step = 1)
            ),
            tabPanel(h5("Modelo"),
              includeMarkdown("formula.Rmd")
            )
          )),
            column(width = 8,
              box(  width = NULL, plotOutput("plot",height="500px"), collapsible = TRUE,
                title = "Varianza vs. Sesgo", status = "primary", solidHeader = TRUE)
          ))
    ),
    tabItem(tabName = "table",
      box( width = NULL, status = "primary", solidHeader = TRUE, title="Table",
        downloadButton('downloadTable', 'Download'),
        br(),br(),
        tableOutput("table")
      )
    ),
    tabItem(tabName = "ui",
      box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
        downloadButton('downloadData2', 'Download'),
        br(),br(),
        pre(includeText("ui.R"))
      )
    ),
    tabItem(tabName = "server",
      box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
        downloadButton('downloadData3', 'Download'),
        br(),br(),
        pre(includeText("server.R"))
      )
    ),
    tabItem(tabName = "about",
      includeMarkdown("acerca.Rmd")
    )
  )
)

dashboardPage(
  dashboardHeader(title = "Bosques Aleatorios"),
  sidebar,
  body
)

