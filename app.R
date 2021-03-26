# SETUP -------------------------------------------------------------------

paquetes <- list(
  "Shiny Core" = list("shiny", "bs4Dash"),
  "Shiny Extras" = list("shinyWidgets","dashboardthemes"),
  "Plotting" = list("ggplot2"),
  "Tidyverse" = list("tidyverse", "glue"),
  "Generales" = list("readr")
)
lapply(as.list(c(paquetes, recursive = T, use.names = F)),
       function(x) {
         if (x %in% rownames(installed.packages()) == FALSE) {
           install.packages(x, verbose = F)
         }
         library(x, character.only = T, verbose = F)
       })
rm(list = c("paquetes"))

# Load data----------------------------------------------------------------
dataset <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("SteamCharts"),
  theme = bslib::bs_theme(bootswatch = "darkly"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "range",
                  label = "Year range:",
                  min = 2012, 
                  max = 2021,
                  value = c(2012,2021),
                  round = TRUE,
                  ticks = FALSE,
                  sep = ""
      ),
      textOutput(outputId = "time"),
      uiOutput("help_text"),
      
      dropdownButton(
        radioButtons(inputId = 'theme',
                     label = 'Dashboard Theme',
                     choices =  c('grey_dark',
                                  'purple_gradient'))
      ),
      uiOutput("myTheme"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(id = "tabcard",
                  side = "left",
                  tabPanel(tabName = "Dashboard",
                           plotOutput("plot_by_year", width = "400px"),
                           dataTableOutput("datatable")
                  )
                  
      )
    )
  )
)


server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  reactiveDf <- reactive({return(
    dataset %>%
      filter(
        year >= input$range[1],
        year <= input$range[2]
      )
  )})
  
  output$datatable <- renderDataTable(reactiveDf(), options = list(pageLength = 10))
  
  output$plot_by_year <- renderPlot({
    ggplot(reactiveDf()) +
      aes(x = year) +
      geom_bar(fill = "#85f979")
  })
  
  output$time <- renderText({
    glue("Last reload: {t}", t = Sys.time())
  })
  
  output$help_text <- renderUI({
    HTML("<br><b>Click below to change appearance</b>")
  })
  
  output$myTheme <- renderUI( shinyDashboardThemes(theme = input$theme))
  
}

shinyApp(ui = ui, server = server)