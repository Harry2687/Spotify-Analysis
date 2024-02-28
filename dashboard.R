library(shiny)
library(tidyverse)

load("data/R_data/kmeans_nfact.RData")

ui <- fluidPage(
  titlePanel("test app"),
  sidebarLayout(
    sidebarPanel(
      selectInput("index", "Choose index", choices = seq(1, nrow(kmeans_nfact_save))),
    ),
    mainPanel(
      plotOutput("cluster_plot")
    )
  )
)

server <- function(input, output) {
  output$cluster_plot <- renderPlot({
    kmeans_nfact_save %>%
      slice(as.numeric(input$index)) %>%
      pull(km_plot) %>%
      print()
  })
}

shinyApp(ui = ui, server = server)