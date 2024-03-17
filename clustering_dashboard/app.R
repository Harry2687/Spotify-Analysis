library(shiny)
library(tidyverse)
library(here)
library(foreach)
library(english)

# Load Data

load("data/R_data/kmeans_nfact.RData")

files <- list.files(
  paste0(here(), "/data/full_history_data"), 
  pattern = "*.json", 
  full.names = TRUE
)

full_streaming_history <- foreach(
  file = files, 
  .packages = c("jsonlite"),
  .combine = rbind
) %do% {
  fromJSON(file, flatten = TRUE)
} %>%
  rename(
    track_name = "master_metadata_track_name",
    artist_name = "master_metadata_album_artist_name"
  ) %>%
  mutate(
    track_uri = gsub("spotify:track:", "", spotify_track_uri),
    month = ts %>%
      substring(1, 7) %>%
      paste0("-01") %>%
      ymd()
  ) %>%
  select(-spotify_track_uri) %>%
  filter(month >= ymd("2019-04-01"))

min_date <- full_streaming_history %>%
  pull(month) %>%
  min()

max_date <- full_streaming_history %>%
  pull(month) %>%
  max()

# UI

ui <- fluidPage(
  titlePanel("Spotify Clustering"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "index", 
        "Clustering Performance Rank", 
        choices = seq(1, nrow(kmeans_nfact_save))
      ),
      sliderInput(
        "dates",
        "Streaming History Date Range",
        min = min_date,
        max = max_date,
        value = c(max_date %m-% months(6), max_date)
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Cluster Plot",
          h1(textOutput("cluster_title")),
          textOutput("cluster_description"),
          h2("Cluster Centres"),
          tableOutput("cluster_table"),
          h2("Cluster Plot"),
          plotOutput("cluster_plot")
        ),
        tabPanel(
          "Cluster History Plot", 
          h1("Cluster History Plot"),
          p("This plot shows the proportion of hours listened for each cluster."),
          plotOutput("cluster_history_plot")
        ),
        tabPanel(
          "Track Clusters", 
          h1("Track Clusters"),
          textOutput("track_clusters_desc"),
          dataTableOutput("track_clusters")
        )
      )
    )
  )
)

# SERVER

server <- function(input, output) {
  output$cluster_title <- renderText({
    rank <- input$index
    
    paste0(
      "K-means Cluster: Rank ",
      rank
    )
  })
  
  output$cluster_description <- renderText({
    total_withinss <- kmeans_nfact_save %>%
      slice(as.numeric(input$index)) %>%
      pull(total_withinss) %>%
      round(digits = 2)
    
    bsstssRatio <- kmeans_nfact_save %>%
      slice(as.numeric(input$index)) %>%
      pull(bsstssRatio) %>%
      round(digits = 2)
    
    index_ordinal <- input$index %>%
      as.numeric() %>%
      ordinal()
    
    paste0(
      "The graph below shows the k-means cluster with the ", 
      index_ordinal,
      " highest BSS/TSS ratio of ",
      bsstssRatio,
      " and a total within sum of squares of ",
      total_withinss,
      "."
    )
  })
  
  output$track_clusters_desc <- renderText({
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    
    paste0(
      "This table shows the cluster of all tracks listened to from ",
      start_date,
      " to ",
      end_date,
      "."
    )
  })
  
  output$track_clusters <- renderDataTable({
    kmeans_nfact_save %>%
      slice(as.numeric(input$index)) %>%
      pull(km) %>%
      pluck(1) %>%
      pluck("cluster") %>%
      as.data.frame() %>%
      rownames_to_column("track_artist") %>%
      rename(cluster = 2) %>% 
      mutate(cluster = paste0("Cluster ", cluster)) %>%
      left_join(
        full_streaming_history %>%
          filter(
            month %>%
              between(input$dates[1], input$dates[2])
          ) %>%
          select(
            track_name,
            artist_name
          ) %>%
          na.omit() %>%
          distinct() %>%
          mutate(
            track_artist = paste(
              track_name,
              artist_name,
              sep = " - "
            )
          ),
        .,
        by = "track_artist"
      ) %>%
      select(
        cluster,
        artist_name,
        track_name
      ) %>%
      arrange(
        cluster,
        artist_name,
        track_name
      ) %>%
      rename(
        Cluster = "cluster",
        `Artist Name` = "artist_name",
        `Track Name` = "track_name"
      )
  })
  
  output$cluster_plot <- renderPlot({
    kmeans_nfact_save %>%
      slice(as.numeric(input$index)) %>%
      pull(km_plot)
  })
  
  output$cluster_table <- renderTable({
    kmeans_nfact_save %>%
      slice(as.numeric(input$index)) %>%
      pull(km) %>%
      pluck(1) %>%
      pluck("centers") %>%
      as.data.frame() %>%
      rownames_to_column("cluster") %>%
      mutate(cluster = paste0("Cluster ", cluster))
  })
  
  output$cluster_history_plot <- renderPlot({
    track_clusters <- kmeans_nfact_save %>%
      slice(as.numeric(input$index)) %>%
      pull(km) %>%
      pluck(1) %>%
      pluck("cluster") %>%
      as.data.frame() %>%
      rownames_to_column("track_artist") %>%
      rename(cluster = 2) %>% 
      mutate(cluster = paste0("Cluster ", cluster)) %>%
      left_join(
        full_streaming_history %>%
          select(
            track_name,
            artist_name
          ) %>%
          na.omit() %>%
          distinct() %>%
          mutate(
            track_artist = paste(
              track_name,
              artist_name,
              sep = " - "
            )
          ),
        .,
        by = "track_artist"
      ) %>%
      select(-track_artist) %>%
      arrange(
        artist_name,
        track_name
      )
    
    cluster_summary <- full_streaming_history %>%
      select(
        month,
        track_name,
        artist_name,
        ms_played
      ) %>%
      na.omit() %>%
      filter(
        month %>%
          between(input$dates[1], input$dates[2])
      ) %>%
      left_join(
        track_clusters, 
        by = c("track_name", "artist_name")
      ) %>%
      group_by(
        month, 
        cluster
      ) %>%
      summarise(hours_listened = sum(ms_played/(1000*60)))
    
    ggplot(cluster_summary, aes(x = month, y = hours_listened, fill = cluster, label = cluster)) +
      xlab("Date") +
      ylab("Proportion") +
      geom_bar(position = "fill", stat = "identity")
  })
}

shinyApp(ui = ui, server = server)