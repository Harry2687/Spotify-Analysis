library(shiny)
library(tidyverse)
library(here)
library(foreach)

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

ui <- fluidPage(
  titlePanel("Spotify Streaming History"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "top_n_artists",
        "Top n Artists",
        min = 1,
        max = 20,
        value = 10
      ),
      sliderInput(
        "top_n_tracks",
        "Top n Tracks",
        min = 1,
        max = 20,
        value = 10
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
          "Artist History Plot", 
          h2(textOutput("artist_history_title")),
          plotOutput("artist_history_plot")
        ),
        tabPanel(
          "Track History Plot",
          h2(textOutput("track_history_title")),
          plotOutput("track_history_plot")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$artist_history_title <- renderText({
    top_n_artists <- input$top_n_artists
    
    paste0(
      "Proportion of Hours Listened: Top ",
      top_n_artists,
      " Artists"
    )
  })
  
  output$artist_history_plot <- renderPlot({
    top_artists <- data.frame(
      month = full_streaming_history %>%
        select(month) %>%
        distinct()
    ) %>%
      mutate(
        top_artists = map(
          month,
          ~full_streaming_history %>%
            filter(month == .x) %>%
            group_by(artist_name) %>%
            summarise(time = sum(ms_played)) %>%
            slice_max(time, n = as.numeric(input$top_n_artists)) %>%
            pull(artist_name)
        )
      )
    
    artist_summary <- full_streaming_history %>%
      filter(
        month %>%
          between(input$dates[1], input$dates[2])
      ) %>%
      left_join(
        top_artists,
        by = "month"
      ) %>%
      rowwise() %>%
      filter(artist_name %in% top_artists) %>%
      group_by(
        artist_name, 
        month
      ) %>%
      summarise(hours_listened = sum(ms_played/(1000*60)))
    
    ggplot(artist_summary, aes(x = month, y = hours_listened, fill = artist_name, label = artist_name)) +
      xlab("Date") +
      ylab("Proportion") +
      geom_bar(position = "fill", stat = "identity") +
      geom_text(size = 3, position = position_fill(vjust = 0.5)) +
      theme(legend.position = "none")
  })
  
  output$track_history_title <- renderText({
    top_n_tracks <- input$top_n_tracks
    
    paste0(
      "Proportion of Hours Listened: Top ",
      top_n_tracks,
      " Tracks"
    )
  })
  
  output$track_history_plot <- renderPlot({
    top_tracks <- data.frame(
      month = full_streaming_history %>%
        select(month) %>%
        distinct()
    ) %>%
      mutate(
        top_tracks = map(
          month,
          ~full_streaming_history %>%
            filter(month == .x) %>%
            mutate(track_artist_name = paste(track_name, artist_name, sep = "\n")) %>%
            group_by(track_artist_name) %>%
            summarise(time = sum(ms_played)) %>%
            slice_max(time, n = as.numeric(input$top_n_tracks)) %>%
            pull(track_artist_name)
        )
      )
    
    track_summary <- full_streaming_history %>%
      filter(
        month %>%
          between(input$dates[1], input$dates[2])
      ) %>%
      mutate(track_artist_name = paste(track_name, artist_name, sep = "\n")) %>%
      left_join(
        top_tracks,
        by = "month"
      ) %>%
      rowwise() %>%
      filter(track_artist_name %in% top_tracks) %>%
      group_by(
        track_artist_name, 
        month
      ) %>%
      summarise(hours_listened = sum(ms_played/(1000*60)))
    
    ggplot(track_summary, aes(x = month, y = hours_listened, fill = track_artist_name, label = track_artist_name)) +
      xlab("Date") +
      ylab("Proportion") +
      geom_bar(position = "fill", stat = "identity") +
      geom_text(size = 3, position = position_fill(vjust = 0.5)) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)