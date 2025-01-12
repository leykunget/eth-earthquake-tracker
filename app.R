library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(leaflet)
library(gt)
library(lubridate)
library(bslib)
library(htmltools)
library(rnaturalearth)
library(rnaturalearthdata)

#' Ethiopia Earthquake Tracker
#' @author Leykun Getaneh
#' @version 1.0

# Function to fetch earthquake data
fetch_earthquake_data <- function(days_ago = 30) {
  # Define Ethiopia's bounding box
  bbox <- c(33, 3, 48, 15)  # min_long, min_lat, max_long, max_lat
  
  end_time <- Sys.time()
  start_time <- end_time - days(days_ago)
  
  start_str <- format(start_time, "%Y-%m-%d")
  end_str <- format(end_time, "%Y-%m-%d")
  
  base_url <- "https://earthquake.usgs.gov/fdsnws/event/1/query"
  query <- list(
    format = "geojson",
    starttime = start_str,
    endtime = end_str,
    minlatitude = bbox[2],
    maxlatitude = bbox[4],
    minlongitude = bbox[1],
    maxlongitude = bbox[3]
  )
  
  response <- GET(base_url, query = query)
  data <- fromJSON(rawToChar(response$content))
  
  if (length(data$features) > 0) {
    earthquakes <- data.frame(
      time = as.POSIXct(data$features$properties$time/1000, origin="1970-01-01"),
      latitude = sapply(data$features$geometry$coordinates, `[`, 2),
      longitude = sapply(data$features$geometry$coordinates, `[`, 1),
      depth = sapply(data$features$geometry$coordinates, `[`, 3),
      magnitude = data$features$properties$mag,
      place = gsub(", Ethiopia$", "", data$features$properties$place) 
      # place = data$features$properties$place
    ) %>%
      arrange(desc(time))
    return(earthquakes)
  } else {
    return(data.frame())
  }
}

# Fetch data
recent_quakes <- fetch_earthquake_data(30)

now_et <- now(tzone = "Africa/Addis_Ababa")
last_24 <- recent_quakes %>% filter(time > (now_et - hours(24)))
n_24 <- nrow(last_24)
hours_last <- round(difftime(now_et, recent_quakes$time[1], units = "hours"))

# Get Ethiopia boundary data
ethiopia_boundary <- ne_countries(scale = "medium", country = "Ethiopia", returnclass = "sf")


# my_theme <- bs_theme(
#   version = 5,
#   bootswatch = "flatly",
#   primary = "#2C3E50",
#   "navbar-bg" = "#2C3E50"
# )
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C3E50",     
  success = "#2C3E50",     
  # danger = "#FF5722",      
  "navbar-bg" = "#455A64", 
  "navbar-light-brand-color" = "#FFFFFF",
  "navbar-light-color" = "#FFFFFF",
  "navbar-light-hover-color" = "#FFB74D"
)

ui <- page_navbar(
  title = div(
    tags$span("Ethiopia Earthquake Tracker", style = "font-size: 20px;"),
    tags$span(" | ", style = "color: #665;"),
    tags$span("by Leykun Getaneh", style = "font-size: 14px;font-style: italic;")
  ),
  theme = my_theme,
  
  # Add custom CSS to reduce navbar height only
  header = tags$head(
    tags$style(HTML("
      .navbar {
        min-height: 40px !important;
        padding-top: 0 !important;
        padding-bottom: 0 !important;
      }
      .navbar-brand {
        padding-top: 2px !important;
        padding-bottom: 2px !important;
        height: 40px !important;
        display: flex !important;
        align-items: center !important;
      }
      .navbar .container-fluid {
        min-height: 40px !important;
      }
    "))
  ),
  
  sidebar = sidebar(
    title = "Filter Options",
    width = "200px",
    selectInput("time_filter", "Time period:",
                choices = c("Last 24 hours" = 1, 
                            "Last 7 days" = 7,
                            "Last 14 days" = 14,
                            "Last 30 days" = 30),
                selected = 7),
    sliderInput("mag_range", "Magnitude Range:",
                min = 0, max = 8, 
                value = c(0, 8),
                step = 0.1)
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    
    # First column: Value boxes and table
    card(
      height = "800px",
      layout_columns(
        value_box(
          title = "Hours since last earthquake",
          value = textOutput("hours_last"),
          showcase = bsicons::bs_icon("stopwatch"),
          theme = "primary",
          height = "130px"
        ),
        value_box(
          title = "Earthquakes in selected period",
          value = textOutput("quakes_24h"),
          showcase = bsicons::bs_icon("activity"),
          theme = "warning",
          height = "130px"
        )
      ),
      div(
        style = "height: calc(100% - 160px); overflow-y: auto;",
        gt_output("earthquake_table")
      )
    ),
    
    # Second column: Map
    card(
      card_header("Earthquake Map"),
      leafletOutput("map", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  quake_data <- reactiveVal(data.frame(
    time = as.POSIXct(character()),
    latitude = numeric(),
    longitude = numeric(),
    depth = numeric(),
    magnitude = numeric(),
    place = character()
  ))
  
  format_datetime <- function(datetime) {
    format(datetime, "%d/%m/%Y %I:%M %p")
  }
  
  observe({
    invalidateLater(1000 * 60 * 5)  # Update every 5 minutes
    days_to_fetch <- as.numeric(input$time_filter)
    recent_quakes <- fetch_earthquake_data(days_to_fetch)
    
    if (nrow(recent_quakes) > 0) {
      recent_quakes <- recent_quakes %>%
        mutate(
          time = force_tz(time, "Africa/Addis_Ababa")
        )
      
      quake_data(recent_quakes)
    }
  })
  
  filtered_data <- reactive({
    req(input$time_filter, input$mag_range)
    df <- quake_data()
    
    if (nrow(df) == 0) return(df)
    
    now_et <- now(tzone = "Africa/Addis_Ababa")
    days_ago <- as.numeric(input$time_filter)
    
    df %>%
      filter(
        time >= (now_et - days(days_ago)),
        magnitude >= input$mag_range[1],
        magnitude <= input$mag_range[2]
      )
  })
  
  output$hours_last <- renderText({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) return("N/A")
    
    now_et <- now(tzone = "Africa/Addis_Ababa")
    round(difftime(now_et, filtered_data()$time[1], units = "hours"))
  })
  
  output$quakes_24h <- renderText({
    req(filtered_data())
    nrow(filtered_data())
  })
  
  output$earthquake_table <- render_gt({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) return(NULL)
    
    now_et <- now(tzone = "Africa/Addis_Ababa")
    
    mag_pal <- function(x) {
      rgb(colorRamp(c("#FFEDA0", "#FEB24C", "#FC4E2A"))(x/8), maxColorValue = 255)
    }
    
    filtered_data() %>%
      slice(1:10) %>%
      mutate(
        date = format(time, "%d/%m/%Y"),
        time_only = format(time, "%I:%M %p")
      ) %>%
      select(magnitude, date, time_only, place, depth) %>%
      gt() %>%
      cols_label(
        date = "Date",
        time_only = "Time",
        place = "Location",
        magnitude = "Magnitude",
        depth = "Depth"
      ) %>%
      fmt_integer(
        columns = depth,
        pattern = "{x} km"
      ) %>%
      fmt_number(
        columns = magnitude,
        decimals = 1
      ) %>%
      data_color(
        columns = "magnitude",
        fn = mag_pal
      ) %>%
      tab_header(
        title = md("**Recent Earthquakes**")
      ) %>%
      tab_source_note(
        source_note = md(paste("Data from USGS API at", format_datetime(now_et)))
      )
  })
  
  output$map <- renderLeaflet({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) {
      return(leaflet() %>%
               addProviderTiles(providers$CartoDB.Positron) %>%
               addPolygons(data = ethiopia_boundary,
                           fillColor = "transparent",
                           weight = 2,
                           color = "#444444",
                           dashArray = "3") %>% 
               setView(lng = 40, lat = 9, zoom = 5))
    }
    
    mag_pal <- colorBin("YlOrRd", domain = c(0, 6), bins = c(0, 1, 2, 3, 4, 5, 6, 7, 8))
    
    filtered_data() %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # Add Ethiopia boundary without label
      addPolygons(data = ethiopia_boundary,
                  fillColor = "transparent",
                  weight = 2,
                  color = "#444444",
                  dashArray = "3") %>%  
      setView(lng = 40, lat = 9, zoom = 5) %>%
      addCircleMarkers(
        color = ~mag_pal(magnitude),
        stroke = FALSE,
        fillOpacity = 0.7,
        radius = ~sqrt(magnitude) * 3,
        popup = ~paste(
          "<strong>Date:</strong>", format(time, "%d/%m/%Y"), "<br/>",
          "<strong>Time:</strong>", format(time, "%I:%M %p"), "<br/>",
          "<strong>Magnitude:</strong>", round(magnitude, 1), "<br/>",
          "<strong>Depth:</strong>", round(depth, 1), "km<br/>",
          "<strong>Location:</strong>", place
        ),
        labelOptions = labelOptions(textsize = "15px")
      ) %>%
      addLegend(
        title = "Magnitude",
        position = "bottomright",
        pal = mag_pal,
        values = ~magnitude,
        opacity = 1
      )
  })
}

shinyApp(ui, server)
