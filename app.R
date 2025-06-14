# Load necessary libraries
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
#' @version 2.0

# Function to fetch earthquake data starting from a specified date
fetch_earthquake_data <- function(start_date) {
  # Define Ethiopia's bounding box (min_long, min_lat, max_long, max_lat)
  bbox <- c(33, 3.4, 48, 15)
  
  # Convert start_date to Ethiopian time zone
  start_time <- force_tz(as.POSIXct(start_date), tzone = "Africa/Addis_Ababa")
  end_time <- with_tz(Sys.time(), "Africa/Addis_Ababa")  # Current time in Ethiopian time zone
  
  # Format start and end times as strings
  start_str <- format(start_time, "%Y-%m-%d")
  end_str <- format(end_time, "%Y-%m-%d")
  
  # Define the base URL and query parameters for the USGS API
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
  
  # Make the API request and parse the response
  response <- GET(base_url, query = query)
  data <- fromJSON(rawToChar(response$content))
  
  # Process the data into a data frame if earthquakes are found
  if (length(data$features) > 0) {
    earthquakes <- data.frame(
      time = as.POSIXct(data$features$properties$time/1000, origin="1970-01-01", tz = "UTC"),
      latitude = sapply(data$features$geometry$coordinates, `[`, 2),
      longitude = sapply(data$features$geometry$coordinates, `[`, 1),
      depth = sapply(data$features$geometry$coordinates, `[`, 3),
      magnitude = data$features$properties$mag,
      place = gsub(", Ethiopia$", "", data$features$properties$place)  # Clean location names
    ) %>%
      arrange(desc(time))  # Sort by time in descending order
    return(earthquakes)
  } else {
    return(data.frame(
      time = as.POSIXct(character()),
      latitude = numeric(),
      longitude = numeric(),
      depth = numeric(),
      magnitude = numeric(),
      place = character()
    ))  # Return an empty data frame if no earthquakes are found
  }
}

# Fetch Ethiopia's boundary data for the map
ethiopia_boundary <- ne_countries(scale = "medium", country = "Ethiopia", returnclass = "sf")

# Function to check if a point is inside Ethiopia's boundary
is_inside_ethiopia <- function(lat, lon) {
  point_sf <- st_sfc(st_point(c(lon, lat)), crs = st_crs(ethiopia_boundary))
  any(st_intersects(point_sf, ethiopia_boundary, sparse = FALSE))
}

# Define a custom Bootstrap theme for the app
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C3E50",     
  success = "#2C3E50",     
  "navbar-bg" = "#455A64", 
  "navbar-light-brand-color" = "#FFFFFF",
  "navbar-light-color" = "#FFFFFF",
  "navbar-light-hover-color" = "#FFB74D"
)

# Define the UI for the Shiny app
ui <- page_navbar(
  title = div(
    tags$span("Ethiopia Earthquake Tracker", style = "font-size: 20px;"),
    tags$span(" | ", style = "color: #665;"),
    tags$span("by Leykun Getaneh", style = "font-size: 14px;font-style: italic;")
  ),
  theme = my_theme,
  
  # Add custom CSS to style the navbar
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
  
  # Sidebar with filter options
  sidebar = sidebar(
    title = "Filter Options",
    width = "200px",
    selectInput("time_filter", "Time period:",
                choices = c("Last 24 hours" = 1, 
                            "Last 7 days" = 7,
                            "Last 30 days" = 30,
                            "Last 2 months" = 60,
                            "Last 3 months" = 90,
                            "Last 6 months" = 180,
                            "Last 9 months" = 270,
                            "Last 12 months" = 365,
                            "Last 2 years" = 730),
                selected = 180),
    sliderInput("mag_range", "Magnitude Range:",
                min = 0, max = 8, 
                value = c(0, 8),
                step = 0.1),
    downloadButton("download_data", "Download Data", 
                   class = "btn-primary btn-sm w-100")
  ),
  
  # Main layout with two columns
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
          value = textOutput("quakes_selected"),
          showcase = bsicons::bs_icon("activity"),
          theme = "warning",
          height = "130px"
        )
      ),
      div(
        style = "height: calc(100% - 160px); overflow-y: auto;",
        gt_output("earthquake_table")  # Table for recent earthquakes
      )
    ),
    
    # Second column: Map
    card(
      card_header("Earthquake Map"),
      leafletOutput("map", height = "800px")  # Interactive map
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output, session) {
  # Reactive value to store earthquake data
  quake_data <- reactiveVal(data.frame(
    time = as.POSIXct(character()),
    latitude = numeric(),
    longitude = numeric(),
    depth = numeric(),
    magnitude = numeric(),
    place = character(),
    in_ethiopia = logical()
  ))
  
  # Function to format datetime
  format_datetime <- function(datetime) {
    format(datetime, "%d/%m/%Y %I:%M %p")
  }
  
  # Observe and update earthquake data every 5 minutes
  observe({
    invalidateLater(1000 * 60 * 5)  # Update every 5 minutes
    now_et <- with_tz(Sys.time(), "Africa/Addis_Ababa")
    days_ago <- as.numeric(input$time_filter)
    start_date <- now_et - days(days_ago)  # Calculate start date based on selected time period
    
    recent_quakes <- fetch_earthquake_data(start_date)  # Fetch data starting from the calculated date
    
    if (nrow(recent_quakes) > 0) {
      # Convert time to Ethiopia's timezone
      recent_quakes <- recent_quakes %>%
        mutate(
          time = force_tz(time, "Africa/Addis_Ababa"),
          # Check if each earthquake is inside Ethiopia's boundary
          in_ethiopia = map2_lgl(latitude, longitude, is_inside_ethiopia)
        )
      
      quake_data(recent_quakes)  # Update reactive value
    }
  })
  
  # Reactive expression to filter data based on user inputs
  filtered_data <- reactive({
    req(input$time_filter, input$mag_range)
    df <- quake_data()
    
    if (nrow(df) == 0) return(df)
    
    now_et <- with_tz(Sys.time(), "Africa/Addis_Ababa")
    days_ago <- as.numeric(input$time_filter)
    start_date <- now_et - days(days_ago)  # Calculate start date based on selected time period
    
    filtered <- df %>%
      filter(
        time >= start_date,  # Filter data starting from the calculated date
        magnitude >= input$mag_range[1],
        magnitude <= input$mag_range[2],
        in_ethiopia == TRUE  # Only include earthquakes inside Ethiopia's boundary
      )
    
    return(filtered)
  })
  
  # Output: Hours since the last earthquake
  output$hours_last <- renderText({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) return("N/A")
    
    now_et <- with_tz(Sys.time(), "Africa/Addis_Ababa")
    hours <- as.numeric(difftime(now_et, filtered_data()$time[1], units = "hours"))
    round(hours)
  })
  
  # Output: Number of earthquakes in the selected period
  output$quakes_selected <- renderText({
    req(filtered_data())
    nrow(filtered_data())
  })
  
  # Output: Table of recent earthquakes
  output$earthquake_table <- render_gt({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) return(NULL)
    
    now_et <- with_tz(Sys.time(), "Africa/Addis_Ababa")
    
    mag_pal <- function(x) {
      rgb(colorRamp(c("#FFEDA0", "#FEB24C", "#FC4E2A"))(x/8), maxColorValue = 255)
    }
    
    filtered_data() %>%
      slice(1:min(10, nrow(.))) %>%  # Show top 10 earthquakes (or fewer if less available)
      mutate(
        date = format(time, "%d/%m/%Y"),
        time_only = format(time, "%I:%M %p"),
        lat = round(latitude, 4),  
        lon = round(longitude, 4)  
      ) %>%
      select(magnitude, date, time_only, place, depth, lat, lon) %>%
      gt() %>%
      cols_label(
        date = "Date",
        time_only = "Time",
        place = "Location",
        magnitude = "Magnitude",
        depth = "Depth",
        lat = "Latitude",
        lon = "Longitude"
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
        fn = mag_pal  # Apply color palette to magnitudes
      ) %>%
      tab_header(
        title = md("**Recent Earthquakes**")
      ) %>%
      tab_source_note(
        source_note = md(paste("Data from USGS API at", format_datetime(now_et)))
      )
  })
  
  # Download handler for earthquake data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("ethiopia_earthquakes_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv", sep = "")
    },
    content = function(file) {
      # Prepare data for download with formatted columns
      data_to_download <- filtered_data() %>%
        mutate(
          date = format(time, "%d/%m/%Y"),
          time_str = format(time, "%H:%M:%S")
        ) %>%
        select(date, time_str, magnitude, depth, place, latitude, longitude)
      
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  # Output: Interactive map of earthquakes
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
          "<strong>Latitude:</strong>", round(latitude, 4), "<br/>",
          "<strong>Longitude:</strong>", round(longitude, 4), "<br/>",
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

# Run the Shiny app
shinyApp(ui, server)