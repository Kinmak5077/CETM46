#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(DT)
library(bsicons) # Ensure bsicons is loaded

# --- 1. CONFIGURATION & DATA ---

# A. User Locations
user_locations <- list(
  "Central (Central District)" = c(lat = 22.282, lon = 114.158),
  "Causeway Bay (Shopping Area)" = c(lat = 22.280, lon = 114.185),
  "Tsim Sha Tsui (Harbour City)" = c(lat = 22.295, lon = 114.172),
  "Mong Kok (Langham Place)" = c(lat = 22.319, lon = 114.169),
  "Kwun Tong (APM)" = c(lat = 22.312, lon = 114.225),
  "Shatin (New Town Plaza)" = c(lat = 22.381, lon = 114.188),
  "Tseung Kwan O (PopCorn)" = c(lat = 22.308, lon = 114.260),
  "Tuen Mun (Town Plaza)" = c(lat = 22.392, lon = 113.976),
  "Yuen Long (Yoho Mall)" = c(lat = 22.445, lon = 114.035),
  "Tai Po (Mega Mall)" = c(lat = 22.451, lon = 114.171)
)

# B. Running Spots Database
running_spots <- data.frame(
  name = c("Victoria Park Jogging Trail", "Bowen Road Fitness Trail", "Central & Western Promenade",
           "Tsim Sha Tsui Promenade", "West Kowloon Art Park", "Shing Mun River (Shatin)",
           "Tseung Kwan O Waterfront", "Tai Po Waterfront Park", "Cyberport Waterfront Park",
           "Tuen Mun Park", "Kwun Tong Promenade"),
  lat = c(22.2825, 22.2720, 22.2885, 22.2934, 22.3013, 22.3837, 22.3050, 22.4430, 22.2610, 22.3920, 22.3100),
  lon = c(114.1889, 114.1670, 114.1550, 114.1717, 114.1537, 114.1925, 114.2600, 114.1770, 114.1290, 113.9720, 114.2200),
  station = c("Causeway Bay", "Central/Western", "Central/Western", "Tsim Sha Tsui", "Sham Shui Po",
              "Sha Tin", "Tseung Kwan O", "Tai Po", "Southern", "Tuen Mun", "Kwun Tong"),
  weather_station = c("Happy Valley", "Hong Kong Park", "Hong Kong Park", 
                      "Hong Kong Observatory", "Kowloon City", "Sha Tin", 
                      "Tseung Kwan O", "Tai Po", "Wong Chuk Hang", 
                      "Tuen Mun", "Kwun Tong"),
  stringsAsFactors = FALSE
)

# C. Distance Calculation
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat/2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  return(R * c)
}

# D. Advice Logic
get_advice <- function(aqhi) {
  val <- as.numeric(aqhi)
  if (is.na(val) || val == 0) {
    return(list(risk="No Data", color="gray", advice="Temporarily Unavailable"))
  }
  if (val <= 3) {
    list(risk="LOW", color="green", advice="Great for running")
  } else if (val <= 6) {
    list(risk="MODERATE", color="orange", advice="Acceptable")
  } else if (val <= 7) {
    list(risk="HIGH", color="red", advice="Avoid outdoor running")
  } else {
    list(risk="VERY HIGH", color="black", advice="Avoid outdoor running")
  }
}

# --- 2. UI ---
ui <- page_sidebar(
  title = "HK RunSafe: Smart Locator",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  fillable = FALSE, 
  
  sidebar = sidebar(
    title = "Your Location",
    selectInput("user_loc", "Where are you now?", choices = names(user_locations)),
    hr(),
    helpText("Select your current location to find the nearest running spots.")
  ),
  
  # Main Content Area
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Nearest Spot",
      value = textOutput("best_spot_name"),
      showcase = bsicons::bs_icon("geo-alt-fill"),
      theme = "primary" # Dark Blue
    ),
    value_box(
      title = "Distance",
      value = textOutput("best_spot_dist"),
      showcase = bsicons::bs_icon("signpost-split"),
      theme = "secondary" 
    ),
    uiOutput("aqhi_box"), 
    uiOutput("weather_box") 
  ),
  
  accordion(
    open = FALSE,
    accordion_panel(
      "Click here to view AQHI Grading Scale",
      HTML("
        <table class='table table-sm'>
          <thead><tr><th>AQHI</th><th>Risk</th><th>Advice</th></tr></thead>
          <tbody>
            <tr style='color:green'><td>1-3</td><td>Low</td><td>Suitable for running</td></tr>
            <tr style='color:orange'><td>4-6</td><td>Moderate</td><td>Acceptable</td></tr>
            <tr style='color:red'><td>7</td><td>High</td><td>Not suitable for running</td></tr>
            <tr style='color:purple'><td>8-10</td><td>Very High</td><td>Not suitable for running</td></tr>
            <tr style='color:black; font-weight:bold'><td>10+</td><td>Serious</td><td>Not suitable for running</td></tr>
            <tr style='color:gray'><td>0 / N/A</td><td>No Data</td><td>Temporarily Unavailable</td></tr>
          </tbody>
        </table>
      ")
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header("Map View (AQHI & Weather)"),
    leafletOutput("map", height = "600px")
  ),
  
  card(
    card_header("Nearby Running Spots"),
    DTOutput("spots_table")
  )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
  
  # Reactive: Fetch AQHI API
  api_data_aqhi <- reactive({
    url <- "https://dashboard.data.gov.hk/api/aqhi-individual?format=json"
    tryCatch({
      res <- GET(url, add_headers(`User-Agent` = "Mozilla/5.0", `Referer` = "https://www.aqhi.gov.hk/"))
      if (status_code(res) == 200) {
        data <- fromJSON(content(res, "text", encoding = "UTF-8"))
        data$aqhi <- as.numeric(gsub("[\\*\\+]", "", data$aqhi))
        return(data)
      } else { return(NULL) }
    }, error = function(e) { return(NULL) })
  })
  
  # Reactive: Fetch Weather API
  api_data_weather <- reactive({
    url <- "https://data.weather.gov.hk/weatherAPI/opendata/weather.php?dataType=rhrread&lang=en"
    tryCatch({
      res <- GET(url)
      if (status_code(res) == 200) {
        data <- fromJSON(content(res, "text", encoding = "UTF-8"))
        temps <- data$temperature$data
        humidity <- data$humidity$data$value[1]
        return(list(temps = temps, humidity = humidity))
      } else { return(NULL) }
    }, error = function(e) { return(NULL) })
  })
  
  # Reactive: Process Data
  processed_data <- reactive({
    req(input$user_loc)
    aqhi_df <- api_data_aqhi()
    weather_data <- api_data_weather()
    
    u_loc <- user_locations[[input$user_loc]]
    df <- running_spots
    
    res_list <- lapply(1:nrow(df), function(i) {
      spot <- df[i, ]
      dist <- calculate_distance(u_loc['lat'], u_loc['lon'], spot$lat, spot$lon)
      
      # 1. Match AQHI
      current_aqhi <- 0
      if (!is.null(aqhi_df)) {
        match_idx <- grep(spot$station, aqhi_df$station, ignore.case = TRUE)
        if (length(match_idx) > 0) current_aqhi <- aqhi_df$aqhi[match_idx[1]]
      }
      advice_info <- get_advice(current_aqhi)
      
      # 2. Match Temperature
      current_temp <- "N/A"
      if (!is.null(weather_data)) {
        t_idx <- grep(spot$weather_station, weather_data$temps$place, ignore.case = TRUE)
        if (length(t_idx) > 0) {
          current_temp <- weather_data$temps$value[t_idx[1]]
        } else {
          hk_idx <- grep("Hong Kong Observatory", weather_data$temps$place, ignore.case = TRUE)
          if(length(hk_idx) > 0) current_temp <- weather_data$temps$value[hk_idx[1]]
        }
      }
      
      data.frame(
        Name = spot$name,
        Distance = round(dist, 2),
        AQHI = current_aqhi,
        Risk = advice_info$risk,
        Color = advice_info$color,
        Advice = advice_info$advice,
        Temp = current_temp,
        Lat = spot$lat,
        Lon = spot$lon,
        stringsAsFactors = FALSE
      )
    })
    
    final_df <- do.call(rbind, res_list)
    final_df <- final_df[order(final_df$Distance), ] 
    return(final_df)
  })
  
  # --- OUTPUTS ---
  
  output$best_spot_name <- renderText({
    req(processed_data())
    processed_data()$Name[1]
  })
  
  output$best_spot_dist <- renderText({
    req(processed_data())
    paste(processed_data()$Distance[1], "km")
  })
  
  output$aqhi_box <- renderUI({
    req(processed_data())
    best <- processed_data()[1, ]
    
    theme_color <- switch(best$Color,
                          "green" = "success", "orange" = "warning",
                          "red" = "danger", "purple" = "danger",
                          "black" = "dark", "gray" = "secondary", "secondary")
    
    display_aqhi <- ifelse(best$AQHI == 0, "No Data", best$AQHI)
    
    value_box(
      title = paste("AQHI:", display_aqhi),
      value = best$Advice,
      showcase = bsicons::bs_icon("heart-pulse"),
      theme = theme_color
    )
  })
  
  # NEW: Weather Box (Fixed Icon & Color)
  output$weather_box <- renderUI({
    req(processed_data())
    weather_info <- api_data_weather()
    best_spot_temp <- processed_data()$Temp[1]
    
    humidity_text <- ifelse(!is.null(weather_info$humidity), paste0("Humidity: ", weather_info$humidity, "%"), "")
    
    value_box(
      title = "Weather",
      value = paste0(best_spot_temp, "°C"),
      p(humidity_text),
      # FIXED: Using a static Bootstrap Icon 'cloud-sun-fill' for reliability
      showcase = bsicons::bs_icon("cloud-sun-fill"),
      # FIXED: Changed theme to 'info' (Light Blue/Cyan)
      theme = "info"
    )
  })
  
  output$map <- renderLeaflet({
    req(processed_data())
    df <- processed_data()
    u_loc <- user_locations[[input$user_loc]]
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = u_loc['lon'], lat = u_loc['lat'], zoom = 12)
    
    m <- addAwesomeMarkers(
      m, lng = u_loc['lon'], lat = u_loc['lat'],
      icon = awesomeIcons(icon = "user", library = "fa", markerColor = "blue"),
      popup = "<b>You are here</b>"
    )
    
    getColor <- function(aqhi) {
      if(aqhi == 0) "gray"
      else if(aqhi <= 3) "green"
      else if(aqhi <= 6) "orange"
      else if(aqhi <= 7) "red"
      else "black"
    }
    
    icons <- awesomeIcons(icon = "star", library = "fa", markerColor = sapply(df$AQHI, getColor))
    
    m <- addAwesomeMarkers(
      m, data = df, lng = ~Lon, lat = ~Lat, icon = icons,
      popup = ~paste0("<b>", Name, "</b><br>",
                      "Distance: ", Distance, " km<br>",
                      "<b>Temp: ", Temp, "°C</b><br>",
                      "AQHI: ", ifelse(AQHI == 0, "No Data", AQHI), " (", Risk, ")<br>",
                      "Advice: ", Advice)
    )
    return(m)
  })
  
  output$spots_table <- renderDT({
    req(processed_data())
    df_show <- processed_data() %>% 
      select(Name, Distance, Temp, AQHI, Advice) %>%
      mutate(AQHI = ifelse(AQHI == 0, "No Data", as.character(AQHI))) %>% 
      rename("Running Spot" = Name, "Dist (km)" = Distance, "Temp (°C)" = Temp)
    
    datatable(df_show, options = list(pageLength = 5), rownames = FALSE) %>%
      formatStyle('AQHI',
                  backgroundColor = styleEqual(
                    c("No Data", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+"), 
                    c('#e9ecef', '#d4edda', '#d4edda', '#d4edda', '#fff3cd', '#fff3cd', '#fff3cd', '#f8d7da', '#f8d7da', '#f8d7da', '#343a40', '#343a40')
                  ),
                  color = styleEqual(c("No Data"), c("#6c757d"))
      )
  })
}

# --- 4. RUN ---
shinyApp(ui, server)