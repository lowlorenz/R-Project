library(shiny)
library(leaflet)
library(leafpop)
library(sf)
library(ggplot2)
library(plotly)
library(data.table)
library(dplyr)

# TODO: package install??!


# define the Shiny-App UI
ui <- fluidPage(
  # Add custom CSS for light-blue background and font
  tags$style(HTML("
    body {
      background-color: lightblue;
      font-family: 'Source Sans Pro', sans-serif;
      height: 100vh;
      width: 100vw;
    }
    .logo-container {
      position: absolute;
      bottom: 10px;
      left: 10px;
    }
  ")),
  
  # App title
  titlePanel("Vehicle Damage Report"),
  
  # Add logo image
  div(class = "logo-container",
      img(src = "logo.png", width = 100, height = 100)
  ),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # text-input for the vehicle ID
      textInput("text_input", "Enter Vehicle ID:", ""),
      
      # Action button to check the vehicle
      actionButton("check_vehicle",
                   "Check Vehicle"),
      
      # only for visual reasons two blank lines
      br(),
      br(),
      
      # text-output to see if the entered vehicle is affected, also
      # provide the approximate prediction for the duration of the repair
      HTML("<b>Vehicle Status:</b>"),
      textOutput("vehicle_status_output"),
      
      # only for visual reasons two blank lines
      br(),
      br(),
      
      # Select the vehicle type
      selectInput("vehicle_type", "Select an affected vehicle type option
                  to download:",
                  choices = c("OEM1_Typ11", "OEM1_Typ12",
                              "OEM2_Typ21", "OEM2_Typ22")),
      
      # Action button to download the dataset
      downloadButton("download_dataset",
                     "Download dataset")
    ),
    
    mainPanel(
      # Add output elements or plots here
      h4("Vehicle Damage Hotspots:"),
      
      # Display the damage heatmap of Germany
      leafletOutput("heatmap_output")
    )
  )
)

# define the Shiny-App logic
server <- function(input, output) {
  # Load your damaged vehicles dataset
  final_dataset <- fread("Final_dataset_group_17.csv", sep = ";")
  
  ########################################################################
  ##################### Check vehicle logic ##############################
  ########################################################################
  
  # Initial value for text_output
  output$vehicle_status_output <- renderText({
    "no vehicle ID entered"
  })
  
  # Function to check the vehicle and update text_output
  checkVehicle <- function() {
    vehicle_id <- input$text_input
    
    # check if entry is null
    if (is.null(vehicle_id) || vehicle_id == "") {
      return("no vehicle ID entered")
    } else {
      # check if the format is correct
      if (!grepl("^\\d+-\\d+-\\d+-\\d+$", vehicle_id)) {
        return("format wrong")
      } else {
        # Check if the entered vehicle ID exists in the final dataset and is marked as damaged
        if (vehicle_id %in% final_dataset$ID_Fahrzeug) {
          # TODO: NAME NEAREST REPAIR SERVICE AND WAITING TIME
          return("Vehicle is damaged")
        } else {
          return("Vehicle is NOT damaged")
        }
      }
    }
  }
  
  # When "Check Vehicle" button is pressed, update text_output
  observeEvent(input$check_vehicle, {
    updated_status <- checkVehicle()
    output$vehicle_status_output <- renderText(updated_status)
  })
  
  ########################################################################
  ###################### Download datasets logic #########################
  ########################################################################
  
  # Function to generate and serve the dataset for download as CSV
  output$download_dataset <- downloadHandler(
    filename = function() {
      # Define the filename for the downloaded CSV dataset
      paste("update_action_", input$vehicle_type, ".csv", sep = "")
    },
    content = function(file) {
      # TODO: logic
      # Generate and write your dataset to the file in CSV format
      # Replace the following example code with your dataset generation logic
      dataset <- data.frame(
        ID = 1:10,
        Value = rnorm(10)
      )
      write.csv(dataset, file, row.names = FALSE)
    }
  )
  
  ########################################################################
  ###################### heatmap map logic ###############################
  ########################################################################
  
  # Load the shapefile for German municipalities
  germany_shapefile <- st_read("www/germany_municipalities.shp")
  
  # add an unique Id to each entry
  germany_shapefile$layerId <- paste0(germany_shapefile$GEN, "_",
                                      1:nrow(germany_shapefile))
  
  # Reproject shapeile to WGS84 format
  germany_shapefile <- st_transform(germany_shapefile,
                                    crs = st_crs("+proj=longlat +datum=WGS84"))
  
  # Create a new dataset with unique "Gemeinde" values and their counts
  damaged_vehicles <- final_dataset %>%
    group_by(Gemeinde, Longitude, Latitude) %>%
    summarize(Anzahl_beschaedigt = n()) %>%
    ungroup()

  # Convert the damaged vehicles data frame to an sf spatial object
  damaged_vehicles_sf <- st_as_sf(damaged_vehicles,
                                  coords = c("Longitude", "Latitude"),
                                  crs = st_crs(germany_shapefile))

  # Perform a spatial join to associate each damaged vehicle with a municipality
  germany_shapefile <- st_join(germany_shapefile, damaged_vehicles_sf)
  
  # Replace NA values with 0 in the Anzahl_beschaedigt column
  germany_shapefile$Anzahl_beschaedigt <- ifelse(is.na(germany_shapefile$Anzahl_beschaedigt),
                                                 0, 
                                                 germany_shapefile$Anzahl_beschaedigt)
  
  # repeat process for all registered vehicles
  registered_vehicles <- fread("Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv", sep = ";")
  
  # Read the input geo CSV file
  df_geo_data <- fread("Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv",
                       header = FALSE, sep = ";", skip = 1)
  
  # Replace commas with periods in the 5th and 6th columns
  df_geo_data[, V5 := gsub(",", ".", V5)]
  df_geo_data[, V6 := gsub(",", ".", V6)]
  
  # Convert the 5th and 6th columns to numeric
  df_geo_data[, c("V5", "V6") := lapply(.SD, as.numeric), .SDcols = c("V5", "V6")]
  
  # Function to format the 5th and 6th columns as floating-point numbers.
  # Sometimes the floatingpoint is missing, eg. 56778 instead of 56.778
  format_as_float <- function(x) {
    if (is.numeric(x)) {
      return(format(x, nsmall = 6))
    } else {
      return(x)
    }
  }
  
  # Apply the format_as_float function to the 5th and 6th columns
  df_geo_data$V5 <- sapply(df_geo_data$V5, format_as_float)
  df_geo_data$V6 <- sapply(df_geo_data$V6, format_as_float)
  
  # Drop the first three columns from 'df_geo_data', we don't need them
  df_geo_data <- df_geo_data[, 4:6, with = FALSE]
  
  # name the columns
  colnames(df_geo_data) <- c("Gemeinden", "Longitude", "Latitude")
  
  # We found out that one Gemeinden is missing in Geo_data that is
  # needed for the damaged vehicles: Gemeinden SEEG. Add SEEG here
  new_data <- data.frame(Gemeinden = "SEEG",
                         Longitude = 10.6085, Latitude = 47.6543)
  df_geo_data <- rbind(df_geo_data, new_data)
  
  # Merge the datasets using the 4th column as the key
  registered_vehicles <- merge(registered_vehicles, df_geo_data, by.x = "Gemeinden",
                          by.y = "Gemeinden", all.x = TRUE)
  
  # Create a new dataset with unique "Gemeinden" values and their counts
  registered_vehicles <- registered_vehicles %>%
    group_by(Gemeinden, Longitude, Latitude) %>%
    summarize(Anzahl_registriert = n()) %>%
    ungroup()
  
  # Convert the damaged vehicles data frame to an sf spatial object
  registered_vehicles_sf <- st_as_sf(registered_vehicles,
                                  coords = c("Longitude", "Latitude"),
                                  crs = st_crs(germany_shapefile))
  
  # Perform a spatial join to associate each damaged vehicle with a municipality
  germany_shapefile <- st_join(germany_shapefile, registered_vehicles_sf)
  
  # Replace NA values with 0 in the registered_vehicles_sf column
  germany_shapefile$Anzahl_registriert <- ifelse(is.na(germany_shapefile$Anzahl_registriert),
                                                 0, 
                                                 germany_shapefile$Anzahl_registriert)
  
  # Define color palette for the heatmap
  color_palette <- colorNumeric(palette = "YlOrRd",
                                domain = as.numeric(germany_shapefile$Anzahl_beschaedigt))
  
  # Function to generate a popup based on the area clicked by the user
  makePopupPlot <- function(clickedArea) {
    # Find the clicked municipality
    clickedMunicipality <- germany_shapefile[
      germany_shapefile$GEN == clickedArea, ]
    
    # Check if the municipality has data
    if (nrow(clickedMunicipality) > 0) {
      # Calculate the percentage of damaged vehicles
      total_vehicles <- sum(clickedMunicipality$Anzahl_registriert)
      damaged_percent <- sum(clickedMunicipality$Anzahl_beschaedigt) / total_vehicles * 100
      undamaged_percent <- 100 - damaged_percent
      
      # Calculate the number of damaged and total vehicles
      num_damaged <- sum(clickedMunicipality$Anzahl_beschaedigt)
      num_total <- total_vehicles
      
      # Create a pie chart using ggplot2
      pie_chart <- ggplot(data = data.frame(category = c("Damaged", "Undamaged"),
                                            percent = c(damaged_percent, undamaged_percent)),
                          aes(x = "", y = percent, fill = category)) +
        geom_bar(stat = "identity") +
        coord_polar(theta = "y") +
        labs(title = paste0("Vehicle Status in ", clickedArea),
             subtitle = paste("Damaged: ", num_damaged, ", Total: ", num_total),
             fill = "Vehicle Status") +
        theme_void()
      
      # this lets the pie_chart be used uin the map
      popup_content <- popupGraph(pie_chart)
      
      print("Clicked Municipality:")
      print(clickedMunicipality$GEN)
      print(clickedMunicipality$Anzahl_beschaedigt)
      print(clickedMunicipality$Anzahl_registriert)
      return(popup_content)
    } else {
      # Return an empty character vector if there's no data for the clicked area
      print("couldnt find data for the clicked spot")
      return(character(0))
    }
  }
  
  # render the output heatmap
  output$heatmap_output <- renderLeaflet({ leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = germany_shapefile,
      fillColor = ~color_palette(as.numeric(Anzahl_beschaedigt)),
      fillOpacity = 0.7,
      color = "white",
      weight = 1,
      label = ~GEN,
      layerId = ~layerId
    ) %>%
      # add a legend for the heatmap in the bottom corner
    addLegend(
      "bottomright",
      pal = color_palette,
      values = as.numeric(germany_shapefile$Anzahl_beschaedigt),
      title = "Damaged Vehicles",
      opacity = 1
    ) %>%
    # Set initial map view to Germany's center
    setView(lng = 10.423446, lat = 51.083419, zoom = 5)
  })
  
  # Add dynamic popups at the position the user clicks
  observeEvent(input$heatmap_output_shape_click, {
    event <- input$heatmap_output_shape_click
    
    if (!is.null(event)) {
      # Remove "_X" from event$id (the unique layer ID) to get the name
      municipality_name <- sub("_\\d+$", "", event$id)
      popupContent <- makePopupPlot(municipality_name)
      
      leafletProxy("heatmap_output") %>%
        addPopups(
          lat = event$lat,
          lng = event$lng,
          popup = popupContent
        )
    }
  })
}

# run the Shiny-App
shinyApp(ui = ui, server = server)