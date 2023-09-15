# install needed packages
install.packages("shiny")
install.packages("leaflet")
install.packages("leafpop")
install.packages("sf")
install.packages("ggplot2")
install.packages("plotly")
install.packages("data.table")
install.packages("dplyr")

library(shiny)
library(leaflet)
library(leafpop)
library(sf)
library(ggplot2)
library(plotly)
library(data.table)
library(dplyr)


# Define the Shiny-App UI
ui <- fluidPage(
  # Custom CSS for light-blue background and font
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
      # Text-input for the vehicle ID
      textInput("text_input", "Enter Vehicle ID:", ""),
      
      # Action button to check the vehicle
      actionButton("check_vehicle",
                   "Check Vehicle"),
      
      # Only for visual reasons two blank lines
      br(),
      br(),
      
      # Text-output to see if the entered vehicle is affected, also
      # provide the approximate prediction for the duration of the repair
      HTML("<b>Vehicle Status:</b>"),
      textOutput("vehicle_status_output"),
      
      # Only for visual reasons two blank lines
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
      leafletOutput("heatmap_output", width = "1000px"),
      
      # Display the final dataset
      fluidRow(
        column(12,
               dataTableOutput('table')
        )
      )
    )
  )
)

# Define the Shiny-App logic
server <- function(input, output) {
  # Load your damaged vehicles dataset
  final_dataset <- fread("Final_dataset_group_17.csv", sep = ";")
  
  # Render the CSV file as a DataTable
  output$table <- renderDataTable(final_dataset,
                                  options = list(
                                    pageLength = 5
                                  )
  )
  
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
    
    # Check if entry is null
    if (is.null(vehicle_id) || vehicle_id == "") {
      return("no vehicle ID entered")
    } else {
      # Check if the format is correct
      if (!grepl("^\\d+-\\d+-\\d+-\\d+$", vehicle_id)) {
        return("format wrong")
      } else {
        # Check if the entered vehicle ID exists in the final dataset and is marked as damaged
        if (vehicle_id %in% final_dataset$ID_Fahrzeug) {
          # Check if the vehicle is damaged at all
          if (final_dataset$Beschaedigt[final_dataset$ID_Fahrzeug == vehicle_id] == "ja") {
            # Find the waitingTime value for the specified vehicle_id
            waiting_time <- final_dataset$waitingTime[final_dataset$ID_Fahrzeug == vehicle_id]
            return(paste("Vehicle is damaged. Waiting Time in days:", waiting_time))
          }
          return("Vehicle is NOT damaged")
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
      vehicle_type <- input$vehicle_type
      
      # Get the last two characters of the string
      vehicleType <- substr(vehicle_type, nchar(vehicle_type) - 1, nchar(vehicle_type))
      # Filter rows where ID_Fahrzeug starts with the right type
      dataset <- final_dataset[grepl(paste0("^", vehicleType), final_dataset$ID_Fahrzeug), ]
      
      # Only keep the relevant information regarding the vehicle_ID and the update time
      dataset <- dataset %>%
        filter(Beschaedigt == "ja") %>%
        select(ID_Fahrzeug, waitingTime)
      
      write.csv(dataset, file, row.names = FALSE)
    }
  )
  
  ########################################################################
  ###################### heatmap map logic ###############################
  ########################################################################
  
  print("Start generating Map")
  # Load the shapefile for German municipalities
  germany_shapefile <- st_read("www/germany_municipalities.shp")
  
  # Add an unique Id to each entry
  germany_shapefile$layerId <- paste0(germany_shapefile$GEN, "_",
                                      1:nrow(germany_shapefile))
  
  # Reproject shapefile to WGS84 format
  germany_shapefile <- st_transform(germany_shapefile,
                                    crs = st_crs("+proj=longlat +datum=WGS84"))
  
  # Create a new dataset with unique "Gemeinde" values and their counts
  damaged_vehicles <- final_dataset %>%
    filter(Beschaedigt == "ja") %>%
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
  
  
  # Repeat process for functioning vehicles
  # Create a new dataset with unique "Gemeinde" values and their counts
  registered_vehicles <- final_dataset %>%
    filter(Beschaedigt == "nein") %>%
    group_by(Gemeinde, Longitude, Latitude) %>%
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
      
      # This lets the pie_chart be used in the map
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
  
  # Render the output heatmap
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
      # Add a legend for the heatmap in the bottom corner
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

# Run the Shiny-App
shinyApp(ui = ui, server = server)