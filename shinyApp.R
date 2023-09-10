library(shiny)
library(leaflet)
library(leafpop)
library(sf)
library(ggplot2)
library(plotly)

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
      
      # OR NUMERIC FOR VEHICLE ID
      # numericInput("numeric_input", "Enter a Number:", 0),
      
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
  # TODO Read the CSV file
  #Komponente_K1BE2 <- read_csv("University/SS23/r_project/Komponente_K1BE2.csv")
  
  ##################### Check vehicle logic ##############################
  # Initial value for text_output
  output$vehicle_status_output <- renderText({
    "no vehicle ID entered"
  })
  
  # Function to check the vehicle and update text_output
  checkVehicle <- function() {
    vehicle_id <- input$text_input
    
    if (is.null(vehicle_id) || vehicle_id == "") {
      return("no vehicle ID entered")
    } else {
      # TODO: Add logic to check the vehicle here
      if (vehicle_id == "ValidID") {
        return("Vehicle is OK")
      } else {
        return("Vehicle is damaged")
      }
    }
  }
  
  # When "Check Vehicle" button is pressed, update text_output
  observeEvent(input$check_vehicle, {
    updated_status <- checkVehicle()
    output$vehicle_status_output <- renderText(updated_status)
  })
  
  ###################### Download datasets logic #########################
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
  
  ###################### heatmap map logic ###############################
  # Load the shapefile for German municipalities
  germany_shapefile <- st_read("www/germany_municipalities.shp")
  
  # add an unique Id to each entry
  germany_shapefile$layerId <- paste0(germany_shapefile$GEN, "_",
                                      1:nrow(germany_shapefile))
  
  # Reproject shapeile to WGS84 format
  germany_shapefile <- st_transform(germany_shapefile,
                                    crs = st_crs("+proj=longlat +datum=WGS84"))
  # TODO: CHANGE
  set.seed(123)
  
  # TODO: CHANGE Generate random numbers between 0 and 10 for damaged vehicles
  germany_shapefile$damaged_vehicles <- sample(0:10, nrow(germany_shapefile),
                                               replace = TRUE)
  
  # TODO: CHANGE Generate random numbers between 10 and 20 for registered vehicles
  germany_shapefile$registered_vehicles <- sample(10:20,
                                                  nrow(germany_shapefile),
                                                  replace = TRUE)
  
  # Define color palette for the heatmap
  color_palette <- colorNumeric(palette = "YlOrRd",
                                domain = as.numeric(germany_shapefile$damaged_vehicles))
  
  # Function to generate a popup based on the area clicked by the user
  makePopupPlot <- function(clickedArea) {
    # Find the clicked municipality
    clickedMunicipality <- germany_shapefile[
      germany_shapefile$GEN == clickedArea, ]
    
    # Check if the municipality has data
    if (nrow(clickedMunicipality) > 0) {
      # TODO: CHANGE Calculate the percentage of damaged vehicles
      total_vehicles <- sum(clickedMunicipality$registered_vehicles)
      damaged_percent <- sum(clickedMunicipality$damaged_vehicles) / total_vehicles * 100
      undamaged_percent <- 100 - damaged_percent
      
      # Create a pie chart using ggplot2
      pie_chart <- ggplot(data = data.frame(category = c("Damaged", "Undamaged"),
                                            percent = c(damaged_percent, undamaged_percent)),
                          aes(x = "", y = percent, fill = category)) +
        geom_bar(stat = "identity") +
        coord_polar(theta = "y") +
        labs(title = paste0("Vehicle Status in ", clickedArea),
             fill = "Vehicle Status") +
        theme_void()
      
      # this lets the pie_chart be used uin the map
      popup_content <- popupGraph(pie_chart)
      
      return(popup_content)
    } else {
      # Return an empty character vector if there's no data for the clicked area
      return(character(0))
    }
  }
  
  # render the output heatmap
  output$heatmap_output <- renderLeaflet({ leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = germany_shapefile,
      fillColor = ~color_palette(as.numeric(damaged_vehicles)),
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
      values = as.numeric(germany_shapefile$damaged_vehicles),
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