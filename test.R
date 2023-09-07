library(shiny)
library(heatmaply)

# TODO: package install??!

# TODO: remove this dummy data
heatmap_data <- matrix(
  data = rnorm(100), 
  nrow = 10, 
  ncol = 10
)

# define the Shiny-App UI
ui <- fluidPage(
  # Add custom CSS for light-blue background and font
  tags$style(HTML("
    body {
      background-color: lightblue;
      font-family: 'Source Sans Pro', sans-serif;
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
      
      # Display the heatmap
      # TODO: correct Heatmap, Popups?
      plotlyOutput("heatmap_output"), 
    )
  )
)

# define the Shiny-App logic
server <- function(input, output) {
  # Read the CSV file
  #Komponente_K1BE2 <- read_csv("University/SS23/r_project/Komponente_K1BE2.csv")
  
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
  
  # TODO: Remove dummyRender the heatmap
  output$heatmap_output <- renderPlotly({
    heatmaply(
      heatmap_data,
      xlab = "X-Axis Label",
      ylab = "Y-Axis Label",
      main = "Heatmap Example"
    )
  })
}

# run the Shiny-App
shinyApp(ui = ui, server = server)
