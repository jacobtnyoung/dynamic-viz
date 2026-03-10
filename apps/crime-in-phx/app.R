# -------------------------------------------- #
# Crime in Phoenix App
# -------------------------------------------- #

# ----
# This app shows the spatial distribution of 
# crime in Phoenix using the open data portal


# ----
# load libraries
library( shiny )          # for shiny functions
library( dplyr )          # used for wrangling the data
library( tidyr )          # used for wrangling the data
library( ggplot2 )        # for plotting
library( here )           # for referencing the local directory
library( sf )             # spatial objects
library( tigris )         # to get geographic boundaries
library( leaflet )        # for producing the leaflet plot
library( leaflet.extras ) # for the heatmap



# ----
# define the objects
crimeData       <- readRDS( here( "apps/crime-in-phx/crime_dat.rds" ) )



# ----
# set up the UI

ui <- fluidPage(
  titlePanel("Crime in Phoenix"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "mapType",
        label = h4("Map Display Type:"),
        choices = c("Heatmap", "Points"),
        selected = "Points",
        inline = TRUE
      ),
      
      checkboxGroupInput(
        inputId = "checkCrimeType",
        label = h4("Crime Type"),
        choices = list(
          "Drugs" = "Drugs",
          "Rape" = "Rape",
          "Assault" = "Assault",
          "Theft" = "Theft",
          "MV Theft" = "MV Theft",
          "Burglary" = "Burglary",
          "Homicide" = "Homicide",
          "Robbery" = "Robbery",
          "Arson" = "Arson"
        ),
        selected = c(
          "Assault", "Homicide", "Robbery"
        )
      ),
      
      sliderInput(
        inputId = "yearRange",
        label = h4("Select Year Range:"),
        min = min(as.numeric(crimeData$year), na.rm = TRUE),
        max = max(as.numeric(crimeData$year), na.rm = TRUE),
        value = c(min(as.numeric(crimeData$year), na.rm = TRUE),
                  max(as.numeric(crimeData$year), na.rm = TRUE)),
        sep = "",
        animate = TRUE
      ),
      
      sliderInput(
        inputId = "hourRange",
        label = h4("Select Time of Day Range:"),
        min = min(crimeData$hour, na.rm = TRUE),
        max = max(crimeData$hour, na.rm = TRUE),
        value = c(min(crimeData$hour, na.rm = TRUE),
                  max(crimeData$hour, na.rm = TRUE)),
        sep = "",
        animate = TRUE
      )
    ),
    
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)
  


# ----
# set up the server

server <- function(input, output, session) {
  
  # reactively filter data
  filteredData <- reactive({
    req(input$checkCrimeType)     # need at least one crime type selected
    dat <- crimeData %>%
      filter(
        crime_type %in% input$checkCrimeType,
        year >= input$yearRange[1],
        year <= input$yearRange[2],
        hour >= input$hourRange[1],
        hour <= input$hourRange[2],
        !is.na(Longitude), !is.na(Latitude),
        is.finite(Longitude), is.finite(Latitude)
      )
    dat
  })
  
  # render base map once
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -112.0740, lat = 33.4484, zoom = 12)
  })
  
  # observe filtered data and update map
  observe({
    dat <- filteredData()
    
    # show friendly message when no points
    validate(
      need(nrow(dat) > 0, "No crime points match the selected filters.")
    )
    
    # sample if too big
    if (input$mapType == "Points" && nrow(dat) > 10000) {
      dat_plot <- dat[sample(nrow(dat), 10000), ]
    } else {
      dat_plot <- dat
    }
    
    # Clear existing layers then add new
    proxy <- leafletProxy("map", data = dat_plot) %>%
      clearShapes() %>%
      clearHeatmap() %>%
      clearControls()
    
    if (input$mapType == "Points") {
      pal <- colorFactor(palette = "Set1", domain = dat_plot$crime_type)
      
      proxy %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          color = ~pal(crime_type),
          fillOpacity = 0.7,
          stroke = FALSE,
          radius = 30,
          label = ~paste0("Type: ", crime_type, ", Hour: ", hour),
          labelOptions = labelOptions(
            direction = "auto",
            style = list("font-size" = "12px"),
            textsize = "12px",
            sticky = FALSE
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = ~crime_type, title = "Crime Type")
    } else {
      
      # heatmap
      proxy %>%
        addHeatmap(
          lng = ~Longitude, lat = ~Latitude,
          blur = 20, radius = 15, max = 0.05
        )
    }
  })
  
}


# ----
# render
shinyApp( ui, server )