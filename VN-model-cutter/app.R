library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
source("D:/Users/Ewan McHenry/OneDrive - the Woodland Trust/GIS/Ewans functions.R")
source("D:/Users/Ewan McHenry/OneDrive - the Woodland Trust/GIS/Ewans gis specifications.R")


# Define the UI
ui <- fluidPage(
  titlePanel("Cut AVT Predictions"),
  
  fileInput("area_file", "Upload Area of Interest Shapefile (.shp)"),
  leafletOutput("map"),
  sliderInput("ceiling_slider", "Ceiling Value:", min = 0, max = 100, value = 98)
)

# Define the server
server <- function(input, output) {
  options(shiny.maxRequestSize = 100*1024^2)  # 100 MB limit
  
  
  observe({
    # Load area of interest from uploaded Shapefile
    if (!is.null(input$area_file)) {
      area_file <- input$area_file$datapath
      interest.area <- st_read(area_file)
    } else {
      # Use drawn polygon on the map as area of interest
      click_coords <- input$map_shape_click
      if (!is.null(click_coords)) {
        interest.area <- st_polygon(list(rbind(click_coords, click_coords)))
      }
    }
    
    # Calculate cut_scape
    cut_scape <- st_intersection(sf_data, interest.area)
    
    # Calculate ceiling_value from slider
    ceiling_value <- quantile(cut_scape$NEGBINC, probs = input$ceiling_slider / 100)
    
    # Create the leaflet map
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        addPolygons(
          data = cut_scape %>% st_transform(4326),
          stroke = FALSE,
          color = "grey",
          fillColor = ~n.est.pal(pmin(cut_scape$NEGBINC, ceiling_value)),
          weight = 0.5,
          smoothFactor = 0.5,
          opacity = 0.3,
          fillOpacity = 0.8,
          popup = paste0(
            "<strong>Model estimated AVTs (N): </strong>", signif(cut_scape$NEGBINC, 3), "<br>",
            "<strong>AVTs recorded (N): </strong>", cut_scape$Count_, "<br>"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = n.est.pal,
          values = c(0, ceiling_value),
          title = paste("Estimated<br> AVTs (N)"),
          opacity = 1,
          group = paste0("Model-estimated")
        ) %>%
        addSearchOSM(options = searchOptions(position = "topleft", autoCollapse = TRUE, minLength = 2))
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)