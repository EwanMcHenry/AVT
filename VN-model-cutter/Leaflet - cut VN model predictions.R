
library(shiny)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

library(sf)
source("D:/Users/Ewan McHenry/OneDrive - the Woodland Trust/GIS/Ewans functions.R")
source("D:/Users/Ewan McHenry/OneDrive - the Woodland Trust/GIS/Ewans gis specifications.R")



# AVT model sf object
sf_data <- st_read("from VN/ZIResultsNegBin.shp")
# area of interest
interest.area = st_read(paste0(gis.wd, "Data\\Designations\\NE_NationalParksEngland_SHP_Full\\data\\National_Parks_England.shp")) %>% 
  filter(name == "DARTMOOR" ) %>% 
  st_first.spatial.curation()



# Render the initial map
rr <- tags$div(
  HTML(paste0('<strong>Cut AVT predictions based on Nolan et al (2021) model </strong><br>Dr. Ewan McHenry, ', format(Sys.time(), '%d %B, %Y')))
)  

cut_scape = st_intersection(sf_data, interest.area)

ceiling_value<- quantile (cut_scape$NEGBINC, probs = 0.98)
est.scaled <- pmin(cut_scape$NEGBINC, ceiling_value)
n.est.pal <- colorNumeric(
  palette = "viridis",
  domain =  c(0,est.scaled))


cut.leaflet <- leaflet() %>%
  # addTiles() %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(data = cut_scape %>% st_transform(4326) , stroke = F, color = "grey" ,
              fillColor =  ~n.est.pal(est.scaled)  , 
              weight = 0.5, smoothFactor = 0.5,
              opacity = 0.3, fillOpacity = 0.8, 
              popup = paste0(
                "<strong>Model estimated AVTs (N): </strong>", signif(cut_scape$NEGBINC,3),"<br>",
                "<strong>AVTs recorded (N): </strong>", cut_scape$Count_,"<br>"
              )) %>% 
  addLegend(position = "bottomright", 
            pal = n.est.pal,
            values = c(0,est.scaled) ,
            title = paste("Estimated<br> AVTs (N)"),
            opacity = 1, 
            group = paste0("Model-estimated")) %>%
  addSearchOSM( options = searchOptions(position = "topleft", autoCollapse = TRUE, minLength = 2)) %>%
  addControl(rr, position = "bottomleft") 
cut.leaflet

saveWidget(cut.leaflet, file="Dartmoor AVT leaflet.html")
