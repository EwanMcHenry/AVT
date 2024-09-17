# pulling out ATI records near location


# library ----
library(sf) # for gis
library(raster)
library("rnrfa") # to convert os gb grif ref
library(rgdal) # to convert lat long to os gb
library(leaflet) # for interactive maps
library(leaflet.extras)
library(leafem)

library("rnaturalearth") # map of countries of the entire world
library("rnaturalearthdata")
library("rgeos")
library(tidyverse)

library(viridis)
# library(parlitools)
# library(extrafont)
# library(nngeo) # for st_nn()
# library(plgraphics) # for prettyscale()
# library(OpenStreetMap)
library(ggmap)
library(osmdata)
library(htmlwidgets)
library(U.utilities)

# load data ----
load(paste0(gis.wd, "\\Data\\ATI\\ati2024-08-28curated_fromATI - 2024_08_05.csv.RData"))

presence.pred = raster("S:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\SoWT\\ATI\\ATI heatmap layer.tif")
hab.suit = raster("S:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\SoWT\\ATI\\HabitatSuitabilityMap.tif")


# curation ----
  # ati as sf and transform----
ati <- st_as_sf(ati, coords = c("eastings", "northings"), #c("Lng", "Lat"), #  , 
                crs = 27700, #WGS84 = 7660; GBgrid = 27700
                agr = "constant") 

ati.wgs <- st_transform(ati, crs = 7660)




# -----


# cut out those within thresh of location of itnerest -----
loc.of.interest = data.frame(eastings = 353020, northings = 555050 )
dist.thresh = 2000

loc.of.interest = st_as_sf(loc.of.interest, coords = c("eastings", "northings"), 
                           crs = 27700, agr = "constant")

ati.coords = st_coordinates(ati)
loc.coords = st_coordinates(loc.of.interest)

dist = sqrt(abs(ati.coords[,1] - loc.coords[,1])^2 +
              abs(ati.coords[,2] - loc.coords[,2])^2)

ati02 = ati[which( dist <= dist.thresh),]
 ati02 = st_transform( ati02, crs = 3857)
 loc.of.interest = st_transform(loc.of.interest, crs = 4326)

write_sf(ati02, "atirequest01.shp")
write.csv(ati02, "atirequest01.csv")
 


# MAP ATI RECORDS ----

#  leaflet maps -----

# subset ----
icon.scaler01 = 0.06

tree.icon = 
  icons(iconUrl = ifelse(ati02$VeteranStatus == "Ancient tree",
                         "S:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\SoWT\\ATI\\atree05.svg",
                         "S:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\SoWT\\ATI\\vtree05.svg")
        ,
        iconWidth = 602*icon.scaler01, iconHeight = 670*icon.scaler01,
        iconAnchorX = 0, iconAnchorY = 0,
  )

lopt = labelOptions(noHide = TRUE,
                    direction = 'top',
                    textOnly = TRUE)


m <- leaflet() %>%
  setView(lng = mean(ati02$Lng), lat = mean(ati02$Lat), zoom = 17) %>%
  addTiles() %>%
  addMarkers(lng=ati02$Lng, lat=ati02$Lat, popup=paste0(
    "<strong>TreeNumber: </strong>",    ati02$TreeNumber, "<br>",
    "<strong>Type: </strong>",          ati02$VeteranStatus, "<br>",
    "<strong>Species: </strong>",       ati02$Species, "<br>",
    "<strong>Girth: </strong>",         ati02$MeasuredGirth, "<br>" ,
    "<strong>Surroundings: </strong>",  ati02$Surroundings, "<br>" 
    ),
    clusterOptions = markerClusterOptions(disableClusteringAtZoom= 15),
    icon = tree.icon) %>% 
  addFeatures(.) %>%
  addStaticLabels(., label = "Map produced by Dr. Ewan McHenry")


m02 <- addControlGPS(m, options = gpsOptions(position = "topleft", activate = TRUE, 
                                               autoCenter = TRUE, maxZoom = 10, 
                                               setView = TRUE))
m03 = activateGPS(m02)

saveWidget(m03, file="ati02.html")

# whole ati ----

r <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=ati$Lng, lat=ati$Lat, popup=paste0(
    "<strong>TreeNumber: </strong>",    ati$TreeNumber, "<br>",
    "<strong>Type: </strong>",          ati$VeteranStatus, "<br>",
    "<strong>Species: </strong>",       ati$Species, "<br>",
    "<strong>Girth: </strong>",         ati$MeasuredGirth, "<br>" ,
    "<strong>Surroundings: </strong>",  ati$Surroundings, "<br>" ),
    clusterOptions = markerClusterOptions())
   
saveWidget(r, file="ati.full.html")

# adding hab suitability


pal <- colorNumeric(viridis(3), values(hab.suit),
                    na.color = "transparent")

r01 = leaflet() %>% addTiles() %>%
  addRasterImage(hab.suit, colors = pal, 
                 opacity = 0.8) %>%
  addLegend(pal = pal, 
            values = values(hab.suit),
            title = "Habitat suitability score")  

  saveWidget(r01, file="ATIhab.suit.html")
  
# adding occ prob and location ----
  icon.scaler01 = 0.04
  tree.icon = 
     icons(iconUrl = ifelse(ati$VeteranStatus == "Ancient tree",
                                     "S:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\SoWT\\ATI\\atree05.svg",
                                     "S:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\SoWT\\ATI\\vtree05.svg")
                     ,
    iconWidth = 602*icon.scaler01, iconHeight = 670*icon.scaler01,
    iconAnchorX = 0, iconAnchorY = 0,
  )
  
  pal.occ <- colorNumeric(viridis(3), values(presence.pred),
                      na.color = "transparent")
  
  r02 = leaflet() %>% addTiles() %>%
    setView(lng = median(ati$Lng), lat = median(ati$Lat), zoom = 7) %>%
    addRasterImage(presence.pred, colors = pal.occ, 
                   opacity = 0.8, group = "Prediction layer") %>%
    addLayersControl(
      overlayGroups = c("Prediction layer", "Recorded trees"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    addMarkers(lng=ati$Lng, lat=ati$Lat, popup=paste0(
      "<strong>TreeNumber: </strong>",    ati$TreeNumber, "<br>",
      "<strong>Type: </strong>",          ati$VeteranStatus, "<br>",
      "<strong>Species: </strong>",       ati$Species, "<br>",
      "<strong>Girth: </strong>",         ati$MeasuredGirth, "<br>" ,
      "<strong>Surroundings: </strong>",  ati$Surroundings, "<br>",
      "<strong>Access: </strong>",  ati$PublicAccessibility, "<br>"
    ),
    clusterOptions = markerClusterOptions(disableClusteringAtZoom= 16),
    group = "Recorded trees", icon = tree.icon )%>%
  addLegend(pal = pal.occ, 
            values = values(presence.pred),
            title = "Occupancy \nprobability", group = "Prediction layer") %>% 
    hideGroup("Prediction layer")
  # Layers control
  
  saveWidget(r02, file="ATI.occpred.html")
  
  r03 <- addControlGPS(r02, options = gpsOptions(position = "topleft", activate = TRUE, 
                                                 autoCenter = TRUE, maxZoom = 10, 
                                                 setView = TRUE))
  r04 = activateGPS(r03)
  
  saveWidget(r04, file="ATI.occpred.location.html")
  
# transforming occ prob to something a bit more informative  -----
  
  #exploration ----
  # if that probability was repeated over a different area, what would total prob be
  a = values(presence.pred)
  b = sort(a[a>= quantile(a, probs = c(0.8), na.rm = T)], decreasing = T)
  c = 1-((1-b)^(10*10))
  d = 1-((1-a)^(10*10))
  e = 1-((1-a)^(5*5))

  # quantile(a, probs = c(seq(0,1,length = 21)), na.rm = T)
  # quantile(a, probs = c(0.99, 0.999, 0.9999), na.rm = T)
  # # squish all to max 0.001% of records = 130 squares
  # 
  # hist(diff(quantile(a, probs = c(seq(0,1,length = 21)), na.rm = T)))
  # plot(quantile(a, probs = c(seq(0,1,length = 100)), na.rm = T))
  # sum(!is.na(a))/10
  # 
  # hist(sort(a, decreasing = T)[1:13072])
  # 
  # hist(1-((1-values(presence.pred))^(10*10)))
  
  dum.pred = presence.pred
  dum.thresh1 =quantile(b, probs = c(0.99), na.rm = T) 
  dum.thresh2 = sort(a, decreasing = T)[26144]
  
  max.colour = quantile(a, probs = c(0.99), na.rm = T)

  values(dum.pred)[values(dum.pred)>=  max.colour] = max.colour
  
  
  # plot specs
  icon.scaler01 = 0.04
  labels <- c("Low", "Med", "High")
  tree.icon = 
    icons(iconUrl = ifelse(ati$VeteranStatus == "Ancient tree",
                           "S:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\SoWT\\ATI\\atree05.svg",
                           "S:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\SoWT\\ATI\\vtree05.svg")
          ,
          iconWidth = 602*icon.scaler01, iconHeight = 670*icon.scaler01,
          iconAnchorX = 0, iconAnchorY = 0,
    )
  
  pal.occ <- colorNumeric(viridis(3), values(dum.pred),
                          na.color = "transparent")
  
  
  #plot
  r02 = leaflet() %>% addTiles() %>%
    setView(lng = median(ati$Lng), lat = median(ati$Lat), zoom = 7) %>%
    addRasterImage(dum.pred, colors = pal.occ, 
                   opacity = 0.8, group = "Predicted veteran occurance") %>%
    addLayersControl(
      overlayGroups = c("Predicted veteran occurance", "Recorded trees"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    addMarkers(lng=ati$Lng, lat=ati$Lat, popup=paste0(
      "<strong>TreeNumber: </strong>",    ati$TreeNumber, "<br>",
      "<strong>Type: </strong>",          ati$VeteranStatus, "<br>",
      "<strong>Species: </strong>",       ati$Species, "<br>",
      "<strong>Girth: </strong>",         ati$MeasuredGirth, "<br>" ,
      "<strong>Surroundings: </strong>",  ati$Surroundings, "<br>",
      "<strong>Access: </strong>",  ati$PublicAccessibility, "<br>"
    ),
    clusterOptions = markerClusterOptions(disableClusteringAtZoom= 16),
    group = "Recorded trees", icon = tree.icon )%>%
    addLegend(pal = pal.occ, 
              values = values(dum.pred),
              title = "Probability of veteran trees", group = "Predicted veteran tree occurance",
              labFormat = function(type, cuts, p) {  # Here's the trick
                paste0(labels)
              }) %>% 
    hideGroup("Predicted veteran tree occurance")
  # Layers control
  
  saveWidget(r02, file="ATI.OccPred.Trees.html")
  
  r03 <- addControlGPS(r02, options = gpsOptions(position = "topleft", activate = TRUE, 
                                                 autoCenter = TRUE, maxZoom = 10, 
                                                 setView = TRUE))
  r04 = activateGPS(r03)
  
  saveWidget(r04, file="ATI.OccPred.location.Trees.html")
## plot same no trees
  
  
  #plot
  r02 = leaflet() %>% addTiles() %>%
    setView(lng = median(ati$Lng), lat = median(ati$Lat), zoom = 7) %>%
    addRasterImage(dum.pred, colors = pal.occ, 
                   opacity = 0.8, group = "Predicted veteran occurance") %>%
    addLayersControl(
      overlayGroups = c("Predicted veteran occurance"),# "Recorded trees"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    # addMarkers(lng=ati$Lng, lat=ati$Lat, popup=paste0(
    #   "<strong>TreeNumber: </strong>",    ati$TreeNumber, "<br>",
    #   "<strong>Type: </strong>",          ati$VeteranStatus, "<br>",
    #   "<strong>Species: </strong>",       ati$Species, "<br>",
    #   "<strong>Girth: </strong>",         ati$MeasuredGirth, "<br>" ,
    #   "<strong>Surroundings: </strong>",  ati$Surroundings, "<br>",
    #   "<strong>Access: </strong>",  ati$PublicAccessibility, "<br>"
    # ),
    # clusterOptions = markerClusterOptions(disableClusteringAtZoom= 16),
    # group = "Recorded trees", icon = tree.icon )%>%
    addLegend(pal = pal.occ, 
              values = values(dum.pred),
              title = "Probability of veteran trees", group = "Predicted veteran occurance",
              labFormat = function(type, cuts, p) {  # Here's the trick
                paste0(labels)
              })# %>% 
  #  hideGroup("Prediction layer")
  # Layers control
  
  saveWidget(r02, file="ATI.TransOccPred.NoTrees.html")
  
  r03 <- addControlGPS(r02, options = gpsOptions(position = "topleft", activate = TRUE, 
                                                 autoCenter = TRUE, maxZoom = 10, 
                                                 setView = TRUE))
  r04 = activateGPS(r03)
  
  saveWidget(r04, file="ATI.OccPred.location.noTrees.html")
  
























# play with openstreetmap ------

 # http://joshuamccrain.com/tutorials/maps/streets_tutorial.html -----
library(tidyverse)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(ggmap)
library(rvest)

getbb("Atlanta Georgia")
 
big_streets <- getbb("Asheville United States")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()


ggmap(big_streets)

ggplot() +
  geom_sf(data = big_streets,
          inherit.aes = FALSE,
          color = "black")


# https://www.linkedin.com/pulse/plot-over-openstreetmap-ggplot2-abel-tortosa-andreu ----
library(OpenStreetMap)
library(ggplot2)

LAT1 =  30 ; LAT2 = 50
LON1 = -10 ; LON2 = 10

map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL,
               type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[1],
               mergeTiles = TRUE) 

# https://cengel.github.io/R-spatial/mapping.html
ph_basemap <- get_map(location=c(lon = -75.16522, lat = 39.95258), zoom=11, maptype = 'roadmap', source = 'esri')

sbbox <- make_bbox(lon = range(ati02$Lng) , lat = range(ati02$Lat), f = .1)




# get map
brisbane = get_map(location = sbbox  , zoom = 13,
                   maptype="satellite", source = "google")
# create map
brisbanemap = ggmap(brisbane)
brisbanemap

# https://www.rdocumentation.org/packages/osmar/versions/1.1-7/topics/get_osm ----



api <- osmsource_api()
  box <- corner_bbox(11.579341, 48.15102, 11.582852, 48.1530)
  gschw <- get_osm(box, source = api)

  kaufstr <- get_osm(way(3810479))
  kaufstr_full <- get_osm(way(3810479), full = TRUE)
  ## End(Not run)

  # https://www.rdocumentation.org/packages/ggmap/versions/3.0.0/topics/get_openstreetmap ----
  get_openstreetmap(bbox = c(left = -95.80204, bottom = 29.38048, right =
                               -94.92313, top = 30.14344))
  
  
  # ------
  
# change coord system
## OSM CRS :: "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

mytheme <- theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
                 panel.background = element_rect(colour = NA),
                 plot.background = element_rect(colour = NA),
                 axis.title = element_text(face = "bold",size = rel(1)),
                 axis.title.y = element_text(angle=90,vjust =2),
                 axis.title.x = element_text(vjust = -0.2))

OSMap <- autoplot(map.latlon)  +
  labs(title = "Plot over OpenStreetMap", subtitle = "Bias [%]",x = "Longitude", y="Latitude")+
  scale_fill_gradientn(colours = rainbow(10)) +
  mytheme



# playing with cod ein https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html -----
sbbox <- make_bbox(lon = ati02$Lng, lat = ati02$Lat, f = .1)




x <- opq(bbox = c(-0.27, 51.47, -0.20, 51.50)) %>% # Chiswick Eyot in London, U.K.
       add_osm_feature(key = 'name', value = "Thames", value_exact = FALSE) %>%
       osmdata_sf()

sq_map <- get_map(location = "Belfast", maptype = "satellite", source = "google")
ggmap(x) 


