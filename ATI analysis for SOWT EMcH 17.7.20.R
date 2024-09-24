# ATI analysis 16.7.20

# library ----
library(sf) # for gis
library("rnrfa") # to convert os gb grif ref
library(rgdal) # to convert lat long to os gb

library("rnaturalearth") # map of countries of the entire world
library("rnaturalearthdata")
library("rgeos")
library(tidyverse)
library(parlitools)
library(extrafont)
library(nngeo) # for st_nn()
library(plgraphics) # for prettyscale()
library(rmapshaper) # for ms_simplify()
library(svglite)
library(ggpubr) 
library(viridis)
library(U.utilities) # devtools::install_github("EwanMcHenry/U.utilities")
library(units)
library(leaflet)
library(leaflet.extras)
library(DT)
library(htmltools)

# load data ----
# ati taken form cuation code, nnjoined with Awi and NFI in QGIS where that processing is quicker
load(paste0(gis.wd, "\\Data\\ATI\\ati2024-09-20curated_fromATI - 2024_08_05.csv.RData"))
countries = st_read("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\Data\\administrative boundaries\\Countries\\5countries.v1.05.shp") %>% 
  st_transform(27700) %>% 
  st_buffer(0)

#convert ati crs
ati <- ati %>% st_transform(27700)

# add countries area
countries$country.ha <-  st_area(countries) %>% 
  set_units(value = "ha") %>%
  as.numeric()
countries <- countries %>% 
  rename(country = ctry17nm) %>%
  mutate(country = factor(country, levels = sort(unique(country))))

# configure ----
constants <- list(hexdist.h = 10000,
                  hexdist.v = 10000,
                  focal_countries = c("England", "Scotland", "Wales", "N.Ireland"),
                  map.height = 5, 
                  map.width = 5
)

# Record points MAPS ----
## curation ----
ati <- st_join(ati, countries) %>% 
  filter(country %in% constants$focal_countries ) %>% 
  #remove unsued levels
  mutate(country = droplevels(country))

## ggplot records----
ggplot(data = countries) +
  geom_sf() +
  geom_sf(data = ati, size = 1, shape = 23, fill = "darkred") 
## leaflet records ----


# Ensure the `ati_wgs` data is transformed to WGS84 (EPSG:4326)
avt_wgs <- st_transform(ati[ati$VeteranStatusName %in% c("Veteran tree", "Ancient tree"),], 4326)
avt_lost_wgs <- st_transform(ati[ati$VeteranStatusName %in% c("Lost Veteran tree", "Lost Ancient tree"),], 4326)

# Define color palette based on VeteranStatusName
cols4trees <- c("darkblue", "orange")
colours_lost <- c("Lost Ancient tree" = cols4trees[1], "Lost Veteran tree" = cols4trees[2])  # Colors for Lost trees
pal_lost <- colours_lost[as.character(avt_lost_wgs$VeteranStatusName)]
colours_avt <- c("Ancient tree" = cols4trees[1], "Veteran tree" = cols4trees[2] )  # Colors for Lost trees

pal_avt <- colorFactor(palette = colours_avt, domain = names(colours_avt))
# Define custom "X" icons with dynamic colors for the Lost group
lost_icons <- awesomeIcons(
  icon = "times",  # 'times' is a FontAwesome "X"
  iconColor = ~pal_lost(VeteranStatusName),  # Icon itself remains white
  markerColor = ~pal_lost(VeteranStatusName),  # Marker background color
  library = "fa"
)


# Create a leaflet map with filtering based on specific VeteranStatus types
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  
  # Add custom icons for Veteran trees with dynamic color
  addAwesomeMarkers(data = avt_wgs,
                    ~st_coordinates(avt_wgs)[,1],
                    ~st_coordinates(avt_wgs)[,2],
                    icon = awesomeIcons(
                      icon = 'leaf-outline',
                      iconColor = 'black',
                      library = 'ion',
                      markerColor = as.character(colours_avt[as.character(avt_wgs$VeteranStatusName)])
                    ),
                    popup = ~paste("<strong>Status:</strong>", VeteranStatusName, "<br>",
                                   "<strong>Species:</strong>", SpeciesName, "<br>",
                                   "<strong>Form:</strong>", TreeFormName, "<br>",
                                   "<strong>Standing:</strong>", StandingStatusName, "<br>",
                                   "<strong>Date added:</strong>", VerifiedDate, "<br>"),
                    group = "AVTs",
                    clusterOptions = markerClusterOptions(disableClusteringAtZoom = 10)) %>%
  
  # Add custom "X" icons for Lost trees with dynamic color
  addAwesomeMarkers(data = avt_lost_wgs,
                    ~st_coordinates(avt_lost_wgs)[,1],
                    ~st_coordinates(avt_lost_wgs)[,2],
                    icon = awesomeIcons(
                      icon = 'ios-close',
                      iconColor = 'black',
                      library = 'ion',
                      markerColor = as.character(pal_lost)
                    ),
                    popup = ~paste("<strong>Status:</strong>", VeteranStatusName, "<br>",
                                   "<strong>Species:</strong>", SpeciesName, "<br>",
                                   "<strong>Form:</strong>", TreeFormName, "<br>",
                                   "<strong>Standing:</strong>", StandingStatusName, "<br>",
                                   "<strong>Date added:</strong>", VerifiedDate, "<br>"),
                    group = "Lost",
                    clusterOptions = markerClusterOptions(disableClusteringAtZoom = 10)) %>%
  
  # Add a legend for the Veteran trees
  addLegend("bottomright",
            colors = colours_avt,
            labels = c("Ancient tree", "Veteran tree"),
            title = NA,
            opacity = 1) %>% 
  
  # Add layer controls to toggle specific VeteranStatus types
  addLayersControl(
    overlayGroups = c("AVTs", "Lost"),  # Allow toggling between groups
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("Lost") %>%
  
  # Optional: Add search bar
  addSearchOSM(options = searchOptions(
    position = "topleft",
    collapsed = T
  ))

rm(avt_wgs, avt_lost_wgs) 



# TABLES OF INFO ----
tree_in_country <- st_join(ati, countries)

## Numbers of and proportions of records ----
### t country - vet status----
table(tree_in_country$VeteranStatusName, tree_in_country$country) %>%
  addmargins(margin = c(1,2)) 

table(tree_in_country$VeteranStatusName, tree_in_country$country) %>%
  prop.table() %>%
  addmargins() %>% 
  round(3) 
  

### t country - form----
create_ordered_table(tree_in_country, "TreeFormName", "country", type = "frequency", round_digits = 0)

create_ordered_table(tree_in_country, "TreeFormName", "country", type = "proportion", round_digits = 5) %>% 
  replace_rounded_zeros_with_string(3)

### t vet status - form----
create_ordered_table(tree_in_country, "TreeFormName", "VeteranStatusName", type = "frequency")

create_ordered_table(tree_in_country, "TreeFormName", "VeteranStatusName", type = "proportion", round_digits = 5) %>% 
  replace_rounded_zeros_with_string(3)

### t vet status - species----
create_ordered_table(tree_in_country, "SpeciesName", "VeteranStatusName", type = "frequency")

create_ordered_table(tree_in_country, "SpeciesName", "VeteranStatusName", type = "proportion", round_digits = 5) %>% 
  replace_rounded_zeros_with_string(3)

### t vet status - standing----
create_ordered_table(tree_in_country, "StandingStatusName", "VeteranStatusName", type = "frequency")

create_ordered_table(tree_in_country, "StandingStatusName", "VeteranStatusName", type = "proportion", round_digits = 5) %>% 
  replace_rounded_zeros_with_string(3)

## record density - country----

### t - density AVT type - country -  per 100 km2, a 10 x 10 km hectad----
T_avt_dens <- table(tree_in_country$VeteranStatusName, tree_in_country$country) %>% 
  addmargins(margin = c(1,2)) %>% 
  sweep(2, c(countries$country.ha, sum(countries$country.ha)), "/")*100 
colnames(T_avt_dens)[ncol(T_avt_dens)] <- "All countries combined"
rownames(T_avt_dens)[nrow(T_avt_dens)] <- "All AVT types combined"

replace_rounded_zeros_with_string(T_avt_dens, 2)


# HEX MAPS ----
  ## curation - hex table - Ancient, vet and lost ----
# hex grid
hexing_area <- countries[countries$country %in% constants$hexing_countries,] %>% 
  st_union() %>% 
  st_remove_holes() %>%  
  st_simplify(dTolerance = 100, preserveTopology = T) %>% 
  st_make_valid()

hex.grid0 = st_make_grid(hexing_area,
                         c(constants$hexdist.h, constants$hexdist.v), 
                         what = "polygons", square = F)
hex.grid0 <- st_sf(grid_id = 1:length(hex.grid0), geometry = hex.grid0) # add hex grid id

hex.grid = hex.grid0 %>%
  st_intersection(hexing_area) 
# note hexes with same grid_id are the same multipolygon feature - possible bug/feature where bits of the same hex separated by water all share same attributes

rm(hex.grid0, hexing_area)

hex.grid$hex.ha <-  st_area(hex.grid) %>% 
  set_units(value = "ha") %>%
  as.numeric()

tree_in_maphex <- st_join(ati, hex.grid) # join layers
tree_in_maphex$grid_id <- factor(tree_in_maphex$grid_id, levels =  unique(tree_in_maphex$grid_id  )) # make factor so that consts with 0 still included
# tables of N ATI vet types by hex 
t_hex.VeteranStatus <-  addmargins(table(tree_in_maphex$grid_id, tree_in_maphex$VeteranStatusName), margin = 2)
maphex.table <- as_tibble( cbind ( rownames(t_hex.VeteranStatus) , t_hex.VeteranStatus ))
maphex.table[, colnames(maphex.table) %in% c(colnames(t_hex.VeteranStatus))] <- # those cols not... the id??
  sapply(maphex.table[, colnames(maphex.table) %in% c(colnames(t_hex.VeteranStatus))], as.integer) # make sure theyre integers
# name grid_id col in maphex.table
colnames(maphex.table)[1] <- "grid_id"

# add hex area from hex.grid, using grid_id
maphex.table$hex.ha <- hex.grid$hex.ha[match(maphex.table$grid_id, hex.grid$grid_id)]

# density of ATs per km sq
maphex.table$dens.ancient = 100* maphex.table$`Ancient tree`/maphex.table$hex.ha
maphex.table$dens.vet = 100* maphex.table$`Veteran tree`/maphex.table$hex.ha
maphex.table$dens.notable = 100* maphex.table$`Notable tree`/maphex.table$hex.ha
maphex.table$dens.avt = 100* (maphex.table$`Ancient tree` + maphex.table$`Veteran tree`)/maphex.table$hex.ha
maphex.table$dens.all = 100* (maphex.table$`Ancient tree` + maphex.table$`Veteran tree` + maphex.table$`Notable tree`)/maphex.table$hex.ha

# proportion lost 
maphex.table$p.ancient.lost = maphex.table$`Lost Ancient tree`/ sum(maphex.table$`Ancient tree`, maphex.table$`Lost Ancient tree`) 
maphex.table$p.vet.lost <- maphex.table$`Lost Veteran tree`/ sum(maphex.table$`Veteran tree`, maphex.table$`Lost Veteran tree`)
maphex.table$p.notable.lost <- maphex.table$`Lost Notable tree`/sum(maphex.table$`Notable tree`, maphex.table$`Lost Notable tree`)
maphex.table$p.avt.lost <- sum(maphex.table$`Lost Ancient tree`, maphex.table$`Lost Veteran tree`)/sum(maphex.table$`Ancient tree`, maphex.table$`Lost Ancient tree`, maphex.table$`Veteran tree`, maphex.table$`Lost Veteran tree`)
maphex.table$p.all.lost <- sum(maphex.table$`Lost Ancient tree`, maphex.table$`Lost Veteran tree`, maphex.table$`Lost Notable tree`)/sum(maphex.table$`Ancient tree`, maphex.table$`Lost Ancient tree`, maphex.table$`Veteran tree`, maphex.table$`Lost Veteran tree`, maphex.table$`Notable tree`, maphex.table$`Lost Notable tree`)

## merge hex grid with maphex.table
hex.ATI.shp = merge(hex.grid,  maphex.table[ , -which(names(maphex.table) == "hex.ha")], by = "grid_id", by.y = "grid_id", all.x = TRUE)
#replace new NAs from maphex.table with 0, except for the proportion lost columns
hex.ATI.shp[is.na(hex.ATI.shp)] <- 0
hex.ATI.shp$`p.ancient.lost`[hex.ATI.shp$`Ancient tree` == 0] <- NA
hex.ATI.shp$`p.vet.lost`[hex.ATI.shp$`Veteran tree` == 0] <- NA
hex.ATI.shp$`p.notable.lost`[hex.ATI.shp$`Notable tree` == 0] <- NA
hex.ATI.shp$`p.avt.lost`[hex.ATI.shp$`Ancient tree` + hex.ATI.shp$`Veteran tree` == 0] <- NA
hex.ATI.shp$`p.all.lost`[hex.ATI.shp$`Ancient tree` + hex.ATI.shp$`Veteran tree` + hex.ATI.shp$`Notable tree` == 0] <- NA

  ## HEX MAP HEATMAPS ----
constants$scale.name = "Hex"
### density HEX MAPS ----
#### ATI records density HEXMAP ----

ati.hex.map <- map.ploter(to.plot = hex.ATI.shp$dens.all,
           fill.scale.title = expression(paste("ATI records per km"^"2",sep = "")),
           main.title = "ATI record density" ,
           sub.title = NULL,
           fillground = hex.ATI.shp,
           background = countries,
           pltly.text = NULL,
           transformation = "log10",
           col.limits = c(0.01, 500),
           use.viridis = TRUE,
           low.col = "white",
           high.col = "red",
           fill.line_size = 0.05,
           fill.line_colour = NA,
           background.fill = "grey90",
           background.size = 0.05,
           background.colour = "black",
           n.breaks = 5,
           round_to = 0.1,
           just_pretty = T,
           manual_breaks = c(0.1,1,10, 100)
) 
ggsave(plot = ati.hex.map , paste("analysis outputs//maps//", ati_filename, "- ati hexmap.pdf"), 
       height = constants$map.height, width = constants$map.width)

#### AVT records density HEXMAP ----

avt.hex.map <- map.ploter(to.plot = hex.ATI.shp$dens.avt,
                          fill.scale.title = expression(paste("AVT records per km"^"2",sep = "")),
                          main.title = "AVT record density" ,
                          sub.title = "Ancient & veteran trees",
                          fillground = hex.ATI.shp,
                          background = countries,
                          pltly.text = NULL,
                          transformation = "log10",
                          col.limits = c(0.01, 500),
                          use.viridis = TRUE,
                          low.col = "white",
                          high.col = "red",
                          fill.line_size = 0.05,
                          fill.line_colour = NA,
                          background.fill = "grey90",
                          background.size = 0.05,
                          background.colour = "black",
                          n.breaks = 5,
                          round_to = 0.1,
                          just_pretty = T,
                          manual_breaks = c(0.1,1,10, 100)
) 
ggsave(plot = avt.hex.map , paste("analysis outputs//maps//", ati_filename, "- avt.hex.map.pdf"), 
       height = constants$map.height, width = constants$map.width)

#### Ancient records density HEXMAP ----

ancient.hex.map <- map.ploter(to.plot = hex.ATI.shp$dens.ancient,
                          fill.scale.title = expression(paste("Ancient tree records per km"^"2",sep = "")),
                          main.title = "Ancient tree record density" ,
                          sub.title = NULL,
                          fillground = hex.ATI.shp,
                          background = countries,
                          pltly.text = NULL,
                          transformation = "log10",
                          col.limits = c(0.01, 500),
                          use.viridis = TRUE,
                          low.col = "white",
                          high.col = "red",
                          fill.line_size = 0.05,
                          fill.line_colour = NA,
                          background.fill = "grey90",
                          background.size = 0.05,
                          background.colour = "black",
                          n.breaks = 5,
                          round_to = 0.1,
                          just_pretty = T,
                          manual_breaks = c(0.1,1,10, 100)
) 
ggsave(plot = ancient.hex.map , paste("analysis outputs//maps//", ati_filename, "- ancient.hex.map.pdf"), 
       height = constants$map.height, width = constants$map.width)

# ##### alt ----
# # bespoke for SoWT
# # drawing hap-hazrd from from https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# # messing up my tidy code, but such is life.. its only me who'll ever have to use it again anyway
# # ... sorry future Ewan :(  feel free to just hack this bit out
# var.name = "ATI.count"
# main.title = "Total ATI records"
# sub.title = "per 10km high Hexagon"
# fill.scale.title = "ATI records"
# colour.limits = c(0,1000)
# transformation = "identity"
# var.scale.factor = 1
# 
# variable = hex.ATI.shp[[which(colnames(hex.ATI.shp) == var.name)]] / var.scale.factor
# 
# 
# labels <- c()
# quantiles <- c(0,1,3,10,100,max(variable))
# no_classes <- length (quantiles)
# for(idx in 1:length(quantiles)){
#   labels <- c(labels, paste0(round(quantiles[idx], 2), 
#                              " – ", 
#                              round(quantiles[idx + 1], 2)))
# }
# labels <- labels[1:length(labels)-1]
# 
# # here I actually create a new 
# # variable on the dataset with the quantiles
# variable02 <- cut(variable, 
#                   breaks = quantiles, 
#                   labels = labels, 
#                   include.lowest = T)
# 
# clr.breaks = colour.brks(lims = colour.limits)
# clr.labels = colour.lable(x = variable ,
#                           lims = colour.limits , 
#                           breaks = colour.brks(colour.limits ),
#                           var.scale.factor = 1)
# 
# sowt.plot = ggplot() +
#   geom_sf(data = countries, size = 0.2) +
#   geom_sf(data = hex.ATI.shp, mapping = aes(fill = variable02  ), colour = NA) +
#   geom_sf(data = hex.ATI.shp, fill = NA, size = 0.05, colour = "grey90") +
#   scale_fill_viridis( name = fill.scale.title,
#                       discrete = T,
#                       guide = guide_legend(
#                         keyheight = unit(5, units = "mm"),
#                         title.position = 'top')
#                       # guide = guide_colorbar(
#                       #   direction = "horizontal", barheight = unit(2, units = "mm"),
#                       #   barwidth = unit(50, units = "mm"), draw.ulim = F,
#                       #   title.position = 'top', title.hjust = 0.5, label.hjust = 0.5)
#   )+
#   labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
#   )+
#   theme_map() +
#   theme(legend.position = "bottom") 
# 
# sowt.plot = ggplot() +
#   geom_sf(data = countries, size = 0.2) +
#   geom_sf(data = hex.ATI.shp, mapping = aes(fill = variable02  ), colour = NA) +
#   geom_sf(data = hex.ATI.shp, fill = NA, size = 0.05, colour = "grey90") +
#   labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
#   )+
#   theme_map() +
#   theme(legend.position = "bottom") +
#   scale_fill_manual(
#     # in manual scales, one has to define colors, well, manually
#     # I can directly access them using viridis' magma-function
#     values = rev(viridis(6)),
#     breaks = rev(brks_scale),
#     name = "Average age",
#     drop = FALSE,
#     labels = labels_scale,
#     guide = guide_legend(
#       direction = "horizontal",
#       keyheight = unit(2, units = "mm"),
#       keywidth = unit(70 / length(labels), units = "mm"),
#       title.position = 'top',
#       # I shift the labels around, the should be placed 
#       # exactly at the right end of each legend key
#       title.hjust = 0.5,
#       label.hjust = 1,
#       nrow = 1,
#       byrow = T,
#       # also the guide needs to be reversed
#       reverse = T,
#       label.position = "bottom"
#     )
#   )
# ggsave(plot = sowt.plot ,paste(constants$scale.name, var.name, "sowt.plot01","map.pdf"), height = 5, width = 5)



# 
# # MP consituency ATI analysis ----
# ## MP constituency - Ancient, vet and lost tables ----
# 
# tree_in_MP <- st_join(ati, westminster.const) # join layers
# tree_in_MP$pcon17nm = factor(tree_in_MP$pcon17nm, levels =  unique(westminster.const$pcon17nm  )) # make factor so that consts with 0 still included
# #tables of lost and remaining and combine
# MP.ther.table = addmargins(table(tree_in_MP$pcon17nm[tree_in_MP$lost==0], tree_in_MP$ancient[tree_in_MP$lost==0]), margin = 2)
# MP.lost.table = addmargins(table(tree_in_MP$pcon17nm[tree_in_MP$lost==1], tree_in_MP$ancient[tree_in_MP$lost==1]), margin = 2)
# colnames(MP.ther.table) = c("Ancient", "Veteran", "Tot")
# colnames(MP.lost.table) = paste(colnames(MP.ther.table), "lost", sep = ".")
# MP.table = as.tibble( cbind ( rownames(MP.ther.table) , MP.ther.table , MP.lost.table ))
# MP.table[, 2:7] <- sapply(MP.table[, 2:7], as.integer)
# # proportion lost
# MP.table$Prop.ancient.lost = MP.table$Ancient.lost/ sum(MP.table$Ancient.lost, MP.table$Ancient) *10^6
# MP.table$Prop.veterans.lost = MP.table$Veteran.lost/ sum(MP.table$Veteran.lost, MP.table$Veteran) *10^6
# MP.table$Prop.total.lost = MP.table$Tot.lost/ sum(MP.table$Tot.lost, MP.table$Tot) *10^6
# 
# # area of consitituency in km sq
# MP.table = merge(westminster.const , MP.table , by.y =  "V1" , by.x = "pcon17nm" )
# MP.table$area = MP.table$area / 1000000
# MP.table = st_set_geometry(MP.table, NULL)
# 
# # # gss code
# # MP.table$gss_code = merge(as.data.frame(westminster.const) , as.data.frame(MP.table) , by.x = "pcon17nm" , by.y =  "V1" )$pcon17cd
# 
# # density of ATs per 
# MP.table$Ancient.dens = MP.table$Ancient/MP.table$area
# MP.table$vet.dens = MP.table$Veteran/MP.table$area
# MP.table$tot.dens = MP.table$Tot/MP.table$area
# 
# # name and rearrange coloums
# colnames(MP.table) = c( "Constituency"  , colnames(MP.table)[2:9], "area", 
#                         "Ancient.count", "Veteran.count" , "ATI.count" , 
#                         "ancient.lost", "Veterans.lost", "Total.lost", "Ancient.loss.per.Million",  "Veteran.loss.per.Million", "Total.loss.per.Million",
#                         "ancient.density", "veteran.density", "ATI.density")
# MP.table = MP.table [, c("Constituency", "area", 
#                          "Ancient.count", "ancient.density", "Veteran.count", "veteran.density", "ATI.count", "ATI.density", 
#                          "ancient.lost","Ancient.loss.per.Million", "Veterans.lost", "Veteran.loss.per.Million", "Total.lost", "Total.loss.per.Million")]
# write.csv(MP.table,file = "MP.table.csv" )
# 
# ## MP constituency heat maps-----
#   MP.ATI.shp = merge(westminster.const, MP.table , by.x = "pcon17nm", by.y = "Constituency")
#   
#   west_hex_map$constituency_name[west_hex_map$constituency_name =="Fermanagh & South Tyrone" ] = "Fermanagh and South Tyrone"   
#   west_hex_map$constituency_name[west_hex_map$constituency_name =="Newry & Armagh" ] = "Newry and Armagh"   
#   
#   westhex.ATI = merge(west_hex_map, MP.table , by.x = "constituency_name", by.y = "Constituency")
#   constants$scale.name = "MP"
#   
#     ### COUNT MAPS ----
#       #### total ATI records ---- 
#     var.name = "ATI.count"
#     main.title = "Total ATI records"
#     sub.title = "2017 Westminster constituencies"
#     fill.scale.title = "ATI records"
#     colour.limits = c(0,max(MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]))
#     transformation = "log10"
#     var.scale.factor = 1
#     
#         # boundry map
#     variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] * var.scale.factor
#     
#     ati.mp.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#                     background = countries, fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
#                     transformation = transformation, 
#                     n.breaks = 6,
#                     to.plot = variable)
#     ggsave(plot = ati.mp.map ,paste(constants$scale.name, var.name, "map.pdf"))
#     ggsave(plot = ati.mp.map ,paste(constants$scale.name, var.name, "map.svg"))
#     
#         # hex map
#     variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]] * var.scale.factor    
#     
#     tot.mp.hexmap =  map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#                     background = NULL,
#                     fillground = westhex.ATI,
#                     transformation = "log10",
#                     n.breaks = 6,
#                     to.plot = variable)
#     
#     ggsave(plot = tot.mp.hexmap, paste(constants$scale.name,var.name, "hexmap.pdf"))
#     
#       #### Ancient Tree records ---- 
#     var.name = "Ancient.count"
#     main.title = "Ancient tree records"
#     sub.title = "2017 Westminster constituencies"
#     fill.scale.title = "Ancient tree records"
#     colour.limits = c(0,max(MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]))
#     transformation = "log10"
#     var.scale.factor = 1
#     
#         # boundry map
#         variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]/ var.scale.factor
#         
#         ancient.mp.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#                                            background = countries, fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
#                                            transformation = transformation,
#                                            n.breaks = 6,
#                                            to.plot = variable)
#         ggsave(plot = ancient.mp.map ,paste(constants$scale.name, var.name, "map.pdf"))
#         
#         # hex map
#         variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
#         
#         ancient.mp.hexmap =  map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#                                          background = NULL,
#                                          fillground = westhex.ATI,
#                                          transformation = "log10",
#                                          n.breaks = 6,
#                                          to.plot = variable)
#         
#         ggsave(plot = ancient.mp.hexmap, paste(constants$scale.name,var.name, "hexmap.pdf"))
#         
#       #### Veteran tree maps -----
#     var.name = "Veteran.count"
#     main.title = "Veteran tree records"
#     sub.title = "2017 Westminster constituencies"
#     fill.scale.title = "Veteran tree records"
#     colour.limits = c(0,max(MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]))
#     transformation = "log10"
#     var.scale.factor = 1
#     
#         # boundry map
#         variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] / var.scale.factor
#         
#         vet.mp.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#                                        background = countries, fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
#                                        transformation = transformation,
#                                        n.breaks = 6,
#                                        to.plot = variable)
#         ggsave(plot = vet.mp.map ,paste(constants$scale.name, var.name, "map.pdf"))
#         
#         # hex map
#         variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
#         
#         vet.mp.hexmap =  map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#                                          background = NULL,
#                                          fillground = westhex.ATI,
#                                          transformation = "log10",
#                                          n.breaks = 6,
#                                          to.plot = variable)
#         
#         ggsave(plot = vet.mp.hexmap, paste(constants$scale.name,var.name, "hexmap.pdf"))
#         
#     ### DENSITY MAPS ----
#       #### ATI records ----
#         var.name = "ATI.density"
#         main.title = expression(paste("ATI records per km"^"2",sep = ""))
#         sub.title = "2017 Westminster constituencies"
#         fill.scale.title = expression(paste("ATI records per km"^"2",sep = ""))
#         colour.limits = c(0,5)
#         var.scale.factor = 1
#         
#         # boundry map
#         variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] / var.scale.factor
# 
#         totdens.mp.map = map.ploter.ident (
#           fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#           background = countries,
#           fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
#           col.limits= colour.limits , 
#           to.plot = variable,
#           clr.breaks = colour.brks(lims = colour.limits),
#           clr.labels = colour.lable(x = variable ,
#                                     lims = colour.limits , 
#                                     breaks = colour.brks(colour.limits ), var.scale.factor = var.scale.factor))
#         ggsave(plot = totdens.mp.map , paste(constants$scale.name, var.name, "map.pdf"))
#         
#         
#         # hex map
#         variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
#         
#         totdens.mp.hexmap =  map.ploter.ident(
#           fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#           background = NULL,
#           fillground = westhex.ATI,
#           to.plot = variable,
#           col.limits= colour.limits , 
#           clr.breaks = colour.brks(lims = colour.limits),
#           clr.labels = colour.lable(x = variable ,
#                                     lims = colour.limits , 
#                                     breaks = colour.brks(colour.limits ), var.scale.factor = var.scale.factor))
#         ggsave(plot = totdens.mp.hexmap, paste(constants$scale.name,var.name, "hexmap.pdf"))
#         
#         
#       #### Ancient records ----
#     var.name = "ancient.density"
#     main.title = expression(paste("Ancient tree records per km"^"2",sep = ""))
#     sub.title = "2017 Westminster constituencies"
#     fill.scale.title = expression(paste("Ancient tree records per km"^"2",sep = ""))
#     colour.limits = c(0,5)
#     var.scale.factor = 1
#         # boundry map
#         variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] / var.scale.factor
# 
#         ancientdens.mp.map = map.ploter.ident (
#           fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#           background = countries,
#           fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
#           col.limits= colour.limits , 
#           to.plot = variable,
#           clr.breaks = colour.brks(lims = colour.limits),
#           clr.labels = colour.lable(x = variable ,
#                                     lims = colour.limits , 
#                                     breaks = colour.brks(colour.limits ), var.scale.factor = var.scale.factor))
#         ggsave(plot = ancientdens.mp.map , paste(constants$scale.name, var.name, "map.pdf"))
#         
#         # hex map
#         variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
#         
#         ancientdens.mp.hexmap =  map.ploter.ident(
#           fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#           background = NULL,
#           fillground = westhex.ATI,
#           to.plot = variable,
#           col.limits= colour.limits , 
#           clr.breaks = colour.brks(lims = colour.limits),
#           clr.labels = colour.lable(x = variable ,
#                                     lims = colour.limits , 
#                                     breaks = colour.brks(colour.limits ), var.scale.factor = var.scale.factor))
#         ggsave(plot = ancientdens.mp.hexmap, paste(constants$scale.name,var.name, "hexmap.pdf"))
#         
#       #### veteran records ----
#     var.name = "veteran.density"
#     main.title = expression(paste("Veteran tree records per km"^"2",sep = ""))
#     sub.title = "2017 Westminster constituencies"
#     fill.scale.title = expression(paste("Veteran tree records per km"^"2",sep = ""))
#     colour.limits = c(0, 5)
#     var.scale.factor = 1
#         # boundry map
#         variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] / var.scale.factor
#         
#         vetdens.mp.map = map.ploter.ident (
#           fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#           background = countries,
#           fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
#           col.limits= colour.limits , 
#           to.plot = variable,
#           clr.breaks = colour.brks(lims = colour.limits),
#           clr.labels = colour.lable(x = variable ,
#                                     lims = colour.limits , 
#                                     breaks = colour.brks(colour.limits ), var.scale.factor = var.scale.factor))
#         ggsave(plot = vetdens.mp.map , paste(constants$scale.name, var.name, "map.pdf"))
#         
#         # hex map
#         variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
#         
#         vetdens.mp.hexmap =  map.ploter.ident(
#           fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
#           background = NULL,
#           fillground = westhex.ATI,
#           to.plot = variable,
#           col.limits= colour.limits , 
#           clr.breaks = colour.brks(lims = colour.limits),
#           clr.labels = colour.lable(x = variable ,
#                                     lims = colour.limits , 
#                                     breaks = colour.brks(colour.limits ), var.scale.factor = var.scale.factor))
#         ggsave(plot = vetdens.mp.hexmap, paste(constants$scale.name,var.name, "hexmap.pdf"))
#         
#         
# 
