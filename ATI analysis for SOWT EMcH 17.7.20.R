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

# load data ----
# ati taken form cuation code, nnjoined with Awi and NFI in QGIS where that processing is quicker
load(paste0(gis.wd, "\\Data\\ATI\\ati2024-09-10curated_fromATI - 2024_08_05.csv.RData"))
countries = st_read("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\Data\\administrative boundaries\\Countries\\5countries.v1.05.shp") %>% 
  st_transform(27700) %>% 
  st_buffer(0)


# configure ----
constants <- list(hexdist.h = 10000,
                  hexdist.v = 10000
)

# curation ----

# add countries area
countries$area.ha <-  st_area(countries) %>% 
  set_units(value = "ha") %>%
  as.numeric()
countries <- countries %>% 
  rename(country = ctry17nm) %>%
  mutate(country = factor(country, levels = sort(unique(country))))

# hex grid
hex.grid0 = st_make_grid(countries %>% st()
                         , c(constants$hexdist.h, constants$hexdist.v), what = "polygons", square = F)
hex.grid = st_sf(hex.grid0) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(hex.grid0)))
rm(hex.grid0)
hex.grid$hex.ha <-  st_area(hex.grid) %>% 
  set_units(value = "ha") %>%
  as.numeric()


#convert ati crs
ati <- ati %>% st_transform(27700)

# TABLES OF INFO ----
tree_in_country <- st_join(ati, countries)
  ## t country - vet status
table(tree_in_country$VeteranStatusName, tree_in_country$country) %>%
  addmargins(margin = c(1,2)) 

table(tree_in_country$VeteranStatusName, tree_in_country$country) %>%
  prop.table() %>%
  addmargins() %>% 
  round(3) 
  

## t country - form
create_ordered_table(tree_in_country, "TreeFormName", "country", type = "frequency", round_digits = 0)

create_ordered_table(tree_in_country, "TreeFormName", "country", type = "proportion", round_digits = 5) %>% 
  replace_rounded_zeros_with_string(3)

## t vet status - form
create_ordered_table(tree_in_country, "TreeFormName", "VeteranStatusName", type = "frequency")

create_ordered_table(tree_in_country, "TreeFormName", "VeteranStatusName", type = "proportion", round_digits = 5) %>% 
  replace_rounded_zeros_with_string(3)

## t vet status - species
create_ordered_table(tree_in_country, "SpeciesName", "VeteranStatusName", type = "frequency")

create_ordered_table(tree_in_country, "SpeciesName", "VeteranStatusName", type = "proportion", round_digits = 5) %>% 
  replace_rounded_zeros_with_string(3)

## t vet status - standing
create_ordered_table(tree_in_country, "StandingStatusName", "VeteranStatusName", type = "frequency")

create_ordered_table(tree_in_country, "StandingStatusName", "VeteranStatusName", type = "proportion", round_digits = 5) %>% 
  replace_rounded_zeros_with_string(3)

# ati record density - country

# t - AVT type density by country -  per 100 km2, a 10 x 10 km hectad
T_avt_dens <- table(tree_in_country$VeteranStatusName, tree_in_country$country) %>% 
  addmargins(margin = c(1,2)) %>% 
  sweep(2, c(countries$area.ha, sum(countries$area.ha)), "/")*10000 
colnames(T_avt_dens)[ncol(T_avt_dens)] <- "All countries combined"
rownames(T_avt_dens)[nrow(T_avt_dens)] <- "All AVT types combined"

replace_rounded_zeros_with_string(T_avt_dens, 2)

# Records MAPS ----
ggplot(data = countries) +
  geom_sf() +
  geom_sf(data = ati, size = 1, shape = 23, fill = "darkred") 

# HEX MAPS ----
  ## hex map table - Ancient, vet and lost ----

tree_in_maphex <- st_join(ati, hexes.for.plot) # join layers
tree_in_maphex$id = factor(tree_in_maphex$id, levels =  unique(hexes.for.plot$id  )) # make factor so that consts with 0 still included
#tables of lost and remaining and combine
maphex.ther.table = addmargins(table(tree_in_maphex$id[tree_in_maphex$lost==0], tree_in_maphex$ancient[tree_in_maphex$lost==0]), margin = 2)
maphex.lost.table = addmargins(table(tree_in_maphex$id[tree_in_maphex$lost==1], tree_in_maphex$ancient[tree_in_maphex$lost==1]), margin = 2)
colnames(maphex.ther.table) = c("Ancient", "Veteran", "Tot")
colnames(maphex.lost.table) = paste(colnames(maphex.ther.table), "lost", sep = ".")
maphex.table = as.tibble( cbind ( rownames(maphex.ther.table) , maphex.ther.table , maphex.lost.table ))
maphex.table[, colnames(maphex.table) %in% c(colnames(maphex.ther.table), colnames(maphex.lost.table))] <- 
  sapply(maphex.table[, colnames(maphex.table) %in% c(colnames(maphex.ther.table), colnames(maphex.lost.table))], as.integer)
# proportion lost - not convinced by this, needs checking and the names later as well if added
# maphex.table$Prop.ancient.lost = maphex.table$Ancient.lost/ sum(maphex.table$Ancient.lost, maphex.table$Ancient) *10^6
# maphex.table$Prop.veterans.lost = maphex.table$Veteran.lost/ sum(maphex.table$Veteran.lost, maphex.table$Veteran) *10^6
# maphex.table$Prop.total.lost = maphex.table$Tot.lost/ sum(maphex.table$Tot.lost, maphex.table$Tot) *10^6

# area of consitituency in km sq
maphex.table = merge(hexes.for.plot , maphex.table, by.x = "id", by.y =  "V1" )
maphex.table$area = maphex.table$hex.area / 1000000
maphex.table = st_set_geometry(maphex.table, NULL)

# # gss code
# maphex.table$gss_code = merge(as.data.frame(id) , as.data.frame(maphex.table) , by.x = "id" , by.y =  "V1" )$pcon17cd

# density of ATs per 
maphex.table$Ancient.dens = maphex.table$Ancient/maphex.table$area
maphex.table$vet.dens = maphex.table$Veteran/maphex.table$area
maphex.table$tot.dens = maphex.table$Tot/maphex.table$area

# name and rearrange coloums
colnames(maphex.table) = c( "id"  , colnames(maphex.table)[2:6], 
                        "Ancient.count", "Veteran.count" , "ATI.count" , 
                        "ancient.lost", "Veterans.lost", "Total.lost", "area" , #"Ancient.loss.per.Million",  "Veteran.loss.per.Million", "Total.loss.per.Million",
                        "ancient.density", "veteran.density", "ATI.density")
maphex.table = maphex.table [, c("id", "area", 
                         "Ancient.count", "ancient.density", "Veteran.count", "veteran.density", "ATI.count", "ATI.density", 
                         "ancient.lost",#"Ancient.loss.per.Million", 
                         "Veterans.lost", # "Veteran.loss.per.Million",
                         "Total.lost"#, "Total.loss.per.Million"
                         )]
write.csv(maphex.table,file = "maphex.table.csv" )

  ## HEX MAP HEATMAPS ----
hex.ATI.shp = merge(hexes.for.plot, maphex.table , by.x = "id", by.y = "id")
scale.name = "Hex"
### COUNT HEX MAPS ----
#### ATI TOTAL COUNT HEXMAPS ----
var.name = "ATI.count"
main.title = "ATI records"
sub.title = NULL
fill.scale.title = expression(paste("Ancient and veteran tree records per km"^"2",sep = ""))
colour.limits = c(0,1000)
transformation = "log10"
dividor = 1

hex.ATI.shp$area2 = hex.ATI.shp$area
hex.ATI.shp$area2[hex.ATI.shp$area2 <0.1 ] = 0.1
variable = (hex.ATI.shp[[which(colnames(hex.ATI.shp) ==var.name)]]/dividor)/hex.ATI.shp$area

ati.hex.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                background = countries, fillground = hex.ATI.shp, fillground2 = NULL,
                                transformation = transformation, 
                                n.breaks = 6,
                                to.plot = variable)

ati.hex.map =   ggplot() +
  geom_sf(data = countries, size = 0.2) +
  geom_sf(data = hex.ATI.shp, mapping = aes(fill = variable  ), colour = NA) +
  # geom_sf(data = fillground2, fill = NA, size = 0.03, colour = "grey90") +
  scale_fill_viridis_c( trans = transformation, 
                        name = fill.scale.title,
                        n.breaks = 4,
                        #option = "magma",direction = -1 
                        guide = guide_colorbar(
                          direction = "horizontal", barheight = unit(2, units = "mm"),
                          barwidth = unit(50, units = "mm"), draw.ulim = F,
                          title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
  labs(x = NULL, y = NULL , title = main.title, subtitle = NULL#, caption = ""
  )+
  theme_map() +
  theme(legend.position = "bottom") 




ggsave(plot = ati.hex.map ,paste(scale.name, var.name, "map.pdf"), height = 5, width = 3.5)
ggsave(plot = ati.hex.map ,paste(scale.name, var.name, "map.svg"), height = 5, width = 3.5)

##### alt ----
# bespoke for SoWT
# drawing hap-hazrd from from https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# messing up my tidy code, but such is life.. its only me who'll ever have to use it again anyway
# ... sorry future Ewan :(  feel free to just hack this bit out
var.name = "ATI.count"
main.title = "Total ATI records"
sub.title = "per 10km high Hexagon"
fill.scale.title = "ATI records"
colour.limits = c(0,1000)
transformation = "identity"
dividor = 1

variable = hex.ATI.shp[[which(colnames(hex.ATI.shp) == var.name)]] / dividor


labels <- c()
quantiles <- c(0,1,3,10,100,max(variable))
no_classes <- length (quantiles)
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1]

# here I actually create a new 
# variable on the dataset with the quantiles
variable02 <- cut(variable, 
                  breaks = quantiles, 
                  labels = labels, 
                  include.lowest = T)

clr.breaks = colour.brks(lims = colour.limits)
clr.labels = colour.lable(x = variable ,
                          lims = colour.limits , 
                          breaks = colour.brks(colour.limits ),
                          dividor = 1)

sowt.plot = ggplot() +
  geom_sf(data = countries, size = 0.2) +
  geom_sf(data = hex.ATI.shp, mapping = aes(fill = variable02  ), colour = NA) +
  geom_sf(data = hex.ATI.shp, fill = NA, size = 0.05, colour = "grey90") +
  scale_fill_viridis( name = fill.scale.title,
                      discrete = T,
                      guide = guide_legend(
                        keyheight = unit(5, units = "mm"),
                        title.position = 'top')
                      # guide = guide_colorbar(
                      #   direction = "horizontal", barheight = unit(2, units = "mm"),
                      #   barwidth = unit(50, units = "mm"), draw.ulim = F,
                      #   title.position = 'top', title.hjust = 0.5, label.hjust = 0.5)
  )+
  labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
  )+
  theme_map() +
  theme(legend.position = "bottom") 

sowt.plot = ggplot() +
  geom_sf(data = countries, size = 0.2) +
  geom_sf(data = hex.ATI.shp, mapping = aes(fill = variable02  ), colour = NA) +
  geom_sf(data = hex.ATI.shp, fill = NA, size = 0.05, colour = "grey90") +
  labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
  )+
  theme_map() +
  theme(legend.position = "bottom") +
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(viridis(6)),
    breaks = rev(brks_scale),
    name = "Average age",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  )
ggsave(plot = sowt.plot ,paste(scale.name, var.name, "sowt.plot01","map.pdf"), height = 5, width = 5)



#### ANCIENT COUNT HEXMAPS ---- 
var.name = "Ancient.count"
main.title = "Ancient tree records"
sub.title = NULL
fill.scale.title = "Ancient tree records"
colour.limits = c(0,max(hex.ATI.shp[[which(colnames(hex.ATI.shp) ==var.name)]]))
transformation = "log10"
dividor = 1

variable = hex.ATI.shp[[which(colnames(hex.ATI.shp) ==var.name)]]/ dividor

ancient.hex.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                    background = countries, fillground = hex.ATI.shp, fillground2 = hex.ATI.shp,
                                    transformation = transformation,
                                    n.breaks = 6,
                                    to.plot = variable)
ggsave(plot = ancient.hex.map ,paste(scale.name, var.name, "map.pdf"))

#### VET COUNT HEXMAPS -----
var.name = "Veteran.count"
main.title = "Veteran tree records"
sub.title = "2017 Westminster constituencies"
fill.scale.title = "Veteran tree records"
colour.limits = c(0,max(hex.ATI.shp[[which(colnames(hex.ATI.shp) ==var.name)]]))
transformation = "log10"
dividor = 1

variable = hex.ATI.shp[[which(colnames(hex.ATI.shp) ==var.name)]] / dividor

vet.hex.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                background = countries, fillground = hex.ATI.shp, fillground2 = hex.ATI.shp,
                                transformation = transformation,
                                n.breaks = 6,
                                to.plot = variable)
ggsave(plot = vet.hex.map ,paste(scale.name, var.name, "map.pdf"))



### DENSITY MAPS ----
#### ATI records DENS HEXMAP ----
var.name = "ATI.density"
main.title = expression(paste("ATI records per km"^"2",sep = ""))
sub.title = NULL
fill.scale.title = expression(paste("ATI records per km"^"2",sep = ""))
colour.limits = c(0,2)
dividor = 1

variable = hex.ATI.shp[[which(colnames(hex.ATI.shp) ==var.name)]] / dividor

totdens.hex.map = map.ploter.ident (
  fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
  background = countries,
  fillground = hex.ATI.shp, fillground2 = hex.ATI.shp,
  col.limits= colour.limits , 
  to.plot = variable,
  clr.breaks = colour.brks(lims = colour.limits),
  clr.labels = colour.lable(x = variable ,
                            lims = colour.limits , 
                            breaks = colour.brks(colour.limits ), dividor = dividor))
ggsave(plot = totdens.hex.map , paste(scale.name, var.name, "map1.pdf"), height = 5, width = 5)


#### Ancient DENS HEXMAP ----
var.name = "ancient.density"
main.title = expression(paste("Ancient tree records per km"^"2",sep = ""))
sub.title = NULL
fill.scale.title = expression(paste("Ancient tree records per km"^"2",sep = ""))
colour.limits = c(0,5)
dividor = 1

variable = hex.ATI.shp[[which(colnames(hex.ATI.shp) ==var.name)]] / dividor

ancientdens.hex.map = map.ploter.ident (
  fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
  background = countries,
  fillground = hex.ATI.shp, fillground2 = hex.ATI.shp,
  col.limits= colour.limits , 
  to.plot = variable,
  clr.breaks = colour.brks(lims = colour.limits),
  clr.labels = colour.lable(x = variable ,
                            lims = colour.limits , 
                            breaks = colour.brks(colour.limits ), dividor = dividor))
ggsave(plot = ancientdens.hex.map , paste(scale.name, var.name, "map.pdf"))

#### VET DENS HEXMAP ----
var.name = "veteran.density"
main.title = expression(paste("Veteran tree records per km"^"2",sep = ""))
sub.title = NULL
fill.scale.title = expression(paste("Veteran tree records per km"^"2",sep = ""))
colour.limits = c(0, 5)
dividor = 1

variable = hex.ATI.shp[[which(colnames(hex.ATI.shp) ==var.name)]] / dividor

vetdens.hex.map = map.ploter.ident (
  fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
  background = countries,
  fillground = hex.ATI.shp, fillground2 = hex.ATI.shp,
  col.limits= colour.limits , 
  to.plot = variable,
  clr.breaks = colour.brks(lims = colour.limits),
  clr.labels = colour.lable(x = variable ,
                            lims = colour.limits , 
                            breaks = colour.brks(colour.limits ), dividor = dividor))
ggsave(plot = vetdens.hex.map , paste(scale.name, var.name, "map.pdf"))


# MP consituency ATI analysis ----
## MP constituency - Ancient, vet and lost tables ----

tree_in_MP <- st_join(ati, westminster.const) # join layers
tree_in_MP$pcon17nm = factor(tree_in_MP$pcon17nm, levels =  unique(westminster.const$pcon17nm  )) # make factor so that consts with 0 still included
#tables of lost and remaining and combine
MP.ther.table = addmargins(table(tree_in_MP$pcon17nm[tree_in_MP$lost==0], tree_in_MP$ancient[tree_in_MP$lost==0]), margin = 2)
MP.lost.table = addmargins(table(tree_in_MP$pcon17nm[tree_in_MP$lost==1], tree_in_MP$ancient[tree_in_MP$lost==1]), margin = 2)
colnames(MP.ther.table) = c("Ancient", "Veteran", "Tot")
colnames(MP.lost.table) = paste(colnames(MP.ther.table), "lost", sep = ".")
MP.table = as.tibble( cbind ( rownames(MP.ther.table) , MP.ther.table , MP.lost.table ))
MP.table[, 2:7] <- sapply(MP.table[, 2:7], as.integer)
# proportion lost
MP.table$Prop.ancient.lost = MP.table$Ancient.lost/ sum(MP.table$Ancient.lost, MP.table$Ancient) *10^6
MP.table$Prop.veterans.lost = MP.table$Veteran.lost/ sum(MP.table$Veteran.lost, MP.table$Veteran) *10^6
MP.table$Prop.total.lost = MP.table$Tot.lost/ sum(MP.table$Tot.lost, MP.table$Tot) *10^6

# area of consitituency in km sq
MP.table = merge(westminster.const , MP.table , by.y =  "V1" , by.x = "pcon17nm" )
MP.table$area = MP.table$area / 1000000
MP.table = st_set_geometry(MP.table, NULL)

# # gss code
# MP.table$gss_code = merge(as.data.frame(westminster.const) , as.data.frame(MP.table) , by.x = "pcon17nm" , by.y =  "V1" )$pcon17cd

# density of ATs per 
MP.table$Ancient.dens = MP.table$Ancient/MP.table$area
MP.table$vet.dens = MP.table$Veteran/MP.table$area
MP.table$tot.dens = MP.table$Tot/MP.table$area

# name and rearrange coloums
colnames(MP.table) = c( "Constituency"  , colnames(MP.table)[2:9], "area", 
                        "Ancient.count", "Veteran.count" , "ATI.count" , 
                        "ancient.lost", "Veterans.lost", "Total.lost", "Ancient.loss.per.Million",  "Veteran.loss.per.Million", "Total.loss.per.Million",
                        "ancient.density", "veteran.density", "ATI.density")
MP.table = MP.table [, c("Constituency", "area", 
                         "Ancient.count", "ancient.density", "Veteran.count", "veteran.density", "ATI.count", "ATI.density", 
                         "ancient.lost","Ancient.loss.per.Million", "Veterans.lost", "Veteran.loss.per.Million", "Total.lost", "Total.loss.per.Million")]
write.csv(MP.table,file = "MP.table.csv" )

## MP constituency heat maps-----
  MP.ATI.shp = merge(westminster.const, MP.table , by.x = "pcon17nm", by.y = "Constituency")
  
  west_hex_map$constituency_name[west_hex_map$constituency_name =="Fermanagh & South Tyrone" ] = "Fermanagh and South Tyrone"   
  west_hex_map$constituency_name[west_hex_map$constituency_name =="Newry & Armagh" ] = "Newry and Armagh"   
  
  westhex.ATI = merge(west_hex_map, MP.table , by.x = "constituency_name", by.y = "Constituency")
  scale.name = "MP"
  
    ### COUNT MAPS ----
      #### total ATI records ---- 
    var.name = "ATI.count"
    main.title = "Total ATI records"
    sub.title = "2017 Westminster constituencies"
    fill.scale.title = "ATI records"
    colour.limits = c(0,max(MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]))
    transformation = "log10"
    dividor = 1
    
        # boundry map
    variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]/dividor
    
    ati.mp.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                    background = countries, fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
                    transformation = transformation, 
                    n.breaks = 6,
                    to.plot = variable)
    ggsave(plot = ati.mp.map ,paste(scale.name, var.name, "map.pdf"))
    ggsave(plot = ati.mp.map ,paste(scale.name, var.name, "map.svg"))
    
        # hex map
    variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]/dividor    
    
    tot.mp.hexmap =  map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                    background = NULL,
                    fillground = westhex.ATI,
                    transformation = "log10",
                    n.breaks = 6,
                    to.plot = variable)
    
    ggsave(plot = tot.mp.hexmap, paste(scale.name,var.name, "hexmap.pdf"))
    
      #### Ancient Tree records ---- 
    var.name = "Ancient.count"
    main.title = "Ancient tree records"
    sub.title = "2017 Westminster constituencies"
    fill.scale.title = "Ancient tree records"
    colour.limits = c(0,max(MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]))
    transformation = "log10"
    dividor = 1
    
        # boundry map
        variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]/ dividor
        
        ancient.mp.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                           background = countries, fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
                                           transformation = transformation,
                                           n.breaks = 6,
                                           to.plot = variable)
        ggsave(plot = ancient.mp.map ,paste(scale.name, var.name, "map.pdf"))
        
        # hex map
        variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
        
        ancient.mp.hexmap =  map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                         background = NULL,
                                         fillground = westhex.ATI,
                                         transformation = "log10",
                                         n.breaks = 6,
                                         to.plot = variable)
        
        ggsave(plot = ancient.mp.hexmap, paste(scale.name,var.name, "hexmap.pdf"))
        
      #### Veteran tree maps -----
    var.name = "Veteran.count"
    main.title = "Veteran tree records"
    sub.title = "2017 Westminster constituencies"
    fill.scale.title = "Veteran tree records"
    colour.limits = c(0,max(MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]]))
    transformation = "log10"
    dividor = 1
    
        # boundry map
        variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] / dividor
        
        vet.mp.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                       background = countries, fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
                                       transformation = transformation,
                                       n.breaks = 6,
                                       to.plot = variable)
        ggsave(plot = vet.mp.map ,paste(scale.name, var.name, "map.pdf"))
        
        # hex map
        variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
        
        vet.mp.hexmap =  map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                         background = NULL,
                                         fillground = westhex.ATI,
                                         transformation = "log10",
                                         n.breaks = 6,
                                         to.plot = variable)
        
        ggsave(plot = vet.mp.hexmap, paste(scale.name,var.name, "hexmap.pdf"))
        
    ### DENSITY MAPS ----
      #### ATI records ----
        var.name = "ATI.density"
        main.title = expression(paste("ATI records per km"^"2",sep = ""))
        sub.title = "2017 Westminster constituencies"
        fill.scale.title = expression(paste("ATI records per km"^"2",sep = ""))
        colour.limits = c(0,5)
        dividor = 1
        
        # boundry map
        variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] / dividor

        totdens.mp.map = map.ploter.ident (
          fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
          background = countries,
          fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
          col.limits= colour.limits , 
          to.plot = variable,
          clr.breaks = colour.brks(lims = colour.limits),
          clr.labels = colour.lable(x = variable ,
                                    lims = colour.limits , 
                                    breaks = colour.brks(colour.limits ), dividor = dividor))
        ggsave(plot = totdens.mp.map , paste(scale.name, var.name, "map.pdf"))
        
        
        # hex map
        variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
        
        totdens.mp.hexmap =  map.ploter.ident(
          fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
          background = NULL,
          fillground = westhex.ATI,
          to.plot = variable,
          col.limits= colour.limits , 
          clr.breaks = colour.brks(lims = colour.limits),
          clr.labels = colour.lable(x = variable ,
                                    lims = colour.limits , 
                                    breaks = colour.brks(colour.limits ), dividor = dividor))
        ggsave(plot = totdens.mp.hexmap, paste(scale.name,var.name, "hexmap.pdf"))
        
        
      #### Ancient records ----
    var.name = "ancient.density"
    main.title = expression(paste("Ancient tree records per km"^"2",sep = ""))
    sub.title = "2017 Westminster constituencies"
    fill.scale.title = expression(paste("Ancient tree records per km"^"2",sep = ""))
    colour.limits = c(0,5)
    dividor = 1
        # boundry map
        variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] / dividor

        ancientdens.mp.map = map.ploter.ident (
          fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
          background = countries,
          fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
          col.limits= colour.limits , 
          to.plot = variable,
          clr.breaks = colour.brks(lims = colour.limits),
          clr.labels = colour.lable(x = variable ,
                                    lims = colour.limits , 
                                    breaks = colour.brks(colour.limits ), dividor = dividor))
        ggsave(plot = ancientdens.mp.map , paste(scale.name, var.name, "map.pdf"))
        
        # hex map
        variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
        
        ancientdens.mp.hexmap =  map.ploter.ident(
          fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
          background = NULL,
          fillground = westhex.ATI,
          to.plot = variable,
          col.limits= colour.limits , 
          clr.breaks = colour.brks(lims = colour.limits),
          clr.labels = colour.lable(x = variable ,
                                    lims = colour.limits , 
                                    breaks = colour.brks(colour.limits ), dividor = dividor))
        ggsave(plot = ancientdens.mp.hexmap, paste(scale.name,var.name, "hexmap.pdf"))
        
      #### veteran records ----
    var.name = "veteran.density"
    main.title = expression(paste("Veteran tree records per km"^"2",sep = ""))
    sub.title = "2017 Westminster constituencies"
    fill.scale.title = expression(paste("Veteran tree records per km"^"2",sep = ""))
    colour.limits = c(0, 5)
    dividor = 1
        # boundry map
        variable = MP.ATI.shp[[which(colnames(MP.ATI.shp) ==var.name)]] / dividor
        
        vetdens.mp.map = map.ploter.ident (
          fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
          background = countries,
          fillground = MP.ATI.shp, fillground2 = MP.ATI.shp,
          col.limits= colour.limits , 
          to.plot = variable,
          clr.breaks = colour.brks(lims = colour.limits),
          clr.labels = colour.lable(x = variable ,
                                    lims = colour.limits , 
                                    breaks = colour.brks(colour.limits ), dividor = dividor))
        ggsave(plot = vetdens.mp.map , paste(scale.name, var.name, "map.pdf"))
        
        # hex map
        variable = westhex.ATI[[which(colnames(westhex.ATI) ==var.name)]]    
        
        vetdens.mp.hexmap =  map.ploter.ident(
          fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
          background = NULL,
          fillground = westhex.ATI,
          to.plot = variable,
          col.limits= colour.limits , 
          clr.breaks = colour.brks(lims = colour.limits),
          clr.labels = colour.lable(x = variable ,
                                    lims = colour.limits , 
                                    breaks = colour.brks(colour.limits ), dividor = dividor))
        ggsave(plot = vetdens.mp.hexmap, paste(scale.name,var.name, "hexmap.pdf"))
        
        

# COUNTRY MAPS ----
COUNTRY.ATI.shp = merge(countries, country.table, by.x = "ctry17nm", by.y = "Country")
scale.name = "country"
ati.pretty.max = max(pretty(max(COUNTRY.ATI.shp[[which(colnames(COUNTRY.ATI.shp) =="ATI.count")]])))

## COUNT COUNTRY MAPS ----
### ATI COUNT COUNTRY MAPS ----
var.name = "ATI.count"
main.title = "ATI records"
sub.title = "By country"
fill.scale.title = "ATI records (thousands)"
colour.limits = c(0,ati.pretty.max)
transformation = "log10"
dividor = 1000

variable = COUNTRY.ATI.shp[[which(colnames(COUNTRY.ATI.shp) ==var.name)]]/dividor

tot.count.cntry.map = map.ploter.log(fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                   background = countries, fillground = COUNTRY.ATI.shp , fillground2 = COUNTRY.ATI.shp,
                                   transformation = transformation,
                                   to.plot = variable,
                                   n.breaks = 6)
ggsave(plot = tot.count.cntry.map ,paste(scale.name, var.name, "map.pdf"))

### ANCIENT COUNT COUNTRY MAPS ----
var.name = "Ancient.count"
main.title = "Ancient tree records"
sub.title = "By country"
fill.scale.title = "Ancient tree records (thousands)"
colour.limits = c(0.000001,ati.pretty.max)
transformation = "log10"
dividor = 1000

variable = COUNTRY.ATI.shp[[which(colnames(COUNTRY.ATI.shp) ==var.name)]]/dividor

ancient.count.cntry.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                        background = countries, fillground = COUNTRY.ATI.shp , fillground2 = COUNTRY.ATI.shp,
                                        transformation = transformation,
                                        to.plot = variable,
                                        n.breaks = 6)
ggsave(plot = ancient.count.cntry.map ,paste(scale.name, var.name, "map.pdf"))

### VET COUNT COUNTRY MAPS ----
var.name = "Veteran.count"
main.title = "Veteran tree records"
sub.title = "By country"
fill.scale.title = "Veteran tree records (thousands)"
colour.limits = c(0,max(COUNTRY.ATI.shp[[which(colnames(COUNTRY.ATI.shp) ==var.name)]]))
transformation = "log10"
dividor = 1000

variable = COUNTRY.ATI.shp[[which(colnames(COUNTRY.ATI.shp) ==var.name)]]/dividor

vet.count.cntry.map =   map.ploter.log( fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
                                            background = countries, fillground = COUNTRY.ATI.shp , fillground2 = COUNTRY.ATI.shp,
                                            transformation = transformation,
                                            to.plot = variable,
                                            n.breaks = 6)
ggsave(plot = vet.count.cntry.map ,paste(scale.name, var.name, "map.pdf"))

## DENSITY COUNTRY MAPS ----
### ATI DENS COUNTRY MAPS ----
var.name = "ATI.density"
main.title = expression(paste("ATI records per km"^"2",sep = ""))
sub.title = "By country"
fill.scale.title = expression(paste("ATI records per km"^"2",sep = ""))
colour.limits = c(0,0.9)
transformation = "identity"
dividor = 1

variable = COUNTRY.ATI.shp[[which(colnames(COUNTRY.ATI.shp) ==var.name)]]/dividor

totdens.country.map = map.ploter.ident (
  fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
  background = countries,
  fillground = COUNTRY.ATI.shp , fillground2 = COUNTRY.ATI.shp,
  col.limits= colour.limits , 
  to.plot = variable,
  clr.breaks = colour.brks(lims = colour.limits),
  clr.labels = colour.lable(x = variable ,
                            lims = colour.limits , 
                            breaks = colour.brks(colour.limits ), dividor = dividor))
ggsave(plot = totdens.country.map , paste(scale.name, var.name, "map.pdf"))

### ANCEINT DENS COUNTRY MAPS ----
var.name = "ancient.density"
main.title = expression(paste("Ancient tree records per km"^"2",sep = ""))
fill.scale.title = expression(paste("Ancient tree records per km"^"2",sep = ""))

sub.title = "By country"
colour.limits = c(0,0.9)
transformation = "identity"
dividor = 1

variable = COUNTRY.ATI.shp[[which(colnames(COUNTRY.ATI.shp) ==var.name)]]/dividor

ancient.dens.cntry.map = map.ploter.ident (
  fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
  background = countries,
  fillground = COUNTRY.ATI.shp , fillground2 = COUNTRY.ATI.shp,
  col.limits= colour.limits , 
  to.plot = variable,
  clr.breaks = colour.brks(lims = colour.limits),
  clr.labels = colour.lable(x = variable ,
                            lims = colour.limits , 
                            breaks = colour.brks(colour.limits ), dividor = dividor))
ggsave(plot = ancient.dens.cntry.map , paste(scale.name, var.name, "map.pdf"))

### VET DENS COUNTRY MAPS ----
var.name = "veteran.density"
main.title = expression(paste("Veteran tree records per km"^"2",sep = ""))
sub.title = "By country"
fill.scale.title = expression(paste("Veteran tree records per km"^"2",sep = ""))
colour.limits = c(0,0.1)
transformation = "identity"
dividor = 1

variable = COUNTRY.ATI.shp[[which(colnames(COUNTRY.ATI.shp) ==var.name)]]/dividor

vet.dens.cntry.map = map.ploter.ident (
  fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
  background = countries,
  fillground = COUNTRY.ATI.shp , fillground2 = COUNTRY.ATI.shp,
  col.limits= colour.limits , 
  to.plot = variable,
  clr.breaks = colour.brks(lims = colour.limits),
  clr.labels = colour.lable(x = variable ,
                            lims = colour.limits , 
                            breaks = colour.brks(colour.limits ), dividor = dividor))
ggsave(plot = vet.dens.cntry.map , paste(scale.name, var.name, "map.pdf"))

