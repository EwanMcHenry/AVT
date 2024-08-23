##------ Thu Aug 24 12:47:50 2023 ------##

# part of project "Bonnie Prince Charlie"

# supporting AVT recording in England

# stratified sampling of 1 km england grid squares 
# re-project VN data to actual GB grid squares
## aera weighted mean prediction

# stratified sampling by a defined project area by same method

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(raster)
library(terra)
# devtools::install_version("velox", version = "0.2.0")
# library(velox)
# devtools::install_github("Pakillo/rgis")
# library(rgis)
library(units)
library(ggpubr)
library(rnaturalearth)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

source("D:/Users/Ewan McHenry/OneDrive - the Woodland Trust/GIS/Ewans functions.R")
source("D:/Users/Ewan McHenry/OneDrive - the Woodland Trust/GIS/Ewans gis specifications.R")

vn.data.wd = "D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\ATI\\AVT\\from VN\\Data and Predictors\\"

vn_results <- st_read("from VN\\ZIResultsNegBin.shp")
ati <- read.csv(paste0(main.wd, "\\ATI\\downloaded data\\ATI records up to 28-07-2022.csv"))

os.grid <- st_read(paste0(gis.wd,"Data\\OSGB_Grids-master\\OSGB_Grids-master\\Shapefile\\OSGB_Grid_1km.shp")) %>% 
  mutate(ha.grid = st_area(.) %>% set_units("ha"))

# curation ----
## ATI curation ----
ati.sf <- st_as_sf(ati, coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(27700)

table(ati$VeteranStatusName)
sum(is.na(ati$VeteranStatusName))

## re-projection via weigthed mean of predictions from original model ----
vn_results$nu_vn_vetcount <- lengths(st_intersects(vn_results, ati.sf %>% 
                                                     filter(VeteranStatusName %in% c("Ancient tree", "Veteran tree"))
                                                   ))

os.grid.reproj <- os.grid %>% 
  st_intersection(vn_results) %>% 
  mutate(ha.inter = st_area(.) %>% set_units("ha"),
         ha.wt.NEGBINC = NEGBINC * ha.inter) %>% 
  as.data.frame() %>% 
  group_by(PLAN_NO) %>% 
  summarise(ha.wt.sum.NEGBINC = sum(ha.wt.NEGBINC),
            ha.grid.sum = sum(ha.inter))

os.grid <- os.grid %>% 
  left_join(os.grid.reproj, by = "PLAN_NO") %>%
  mutate(ha.wt.mean.NEGBINC = as.numeric(ha.wt.sum.NEGBINC/ha.grid.sum)) %>% 
  filter(!is.na(ha.wt.mean.NEGBINC))

st_write(os.grid, "from VN\\reprojection\\VN_osgrid00.GeoJSON", )
save(os.grid, file = "from VN\\reprojection\\reprojected.VN.osgrid.RData")

# exploration ----

improv.mod <- glm(nu_vn_vetcount~NEGBINC, data = vn_results %>% 
                    as.data.frame() %>% 
                    filter(nu_vn_vetcount >0) %>% # fit a model jsut for where has been recorded
                    mutate(nu_vn_vetcount = ifelse(nu_vn_vetcount > 10, 10, nu_vn_vetcount),
                           NEGBINC = ifelse(NEGBINC > 10, 10, NEGBINC)) , family = "poisson")
vn_results$improv.pred = predict(improv.mod, vn_results, "response") # fit to all, includign 0 record places

vn_results %>% 
  as.data.frame()  %>% 
  mutate(nu_vn_vetcount = ifelse(nu_vn_vetcount > 10, 10, nu_vn_vetcount),
         NEGBINC = ifelse(NEGBINC > 10, 10, NEGBINC),
         improv.pred = ifelse(improv.pred > 10, 10, improv.pred)) %>% 
  ggplot() +
  geom_point(aes(y = nu_vn_vetcount, x = NEGBINC), alpha = 0.2) +
  geom_line(aes(y = improv.pred, x = NEGBINC), colour = "red") +
  theme_pubr()

# stratification ----

## england stratification ----
### strata specification ----
samp.size = 100
bonus.sample = 50
os.grid$eng.strata1 = NA
quantile(os.grid$ha.wt.mean.NEGBINC, probs = c(0.5,0.75,0.875, 0.9375))
thresholds <- quantile(os.grid$ha.wt.mean.NEGBINC, probs = c(0.5,0.75,0.875, 0.9375)) %>% as.numeric()
# strata level 1:  
os.grid$eng.strata1[os.grid$ha.wt.mean.NEGBINC <= thresholds[1]] = 1
# strata level 2:
os.grid$eng.strata1[os.grid$ha.wt.mean.NEGBINC <= thresholds[2] & os.grid$ha.wt.mean.NEGBINC > thresholds[1]] = 2
# strata level 3:
os.grid$eng.strata1[os.grid$ha.wt.mean.NEGBINC <= thresholds[3] & os.grid$ha.wt.mean.NEGBINC > thresholds[2]] = 3
# strata level 4:
os.grid$eng.strata1[os.grid$ha.wt.mean.NEGBINC <= thresholds[4] & os.grid$ha.wt.mean.NEGBINC > thresholds[3]] = 4
# strata level 5:
os.grid$eng.strata1[os.grid$ha.wt.mean.NEGBINC > thresholds[4]] = 5


os.grid$eng.strata1 <- as.factor(os.grid$eng.strata1)
table(os.grid$strata1[os.grid$sample.priority<samp.size], os.grid$strata2[os.grid$sample.priority<100]) %>% print()

### Strata Map ----

map.strata1 <- map_cat(grid = os.grid %>% st_simplify(dTolerance = 100),
                       var.name = "eng.strata1",
                       main.title = "Stratafication ",
                       sub.title = "5 strata based on predictions from Nolan model",
                       fill.scale.title = "Strata")
image.scale = 1
ggsave(map.strata1, filename = paste0("Sampling design\\England_NolanOS_strata.png"),
       height = 10.80*image.scale , width = 8.20*image.scale, units = "in")

### sampling ----
os.grid = os.grid %>%
  group_by(eng.strata1) %>%
  mutate(strata.count = length(eng.strata1)) %>% 
  mutate(samp.prob = 1/strata.count)

# randomly order grid IDs, weighted based on sampling probability form stratification
set.seed(10025)
os.grid$england.sample.priority = order(sample(os.grid$PLAN_NO, 
                                       size = length(os.grid$PLAN_NO),
                                       replace = F,
                                       prob = os.grid$samp.prob))

# select primary sample size

# Map England sample ----

# CUT OUT SAMPLED CELLS AND MAP
os.grid$samp.run = NA
os.grid$samp.run[os.grid$england.sample.priority<=samp.size] = paste("First" , samp.size)
os.grid$samp.run[os.grid$england.sample.priority>samp.size &
                   os.grid$england.sample.priority<=(bonus.sample+samp.size)] = paste("Bonus" , bonus.sample)
os.grid$samp.run = factor(os.grid$samp.run, levels = c("First 100", "Bonus 50"))

table(os.grid$samp.run, os.grid$eng.strata1)

os.grid %>% filter(!is.na(samp.run)) %>% 
  st_write("Sampling design\\England_NolanOS_Sample100_50.gpkg")

nu.points = st_centroid(os.grid[!is.na(os.grid$samp.run),])
world <- ne_countries(scale = "large", returnclass = "sf")

map.sampled.grids <- ggplot(data=nu.points %>% st_simplify(dTolerance = 100)) +
  geom_sf(data = world %>%  st_transform(27700), size = 0.1) +
  geom_sf(mapping = aes(colour = samp.run)) +
  geom_sf(data = world %>%  st_transform(27700), size = 0.1, fill = NA) +
  labs(x = NULL, y = NULL , title = "Selected grid cells", 
       subtitle = "100 priority cells, and next 50 reserve",# caption = ""
  could = "Sample Priority")+
  coord_sf(xlim = pad.lim(st_bbox(os.grid) [c(1,3)]), ylim = pad.lim(st_bbox(os.grid) [c(2,4)]), expand = FALSE) +
  theme_map() +
  theme(legend.position = "right"
  ) 


ggsave(map.sampled.grids, filename = paste0("Sampling design\\England_NolanOS_Sample100_50.png"),
       height = 10.80*image.scale , width = 8.20*image.scale, units = "in")

# England-level squares within project area

## dartmoor ----
# cut landscape info
dart = st_read(paste0(gis.wd, "Data\\Designations\\NE_NationalParksEngland_SHP_Full\\data\\National_Parks_England.shp")) %>% 
  filter(name == "DARTMOOR" ) %>% 
  st_first.spatial.curation()

dart_grids <- lengths(st_intersects(os.grid, dart)) > 0
dart_os.grid <- os.grid[dart_grids,]

# count N AVT records
dart_os.grid$nu_ati_count <-   lengths(st_intersects(dart_os.grid, ati.sf %>% 
                          filter(VeteranStatusName %in% c("Ancient tree", "Veteran tree"))
  ))
# descrepencey with estimated
dart_os.grid <- dart_os.grid %>% 
  mutate(ati_discrep = ha.wt.mean.NEGBINC - nu_ati_count) %>% 
  mutate(ati_discrep = if_else(ati_discrep>=0, ati_discrep, 0))
         
hist(dart_os.grid$ati_discrep, xlab = "Unrecorded AVTs, estimated (N)", main = NULL)
      
  
st_write(dart_os.grid, "from VN\\reprojection\\dart_VN_osgrid00.GeoJSON")




