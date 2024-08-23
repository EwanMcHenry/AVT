##------ Tue Jan 24 12:19:12 2023 ------##
# EMcH

# samplign design for scotland AVT survey
set.seed(50235)
# libraries ----

library(sf) # for gis
library(tidyverse)
library(units) # for set_units()
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(hexbin)
library(raster)
library(rgis) # remotes::install_github("Pakillo/rgis") # for fast_mask # -- other of same name... devtools::install_github('jgcri/rgis')
library(exactextractr) #exact_extract()
library(scales) # for squish and oob
library(brew)

library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires") # install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")


source("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\Ewans functions.R")
source("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\Ewans gis specifications.R")
pad.lim = function (x, map.pad = 0.05){
  # function to add % padding to a range of two numbers
  c(x[1] - diff(range(x)*map.pad),x[2] + diff(range(x)*map.pad) )
}

# directories ----
maindrive = "D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust"
gis.wd = paste0( maindrive, "\\GIS")
scot.ati.wd = paste0( maindrive, "\\ATI\\Scotland ATI concentration mapping")

# data ---------
setwd(scot.ati.wd)

load( file = "CMcC predictions 2023-01-24\\predictions\\scot.data.RData")
load( file = "CMcC predictions 2023-01-24\\predictions\\england.data.RData")

countries = st_read(paste0(gis.wd,"\\Data\\administrative boundaries\\Countries\\R.5countries.simp100m.shp"))%>% st_transform( 27700) %>% arrange(name)
lcm19.rast25.gb = raster(paste0(gis.wd, "\\Data\\LCM\\LCM2019\\25m land parcel\\gb2019lcm25m.tif"))
nwss <- st_read(paste0(gis.wd,"\\Data\\NWSS\\Native_Woodland_Survey_of_Scotland.shp")) %>% st_transform( 27700)
nwss_SS <- read.csv(paste0(gis.wd,"\\Data\\NWSS\\NWSS_Species_Structures.csv")) 
nwss_CS <- read.csv(paste0(gis.wd,"\\Data\\NWSS\\NWSS_Canopy_Structures.csv")) 


load(file = "CMcC predictions 2023-01-24\\m2.RData") # model

# init curation ----

world <- ne_countries(scale = "large", returnclass = "sf")

scot.data$id = 1:dim(scot.data)[1] # add id row
scot.data$area.ha = st_area(scot.data) %>% 
  set_units(value = "ha") %>%
  as.numeric()

## find mainland/area of interest
scot.poly = st_cast(countries[countries$name == "Scotland" ,], "POLYGON")  # cut multipolygon to polygons for islands
scot.poly$area.ha = st_area(scot.poly) %>% 
  set_units(value = "ha") %>%
  as.numeric()
scot.poly$area.rank <- rank(-scot.poly$area.ha) 

scot.main = scot.poly[scot.poly$area.rank<=1, 5] # biggest island polygon

## cut LCM to landscape
scotcrop.lcm19.rast25 <- crop(lcm19.rast25.gb, extent(scot.main))
scot.lcm19.rast25 <- fast_mask(scotcrop.lcm19.rast25, scot.main)
scot.water = scot.lcm19.rast25
values(scot.water) = NA
values(scot.water) [values(scot.lcm19.rast25) %in% 13:14] = 1

## cut prediction dataset to those gird within area of interest
pred.scot.main = scot.data[scot.main,]

## find area of water within each grid cell
pred.scot.main$water.area = exact_extract(scot.water, pred.scot.main, "sum") * # sum of N water cells 
  area(scot.water)@data@values %>% mean()/(1000*1000) # multiply by km per raster cell

#---------------------------------------------------------------------------------------
## INTITIAL PREDICATION exploration etc
#---------------------------------------------------------------------------------------

# FUNCTION TO PREDICT RESPONSES AND NUMERIC EFFECTS with the model - adapted from input+x_script.r files ----
repredict <- function (repred_data = pred.scot.main[,!grepl("influ_", names(pred.scot.main))]){
  data.sf = repred_data[,"geometry"]
  repred_data = as.data.frame(repred_data)  [,names(as.data.frame(repred_data)) !="geometry" ]
  
  # For Scotland
  for (i in 1:10) {
    x <- unname(unlist(predict(m2,repred_data[which(repred_data$FOLD==i),],type='count')))
    repred_data$NEGBINCOUNT[repred_data$FOLD==i] <- x} #COUNT PREDICTION
  for (i in 1:10) {
    x <- unname(unlist(predict(m2,repred_data[which(repred_data$FOLD==i),],type='response')))
    repred_data$NEGBINRESP[repred_data$FOLD==i] <- x} #RESPONSE PREDICTIONS
  for (i in 1:10) {
    x <- unname(unlist(predict(m2,repred_data[which(repred_data$FOLD==i),],type='zero')))
    repred_data$NEGBINZERO[repred_data$FOLD==i] <- x} #ZERO PREDICTIONS
  
  # prediction of influence of specific terms
  # a dummy data where all numeric covars == 0
  cols.numeric.covars = (names(repred_data) %in% names(m2$coefficients$count) & sapply(repred_data, is.numeric ))
  dum_zero_data.scotland = repred_data
  dum_zero_data.scotland[,cols.numeric.covars] <- 0 
  # create cols for prediction of influence of each cavariate
  influ.df =  data.frame (matrix(ncol =  sum(cols.numeric.covars), nrow = dim(repred_data)[1] ))
  colnames(influ.df) = paste0( "influ_",names(repred_data)[cols.numeric.covars])
  
  for ( ii in 1:sum(cols.numeric.covars)){  # for each numeric covariate
    this.covar = names(repred_data)[which(cols.numeric.covars)[ii]] # select covar name
    influ.df[, ii] = repred_data[, this.covar] * m2$coefficients$count[this.covar]
  }
  
  repred_data <-  cbind(data.sf, repred_data, influ.df )
  repred_data
}

# FUNCTION TO MAP VARIABLE ----
map_pred = function(grid = pred.scot.main %>% st_simplify(dTolerance = 100),
                     var.name = "NEGBINCOUNT",
                     main.title = "Rough AVT prediction 1",
                    sub.title = NULL,
                    fill.scale.title = "Rough AVT prediction (N)",
                    col.lim.upper.quantile = 0.98){
  # variables
  
  
  var = as.data.frame(grid)[,names(grid) == var.name]
  # col limts 
  colour.limits = c(0, quantile(var, probs = c(col.lim.upper.quantile), na.rm = T)) 
  
  # colour limits & plot
  pred.map = ggplot(data=grid) +
    geom_sf(data = world %>%  st_transform(27700), size = 0.1) +
    geom_sf(mapping = aes(fill = var), colour = NA, size = 0) +
    scale_fill_viridis_c(name = fill.scale.title,
                         limits = range(colour.brks(colour.limits )), 
                         oob = scales::squish, 
                         breaks = colour.brks(lims = colour.limits),
                         labels = colour.lable(x = var ,
                                               lims = colour.limits),
                         #option = "magma",direction = -1 
                         guide = guide_colorbar(
                           direction = "horizontal", barheight = unit(2, units = "mm"),
                           barwidth = unit(50, units = "mm"), draw.ulim = F,
                           title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
    
    geom_sf(data = world %>%  st_transform(27700), size = 0.1, fill = NA) +
    
    labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
    )+
    coord_sf(xlim = pad.lim(st_bbox(grid) [c(1,3)]), ylim = pad.lim(st_bbox(grid) [c(2,4)]), expand = FALSE) +
    theme_map() +
    theme(legend.position = "bottom"
    ) 
  pred.map
}

# INITIAL PREDICTION MAP + SAVE  ----
pred.map01 <- map_pred(grid = pred.scot.main %>% st_simplify(dTolerance = 100),
                        var.name = "NEGBINCOUNT",
                        main.title = "Rough AVT prediction 1",
                       sub.title = "Original scottish data extrapolation")
  
image.scale = 0.5
ggsave(pred.map01, filename = paste0("analysis\\sampling design outputs\\pred.map01.png"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
ggsave(pred.map01, filename = paste0("analysis\\sampling design outputs\\pred.map01.pdf"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")


## explore prediction distributions ----
top.1per = quantile(pred.scot.main$NEGBINC, probs = 0.99)

hist(pred.scot.main$NEGBINC, breaks = 50,
     xlab = "strata: predicted N trees", main = NULL)

hist(pred.scot.main$NEGBINC[pred.scot.main$NEGBINC < top.1per], breaks = 50,
     xlab = "strata: predicted N trees", main = NULL)
plot(density(pred.scot.main$NEGBINC[pred.scot.main$NEGBINC < top.1per], n = 100))

# EXPLORATION  - bars of categorical covaraite effects ----
# mmess, workin on it 
#cat.covar.name = "Agriclass"
# 
# # violin plots of prediction based on distibution of modeled predictions 
# pred.scot.main %>% 
#   ggplot( aes(x=Agriclass, y=NEGBINCOUNT), fill= F) +
#     geom_violin(width=1.4) +
#     geom_boxplot(width=0.1, color="grey", alpha=0.2) +
#     theme_ipsum() +
#     theme(
#       legend.position="none",
#       plot.title = element_text(size=11)
#     ) +
#     ggtitle("A Violin wrapping a boxplot") +
#     xlab("")
# 
# 
# effs = m2$coefficients$count
# effs[grep(cat.covar.name, names(effs))] + effs["(Intercept)"]
# 
# pred.scot.main$AncVPlan
# AncVPlan
# Landclass
# SoilType


#---------------------------------------------------------------------------------------
## EFFECTS MAPS
#---------------------------------------------------------------------------------------
# MAP of NUMERIC covaraite effects ----

# should really fix all categorical to their referecne factor 1 and add separate plots for them as bar

# function to map prediction of effect of each numeric covariate ----
#     takes the prediction sf, 
map_pred_effect_influence <- function(  grid = pred.scot.main %>% st_simplify(dTolerance = 100),
                                        indiv.map.name = "m2_01"){
  fill.scale.title = "Effect"
  # variables
  grid.df = as.data.frame(grid)
  # the coloums  to plot
  eff.cols = grep("influ_",names(grid.df))[colSums(grid.df[,grepl("influ_",names(grid.df))]) != 0] # col numbers
  col.lim.var.names = names(grid.df)[eff.cols]
  nice.covar.names = c("Altitude", "Moated site dist", "Medieval deer park dist", "Nat. Trust dist",
                       "Cities dist", "Towns dist", "Maj. road dist", "Woodland area", 
                       "AWI area", "Trad. orchard area", "Woodpasture area", "Watercourse dist",
                       "Minor road dist")
  
  # colour limts for plots
  col.lim.effects =  c(as.matrix(as.data.frame(grid) [,col.lim.var.names] )) # all covars effects in df
  abs.col.lim = max(abs(range(col.lim.effects))) # range of all col vars, abs + and - to allow comparision of effects between covars and + and - 
  colour.limits = c(-abs.col.lim, abs.col.lim) 
  low.col = E.cols$connectiv.low
  high.col = E.cols$connectiv.high
  
  # list of effects map plots
  eff.maps <- lapply(seq_along(eff.cols), FUN = function(i) {
    var.name = names(grid.df)[eff.cols[i]]
    main.title = paste( nice.covar.names[i] , "Influence")
    sub.title = NULL
    
    var = as.data.frame(grid)[,names(grid) == var.name]
    
    eff.map = ggplot(data=grid) +
      geom_sf(data = world %>%  st_transform(27700), size = 0.1) +
      geom_sf(mapping = aes(fill = var), colour = NA, size = 0) +
      scale_fill_gradient2(name = fill.scale.title,
                           low= low.col, high= high.col ,
                           breaks = colour.brks(lims = colour.limits),
                           labels = colour.lable(x = col.lim.effects ,
                                                 lims = colour.limits),
                           limits = colour.limits, 
                           oob = scales::squish,
                           guide = guide_colorbar(
                             direction = "horizontal", barheight = unit(2, units = "mm"),
                             barwidth = unit(50, units = "mm"), draw.ulim = F,
                             title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
      
      geom_sf(data = world %>%  st_transform(27700), size = 0.1, fill = NA) +
      
      labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
      )+
      coord_sf(xlim = pad.lim(st_bbox(grid) [c(1,3)]), ylim = pad.lim(st_bbox(grid) [c(2,4)]), expand = FALSE) +
      theme_map() +
      theme(legend.position = "bottom"
      ) 
    image.scale = 0.5
    ggsave(eff.map, filename = paste0("analysis\\sampling design outputs\\strata_predication\\indiv strata prediction effects\\",
                                      var.name, "_map_", indiv.map.name, ".png"),
           height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
    eff.map
  })
  eff.maps
}
# plot prediction map when all other numeric effects are set to 0
eff.maps <- map_pred_effect_influence(  grid = pred.scot.main %>% st_simplify(dTolerance = 100),
                                        indiv.map.name = "m2_01")

# arrange numeric effects maps on one device & save
comparison.plot = ggarrange(plotlist =  eff.maps,
                            ncol = 5, nrow = 3, common.legend = TRUE, legend = "bottom")

ggsave(comparison.plot, filename = "analysis\\sampling design outputs\\strata_predication.effects_map_comparison.pdf",
       height = 10.80 , width = 19.20, units = "in")

ggsave(comparison.plot, filename = "analysis\\sampling design outputs\\strata_predication.effects_map_comparison.png",
       height = 10.80 , width = 19.20, units = "in")

#---------------------------------------------------------------------------------------
## TRUNCATION OF SCOTLAND COVARIATE DATA
#---------------------------------------------------------------------------------------
# truncating some covariates with much greater range in scotland than england, 
# based on viz. an. of mapped prediction  and influence of different effects

## MEDEVIL DP and NT distances, re-predict and re-map ----
# truncate
repred_scot.data <- pred.scot.main[,!grepl("influ_", names(pred.scot.main))] # not the "influence of..." coloum , they get put in alter
repred_scot.data$MedDP[repred_scot.data$MedDP > max(england.data$MedDP) ] <-  max(england.data$MedDP)
repred_scot.data$NatTrust[repred_scot.data$NatTrust > max(england.data$NatTrust) ] <-  max(england.data$NatTrust)
# predict RESPONSES AND NUMERIC EFFECTS
repred_scot.data01 = repredict(repred_data = repred_scot.data)
# map prediction plot
pred.map02 <- map_pred(grid = repred_scot.data01 %>% st_simplify(dTolerance = 100),
                       var.name = "NEGBINCOUNT",
                       main.title = "Rough AVT prediction 2",
                       sub.title = "Truncated National Trust and Med. deerpark distance")
eff.maps02 <- map_pred_effect_influence(  grid = repred_scot.data01 %>% st_simplify(dTolerance = 100),
                                          indiv.map.name = "m2_02")
comparison.plot = ggarrange(plotlist =  eff.maps02,
                            ncol = 5, nrow = 3, common.legend = TRUE, legend = "bottom")

# save
image.scale = 0.5
ggsave(pred.map02, filename = paste0("analysis\\sampling design outputs\\pred.map02.png"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
ggsave(pred.map02, filename = paste0("analysis\\sampling design outputs\\pred.map02.pdf"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
ggsave(comparison.plot, filename = "analysis\\sampling design outputs\\strata_predication.effects_map_comparison02.pdf",
       height = 10.80 , width = 19.20, units = "in")
ggsave(comparison.plot, filename = "analysis\\sampling design outputs\\strata_predication.effects_map_comparison02.png",
       height = 10.80 , width = 19.20, units = "in")


## TOWNS AND CITIES distances, re-predict and re-map ----
# truncate
repred_scot.data <- pred.scot.main[,!grepl("influ_", names(pred.scot.main))]
repred_scot.data$MedDP[repred_scot.data$MedDP > max(england.data$MedDP) ] <-  max(england.data$MedDP)
repred_scot.data$NatTrust[repred_scot.data$NatTrust > max(england.data$NatTrust) ] <-  max(england.data$NatTrust)
repred_scot.data$Towns[repred_scot.data$Towns > max(england.data$Towns) ] <-  max(england.data$Towns)
repred_scot.data$Cities[repred_scot.data$Cities > max(england.data$Cities) ] <-  max(england.data$Cities)
# predict RESPONSES AND NUMERIC EFFECTS
repred_scot.data02 = repredict(repred_data = repred_scot.data)
pred.map03 <- map_pred(grid = repred_scot.data02 %>% st_simplify(dTolerance = 100),
                       var.name = "NEGBINCOUNT",
                       main.title = "Rough AVT prediction 3",
                       sub.title = "Truncated NT Med. deerpark, cities & towns distances")
eff.maps03 <- map_pred_effect_influence(  grid = repred_scot.data02 %>% st_simplify(dTolerance = 100),
                                          indiv.map.name = "m2_03")
comparison.plot = ggarrange(plotlist =  eff.maps03,
                            ncol = 5, nrow = 3, common.legend = TRUE, legend = "bottom")
# save
image.scale = 0.5
ggsave(pred.map03, filename = paste0("analysis\\sampling design outputs\\pred.map03.png"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
ggsave(pred.map03, filename = paste0("analysis\\sampling design outputs\\pred.map03.pdf"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
ggsave(comparison.plot, filename = "analysis\\sampling design outputs\\strata_predication.effects_map_comparison03.pdf",
       height = 10.80 , width = 19.20, units = "in")
ggsave(comparison.plot, filename = "analysis\\sampling design outputs\\strata_predication.effects_map_comparison03.png",
       height = 10.80 , width = 19.20, units = "in")


#---------------------------------------------------------------------------------------
## NWSS
#---------------------------------------------------------------------------------------

# curation  ----
# fix typos
nwss_SS$STOCKING[nwss_SS$STOCKING == "100 <600 stems/ha"] = "100 < 600 stems/ha"
nwss_SS$STOCKING[nwss_SS$STOCKING == "1100 <2500 stems/ha"] = "1100 < 2500 stems/ha"
nwss_SS$STOCKING[nwss_SS$STOCKING == "600 <1100 stems/ha"] = "600 < 1100 stems/ha"
nwss_SS$STOCKING[nwss_SS$STOCKING == " "] = "<100 stems/ha" # only one instance, seems clear-cut
# order levels of stocking
nwss_SS$STOCKING = factor(nwss_SS$STOCKING, 
                          levels = c("<100 stems/ha", "100 < 600 stems/ha", "600 < 1100 stems/ha", 
                                     "1100 < 2500 stems/ha", ">2500 stems/ha"))
# no structure given - remove structure -- 
## looking at this one recored in main file suggests it is pole immature (dom structure type there)
## but that already in the df for this site.. maybe shrub???
nwss_CS <- nwss_CS[nwss_CS$STRUCTURE != " ",]

# exploration ----
record.entry.type <- paste(nwss_SS$SCPTDATA_I ,nwss_SS$STRUCTURE,nwss_SS$SPECIES)#, nwss_SS$STOCKING)
record.entry.type[nwss_SS$STRUCTURE == "Veteran"] %>% duplicated() %>% sum() # no duplicate veteran records for site-species combos, e.g. in different compartments or densities 

# PERCENT AND AREA OF EACH STRUCTURE to main nwss sf  - from canopy structure df ----
nwss2 <- nwss %>% 
  mutate(area.ha.nwss = st_area(nwss) %>%  # add area coloum
           set_units(value = "ha") %>%
           as.numeric()) %>%
  # add percentage of each structure
  left_join(nwss_CS %>% 
              dplyr::select(SCPTDATA_I, STRUCT_PCT, STRUCTURE ) %>% 
              mutate(STRUCTURE = paste(STRUCTURE, "percent")) %>% 
              pivot_wider(names_from = STRUCTURE , values_from = STRUCT_PCT),# percentage for eafch structure as their own cols
            by = "SCPTDATA_I") 
# add area of each structure
structure.area.names = paste(str_remove(names(nwss2)[grepl("percent", names(nwss2))], " percent"), "area.ha")
structure.areas.df = as.data.frame(nwss2)[,grepl("percent", names(as.data.frame(nwss2)))]/100 *
  nwss2$CANOPY_PCT/100 *
  nwss2$area.ha.nwss
colnames(structure.areas.df) = structure.area.names
nwss2 = cbind(nwss2 , structure.areas.df)

# N VET SPECIES PER nwss sf                           - from species structure df ----

# number of veteran species within each nwss plot
nwss2 <- nwss2 %>%
  left_join(nwss_SS %>%
              filter(STRUCTURE == "Veteran" ) %>%
              group_by(SCPTDATA_I) %>% 
              summarise(N.vet.species = length(unique(SPECIES))))
nwss2$N.vet.species [is.na(nwss2$N.vet.species)] <- 0  

## FUNCTION - VET n species, n sites and area TO GRID ----
NWSS_vet_data.to_grid <- function(prediction.grid = repred_scot.data02,
                          nwss.data = nwss2 # needs to contain N vet species per and area of vet 
                          ){
  # N VET SPECIES PER GRID SQUARE - from species structure ----
  # intersect nwss-grid
  predgrid.nwss = st_intersection(prediction.grid, nwss.data) 
  predgrid.nwss$nwss_grid_inter.area.ha = st_area(predgrid.nwss) %>%  # add area coloum
    set_units(value = "ha") %>%
    as.numeric()
  
  # grid area of nwss survey
  prediction.grid <- prediction.grid %>% 
    left_join(predgrid.nwss %>%
                as.data.frame() %>% 
                group_by(id) %>% 
                summarise(nwss_surv.area.ha = sum(nwss_grid_inter.area.ha))
              , by = "id")
  prediction.grid$nwss_surv.area.ha[is.na(prediction.grid$nwss_surv.area.ha)]= 0

  ## N unique vet species per grid cell
  indiv.vet.spp.per.grid <- nwss_SS %>%
    filter(STRUCTURE == "Veteran" ) %>% # veteran species structure data
    # givs grid ID to species structure
    left_join(predgrid.nwss %>%  # join nwss-grid_inter to species structure data - 
                dplyr::select(SCPTDATA_I, id ),
              by = "SCPTDATA_I") %>% 
    # count unique species per grid
    group_by(id) %>% 
    summarise(n.vet.species.grid = length(unique(SPECIES))) 
  # add count to grid
  prediction.grid <- prediction.grid %>% 
    left_join(indiv.vet.spp.per.grid, by = "id")
  
  
  # AREA OF AND N SITES WITH VETS PER GRID SQUARE - from ----
  # area of vets within grid square asume even distribution 
  ## where nwss polygons split by grid >> split vet area by proportion in each grid
  ## multiply vet area by fraction of poly within grid square 
  predgrid.nwss$vet.area_grid.nwsspoly <- predgrid.nwss$Veteran.area.ha * (predgrid.nwss$nwss_grid_inter.area.ha /
                                                                             predgrid.nwss$area.ha.nwss)
  # count non-na polys per grid sq
  prediction.grid <- 
    prediction.grid %>% 
    # join count and area of vet within non-na polys per grid sq
    left_join(predgrid.nwss %>%
              as.data.frame() %>% 
              filter(!is.na(Veteran.area.ha)) %>% 
              group_by(id) %>% 
              summarise(vet.area.ha.grid = sum(vet.area_grid.nwsspoly), # sum all non-na vet areas within grid sq
                        N.vet.sites.grid = length(vet.area_grid.nwsspoly)), # count all non-na vet polys within
            by = "id")
  
  # add zero areas to places with nwss surveys and no vet records
    # prediction.grid$N.vet.sites.grid[is.na(prediction.grid$N.vet.sites.grid) &
    #                                    prediction.grid$nwss_surv.area.ha>0] <- 0
    # prediction.grid$n.vet.species.grid[is.na(prediction.grid$n.vet.species.grid) &
    #                                    prediction.grid$nwss_surv.area.ha>0] <- 0
    prediction.grid
}
# ADD NWSS DATA TO GRID: N VET SITES, N VET SPECIES, VET AREA ----
pred.nwss.grid <- NWSS_vet_data.to_grid(prediction.grid = repred_scot.data02,
                                        nwss.data = nwss2 )

# MAP PLOT NWSS grid N species Nsite VetCover  ----
map_plot.grid.nwss_nvetspp <- 
 map_pred(grid = pred.nwss.grid %>% st_simplify(dTolerance = 100),
          var.name = "n.vet.species.grid",
          main.title = "N veteran species",
          sub.title = "Recorded in NWSS surveys",
          fill.scale.title = "N veteran species recorded")
map_plot.grid.nwss_nvetsites <- 
  map_pred(grid = pred.nwss.grid %>% st_simplify(dTolerance = 100),
         var.name = "N.vet.sites.grid",
         main.title = "N sites with veterans",
         sub.title = "Vetran trees recorded in NWSS surveys",
         fill.scale.title = "N sites")

map_plot.grid.nwss_ha.vets <- 
  map_pred(grid = pred.nwss.grid %>% st_simplify(dTolerance = 100),
         var.name = "vet.area.ha.grid",
         main.title = "Veteran canopy cover",
         sub.title = "In grid squares where NWSS recorded vets",
         fill.scale.title = "ha")

comboplot.nwss.grid.vet_spp_ha_sites <- ggarrange(map_plot.grid.nwss_nvetspp,
                                                  map_plot.grid.nwss_nvetsites,
                                                  map_plot.grid.nwss_ha.vets,
                                                  ncol = 3, nrow = 1, common.legend = F, legend = "bottom")

ggsave(comboplot.nwss.grid.vet_spp_ha_sites, filename = "analysis\\sampling design outputs\\map_grid_nwss_veteran.pdf",
       height = 10.80 , width = 19.20, units = "in")

# EXPLORE nolan pred, vet area and n species distributions ----

quantile(pred.nwss.grid$NEGBINCOUNT, probs = seq(0.9,1,length = 11))
pred.nwss.grid$trunc.NEGBINCOUNT <- pred.nwss.grid$NEGBINCOUNT
pred.nwss.grid$trunc.NEGBINCOUNT[pred.nwss.grid$trunc.NEGBINCOUNT>4] <- 4

quantile(pred.nwss.grid$vet.area.ha.grid , probs = seq(0.95,1,length = 11), na.rm = T)
pred.nwss.grid$trunc.vet.area.ha.grid <- pred.nwss.grid$vet.area.ha.grid
pred.nwss.grid$trunc.vet.area.ha.grid[pred.nwss.grid$trunc.vet.area.ha.grid>2] <- 2

quantile(pred.nwss.grid$n.vet.species.grid , probs = seq(0.8,1,length = 11), na.rm = T)
pred.nwss.grid$trunc.vet.area.ha.grid <- pred.nwss.grid$vet.area.ha.grid
pred.nwss.grid$trunc.vet.area.ha.grid[pred.nwss.grid$trunc.vet.area.ha.grid>2] <- 2


# english model comparison plots
gg.compare_eng_nspp = pred.nwss.grid %>% 
  ggplot(aes(x=n.vet.species.grid, y=trunc.NEGBINCOUNT)) +
  geom_bin2d(bins = 15) +
  scale_fill_continuous(type = "viridis", limits=c(0, 200), oob=squish) +
  scale_x_continuous(limits = c(0,16))+
  labs(x = "N vet. spp", y = "English model prediction", 
       title = "English  model:NWSS comparison - N spp.")+
  theme_pubr()+
  theme(legend.position = "right")

gg.compare_eng_vetcov = pred.nwss.grid %>% 
  ggplot(aes(x=n.vet.species.grid, y=trunc.vet.area.ha.grid)) +
  geom_bin2d(bins = 20) +
  scale_fill_continuous(type = "viridis", limits=c(0, 200), oob=squish) +
  scale_x_continuous(limits = c(0,16))+
  labs(x = "Vet cover (ha)", y = "English model prediction", 
       title = "English  model:NWSS comparison - vet cover")+
  theme_pubr()+
  theme(legend.position = "right")

# CATEGORISATION OF types of NWSS vet record grid sqaures ----
pred.nwss.grid$grid.vet.type <- NA
pred.nwss.grid$grid.vet.type[is.na(pred.nwss.grid$N.vet.sites.grid) & 
                               pred.nwss.grid$nwss_surv.area.ha>0] <- "No record - surveyed"
pred.nwss.grid$grid.vet.type[pred.nwss.grid$nwss_surv.area.ha == 0] <- "No record - No survey"
pred.nwss.grid$grid.vet.type[pred.nwss.grid$vet.area.ha.grid == 0] <- "Few records < 5% cover"
pred.nwss.grid$grid.vet.type[pred.nwss.grid$vet.area.ha.grid > 0] <- "Records >=5% cover"

# additional site defintion 
pred.nwss.grid$grid.vet.type[pred.nwss.grid$grid.vet.type == "Records >=5% cover" &
                               pred.nwss.grid$vet.area.ha.grid <0.05] <- "Few records < 5% cover"
pred.nwss.grid$grid.vet.type[pred.nwss.grid$grid.vet.type == "Few records < 5% cover" &
                               (pred.nwss.grid$N.vet.sites.grid >=5|
                                  pred.nwss.grid$n.vet.species.grid >=5)  ] <- "Records >=5% cover"
pred.nwss.grid$grid.vet.type <- factor(pred.nwss.grid$grid.vet.type,
                                       levels = c("No record - No survey", "No record - surveyed", "Few records < 5% cover", "Records >=5% cover"))


# table types
table(pred.nwss.grid$grid.vet.type)
(table(pred.nwss.grid$grid.vet.type)/dim(pred.nwss.grid)[1]) %>% round(2)
boxplot(pred.nwss.grid$trunc.NEGBINCOUNT~pred.nwss.grid$grid.vet.type)

# MAP CATEGORIES OF vet record types -----
##  FUNCTION - MAP CATEGORIES ----
map_cat = function(grid = pred.nwss.grid %>% st_simplify(dTolerance = 100),
                    var.name = "grid.vet.type",
                    main.title = "NWSS veteran records",
                    sub.title = NULL,
                    fill.scale.title = NULL){
  var = as.data.frame(grid)[,names(grid) == var.name]

  
  var.map = ggplot(data=grid) +
    geom_sf(data = world %>%  st_transform(27700), size = 0.1) +
    geom_sf(mapping = aes(fill = var), colour = NA, size = 0) +
    scale_fill_viridis_d(name = fill.scale.title, na.value="transparent",
                      guide = guide_legend(title.position = "top",  title.hjust = 0))+
    geom_sf(data = world %>%  st_transform(27700), size = 0.1, fill = NA) +
    
    labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
    )+
    coord_sf(xlim = pad.lim(st_bbox(grid) [c(1,3)]), ylim = pad.lim(st_bbox(grid) [c(2,4)]), expand = FALSE) +
    theme_map() +
    theme(legend.position = "right"
    ) 
  var.map
}

map.nwss.vettype <- map_cat(grid = pred.nwss.grid %>% st_simplify(dTolerance = 100),
                       var.name = "grid.vet.type",
                       main.title = "NWSS veteran records",
                       sub.title = "no, few or many veteran records in NWSS")

image.scale = 0.5
ggsave(map.nwss.vettype, filename = paste0("analysis\\sampling design outputs\\map.nwss.vettype.png"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
ggsave(map.nwss.vettype, filename = paste0("analysis\\sampling design outputs\\map.nwss.vettype.pdf"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")

## few records exploration ----
pred.nwss.grid$N.vet.sites.grid[pred.nwss.grid$grid.vet.type == "Few records < 5% cover"] %>% 
  hist(main = "N sites; Few: all vet records < 5% cover", xlab = "N sites", xlim = c(0,20))
pred.nwss.grid$n.vet.species.grid[pred.nwss.grid$grid.vet.type == "Few records < 5% cover"] %>% 
  hist(main = "N vet spp; Few: all vet records < 5% cover", 
       xlim = c(0,20), xlab = "N vet. spp.")

pred.nwss.grid %>% 
  subset(grid.vet.type == "Few records < 5% cover") %>% 
  ggplot(aes(x=N.vet.sites.grid, y=n.vet.species.grid)) +
  geom_bin2d(bins = 15) +
  scale_fill_continuous(type = "viridis", limits=c(0, 200), oob=squish) +
  labs(x = "N vet. sites", y = "N vet. spp.", 
       title = "Few: all vet records < 5% cover")+
  theme_pubr()+
  theme(legend.position = "right")

## more records exploration ----
pred.nwss.grid$N.vet.sites.grid[pred.nwss.grid$grid.vet.type == "Records >=5% cover"] %>% 
  hist(main = "N sites; More: <= 5% cover recorded", xlab = "N sites", xlim = c(0,20))
pred.nwss.grid$n.vet.species.grid[pred.nwss.grid$grid.vet.type == "Records >=5% cover"] %>% 
  hist(main = "N vet spp.; More: <= 5% cover recorded", 
       xlim = c(0,20), xlab = "N vet. spp.")
pred.nwss.grid$vet.area.ha.grid[pred.nwss.grid$grid.vet.type == "Records >=5% cover"] %>% 
  hist(main = "Cover; More: <= 5% cover recorded", 
       xlim = c(0,10), xlab = "Cover (ha)")

max.area = 0.05
pred.nwss.grid$vet.area.ha.grid[pred.nwss.grid$grid.vet.type == "Records >=5% cover" &
                                  pred.nwss.grid$vet.area.ha.grid <max.area ] %>%
 hist(main = "Cover; More: <= 5% cover recorded", 
       xlim = c(0,max.area), xlab = "Cover (ha)")

#0.05 is the "fewer" cutoff sayz emma

dens.plot.nwss.ssp.sites <- pred.nwss.grid %>% 
  subset(grid.vet.type == "Records >=5% cover") %>% 
  ggplot(aes(x=N.vet.sites.grid, y=n.vet.species.grid)) +
  geom_bin2d(bins = 15) +
  scale_fill_continuous(type = "viridis", limits=c(0, 200), oob=squish) +
  labs(x = "N vet. sites", y = "N vet. spp.", 
       title = "More: <= 5% cover recorded")+
  theme_pubr()+
  theme(legend.position = "right")


dens.plot.nwss.ssp.vetcov <- pred.nwss.grid %>% 
  subset(grid.vet.type == "Records >=5% cover") %>% 
  ggplot(aes(x=trunc.vet.area.ha.grid, y=n.vet.species.grid)) +
  geom_bin2d(bins = 15) +
  scale_fill_continuous(type = "viridis", limits=c(0, 200), oob=squish) +
  labs(x = "Vet cover (ha)", y = "N vet. spp.", 
       title = "More: >= 5% cover recorded")+
  theme_pubr()+
  theme(legend.position = "right")


# save .shp of nwss data
nwss3 = nwss2
names(nwss3)[22:35] = c("area_ha","per_Pole_immature", "per_Shrub", "per_Estab_regen",
                        "per_Mature", "per_Vis_regen", "per_Veteran", "ha_Pole_immature",
                        "ha_Shrub", "ha_Estab_regen", "ha_Mature", "ha_Vis_regen",
                        "ha_Veteran", "N_vet_spp") 
st_write(nwss3, "D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\Data\\NWSS\\nwss_v2.00EMcH.shp",
         overwrite = T, delete_dsn = TRUE)

#---------------------------------------------------------------------------------------
## PLOT ATI DATA
#---------------------------------------------------------------------------------------


map_plot.ati_count <- 
  map_pred(grid = pred.nwss.grid %>% st_simplify(dTolerance = 100),
           var.name = "Count_",
           main.title = "ATI records",
           sub.title = "AVTs recorded on ATI",
           fill.scale.title = "N trees",
           col.lim.upper.quantile = 0.995)

ggsave(map_plot.ati_count, filename = "analysis\\sampling design outputs\\map_grid_ati.pdf",
       height = 10.80 , width = 19.20/3, units = "in")


#---------------------------------------------------------------------------------------
## STRATIFICATION
#---------------------------------------------------------------------------------------
# cut out areas with too much water ----
nu.grid = pred.nwss.grid
nu.grid = nu.grid[nu.grid$ water.area<0.7,]

# ASSIGN STRATA  FROM NOLAN MODEL PREDICTION AND NWSS DATA ----
# explroation
table(nu.grid$grid.vet.type)
(table(nu.grid$grid.vet.type)/dim(nu.grid)[1]) %>% round(2)

quantile(nu.grid$NEGBINCOUNT, probs = seq(0.95,1, length = 11))
quantile(nu.grid$NEGBINCOUNT, probs = seq(0.5,1, length = 11))

nu.grid$NEGBINCOUNT_trunc = nu.grid$NEGBINCOUNT
# nu.grid$NEGBINCOUNT_trunc[nu.grid$NEGBINCOUNT_trunc > 
#                                    quantile(nu.grid$NEGBINCOUNT, probs = 0.99)] <- 
#   quantile(nu.grid$NEGBINCOUNT, probs = 0.99)
nu.grid$NEGBINCOUNT_trunc[nu.grid$NEGBINCOUNT_trunc > 2 ]<- 2
nu.grid$NEGBINCOUNT_trunc %>% hist(xlim = c(0,2))

nu.grid$strata1 = 1
nu.grid$strata1[nu.grid$NEGBINCOUNT > median(nu.grid$NEGBINCOUNT)] = 2 # 50% - 0.2564874
nu.grid$strata1[nu.grid$NEGBINCOUNT > median(nu.grid$NEGBINCOUNT[nu.grid$strata1>1])] = 3 # 75% - 0.6055862
nu.grid$strata1[nu.grid$NEGBINCOUNT > median(nu.grid$NEGBINCOUNT[nu.grid$strata1>2])] = 4 # 87.5% - 0.9988438
nu.grid$strata1[nu.grid$NEGBINCOUNT > median(nu.grid$NEGBINCOUNT[nu.grid$strata1>3])] = 5 # 93.75% - 1.617127
nu.grid$strata1 <- as.factor(nu.grid$strata1)

nu.grid$strata2 = NA
nu.grid$strata2[grepl("No record", nu.grid$grid.vet.type)] = "No record"
nu.grid$strata2[grepl("Few records", nu.grid$grid.vet.type)] = "Few records"
nu.grid$strata2[grepl("Records >=5%", nu.grid$grid.vet.type)] = "Many records"
nu.grid$strata2 <- factor(nu.grid$strata2, levels = (c("No record", "Few records", "Many records")))

# MAP STRATA ----
map.strata1 <- map_cat(grid = nu.grid %>% st_simplify(dTolerance = 100),
                            var.name = "strata1",
                            main.title = "Strata1 - Nolan model prediction",
                            sub.title = "5 strata based on predictions extrapolated from England model",
                       fill.scale.title = "Strata 1")

map.strata2 <- map_cat(grid = nu.grid %>% st_simplify(dTolerance = 100),
                       var.name = "strata2",
                       main.title = "Strata 2 - NWSS veteran records",
                       sub.title = "3 strata based on cover, species/site number records in NWSS",
                       fill.scale.title = "Strata 2")

hist.strata1 <- nu.grid %>% 
  ggplot(aes(x = NEGBINCOUNT_trunc, fill = strata1) ) +
  geom_histogram(bins = 20)+
  labs(x = "Nolan prediction - truncated") +
  scale_fill_viridis_d() +
  theme_pubr() 
hist.strata2 <- nu.grid %>% 
  ggplot(aes(x = NEGBINCOUNT_trunc, fill = strata2) ) +
  geom_histogram(bins = 20)+
  labs(x = "Nolan prediction - truncated") +
  scale_fill_viridis_d() +
  theme_pubr() +
  theme(legend.position = "right")

comboplot.strata.map <- ggarrange(map.strata1, map.strata2, 
                                  ncol = 2, nrow = 1, common.legend = F, legend = "bottom")
comboplot.strata.hists <- ggarrange(hist.strata1, hist.strata2,
                                    ncol = 2, nrow = 1, common.legend = F, legend = "bottom")
image.scale = 0.5
ggsave(comboplot.strata.map, filename = paste0("analysis\\sampling design outputs\\comboplot.strata.map.pdf"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
ggsave(comboplot.strata.hists, filename = paste0("analysis\\sampling design outputs\\comboplot.strata.hists.pdf"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")


ggsave(map.strata1, filename = paste0("analysis\\sampling design outputs\\map.strata1.png"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")
ggsave(map.strata1, filename = paste0("analysis\\sampling design outputs\\map.strata1.pdf"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")

# TABLE STRATA  ----

table(nu.grid$strata1, nu.grid$strata2)
(table(nu.grid$strata1, nu.grid$strata2)/dim(nu.grid)[1]) %>% round(2)

# COMBINE STRATA AND SAMPLE ----

nu.grid$strata.combo <- paste(nu.grid$strata1, nu.grid$strata2)

nu.grid = nu.grid %>%
  group_by(strata.combo) %>%
  mutate(strata.count = length(strata.combo)) %>% 
  mutate(samp.prob = 1/strata.count)

# sample
samp.size = 100
bonus.sample = 50
# randomly order grid IDs, weighted based on sampling probability form stratification
nu.grid$sample.priority = order(sample(nu.grid$id, 
                                              size = length(nu.grid$id),
                                              replace = F,
                                              prob = nu.grid$samp.prob))

table(nu.grid$strata1[nu.grid$sample.priority<samp.size], nu.grid$strata2[nu.grid$sample.priority<100]) %>% print()

# CUT OUT SAMPLED CELLS AND MAP
nu.grid$samp.run = NA
nu.grid$samp.run[nu.grid$sample.priority<=samp.size] = paste("First" , samp.size)
nu.grid$samp.run[nu.grid$sample.priority>samp.size &
                   nu.grid$sample.priority<=(bonus.sample+samp.size)] = paste("Bonus" , bonus.sample)
nu.grid$samp.run = factor(nu.grid$samp.run, levels = c("First 100", "Bonus 50"))

nu.points = st_centroid(nu.grid[!is.na(nu.grid$samp.run),])

map.sampled.grids <- ggplot(data=nu.points %>% st_simplify(dTolerance = 100)) +
  geom_sf(data = world %>%  st_transform(27700), size = 0.1) +
  geom_sf(mapping = aes(colour = samp.run)) +
  geom_sf(data = world %>%  st_transform(27700), size = 0.1, fill = NA) +
  labs(x = NULL, y = NULL , title = "Selected grid cells", 
       subtitle = "100 priority cells, and next 50 reserve"#, caption = ""
  )+
  coord_sf(xlim = pad.lim(st_bbox(nu.grid) [c(1,3)]), ylim = pad.lim(st_bbox(nu.grid) [c(2,4)]), expand = FALSE) +
  theme_map() +
  theme(legend.position = "right"
  ) 
  

ggsave(map.sampled.grids, filename = paste0("analysis\\sampling design outputs\\map.sampled.grids.pdf"),
       height = 10.80*image.scale , width = 19.20*image.scale, units = "in")


# water cover in sampled ----
hist(nu.grid$water.area[nu.grid$sample.priority<samp.size])

# save objects ----
st_write(nu.grid,  "analysis\\sampling design outputs\\sample_priority01.gpkg", delete_dsn = TRUE)
st_write(nu.grid[!is.na(nu.grid$samp.run),],  "analysis\\sampling design outputs\\sampled100_and_bonus01.gpkg",
         delete_dsn = TRUE)

# TIDY AND SAVE NEW DATA ----
save(nu.grid, nwss2, 
     file = "analysis\\sampling design outputs\\grid_and_nuNWSS.RData"  )

#---------------------------------------------------------------------------------------
## strata density plot
#---------------------------------------------------------------------------------------
load("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\ATI\\Scotland ATI concentration mapping\\analysis\\sampling design outputs\\grid_and_nuNWSS.RData" )

table(nu.grid$strata1, nu.grid$strata2)

nu.grid %>% as.data.frame() %>%
  ggplot(., aes(x = strata1, y = strata2, fill = Count_)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "Strata 1", y = "Strata 2", title = "Heatmap of Data Points in Strata1 and Strata2") +
  theme_minimal()
  
  
  

# Create the heatmap with ggplot2
nu.grid %>%
  as.data.frame() %>%
  ggplot(., aes(x = strata1, y = strata2, fill = Count_)) +
  geom_tile() +
  scale_fill_viridis_c() +  # You can choose a different color palette if you prefer
  labs(x = "Strata 1", y = "Strata 2", title = "2D Heatmap of Data Points in Strata1 and Strata2") +
  theme_minimal() +
  geom_text(aes(label = ifelse(Count_ > 0, Count_, "")), color = "black", size = 3)

# Add text labels for the count values
heatmap_plot_with_labels <- heatmap_plot +
  geom_text(aes(label = ifelse(Count_ > 0, Count_, "")), color = "black", size = 3)


#---------------------------------------------------------------------------------------
## TRAVELING SALESMAN
#---------------------------------------------------------------------------------------
# an attempt to give some guidance on a sensible order in which to visit sites

library(osrm)
library(TSP)

travel_time <- osrmTable(loc = nu.points[nu.points$samp.run == "First 100",])
travel_trip <- osrmTrip(loc = nu.points[nu.points$samp.run == "First 100",])

# Initialize the TSP object
tsp_prob <- TSP(as.dist(travel_time$durations))

# We add a dummy to the TSP, in this way we remove

# the constraint of having a path that ends at the
# starting point
tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')
# TSP solver
tour <-
  solve_TSP(
    tsp_prob,
    method = 'two_opt',
    control = list(rep = 16)
  )
# Optimal path
path <- names(cut_tour(tour, 'dummy'))


# Prepare the data for plotting
data <- nu.points[nu.points$samp.run == "First 100",]
data$id_order <- order(as.integer(path))
data$lng = st_coordinates(data %>% st_transform(4326))[,1] 
data$lat = st_coordinates(data %>% st_transform(4326))[,2] 

data2 <- travel_trip[[1]]$trip
data2$id_order <- order(as.integer(path))
data2$lng = st_coordinates(data %>% st_transform(4326))[,1] 
data2$lat = st_coordinates(data %>% st_transform(4326))[,2] 

route <- osrmRoute(data[data$id_order %in% 1:2,])
 
  
  
  
# Plot a map with the data and overlay the optimal path
data %>%
  arrange(id_order) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    ~lng,
    ~lat,
    fillColor = 'red',
    fillOpacity = 0.5,
    stroke = FALSE
  ) %>%
  addPolylines(~lng, ~lat)


data2 %>%
  arrange(id_order) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    ~lng,
    ~lat,
    fillColor = 'red',
    fillOpacity = 0.5,
    stroke = FALSE
  ) %>%
  addPolylines(~lng, ~lat)






#---------------------------------------------------------------------------------------
## NICE CODE STOPS HERE
#---------------------------------------------------------------------------------------




