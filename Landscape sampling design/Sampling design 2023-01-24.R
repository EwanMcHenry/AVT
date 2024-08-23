##------ Tue Jan 24 12:19:12 2023 ------##
# EMcH

# samplign design for scotland AVT survey

# libraries ----

library(sf) # for gis
library(tidyverse)
library(units) # for set_units()
library(ggpubr)
library(raster)
library(rgis) # remotes::install_github("Pakillo/rgis") # for fast_mask # -- other of same name... devtools::install_github('jgcri/rgis')
library(exactextractr) #exact_extract()

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

countries = st_read(paste0(gis.wd,"\\Data\\administrative boundaries\\Countries\\R.5countries.simp100m.shp"))%>% st_transform( 27700) %>% arrange(name)
lcm19.rast25.gb = raster(paste0(gis.wd, "\\Data\\LCM\\LCM2019\\25m land parcel\\gb2019lcm25m.tif"))

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

# exploration ----
## map influence of each numeric covariate ----

# variables
i = 1
fill.scale.title = "Effect"
grid = pred.scot.main %>% st_simplify(dTolerance = 100)
grid.df = as.data.frame(grid)

eff.cols = grep("influ_",names(grid.df))[colSums(grid.df[,grepl("influ_",names(grid.df))]) != 0] # the coloums we want to plot

col.lim.var.names = names(grid.df)[eff.cols]
nice.covar.names = c("Altitude", "Moated site dist", "Medieval deer park dist", "Nat. Trust dist",
                           "Cities dist", "Towns dist", "Maj. road dist", "Woodland area", 
                           "AWI area", "Trad. orchard area", "Woodpasture area", "Watercourse dist",
                           "Minor road dist")

# col limts for plots
col.lim.effects =  c(as.matrix(as.data.frame(grid) [,col.lim.var.names] )) # all covars effects in df
abs.col.lim = max(abs(range(col.lim.effects))) # range of all col vars, abs + and - to allow comparision of effects between covars and + and - 
colour.limits = c(-abs.col.lim, abs.col.lim) 
low.col = E.cols$connectiv.low
high.col = E.cols$connectiv.high

eff.maps <- lapply(seq_along(eff.cols), FUN = function(i) {

  var.name = names(pred.scot.main)[eff.cols[i]]
  main.title = paste( nice.covar.names[i] , "Influence")
  sub.title = NULL
  
  var = as.data.frame(grid)[,names(grid) == var.name]
  
  # colour limits & plot
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
  eff.map
})

comparison.plot = ggarrange(plotlist =  eff.maps,
                             ncol = 5, nrow = 3, common.legend = TRUE, legend = "bottom")

ggsave(comparison.plot, filename = "analysis\\sampling design outputs\\strata_predication.effects_map_comparison.pdf",
       height = 10.80 , width = 19.20, units = "in")

ggsave(comparison.plot, filename = "analysis\\sampling design outputs\\strata_predication.effects_map_comparison.png",
       height = 10.80 , width = 19.20, units = "in")

 






# fix upper 1% very high to 99% ceiling
top.1per = quantile(pred.scot.main$NEGBINC, probs = 0.99)

hist(pred.scot.main$NEGBINC, breaks = 50,
     xlab = "strata: predicted N trees", main = NULL)

hist(pred.scot.main$NEGBINC[pred.scot.main$NEGBINC < top.1per], breaks = 50,
     xlab = "strata: predicted N trees", main = NULL)
plot(density(pred.scot.main$NEGBINC[pred.scot.main$NEGBINC < top.1per], n = 100))

# strata bins ----
max.bin = 3
n_bins = 5

# equal strat distance bins 
bin.bottom = seq(from = 0, to = max.bin, length = n_bins) 
pred.scot.main$bin_id = 1
for ( i in 2:length(bin.bottom)){
  pred.scot.main$bin_id [pred.scot.main$NEGBINC > bin.bottom[i]] = i
}
pred.scot.main$bin_id = pred.scot.main$bin_id %>% as.factor()

pred.scot.main = pred.scot.main %>%
  group_by(bin_id) %>%
  mutate(bin.count = length(bin_id)) %>% 
  mutate(samp.prob = 1/bin.count)

# split first strata in half
alt.bin.bottm = c(bin.bottom[1], mean(bin.bottom[1:2]) , seq(from = bin.bottom[2],
                                                             to = bin.bottom[n_bins],
                                                             length = n_bins - 2))
pred.scot.main$alt_bin_id = 1
for ( i in 2:length(alt.bin.bottm)){
  pred.scot.main$alt_bin_id [pred.scot.main$NEGBINC > alt.bin.bottm[i]] = i
}
pred.scot.main$alt_bin_id = pred.scot.main$alt_bin_id %>% as.factor()
pred.scot.main = pred.scot.main %>%
  group_by(alt_bin_id) %>%
  mutate(alt_bin.count = length(alt_bin_id)) %>% 
  mutate(alt_samp.prob = 1/alt_bin.count)
# double sampling relative sampling effort of first strata
pred.scot.main$alt01_samp.prob =   pred.scot.main$samp.prob
pred.scot.main$alt01_samp.prob[pred.scot.main$bin_id == 1] =   pred.scot.main$samp.prob[pred.scot.main$bin_id == 1]*2

# table relative effort to strata ----
table(pred.scot.main$bin_id)/length(pred.scot.main$bin_id)
table(pred.scot.main$alt_bin_id)/length(pred.scot.main$alt_bin_id)
table(pred.scot.main$bin_id)/length(pred.scot.main$bin_id)/ c(2, rep(1, 4)) # prop of sampling sq relative to sampling effort

# sample ----
samp.size = 100
pred.scot.main$sample.priority = order(sample(pred.scot.main$id, 
                                              size = length(pred.scot.main$id),
                                              replace = F,
                                              prob = pred.scot.main$samp.prob))



pred.scot.main = pred.scot.main %>% 
  mutate(sample.priority = order(sample(id, size = length(id),
                                        replace = F, prob = samp.prob)),
         alt_sample.priority = order(sample(id, size = length(id),
                                        replace = F, prob = alt_samp.prob)),
         alt01_sample.priority = order(sample(id, size = length(id),
                                            replace = F, prob = alt01_samp.prob)),
         )

hist(pred.scot.main$NEGBINC[pred.scot.main$NEGBINC<3.5],breaks = 6,
     xlab = "strata: predicted N trees", main = NULL)

pred.scot.main = pred.scot.main %>% 
  mutate(sampled = sample.priority <= samp.size,
         alt_sampled = alt_sample.priority <= samp.size,
         alt01_sampled = alt01_sample.priority <= samp.size
         )

pred.scot.main.sampled = pred.scot.main[pred.scot.main$sample.priority <= samp.size,]

# water cover ----
hist(pred.scot.main.sampled$Water)/max(pred.scot.main.sampled$boundary_area))

# simulate from ATI

pred.scot.main.sampled$Count_

test.glm = glm(Count_ ~ NEGBINC, data = pred.scot.main.sampled, family = "poisson")

test.pred = predict(test.glm, newdata = pred.scot.main, type = "link" )

# save objects ----
st_write(pred.scot.main,  "analysis\\sampling design outputs\\sample_priority.shp")
st_write(pred.scot.main.sampled,  "analysis\\sampling design outputs\\sampled100.shp")

# maps ----

# map - prediction N AVT ----
var.name = "NEGBINC" # small hex area scales for
col.lim.var.name = "NEGBINC" # this solves issue where small portion-hexes with high cover where skewing colour scale. If they are high they get high colour, but dont mess with the scale
line.size <- 0

main.title = paste("Rough AVT abundance prediction")
sub.title = NULL
fill.scale.title = "N AVTs predicted"

grid = pred.scot.main
var = as.data.frame(grid)[,names(grid) == var.name]

colour.limits = c(0,top.1per) 


gg.pred.abundance <- ggplot(data=pred.scot.main) +
  geom_sf(mapping = aes(fill = NEGBINC),  colour = NA, size = line.size) +
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
  labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
  )+
  coord_sf(xlim = pad.lim(st_bbox(grid) [c(1,3)]), ylim = pad.lim(st_bbox(grid) [c(2,4)]), expand = FALSE) +
  theme_map() +
  theme(legend.position = "bottom"
  ) 

# strata ----
## hist strata
pred.scot.main %>% 
  as.data.frame() %>% 
  count(bin = factor(bin_id)) %>% 
  mutate(pct = (prop.table(n) %>% round(., digits = 2))*100) %>% 
  ggplot(., aes(x = bin, y = n, label = paste(pct, "%") ))+
  geom_col() +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(x = "strata", y = "N grid squares", title = "All Scotland population") +
  theme_pubr()

## map strata

var.name <- col.lim.var.name <-  "bin_id" # small hex area scales for
line.size <- 0

main.title = paste("Strata")
sub.title = NULL
fill.scale.title = "sampling strata"

grid = pred.scot.main
var = as.data.frame(grid)[,names(grid) == var.name]

colour.limits = c(0,max(var)) 


#gg.strata <- 
  ggplot(data=pred.scot.main) +
  geom_sf(mapping = aes(fill = bin_id),  colour = NA, size = line.size) +
  scale_fill_viridis_d(name = fill.scale.title,
                       #option = "magma",direction = -1 
                       guide = guide_legend(
                         direction = "horizontal", barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"), draw.ulim = F,
                         title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
  labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
  )+
  coord_sf(xlim = pad.lim(st_bbox(grid) [c(1,3)]), ylim = pad.lim(st_bbox(grid) [c(2,4)]), expand = FALSE) +
  theme_map() +
  theme(legend.position = "bottom"
  ) 


# hist 




#write
st_write(pred.scot.main)


ggplot() +
  theme_map(leg.tit.size = 11,
            legend.position = "right")