# ATI data curation for SoWT analysis July 2020 Ewan McHenry


# library ----
library(sf) # for gis
library("rnrfa") # to convert os gb grif ref
library(rgdal) # to convert lat long to os gb
library(tidyverse)
library("rnaturalearth") # map of countries of the entire world
library("rnaturalearthdata")
library("rgeos")
library(ggplot2)
library(stringr) # for str_detect()
library(U.utilities) # devtools::install_github("Ewan McHenry/U.utilities")

# library(geosphere) # for coordinate conversion
# library(proj4)# for coordinate conversion
library(igr) # for coordinate conversion
library(rnrfa) # for coordinate conversion


ati_filename <- "ATI - 2024_08_05.csv"
# load data ----

ati <- read.csv(paste0(gis.wd, "\\Data\\ATI\\downloaded data\\", ati_filename)) %>%
  mutate(Longitude_orig = Longitude, 
         Latitude_orig = Latitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(Longitude = Longitude_orig, Latitude = Latitude_orig)
world <- ne_countries(scale = "large", returnclass = "sf")
map_sov <- ne_countries(scale = "large", sovereignty =  c("United Kingdom", "Ireland", "France"), returnclass = "sf")
uk_ireland <- ne_countries(scale = "large", country = c("United Kingdom", "Ireland", "Isle of Man", "Guernsey"), returnclass = "sf")
uk_ireland_france <- ne_countries(scale = "large", country = c("United Kingdom", "Ireland", "Isle of Man", "Guernsey","France"), returnclass = "sf")
bbox_uk_ireland <- uk_ireland %>% st_bbox() %>% st_as_sfc() %>% st_buffer(0) %>% st_bbox()%>% st_as_sfc()
mapping.area <- st_intersection(map_sov, bbox_uk_ireland)

# data curation ----
# Replace all "" with NA in character columns only
ati <- ati %>% 
  mutate(across(where(is.character), ~na_if(.x, "")))
ati$CountryName[ati$CountryName == "Northern Ireland" ] <- "N. Ireland" # sync northern ireland naming
ati <- subset(ati, select = -X) #remove X column
ati$CreatedDate <- as.Date(ati$CreatedDate, format = "%d/%m/%Y") # Convert CreatedDate to Date format
ati$SurveyDate <- as.Date(ati$SurveyDate, format = "%d/%m/%Y") # Convert SurveyDate to Date format
ati$VerifiedDate <- as.Date(ati$VerifiedDate, format = "%d/%m/%Y") # Convert VerifiedDate to Date format
ati$GridReference <- gsub(" ", "", ati$GridReference)# remove all spaces from grid reference


## manuel fixes ----
ati$GridReference[ati$GridReference == "S025879030"] = "SO25879030"
ati$GridReference[ati$GridReference == "J340694" & ati$CountyName == "Cheshire"] = "SJ406940" # added letter, easy using town name        
ati$CountryName[ati$GridReference == "J3413969393" & ati$SiteName == "Belvoir Park"] = "N. Ireland" # added letter, easy using town name
ati$GridReference[ati$GridReference %in%c( "ZQ1109209934", "ZQ1106710004") ] = NA # not so interested in ... what i assume is the french grid system??


# some aproximated based on site name the last half is right and first half of first half seems good - these need verified
ati$GridReference[ati$GridReference == "NO238124462" & ati$SiteName == "Megginch Castle"] = "NO2381024462" # looks sensible enough https://gridreferencefinder.com?gr=NO2381024462|NO2381024462|1&t=NO2381024462&v=r
ati$GridReference[ati$GridReference == "NO239522448" & ati$SiteName == "Megginch Castle"] = "NO2395024448" # doesnt look right at all originally https://gridreferencefinder.com?gr=NO2390522448|NO2390522448|1&t=NO2390522448&v=r look som eliberties, assumed a mis-placed 4
ati$GridReference[ati$GridReference == "NO244624627" & ati$SiteName == "Megginch Castle"] = "NO2446024627" # looks okay https://gridreferencefinder.com?gr=NO2446024627|NO2446024627|1&t=NO2446024627&v=r
ati$GridReference[ati$GridReference == "NX850589132" & ati$LocationAccessComments  == "Within formal gardens of Drumlanrig Castle"] = "NX8505899132" # aint no avenue, but looks okay  https://gridreferencefinder.com?gr=NX8505899132|NX8505899132|1&t=NX8505899132&v=r
ati$GridReference[ati$GridReference == "SO265818884" & ati$CountyName  == "Powys"] = "SO2658188840" # based on comments RE Nut Wood https://gridreferencefinder.com?gr=SO2658188840|SO2658188840|1&t=SO2658188840&v=r
ati$GridReference[ati$GridReference == "SW795343626" & ati$CountyName  == "Cornwall"] = "SW7953436260" # bit of a guess, but in right area SW795343626
ati$GridReference[ati$GridReference == "TQ157452042" & ati$SiteName  == "Knepp Castle Park"] = "TQ1574520420" #
ati$GridReference[ati$GridReference == "SZ02391256" ] = "SU02391256" # based on comments RE all hallows 
ati$GridReference[ati$GridReference == "SZ02401260" ] = "SU02401260" # based on comments RE all hallows 
ati$GridReference[ati$GridReference == "SZ03201195" ] = "SU03201195" # based on comments RE church yard https://gridreferencefinder.com?gr=SU0320011950|SU03201195|1&t=SU03201195&v=r
ati$GridReference[ati$GridReference == "SX9977100448" ] = "SS9977100448" # its close to the mentioned park, aye, and it looks in a hedge



# remove these with locations that cant be worked out
ati <- ati[!ati$GridReference %in% c("NY952304530", "NY955504542","NY985904685" , "NY955804554", # all clearly close together, but te closest I can figure out is up on a grouse moor somewhere https://gridreferencefinder.com?gr=NY9555504542|NY9555504542|1&t=NY9555504542&v=r
                                     "SE515215860", "SE517115849", "SE517165851", "SE518025444", "SE518235844", "SE518575846", "SE519015848",  # cant get anywhere near to "Wortley Hall" with these by trying missing from end etc
                                     "SV0000000000", # good luck!
                                     "SX46640907",
                                     "SY59625152", # in gurnesey
                                     "TR9936703987", "TR8018013586", # in france
                                     "SY078034" # comments say in NI, but grid ref is in the English channel, cant make much of it
),]


ati <- ati[!ati$CountyName %in% c("Guernsey" # 
),]

# longitude was wrong, edit geometry manually
ati$geometry[ati$GridReference == "SJ406940"] <- st_sfc(st_point(c(-2.8956647, 53.439411)), crs = 4326) # was way over in eastern europe


# # Misc manuel edits on previous version - to be reviewed
# ati = ati [!ati$TreeNumber %in% c(83027),]# duplicate foudn when fixing locations in water
# 
# # shift a few locations that are in sea etc, that dont join to countries shapefile.
# ati$eastings[ati$TreeNumber == 114516 ] = ati$eastings[ati$TreeNumber == 114516 ] -20
# ati$northings[ati$TreeNumber == 11137 ] = ati$northings[ati$TreeNumber == 11137 ] - 20
# ati$eastings[ati$TreeNumber == 59835 ] = ati$eastings[ati$TreeNumber == 59835 ] +20
# ati$northings[ati$TreeNumber == 142657 ] = ati$northings[ati$TreeNumber == 142657 ] -5
# ati$northings[ati$TreeNumber == 67830 ] = ati$northings[ati$TreeNumber == 67830 ] +2000  # a fair bit off, but comments suggest aprox in right area
# ati$northings[ati$TreeNumber == 83102 ] = ati$northings[ati$TreeNumber == 83102 ] -30
# ati$northings[ati$TreeNumber == 10754 ] = ati$northings[ati$TreeNumber == 10754 ] +300
# ati$northings[ati$TreeNumber == 31668 ] = ati$northings[ati$TreeNumber == 31668 ] +2
# ati$eastings[ati$TreeNumber == 96334 ] = ati$eastings[ati$TreeNumber == 96334 ] -10
# ati$eastings[ati$TreeNumber == 190080 ] = ati$eastings[ati$TreeNumber == 190080 ] -10
# ati$northings[ati$TreeNumber == 174419 ] = ati$northings[ati$TreeNumber == 174419 ] +5
# ati$eastings[ati$TreeNumber == 174418 ] = ati$eastings[ati$TreeNumber == 174418 ] -10
# ati$northings[ati$TreeNumber == 198591 ] = ati$northings[ati$TreeNumber == 198591 ] -10
# ati$northings[ati$TreeNumber == 198589 ] = ati$northings[ati$TreeNumber == 198589 ] -10
# ati$northings[ati$TreeNumber %in% 62953:62956 ] = ati$northings[ati$TreeNumber == 62953 ] -15
# ati$northings[ati$TreeNumber == 88930 ] = ati$northings[ati$TreeNumber == 88930 ] -5
# ati$northings[ati$TreeNumber %in% 62933:62936 ] = ati$northings[ati$TreeNumber == 62933 ] -25
# ati$eastings[ati$TreeNumber == 102547 ] = ati$eastings[ati$TreeNumber == 102547 ] -40
# ati$eastings[ati$TreeNumber == 2387 ] = ati$eastings[ati$TreeNumber == 2387 ] -20
# ati$northings[ati$TreeNumber == 143464 ] = ati$northings[ati$TreeNumber == 143464  ] -5
# ati$eastings[ati$TreeNumber == 110290 ] = ati$eastings[ati$TreeNumber == 110290 ] -5
# ati$eastings[ati$TreeNumber == 5762 ] = ati$eastings[ati$TreeNumber == 5762 ] +5
# 
# # shift a few more that are aree wee bit oot aa teh waa parleymynt constituenees
# ati$eastings[ati$TreeNumber %in% 62933:62936 ] = ati$eastings[ati$TreeNumber == 62933 ] + 10
# ati$northings[ati$TreeNumber == 96074 ] = ati$northings[ati$TreeNumber == 96074  ] - 5
# ati$northings[ati$TreeNumber == 83102 ] = ati$northings[ati$TreeNumber == 83102  ] -  10
# ati$northings[ati$TreeNumber %in% 62953:62956 ] = ati$northings[ati$TreeNumber == 62953 ] - 15
# ati$northings[ati$TreeNumber == 2387 ] = ati$northings[ati$TreeNumber == 2387 ] + 5
# ati$northings[ati$TreeNumber == 194858 ] = ati$northings[ati$TreeNumber == 194858 ] -15
# ati$eastings[ati$TreeNumber == 190080 ] = ati$eastings[ati$TreeNumber == 190080 ] - 15
# ati$northings[ati$TreeNumber == 67643 ] = ati$northings[ati$TreeNumber == 67643 ] + 5
# ati$northings[ati$TreeNumber == 187576 ] = ati$northings[ati$TreeNumber == 187576 ] + 5
# ati$eastings[ati$TreeNumber == 5368 ] = ati$eastings[ati$TreeNumber == 5368 ] - 5
# ati$eastings[ati$TreeNumber == 186026 ] = ati$eastings[ati$TreeNumber == 186026 ] - 5
# ati$northings[ati$TreeNumber == 145378 ] = ati$northings[ati$TreeNumber == 145378 ] + 5
# 


## syncronising irish grid refs ------ 
one.letter <- grepl("^[A-Za-z]{1}", ati$GridReference) & !grepl("^[A-Za-z]{2}", ati$GridReference) # grid ref has 1 letter - theyre irish grid

# ## check no non-Irish have only 1 letter? 
# ati$CountryName[one.letter  ] %>% table() # should be no non-Irish trees
# ati[one.letter & ati$CountryName == "",]
# ati$GridReference[one.letter & ati$CountryName == "England"  ] 


# # some are in UK, some are in polanmd or somehwere... wrong long/lat
# ggplot() + geom_sf(data = mapping.area) +
#   geom_sf(data = ati[one.letter,],
#           aes(color = CountryName)) + labs(title = "All trees")

# ## are there any NI records not with 1 letter? - yes, they are done on GB grid... presumably recorded by penguins 
# ati$GridReference[!one.letter & ati$CountryName == "N. Ireland"]

## Separate out the different grid ref systems, reproject and re-join 
### irish grid refs ----
ati_irl_gridref <- st_igr_as_sf(ati[one.letter,], "GridReference") %>% 
  as.data.frame() %>%
  st_as_sf(coords = c("x", "y"), crs = 29902) %>% 
  st_transform(4326)

# # check - are they all in island of irealnd
# ggplot() + geom_sf(data = mapping.area) +
#   geom_sf(data = ati_irl_gridref)


### GB grid refs ----
ati_gb_gridref <- ati[!one.letter & !is.na(ati$GridReference),]
# lat long from gb grid ref
ati_gb_gridref$easting <- osg_parse(grid_refs = ati_gb_gridref$GridReference)$easting
ati_gb_gridref$northing <- osg_parse(grid_refs = ati_gb_gridref$GridReference)$northing

# make location from grid ref info
ati_gb_gridref <- ati_gb_gridref %>% 
  as.data.frame() %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% 
  st_transform(4326)
# #check - lat and long not changed much
# og_longlat <- cbind(ati_gb_gridref$Longitude, ati_gb_gridref$Latitude)
# nu_longlat <- ati_gb_gridref %>% st_coordinates() %>% as.data.frame() %>%
#   setNames(c("Longitude", "Latitude"))
# diff_longlat <- og_longlat - nu_longlat
# ati_gb_gridref$GridReference[which(rowSums(diff_longlat) > 0.01)] # SJ406940 is fine - changed it manuelly above

# # check - none at sea
# ati_gb_gridref_on_land <- st_intersection(ati_gb_gridref, mapping.area %>% st_buffer(0.1)) # buffer to avoid edge effects
# ati_gb_gridref_at_sea <- ati_gb_gridref[!ati_gb_gridref$Id %in% ati_gb_gridref_on_land$Id,]
# # plot map of those outside uk_ireland_france
# ggplot() + geom_sf(data = mapping.area) +
#   geom_sf(data = ati_gb_gridref_at_sea,
#           aes(col = CountyName )) + labs(title = "All trees")
# 
# ati_gb_gridref_at_sea[, c("GridReference", "LocalName", "CountyName", "CountryName", "SiteName", "SurroundingsNames", "LocationAccessComments", "LivingStatusName", "TreeFormName")]

### no grid refs ----
ati_no_gridref <- ati[is.na(ati$GridReference),]

# # check theyre all international
# ati_no_gridref$CountryName %>% unique() %>% sort()
# ggplot() + geom_sf(data = mapping.area) +
#   geom_sf(data = ati_no_gridref) + labs(title = "All trees")

# # find all outside bbox_uk_ireland
# not_uk_ati <- ati[st_difference(ati, bbox_uk_ireland),]
# ggplot() + geom_sf(data = mapping.area) +
#   geom_sf(data = not_uk_ati)+ labs(title = "All trees")



## recombine ----
ati <- rbind(ati_gb_gridref, ati_irl_gridref)

# ## check - no odd number of numbers in grid ref? ----
# n.numbers_in_gridref <- str_count(ati$GridReference, "[0-9]") # number of numbers in grid ref
# ati[n.numbers_in_gridref %% 2 == 1,  # odd number of numbers in grid ref
#     c("GridReference", "LocalName", "CountyName", "CountryName", "SiteName", "SurroundingsNames", "LocationAccessComments", "LivingStatusName", "TreeFormName") ] 

# # check all good
# ggplot() + geom_sf(data = mapping.area) +
#   geom_sf(data = ati) + labs(title = "All trees")


## pruning based on location data ----



ati <- st_intersection( ati, bbox_uk_ireland) # keep only trees within UK and Ireland

# where there is no GridReference? - they are all outside UK ----
# ggplot()+
#   geom_sf(data = mapping.area) +
#   geom_sf(data = ati[is.na(ati$GridReference) ,],
#                                                      color = "red") + labs(title = "All trees")



# # those wronguns arent in poland anymore, thats good!
# ggplot() + geom_sf(data = mapping.area) + 
#   geom_sf(data = ati_irl_gridref,
#           aes(color = CountryName)) + labs(title = "All trees")
# 
# ggplot() + geom_sf(data = mapping.area) + 
#   geom_sf(data = ati[one.letter,],
#           aes(color = CountryName)) + labs(title = "All trees")

# # the differences between corrected and original long/lat are very small, except where theyre meant to be big
# ati_irl_gridref <- ati_irl_gridref %>% mutate(Longitude2 = st_coordinates(.)[,1], Latitude2 = st_coordinates(.)[,2])
# ati_irl_gridref$diiff.long <- ati_irl_gridref$Longitude - ati_irl_gridref$Longitude2
# ati_irl_gridref$diiff.lat <- ati_irl_gridref$Latitude - ati_irl_gridref$Latitude2
# cbind(ati_irl_gridref$GridReference, ati_irl_gridref$Longitude, ati_irl_gridref$Longitude2, ati_irl_gridref$diiff.long, ati_irl_gridref$Latitude, ati_irl_gridref$Latitude2, ati_irl_gridref$diiff.lat) [order(-ati_irl_gridref$diiff.long),]



# ati[one.letter, c("TreeNumber", "Longitude", "Latitude", "GridReference", "CountryName")] %>%

# identify which GridReference are using irish grid ref system





# some dates are missing which are recorded and verified by the same person, and there is a verification date. For these I have made the origianl date teh verified date
# # n = 39, most of them in 1st half of 2007
ati$date1[!is.na(ati$date.verified) & is.na(ati$date1) & ati$OriginalRecorderID == ati$VerifierUserID]= ati$date.verified[!is.na(ati$date.verified) & is.na(ati$date1) & ati$OriginalRecorderID == ati$VerifierUserID]
# some verification dates are before OG record
ati[which(ati$CreatedDate > ati$VerifiedDate ),c("Id", "CreatedDate", "VerifiedDate", "SurveyDate")]
dim (ati[which(ati$CreatedDate > ati$VerifiedDate ),c("CreatedDate", "VerifiedDate", "SurveyDate")])
dim (ati[which(ati$CreatedDate > ati$VerifiedDate ),c("CreatedDate", "VerifiedDate")])

save(ati, file = paste0(gis.wd, "\\Data\\ATI\\ati", Sys.Date(),"curated_from",ati_filename, ".RData"))

