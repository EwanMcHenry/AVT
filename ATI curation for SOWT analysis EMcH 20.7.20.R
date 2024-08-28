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


# load data ----
ati <- read.csv(paste0(gis.wd, "\\Data\\ATI\\downloaded data\\ATI - 2024_08_05.csv")) %>%
  mutate(Longitude_orig = Longitude, 
         Latitude_orig = Latitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(Longitude = Longitude_orig, Latitude = Latitude_orig)
world <- ne_countries(scale = "medium", returnclass = "sf")
uk_ireland <- ne_countries(scale = "medium", country = c("United Kingdom", "Ireland", "Isle of Man"), returnclass = "sf")
uk_ireland_france <- ne_countries(scale = "medium", country = c("United Kingdom", "Ireland", "Isle of Man", "France"), returnclass = "sf")
bbox_uk_ireland <- uk_ireland %>% st_bbox() %>% st_as_sfc() %>% st_buffer(0) %>% st_bbox()%>% st_as_sfc()
mapping.area <- st_intersection(uk_ireland_france, bbox_uk_ireland)

# data curation ----
# Replace all "" with NA in character columns only
ati <- ati %>% 
  mutate(across(where(is.character), ~na_if(.x, "")))
ati$CountryName[ati$CountryName == "Northern Ireland" ] <- "N. Ireland" # sync northern ireland naming
ati <- subset(ati, select = -X) #remove X column
ati$CreatedDate <- as.Date(ati$CreatedDate, format = "%d/%m/%Y") # Convert CreatedDate to Date format
ati$SurveyDate <- as.Date(ati$SurveyDate, format = "%d/%m/%Y") # Convert SurveyDate to Date format
ati$VerifiedDate <- as.Date(ati$VerifiedDate, format = "%d/%m/%Y") # Convert VerifiedDate to Date format

## manuel fixes ----
ati$GridReference[ati$GridReference == "S025879030"] = "SO25879030"
ati$GridReference[ati$GridReference == "J340694" & ati$CountyName == "Cheshire"] = "SJ406940" # added letter, easy using town name        
ati$CountryName[ati$GridReference == "J3413969393" & ati$SiteName == "Belvoir Park"] = "N. Ireland" # added letter, easy using town name

# some aproximated based on site name the last half is right and first half of first half seems good
ati$GridReference[ati$GridReference == "NO238124462" & ati$SiteName == "Megginch Castle"] = "NO2381024462" # looks sensible enough https://gridreferencefinder.com?gr=NO2381024462|NO2381024462|1&t=NO2381024462&v=r
ati$GridReference[ati$GridReference == "NO239522448" & ati$SiteName == "Megginch Castle"] = "NO2395024448" # doesnt look right at all originally https://gridreferencefinder.com?gr=NO2390522448|NO2390522448|1&t=NO2390522448&v=r look som eliberties, assumed a mis-placed 4
ati$GridReference[ati$GridReference == "NO244624627" & ati$SiteName == "Megginch Castle"] = "NO2446024627" # looks okay https://gridreferencefinder.com?gr=NO2446024627|NO2446024627|1&t=NO2446024627&v=r
ati$GridReference[ati$GridReference == "NX850589132" & ati$LocationAccessComments  == "Within formal gardens of Drumlanrig Castle"] = "NX8505899132" # aint no avenue, but looks okay  https://gridreferencefinder.com?gr=NX8505899132|NX8505899132|1&t=NX8505899132&v=r
# remove these with locations that cant be worked out
ati <- ati[!ati$GridReference %in% c("NY952304530", "NY955504542","NY985904685" , "NY955804554", # all clearly close together, but te closest I can figure out is up on a grouse moor somewhere https://gridreferencefinder.com?gr=NY9555504542|NY9555504542|1&t=NY9555504542&v=r
                                     "SE515215860", "SE517115849" # cant get anywhere near to "Wortley Hall" with these
),]


## pruning based on location data ----

ati <- st_intersection( ati, bbox_uk_ireland) # keep only trees within UK and Ireland

# where there is no GridReference? - they are all outside UK ----
ggplot()+
  geom_sf(data = mapping.area) +
  geom_sf(data = ati[is.na(ati$GridReference) ,],
                                                     color = "red") + labs(title = "All trees")

# location curation ----

## Where grid reference only has 1 letter in its first two characters - theyre all irish grid refs ------ 
GridReferenceLength = nchar(as.character(ati$GridReference))
one.letter <- grepl("^[A-Za-z]{1}", ati$GridReference) & !grepl("^[A-Za-z]{2}", ati$GridReference) # grid ref has 1 letter

# # some are in UK, some are in polanmd or somehwere... wrong long/lat
ggplot() + geom_sf(data = mapping.area) +
  geom_sf(data = ati[one.letter,],
          aes(color = CountryName)) + labs(title = "All trees")

# ## are there any NI records not with 1 letter? - yes, they are done on GB grid... presumably recorded by penguins 
# ati$GridReference[!one.letter & ati$CountryName == "N. Ireland"]

## Some non-Irish have only 1 letter? 
ati$CountryName[one.letter  ] %>% table() # should be no non-Irish trees
# ati[one.letter & ati$CountryName == "",]
# ati$GridReference[one.letter & ati$CountryName == "England"  ] 


## Separate out the two different grid ref systems, reproject and re-join ----
ati_irl_gridref <- st_igr_as_sf(ati[one.letter,], "GridReference") %>% 
  as.data.frame() %>%
  st_as_sf(coords = c("x", "y"), crs = 29902) %>% 
  st_transform(4326)

ati_gb_gridref <- ati[!one.letter,]

ati <- rbind(ati_gb_gridref, ati_irl_gridref)


## odd number of numbers in grid ref?
n.numbers_in_gridref <- str_count(ati$GridReference, "[0-9]") # number of numbers in grid ref
ati[n.numbers_in_gridref %% 2 == 1,  # odd number of numbers in grid ref
    c("GridReference", "LocalName", "CountyName", "CountryName", "SiteName", "SurroundingsNames", "LocationAccessComments", "LivingStatusName", "TreeFormName") ] %>% 
  arrange(GridReference) 
# # some aproximated based on site name the last half is right and first half of first half seems good
# ati$GridReference[ati$GridReference == "NO238124462" & SiteName == "Megginch Castle"] = "NO2381024462" # looks sensible enough https://gridreferencefinder.com?gr=NO2381024462|NO2381024462|1&t=NO2381024462&v=r
# ati$GridReference[ati$GridReference == "NO239522448" & SiteName == "Megginch Castle"] = "NO2395024448" # doesnt look right at all originally https://gridreferencefinder.com?gr=NO2390522448|NO2390522448|1&t=NO2390522448&v=r look som eliberties, assumed a mis-placed 4
# ati$GridReference[ati$GridReference == "NO244624627" & SiteName == "Megginch Castle"] = "NO2446024627" # looks okay https://gridreferencefinder.com?gr=NO2446024627|NO2446024627|1&t=NO2446024627&v=r
# ati$GridReference[ati$GridReference == "NX850589132" & LocationAccessComments  == "Within formal gardens of Drumlanrig Castle"] = "NX8505899132" # aint no avenue, but looks okay  https://gridreferencefinder.com?gr=NX8505899132|NX8505899132|1&t=NX8505899132&v=r
# 
# # remove these with locations that cant be worked out
# ati <- ati[!ati$GridReference %in% c("NY952304530", "NY955504542","NY985904685" , "NY955804554", # all clearly close together, but te closest I can figure out is up on a grouse moor somewhere https://gridreferencefinder.com?gr=NY9555504542|NY9555504542|1&t=NY9555504542&v=r
#                                      "SE515215860", "SE517115849" # cant get anywhere near to "Wortley Hall" with these
#                                      ),]
# 





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





## find GridReference chr string length
### nchar(as.character(ati$GridReference)) %>% is.na() %>% table()
### nchar(as.character(ati$GridReference))%>% table()

# plot longitudes and latitudes of trees where GridReference does not starts with exactly two letters followed by numbers
letters.formated <- grepl("^[A-Za-z]{2}", ati$GridReference)

# plot(ati$Longitude[!letters.formated], ati$Latitude[!letters.formated], xlab = "Longitude", ylab = "Latitude", main = "Trees with GridReference not starting with exactly two letters followed by numbers")

# Keep only rows where GridReference starts with exactly two letters followed by numbers
ati <- ati[grepl("^[A-Za-z]{2}", ati$GridReference), ]





# tree without location info
ati = ati[!ati$TreeNumber ==  96976,] 



# date ----
first.date = "2002-01-01"
ati$date1 = as.Date(ati$DateOfSurvey, format = "%B %d, %Y %H:%M:%S")
ati$date.verified = as.Date(ati$VerifiedTime, format = "%B %d, %Y %H:%M:%S")

# some dates are missing which are recorded and verified by the same person, and there is a verification date. For these I have made the origianl date teh verified date
# # n = 39, most of them in 1st half of 2007
ati$date1[!is.na(ati$date.verified) & is.na(ati$date1) & ati$OriginalRecorderID == ati$VerifierUserID]= ati$date.verified[!is.na(ati$date.verified) & is.na(ati$date1) & ati$OriginalRecorderID == ati$VerifierUserID]
# some verification dates are before OG record
ati[which(ati$date1> ati$date.verified ),c("TreeNumber",#"DateOfSurvey" , 
                                           "date1", "date.verified" , "OriginalRecorderID" , "VerifierUserID") ]
# fixed a few going through and looking at created date
ati$date1[ati$TreeNumber == 30216 ] = as.Date("23-01-2008" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 195621] = as.Date("31-08-2019" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 82001 ] = as.Date("25-05-2010" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 46379 ] = as.Date("28-04-2009" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 54512 ] = as.Date("12-03-2009" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 46370 ] = as.Date("28-04-2009" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 10018 ] = as.Date("24-01-2010" , format ="%d-%m-%Y")
# in teh meantime we found this verifier was same and OG recorder
ati$OriginalRecorderID[ati$TreeNumber == 10018 ] = 6906
# and others with same verifier and recorder
ati$date.verified[ati$date1> ati$date.verified & ati$OriginalRecorderID == ati$VerifierUserID] = ati$date1[ati$date1> ati$date.verified & ati$OriginalRecorderID == ati$VerifierUserID]

ati[which(is.na(ati$date1)),c("TreeNumber",#"DateOfSurvey" , 
                              "date1", "date.verified" , "OriginalRecorderID" , "VerifierUserID") ]


ati$date1[ati$TreeNumber == 178624] = as.Date("10-04-2018" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 74459] = as.Date("10-04-2018" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 4711] = as.Date("10-04-2018" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 182934] = as.Date("10-04-2018" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 175091] = as.Date("10-04-2018" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 183447] = as.Date("10-04-2018" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 183676] = as.Date("10-04-2018" , format ="%d-%m-%Y")
ati$date1[ati$TreeNumber == 2627] = as.Date("10-04-2018" , format ="%d-%m-%Y")
# some of those verified dates wrong and need changed if they are before this
ati$date.verified[ati$TreeNumber == 2627] = ati$date1[ati$TreeNumber == 2627]
ati$date.verified[ati$TreeNumber == 4711 ] = ati$date1[ati$TreeNumber == 4711 ]
ati$date.verified[ati$TreeNumber == 74459 ] = ati$date1[ati$TreeNumber == 74459 ]


# origianl recorder ID
# 191 have no original id but are last verified on same date as original record
# gave them the id of verifiter as original
ati$date1[which(is.na( ati$OriginalRecorderID) & ati$date1 == ati$date.verified )]
ati$OriginalRecorderID [which(is.na( ati$OriginalRecorderID) & ati$date1 == ati$date.verified )] = ati$VerifierUserID [which(is.na( ati$OriginalRecorderID) & ati$date1 == ati$date.verified )] 

##########################################################################
##########################################################################
# location -----

# coords
ati$Lng[ati$OSGBGridRef == "TL37702449"] = 0.000072
ati$Lat[ati$OSGBGridRef == "TL37702449"] = 51.901944

# locaitons in ROI that are apparently in England
# 3 in ROI which comments say are in Wortley Hall, a place that has other A&V trees
# gave aproxsiamte location - i.e. that of Wortley Hall itself
ati[ati$TreeNumber %in% c(100836, 100834, 100831), c("Lng","Lat")] = rep(c(-1.5304781,53.491277), each = 3)



# CRS conversion ----
cord.dec = SpatialPoints(cbind(ati$Lng, ati$Lat), proj4string = CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS(ukgrid))
ati$eastings = cord.UTM@coords[,1]
ati$northings = cord.UTM@coords[,2]

# remove duplicate entries for same tree

ati = ati [!ati$TreeNumber %in% c(83027),]# duplicate foudn when fixing locations in water
# shift a few locations that are in sea etc, that dont join to countries shapefile.
ati$eastings[ati$TreeNumber == 114516 ] = ati$eastings[ati$TreeNumber == 114516 ] -20
ati$northings[ati$TreeNumber == 11137 ] = ati$northings[ati$TreeNumber == 11137 ] - 20
ati$eastings[ati$TreeNumber == 59835 ] = ati$eastings[ati$TreeNumber == 59835 ] +20
ati$northings[ati$TreeNumber == 142657 ] = ati$northings[ati$TreeNumber == 142657 ] -5
ati$northings[ati$TreeNumber == 67830 ] = ati$northings[ati$TreeNumber == 67830 ] +2000  # a fair bit off, but comments suggest aprox in right area
ati$northings[ati$TreeNumber == 83102 ] = ati$northings[ati$TreeNumber == 83102 ] -30
ati$northings[ati$TreeNumber == 10754 ] = ati$northings[ati$TreeNumber == 10754 ] +300
ati$northings[ati$TreeNumber == 31668 ] = ati$northings[ati$TreeNumber == 31668 ] +2
ati$eastings[ati$TreeNumber == 96334 ] = ati$eastings[ati$TreeNumber == 96334 ] -10
ati$eastings[ati$TreeNumber == 190080 ] = ati$eastings[ati$TreeNumber == 190080 ] -10
ati$northings[ati$TreeNumber == 174419 ] = ati$northings[ati$TreeNumber == 174419 ] +5
ati$eastings[ati$TreeNumber == 174418 ] = ati$eastings[ati$TreeNumber == 174418 ] -10
ati$northings[ati$TreeNumber == 198591 ] = ati$northings[ati$TreeNumber == 198591 ] -10
ati$northings[ati$TreeNumber == 198589 ] = ati$northings[ati$TreeNumber == 198589 ] -10
ati$northings[ati$TreeNumber %in% 62953:62956 ] = ati$northings[ati$TreeNumber == 62953 ] -15
ati$northings[ati$TreeNumber == 88930 ] = ati$northings[ati$TreeNumber == 88930 ] -5
ati$northings[ati$TreeNumber %in% 62933:62936 ] = ati$northings[ati$TreeNumber == 62933 ] -25
ati$eastings[ati$TreeNumber == 102547 ] = ati$eastings[ati$TreeNumber == 102547 ] -40
ati$eastings[ati$TreeNumber == 2387 ] = ati$eastings[ati$TreeNumber == 2387 ] -20
ati$northings[ati$TreeNumber == 143464 ] = ati$northings[ati$TreeNumber == 143464  ] -5
ati$eastings[ati$TreeNumber == 110290 ] = ati$eastings[ati$TreeNumber == 110290 ] -5
ati$eastings[ati$TreeNumber == 5762 ] = ati$eastings[ati$TreeNumber == 5762 ] +5

# shift a few more that are aree wee bit oot aa teh waa parleymynt constituenees
ati$eastings[ati$TreeNumber %in% 62933:62936 ] = ati$eastings[ati$TreeNumber == 62933 ] + 10
ati$northings[ati$TreeNumber == 96074 ] = ati$northings[ati$TreeNumber == 96074  ] - 5
ati$northings[ati$TreeNumber == 83102 ] = ati$northings[ati$TreeNumber == 83102  ] -  10
ati$northings[ati$TreeNumber %in% 62953:62956 ] = ati$northings[ati$TreeNumber == 62953 ] - 15
ati$northings[ati$TreeNumber == 2387 ] = ati$northings[ati$TreeNumber == 2387 ] + 5
ati$northings[ati$TreeNumber == 194858 ] = ati$northings[ati$TreeNumber == 194858 ] -15
ati$eastings[ati$TreeNumber == 190080 ] = ati$eastings[ati$TreeNumber == 190080 ] - 15
ati$northings[ati$TreeNumber == 67643 ] = ati$northings[ati$TreeNumber == 67643 ] + 5
ati$northings[ati$TreeNumber == 187576 ] = ati$northings[ati$TreeNumber == 187576 ] + 5
ati$eastings[ati$TreeNumber == 5368 ] = ati$eastings[ati$TreeNumber == 5368 ] - 5
ati$eastings[ati$TreeNumber == 186026 ] = ati$eastings[ati$TreeNumber == 186026 ] - 5
ati$northings[ati$TreeNumber == 145378 ] = ati$northings[ati$TreeNumber == 145378 ] + 5

###############################################
# vet status ----
ati$ancient = str_detect(ati$VeteranStatus, "Ancient")

# lost.status ----
ati$lost = str_detect(ati$VeteranStatus, "Lost")

# species

# combine bircha nd silver birch





# write new ati ----
write.csv(ati, "S:\\Users\\Ewan McHenry\\Documents\\Work\\SoWT\\ATI\\ewan.ATI_v2.01.csv")

