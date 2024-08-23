# ATI data curation for SoWT analysis July 2020 Ewan McHenry


# library ----
library(sf) # for gis
library("rnrfa") # to convert os gb grif ref
library(rgdal) # to convert lat long to os gb

library("rnaturalearth") # map of countries of the entire world
library("rnaturalearthdata")
library("rgeos")
library(ggplot2)
library(stringr) # for str_detect()


# load data ----
ati = read.csv("S:\\Users\\Ewan McHenry\\Documents\\Work\\SoWT\\ATI\\ewan.ATI.16.7.2020.csv")

# projection shortcuts ----
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"
# data curation ----
ati = ati[!is.na(ati$TreeNumber) ,]

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

