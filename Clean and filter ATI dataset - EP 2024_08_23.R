### ATI data cleaning 

# 1)	All unverified records were removed 
# 2)	Only records with a 10 figure grid reference retained 
# 3)	Data shows standing, remnant, fallen and fragmented trees. 
#     Fallen trees retained to pull through phoenix trees. All trees listed as 
#     "gone" were excluded from the total number of extant records. 
# 4)	All records with no girth measurement removed. 
# 5)	England, Wales, Scotland and Northern Ireland only. All records with no country status and a value of "other" removed  
# 6)	Last time notable trees were filtered, I have kept them and will filter dependent on the analysis performed. 


## Load Packages 

library(data.table) # for data manipulations 
library(ggplot2) # for plotting 

rm(list = ls()) # Clear everything away

# set directory 
setwd("C:\\Users\\dkp20stb\\OneDrive - Bangor University\\ATI SOWT2")
getwd()

# Load ATI data 
ATI <- fread("ATI dataset.csv") 

## get verified records only 

ATI <- ATI[VerifierId == "", VerifierId := NA] 
ATI.2 <- ATI[!is.na(VerifierId)] ## removes record with no varifier ID

#shouldn't make a difference but for completeness 
ATI.2 <- ATI.2[VerifiedDate == "", VerifiedDate := NA] ## converst blank verified dates to NA
ATI.2 <- ATI.2[!is.na(VerifiedDate)]


# keep only records with 10 figure grid reference 

ATI.3 <- ATI.2[nchar(GridReference) >= 12] ## keep 10 gig grid references


### filter on standing status

dput(unique(ATI.3$StandingStatusName)) # check values and get them in nice vector format 
ATI.4 <- ATI.3[StandingStatusName %in% c("Standing", "Fallen", "Fragmented", "Remnant")] # keep the good values

## remove records with no girth measurment 

ATI.5 <- ATI.4[MeasuredGirth == "", MeasuredGirth := NA] ## converst blank verified dates to NA
ATI.5 <- ATI.5[!is.na(MeasuredGirth)]

## Keep only England, Wales, Scotland and Northern Ireland trees and standardise NI 

unique(ATI.5$CountryName)
ATI.6 <- ATI.5[CountryName %in% c("England" , "Wales", "Northern Ireland", "Scotland", "N. Ireland")]

ATI.6$CountryName <- gsub("N. Ireland", "Northern Ireland", ATI.6$CountryName) 


write.csv(ATI.6, "Clean ATI data.csv")



