#---------------------------------------------------------------------------------------
# Script to fit zero-inflated distribution models at a 1-km resolution across England
# Nolan et al.(2022) - Distribution models calibrated with independent field data predict two million ancient and veteran trees in England
# https://doi.org/10.1002/eap.2695
#---------------------------------------------------------------------------------------

# Packages that you may need at some point ----

require(rgdal)
require(caret)
require(corrplot)
require(mctest)
require(ggplot2)
require(dplyr)
require(magrittr)
require(pscl)
require(car)
require(Metrics)
require(pROC)
library(Hmisc)
library(skimr)
library(sf)

# data and workign directories
setwd("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\ATI\\Scotland ATI concentration mapping\\CMcC predictions 2023-01-24")

# Read in data - stored as a shapefile with attribute table containing all predictor values
# Also contains a column called 'Count_' with number of ATI records per grid
data <- readOGR('model_data/england_data/ZIDataEnv/ZIDataEnv.shp')

# Read in Scotland data. This is in geopackage format. The 'Count_' column has the number of ancient and veteran ATI records per grid in Scotland.
# input_1_fullgrid.gpkg contains all grid cells intersecting the land boundaries of Scotland. 

scot.data <- readOGR('model_data/scotland_data/input_1_fullgrid.gpkg')

#---------------------------------------------------------------------------------------
# CREATING CROSS VALIDATION FOLDS IN DATA - used 10 fold cross validation for model eval
#---------------------------------------------------------------------------------------
# Code from Ewan
#set.seed(123)
#fold <- createFolds(data$Count_, k = 10, list = TRUE, returnTrain = FALSE)
#data$FOLD <- 1:nrow(data)
#scot.data$FOLD <- 1:nrow(scot.data)
#for(i in 1:10){
#  data$FOLD[unlist(fold[[i]])] <- i
#  scot.data$FOLD[unlist(fold[[i]])] <- i
#}
# Code from Ewan threw an error - "Error in `[[<-.data.frame`(`*tmp*`, name, value = c(1L, 2L, 3L, 4L, 5L,  : 
# replacement has 130754 rows, data has 85291". 

# The dataframes can be folded separately:

set.seed(123)
fold <- createFolds(data$Count_, k = 10, list = TRUE, returnTrain = FALSE)
data$FOLD <- 1:nrow(data)
for(i in 1:10){data$FOLD[unlist(fold[[i]])] <- i}

set.seed(123)
fold <- createFolds(scot.data$Count_, k = 10, list = TRUE, returnTrain = FALSE)
scot.data$FOLD <- 1:nrow(scot.data)
for(i in 1:10){scot.data$FOLD[unlist(fold[[i]])] <- i}

#---------------------------------------------------------------------------------------
# CHECKING COLLINEARITY AND DATA ASSUMPTIONS OF RAW DATA BEFORE MODEL FITTING
#---------------------------------------------------------------------------------------

# Correlation plots of the covariates (England)
corrplot(cor(data@data[,c(4,6,10:24)], method='pearson'), method='number', 
         bg = 'black', number.cex = .6) 

# Scotland correlation plot with zeroed variables omitted
corrplot(cor(scot.data@data[,c(1, 3, 8, 10:13, 15:18, 20, 21)], method='pearson'), method='number', 
         bg = 'black', number.cex = .6) 

# Histogram of ancient and veteran tree counts per grid squares
# Both histograms give warnings: "Removed (nrow) rows containing non-finite values ('stat_bin()')"
# For England
ggplot(data@data, aes(log(Count_)))+
  geom_histogram()
# For Scotland
ggplot(scot.data@data, aes(log(Count_)))+
  geom_histogram()

# Testing for zero-inflation in the counts (England)
pois_data <- as.numeric(data$Count_)
lambda_est <- mean(pois_data)
p0_tilde <- exp(-lambda_est)
p0_tilde
n0 <- sum(1*(!(pois_data >0)))
n <- length(pois_data)
n*p0_tilde
numerator <- (n0 -n*p0_tilde)^2
denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda_est*(p0_tilde^2)
test_stat <- numerator/denominator
pchisq(test_stat,df=1, ncp=0, lower.tail=FALSE)
varminusmean <- var(data$Count_)-mean(data$Count_)
logvardivmean   <- log(var(data$Count_)/mean(data$Count_))                
1+(varminusmean*log(nrow(data[which(data$Count_==0),])/nrow(data)))/(var(data$Count_)*logvardivmean) 

# Testing for zero-inflation in the counts (Scotland)
pois_data <- as.numeric(scot.data$Count_)
lambda_est <- mean(pois_data)
p0_tilde <- exp(-lambda_est)
p0_tilde
n0 <- sum(1*(!(pois_data >0)))
n <- length(pois_data)
n*p0_tilde
numerator <- (n0 -n*p0_tilde)^2
denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda_est*(p0_tilde^2)
test_stat <- numerator/denominator
pchisq(test_stat,df=1, ncp=0, lower.tail=FALSE)
varminusmean <- var(scot.data$Count_)-mean(scot.data$Count_)
logvardivmean   <- log(var(scot.data$Count_)/mean(scot.data$Count_))                
1+(varminusmean*log(nrow(scot.data[which(scot.data$Count_==0),])/nrow(scot.data)))/(var(scot.data$Count_)*logvardivmean)

# Testing for overdispersion in the counts (England)
var(data$Count_)/mean(data$Count_) # d = 122.7 so extremely overdispersed

# Testing for overdispersion in the counts (Scotland)
var(scot.data$Count_)/mean(scot.data$Count_) # d = 46.3 for Scotland (input 1), still very overdispersed

#---------------------------------------------------------------------------------------
# COMBINING FACTOR LEVELS TO AID MODEL FITTING
#---------------------------------------------------------------------------------------
# Some of the categorical predictors had too many factor levels so models would not fit well or 
# converge. We combined some factors levels here to reduce that issue.

modeldata.england <- data@data[,c(4:25)]
modeldata.england %<>%
  mutate_each_(funs(factor(.)),c('SoilType','AncVPlan','Agriclass','Landclass'))

levels(modeldata.england$SoilType) <- c('Other','Other','Luvisol','Luvisol','Cambisol','Gleysol','Other',
                                        'Fluvisol','Gleysol','Cambisol','Podzol','Leptosol','Cambisol',
                                        'Cambisol','Arenosol','Fluvisol','Leptosol','Other','Histosol',
                                        'Urban','Histosol','Podzol','Gleysol','Other','Luvisol','Luvisol',
                                        'Podzol','Cambisol')
levels(modeldata.england$AncVPlan) <- c('Planned','Ancient','Highland','Highland Cornwall')
levels(modeldata.england$Agriclass) <- c('Other','Agri','Agri','Agri','Agri','NonAg','NonAg','Agri','Other')

levels(modeldata.england$Landclass) <- c('Other','Broadleaved','Heather/Bog','Heather/Bog','Other',
                                         'Saltwater','Freshwater','Coastal','Coastal','Coastal','Coastal',
                                         'Coastal','Coniferous','Urban','Urban','Other','Arable',
                                         'Grassland','Grassland','Grassland','Grassland','Other','Heather/Bog')

# Combining for Scottish data
modeldata.scotland <- scot.data@data[,c(1:22)]
modeldata.scotland %<>%
  mutate_each_(funs(factor(.)),c('SoilType','AncVPlan','Agriclass','Landclass'))

# Following the same pattern as for the English soil data, as this is based on the same dataset.
# Note that cells with no data (coastal) are included in 'Other', as are freshwater cells and regisol cells.
levels(modeldata.scotland$SoilType) <- c('Other', 'Urban','Other','Cambisol','Cambisol','Fluvisol','Fluvisol','Gleysol',
                                         'Gleysol','Gleysol','Histosol','Leptosol','Luvisol', 'Podzol', 'Podzol', 'Other', 'Other')

# Far North, Highland, Lowland and Southern Upland 
levels(modeldata.scotland$AncVPlan) <- c('Highland','Highland','Ancient','Highland')

# Three levels to match English data.
# As with English data, 'NonAg' is forest, inland water, and urban. 'Other' is coastal/estuarine. All else is 'Agri'. 
levels(modeldata.scotland$Agriclass) <- c('NonAg', 'NonAg', 'Other', 'Agri')

# Both datasets use CEH LCM, so levels are simplified in the same way as for the English data
levels(modeldata.scotland$Landclass) <- c('Other', 'Broadleaved','Coniferous','Arable','Grassland','Grassland',
                                          'Grassland', 'Grassland', 'Heather/Bog','Heather/Bog','Heather/Bog','Heather/Bog','Other',
                                          'Saltwater','Freshwater','Coastal','Coastal','Coastal','Coastal',
                                          'Coastal','Urban','Urban')

#---------------------------------------------------------------------------------------
# SCALE AND CENTER CONTINUOUS MODEL PREDICTORS
#---------------------------------------------------------------------------------------
og_num.data.england <- modeldata.england[,c(3,7:21)] 
colnames(og_num.data.england) <- paste0("OG_",colnames(modeldata.england[,c(3,7:21)] ))

num.data.england <- scale(modeldata.england[,c(3,7:21)] , scale = T,center = T)
modeldata.england[,c(3,7:21)] <- num.data.england



# alter any real covariate numbers here 


# Scale Scottish numeric data to be comparable to English data (from Ewan)
num.data.scotland <-  modeldata.scotland[,c(3,7:21)] 
for (i in 1:dim(num.data.scotland)[2] ){
  num.data.scotland[,i] <- 
    (num.data.scotland[,i] - attributes(num.data.england)$`scaled:center`[i]) / attributes(num.data.england)$`scaled:scale`[i]
}
modeldata.scotland[,c(3,7:21)] <- num.data.scotland

# Check histograms to make sure that it makes appropriate sense
# hist.data.frame(modeldata.england[,c(3,7:21)])
# 
# warnings()
# 
# # hist.data.frame(modeldata.scotland[,c(3,7:21)])
# 
# warnings()

# Fix any "to assume average" covariates  -- for covaraites not included in scotland (due to avilibility of data normally), and for which we assume to have averaged out
modeldata.scotland$Commons  <-  0
modeldata.scotland$TudDP  <-  0
modeldata.scotland$HistForest  <-  0
#modeldata.scotland$WoodPast  <-  0

# #---------------------------------------------------------------------------------------
# ## FITTING POISSON MODELS AND CREATING PREDICTIONS
# #---------------------------------------------------------------------------------------
# # Poisson model
# m1 <- zeroinfl(Count_~.|.,data = modeldata.england[,c(1,2:21)], dist='poisson')
# summary(m1)
# 
# # Chi-square test
# s1 <- step(m1, test='Chisq')
# summary(s1)
# 
# # For England
# for (i in 1:10) {
#   x <- unname(unlist(predict(m1,modeldata.england[which(modeldata.england$FOLD==i),],type='count')))
#   modeldata.england$POISSCOUNT[modeldata.england$FOLD==i] <- x} #COUNT PREDICTION
# for (i in 1:10) {
#   x <- unname(unlist(predict(m1,modeldata.england[which(modeldata.england$FOLD==i),],type='response')))
#   modeldata.england$POISSRESP[modeldata.england$FOLD==i] <- x} #RESPONSE PREDICTIONS
# for (i in 1:10) {
#   x <- unname(unlist(predict(m1,modeldata.england[which(modeldata.england$FOLD==i),],type='zero')))
#   modeldata.england$POISSZERO[modeldata.england$FOLD==i] <- x} #ZERO PREDICTIONS
#  
# data@data <- modeldata.england
# writeOGR(data, dsn=getwd(), layer = 'ZIResultsPoiss', driver="ESRI Shapefile",overwrite_layer = T)
# 
# # For Scotland
# for (i in 1:10) {
#   x <- unname(unlist(predict(m1,modeldata.scotland[which(modeldata.scotland$FOLD==i),],type='count')))
#   modeldata.scotland$POISSCOUNT[modeldata.scotland$FOLD==i] <- x} #COUNT PREDICTION
# for (i in 1:10) {
#   x <- unname(unlist(predict(m1,modeldata.scotland[which(modeldata.scotland$FOLD==i),],type='response')))
#   modeldata.scotland$POISSRESP[modeldata.scotland$FOLD==i] <- x} #RESPONSE PREDICTIONS
# for (i in 1:10) {
#   x <- unname(unlist(predict(m1,modeldata.scotland[which(modeldata.scotland$FOLD==i),],type='zero')))
#   modeldata.scotland$POISSZERO[modeldata.scotland$FOLD==i] <- x} #ZERO PREDICTIONS
# 
# scot.data@data <- modeldata.scotland
# writeOGR(scot.data, dsn=getwd(), layer = 'ZIResultsPoiss_Scotland_input1', driver="ESRI Shapefile",overwrite_layer = T)
# 
#---------------------------------------------------------------------------------------
## FITTING NEGBIN MODELS AND CREATING PREDICTIONS
#---------------------------------------------------------------------------------------
# Negative binomial model 
m2 <- zeroinfl(Count_~.|.,data = modeldata.england[,c(1,2:21)], dist='negbin')
save(m2,      file = "m2.RData")
load(file = "m2.RData")
summary(m2)

# "Warning message: "In sqrt(diag(object$vcov)) : NaNs produced"

# Chi-square test: note this takes a long time to process
s2 <- step(m2, test='Chisq')
summary(s2)
save(s2, file = "s2.RData")

# For England
for (i in 1:10) {
  x <- unname(unlist(predict(m2,modeldata.england[which(modeldata.england$FOLD==i),],type='count')))
  modeldata.england$NEGBINCOUNT[modeldata.england$FOLD==i] <- x} #COUNT PREDICTION
for (i in 1:10) {
  x <- unname(unlist(predict(m2,modeldata.england[which(modeldata.england$FOLD==i),],type='response')))
  modeldata.england$NEGBINRESP[modeldata.england$FOLD==i] <- x} #RESPONSE PREDICTIONS
for (i in 1:10) {
  x <- unname(unlist(predict(m2,modeldata.england[which(modeldata.england$FOLD==i),],type='zero')))
  modeldata.england$NEGBINZERO[modeldata.england$FOLD==i] <- x} #ZERO PREDICTIONS


data@data <- cbind(modeldata.england, og_num.data.england)
writeOGR(data, dsn=getwd(), layer = 'ZIResultsNegBin', driver="ESRI Shapefile", overwrite_layer = T)
england.data <- st_as_sf(data)
save(england.data, file = "predictions\\england.data.RData")

# For Scotland
for (i in 1:10) {
  x <- unname(unlist(predict(m2,modeldata.scotland[which(modeldata.scotland$FOLD==i),],type='count')))
  modeldata.scotland$NEGBINCOUNT[modeldata.scotland$FOLD==i] <- x} #COUNT PREDICTION
for (i in 1:10) {
  x <- unname(unlist(predict(m2,modeldata.scotland[which(modeldata.scotland$FOLD==i),],type='response')))
  modeldata.scotland$NEGBINRESP[modeldata.scotland$FOLD==i] <- x} #RESPONSE PREDICTIONS
for (i in 1:10) {
  x <- unname(unlist(predict(m2,modeldata.scotland[which(modeldata.scotland$FOLD==i),],type='zero')))
  modeldata.scotland$NEGBINZERO[modeldata.scotland$FOLD==i] <- x} #ZERO PREDICTIONS

# prediction of influence of specific terms

# a dummy data where all numeric covars == 0
cols.numeric.covars = (names(modeldata.scotland) %in% names(m2$coefficients$count) & sapply(modeldata.scotland, is.numeric ))
dum_zero_data.scotland = modeldata.scotland
dum_zero_data.scotland[,cols.numeric.covars] <- 0 
# create cols for prediction of influence of each cavariate
influ.df =  data.frame (matrix(ncol =  sum(cols.numeric.covars), nrow = dim(modeldata.scotland)[1] ))
colnames(influ.df) = paste0( "influ_",names(modeldata.scotland)[cols.numeric.covars])

for ( ii in 1:sum(cols.numeric.covars)){  # for each numeric covariate
  this.covar = names(modeldata.scotland)[which(cols.numeric.covars)[ii]] # select covar name
  influ.df[, ii] = modeldata.scotland[, this.covar] * m2$coefficients$count[this.covar]
      }

modeldata.scotland <-  cbind(modeldata.scotland, influ.df)

scot.data@data <- modeldata.scotland

writeOGR(scot.data, dsn=getwd(), layer = 'ZIResultsNegBin_Scotland_input1', driver="ESRI Shapefile", overwrite_layer = T)

scot.data <- st_as_sf(scot.data)
save(scot.data, file = "predictions\\scot.data.RData")


#---------------------------------------------------------------------------------------
## WRITE OUT MODEL PREDICTION RESULTS
#---------------------------------------------------------------------------------------

# For England

write.csv(data@data, 'Predictions.csv', sep=',')
modeldata.england <- read.csv('Predictions.csv',header=T)

#For Scotland

write.csv(scot.data, 'Predictions_Scotland_input1.csv')
modeldata.scotland <- read.csv('Predictions_Scotland_input1.csv',header=T)

# The code below runs tests on the original England data only, some adaptation will be needed to apply this to the Scotland models.

#---------------------------------------------------------------------------------------
## TEST OF MODEL PERFORMANCE - COMPARE POISSON AND NEGBIN MODELS
#---------------------------------------------------------------------------------------
vuong(m1,m2)
AIC(m1,m2)


#---------------------------------------------------------------------------------------
# TEST OF RESIDUALS - CHECK FOR SPATIAL AUTOCORRELATION AND COLLINEARITY
#---------------------------------------------------------------------------------------
vif(m1)
vif(m2)

wp <- coordinates(spTransform(data, CRS("+proj=longlat +datum=WGS84")))
data$Latitude <- wp[,2] 
data$Longitude <- wp[,1]
cor(residuals(m1), data$Longitude, method='pearson')
cor(residuals(m2), data$Latitude, method='pearson')

imcdiag(model,all=T, vif = 5)


#---------------------------------------------------------------------------------------
# TEST OF MODEL PREDICTIONS - CORRELATION COEFFICIENTS
#---------------------------------------------------------------------------------------
cor(modeldata$Count_, modeldata$POISSCOUNT, method='spearman')
cor(modeldata$Count_, modeldata$NEGBINCOUNT, method='spearman')
nozero <- modeldata[which(modeldata$Count_>0),]
cor(nozero$Count_, nozero$POISSCOUNT, method='pearson')
cor(nozero$Count_, nozero$NEGBINCOUNT, method='pearson')

cor(modeldata$Count_, modeldata$POISSRESP, method='spearman')
cor(modeldata$Count_, modeldata$NEGBINRESP, method='spearman')
nozero <- modeldata[which(modeldata$Count_>0),]
cor(nozero$Count_, nozero$POISSRESP, method='pearson')
cor(nozero$Count_, nozero$NEGBINRESP, method='pearson')


#---------------------------------------------------------------------------------------
# TEST OF MODEL PREDICTIONS - RMSLE
#---------------------------------------------------------------------------------------
rmsle(modeldata$Count_,modeldata$POISSCOUNT)
rmsle(modeldata$Count_,modeldata$NEGBINCOUNT)
rmsle(modeldata$Count_,modeldata$POISSRESP)
rmsle(modeldata$Count_,modeldata$NEGBINRESP)

rmsle(nozero$Count_,nozero$POISSCOUNT)
rmsle(nozero$Count_,nozero$NEGBINCOUNT)
rmsle(nozero$Count_,nozero$POISSRESP)
rmsle(nozero$Count_,nozero$NEGBINRESP)


#---------------------------------------------------------------------------------------
# TEST OF MODEL PREDICTIONS - AUC (TO COMPARE TO MAXENT MODELS)
#---------------------------------------------------------------------------------------
OB <- as.numeric(as.factor(modeldata$Count_>1))

modeldata$split_poiss[modeldata$POISSZERO <= mean(modeldata$POISSZERO)] <- 1
modeldata$split_poiss[modeldata$POISSZERO > mean(modeldata$POISSZERO)] <- 0
modeldata$split_NEGBIN[modeldata$NEGBINZERO <= mean(modeldata$NEGBINZERO)] <- 1
modeldata$split_NEGBIN[modeldata$NEGBINZERO > mean(modeldata$NEGBINZERO)] <- 0

p1 <- as.numeric(as.factor(modeldata$POISSCOUNT>mean(modeldata$POISSCOUNT)))
p2 <- as.numeric(as.factor(modeldata$POISSRESP>mean(modeldata$POISSRESP)))
p3 <- as.numeric(as.factor(modeldata$NEGBINCOUNT>mean(modeldata$NEGBINCOUNT)))
p4 <- as.numeric(as.factor(modeldata$NEGBINRESP>mean(modeldata$NEGBINRESP)))

pROC::auc(roc(OB,modeldata$split_poiss))
pROC::auc(roc(OB,modeldata$split_NEGBIN))
pROC::auc(roc(OB,p2))

auc <- data.frame(1:10)
for (i in 1:10){
  OB <- as.factor(modeldata$Count[modeldata$FOLD == i]>1)
  levels(OB) <- c(0,1)
  p1 <- as.numeric(as.factor(modeldata$POISSCOUNT[modeldata$FOLD == i]>mean(modeldata$POISSCOUNT[modeldata$FOLD == i])))
  p2 <- as.numeric(as.factor(modeldata$POISSRESP[modeldata$FOLD == i]>mean(modeldata$POISSRESP[modeldata$FOLD == i])))
  p3 <- as.numeric(as.factor(modeldata$NEGBINCOUNT[modeldata$FOLD == i]>mean(modeldata$NEGBINCOUNT[modeldata$FOLD == i])))
  p4 <- as.numeric(as.factor(modeldata$NEGBINRESP[modeldata$FOLD == i]>mean(modeldata$NEGBINRESP[modeldata$FOLD == i])))
  a1 <- pROC::auc(OB,p1)
  a2 <- pROC::auc(OB,p2)
  a3 <- pROC::auc(OB,p3)
  a4 <- pROC::auc(OB,p4)
  auc$p1[i] <- a1 ;  auc$p2[i] <- a2 ; auc$p3[i] <- a3 ; auc$p4[i] <- a4
}
colMeans(auc)
var(auc$p4)

auc <- data.frame(1:10)
for (i in 1:10){
  OB <- as.factor(modeldata$Count[modeldata$FOLD != i]>1)
  levels(OB) <- c(0,1)
  p1 <- as.numeric(as.factor(modeldata$POISSCOUNT[modeldata$FOLD != i]>mean(modeldata$POISSCOUNT[modeldata$FOLD != i])))
  p2 <- as.numeric(as.factor(modeldata$POISSRESP[modeldata$FOLD != i]>mean(modeldata$POISSRESP[modeldata$FOLD != i])))
  p3 <- as.numeric(as.factor(modeldata$NEGBINCOUNT[modeldata$FOLD != i]>mean(modeldata$NEGBINCOUNT[modeldata$FOLD != i])))
  p4 <- as.numeric(as.factor(modeldata$NEGBINRESP[modeldata$FOLD != i]>mean(modeldata$NEGBINRESP[modeldata$FOLD != i])))
  a1 <- pROC::auc(OB,p1)
  a2 <- pROC::auc(OB,p2)
  a3 <- pROC::auc(OB,p3)
  a4 <- pROC::auc(OB,p4)
  auc$p1[i] <- a1 ;  auc$p2[i] <- a2 ; auc$p3[i] <- a3 ; auc$p4[i] <- a4
}
colMeans(auc)

#---------------------------------------------------------------------------------------