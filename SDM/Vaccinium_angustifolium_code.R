install.packages("tidyverse") # only for the first time  
install.packages("rgbif") 
install.packages("CoordinateCleaner") 
install.packages("terra") 
install.packages("geodata") 
install.packages("scales") 
install.packages("magrittr") 
install.packages("raster") 

library(tidyverse) #data management, grammar 
library(rgbif) #access GBIF data 

getwd()#check your working directory  

#GBIF user info 

user='elisecadiou' 
pwd='RIstage2024!' 
email='elise.cadiou@orange.fr' 

#find the taxon name 

angus<-name_backbone('Vaccinium anfustifolium Aiton') 

#download data 
#'pred()' for one argument and 'pred in()' for multiple arguments 

down_code=rgbif::occ_download( 
  pred("taxonKey", c(2882868)), 
  pred("hasCoordinate", TRUE), #remove records without coordinates 
  format="SIMPLE_CSV", #dowload as a csv file 
  user=user,  
  pwd=pwd,  
  email=email) 

setwd("C:/Users/elise/Documents") 
download_angus<-occ_download_get(down_code[1],overwrite=TRUE) 

library(readr) 

V1 <- read_delim("0052432-240506114902167.zip",  
                 delim = "\t", escape_double = FALSE,  
                 trim_ws = TRUE) 

#To analyse a little bit the data 
sort(table(basisOfRecord), decreasing = TRUE) #table of types of records from the most to the least numbre of records 
......# ... = name of variable -> to see value of variable for each specimen 
table(....)#....=variable name to make a summary table of the variable data 

#Cleaning the data 

library(CoordinateCleaner) 
library(terra) 
library(scales) 
library(raster) 
library(magrittr) 

#Types of records 

basisOfRecord<-c('PRESERVED_SPECIMEN', 'HUMAN_OBSERVATION', 'OCCURRENCE') 

barplot(table(d1$basisOfRecord),main="V. angustifolium Ait. records distribution", 
        xlab="Types of records", 
        ylab="Number of records") #to visualize the distribution of types of records 

occ_angus<-V1 %>% 
  filter(countryCode %in% c('CA','US'))#keep data from Canada and the United States 

occ_angus2<-occ_angus %>% 
  filter(!(decimalLongitude < -105))#remove some records on the West Coast 

occ_angus3<-occ_angus2 %>% 
  filter(!(decimalLatitude < 35))#remove some records from Louisiana 

occ_angus4<-occ_angus3 %>% 
  filter(basisOfRecord=='PRESERVED_SPECIMEN'|basisOfRecord=='HUMAN_OBSERVATION')#only keep the herbarium specimens and observations like iNat: |="or" 

occ_angus5<-occ_angus4 %>% 
  filter(!year=='NA'|month=='NA'|day=='NA')#remove records without a full date 

clean<-occ_angus5 %>% 
  dplyr::select(species, countryCode, decimalLatitude, decimalLongitude, eventDate, basisOfRecord, year)#keep only the usefull variables for the rest of the analysis 

head(clean)#visualize the new table 

setwd("~/Documents/Stage RI/R_SDM") 
saveRDS(clean,"V0.Rdata") 
V0<-readRDS(file = "V0.Rdata") 

head(V0) 
table(V0$basisOfRecord)
#HUMAN_OBSERVATION PRESERVED_SPECIMEN 
#6158               3317 

barplot(table(V0$basisOfRecord),main="V. angustifolium Ait. records distribution", 
        xlab="Types of records", 
        ylab="Number of records") #to visualize the distribution of types of records 

install.packages("C:/Users/elise/Downloads/geodata_0.5-9.tar.gz", repos = NULL, type = "source") 

library(terra) 
library(geodata) 
library(magrittr) 
library(tidyverse) 

#Distribution through the years 

us_map<-gadm(country='USA', level=1, resolution=2, path= "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM") 

ca_map <- gadm(country = 'CA', level = 1, resolution = 2,path = "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM") 

canUS_map <- rbind(us_map, ca_map) 

plot(canUS_map, xlim = c(-100, -45), ylim = c(25, 60)) 

library(scales)
median(V0$eventDate)
#2019-05-25T19:39:49 but I chose the end of the month 

angus_pre<-V0 %>%  
  filter(V0$eventDate <= "2019-05-31T16:12:50")

angus_post<-V0 %>% 
  filter(V0$eventDate > "2019-05-31T16:12:50")

head(angus_post) 

# plot angustifolium occurrences 
# pre-2010 
points(angus_pre$decimalLongitude, angus_pre$decimalLatitude, pch = 16, 
       col = alpha("forestgreen", 0.3)) 

# post-2010 
points(angus_post$decimalLongitude, angus_post$decimalLatitude, pch = 16, 
       col = alpha("darkorange1", 0.1)) 

legend(x = -79, y = 33, title = "V. angustifolium records", legend = c('Pre/during May 2019 (n=4765)', 'Post May 2019 (n=4710)'), fill = c('forestgreen', 'darkorange1'), cex = 0.6, bty = "n") 

hist(V0$year, breaks=100, main="Time distribution", xlab="Year", xlim=c(2025,1850), ylim=c(0,2500)) 
abline(v=median(V0$year), col='red', lwd=3) 

hist(V0$year, breaks=100, main="Time distribution", xlab="Year", xlim=c(2025,1980), ylim=c(0,2500)) 

----------------------------------------------------------------------------
#Thinning occurences: get a single data point per predictor (raster cell) 

library(tidyverse) 
library(terra) 
library(geodata) 

#Load cleaned data 
setwd("~/Documents/Stage RI/R_SDM")
V0<-readRDS(file = "V0.Rdata") 

#Vectorize occurence df to coordinates 

V0<- vect(V0, geom = c('decimalLongitude', 'decimalLatitude'), 
          crs="+proj=+longlat +datum=WGS84") 

#Downloading wordclim data 
#Set working directory to the Wordclim folder 

setwd("~/Stage RI/R_SDM/Worldclim") 

wclim<-worldclim_global(var = 'bio', res = 2.5, version = '2.1', path = "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM\\Worldclim") 

plot(wclim$wc2.1_2.5m_bio_1,main = 'Annual Mean Temperature') #plot the raster layer to check if it downloaded properly 

set.seed(5) 

#V. angus thinning - sample 1 occurence from each climatic cell 

V0<-spatSample(V0, size=1, 
               strata = wclim) 

#save thinned data for further analysis 
#set working directory where I want to save the file (only do it once) 

saveRDS(V0, file = 'occ_thin_ang.Rdata') 

#Setting background area and predictor variables 

library(tidyverse) 
library(geodata) 
library(terra) 
install.packages("predicts") 
library(predicts) 
install.packages("ENMTools") 
library(ENMTools) 
install.packages("plotly") 
library(plotly) 
install.packages("MASS") #3D surface plots 
library(MASS) 


#Download NA ecoregion shapefile from: https://www.epa.gov/eco-research/ecoregions-north-america 

#Load shapefile from file 

ecoNA<-vect(x="C:\\Users\\elise\\Documents\\Stage RI\\R_SDM\\Ecoregions NA Shapefile", layer='NA_CEC_Eco_Level2') 

ecoNA<-project(ecoNA, 'WGS84') #to project ecoregion vector to same soordinates ref as basemap 

#Load maps 

us_map<-gadm(country='USA', level=1, resolution=2, path= "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM") 

ca_map <- gadm(country = 'CA', level = 1, resolution = 2,path = "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM") 

canUS_map <- rbind(us_map, ca_map) 

plot(canUS_map, xlim = c(-180, -50)) 

lines(ecoNA, col='red') 

#load thinned data 

setwd("~/Stage RI/R_SDM") 
occ_thin_ang<-readRDS(file='occ_thin_ang.Rdata') 

#Ecoregions wher V. angus occur 

eco_ang<-extract(ecoNA, occ_thin_ang) #to extract what polygons contain points 

#Return vector of ecoregion codes of the polygons that contain occurences 

eco_ang_code<-eco_ang$NA_L2CODE %>% unique() #can take a long time!! 
eco_ang_code<-eco_ang_code[eco_ang_code != '0.0']# 0.0 = water so we remove it 

ecoNA_ang<-terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_ang_code) #subset ecoregion spat vector by the codes 

plot(ecoNA_ang) 

points(occ_thin_ang, pch = 20, col = 'red', alpha=0.7) 

setwd("~/Stage RI/R_SDM") 
saveRDS(ecoNA_ang, file = 'ecoNA_ang.Rdata') 

#Crop & mask extent of worldclim data to the selected ecoregions 

wclim_ang<-terra::crop(wclim, ecoNA_ang, mask = T) 

setwd("~/Stage RI/R_SDM") 
saveRDS(wclim_ang, file = 'wclim_ang.Rdata') 

set.seed(5) 

# NOTE: Set arguments as.raster = T to return raster 
# OR as.points to return spatvector = T to return spatvector 
# Note upped bg points from 5000 to 20000 to be more suitable to better reflect a mean probability of presence 1 - a/2 

ang_bg_vec <- spatSample(wclim_ang, 15000, 'random', na.rm = T, as.points = T) #ignore NA values 
plot(wclim_ang[[1]]) 

points(ang_bg_vec, cex = 0.01) 

#get the total area of raster in km² 

expanse(wclim_ang[[1]], unit = 'km') 

# 8 717 180 km² 

#15000/8717180 
#0.0017 samples/km 

#save and load background spat vectors 

setwd("~/Stage RI/R_SDM") 
saveRDS(ang_bg_vec, file = 'ang_bg_vect') 
ang_bg_vect<-readRDS(file = 'ang_bg_vect') 

#Extracting presence background raster values 

ang_predvals<-extract(wclim_ang, occ_thin_ang) 
ang_predvals<-ang_predvals[-1]#drop ID column 
ang_bgvals<-values(ang_bg_vec) 

# Create a df for presence-background raster values for SDM --------------- 

ang_pb <- c(rep(1, nrow(ang_predvals)), rep(0, nrow(ang_bgvals))) #T/F presence or background string 

# combine presence and background dataframes for SDM 
ang_sdmData <- data.frame(cbind(ang_pb, rbind(ang_predvals, ang_bgvals))) 

#Save df for downstream SDM work 
setwd("~/Stage RI/R_SDM") 
saveRDS(ang_sdmData, file = 'ang_sdmData.Rdata') 
ang_sdmData <- readRDS(file = 'ang_sdmData.Rdata') 

# Check for colinearity of predictor variables for presence-bg ----------- 
# Although Maxent is equipped to handle variable selection through its built in variable regularization, it is still important to understand the relationships between predictors. 

# Dendograms useful for identifying groupings or clusters of colinear variables 

pairs(ang_sdmData[,-1]) # drop the first column of 0/1 

# Dendogram cluster of predictor colinearity 

threshold <- 0.7 # set the threshold for colinearity  
ang_cors <- raster.cor.matrix(wclim_ang) # pearson correlation 
ang_dist <- as.dist(1 - abs(ang_cors)) # calculate distance of predictors 
ang_clust <- hclust(ang_dist, method = 'single') # calculate cluster dendogram 
ang_groups <- cutree(ang_clust, h = 1 - threshold) #calculate groupings of variables 
plot(ang_clust, hang = -1, main = "V. angustifolium Predictors") 
rect.hclust(ang_clust, h = 1 - threshold) 


# Predictor Kernel Density Plots-------------------------------------------

# It is helpful to visualize two predictors pairs of presence points and background points 

ang_occ.temp <- ang_sdmData %>% filter(ang_pb == 1) %>% # Presence points 
  dplyr::select(wc2.1_2.5m_bio_1) %>% # Mean annual temp 
  drop_na() %>%  
  unlist() 

ang_bg.temp <- ang_sdmData %>% filter(ang_pb == 0) %>% # Background points 
  dplyr::select(wc2.1_2.5m_bio_1) %>% # Mean annual temp 
  drop_na() %>%  
  unlist() 

ang_occ.perc <- ang_sdmData %>% filter(ang_pb == 1) %>% # Presence points 
  dplyr::select(wc2.1_2.5m_bio_12) %>% # Annual precipitation 
  drop_na() %>%  
  unlist() 

ang_bg.perc <- ang_sdmData %>% filter(ang_pb == 0) %>% # Background points 
  dplyr::select(wc2.1_2.5m_bio_12) %>% # Annual precipitation 
  drop_na() %>%  
  unlist() 

ang_occ.3d <- kde2d(ang_occ.temp, ang_occ.perc)
ang_bg.3d <- kde2d(ang_bg.temp, ang_bg.perc) 

#Plot 3D surface Kernel density estimation 

plot_ang.occ_3d <- plot_ly(x=ang_occ.3d$x, y=ang_occ.3d$y, z=ang_occ.3d$z) %>%  
  add_surface() %>%  
  layout(scene = list(xaxis = list(title = 'Mean Annual Temp (C)', autotick = F, nticks = 5, tickvals = list(0,5,10,15,20)),  
                      yaxis = list(title = 'Mean Annual Percip. (mm)', tick0=0, tick1=2000, dtick=200),  
                      zaxis = list(title = 'Kernel Density', tick0=0, tick1=0.001, dtick=0.0002)), 
         title = list(text = "<i>V. angustifolium<i> Occurrence Points",  
                      y = 0.95, x = 0.5,  
                      xanchor = 'center',  
                      yanchor = 'top')) 

plot_ang.occ_3d # run to view 

plot_ang.bg_3d <- plot_ly(x=ang_bg.3d$x, y=ang_bg.3d$y, z=ang_bg.3d$z) %>%  
  add_surface() %>%  
  layout(scene = list(xaxis = list(title = 'Mean Annual Temp (C)', tick0=0, tick1=20, dtick=5),  
                      yaxis = list(title = 'Mean Annual Percip. (mm)', tick0=0, tick1=2000, dtick=200),  
                      zaxis = list(title = 'Kernel Density')), 
         title = list(text = "<i>V. angustifolium<i> Background Points",  
                      y = 0.95, x = 0.5,  
                      xanchor = 'center',  
                      yanchor = 'top')) 

plot_ang.bg_3d 


install.packages("rJava") 
install.packages("predicts") 
install.packages("ENMeval") 
install.packages("ecospat") 
install.packages("parallel") 
install.packages("doParallel") 

library(tidyverse) # Grammar and data management 
library(terra) # Spatial Data package 
library(predicts) # SDM package 
library(geodata) # basemaps 
library(rJava) # MaxEnt models are dependant on JDK 
library(ENMeval) # Another modeling package, useful for data partitioning (Checkerboarding) 
library(raster) # RasterStack dependancy (a now deprecated function) 
library(ecospat) # Useful spatial ecology tools 
library(parallel) # speed up computation by running in parallel 
library(doParallel) # added functionality to parallel 

setwd("~/Documents/Stage RI/R_SDM") 

# Background points in SpatVectors & occurrence Points in SpatVectors 

occ_thin_ang<-readRDS(file='occ_thin_ang.Rdata') 
ang_bg_vect<-readRDS(file = 'ang_bg_vect') 

# Download/load basemaps 

us_map<-gadm(country='USA', level=1, resolution=2,  
             path= "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM") 

ca_map <- gadm(country = 'CA', level = 1, resolution = 2, 
               path = "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM") 

canUS_map <- rbind(us_map, ca_map) # Combine US and Canada vector map 

NA_ext <- ext(-180, -30, 18, 85) # Set spatial extent of analyis to NA in Western Hemisphere 

canUS_map <- crop(canUS_map, NA_ext) # crop to Western Hemisphere 
plot(canUS_map, xlim = c(-180, -50)) 


# Download/load WorldClim data under future climate scenarios ------------- 

setwd("~/Documents/Stage RI/R_SDM/Wordclim") 

# Historical climate 1970-2000 

wclim<-geodata::worldclim_global(var = 'bio',  
                                 res = 2.5,  
                                 version = '2.1',  
                                 path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM") %>% 
    crop(NA_ext) #crop raster to NA 

# SSP (Shared social-economic pathway) 2.45  

# middle of the road projection, high climate adaptation, low climate mitigation 

ssp245_2030 <- cmip6_world(model = "CanESM5", 
                           ssp = "245", 
                           time = "2021-2040", 
                           var = "bioc", 
                           res = 2.5, 
                           path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM") %>%  
  crop(NA_ext) #crop raster to NA 

ssp245_2050 <- cmip6_world(model = "CanESM5", 
                           ssp = "245", 
                           time = "2041-2060", 
                           var = "bioc", 
                           res = 2.5, 
                           path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM") %>%  
  crop(NA_ext) #crop raster to NA 

ssp245_2070 <- cmip6_world(model = "CanESM5", 
                           ssp = "245", 
                           time = "2061-2080", 
                           var = "bioc", 
                           res = 2.5, 
                           path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM") %>%  
  crop(NA_ext) #crop raster to NA 

# SPP 5.85  

# low regard for environmental sustainability, increased fossil fuel reliance, this is the current tracking projection 

ssp585_2030 <- cmip6_world(model = "CanESM5", 
                           ssp = "585", 
                           time = "2021-2040", 
                           var = "bioc", 
                           res = 2.5, 
                           path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM") %>%  
  crop(NA_ext) #crop raster to NA 

ssp585_2050 <- cmip6_world(model = "CanESM5", 
                           ssp = "585", 
                           time = "2041-2060", 
                           var = "bioc", 
                           res = 2.5, 
                           path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM") %>%  
  crop(NA_ext) #crop raster to NA 

ssp585_2070 <- cmip6_world(model = "CanESM5", 
                           ssp = "585", 
                           time = "2061-2080", 
                           var = "bioc", 
                           res = 2.5, 
                           path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM")%>%  
  crop(NA_ext) #crop raster to NA 

# Load cropped climate Rasters 

# These Rasters are useful for sampling spatial checkerboards and making habitat suitability predictions (Historical and under future SSPs climate scenarios) 

setwd("~/Stage RI/R_SDM") 

# Historical (1970-2000) 

saveRDS(wclim_ang, file = 'wclim_ang.Rdata') 
wclim_ang <- readRDS(file = 'wclim_ang.Rdata')  

#wclim_cor_stack <- raster::stack(wclim_cor) # covert SpatRaster to RasterStack for dependency in ENMeval checkboarding 

climate_predictors <- names(wclim_ang) # extract climate predictor names, to rename layers in the rasters below 

# This is important to do for making predictions once the SDMs have been made on future climate data 
# Note that the names of the layers still correspond to the same environmental variables 

# Future SSPs 
# Do not need to create RasterStacks 

# SSP 245 

names(ssp245_2030) <- climate_predictors #rename raster layers for downsteam analysis 
names(ssp245_2050) <- climate_predictors 
names(ssp245_2070) <- climate_predictors 

# SSP 585 

names(ssp585_2030) <- climate_predictors #rename raster layers for downstream analysis 
names(ssp585_2050) <- climate_predictors 
names(ssp585_2070) <- climate_predictors 


# Angustifolium - MaxEnt Model----------------------------------------------
# Spatial partitioning preparation 

occ_ang_coords <- as.data.frame(geom(occ_thin_ang)[,3:4]) # extract longitude, latitude from occurence points 

ang_bg_vect<-readRDS(file='ang_bg_vect') 

bg_ang_coords <- as.data.frame(geom(ang_bg_vect)[,3:4]) # extract longitude, latitude from background points 

# Build Species Distribution Model using MaxEnt from the <ENMeval> package 

# Run prediction in a parallel using 'socket' clusters to help speed up computation 

# <ENMeval> implements parallel functions natively 

# But load <parallel> library for additional functions like <decectCores()> 

cn <- detectCores(logical = F) # logical = F, is number of physical RAM cores in your computer 

set.seed(5) 

# current version of maxent.jar =  v3.4.4 

#next line took 5h to run on a server 

ang_maxent <- ENMevaluate(occ_ang_coords, # occurrence records 
                          envs = wclim_ang, # environment from background training area 
                          n.bg = 15000, # 15000 bg points 
                          tune.args = 
                            list(rm = seq(0.5, 8, 0.5), 
                                 fc = c("L", "LQ", "H", 
                                        "LQH", "LQHP", "LQHPT")), 
                          partition.settings = 
                            list(aggregation.factor = c(9, 9), gridSampleN = 15000), # 9,9 agg 
                          partitions = 'checkerboard2', 
                          parallel = TRUE, 
                          numCores = cn - 1, # leave one core available for other apps 
                          parallelType = "doParallel", # use doParrallel on Windows - socket cluster   
                          algorithm = 'maxent.jar') 

# Save the MaxEnt model to not waste time re-running the model 

setwd("~/Documents/Stage RI/R_SDM/MaxEnt/sdm_output") 

saveRDS(ang_maxent, file = 'ang_maxent.Rdata')
ang_maxent <- readRDS(file = 'ang_maxent.Rdata')

#Model Selection -----------------------------------------------------------

# Note that maxent results provide Continuous Boyce Index (cbi) 

res<-ang_maxent@results
subset(ang_maxent@results, cbi.train == 1)
length(subset(ang_maxent@results, cbi.train == 1))
length(ang_maxent@results)

best_ang_maxent <- subset(ang_maxent@results, delta.AICc == 0) # selects the best performing model based on delta AICc - returns data frame object 

mod.best_ang_maxent <- eval.models(ang_maxent)[[best_ang_maxent$tune.args]] # extracts the best model - returns MaxEnt object

# Predictions --------------------------------------------------------------

# Now use the <terra> package to plot the SDM prediction. 

# Wclim is the historical climatic conditions (1970-2000) 

cn <- detectCores(logical = F) # logical = F, is number of physical RAM cores in your computer 

ang_pred_hist <- terra::predict(wclim, mod.best_ang_maxent, cores = cn - 1, na.rm = T) 

setwd("~/Documents/Stage RI/R_SDM") 
saveRDS(ang_pred_hist, file='ang_pred_hist.Rdata') 
ang_pred_hist<-readRDS(file='ang_pred_hist.Rdata')

plot(ang_pred_hist) 

occ_thin_ang<-readRDS(file='occ_thin_ang.Rdata') 

points(occ_thin_ang, cex = 0.05) 

# Evaluate predictions using Boyce Index 

# the number of true presences should decline with suitability groups 100-90, 90-81, etc.  
# First extract suitability values for the background and presence points, make sure to omit NA values 

angPred_bg_val <- terra::extract(ang_pred_hist, bg_ang_coords)$lyr1 %>%  
  na.omit() 

angPred_val_na <- terra::extract(ang_pred_hist, occ_ang_coords)$lyr1 %>%  
  na.omit() 

# Evaluate predictions using Boyce Index 

ecospat.boyce(fit = angPred_bg_val, # vector of predicted habitat suitability of bg points 
              obs = angPred_val_na,  
              nclass = 0,  
              PEplot = TRUE, 
              method = 'spearman') 

# Gradients can be hard to understand at a glance, so lets create categorical bins of high suitability, moderate suitability, low suitability using thresholds 

angPred_val <- terra::extract(ang_pred_hist, occ_ang_coords)$lyr1 
angPred_threshold_1 <- quantile(angPred_val, 0.01, na.rm = T) # Low suitability 
angPred_threshold_10 <- quantile(angPred_val, 0.1, na.rm = T) # Moderate suitability 
angPred_threshold_50 <- quantile(angPred_val, 0.5, na.rm = T) # High suitability 

legend_labs <- c('Low Suitability (1st percentile)', 'Moderate Suitability (10th percentile)', 'High Suitability (50th percentile)') 
fill_cols <- c('#D81B60', '#FFC107', 'darkolivegreen4') 

par(mar = c(5, 5, 5, 5)) 

terra::plot(ang_pred_hist > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F, xlim = c(-110, -50), ylim = c(25, 60), main = 'Historical (1970-2000)') 

terra::plot(ang_pred_hist > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_hist > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map, add = T) 

points(occ_thin_ang, col = 'black', cex = 0.75, pch = 4, alpha=0.5) 

legend(x = c(-75,-55) , y = c(26,33), xpd = NA, inset = c(5, 0),  
       title = 'Habitat Suitability',  
       legend = legend_labs, 
       fill = fill_cols) 

# Future Climate predictions 

# SSP 245 

ang_pred_ssp245_30 <- terra::predict(ssp245_2030, mod.best_ang_maxent, cores = cn - 1, na.rm = T) 
ang_pred_ssp245_50 <- terra::predict(ssp245_2050, mod.best_ang_maxent, cores = cn - 1, na.rm = T) 
ang_pred_ssp245_70 <- terra::predict(ssp245_2070, mod.best_ang_maxent, cores = cn - 1, na.rm = T) 

# SSP 585 

ang_pred_ssp585_30 <- terra::predict(ssp585_2030, mod.best_ang_maxent, cores = cn - 1, na.rm = T) 
ang_pred_ssp585_50 <- terra::predict(ssp585_2050, mod.best_ang_maxent, cores = cn - 1, na.rm = T) 
ang_pred_ssp585_70 <- terra::predict(ssp585_2070, mod.best_ang_maxent, cores = cn - 1, na.rm = T) 

# Plot SSP 585 2030 

par(mar = c(5, 5, 5, 5)) 

terra::plot(ang_pred_ssp585_30 > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F, xlim = c(-110, -50), ylim = c(25, 63), main = 'SSP5-8.5 Early Cent. (2020-2040)') 

terra::plot(ang_pred_ssp585_30 > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_ssp585_30 > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map, add = T) 

legend(x = -74, y = 36, xpd = NA, inset = c(5, 0),  
       title = 'Habitat Suitability',  
       legend = legend_labs, 
       fill = fill_cols, 
       cex=0.8) 


# Save/Load SDM predictions ---------------------------------------- 

setwd('~/Stage RI/R_SDM/SDM_output') 

# Save 

saveRDS(ang_pred_hist, file = 'ang_pred_hist.Rdata')

saveRDS(ang_pred_ssp245_30, file = 'ang_pred_ssp245_30.Rdata') 
saveRDS(ang_pred_ssp245_50, file = 'ang_pred_ssp245_50.Rdata') 
saveRDS(ang_pred_ssp245_70, file = 'ang_pred_ssp245_70.Rdata') 

saveRDS(ang_pred_ssp585_30, file = 'ang_pred_ssp585_30.Rdata') 
saveRDS(ang_pred_ssp585_50, file = 'ang_pred_ssp585_50.Rdata') 
saveRDS(ang_pred_ssp585_70, file = 'ang_pred_ssp585_70.Rdata') 

# Load 
setwd('~/Documents/Stage RI/R_SDM/SDM_output')
ang_pred_hist <- readRDS(file = 'ang_pred_hist.Rdata') 

ang_pred_ssp245_30 <- readRDS(file = 'ang_pred_ssp245_30.Rdata') 
ang_pred_ssp245_50 <- readRDS(file = 'ang_pred_ssp245_50.Rdata') 
ang_pred_ssp245_70 <- readRDS(file = 'ang_pred_ssp245_70.Rdata') 

ang_pred_ssp585_30 <- readRDS(file = 'ang_pred_ssp585_30.Rdata') 
ang_pred_ssp585_50 <- readRDS(file = 'ang_pred_ssp585_50.Rdata') 
ang_pred_ssp585_70 <- readRDS(file = 'ang_pred_ssp585_70.Rdata') 

# Save thresholds ----------------------------------------------- 

setwd('~/Stage RI/R_SDM/SDM_output/Thresholds') 

saveRDS(angPred_threshold_1, file = 'angPred_threshold_1.Rdata') 
saveRDS(angPred_threshold_10, file = 'angPred_threshold_10.Rdata') 
saveRDS(angPred_threshold_50, file = 'angPred_threshold_50.Rdata') 

#Load the thresholds 

setwd("~/Documents/Stage RI/R_SDM/SDM_output/Thresholds") 

angPred_threshold_1 <- readRDS(file = 'angPred_threshold_1.Rdata') 
angPred_threshold_10 <- readRDS(file = 'angPred_threshold_10.Rdata') 
angPred_threshold_50 <- readRDS(file = 'angPred_threshold_50.Rdata') 

# Habitat predictions ----------------------------------------------------- 

# Categorical habitat suitability 

# Historical 

setwd("~/Documents/Stage RI/R_SDM/SDM_output/Habitat_predictions")

ang_pred_high_hist <- ang_pred_hist > angPred_threshold_50 
ang_pred_mod_hist <- ang_pred_hist > angPred_threshold_10 
ang_pred_low_hist <- ang_pred_hist > angPred_threshold_1 

#SSP245  

ang_pred_high_ssp245_30 <- ang_pred_ssp245_30 > angPred_threshold_50 
ang_pred_mod_ssp245_30 <- ang_pred_ssp245_30 > angPred_threshold_10 
ang_pred_low_ssp245_30 <- ang_pred_ssp245_30 > angPred_threshold_1 

ang_pred_high_ssp245_50 <- ang_pred_ssp245_50 > angPred_threshold_50 
ang_pred_mod_ssp245_50 <- ang_pred_ssp245_50 > angPred_threshold_10 
ang_pred_low_ssp245_50 <- ang_pred_ssp245_50 > angPred_threshold_1 

ang_pred_high_ssp245_70 <- ang_pred_ssp245_70 > angPred_threshold_50 
ang_pred_mod_ssp245_70 <- ang_pred_ssp245_70 > angPred_threshold_10 
ang_pred_low_ssp245_70 <- ang_pred_ssp245_70 > angPred_threshold_1 

#SSP585 

ang_pred_high_ssp585_30 <- ang_pred_ssp585_30 > angPred_threshold_50 
ang_pred_mod_ssp585_30 <- ang_pred_ssp585_30 > angPred_threshold_10 
ang_pred_low_ssp585_30 <- ang_pred_ssp585_30 > angPred_threshold_1 

ang_pred_high_ssp585_50 <- ang_pred_ssp585_50 > angPred_threshold_50 
ang_pred_mod_ssp585_50 <- ang_pred_ssp585_50 > angPred_threshold_10 
ang_pred_low_ssp585_50 <- ang_pred_ssp585_50 > angPred_threshold_1 

ang_pred_high_ssp585_70 <- ang_pred_ssp585_70 > angPred_threshold_50 
ang_pred_mod_ssp585_70 <- ang_pred_ssp585_70 > angPred_threshold_10 
ang_pred_low_ssp585_70 <- ang_pred_ssp585_70 > angPred_threshold_1 

# Save 

setwd('~/Stage RI/R_SDM/SDM_output/Habitat_predictions') 

# Historical 

saveRDS(ang_pred_high_hist, file = 'ang_pred_high_hist.Rdata') 
saveRDS(ang_pred_mod_hist, file = 'ang_pred_mod_hist.Rdata') 
saveRDS(ang_pred_low_hist, file = 'ang_pred_low_hist.Rdata') 

# SSP245 

saveRDS(ang_pred_high_ssp245_30, file = 'ang_pred_high_ssp245_30.Rdata') 
saveRDS(ang_pred_mod_ssp245_30, file = 'ang_pred_mod_ssp245_30.Rdata') 
saveRDS(ang_pred_low_ssp245_30, file = 'ang_pred_low_ssp245_30.Rdata') 

saveRDS(ang_pred_high_ssp245_50, file = 'ang_pred_high_ssp245_50.Rdata') 
saveRDS(ang_pred_mod_ssp245_50, file = 'ang_pred_mod_ssp245_50.Rdata') 
saveRDS(ang_pred_low_ssp245_50, file = 'ang_pred_low_ssp245_50.Rdata') 

saveRDS(ang_pred_high_ssp245_70, file = 'ang_pred_high_ssp245_70.Rdata') 
saveRDS(ang_pred_mod_ssp245_70, file = 'ang_pred_mod_ssp245_70.Rdata') 
saveRDS(ang_pred_low_ssp245_70, file = 'ang_pred_low_ssp245_70.Rdata') 

# SSP585 

saveRDS(ang_pred_high_ssp585_30, file = 'ang_pred_high_ssp585_30.Rdata') 
saveRDS(ang_pred_mod_ssp585_30, file = 'ang_pred_mod_ssp585_30.Rdata') 
saveRDS(ang_pred_low_ssp585_30, file = 'ang_pred_low_ssp585_30.Rdata') 

saveRDS(ang_pred_high_ssp585_50, file = 'ang_pred_high_ssp585_50.Rdata') 
saveRDS(ang_pred_mod_ssp585_50, file = 'ang_pred_mod_ssp585_50.Rdata') 
saveRDS(ang_pred_low_ssp585_50, file = 'ang_pred_low_ssp585_50.Rdata') 

saveRDS(ang_pred_high_ssp585_70, file = 'ang_pred_high_ssp585_70.Rdata') 
saveRDS(ang_pred_mod_ssp585_70, file = 'ang_pred_mod_ssp585_70.Rdata') 
saveRDS(ang_pred_low_ssp585_70, file = 'ang_pred_low_ssp585_70.Rdata') 

# Load 

#Historical 

ang_pred_high_hist<-readRDS(file = 'ang_pred_high_hist.Rdata') 
ang_pred_mod_hist<-readRDS(file = 'ang_pred_mod_hist.Rdata') 
ang_pred_low_hist<-readRDS(file = 'ang_pred_low_hist.Rdata') 

#SSP245 

ang_pred_high_ssp245_30 <- readRDS(file = 'ang_pred_high_ssp245_30.Rdata') 
ang_pred_mod_ssp245_30 <- readRDS(file = 'ang_pred_mod_ssp245_30.Rdata') 
ang_pred_low_ssp245_30 <- readRDS(file = 'ang_pred_low_ssp245_30.Rdata') 

ang_pred_high_ssp245_50 <- readRDS(file = 'ang_pred_high_ssp245_50.Rdata') 
ang_pred_mod_ssp245_50 <- readRDS(file = 'ang_pred_mod_ssp245_50.Rdata') 
ang_pred_low_ssp245_50 <- readRDS(file = 'ang_pred_low_ssp245_50.Rdata') 

ang_pred_high_ssp245_70<-readRDS(file = 'ang_pred_high_ssp245_50.Rdata') 
ang_pred_mod_ssp245_70<-readRDS(file = 'ang_pred_mod_ssp245_50.Rdata') 
ang_pred_low_ssp245_70<-readRDS(file = 'ang_pred_low_ssp245_50.Rdata') 

#SSP585 

ang_pred_high_ssp585_30<-readRDS(file = 'ang_pred_high_ssp585_30.Rdata') 
ang_pred_mod_ssp585_30<-readRDS(file = 'ang_pred_mod_ssp585_30.Rdata') 
ang_pred_low_ssp585_30<-readRDS(file = 'ang_pred_low_ssp585_30.Rdata') 

ang_pred_high_ssp585_50<-readRDS(file = 'ang_pred_high_ssp585_50.Rdata') 
ang_pred_mod_ssp585_50<-readRDS(file = 'ang_pred_mod_ssp585_50.Rdata') 
ang_pred_low_ssp585_50<-readRDS(file = 'ang_pred_low_ssp585_50.Rdata') 

ang_pred_high_ssp585_70<-readRDS(file = 'ang_pred_high_ssp585_70.Rdata') 
ang_pred_mod_ssp585_70<-readRDS(file = 'ang_pred_mod_ssp585_70.Rdata') 
ang_pred_low_ssp585_70<-readRDS(file = 'ang_pred_low_ssp585_70.Rdata') 

# Binary pred habitat GAP ANALYSIS ---------------------------------------- 

setwd("~/Documents/Stage RI/R_SDM")
wclim_ang<-readRDS(file='wclim_ang.Rdata')

ang_pa <- predicts::pa_evaluate(p = occ_ang_coords_mat, a = bg_ang_coords_mat, model = ang_maxent, x = wclim_ang) 

ang_threshold <- predicts::threshold(ang_pa) 

ang_hist_habitat <- ang_pred_hist > ang_threshold$max_spec_sens #the threshold at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest 

# SDM plotting and map making--------------------------------------------- 

library(tidyverse) # Grammar and data management 
library(terra) # Spatial Data package 
library(geodata) # Basemaps 

# Load basemaps and sdm raster layers ------------------------------------- 

# Occurrence Points in SpatVectors 

setwd("~/Documents/Stage RI/R_SDM") 
occ_thin_ang <- readRDS(file = 'occ_thin_ang.Rdata') 

# Download/load basemaps 

us_map <- gadm(country = 'USA', level = 1, resolution = 2, 
               path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM/gadm") 

us_map_0 <- gadm(country = 'USA', level = 0, resolution = 2, 
                 path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM/gadm") #USA basemap without States 

ca_map <- gadm(country = 'CA', level = 1, resolution = 2, 
               path = '/Users/elise.cadiou/Documents/Stage RI/R_SDM/gadm') 

ca_map_0 <- gadm(country = 'CA', level = 0, resolution = 2, 
                 path = '/Users/elise.cadiou/Documents/Stage RI/R_SDM/gadm') # Canada basemap without Provinces 

canUS_map <- rbind(us_map, ca_map) # Combine US and Canada vector map 
canUS_map_0 <- rbind(us_map_0, ca_map_0) # Combine Country boundaries only 

NA_ext <- ext(-180, -30, 18, 85) # Set spatial extent of analyis to NA in Western Hemisphere 

canUS_map <- crop(canUS_map, NA_ext) # crop to Western Hemisphere 
canUS_map_0 <- crop(canUS_map_0, NA_ext) # crop to Western Hemisphere 

# Predicted habitat suitability rasters 

ang_pred_hist <- readRDS(file = 'ang_pred_hist.Rdata') 
setwd("~/Documents/Stage RI/R_SDM/SDM_output") 

ang_pred_ssp245_30 <- readRDS(file = 'ang_pred_ssp245_30.Rdata') 
ang_pred_ssp245_50 <- readRDS(file = 'ang_pred_ssp245_50.Rdata') 
ang_pred_ssp245_70 <- readRDS(file = 'ang_pred_ssp245_70.Rdata') 

ang_pred_ssp585_30 <- readRDS(file = 'ang_pred_ssp585_30.Rdata') 
ang_pred_ssp585_50 <- readRDS(file = 'ang_pred_ssp585_50.Rdata') 
ang_pred_ssp585_70 <- readRDS(file = 'ang_pred_ssp585_70.Rdata') 

# Thresholds 

setwd("~/Documents/Stage RI/R_SDM/SDM_output/Thresholds") 

angPred_threshold_1 <- readRDS(file = 'angPred_threshold_1.Rdata') 
angPred_threshold_10 <- readRDS(file = 'angPred_threshold_10.Rdata') 
angPred_threshold_50 <- readRDS(file = 'angPred_threshold_50.Rdata') 

# Project to Lambert Conformal Conic -------------------------------------- 

projLam <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" 

# Basemaps 

canUS_map.lcc <- project(canUS_map, projLam) 
canUS_map_0.lcc <- project(canUS_map_0, projLam) 

# Occurrences 

occ_thin_ang.lcc <- project(occ_thin_ang, projLam) 


# Habitat suitability 

ang_pred_hist.lcc <- project(ang_pred_hist, projLam) 

ang_pred_ssp245_30.lcc <- project(ang_pred_ssp245_30, projLam) 
ang_pred_ssp245_50.lcc <- project(ang_pred_ssp245_50, projLam) 
ang_pred_ssp245_70.lcc <- project(ang_pred_ssp245_70, projLam) 

ang_pred_ssp585_30.lcc <- project(ang_pred_ssp585_30, projLam) 
ang_pred_ssp585_50.lcc <- project(ang_pred_ssp585_50, projLam) 
ang_pred_ssp585_70.lcc <- project(ang_pred_ssp585_70, projLam) 

#Save
setwd("~/Documents/Stage RI/R_SDM/SDM_output")
saveRDS(ang_pred_hist.lcc, file = 'ang_pred_hist.lcc.Rdata')

saveRDS(ang_pred_ssp245_30.lcc, file = 'ang_pred_ssp245_30.lcc.Rdata')
saveRDS(ang_pred_ssp245_50.lcc, file = 'ang_pred_ssp245_50.lcc.Rdata')
saveRDS(ang_pred_ssp245_70.lcc, file = 'ang_pred_ssp245_70.lcc.Rdata')

saveRDS(ang_pred_ssp585_30.lcc, file = 'ang_pred_ssp585_30.lcc.Rdata')
saveRDS(ang_pred_ssp585_50.lcc, file = 'ang_pred_ssp585_50.lcc.Rdata')
saveRDS(ang_pred_ssp585_70.lcc, file = 'ang_pred_ssp585_70.lcc.Rdata')


# Future habitat plot ---------------------------------------- 

# Predicted historical distribtuion 

legend_labs <- c('Low Suitability (1st percentile)', 'Moderate Suitability (10th percentile)', 'High Suitability (50th percentile)') 

fill_cols <- c('#D81B60', '#FFC107', 'darkolivegreen4') 

ang.xlim <- c(-5*10^5, 3.1*10^6) 
ang.ylim <- c(-2*10^6, 2*10^6) 

# Plot a legend that can be added on its own 

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1) 
legend('center', xpd = NA, title = c(as.expression(bquote(bold('Habitat Suitability')))), legend = legend_labs, fill = fill_cols, cex = 2) 

terra::plot(ang_pred_hist.lcc > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F,  
            xlim = ang.xlim, ylim = ang.ylim,  
            main = 'Historical (1970-2000)', 
            cex.main = 1.5, 
            axes = F, 
            box = T, 
            mar = c(1, 0, 2, 0)) 

terra::plot(ang_pred_hist.lcc > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_hist.lcc > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map.lcc, add = T) 

# Make a figure with three plots next to one another 

par(mfrow = c(1, 3)) 

# SSP245 

# 2030 

terra::plot(ang_pred_ssp245_30.lcc > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F,  
            xlim = ang.xlim, ylim = ang.ylim,  
            main = '2021-2040', 
            cex.main = 3, 
            axes = F, 
            box = T, 
            mar = c(0, 0.1, 0, 0)) 

terra::plot(ang_pred_ssp245_30.lcc > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_ssp245_30.lcc > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map.lcc, add = T) 

# 2050 

terra::plot(ang_pred_ssp245_50.lcc > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F,  
            xlim = ang.xlim, ylim = ang.ylim,  
            main = '2041-2060', 
            cex.main = 3, 
            axes = F, 
            box = T, 
            mar = c(0, 0, 0, 0)) 

terra::plot(ang_pred_ssp245_50.lcc > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_ssp245_50.lcc > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map.lcc, add = T) 

# 2070 

terra::plot(ang_pred_ssp245_70.lcc > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F,  
            xlim = ang.xlim, ylim = ang.ylim,  
            main = '2061-2080', 
            cex.main = 3, 
            axes= F, 
            box = T, 
            mar = c(0, 0, 0,  0.1)) 

terra::plot(ang_pred_ssp245_50.lcc > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_ssp245_50.lcc > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map.lcc, add = T) 

# SSP585 

par(mfrow = c(1, 3)) 

# 2030 

terra::plot(ang_pred_ssp585_30.lcc > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F,  
            xlim = ang.xlim, ylim = ang.ylim,  
            main = '2021-2040', 
            cex.main = 3, 
            axes = F, 
            box = T, 
            mar = c(0, 0, 0, 0)) 

terra::plot(ang_pred_ssp585_30.lcc > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_ssp585_30.lcc > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map.lcc, add = T) 

# 2050 

terra::plot(ang_pred_ssp585_50.lcc > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F,  
            xlim = ang.xlim, ylim = ang.ylim,  
            main = '2041-2060', 
            cex.main = 3, 
            axes = F, 
            box = T, 
            mar = c(0, 0, 0, 0)) 

terra::plot(ang_pred_ssp585_50.lcc > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_ssp585_50.lcc > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map.lcc, add = T) 

# 2070 

terra::plot(ang_pred_ssp585_70.lcc > angPred_threshold_1, col = c('lightgrey', '#D81B60'), legend = F,  
            xlim = ang.xlim, ylim = ang.ylim,  
            main = '2061-2080', 
            cex.main = 3, 
            axes= F, 
            box = T, 
            mar = c(0, 0, 0,  0)) 

terra::plot(ang_pred_ssp585_50.lcc > angPred_threshold_10, col = c(NA, '#FFC107'), add = T, legend = F) 

terra::plot(ang_pred_ssp585_50.lcc > angPred_threshold_50, col = c(NA, 'darkolivegreen4'), add = T, legend = F) 

terra::plot(canUS_map.lcc, add = T) 



p<-freq(ang_pred_ssp245_70.lcc > angPred_threshold_50)
p$area<-p$count*5.135
p
