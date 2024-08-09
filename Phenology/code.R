setwd("~/Documents/Stage RI/Phenology/Data") 

library(tidyverse) #includes ggplot2 as well 
library(magrittr) 

pheno <- read_excel("ECSmithHerbariumAccessions_PhenologyScoring.xlsx", 
                                                          col_types = c("text", "text", "text", 
                                                          "date", "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "text", "text", "text", "text", "text", 
                                                          "numeric", "text", "text", "text", 
                                                          "text", "text", "text", "text", "numeric", 
                                                          "text"))
pheno<-ECSmithHerbariumAccessions_PhenologyScoring

#Filtering 

angus1<- pheno %>% 
  filter(`Scientific Name` =='Vaccinium angustifolium' | `Scientific Name` =='Vaccinium corymbosum' |  `Scientific Name` =='Vaccinium myrtilloides') 

angus2<-angus1 %>% 
  filter(!(institutionCode=='HIBG')) 

angus3<-angus2 %>% 
  select(`Scientific Name`, eventDate, decimalLatitude, decimalLongitude, Bud:Only_Vegetative) 

clean_data_3sp<-angus3 %>% 
  filter(!(decimalLatitude=='NA'|decimalLongitude=='NA')) 

setwd("~/Documents/Stage RI/Phenology")
saveRDS(clean_data_3sp, file = 'clean_data_3sp.Rdata' )
clean_data_3sp<- readRDS(file = 'clean_data_3sp.Rdata')

#Map the records 

library(geodata) 

us_map<-gadm(country='USA', level=1, resolution=2,  
             path= "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM") 

ca_map <- gadm(country = 'CA', level = 1, resolution = 2, 
               path = "C:\\Users\\elise\\Documents\\Stage RI\\R_SDM") 

canUS_map <- rbind(us_map, ca_map) 

plot(canUS_map, xlim = c(-75, -50), ylim = c(40, 55)) 

points(clean_data_3sp$decimalLongitude, clean_data_3sp$decimalLatitude, pch = 16, 
       col = alpha("red", 0.2)) 

#Plots 
#Date of sampling (histo/density plot) 
#Look at cheatsheets for ggplots tricks 

clean_data_3sp %>% ggplot(aes(Year, fill = `Scientific Name`))+geom_histogram(bins = 116, mar = c(5, 5), col = 'black', size = 0.2)+labs(title="Time distribution", fill = "Species", y="Number of records", x="Year")+theme_light()+scale_x_continuous(breaks=seq(1900, 2020, 20))

Vacang<-clean_data_3sp %>% filter(`Scientific Name`=='Vaccinium angustifolium')
median(Vacang$Year)
mean(Vacang$Year)
Vacmyr<-clean_data_3sp %>% filter(`Scientific Name`=='Vaccinium myrtilloides')
median(Vacmyr$Year)
mean(Vacmyr$Year)

# Tables to know the number of records for a certain dev stage (or several) 
table(clean_data_3sp$Bud) 
table(clean_data_3sp$Flower) 
table(clean_data_3sp$Seed_disperse) 
table(clean_data_3sp$Fruit) 
table(clean_data_3sp$Only_Vegetative) 
table(clean_data_3sp$Bud, angus4$Flower) 
table(clean_data_3sp$Fruit, angus4$Flower) 
table(clean_data_3sp$Bud, angus4$Seed_disperse) 
table(clean_data_3sp$Bud, angus4$Flower, angus4$Fruit) 
table(clean_data_3sp$bud, ) 

table(clean_data_3sp$`Scientific Name`)

#separate month, day from year (add 2 column : 1 with only year and 1 day+month) 

library(lubridate) 

year<-format(clean_data_3sp$eventDate, format = "%Y") 
month<-format(clean_data_3sp$eventDate, format = "%m") 
day<-format(clean_data_3sp$eventDate, format = "%d") 
clean_data_3sp$year<-year 
clean_data_3sp$month<-month 

library(ggplot2) 

clean_data_3sp %>% ggplot(aes(year(eventDate),fill=`Scientific Name`))+geom_density(alpha=0.5)+xlab("Time")+ theme_minimal()+scale_fill_hue(labels=c("V. angustifolium", "V. myrtilloides"))+theme(legend.position= "top")+

clean_data_3sp %>% ggplot(aes(month(eventDate),fill=`Scientific Name`))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+scale_fill_hue(labels=c("V. angustifolium", "V. myrtilloides"))+theme(legend.position= "top")

clean_data_3sp %>% ggplot(aes(month(eventDate),fill=Flower))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Flower")+scale_fill_hue(labels=c("Yes", "None"))+theme(legend.position= "top") 

clean_data_3sp %>% ggplot(aes(month(eventDate),fill=Flower))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Flower")+scale_fill_hue(labels=c("Yes", "None"))+theme(legend.position= "top")

clean_data_3sp %>% ggplot(aes(month(eventDate),fill=Fruit))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Fruits")+scale_fill_hue(labels=c("Yes", "None"))+theme(legend.position= "top") 

clean_data_3sp %>% ggplot(aes(month(eventDate),fill=Bud))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Bud")+scale_fill_hue(labels=c("Yes", "None"))+theme(legend.position= "top") 

clean_data_3sp %>% ggplot(aes(month(eventDate),fill=Seed_disperse))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Mature fruits")+scale_fill_hue(labels=c("Yes", "None"))+theme(legend.position= "top") 

clean_data_3sp %>% ggplot(aes(month(eventDate),fill=Only_Vegetative))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Vegetative stage")+scale_fill_hue(labels=c("Yes", "No"))+theme(legend.position= "top") 

ymd(clean_data_3sp$eventDate) #displays all dates 'yyyy-mm-dd" 
month(clean_data_3sp$eventDate, label=TRUE) #displays the month of each record 
year(clean_data_3sp$eventDate) 

table(year(clean_data_3sp$eventDate)) #shows how many record/year of records 

clean_data_3sp$BF<-paste(angus4$Bud, angus4$Flower) 

angus5<-clean_data_3sp %>% 
  filter(!(clean_data_3sp$BF=='NA Y'|angus4$BF=='Y NA')) 

angus5 %>% ggplot(aes(month(eventDate),fill=BF))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Buds and flower")+scale_fill_hue(labels=c("Neither", "Both"))+theme(legend.position= "top") 

clean_data_3sp$FF<-paste(clean_data_3sp$Fruit, clean_data_3sp$Seed_disperse) 

angus6<-angus4 %>% 
  filter(angus4$FF=='Y Y'|angus4$FF=='NA NA') 

angus6 %>% ggplot(aes(month(eventDate),fill=FF))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Immature and mature fruits")+scale_fill_hue(labels=c("Neither", "Both"))+theme(legend.position= "top") 

clean_data_3sp$BFF<-paste(clean_data_3sp$Bud, clean_data_3sp$Flower, clean_data_3sp$Fruit) 

angus7<-clean_data_3sp %>% 
  filter(clean_data_3sp$BFF=='Y Y Y'|clean_data_3sp$BFF=='NA NA NA') 

angus7 %>% ggplot(aes(month(eventDate),fill=BFF))+geom_density(alpha=0.5)+xlab("Months of the year")+ theme_minimal()+labs(fill="Buds, flowers and immature fruits")+scale_fill_hue(labels=c("Neither", "All"))+theme(legend.position= "top") 


#----------------------------------------------------
#To add climate data to the analyses we are going to download it from the CRU TS Version 4.07 which is available here:https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/

#This is the citation that should be used for these data: https://www.nature.com/articles/s41597-020-0453-3

#Variables: pre, tmp, tmx, tmn, dtr, vap, cld, wet, frs, pet
#Variables include cloud cover (cld), diurnal temperature range (dtr), frost day frequency (frs), precipitation (pre), daily mean temperature (tmp), monthly average daily maximum (tmx) and minimum (tmn) temperature, vapour pressure (vap), Potential Evapo-transpiration (pet) and wet day frequency (wet)

#From these, we are interested in tmp primarily which is the daily mean temperature 

#For variables of interest, we download from https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/cruts.2304141047.v4.07/

#Select the file name, for example, tmp for temperature and download the file that covers the full period (1901.2022), such as cru_ts4.07.1901.2022.tmp.dat.nc.gz it should end with dat.nc.gz and you can copy the link to download 

#To get the matching measurements for the phenology data, we can use this function to extract the data

install.packages("raster")
install.packages("R.utils")
install.packages("ncdf4")
library(magrittr)
library(raster)
library(tidyverse)
library(R.utils) #for the gunzip function
library(ncdf4) #for the gunzip function

# Unzipping the dataset
gunzip("cru_ts4.07.1901.2022.tmp.dat.nc.gz",
       remove = TRUE, overwrite = TRUE)

#Load in temperature data
setwd("~/Documents/Stage RI/Phenology/Data")
temperature_data <- raster::brick('cru_ts4.07.1901.2022.tmp.dat.nc')

temperature_data

#There are 1464 temperatures (nlayers), one for each month from across 122 years 

nlayers(temperature_data)
#1464

#Load in vaccinium data
setwd("~/Documents/Stage RI/Phenology")
vaccinium <- readRDS("clean_data_3sp.Rdata")

#What dates do these cover?
sort(vaccinium$eventDate)[1]
#[1] "1901-07-02 UTC 

#CRU only goes to 1901 but turns out that is where the data start or we would have to filter down to 1901 and later only 

#Add a column to the data set for the Year of sampling
vaccinium <- vaccinium %>% mutate(Year=year(eventDate))

#Filter out any without a year (there are some that appear to be missing this information)

vaccinium <- vaccinium %>% filter(!is.na(vaccinium$Year))
#427 records (out of 429) are kept

#Filter out any with lat and long (there are some that appear to be missing this information)

vaccinium <- vaccinium %>% filter(decimalLatitude != 0) %>% filter(decimalLongitude != 0)
#reduced dataset is 337

#how many samples do we have left for each species?
table(vaccinium$`Scientific Name`)

#Vaccinium angustifolium    Vaccinium corymbosum  Vaccinium myrtilloides 
#217                      47                      73 

#Order based on year
vaccinium <- vaccinium %>% arrange(Year)

#We want to go through the CRU data and for each year that we have herbarium data for grab the mean temp for March, April, May, June, July, August

#create an empty vector to store data
march_temperature_data <- numeric(nrow(vaccinium))
april_temperature_data <- numeric(nrow(vaccinium))
may_temperature_data <- numeric(nrow(vaccinium))
june_temperature_data<-numeric(nrow(vaccinium))
july_temperature_data<-numeric(nrow(vaccinium))
august_temperature_data<-numeric(nrow(vaccinium))
sept_temperature_data<-numeric(nrow(vaccinium))
oct_temperature_data<-numeric(nrow(vaccinium))
# Loop through each row in the coordinate file
for (i in 1:nrow(vaccinium)) {
  year <- vaccinium$Year[i]
  lat <- vaccinium$decimalLatitude[i]
  lon <- vaccinium$decimalLongitude[i]
  
  # Calculate the index for april of the given year
  layer_march_index <- (year - 1901) * 12 + 3
  layer_april_index <- (year - 1901) * 12 + 4
  layer_may_index <- (year - 1901) * 12 + 5
  layer_june_index <- (year - 1901) * 12 + 6
  layer_july_index <- (year - 1901) * 12 + 7
  layer_august_index <- (year - 1901) * 12 + 8
  layer_sept_index <- (year - 1901) * 12 + 9
  layer_oct_index <- (year - 1901) * 12 + 10
  
  # Extract the temperature value for the given coordinates
  point <- SpatialPoints(cbind(lon, lat))
  temp_value_march <- raster::extract(temperature_data[[layer_march_index]], point)
  temp_value_april <- raster::extract(temperature_data[[layer_april_index]], point)
  temp_value_may <- raster::extract(temperature_data[[layer_may_index]], point)
  temp_value_june <- raster::extract(temperature_data[[layer_june_index]], point)
  temp_value_july <- raster::extract(temperature_data[[layer_july_index]], point)
  temp_value_august <- raster::extract(temperature_data[[layer_august_index]], point)
  temp_value_sept <- raster::extract(temperature_data[[layer_sept_index]], point)
  temp_value_oct <- raster::extract(temperature_data[[layer_oct_index]], point)
  
  # Store the temperature value in the vector
  march_temperature_data[i] <- temp_value_march
  april_temperature_data[i] <- temp_value_april
  may_temperature_data[i] <- temp_value_may
  june_temperature_data[i] <- temp_value_june
  july_temperature_data[i] <- temp_value_july
  august_temperature_data[i] <- temp_value_august
  sept_temperature_data[i] <- temp_value_sept
  oct_temperature_data[i] <- temp_value_oct
}

# Add the extracted temperature data to the coordinates data frame
vaccinium$march_temperature <- march_temperature_data
vaccinium$april_temperature <- april_temperature_data
vaccinium$may_temperature <- may_temperature_data
vaccinium$june_temperature <- june_temperature_data
vaccinium$july_temperature <- july_temperature_data
vaccinium$august_temperature <- august_temperature_data
vaccinium$sept_temperature <- sept_temperature_data
vaccinium$oct_temperature <- oct_temperature_data

#If we want to average the temperatures of several months (could modify the loop to take into consideration other months):

#mean_temp_data<-numeric(nrow(cleandata))
#for (i in 1:nrow(cleandata)) {
  year <- cleandata$Year[i]
  lat <- cleandata$decimalLatitude[i]
  lon <- cleandata$decimalLongitude[i]
  
  # Calculate mean temperature index
  mean_temp_index<-(cleandata$march_temperature[i]+
                      cleandata$april_temperature[i]+
                      cleandata$may_temperature[i])/3
  
  # Extract mean temperature value
  point<-SpatialPoints(cbind(lon, lat))
  mean_value_temp<-raster::extract(temperature_data, point)
  
  # Store mean temperature value in mean_temp_data
  mean_temp_data[i]<-mean(mean_value_temp, na.rm = TRUE)
#}

# Assign mean_temp_data to cleandata$mean_temp column
#cleandata$mean_temp<-mean_temp_data


#There are a couple for which temperature data were not available so these have also been filtered out = data from Cape Sable Island + one from Black Point, NS

vaccinium %>% filter(is.na(march_temperature))
vaccinium %>% filter(is.na(april_temperature))
vaccinium %>% filter(is.na(may_temperature))
vaccinium %>% filter(is.na(june_temperature))
vaccinium %>% filter(is.na(july_temperature))
vaccinium %>% filter(is.na(august_temperature))
vaccinium %>% filter(is.na(sept_temperature))
vaccinium %>% filter(is.na(oct_temperature))

#These are the same six missing data

vaccinium <- vaccinium %>% filter(!is.na(may_temperature)) %>% filter(!is.na(april_temperature)) %>% filter(!is.na (march_temperature)) %>% filter(!is.na (june_temperature)) %>% filter(!is.na (july_temperature))%>% filter(!is.na (august_temperature)) %>% filter(!is.na (sept_temperature))%>% filter(!is.na (oct_temperature))
#originally had 337, 6 removed - there are 331 datapoints left 

#Plot temperature against year (for years which have herbarium data which we sampled in this project, may not represent all data)

vaccinium %>% ggplot(aes(x=Year,y=may_temperature))+
  geom_point(colour="darkslategray", size=1,shape="circle")+
  geom_smooth(method='lm', alpha=0.05, colour="red", se=F)+
  theme_test() +
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="Mean May temperature")

vaccinium %>% ggplot(aes(x=Year,y=april_temperature))+
  geom_point(colour="darkslategray", size=1,shape="circle")+
  geom_smooth(method='lm', alpha=0.05, colour="red", se=F)+
  theme_test() +
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="Mean April temperature")

#There is one temperature in there for May which is negative, when I looked up the location this was sampled in northern Kuujjuarapik, QC, however it seems to be driving these trends a bit falsely. In regards to the species' historical distribution model, it should'nt have been found there.

#Also investigate the one really high temp value for a corymbosum sample --> missing '-' before the longitude so have to replace the value: (I actually have changed the value directly in the spreadsheet instead)
#vaccinium<- replace(vaccinium$decimalLongitude, 318, '-65,22830')

vaccinium <- vaccinium %>% filter(may_temperature != -0.5)  %>% filter(may_temperature < 20)
#329 left

# plus, there is another longitude value incorrect (85) that gives a may mean tmp of 16 so we have to delete it as well
vaccinium <- vaccinium %>% filter(decimalLongitude != 85.00000)
#There are 328 data points left

#Reformat so we have just month and day
#I've kept a placeholder year for this so that it still treated like a date 

vaccinium <- vaccinium %>% mutate(date=make_date(year = 1900, month = month(eventDate), day = day(eventDate)))

#Generate a new column that summarizes the other five phenological data columns, which are: Bud, Flower, Fruit, Seed_disperse, Only_Vegetative

vaccinium <-  vaccinium %>% mutate(pheno=ifelse(is.na(Bud), "", "Bud - "), pheno=paste(pheno, ifelse(is.na(Flower), "", "Flower - ")), pheno=paste(pheno, ifelse(is.na(Fruit), "", "Fruit - ")), pheno=paste(pheno, ifelse(is.na(Seed_disperse), "", "Disperse - ")), pheno=paste(pheno, ifelse(is.na(Only_Vegetative), "", "Veg")))

#We will remove the samples without phenological data - away around this to strip white space from the column

vaccinium$pheno <- str_trim(vaccinium$pheno)

table(vaccinium$pheno)
vaccinium %>%filter(vaccinium$pheno == '')
#based on this there are 7 samples that do not have any data associated with them

vaccinium <- vaccinium %>% filter(pheno != "")
#321 datapoints remaining

setwd("~/Documents/Stage RI/Phenology")
saveRDS(vaccinium, file = 'vaccinium.Rdata' )

#-----------------------------------------------------------------------
#Cleaning and preliminary plots
library(tidyverse) #includes ggplot2 as well 
library(magrittr) 

setwd("~/Documents/Stage RI/Phenology")
clean_data_3sp<- readRDS(file='vaccinium.Rdata')

clean_data_3sp<- clean_data_3sp %>% filter(!`Scientific Name`== 'Vaccinium corymbosum')
#277 remaining

summary(clean_data_3sp)
median(clean_data_3sp$Year)
mean(cleandaclean_data_3spta$Year)
table(clean_data_3sp$`Scientific Name`)

clean_data_3sp %>% filter(Flower=="Y") %>% ggplot(aes(x=date, y=may_temperature, colour=Year))+
  geom_point()+
  geom_smooth(method='lm', alpha=0.05, colour="firebrick2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with flowers)", y= "Mean May Temperature")+
  facet_grid(~`Scientific Name`)

#three vouchers in september seem atypical so I delete them to see how the line would look without them

clean_data_3sp<-clean_data_3sp %>% filter(!(cleandata$date=='1900-09-16' & cleandata$Year==1920))
clean_data_3sp<-clean_data_3sp %>% filter(!cleandata$date=='1900-09-14')
clean_data_3sp<-clean_data_3sp %>% filter(!cleandata$date=='1900-09-07')

setwd("~/Documents/Stage RI/Phenology")
saveRDS(clean_data_3sp, file='clean_data_3sp.Rdata')

clean_data_3sp %>% filter(Flower=="Y") %>% ggplot(aes(x=date, y=may_temperature, colour=Year))+
  geom_point()+
  geom_smooth(method='lm', alpha=0.05, colour="firebrick2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with flowers)", y= "Mean May Temperature")+
  facet_grid(~`Scientific Name`)
#Now we see that it is still the same trend with observations of flowers being held later when may temperature are decreasing. But we can't really see anything trend regarding the year of collection.What about for April?

clean_data_3sp %>% filter(Flower=="Y") %>% ggplot(aes(x=date, y=april_temperature, colour=Year))+
  geom_point()+
  geom_smooth(method='lm', alpha=0.05, colour="firebrick2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with flowers)", y= "Mean April Temperature")+
  facet_grid(~`Scientific Name`)
#Same trend

clean_data_3sp %>% filter(Flower=="Y") %>% ggplot(aes(x=date, y=march_temperature, colour=Year))+
  geom_point()+
  geom_smooth(method='lm', alpha=0.05, colour="firebrick2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with flowers)", y= "Mean March Temperature")+
  facet_grid(~`Scientific Name`)

clean_data_3sp %>% filter(Flower=='Y') %>% ggplot(aes(x=date, y=may_temperature, colour=pheno))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_test() +
  labs(x="Date of Sampling", y= "Mean May Temperature")+
  facet_grid(~`Scientific Name`)
#Select all the vouchers with at least one flower. All the possible combinations show the same trend as earlier even though the steepness are different except the Bud - Flower scenario. What about April?


clean_data_3sp %>% filter(Flower=='Y') %>% ggplot(aes(x=date, y=april_temperature, colour=pheno))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_test() +
  labs(x="Date of Sampling", y= "Mean April Temperature")+
  facet_grid(~`Scientific Name`)
#Exactly the same

clean_data_3sp %>% filter(Flower=='Y') %>% ggplot(aes(x=date, y=march_temperature, colour=pheno))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_test() +
  labs(x="Date of Sampling", y= "Mean March Temperature")+
  facet_grid(~`Scientific Name`)

clean_data_3sp %>% filter(Flower=='Y') %>% ggplot(aes(x=date, y=april_temperature, colour=))+
  geom_point()+
  geom_smooth(method='lm', alpha=0.05, se=F, colour="firebrick2")+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean April Temp")+
  facet_grid(~`Scientific Name`)


#Plot one phenological stage to compare with the other ones (may_tmp)

clean_data_3sp %>% ggplot(aes(x=date, y=may_temperature, colour=Bud))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE, aes(colour=Bud))+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean May Temp")+
  facet_grid(~`Scientific Name`)

clean_data_3sp %>% ggplot(aes(x=date, y=may_temperature, colour=Flower))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE, aes(colour=Flower))+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean May Temp")+
  facet_grid(~`Scientific Name`)

clean_data_3sp %>% ggplot(aes(x=date, y=may_temperature, colour=Fruit))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE, aes(colour=Fruit))+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean May Temp")+
  facet_grid(~`Scientific Name`)

clean_data_3sp %>% ggplot(aes(x=date, y=may_temperature, colour=Seed_disperse))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE, aes(colour=Seed_disperse))+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean May Temp", col= 'Seed disperse')+
  facet_grid(~`Scientific Name`)

clean_data_3sp %>% ggplot(aes(x=date, y=may_temperature, colour=Only_Vegetative))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE, aes(colour=Only_Vegetative))+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean May Temp", col = 'Only vegetative')+
  facet_grid(~`Scientific Name`)


clean_data_3sp %>% ggplot(aes(y=date, x=Year, colour=pheno))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  theme_test() 

#Some exploratory visualization 

clean_data_3sp %>% filter(Flower=="Y") %>% ggplot(aes(x=date, y=may_temperature))+
  geom_point(colour="darkslategray", size=1,shape="circle")+
  geom_smooth(method='lm', alpha=0.05, colour="darkorange2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with flowers)", y= "Mean May Temperature")

clean_data_3sp %>% filter(Flower=="Y") %>% ggplot(aes(x=date, y=may_temperature, colour=Year))+
  geom_point()+
  geom_smooth(method='lm', alpha=0.05, colour="darkorange2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with flowers)", y= "Mean May Temperature")

clean_data_3sp %>% filter(Flower=="Y") %>% ggplot(aes(x=date, y=may_temperature))+
  geom_point(colour="darkslategray", size=1,shape="circle")+
  geom_smooth(method='lm',  alpha=0.05, colour="darkorange2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with flowers)", y= "Mean May Temperature")

clean_data_3sp %>% filter(Fruit=="Y") %>% ggplot(aes(x=date, y=may_temperature))+
  geom_point(colour="darkslategray", size=1,shape="circle")+
  geom_smooth(method='lm',  alpha=0.05, colour="darkorange2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with fruit)", y= "Mean May Temperature")

#How about the temperature for July? Would that impact when the fruit developed?

clean_data_3sp %>% filter(Fruit=="Y") %>% ggplot(aes(x=date, y=may_temperature, colour=Year))+
  geom_point()+
  geom_smooth(method='lm', alpha=0.05, colour="darkorange2", se=F)+
  theme_test() +
  labs(x="Date of Sampling (with fruit)", y= "Mean May Temperature")

#We also have the pheno column that labels with all information so we can filter based on that to only include things that have fruits and flowers vs flowers only etc 

clean_data_3sp %>% ggplot(aes(x=date, y=may_temperature, colour=pheno))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_test() +
  labs(x="Date of Sampling", y= "Mean May Temperature")

#We see different slopes across different categories - in some cases we have very few samples so the lines aren't really representative (like disperse)

clean_data_3sp %>% filter(pheno=="Disperse -") %>% nrow()

#Only 16 samples 

#But these are showing multiple species at once

clean_data_3sp %>% ggplot(aes(x=date, y=may_temperature, colour=pheno))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean May Temp")+
  facet_grid(~`Scientific Name`)

clean_data_3sp %>% ggplot(aes(x=date, y=april_temperature, colour=pheno))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean April Temp")+
  facet_grid(~`Scientific Name`)

#lots more visualization and exploration of data to do! 

#Can explore if there are changes over time 

clean_data_3sp %>% ggplot(aes(x=date, y=april_temperature, colour=pheno))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean April Temp")+
  facet_grid(~pheno)

clean_data_3sp %>% filter(Flower=="Y")  %>% ggplot(aes(x=date, y=april_temperature, colour=pheno))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_test()+
  labs(x="Date of Sampling", y= "Mean April Temp")+
  facet_grid(~pheno)


setwd("~/Documents/Stage RI/Phenology")
saveRDS(vaccinium, file='clean_data_3sp.Rdata')
#Next up is to statistically compare these relationships as well 

#------------------------------------------------------------------------------------------------
install.packages('lme4')
install.packages('lmerTest')
install.packages('performance')

setwd("~/Documents/Stage RI/Phenology")
vaccinium<-readRDS(file='clean_data_3sp')
library(tidyverse)

table(vaccinium$`Scientific Name`)
#Make a table with mean, standard deviation, max, min and range of flowering, bud, fruiting, seed dispersing times for each species 

flowering_table <- vaccinium %>%
  filter(Flower =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="flowering")

bud_table <- vaccinium %>%
  filter(Bud =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="Bud")

fruit_table <- vaccinium %>%
  filter(Fruit =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="Fruit")

seed_disperse_table <- vaccinium %>%
  filter(Seed_disperse =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="Seed disperse")

#Vouchers from V. myrtilloides:
vaccinium<- vaccinium %>% mutate(eventDate = paste(ifelse(eventDate == as.character("1997-04-08"), as.character("1997-08-04"), as.character(eventDate)))) %>% mutate(eventDate=as.Date(eventDate))

vaccinium<-vaccinium %>% filter(!(vaccinium$date=='1900-03-19' & vaccinium$Year==1905))

fruit_table <- vaccinium %>%
  filter(Fruit =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="Fruit")

#Merge into one table
phenological_stats <- bind_rows(bud_table, flowering_table, fruit_table, seed_disperse_table)


#Plot the occurrences data on a map, color = species
library(geodata) 
vac_ang<-vaccinium %>% 
  filter(vaccinium$`Scientific Name`=='Vaccinium angustifolium')
vac_cor<-vaccinium %>% 
  filter(vaccinium$`Scientific Name`=='Vaccinium corymbosum')
vac_myr<-vaccinium %>% 
  filter(vaccinium$`Scientific Name`=='Vaccinium myrtilloides')

us_map <- gadm(country = 'USA', level = 1, resolution = 2, 
               path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM/gadm")

ca_map <- gadm(country = 'CA', level = 1, resolution = 2, 
               path = '/Users/elise.cadiou/Documents/Stage RI/R_SDM/gadm') 

canUS_map <- rbind(us_map, ca_map) # Combine US and Canada vector map

plot(canUS_map, xlim = c(-160, -50), ylim = c(40, 100)) 

points(vac_ang$decimalLongitude, vac_ang$decimalLatitude, pch = 16, 
       col = alpha('forestgreen', 0.8))
points(vac_cor$decimalLongitude, vac_cor$decimalLatitude, pch = 16, 
       col = alpha('darkorange1', 0.8))
points(vac_myr$decimalLongitude, vac_myr$decimalLatitude, pch = 16, 
       col = alpha('blue', 0.8))

#Remove 2 vouchers located in the US (with same coordinates)
vaccinium<- vaccinium %>% 
filter(!vaccinium$decimalLatitude==40.23330)

#Run again to adjust the values in the table (only seed disperse data affected)

flowering_table <- vaccinium %>%
  filter(Flower =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="flowering")

bud_table <- vaccinium %>%
  filter(Bud =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="Bud")

fruit_table <- vaccinium %>%
  filter(Fruit =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="Fruit")

seed_disperse_table <- vaccinium %>%
  filter(Seed_disperse =='Y') %>%
  group_by(`Scientific Name` ) %>%
  summarise(mean_date = mean(date), stdev=sd(date), n = n(), min_date=min(date), max_date=max(date), range=max(date)-min(date)) %>%
  mutate(pheno_state="Seed disperse")

phenological_stats <- bind_rows(bud_table, flowering_table, fruit_table, seed_disperse_table)

#Let's map the 3 species occurrences:

library(geodata) 
vac_ang<-vaccinium %>% 
  filter(vaccinium$`Scientific Name`=='Vaccinium angustifolium')
vac_cor<-vaccinium %>% 
  filter(vaccinium$`Scientific Name`=='Vaccinium corymbosum')
vac_myr<-vaccinium %>% 
  filter(vaccinium$`Scientific Name`=='Vaccinium myrtilloides')

us_map <- gadm(country = 'USA', level = 1, resolution = 2, 
               path = "/Users/elise.cadiou/Documents/Stage RI/R_SDM/gadm")

ca_map <- gadm(country = 'CA', level = 1, resolution = 2, 
               path = '/Users/elise.cadiou/Documents/Stage RI/R_SDM/gadm') 

canUS_map <- rbind(us_map, ca_map) # Combine US and Canada vector map

plot(canUS_map, xlim = c(-95, -50), ylim = c(42, 60)) 

points(vac_ang$decimalLongitude, vac_ang$decimalLatitude, pch = 16, 
       col = alpha('forestgreen', 0.8))
points(vac_cor$decimalLongitude, vac_cor$decimalLatitude, pch = 16, 
       col = alpha('darkorange1', 0.8))
points(vac_myr$decimalLongitude, vac_myr$decimalLatitude, pch = 16, 
       col = alpha('blue', 0.8))

legend(x = -62, y = 59, title = "Species", legend = c('Vaccinium angustifolium', 'Vaccinium corymbosum', 'Vaccinium myrtilloides'), fill = c('forestgreen', 'darkorange1','blue'), cex = 0.8, bty = "y") 


#Save the file
setwd("~/Documents/Stage RI/Phenology")
saveRDS(vaccinium, file='vac_stat.Rdata')
vac_stat<-readRDS(file='vac_stat.Rdata')

table(vac_stat$`Scientific Name`)
#Vaccinium angustifolium    Vaccinium corymbosum  Vaccinium myrtilloides 
#203                      44                      68 


#Create a histogram each for V. angustifolium and V. myrtilloides
vac_stat<-vac_stat %>% filter(!vac_stat$`Scientific Name`=='Vaccinium corymbosum')

vac_stat %>% ggplot(aes(Year)) +
  geom_histogram(bins = 116, col = 'black', size = 0.2)+
  labs(y="Number of records", x="Year")+
  theme_light()+
  scale_x_continuous(breaks=seq(1900, 2020, 20))+
  facet_grid(~vac_stat$`Scientific Name`)+
  theme(strip.background = element_rect(fill='black', size=1.5, linetype="solid"))+
  scale_fill_manual(values = c('forestgreen', 'darkorange1'))

#Create a density plot for each phenological stage with V. angustifolium and V. myrtilloides

bud_plot<-vac_stat %>%
  filter(Bud =='Y') %>%
  ggplot(aes(date, fill = `Scientific Name`))+
  geom_density(alpha=0.75)+
  xlab("Months of the year")+ 
  theme_light()+
  labs(fill="Species")+
  scale_fill_hue(labels=c("Vaccinium angustifolium", "Vaccinium myrtilloides"))+
  scale_fill_manual(values=c('forestgreen', 'darkorange1'))+
  theme(legend.position = c(0.8,0.8)) 

fl_plot<-vac_stat %>%
  filter(Flower =='Y') %>%
  ggplot(aes(date, fill = `Scientific Name`))+
  geom_density(alpha=0.75)+
  xlab("Months of the year")+ 
  theme_light()+
  labs(fill="Species")+
  scale_fill_hue(labels=c("Vaccinium angustifolium", "Vaccinium myrtilloides"))+
  scale_fill_manual(values=c('forestgreen', 'darkorange1'))+
  theme(legend.position = c(0.8,0.8))

fr_plot<-vac_stat %>%
  filter(Fruit =='Y') %>%
  ggplot(aes(date, fill = `Scientific Name`))+
  geom_density(alpha=0.75)+
  xlab("Months of the year")+ 
  theme_light()+
  labs(fill="Species")+
  scale_fill_hue(labels=c("Vaccinium angustifolium", "Vaccinium myrtilloides"))+
  scale_fill_manual(values=c('forestgreen', 'darkorange1'))+
  theme(legend.position = c(0.8,0.8))

sd_plot<-vac_stat %>%
  filter(Seed_disperse =='Y') %>%
  ggplot(aes(date, fill = `Scientific Name`))+
  geom_density(alpha=0.75)+
  xlab("Months of the year")+ 
  theme_light()+
  labs(fill="Species")+
  scale_fill_hue(labels=c("Vaccinium angustifolium", "Vaccinium myrtilloides"))+
  scale_fill_manual(values=c('forestgreen', 'darkorange1'))+
  theme(legend.position = c(0.8,0.8))


install.packages('cowplot')
library(cowplot)
plot_grid(bud_plot, fl_plot, fr_plot, sd_plot,
          labels= c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


#----------------------------
#Let's run the models!
install.packages('car')
install.packages("extrafont")
install.packages('ggthemes')
library(car)
library(lme4)
library(lmerTest)
library(performance)
library(tidyr)
library(tidyverse)
library(lubridate)
library(extrafont)
library(broom)
library(ggthemes)
library(viridis)
library(leaps)
library(cowplot)

#Date has to be treated as numeric for the models 

#Filter down to the phenological stage of interest = FLOWER
vaccinium_flower <- vac_stat %>% filter(Flower=='Y') %>% filter(`Scientific Name` !=  "Vaccinium myrtilloides")

#Mixed model (with myrtilloides & angustifolium):boundary error due to lower sample size/lack of variance --> linear model
fl_model <- lm(as.numeric(date)  ~ april_temperature + may_temperature + march_temperature + june_temperature + july_temperature, data = vaccinium_flower)

# Summarize the model
summary(fl_model)

#Cp Mallows to find the best model
choix<-regsubsets(as.numeric(date) ~ april_temperature + may_temperature + march_temperature + june_temperature + july_temperature, data=vaccinium_flower,nbest=1,nvmax=5)
summary(choix)
plot(choix, scale="Cp")

#Let's run the model with  May and June temperatures
fl_model2 <- lm(as.numeric(date)  ~ may_temperature + june_temperature, data = vaccinium_flower)
summary(fl_model2)

test_performance(fl_model, fl_model2) #no significant difference between the two models 

#use anova (type 2) to determine significance of each predictor 

fl_model2_anova <-Anova(fl_model2, type=2)
fl_model2_anova <- tidy(fl_model2_anova)
fl_model2_var <- fl_model2_anova %>% mutate(total_sum=sum(fl_model2_anova$sumsq)) %>% filter(term !="Residuals") %>% dplyr::select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="temp") %>% dplyr::select(phenotype, term, var, p.value)
fl_model2_var %>% filter(p.value <0.05)

r2(fl_model2) # Marginal R2 represents the variance explained by the fixed effects (marginal variance) vs the Conditional which is both fixed and random effects 

performance_rmse(fl_model2, normalized = FALSE) #The RMSE is the square root of the variance of the residuals and indicates the absolute fit of the model to the data (difference between observed data to model's predicted values). It can be interpreted as the standard deviation of the unexplained variance, and is in the same units as the response variable. Lower values indicate better model fit.

vac_fl<-vaccinium_flower %>% ggplot(aes(y=date, x=may_temperature))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_light()+
  labs(y="Flowering DOY", x= "Mean May Temperature")

#Correlation between flowering time and the year: is it earlier now than it was before?
fl_model3 <- lm(as.numeric(date)  ~ Year, data = vaccinium_flower)
summary(fl_model3)

vac_fl_year<-vaccinium_flower %>% ggplot(aes(y=date, x=Year))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_light()+
  labs(y="Flowering DOY", x= "Years")

#--------------------------------------------------------
#Filter down to the phenological stage of interest = BUD

vaccinium_bud <- vac_stat %>% filter(Bud=='Y') %>% filter(`Scientific Name` !=  "Vaccinium myrtilloides")

#Use a linear model instead of a linear mixed model because we can't account for species 
bud_model <- lm(as.numeric(date)  ~ march_temperature + april_temperature + may_temperature + june_temperature + july_temperature,
              data = vaccinium_bud)

# Summarize the model
summary(bud_model)

#Find the best model
choix<-regsubsets(as.numeric(date) ~ april_temperature + may_temperature + march_temperature + june_temperature + july_temperature, data=vaccinium_bud,nbest=1,nvmax=5)
summary(choix)
plot(choix, scale="Cp")


#Let's run the model with March, April and May temperature
bud_model2 <- lm(as.numeric(date)  ~ march_temperature + april_temperature + may_temperature, data = vaccinium_bud)
summary(bud_model2)

test_performance(bud_model, bud_model2) #no significant difference between the two models 

#use anova (type 2) to determine significance of each predictor 

bud_model2_anova <-Anova(bud_model2, type=2)
bud_model2_anova <- tidy(bud_model2_anova)
bud_model2_var <- bud_model2_anova %>% mutate(total_sum=sum(bud_model2_anova$sumsq)) %>% filter(term !="Residuals") %>% dplyr::select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="temp") %>% dplyr::select(phenotype, term, var, p.value)
bud_model2_var %>% filter(p.value <0.05)

r2(bud_model2) # Marginal R2 represents the variance explained by the fixed effects (marginal variance) vs the Conditional which is both fixed and random effects 

performance_rmse(bud_model2, normalized = FALSE)

vac_bud<-vaccinium_bud %>% ggplot(aes(y=date, x=may_temperature))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_light()+
  labs(y="Budding DOY", x= "Mean May Temp")


#Correlation between budding time and the year: is it earlier now than it was before?
bud_model3<- lm(as.numeric(date) ~Year, data=vaccinium_bud)
summary(bud_model3)

vac_bud_year<-vaccinium_bud %>% ggplot(aes(y=date, x=Year))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_light()+
  labs(y="Budding DOY", x= "Years")

#--------------------------------------------------------
#Filter down to the phenological stage of interest = FRUIT
vaccinium_fruit <- vac_stat %>% filter(Fruit=='Y') %>% filter(`Scientific Name` !=  "Vaccinium myrtilloides")

fr_model <- lm(as.numeric(date) ~ march_temperature + april_temperature + may_temperature + june_temperature + july_temperature + august_temperature + sept_temperature, data = vaccinium_fruit)

# Summarize the model
summary(fr_model)

#Find the best model
choix<-regsubsets(as.numeric(date) ~ march_temperature + april_temperature + may_temperature + june_temperature + july_temperature + august_temperature + sept_temperature, data=vaccinium_fruit,nbest=1,nvmax=7)
summary(choix)
plot(choix, scale="Cp")

#Let's run the model with May and July temperature?
fr_model2 <- lm(as.numeric(date) ~ may_temperature + july_temperature, data = vaccinium_fruit)
summary(fr_model2)

test_performance(fr_model, fr_model2) #no significant difference between the two models 

#use anova (type 2) to determine significance of each predictor 

fr_model2_anova <-Anova(fr_model2, type=2)
fr_model2_anova <- tidy(fr_model2_anova)
fr_model2_var <- fr_model2_anova %>% mutate(total_sum=sum(fr_model2_anova$sumsq)) %>% filter(term !="Residuals") %>% dplyr::select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="temp") %>% dplyr::select(phenotype, term, var, p.value)
fr_model2_var %>% filter(p.value <0.05)

r2(fr_model2)

performance_rmse(fr_model2, normalized = FALSE)

vac_fr<-vaccinium_fruit %>% ggplot(aes(y=date, x=may_temperature))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_light()+
  labs(y="Fruiting DOY", x= "Mean May Temp")

#Correlation between fruiting time and the year: is it earlier now than it was before?
fr_model3<- lm(as.numeric(date) ~Year, data=vaccinium_fruit)
summary(fr_model3)

vac_fr_year<-vaccinium_fruit %>% ggplot(aes(y=date, x=Year))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_light()+
  labs(y="Fruiting DOY", x= "Years")

#--------------------------------------------------------
#Filter down to the phenological stage of interest = SEED DISPERSE
vaccinium_sd <- vac_stat %>% filter(Seed_disperse=='Y') %>% filter(`Scientific Name` !=  "Vaccinium myrtilloides")

sd_model <- lm(as.numeric(date) ~ march_temperature + april_temperature + may_temperature + june_temperature + july_temperature + august_temperature + sept_temperature + oct_temperature, data = vaccinium_sd)

# Summarize the model
summary(sd_model) #only june is significant

#Find the best model - actually here it diesn't work but I don't know why. So I stick with only June temp for the optimized model
choix<-regsubsets(as.numeric(date) ~ march_temperature + april_temperature + may_temperature + june_temperature + july_temperature + august_temperature + sept_temperature + oct_temperature, data=vaccinium_fruit,nbest=1,nvmax=8)
summary(choix)
plot(choix, scale="Cp")

#Let's run the model with only june temperature
sd_model2 <- lm(as.numeric(date) ~ june_temperature, data = vaccinium_sd)
summary(sd_model2)

test_performance(sd_model, sd_model2) #no significant difference between the two models 

#use anova (type 2) to determine significance of each predictor 

sd_model2_anova <-Anova(sd_model2, type=2)
sd_model2_anova <- tidy(sd_model2_anova)
sd_model2_var <- sd_model2_anova %>% mutate(total_sum=sum(sd_model2_anova$sumsq)) %>% filter(term !="Residuals") %>% dplyr::select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="temp") %>% dplyr::select(phenotype, term, var, p.value)
sd_model2_var %>% filter(p.value <0.05)

r2(fr_model2)

performance_rmse(sd_model2, normalized = FALSE)

vac_sd<-vaccinium_sd %>% ggplot(aes(y=date, x=june_temperature))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_light()+
  labs(y="Seed dispersing DOY", x= "Mean June Temp")

plot_grid(vac_bud, vac_fl, vac_fr, vac_sd,
          labels= c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

#Correlation between seed dispersing time and the year: is it earlier now than it was before?
sd_model3<- lm(as.numeric(date) ~Year, data=vaccinium_sd)
summary(sd_model3)

vac_sd_year<-vaccinium_sd %>% ggplot(aes(y=date, x=Year))+
  geom_point()+
  geom_smooth(method='lm',  alpha=0.05, se=F)+
  theme_light()+
  labs(y="Seed dispersing DOY", x= "Years")

plot_grid(vac_bud_year, vac_fl_year, vac_fr_year, vac_sd_year,
          labels= c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
