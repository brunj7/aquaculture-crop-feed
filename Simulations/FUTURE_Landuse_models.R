# load required libraries
library(ggplot2)
library(maps)
library(sp)
library(maptools)
library(scales)
library(mapproj)
library(RColorBrewer)
library(rworldmap)
library(RColorBrewer)
library(matrixStats) #for rowSds
library(data.table)
library(dplyr)


rm(list=ls())
#Total production (FAO)
prod<-read.csv("All_ProductionScenarios_ONFEED.csv")
head(prod)
length(prod$Country)
length(unique(prod$Country))

#farmed animals
amls = read.csv("animals.csv")

#Calculate Feed adjust (i.e., harmonized) by Farm Factor
animal_comp <- read.csv("MARINE&FW&LIVESTOCK_CropProps_2050.csv") #based on linear % increase to 2050 from FAO data
head(animal_comp)

#Adjust (harmonize) feed at country level
ff<-read.csv("Country_Harmonize_Factors.csv") 
head(ff)

#In-country (domestic) % production of crop
inhouse_perct<-read.csv("FAO_2013_Percent_InCountryCrop_Prod.csv") 
head(inhouse_perct)

#% exporter of crop
export_perct<-read.csv("Country_Export_Percent.csv") 
head(export_perct)

#Future (2050) yeild (ha)
yield<-read.csv("FAO_2013_yield_ha_2050.csv") 
head(yield)
length(unique(yield$Country))
length(yield$Country)

##CALCULATIONS---AMOUNT OF CROP FOR ANIMAL PRODUCTION----------
##------------------------------------------------------------
#BAU 2050 Production--------------------------------
##------------------------------------------------------------
  #Create NA bank for each feed type
  r.length = length(prod$Country)
  wheat<-matrix(NA,nrow=r.length,ncol=500)
  corn<-matrix(NA,nrow=r.length,ncol=500)
  soy<-matrix(NA,nrow=r.length,ncol=500)
  rapeseed<-matrix(NA,nrow=r.length,ncol=500)
  pulses<-matrix(NA,nrow=r.length,ncol=500)
  barley<-matrix(NA,nrow=r.length,ncol=500)
  cv<-matrix(NA,nrow=r.length,ncol=500)

  # Create NA bank for each feed type landuse (ha)
  r.length2 = 230 #number of regions calculated from CURRENT calcs
  w.ha<-matrix(NA,nrow=r.length2,ncol=500)
  c.ha<-matrix(NA,nrow=r.length2,ncol=500)
  s.ha<-matrix(NA,nrow=r.length2,ncol=500)
  rs.ha<-matrix(NA,nrow=r.length2,ncol=500)
  p.ha<-matrix(NA,nrow=r.length2,ncol=500)
  b.ha<-matrix(NA,nrow=r.length2,ncol=500)
  cv.ha<-matrix(NA,nrow=r.length2,ncol=500)
  total.ha<-matrix(NA,nrow=r.length2,ncol=500)

set.seed(29)
for(i in 1:500){
  fcr_beef = round(runif(min=6,max=31.2, 1),2)
  fcr_chick = round(runif(min=1.8,max= 3.5, 1), 2)
  fcr_dairy = round(runif(min=6,max=31.2, 1),2) 
  fcr_goat = round(runif(min = 4, max = 12.9, 1),2)
  fcr_hen = round(runif(min=1.8,max= 3.5, 1), 2)
  fcr_pig = round(runif(min = 3, max = 5.9, 1), 2)
  fcr_sheep = round(runif(min = 4, max = 12.9, 1),2)
  fcr_fish = round(runif(min = 1.1, max=1.6, 1),2)
  fcr_crust = round(runif(min = 1.1, max = 1.7,1), 2)
  fcr_fwfish = round(runif(min = 1.1, max = 1.6, 1), 2)
  fcr_fwcrust = round(runif(min=1.1, max = 1.8, 1), 2)
  moll_fcr = 0
  
  fcrs = c(fcr_dairy,fcr_hen,fcr_beef,fcr_goat,fcr_sheep,
          fcr_chick,fcr_pig,fcr_fish,fcr_crust,fcr_fwfish,fcr_fwcrust,moll_fcr)
  
  amls$fcrs<-fcrs
  all_fcrs = amls
  
  #join values
  new_prod = left_join(prod,all_fcrs)
  length(prod$Country) 
  new_prod = left_join(new_prod,animal_comp)
  head(new_prod)
  length(new_prod$Country) 

  #Calculate total feed AND adjust (harmonize) feed at country level
  wheat[,i] = (new_prod$meanBAU2050_onfeed*new_prod$fcrs*new_prod$Wheat)*ff$Farm_factor_wheat
  corn[,i] = (new_prod$meanBAU2050_onfeed*new_prod$fcrs*new_prod$Maize)*ff$Farm_factor_corn
  soy[,i] = (new_prod$meanBAU2050_onfeed*new_prod$fcrs*new_prod$Soy)*ff$Farm_factor_soy
  rapeseed[,i] = (new_prod$meanBAU2050_onfeed*new_prod$fcrs*new_prod$Rapeseed)*ff$Farm_factor_rapeseed
  pulses[,i] = (new_prod$meanBAU2050_onfeed*new_prod$fcrs*new_prod$Pulses)*ff$Farm_factor_pulses
  barley[,i] = (new_prod$meanBAU2050_onfeed*new_prod$fcrs*new_prod$Barley)*ff$Farm_factor_barley
  cv[,i] = (new_prod$meanBAU2050_onfeed*new_prod$fcrs*new_prod$Cassava)*ff$Farm_factor_cassava
  
  df_harmz = as.data.frame(cbind(wheat[,i],corn[,i],soy[,i],rapeseed[,i], pulses[,i],barley[,i],cv[,i]))
  names(df_harmz) = c("wheat","corn","soy","rapeseed","pulses","barley","cv")
  df_harmz$Country = new_prod$Country
  all_join = left_join(df_harmz,inhouse_perct)
  all_join[is.na(all_join)] <- 0

  #Calculate how much of feed COULD BE produced in-house
  wheat_inhouse = all_join$wheat*all_join$Wheat..Prod
  corn_inhouse = all_join$corn*all_join$Maize..Prod
  soy_inhouse = all_join$soy*all_join$Soybeans..Prod
  rapeseed_inhouse = all_join$rapeseed*all_join$Rapeseed..Prod
  pulses_inhouse = all_join$pulses*all_join$Pulses..Prod
  barley_inhouse = all_join$barley*all_join$Barley..Prod
  cassava_inhouse = all_join$cv*all_join$Cassava..Prod
  
  total_inhouse = as.data.frame(cbind(wheat_inhouse,corn_inhouse,soy_inhouse,rapeseed_inhouse,
                        pulses_inhouse,barley_inhouse,cassava_inhouse))
  total_inhouse$Country = all_join$Country
  total_inhouse[is.na(total_inhouse)] <- 0
  
  #Sum across all for total country in-house values (across all animals)
  summed_wheat = as.data.frame(tapply(total_inhouse[,1], total_inhouse$Country, sum))
  summed_corn = as.data.frame(tapply(total_inhouse[,2], total_inhouse$Country, sum))
  summed_soy = as.data.frame(tapply(total_inhouse[,3], total_inhouse$Country, sum))
  summed_rapeseed = as.data.frame(tapply(total_inhouse[,4], total_inhouse$Country, sum))
  summed_pulses = as.data.frame(tapply(total_inhouse[,5], total_inhouse$Country, sum))
  summed_barley = as.data.frame(tapply(total_inhouse[,6], total_inhouse$Country, sum))
  summed_cassava = as.data.frame(tapply(total_inhouse[,7], total_inhouse$Country, sum))
  
  #Calculate remaining crop needed from exporting countries
  wheat_need = sum(all_join$wheat-total_inhouse$wheat_inhouse)
  corn_need = sum(all_join$corn-total_inhouse$corn_inhouse)
  soy_need = sum(all_join$soy-total_inhouse$soy_inhouse)
  rapeseed_need = sum(all_join$rapeseed-total_inhouse$rapeseed_inhouse)
  pulses_need = sum(all_join$pulses-total_inhouse$pulses_inhouse)
  barley_need = sum(all_join$barley-total_inhouse$barley_inhouse)
  cassava_need = sum(all_join$cv-total_inhouse$cassava_inhouse)
  
  total_need = as.data.frame(cbind(wheat_need,corn_need,soy_need,rapeseed_need,pulses_need,barley_need,cassava_need))
  
  #Calculate which countries and propotionally how much of each crop will be exported
  total_export = as.data.frame(export_perct$Country)
  total_export$wheat_export = export_perct$WheatExport*total_need$wheat_need
  total_export$corn_export = export_perct$MaizeExport*total_need$corn_need
  total_export$soy_export = export_perct$SoybeansExport*total_need$soy_need
  total_export$rapeseed_export = export_perct$RapeseedExport*total_need$rapeseed_need
  total_export$pulses_export = export_perct$PulsesExport*total_need$pulses_need
  total_export$barley_export = export_perct$BarleyExport*total_need$barley_need
  total_export$cassava_export = export_perct$CassavaExport*total_need$cassava_need
  
  names(total_export) = c("Country","wheat_exp","corn_exp","soy_exp","rapeseed_exp","pulses_exp",
                          "barley_exp","cassava_exp")
  
  #Merge in-house and export df
  summed_inhouse <- cbind(summed_wheat,summed_corn,summed_soy,summed_rapeseed,summed_pulses,summed_barley,summed_cassava)
  head(summed_inhouse)
  summed_inhouse2<-setDT(summed_inhouse, keep.rownames = TRUE)[]
  head(summed_inhouse2)
  names(summed_inhouse2) <- c("Country","wheat","corn","soy","rapeseed","pulses","barley","cassava")
  summed_inhouse2 = as.data.frame(summed_inhouse2)
  head(summed_inhouse2)
  length(unique(summed_inhouse2$Country)) 
  
  merged_data<-merge(summed_inhouse2,total_export, by="Country", all=TRUE) #all maintains all Countries even if not an export country
  megered_data<- as.data.frame(merged_data)
  merged_data[is.na(merged_data)]<-0 
  head(merged_data)
  length(unique(merged_data$Country)) #(some countries produce crop but NOT animal - those added)
  
  #Sum in-house and exports for TOTAL crops needed to be produced at the country level   
  total_country_crop <-as.data.frame(merged_data[,1])
  names(total_country_crop) <- "Country"
  head(total_country_crop)
  total_country_crop$wheat_total = merged_data$wheat+merged_data$wheat_exp
  total_country_crop$corn_total = merged_data$corn+merged_data$corn_exp
  total_country_crop$soy_total = merged_data$soy+merged_data$soy_exp
  total_country_crop$rapeseed_total = merged_data$rapeseed+merged_data$rapeseed_exp
  total_country_crop$pulses_total = merged_data$pulses+merged_data$pulses_exp
  total_country_crop$barley_total = merged_data$barley+merged_data$barley_exp
  total_country_crop$cassava_total = merged_data$cassava+merged_data$cassava_exp
  
  total_country = as.data.frame(total_country_crop)
  head(total_country)
  length(total_country$Country) #230
  
  ###--CALCULATE HECTARES FROM TOTAL COUNTRY CROP (MT) ---
  all_data = left_join(total_country,yield)
  length(all_data$Country) #230
  
  w.ha[,i] = all_data$wheat_total*all_data$Wheat_crop_ha
  c.ha[,i] = all_data$corn_total*all_data$Maize_crop_ha
  s.ha[,i] = all_data$soy_total*all_data$Soybeans_crop_ha
  rs.ha[,i] = all_data$rapeseed_total*all_data$Rapeseed_crop_ha
  p.ha[,i] = all_data$pulses_total*all_data$Pulses_crop_ha
  b.ha[,i] = all_data$barley_total*all_data$Barley_crop_ha
  cv.ha[,i] = all_data$cassava_total*all_data$Cassava_crop.ha
  
  all_ha <- cbind(w.ha[,i],c.ha[,i],s.ha[,i],rs.ha[,i],p.ha[,i],b.ha[,i],cv.ha[,i])
  total.ha[,i] = rowSums(all_ha)
}

#Hectares
mean.ha = rowMeans(total.ha)
sd.ha = rowSds(total.ha)
final_ha <- cbind(mean.ha,sd.ha)
sum(final_ha[,1])
write.csv(final_ha,file = "MEANS&SDs_2050_BAU_ha.csv")

#Feed
total_feed <- as.data.frame(cbind(rowMeans(wheat),rowMeans(corn),rowMeans(soy),rowMeans(rapeseed),rowMeans(pulses),rowMeans(barley),rowMeans(cv)))
names(total_feed) <- c("wheat","corn","soy","rapeseed","pulses","barley","cassava")
head(total_feed)
sum(total_feed, na.rm=TRUE)
write.csv(total_feed,file = "MEANS_2050_BAU_feed.csv")

total_feed_sds <- as.data.frame(cbind(rowSds(wheat),rowSds(corn),rowSds(soy),rowSds(rapeseed),rowSds(pulses),rowSds(barley),rowSds(cv)))
names(total_feed_sds) <- c("wheat","corn","soy","rapeseed","pulses","barley","cassava")
head(total_feed_sds)
write.csv(total_feed_sds,file = "SDs_2050_BAU_feed.csv")


#*************************************************************
##------------------------------------------------------------
#--------- MIXED 2050 Production--------------------------------
##------------------------------------------------------------
#Create NA bank for each feed type
r.length = length(prod$Country)
wheat<-matrix(NA,nrow=r.length,ncol=500)
corn<-matrix(NA,nrow=r.length,ncol=500)
soy<-matrix(NA,nrow=r.length,ncol=500)
rapeseed<-matrix(NA,nrow=r.length,ncol=500)
pulses<-matrix(NA,nrow=r.length,ncol=500)
barley<-matrix(NA,nrow=r.length,ncol=500)
cv<-matrix(NA,nrow=r.length,ncol=500)

# Create NA bank for each feed type landuse (ha)
r.length2 = 230
w.ha<-matrix(NA,nrow=r.length2,ncol=500)
c.ha<-matrix(NA,nrow=r.length2,ncol=500)
s.ha<-matrix(NA,nrow=r.length2,ncol=500)
rs.ha<-matrix(NA,nrow=r.length2,ncol=500)
p.ha<-matrix(NA,nrow=r.length2,ncol=500)
b.ha<-matrix(NA,nrow=r.length2,ncol=500)
cv.ha<-matrix(NA,nrow=r.length2,ncol=500)
total.ha<-matrix(NA,nrow=r.length2,ncol=500)

set.seed(29)
for(i in 1:500){
  fcr_beef = round(runif(min=6,max=31.2, 1),2)
  fcr_chick = round(runif(min=1.8,max= 3.5, 1), 2)
  fcr_dairy = round(runif(min=6,max=31.2, 1),2) 
  fcr_goat = round(runif(min = 4, max = 12.9, 1),2)
  fcr_hen = round(runif(min=1.8,max= 3.5, 1), 2)
  fcr_pig = round(runif(min = 3, max = 5.9, 1), 2)
  fcr_sheep = round(runif(min = 4, max = 12.9, 1),2)
  fcr_fish = round(runif(min = 1.1, max=1.6, 1),2)
  fcr_crust = round(runif(min = 1.1, max = 1.7,1), 2)
  fcr_fwfish = round(runif(min = 1.1, max = 1.6, 1), 2)
  fcr_fwcrust = round(runif(min=1.1, max = 1.8, 1), 2)
  moll_fcr = 0
  
  fcrs = c(fcr_dairy,fcr_hen,fcr_beef,fcr_goat,fcr_sheep,
           fcr_chick,fcr_pig,fcr_fish,fcr_crust,fcr_fwfish,fcr_fwcrust,moll_fcr)
  
  amls$fcrs<-fcrs
  all_fcrs = amls
  
  #join values
  new_prod = left_join(prod,all_fcrs)
  new_prod = left_join(new_prod,animal_comp)
  head(new_prod)
  
  #Calculate total feed AND adjust (harmonize) feed at country level
  wheat[,i] = new_prod$meanMixed2050_onfeed*new_prod$fcrs*new_prod$Wheat*ff$Farm_factor_wheat
  corn[,i] = new_prod$meanMixed2050_onfeed*new_prod$fcrs*new_prod$Maize*ff$Farm_factor_corn
  soy[,i] = new_prod$meanMixed2050_onfeed*new_prod$fcrs*new_prod$Soy*ff$Farm_factor_soy
  rapeseed[,i] = new_prod$meanMixed2050_onfeed*new_prod$fcrs*new_prod$Rapeseed*ff$Farm_factor_rapeseed
  pulses[,i] = new_prod$meanMixed2050_onfeed*new_prod$fcrs*new_prod$Pulses*ff$Farm_factor_pulses
  barley[,i] = new_prod$meanMixed2050_onfeed*new_prod$fcrs*new_prod$Barley*ff$Farm_factor_barley
  cv[,i] = new_prod$meanMixed2050_onfeed*new_prod$fcrs*new_prod$Cassava*ff$Farm_factor_cassava
  
  df_harmz = as.data.frame(cbind(wheat[,i],corn[,i],soy[,i],rapeseed[,i], pulses[,i],barley[,i],cv[,i]))
  names(df_harmz) = c("wheat","corn","soy","rapeseed","pulses","barley","cv")
  df_harmz$Country = new_prod$Country
  all_join = left_join(df_harmz,inhouse_perct)
  all_join[is.na(all_join)] <- 0
  
  #Calculate how much of feed COULD BE produced in-house
  wheat_inhouse = all_join$wheat*all_join$Wheat..Prod
  corn_inhouse = all_join$corn*all_join$Maize..Prod
  soy_inhouse = all_join$soy*all_join$Soybeans..Prod
  rapeseed_inhouse = all_join$rapeseed*all_join$Rapeseed..Prod
  pulses_inhouse = all_join$pulses*all_join$Pulses..Prod
  barley_inhouse = all_join$barley*all_join$Barley..Prod
  cassava_inhouse = all_join$cv*all_join$Cassava..Prod
  
  total_inhouse = as.data.frame(cbind(wheat_inhouse,corn_inhouse,soy_inhouse,rapeseed_inhouse,
                                      pulses_inhouse,barley_inhouse,cassava_inhouse))
  total_inhouse$Country = all_join$Country
  total_inhouse[is.na(total_inhouse)] <- 0
  
  #Sum across all for total country in-house values (across all animals)
  summed_wheat = as.data.frame(tapply(total_inhouse[,1], total_inhouse$Country, sum))
  summed_corn = as.data.frame(tapply(total_inhouse[,2], total_inhouse$Country, sum))
  summed_soy = as.data.frame(tapply(total_inhouse[,3], total_inhouse$Country, sum))
  summed_rapeseed = as.data.frame(tapply(total_inhouse[,4], total_inhouse$Country, sum))
  summed_pulses = as.data.frame(tapply(total_inhouse[,5], total_inhouse$Country, sum))
  summed_barley = as.data.frame(tapply(total_inhouse[,6], total_inhouse$Country, sum))
  summed_cassava = as.data.frame(tapply(total_inhouse[,7], total_inhouse$Country, sum))
  
  #Calculate remaining crop needed from exporting countries
  wheat_need = sum(all_join$wheat-total_inhouse$wheat_inhouse)
  corn_need = sum(all_join$corn-total_inhouse$corn_inhouse)
  soy_need = sum(all_join$soy-total_inhouse$soy_inhouse)
  rapeseed_need = sum(all_join$rapeseed-total_inhouse$rapeseed_inhouse)
  pulses_need = sum(all_join$pulses-total_inhouse$pulses_inhouse)
  barley_need = sum(all_join$barley-total_inhouse$barley_inhouse)
  cassava_need = sum(all_join$cv-total_inhouse$cassava_inhouse)
  
  total_need = as.data.frame(cbind(wheat_need,corn_need,soy_need,rapeseed_need,pulses_need,barley_need,cassava_need))
  
  #Calculate which countries and propotionally how much of each crop will be exported
  total_export = as.data.frame(export_perct$Country)
  total_export$wheat_export = export_perct$WheatExport*total_need$wheat_need
  total_export$corn_export = export_perct$MaizeExport*total_need$corn_need
  total_export$soy_export = export_perct$SoybeansExport*total_need$soy_need
  total_export$rapeseed_export = export_perct$RapeseedExport*total_need$rapeseed_need
  total_export$pulses_export = export_perct$PulsesExport*total_need$pulses_need
  total_export$barley_export = export_perct$BarleyExport*total_need$barley_need
  total_export$cassava_export = export_perct$CassavaExport*total_need$cassava_need
  
  names(total_export) = c("Country","wheat_exp","corn_exp","soy_exp","rapeseed_exp","pulses_exp",
                          "barley_exp","cassava_exp")
  
  #Merge in-house and export df
  summed_inhouse <- cbind(summed_wheat,summed_corn,summed_soy,summed_rapeseed,summed_pulses,summed_barley,summed_cassava)
  head(summed_inhouse)
  summed_inhouse2<-setDT(summed_inhouse, keep.rownames = TRUE)[]
  head(summed_inhouse2)
  names(summed_inhouse2) <- c("Country","wheat","corn","soy","rapeseed","pulses","barley","cassava")
  summed_inhouse2 = as.data.frame(summed_inhouse2)
  head(summed_inhouse2)
  length(unique(summed_inhouse2$Country))
  
  merged_data<-merge(summed_inhouse2,total_export, by="Country", all=TRUE) #all maintains all Countries even if not an export country
  megered_data<- as.data.frame(merged_data)
  merged_data[is.na(merged_data)]<-0 
  head(merged_data)
  length(unique(merged_data$Country)) #(some countries produce crop but NOT animal - those added)
  
  #Sum in-house and exports for TOTAL crops needed to be produced at the country level   
  total_country_crop <-as.data.frame(merged_data[,1])
  names(total_country_crop) <- "Country"
  head(total_country_crop)
  total_country_crop$wheat_total = merged_data$wheat+merged_data$wheat_exp
  total_country_crop$corn_total = merged_data$corn+merged_data$corn_exp
  total_country_crop$soy_total = merged_data$soy+merged_data$soy_exp
  total_country_crop$rapeseed_total = merged_data$rapeseed+merged_data$rapeseed_exp
  total_country_crop$pulses_total = merged_data$pulses+merged_data$pulses_exp
  total_country_crop$barley_total = merged_data$barley+merged_data$barley_exp
  total_country_crop$cassava_total = merged_data$cassava+merged_data$cassava_exp
  
  total_country = as.data.frame(total_country_crop)
  head(total_country)
  length(total_country$Country) 
  
  ###--CALCULATE HECTARES FROM TOTAL COUNTRY CROP (MT) ---
  all_data = left_join(total_country,yield)
  
  w.ha[,i] = all_data$wheat_total*all_data$Wheat_crop_ha
  c.ha[,i] = all_data$corn_total*all_data$Maize_crop_ha
  s.ha[,i] = all_data$soy_total*all_data$Soybeans_crop_ha
  rs.ha[,i] = all_data$rapeseed_total*all_data$Rapeseed_crop_ha
  p.ha[,i] = all_data$pulses_total*all_data$Pulses_crop_ha
  b.ha[,i] = all_data$barley_total*all_data$Barley_crop_ha
  cv.ha[,i] = all_data$cassava_total*all_data$Cassava_crop.ha
  
  all_ha <- cbind(w.ha[,i],c.ha[,i],s.ha[,i],rs.ha[,i],p.ha[,i],b.ha[,i],cv.ha[,i])
  total.ha[,i] = rowSums(all_ha)
}

#Hectares
mean.ha = rowMeans(total.ha)
sd.ha = rowSds(total.ha)
final_ha <- cbind(mean.ha,sd.ha)
write.csv(final_ha,file = "MEANS&SDs_2050_MIXED_ha.csv")

#Feed
total_feed <- as.data.frame(cbind(rowMeans(wheat),rowMeans(corn),rowMeans(soy),rowMeans(rapeseed),rowMeans(pulses),rowMeans(barley),rowMeans(cv)))
names(total_feed) <- c("wheat","corn","soy","rapeseed","pulses","barley","cassava")
head(total_feed)
write.csv(total_feed,file = "MEANS_2050_MIXED_feed.csv")

total_feed_sds <- as.data.frame(cbind(rowSds(wheat),rowSds(corn),rowSds(soy),rowSds(rapeseed),rowSds(pulses),rowSds(barley),rowSds(cv)))
names(total_feed_sds) <- c("wheat","corn","soy","rapeseed","pulses","barley","cassava")
head(total_feed_sds)
write.csv(total_feed_sds,file = "SDs_2050_MIXED_feed.csv")



#*************************************************************
##------------------------------------------------------------
#--------- MARINE 2050 Production--------------------------------
##------------------------------------------------------------
#Create NA bank for each feed type
r.length = length(prod$Country)
wheat<-matrix(NA,nrow=r.length,ncol=500)
corn<-matrix(NA,nrow=r.length,ncol=500)
soy<-matrix(NA,nrow=r.length,ncol=500)
rapeseed<-matrix(NA,nrow=r.length,ncol=500)
pulses<-matrix(NA,nrow=r.length,ncol=500)
barley<-matrix(NA,nrow=r.length,ncol=500)
cv<-matrix(NA,nrow=r.length,ncol=500)

# Create NA bank for each feed type landuse (ha)
r.length2 = 230
w.ha<-matrix(NA,nrow=r.length2,ncol=500)
c.ha<-matrix(NA,nrow=r.length2,ncol=500)
s.ha<-matrix(NA,nrow=r.length2,ncol=500)
rs.ha<-matrix(NA,nrow=r.length2,ncol=500)
p.ha<-matrix(NA,nrow=r.length2,ncol=500)
b.ha<-matrix(NA,nrow=r.length2,ncol=500)
cv.ha<-matrix(NA,nrow=r.length2,ncol=500)
total.ha<-matrix(NA,nrow=r.length2,ncol=500)

set.seed(29)
for(i in 1:500){
  fcr_beef = round(runif(min=6,max=31.2, 1),2)
  fcr_chick = round(runif(min=1.8,max= 3.5, 1), 2)
  fcr_dairy = round(runif(min=6,max=31.2, 1),2) 
  fcr_goat = round(runif(min = 4, max = 12.9, 1),2)
  fcr_hen = round(runif(min=1.8,max= 3.5, 1), 2)
  fcr_pig = round(runif(min = 3, max = 5.9, 1), 2)
  fcr_sheep = round(runif(min = 4, max = 12.9, 1),2)
  fcr_fish = round(runif(min = 1.1, max=1.6, 1),2)
  fcr_crust = round(runif(min = 1.1, max = 1.7,1), 2)
  fcr_fwfish = round(runif(min = 1.1, max = 1.6, 1), 2)
  fcr_fwcrust = round(runif(min=1.1, max = 1.8, 1), 2)
  moll_fcr = 0
  
  fcrs = c(fcr_dairy,fcr_hen,fcr_beef,fcr_goat,fcr_sheep,
           fcr_chick,fcr_pig,fcr_fish,fcr_crust,fcr_fwfish,fcr_fwcrust,moll_fcr)
  
  amls$fcrs<-fcrs
  all_fcrs = amls
  
  #join values
  new_prod = left_join(prod,all_fcrs)
  new_prod = left_join(new_prod,animal_comp)
  head(new_prod)
  
  #Calculate total feed AND adjust (harmonize) feed at country level
  wheat[,i] = new_prod$meanMarine2050_onfeed*new_prod$fcrs*new_prod$Wheat*ff$Farm_factor_wheat
  corn[,i] = new_prod$meanMarine2050_onfeed*new_prod$fcrs*new_prod$Maize*ff$Farm_factor_corn
  soy[,i] = new_prod$meanMarine2050_onfeed*new_prod$fcrs*new_prod$Soy*ff$Farm_factor_soy
  rapeseed[,i] = new_prod$meanMarine2050_onfeed*new_prod$fcrs*new_prod$Rapeseed*ff$Farm_factor_rapeseed
  pulses[,i] = new_prod$meanMarine2050_onfeed*new_prod$fcrs*new_prod$Pulses*ff$Farm_factor_pulses
  barley[,i] = new_prod$meanMarine2050_onfeed*new_prod$fcrs*new_prod$Barley*ff$Farm_factor_barley
  cv[,i] = new_prod$meanMarine2050_onfeed*new_prod$fcrs*new_prod$Cassava*ff$Farm_factor_cassava
  
  df_harmz = as.data.frame(cbind(wheat[,i],corn[,i],soy[,i],rapeseed[,i], pulses[,i],barley[,i],cv[,i]))
  names(df_harmz) = c("wheat","corn","soy","rapeseed","pulses","barley","cv")
  df_harmz$Country = new_prod$Country
  all_join = left_join(df_harmz,inhouse_perct)
  all_join[is.na(all_join)] <- 0
  
  #Calculate how much of feed COULD BE produced in-house
  wheat_inhouse = all_join$wheat*all_join$Wheat..Prod
  corn_inhouse = all_join$corn*all_join$Maize..Prod
  soy_inhouse = all_join$soy*all_join$Soybeans..Prod
  rapeseed_inhouse = all_join$rapeseed*all_join$Rapeseed..Prod
  pulses_inhouse = all_join$pulses*all_join$Pulses..Prod
  barley_inhouse = all_join$barley*all_join$Barley..Prod
  cassava_inhouse = all_join$cv*all_join$Cassava..Prod
  
  total_inhouse = as.data.frame(cbind(wheat_inhouse,corn_inhouse,soy_inhouse,rapeseed_inhouse,
                                      pulses_inhouse,barley_inhouse,cassava_inhouse))
  total_inhouse$Country = all_join$Country
  total_inhouse[is.na(total_inhouse)] <- 0
  
  #Sum across all for total country in-house values (across all animals)
  summed_wheat = as.data.frame(tapply(total_inhouse[,1], total_inhouse$Country, sum))
  summed_corn = as.data.frame(tapply(total_inhouse[,2], total_inhouse$Country, sum))
  summed_soy = as.data.frame(tapply(total_inhouse[,3], total_inhouse$Country, sum))
  summed_rapeseed = as.data.frame(tapply(total_inhouse[,4], total_inhouse$Country, sum))
  summed_pulses = as.data.frame(tapply(total_inhouse[,5], total_inhouse$Country, sum))
  summed_barley = as.data.frame(tapply(total_inhouse[,6], total_inhouse$Country, sum))
  summed_cassava = as.data.frame(tapply(total_inhouse[,7], total_inhouse$Country, sum))
  
  #Calculate remaining crop needed from exporting countries
  wheat_need = sum(all_join$wheat-total_inhouse$wheat_inhouse)
  corn_need = sum(all_join$corn-total_inhouse$corn_inhouse)
  soy_need = sum(all_join$soy-total_inhouse$soy_inhouse)
  rapeseed_need = sum(all_join$rapeseed-total_inhouse$rapeseed_inhouse)
  pulses_need = sum(all_join$pulses-total_inhouse$pulses_inhouse)
  barley_need = sum(all_join$barley-total_inhouse$barley_inhouse)
  cassava_need = sum(all_join$cv-total_inhouse$cassava_inhouse)
  
  total_need = as.data.frame(cbind(wheat_need,corn_need,soy_need,rapeseed_need,pulses_need,barley_need,cassava_need))
  
  #Calculate which countries and propotionally how much of each crop will be exported
  total_export = as.data.frame(export_perct$Country)
  total_export$wheat_export = export_perct$WheatExport*total_need$wheat_need
  total_export$corn_export = export_perct$MaizeExport*total_need$corn_need
  total_export$soy_export = export_perct$SoybeansExport*total_need$soy_need
  total_export$rapeseed_export = export_perct$RapeseedExport*total_need$rapeseed_need
  total_export$pulses_export = export_perct$PulsesExport*total_need$pulses_need
  total_export$barley_export = export_perct$BarleyExport*total_need$barley_need
  total_export$cassava_export = export_perct$CassavaExport*total_need$cassava_need
  
  names(total_export) = c("Country","wheat_exp","corn_exp","soy_exp","rapeseed_exp","pulses_exp",
                          "barley_exp","cassava_exp")
  
  #Merge in-house and export df
  summed_inhouse <- cbind(summed_wheat,summed_corn,summed_soy,summed_rapeseed,summed_pulses,summed_barley,summed_cassava)
  head(summed_inhouse)
  summed_inhouse2<-setDT(summed_inhouse, keep.rownames = TRUE)[]
  head(summed_inhouse2)
  names(summed_inhouse2) <- c("Country","wheat","corn","soy","rapeseed","pulses","barley","cassava")
  summed_inhouse2 = as.data.frame(summed_inhouse2)
  head(summed_inhouse2)
  length(unique(summed_inhouse2$Country)) 
  
  merged_data<-merge(summed_inhouse2,total_export, by="Country", all=TRUE) #all maintains all Countries even if not an export country
  megered_data<- as.data.frame(merged_data)
  merged_data[is.na(merged_data)]<-0 
  head(merged_data)
  length(unique(merged_data$Country)) #(some countries produce crop but NOT animal - those added)
  
  #Sum in-house and exports for TOTAL crops needed to be produced at the country level   
  total_country_crop <-as.data.frame(merged_data[,1])
  names(total_country_crop) <- "Country"
  head(total_country_crop)
  total_country_crop$wheat_total = merged_data$wheat+merged_data$wheat_exp
  total_country_crop$corn_total = merged_data$corn+merged_data$corn_exp
  total_country_crop$soy_total = merged_data$soy+merged_data$soy_exp
  total_country_crop$rapeseed_total = merged_data$rapeseed+merged_data$rapeseed_exp
  total_country_crop$pulses_total = merged_data$pulses+merged_data$pulses_exp
  total_country_crop$barley_total = merged_data$barley+merged_data$barley_exp
  total_country_crop$cassava_total = merged_data$cassava+merged_data$cassava_exp
  
  total_country = as.data.frame(total_country_crop)
  head(total_country)
  length(total_country$Country) 
  
  ###--CALCULATE HECTARES FROM TOTAL COUNTRY CROP (MT) ---
  all_data = left_join(total_country,yield)
  
  w.ha[,i] = all_data$wheat_total*all_data$Wheat_crop_ha
  c.ha[,i] = all_data$corn_total*all_data$Maize_crop_ha
  s.ha[,i] = all_data$soy_total*all_data$Soybeans_crop_ha
  rs.ha[,i] = all_data$rapeseed_total*all_data$Rapeseed_crop_ha
  p.ha[,i] = all_data$pulses_total*all_data$Pulses_crop_ha
  b.ha[,i] = all_data$barley_total*all_data$Barley_crop_ha
  cv.ha[,i] = all_data$cassava_total*all_data$Cassava_crop.ha
  
  all_ha <- cbind(w.ha[,i],c.ha[,i],s.ha[,i],rs.ha[,i],p.ha[,i],b.ha[,i],cv.ha[,i])
  total.ha[,i] = rowSums(all_ha)
}

#Hectares
mean.ha = rowMeans(total.ha)
sd.ha = rowSds(total.ha)
final_ha <- cbind(mean.ha,sd.ha)
write.csv(final_ha,file = "MEANS&SDs_2050_MARINE_ha.csv")

#Feed
total_feed <- as.data.frame(cbind(rowMeans(wheat),rowMeans(corn),rowMeans(soy),rowMeans(rapeseed),rowMeans(pulses),rowMeans(barley),rowMeans(cv)))
names(total_feed) <- c("wheat","corn","soy","rapeseed","pulses","barley","cassava")
head(total_feed)
write.csv(total_feed,file = "MEANS_2050_MARINE_feed.csv")

total_feed_sds <- as.data.frame(cbind(rowSds(wheat),rowSds(corn),rowSds(soy),rowSds(rapeseed),rowSds(pulses),rowSds(barley),rowSds(cv)))
names(total_feed_sds) <- c("wheat","corn","soy","rapeseed","pulses","barley","cassava")
head(total_feed_sds)
write.csv(total_feed_sds,file = "SDs_2050_MARINE_feed.csv")






