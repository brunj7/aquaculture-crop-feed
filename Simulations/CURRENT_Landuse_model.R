
library(ggplot2)
library(maps)
library(sp)
library(maptools)
library(mapdata)
library(scales)
library(mapproj)
library(RColorBrewer)
library(rworldmap)
library(RColorBrewer)
library(data.table)
library(dplyr)

rm(list=ls())
#Total animal feed need - not harmonized (FAO)
#Calculated from 'Total_NonHarmonized_feed.R'
feed_noff<-read.csv("Total_Feed_mt_NonHarmonized.csv") 
head(feed_noff)

#######-------------------------------------  
##CALCULATE ADJUSTED FEED AND IN-COUNTRY + EXPORT
###----------------------------------------------------

#Calculate Feed adjust (i.e., harmonized) by "Farm Factor" (or ff)

#Adjust (harmonize) feed at country level
  ff<-read.csv("Country_Harmonize_Factors.csv", header=TRUE) 
  head(ff)

  adj_feed <- feed_noff[,c(1:5)]
  head(adj_feed)
  adj_feed$wheat = (feed_noff$mean_wheat)*ff$Farm_factor_wheat
  adj_feed$corn = (feed_noff$mean_corn)*ff$Farm_factor_corn
  adj_feed$soy = (feed_noff$mean_soy)*ff$Farm_factor_soy
  adj_feed$rapeseed = (feed_noff$mean_rs)*ff$Farm_factor_rapeseed
  adj_feed$pulses = (feed_noff$mean_pulses)*ff$Farm_factor_pulses
  adj_feed$barley = (feed_noff$mean_barley)*ff$Farm_factor_barley
  adj_feed$cassava = (feed_noff$mean_cv)*ff$Farm_factor_cassava
  
  head(adj_feed)

 #write.csv(adj_feed,file = "CURRENT_Animal_Feed_mt.csv") ###################

#Calculate how much of feed COULD BE produced in-house (domestically)
  inhouse_perct<-read.csv("FAO_2013_Percent_InCountryCrop_Prod.csv", header=TRUE)
  head(inhouse_perct)
  
  new_adj <-left_join(adj_feed,inhouse_perct)
  new_adj[is.na(new_adj)] <- 0
  head(new_adj)

  inhouse_prod <- new_adj[,c(1:5)]
  head(inhouse_prod)
  inhouse_prod$wheat = new_adj$wheat*new_adj$Wheat..Prod
  inhouse_prod$corn = new_adj$corn*new_adj$Maize..Prod
  inhouse_prod$soy = new_adj$soy*new_adj$Soybeans..Prod
  inhouse_prod$rapeseed = new_adj$rapeseed*new_adj$Rapeseed..Prod
  inhouse_prod$pulses = new_adj$pulses*new_adj$Pulses..Prod
  inhouse_prod$barley =new_adj$barley*new_adj$Barley..Prod
  inhouse_prod$cassava =new_adj$cassava*new_adj$Cassava..Prod
  head(inhouse_prod)


#Calculate remaining crop needed from exporting countries
  import_need <- inhouse_prod[,c(1:5)]
  head(import_need)
  import_need$wheat = new_adj$wheat-inhouse_prod$wheat
  import_need$corn = new_adj$corn-inhouse_prod$corn
  import_need$soy = new_adj$soy-inhouse_prod$soy
  import_need$rapeseed = new_adj$rapeseed-inhouse_prod$rapeseed
  import_need$pulses = new_adj$pulses-inhouse_prod$pulses
  import_need$barley = new_adj$barley-inhouse_prod$barley
  import_need$cassava = new_adj$cassava-inhouse_prod$cassava
  
  head(import_need)
  
  total_need<-colSums(import_need[,c(6:12)])
  total_need

#Calculate which countries and propotionally how much of each crop will be exported
  export_perct<-read.csv("Country_Export_Percent.csv", header=TRUE)
  
  head(export_perct)

  num<-length(export_perct$Country)
  total_export = export_perct[,c(2:8)]*rep(total_need,num)
  total_export$Country <- export_perct$Country
  head(total_export)

  
  #Combine the two dataframes by 'Country'
  head(inhouse_prod)
  
  summed_wheat = as.data.frame(tapply(inhouse_prod$wheat, inhouse_prod$Country, sum))
  summed_corn = as.data.frame(tapply(inhouse_prod$corn, inhouse_prod$Country, sum))
  summed_soy = as.data.frame(tapply(inhouse_prod$soy, inhouse_prod$Country, sum))
  summed_rapeseed = as.data.frame(tapply(inhouse_prod$rapeseed, inhouse_prod$Country, sum))
  summed_pulses = as.data.frame(tapply(inhouse_prod$pulses, inhouse_prod$Country, sum))
  summed_barley = as.data.frame(tapply(inhouse_prod$barley, inhouse_prod$Country, sum))
  summed_cassava = as.data.frame(tapply(inhouse_prod$cassava, inhouse_prod$Country, sum))

  summed_inhouse <- cbind(summed_wheat,summed_corn,summed_soy,summed_rapeseed,summed_pulses,summed_barley,summed_cassava)
  head(summed_inhouse)
  summed_inhouse2<-setDT(summed_inhouse, keep.rownames = TRUE)[]
  head(summed_inhouse2)
  names(summed_inhouse2) <- c("Country","wheat","corn","soy","rapeseed","pulses","barley","cassava")
  summed_inhouse2 = as.data.frame(summed_inhouse2)

  merged_data<-merge(summed_inhouse2,total_export, by="Country", all=TRUE) #all maintains all Countries even if not an export country
  megered_data<- as.data.frame(merged_data)
  merged_data[is.na(merged_data)]<-0
  merged_data
  length(unique(merged_data$Country)) #some countries produce crop but NOT animal - those added)

#Sum in-house and exports for TOTAL crops needed to be produced at the country level   
  total_country_crop <-as.data.frame(merged_data[,-c(2:15)])
  names(total_country_crop) <- "Country"
  head(total_country_crop)
  total_country_crop$wheat_total = merged_data$wheat+merged_data$WheatExport
  total_country_crop$corn_total = merged_data$corn+merged_data$MaizeExport
  total_country_crop$soy_total = merged_data$soy+merged_data$SoybeansExport
  total_country_crop$rapeseed_total = merged_data$rapeseed+merged_data$RapeseedExport
  total_country_crop$pulses_total = merged_data$pulses+merged_data$PulsesExport
  total_country_crop$barley_total = merged_data$barley+merged_data$BarleyExport
  total_country_crop$cassava_total = merged_data$cassava+merged_data$CassavaExport

  total_country = as.data.frame(total_country_crop)

###---------------------
###--CALCULATE HECTARES FROM TOTAL COUNTRY CROP (MT)
###-----------------------
yield<-read.csv("FAO_2013_yield_ha.csv", header=TRUE) #no need to revise
head(yield)
length(unique(yield$Country)) 
length(yield$Country)

new_total_country<- left_join(total_country,yield)
head(new_total_country)
length(new_total_country$Country) 
new_total_country[is.na(new_total_country)] <- 0

landuse_ha <- as.data.frame(total_country[,1])
names(landuse_ha) <- c("country")
head(landuse_ha)
  landuse_ha$wheat_ha = new_total_country$wheat_total*new_total_country$Wheat_crop_ha
  landuse_ha$corn_ha = new_total_country$corn_total*new_total_country$Maize_crop_ha
  landuse_ha$soy_ha = new_total_country$soy_total*new_total_country$Soybeans_crop_ha
  landuse_ha$rapeseed_ha = new_total_country$rapeseed_total*new_total_country$Rapeseed_crop_ha
  landuse_ha$pulses_ha = new_total_country$pulses_total*new_total_country$Pulses_crop_ha
  landuse_ha$barley_ha = new_total_country$barley_total*new_total_country$Barley_crop_ha
  landuse_ha$cassava_ha = new_total_country$cassava_total*new_total_country$Cassava_crop.ha
  head(landuse_ha)
  landuse_ha$total_ha = rowSums(landuse_ha[,c(2:8)])
head(landuse_ha)

#Total hectares of land use from feed-crops
sum(landuse_ha$total_ha) 
write.csv(landuse_ha,file = "CURRENT_Landuse_HA.csv") 

