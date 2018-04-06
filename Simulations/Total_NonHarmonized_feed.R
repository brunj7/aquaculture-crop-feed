##CODE CACLUALTES TOTAL, CURRENT DRY-MATTER CROP REQUIREMENTS

rm(list=ls())

library(dplyr)
library(matrixStats) #for rowSds

#COMPOSITION OF FEED (proportions) from FAO, Tilman & Clark (2014), and other---------------
#If fed now, assume increase propotional to linear increase of FAO feed results (for each crop type)
#marine & fw based on Tilman & CLark means assigned to each region
animal.comp<-read.csv("MARINE&FW&LIVESTOCK_CropProps.csv", header=TRUE)
head(animal.comp)

#Total production (FAO)
prod<-read.csv("All_ProductionScenarios_ONFEED.csv", header=TRUE)
head(prod)

#animals
amls = read.csv("animals.csv")

##FEED CONVERSOPN RATIOS-------------

# Create NA bank for each feed type
r.length = length(prod$Country) 
wheat<-matrix(NA,nrow=r.length,ncol=500)
corn<-matrix(NA,nrow=r.length,ncol=500)
soy<-matrix(NA,nrow=r.length,ncol=500)
rapeseed<-matrix(NA,nrow=r.length,ncol=500)
pulses<-matrix(NA,nrow=r.length,ncol=500)
barley<-matrix(NA,nrow=r.length,ncol=500)
cv<-matrix(NA,nrow=r.length,ncol=500)


for(i in 1:500){
set.seed(29)
fcr_beef = round(runif(min=6.0,max=31.2, 1),2)
fcr_chick = round(runif(min=1.8,max= 3.5, 1), 2)
fcr_dairy = round(runif(min=6,max=31.2, 1),2) 
fcr_goat = round(runif(min = 4.0, max = 12.9, 1),2)
fcr_hen = round(runif(min=1.8,max= 3.5, 1), 2)
fcr_pig = round(runif(min = 3.0, max = 5.9, 1), 2)
fcr_sheep = round(runif(min = 4.0, max = 12.9, 1),2)
fcr_fish = round(runif(min = 1.1, max=1.6, 1),2)
fcr_crust = round(runif(min = 1.1, max = 1.7,1), 2)
fcr_fwfish = round(runif(min = 1.1, max = 1.6, 1), 2)
fcr_fwcrust = round(runif(min=1.1, max = 1.8, 1), 2)
moll_fcr = 0

fcrs = c(fcr_dairy,fcr_hen,fcr_beef,fcr_goat,fcr_sheep,
         fcr_chick,fcr_pig,fcr_fish,fcr_crust,fcr_fwfish,fcr_fwcrust,moll_fcr)

amls$fcrs<-fcrs
all_fcrs = amls
all_fcrs
head(prod)

#join values
  new_prod = left_join(prod,all_fcrs)
  new_prod = left_join(new_prod,animal.comp)
  head(new_prod)

##CALCULATIONS---AMOUNT OF CURRENT CROP FOR ANIMAL PRODUCTION (NON-HARMONIZED)----------

  wheat[,i] = new_prod$Current_onfeed*new_prod$fcrs*new_prod$Wheat
  corn[,i] = new_prod$Current_onfeed*new_prod$fcrs*new_prod$Maize
  soy[,i] = new_prod$Current_onfeed*new_prod$fcrs*new_prod$Soy
  rapeseed[,i] = new_prod$Current_onfeed*new_prod$fcrs*new_prod$Rapeseed
  pulses[,i] = new_prod$Current_onfeed*new_prod$fcrs*new_prod$Pulses
  barley[,i] = new_prod$Current_onfeed*new_prod$fcrs*new_prod$Barley
  cv[,i] = new_prod$Current_onfeed*new_prod$fcrs*new_prod$Cassava

}

head(new_prod)
truc_prod = prod[,c(1:6)]
head(truc_prod)

truc_prod$mean_wheat=rowMeans(wheat)
truc_prod$mean_corn=rowMeans(corn)
truc_prod$mean_soy=rowMeans(soy)
truc_prod$mean_rs=rowMeans(rapeseed)
truc_prod$mean_pulses=rowMeans(pulses)
truc_prod$mean_barley=rowMeans(barley)
truc_prod$mean_cv=rowMeans(cv)

truc_prod$sd_wheat=rowSds(wheat)
truc_prod$sd_corn=rowSds(corn)
truc_prod$sd_soy=rowSds(soy)
truc_prod$sd_rs=rowSds(rapeseed)
truc_prod$sd_pulses=rowSds(pulses)
truc_prod$sd_barley=rowSds(barley)
truc_prod$sd_cv=rowSds(cv)

head(truc_prod)

write.csv(truc_prod,file = "Total_Feed_mt_NonHarmonized.csv")

