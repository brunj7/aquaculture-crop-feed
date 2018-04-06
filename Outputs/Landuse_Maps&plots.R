library(maps)
library(sp)
library(maptools)
library(mapdata)
library(scales)
library(mapproj)
library(RColorBrewer)
library(rgdal)
library(ggplot2)
library(beyonce) 
library(rworldmap)
library(RColorBrewer)

#Percent of land spared, used, or unchanged relative to BAU scenario
prop<-read.csv("Percent_map_all.csv", header=TRUE)
head(prop)

sPDF.prop.nt <- joinCountryData2Map(prop ,joinCode = "ISO3", nameJoinColumn = "ISO3", verbose=TRUE)
sPDF.prop <- spTransform(sPDF.prop.nt, CRS="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")


##CROP LANDUSE % MAPS------

numCats <- 6
colourPalette <- colorRampPalette(brewer.pal(numCats, "RdBu")) (6)
catmethod=c(-1,-0.50,-0.25,0,0.25,0.50,1) #feed

#quartz()
par(mfrow = c(2, 1))
par(mar = c(2, 1, 1, 1) + 0.1) # c(bottom, left, top, right)
mapParams<-mapCountryData(sPDF.prop,nameColumnToPlot='MIXED_crop', catMethod=catmethod, colourPalette=colourPalette, 
                          missingCountryCol="dark gray",addLegend=F,oceanCol="white", mapTitle=" ")

  do.call(addMapLegend, c(mapParams, legendLabels="all", legendWidth=2, digits=1, horizontal = T))


mapParamsMAR<-mapCountryData(sPDF.prop,nameColumnToPlot='MARINE_crop', catMethod=catmethod, colourPalette=colourPalette, 
                             missingCountryCol="dark gray", addLegend=F,oceanCol="white", mapTitle=" ")



  
##GRAZING + CROP % MAP(S)------- 
  mapParams<-mapCountryData(sPDF.prop,nameColumnToPlot='MIXED_c.g', catMethod=catmethod, colourPalette=colourPalette, 
                            missingCountryCol="dark gray",addLegend=F,oceanCol="white", mapTitle=" ")

  do.call(addMapLegend, c(mapParams, legendLabels="all", legendWidth=2, digits=1, horizontal = T))

  #MARINE SCENARIO NOT DEPICTED IN STUDY BECAUSE RESULT THE SAME
  #mapParamsMAR<-mapCountryData(sPDF.prop,nameColumnToPlot='MARINE_c.g', catMethod=catmethod, colourPalette=colourPalette, 
  #                             missingCountryCol="dark gray", addLegend=F,oceanCol="white", mapTitle=" ")
  #do.call(addMapLegend, c(mapParamsMAR, legendLabels="all", legendWidth=7, digits=1, horizontal = F))
  
##----------
#Number of countries pos (spared) or neg (used) land
  num_con<-read.csv("PosNeg_plot_all.csv", header=TRUE) 
  head(num_con)
  
  counts = num_con[,-1]
  head(counts)
  
  totals <- as.data.frame(colSums(counts))
  names(totals) = "no_regions"
  totals$scenario = c("Used","Spared","Used","Spared","Used","Spared","Used","Spared")
  totals$id <- c("Mixed","Mixed","Marine","Marine","Mixed","Mixed","Marine","Marine")
  totals
  
  crop_tot = as.data.frame(totals[c(1:4),])
  both_tot = totals[c(5:8),]
  
  head(crop_tot)
  
  cols <- colorRampPalette((beyonce_palette(12)))
  myPal <- cols(length(unique(crop_tot$id)))
  
  #Change Order of Factors
  crop_tot$id <-factor(crop_tot$id,levels=c("Mixed","Marine")) #change order
  levels(crop_tot$id)
  
  ggplot(crop_tot, aes(x=scenario, y = no_regions, fill=factor(id))) +
    geom_bar(stat = "identity",position=position_dodge()) +
    #coord_flip()+
    labs(x="", y = "No. Regions" )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x  = element_text(size=13),axis.text.y  = element_text(size=13),
          axis.title.x = element_text(size=15), axis.title.y = element_text(size=15),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "white", fill=NA)) +
    scale_y_continuous(limits = c(0, 215))+
    scale_fill_manual(values = myPal)

##---------------
#Total land plots
  #Number of countries pos or neg
  total_ha<-read.csv("Total_ha_plot_all.csv", header=TRUE) 
  
  head(total_ha)
  
  #Change Order of Factors
  levels(total_ha$Scenario) #Order of default levels 
  total_ha$Scenario <-factor(total_ha$Scenario,levels=c("Current","BAU","Mixed", "Marine")) #change order
  head(total_ha$Scenario)
  
  crop_tot = total_ha[c(2,4,6,8),]
  crop_tot
  
  cols <- colorRampPalette((beyonce_palette(66)))
  myPal <- cols(length(unique(crop_tot$Scenario)))
 
  pd <- position_dodge(0.1)
  ggplot(crop_tot, aes(x=Scenario, y = ha_total, fill=factor(Scenario))) +
    geom_bar(stat = "identity",position=position_dodge()) +
    geom_errorbar(aes(ymin=ha_total-ha_total_sd, 
                      ymax=ha_total+ha_total_sd), 
                  width=.2, position=pd)+ 
    labs(x="", y = "Total Cropland Use (ha)" )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x  = element_text(size=13),axis.text.y  = element_text(size=13),
          axis.title.x = element_text(size=15), axis.title.y = element_text(size=15),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "white", fill=NA)) +
    scale_fill_manual(values = rev(myPal))
  
  ggplot(total_ha, aes(x=Scenario, y = ha_total, fill=factor(id))) +
    geom_bar(stat = "identity") +
    labs(x="", y = "Total Land Use (ha)" )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x  = element_text(size=13),axis.text.y  = element_text(size=13),
          axis.title.x = element_text(size=15), axis.title.y = element_text(size=15),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "white", fill=NA)) +
    scale_fill_manual(values = myPal)

  