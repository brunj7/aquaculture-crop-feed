rm(list=ls())

library(ggplot2)
library(scales)
library(RColorBrewer)
library(dplyr)

#install.packages("cowplot")
#install.packages("ggpubr")
library(ggpubr)

# Set paths
# setwd("~/Desktop/github/aquaculture-crop-feed")
dir_outputs <- "Outputs"
dir_sim <- "Simulations"

# Read feed file
feed<-read.csv(file.path(dir_outputs,"Feed_plot_animal_wSD.csv"), header=TRUE) #sorted by animal
head(feed)
length(feed$Country)

#Change Order of Factors
levels(feed$scenario) #Order of default levels 
feed$scenario <-factor(feed$scenario,levels=c("Current","BAU","Mixed", "Marine")) #change order
levels(feed$scenario)

levels(feed$animals) #Order of default levels 
feed$animals <-factor(feed$animals,levels=c("beef","dairy cow","pig","sheep","goat","chicken","hen","marine fish", "marine crustacean","fw fish","fw crustacean","mollusc")) #change order
levels(feed$animals)

scen = c("Current", "BAU","Mixed", "Marine")
means = c( 1017080528 ,2882693553,2284029080,2318044162)
sds = c(0,810293804,637714615,642213996)
totals = data.frame(scenario = scen,
                       means = as.numeric(means),
                       sds = as.numeric(sds))
# head(totals)
# levels(totals$scen) #Order of default levels 
totals$scenario <-factor(totals$scenario,levels=c("Current","BAU","Mixed", "Marine")) #change order
levels(totals$scenario)
# colnames(totals) <- c("scenario","means","sds")
head(totals)

totals_new<-left_join(feed,totals,by="scenario")
head(totals_new)

cols <- colorRampPalette(brewer.pal(11, "Spectral")) #animal
myPal <- cols(length(unique(feed$animals)))

ggplot(totals_new, aes(y = Feed_tonnes_mean, x = factor(scenario), fill=factor(animals))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=means-sds, 
                    ymax=means+sds), 
                width=.2)+
  labs(x="", y = "Total Feed-crop (tonnes)" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=15),axis.text.y  = element_text(size=15),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size=17),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "gray", fill=NA), legend.position="none") +
  scale_y_continuous(limits = c(0, 3920000000))+
  scale_fill_manual(values = myPal)

cols <- colorRampPalette(brewer.pal(8, "YlGnBu")) #crops
myPal <- cols(length(unique(feed$crop)))

ggplot(totals_new, aes(y = Feed_tonnes_mean, x = factor(scenario), fill=factor(crop))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=means-sds, 
                    ymax=means+sds), 
                width=.2)+
  labs(x="", y = "Total Feed-crop (tonnes)" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=15),axis.text.y  = element_text(size=15),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size=17),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "gray", fill=NA), legend.position="none") +
  scale_y_continuous(limits = c(0, 3920000000))+
  scale_fill_manual(values = myPal)

 