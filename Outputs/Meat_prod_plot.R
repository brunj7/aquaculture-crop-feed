library(ggplot2)
library(scales)
library(wesanderson)
library(RColorBrewer)

prod<-read.csv("Meat_Estimates.csv", header=TRUE) 
head(prod)

#Change Order of Factors
levels(prod$Scenario) #Order of default levels 
prod$Scenario <-factor(prod$Scenario,levels=c("Current","BAU","Mixed", "Marine")) #change order
levels(prod$Scenario)

levels(prod$animals) #Order of default levels 
prod$animals <-factor(prod$animals,levels=c("beef","dairy cow","pig","sheep","goat","chicken","hen","marine fish", "marine crustacean","fw fish","fw crustacean","mollusc"))
levels(prod$animals)

cols <- colorRampPalette(brewer.pal(11, "Spectral"))
myPal <- cols(length(unique(prod$animals)))

ggplot(prod, aes(y = Prod_tonnes_mean, x = factor(Scenario), fill=factor(animals), order = as.factor(animals))) +
  geom_bar(stat = "identity") +
  labs(x="", y = "Total Biomass (tonnes)" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=15),axis.text.y  = element_text(size=15),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size=17),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "gray", fill=NA)) +
  scale_y_continuous(limits = c(0, 842263775))+
  scale_fill_manual(values = myPal)