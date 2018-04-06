
feed_harmonizer <- function(prod_biomass){
  wheat[,i] = (new_prod[,prod_biomass]*new_prod$fcrs*new_prod$Wheat)*ff$Farm_factor_wheat
  corn[,i] = (new_prod[,prod_biomass]*new_prod$fcrs*new_prod$Maize)*ff$Farm_factor_corn
  soy[,i] = (new_prod[,prod_biomass]*new_prod$fcrs*new_prod$Soy)*ff$Farm_factor_soy
  rapeseed[,i] = (new_prod[,prod_biomass]*new_prod$fcrs*new_prod$Rapeseed)*ff$Farm_factor_rapeseed
  pulses[,i] = (new_prod[,prod_biomass]*new_prod$fcrs*new_prod$Pulses)*ff$Farm_factor_pulses
  barley[,i] = (new_prod[,prod_biomass]*new_prod$fcrs*new_prod$Barley)*ff$Farm_factor_barley
  cv[,i] = (new_prod[,prod_biomass]*new_prod$fcrs*new_prod$Cassava)*ff$Farm_factor_cassava
}