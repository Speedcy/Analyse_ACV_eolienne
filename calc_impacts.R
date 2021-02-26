##R script with the functions necessary to compute the GHG performances of a wind turbine

# use  rm(list = ls()) if you want to remove all previous objects
# use  cat("\014") if you want to clean the console


setwd("C:/Users/cypri/Documents/Documents Cyprien/3A M2 - Economie de l'Energie/VF12/TD_Documents/TD_Documents/SECTIONS-2-3_ACV-de-l-eolien") 
## the pathway must be adjusted to your own location

load("oneWT_data.RData")
ls()

load("Ematrix_unitProcess.RData")
E[1,]
dim(E)

source("functions_WT.R")
ls()

load("Qmatrix_IPCC.RData")
Q[1,]
dim(Q)


#let's go!

CF_selected = "CF.IPCC.100y.2007"
QE = impact_unitProcess(Q, E, CF_selected)

per_replaced_nacelle = maintenance[1,1]


Impact_WT = impact_WT(M_nacelle, share_nacelle, M_rotor, share_rotor, M_tower, share_tower, M_foundation, share_foundation, per_replaced_nacelle, share_transport_movingPart, distance_movingPart, share_transport_fixedPart, distance_fixedPart, Q_diesel, Q_electMedVolt, QE)   
elec_prod = elecProd(availability, load_factor, life_time, nom_power)
GHG_perf = Impact_WT / elec_prod

