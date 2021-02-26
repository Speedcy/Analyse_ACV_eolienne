##R script with the functions necessary to compute the GHG performances of a wind turbine

#Functions definition

# 1.FUNCTIONS TO MODEL THE ONSHORE WIND POWER ELECTRICITY
# PROCESS CHAIN

prod_invent_ws = function(M_part, share_part){
  return(M_part*share_part/100)
}

# Vecteur procédé nacelle
p_nacelle<-prod_invent_ws(M_nacelle,share_nacelle)

# Vecteur procédé rotor
p_rotor<-prod_invent_ws(M_rotor,share_rotor)

# Vecteur procédé fondation
p_foundation<-prod_invent_ws(M_foundation,share_foundation)

# Vecteur procédé tower
p_tower<-prod_invent_ws(M_tower,share_tower)

# Vecteur procédé maintenance
p_maintenance=prod_invent_ws(M_nacelle,share_nacelle)*maintenance[1,1]*0.01

# Vecteur procédé énergie
p_energy=rbind(Q_diesel,Q_electMedVolt)
colnames(p_energy)<-"WT1"

# Vecteur procédé transport
p_transport<-rbind(1,2,3)
p_transport[2,1]<-(M_nacelle+M_rotor)*distance_movingPart*share_transport_movingPart[1,1]/1000
p_transport[3,1]<-(M_tower+M_foundation)*distance_fixedPart*share_transport_fixedPart[1,1]/1000
p_transport[1,1]<-(M_nacelle+M_rotor)*distance_movingPart*share_transport_movingPart[2,1]/1000+(M_tower+M_foundation)*distance_fixedPart*share_transport_fixedPart[2,1]/1000
rownames(p_transport)<-c("train","lorry 28t","lorry 32t")
colnames(p_transport)<-"WT1"


p=rbind(rep(0,18))

# 1 Cast Iron
p[1]=p_nacelle[4,1]+p_rotor[3,1]+p_maintenance[4,1]
# 2 Chromium Steel
p[2]=p_nacelle[3,1]+p_rotor[1,1]+p_maintenance[3,1]
# 3 Reinforcing Steel
p[3]=p_foundation[2,1]
# 4 Steel low
p[4]=p_tower[1,1]+p_nacelle[2,1]+p_maintenance[2,1]
# 5 Aluminium
p[5]=p_rotor[4,1]+p_nacelle[7,1]+p_maintenance[7,1]
# 6 Copper
p[6]=p_nacelle[6,1]+p_maintenance[6,1]
# 7 Concrete
p[7]=p_foundation[1,1]/density_conc[1]
# 8 GPRF
p[8]=p_nacelle[1,1]+p_rotor[2,1]+p_maintenance[1,1]
# 9 Rail
p[9]=p_transport[1,1]
# 10 lorry 28
p[10]=p_transport[2,1]
# 11 lorry 32
p[11]=p_transport[3,1]
# 12 Sheet Rolling Aluminium
p[12]=p_rotor[4,1]+p_nacelle[7,1]+p_maintenance[7,1]
# 13 Sheet Rolling Chromium Steel
p[13]=p_nacelle[3,1]+p_rotor[1,1]+p_maintenance[3,1]
# 14 Sheet Rolling Steel
p[14]=p_tower[1,1]+p_nacelle[2,1]+p_maintenance[2,1]+p_nacelle[4,1]+p_rotor[3,1]+p_maintenance[4,1]
# 15 Wire drawing steel
p[15]=p_nacelle[6,1]+p_maintenance[6,1]
# 16 Lubricating oil
p[16]=p_nacelle[5,1]+p_maintenance[5,1]
# 17 Diesel
p[17]=p_energy[1,1]
# 18 Elec
p[18]=p_energy[2,1]

p=data.frame(p)
p=t(as.matrix(p))
colnames(p)<-"WT1"


# 2.FUNCTIONS TO MODEL THE IMPACTS OF THE ONSHORE WIND POWER ELECTRICITY

impact_unitProcess = function(Q, E, CF_name){
  Q_cf = Q[CF_name]
  E1 = E[,4:ncol(E)]
  QE = as.matrix(t(Q_cf)) %*% as.matrix(E1)
  return(QE)
}

impact_nacelle = function(M_nacelle, share_nacelle, I_GFRP, I_steel, I_sheetSteel, I_castIron, I_chromiumSteel, I_sheetChromium, I_oil, I_copper, I_wire, I_alu, I_sheetAlu){
  p_nacelle = prod_invent_ws(M_nacelle, share_nacelle)  
  Imp_nacelle = p_nacelle["GFRP",1]*I_GFRP+
                p_nacelle["steel",1]*(I_steel+I_sheetSteel)+
                p_nacelle["inox",1]*(I_chromiumSteel+I_sheetChromium)+
                p_nacelle["cast_iron",1]*(I_castIron+I_sheetSteel)+
                p_nacelle["oil",1]*I_oil+
                p_nacelle["copper",1]*(I_copper+I_wire)+
                p_nacelle["aluminium",1]*(I_alu+I_sheetAlu)
  return(Imp_nacelle)
}

impact_rotor = function(M_rotor, share_rotor, I_GFRP, I_sheetSteel, I_castIron, I_chromiumSteel, I_sheetChromium, I_alu, I_sheetAlu){
  p_rotor = prod_invent_ws(M_rotor,share_rotor)
  Imp_rotor = p_rotor["inox",1]*(I_chromiumSteel+I_sheetChromium)+
    p_rotor["GFRP",1]*I_GFRP+
    p_rotor["aluminium",1]*(I_alu+I_sheetAlu)+
    p_rotor["cast_iron",1]*(I_castIron+I_sheetSteel)
  return(Imp_rotor)
}

impact_tower = function(M_tower, share_tower, I_steel, I_sheetSteel){
  p_tower = prod_invent_ws(M_tower, share_tower)  
  Imp_tower = p_tower["steel",1]*(I_steel+I_sheetSteel)
  return(Imp_tower)
}

impact_foundation = function(M_foundation, share_foundation, I_reinfSteel, I_concrete){
  p_foundation = prod_invent_ws(M_foundation, share_foundation)  
  Imp_foundation = p_foundation["concrete",1]*I_concrete/2200+
    p_foundation["reinforcing_steel",1]*I_reinfSteel
  return(Imp_foundation)
}

impact_maintenance = function(per_replaced_nacelle, Ip_nacelle){
  Imp_maintenance = Ip_nacelle*per_replaced_nacelle/100
  return(Imp_maintenance)
}

impact_energy = function(Q_diesel, Q_electMedVolt, I_diesel, I_electMedVolt){
  p_energy=rbind(Q_diesel,Q_electMedVolt)
  colnames(p_energy)<-"WT1"
  Imp_energy = p_energy["Q_diesel",1]*I_diesel+
    p_energy["Q_electMedVolt",1]*I_electMedVolt
  return(Imp_energy)
}

impact_transport = function(M_nacelle, M_rotor, M_tower, M_foundation, share_transport_movingPart, distance_movingPart, share_transport_fixedPart, distance_fixedPart, I_lorry28t, I_lorry32t, I_train){
  p_transport<-rbind(1,2,3)
  p_transport[2,1]<-(M_nacelle+M_rotor)*distance_movingPart*share_transport_movingPart[1,1]/1000
  p_transport[3,1]<-(M_tower+M_foundation)*distance_fixedPart*share_transport_fixedPart[1,1]/1000
  p_transport[1,1]<-(M_nacelle+M_rotor)*distance_movingPart*share_transport_movingPart[2,1]/1000+(M_tower+M_foundation)*distance_fixedPart*share_transport_fixedPart[2,1]/1000
  rownames(p_transport)<-c("train","lorry 28t","lorry 32t")
  colnames(p_transport)<-"WT1"
  Imp_transport = p_transport["train",1]*I_train+
    p_transport["lorry 28t",1]*I_lorry28t+
    p_transport["lorry 32t",1]*I_lorry32t
  return(Imp_transport)
}

impact_WT = function(M_nacelle, share_nacelle, M_rotor, share_rotor, M_tower, share_tower, M_foundation, share_foundation, per_replaced_nacelle, share_transport_movingPart, distance_movingPart, share_transport_fixedPart, distance_fixedPart, Q_diesel, Q_electMedVolt, QE){
  
  I_GFRP = QE[8]
  I_steel = QE[4] 
  I_sheetSteel = QE[14] 
  I_castIron = QE[1]
  I_chromiumSteel  = QE[2]
  I_sheetChromium  = QE[13]
  I_oil = QE[16]
  I_copper = QE[6]
  I_wire = QE[15]
  I_alu  = QE[5]
  I_sheetAlu = QE[12]
  I_reinfSteel = QE[3]
  I_concrete = QE[7]
  I_lorry28t = QE[10]
  I_lorry32t = QE[11]
  I_train = QE[9]
  I_diesel = QE[17]
  I_electMedVolt = QE[18] 

  Ip_nacelle = impact_nacelle(M_nacelle, share_nacelle, I_GFRP, I_steel, I_sheetSteel, I_castIron, I_chromiumSteel, I_sheetChromium, I_oil, I_copper, I_wire, I_alu, I_sheetAlu)
  Ip_rotor = impact_rotor(M_rotor, share_rotor, I_GFRP, I_sheetSteel, I_castIron, I_chromiumSteel, I_sheetChromium, I_alu, I_sheetAlu)
  Ip_tower = impact_tower(M_tower, share_tower, I_steel, I_sheetSteel)
  Ip_foundation = impact_foundation(M_foundation, share_foundation, I_reinfSteel, I_concrete)
  Ip_maintenance = impact_maintenance(per_replaced_nacelle, Ip_nacelle)
  Ip_transport = impact_transport(M_nacelle, M_rotor, M_tower, M_foundation, share_transport_movingPart, distance_movingPart, share_transport_fixedPart, distance_fixedPart, I_lorry28t, I_lorry32t, I_train)
  Ip_energy = impact_energy(Q_diesel, Q_electMedVolt, I_diesel, I_electMedVolt) 

   print(c("Nacelle",Ip_nacelle))
   print(c("Rotor",Ip_rotor))
   print(c("Tower",Ip_tower))
   print(c("Foundation",Ip_foundation))
   print(c("Maintenance",Ip_maintenance))
   print(c("Transport",Ip_transport))
   print(c("Energy",Ip_energy))
 
    return(Ip_nacelle+Ip_rotor+Ip_tower+Ip_foundation+Ip_maintenance+Ip_energy+Ip_transport)
}


# 3. FUNCTIONS TO CALCULATE ELECTRICITY PRODUCTION
# OVER THE TURBINE LIFETIME (TO BE CREATED BY THE STUDENTS)

elecProd = function(availability, load_factor, life_time, nom_power){
  E=8760*availability*load_factor*life_time*nom_power
  return(E)
}


# 4. FUNCTIONS TO CALCULATE THE TURBINE DIMENSIONS 
# (NOT NEEDED FOR OUR PROJECT)

calc_D_h = function(P){
  D = 1.5624 * P^(0.522)
  h = 0.0153 *P + 48.493
  return(c(D, h))
}

calc_weights = function(D, h){
  M_nacelle = 10^(0.64)*D^(2.19)
  M_rotor = 10^(0.3)*D^(2.22)
  M_tower = 10^(1.7)*D^(1.82)
  V_conc = 4500*h/2200
  M_reinfSteel = 280*h
  M_foundation = V_conc*density_conc + M_reinfSteel
  return(c(M_nacelle, M_rotor, M_tower, M_foundation, V_conc, M_reinfSteel))
}



