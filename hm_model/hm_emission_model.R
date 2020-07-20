### model to simulate the heavy metals emission for coal power plants 
# Model: hm_emission = E*Ratio*C*R*(1-PM)* (1-SO2)*(1-NOX)*(1-Hg)
# E is the electricity generation (kwh);
# Ratio is the coal consumption (g) per Kwh power generation;
# C is the provincial average concentration of one toxic heavy metal in feed coal (ug/g); 
# R is the average release ratio of one toxic heavy metal in flue gas compared with the element concentration in feed coal from pulverized-coal (PC) boilers, circulating fluidized-bed (CFB) boilers or stoker fired (SF) boilers (%); 
# PM, SO2 NOx and Hg represent the averaged fraction of one heavy metal co- benefit removed from flue gas by the conventional PM/SO2/NOX/Hg emission control devices (%), respectively.
library(readxl)
library(openxlsx)
library(data.table)

# 1) E electricity generation 
elec_genaration = read_excel("processeddata0717_cleanversion.xlsx")

# 2) Ratio
# extract data from cec
coal_consupmtion_rate = read_excel("all_cec.xlsx")
elec_genaration$coal_consumption_rate = NA
for(i in 1:nrow(coal_consupmtion_rate)){
  if(!is.na(coal_consupmtion_rate$coal_consumption[i])){
    index = which(elec_genaration$cec_plant_code_test==coal_consupmtion_rate$plant_code[i] &
                  elec_genaration$cec_unit_code_test == coal_consupmtion_rate$unit_name[i])
    elec_genaration$coal_consumption_rate[index] = coal_consupmtion_rate$coal_consumption[i]
  }
}
# default to 300 gce/kwh
elec_genaration$coal_consumption_rate[which(is.na(elec_genaration$coal_consumption_rate))] = 300
elec_genaration$coal_consumption = elec_genaration$coal_consumption_rate*elec_genaration$eletricity_generation # unit: g 
coal_total = ele_total*coal_consump_rate  # coal_consump_rate is partially available

# 3) C provincial average concentration of one toxic heavy metal in feed coal (ug/g)
# No information for Hong Kong, and it is assume to be the same as Guangdong
coal_hm_province = read_excel("province_heavymetals_china_data.xlsx") #ug/g

# 4) R release rate boilers
# check the excel: "release_rate_boiler.xslx" for detailed releasing rate, here in the model, we use an 
# average release rate for the boilers. 
# As: 89.63%   Pb: 71.86%   Cd: 78.53%   Cr: 71.21% 
release_rate = c(0.8963, 0.7186, 0.7853, 0.7121)

# 5) PM/SO2/NOx/Hg removal rate 
# check the excel: "removal_rate_APCD.xslx" for detailed releasing rate
removal_rate_table = read_excel("removal_rates_matrix.xlsx")
# sequence: ESP FF WFGD SCR (1 means exist, 0 means doesnt exist)
apcd_setup = c(1, 0, 1, 1) # e.g., ESP + WFGD + SCR
removal_rate_cal <- function(apcd_setup){
  removal_rate = c(1, 1, 1, 1) # initial removal rates for As, Pb, Cd, Cr 
  # ESP
  if(apcd_setup[1] == 1){
    removal_rate = removal_rate*(1-removal_rate_table$ESP/100)
  }
  if(apcd_setup[2] == 1){
    removal_rate = removal_rate*(1-removal_rate_table$FF/100)
  }
  if(apcd_setup[3] == 1){
    removal_rate = removal_rate*(1-removal_rate_table$WFGD/100)
  }
  if(apcd_setup[4] == 1){
    removal_rate = removal_rate*(1-removal_rate_table$SCR/100)
  }
}

# calculate the total heavy metal emission (unit:g)
elec_genaration$As_emission = NA
elec_genaration$Pb_emission = NA
elec_genaration$Cd_emission = NA
elec_genaration$Cr_emission = NA

for(i in 1:nrow(elec_genaration)){
  #print(i)
  if(!is.na(elec_genaration$coal_consumption[i])){
    #i = 3
    index_state = which(coal_hm_province$province==elec_genaration$STATE[i])
    if(length(index_state)>0){
      coal_vector = coal_hm_province[index_state, 2:5]
      #coal_vector
      removal_rate = removal_rate_cal(apcd_setup)
      coal_hm_ug = coal_vector*elec_genaration$coal_consumption[i]  # ug 
      coal_emission = coal_hm_ug*release_rate*removal_rate
      #coal_emission
      elec_genaration$As_emission[i] = coal_emission[1]/1000 #ug->g
      elec_genaration$Pb_emission[i] = coal_emission[2]/1000 
      elec_genaration$Cd_emission[i]  = coal_emission[3]/1000 
      elec_genaration$Cr_emission[i]  = coal_emission[4]/1000  
    }
  }
}

# age and removal efficiency?


