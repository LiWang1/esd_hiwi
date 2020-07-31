### model to simulate the heavy metals emission for coal power plants 
# Model: hm_emission = E*Ratio*C*R*(1-PM)* (1-SO2)*(1-NOX)*(1-Hg)
# E is the electricity generation (kwh);
# Ratio is the coal consumption (g) per Kwh power generation;
# C is the provincial average concentration of one toxic heavy metal in feed coal (ug/g); 
# R is the average release ratio of one toxic heavy metal in flue gas compared with the element concentration in feed coal from pulverized-coal (PC) boilers, circulating fluidized-bed (CFB) boilers or stoker fired (SF) boilers (%); 
# PM, SO2 NOx and Hg represent the averaged fraction of one heavy metal co- benefit removed from flue gas by the conventional PM/SO2/NOX/Hg emission control devices (%), respectively.

# HTEs emissions in CFPPs differ on the basis of the combustion method, coal type, and coal cleaning technique.
library(readxl)
library(openxlsx)
library(data.table)

# read the data available 
elec_genaration = read_excel("datainput_hm_models.xlsx")
elec_genaration$coal_consumption_rate[which(is.na(elec_genaration$coal_consumption_rate & !is.na(elec_genaration$eletricity_generation)))] = 312 # annual report 2017
elec_genaration$coal_consumption = elec_genaration$coal_consumption_rate*elec_genaration$eletricity_generation # unit: g 

# raw coal hm concentrations
coal_hm_province = read_excel("province_heavymetals_china_data.xlsx") # provincial average concentration https://www.sciencedirect.com/science/article/pii/S0959652615005417
coal_hm_type = read_excel("coaltype_hm_data.xlsx")  # average concentration of different types: https://pubs.acs.org/doi/10.1021/ef3017305
hm_concentration <- function(prov, coal_type){
  index_state = which(coal_hm_province$province==prov)
  # if coaltype not exist, use the province average
  if(is.na(coal_type)){
    coal_concentration = coal_hm_province[index_state, 2:7]
  }  
  
  # if coaltype exist 
  if(!is.na(coal_type)){
    coal_concentration = NA
    coal_concentration_prov = coal_hm_province[index_state, 2:7]
    index_type = which(stri_detect_fixed(coal_type, coal_hm_type$coaltype))
    for(j in 1:length(coal_concentration_prov)){
      low = 2+(j-1)*3
      high = 4+(j-1)*3
      range = coal_hm_type[index_type, low:high]
      
      # if the province concentration falls in the range, take it as the concentration
      if(coal_concentration_prov[j]>range[2] & coal_concentration_prov[j]<range[3]){
        coal_concentration[j] = coal_concentration_prov[j]
      }
      
      # if not, choose the mean of the coal type 
      else{
        coal_concentration[j] = range[1]
      }
      coal_concentration = unlist(coal_concentration)
    }
  }
  return(coal_concentration)
}

# pre-treament of the coal, mainly coal washing. 
pre_treatment <- function(hm_concentration_ori){
  ratio_washing = 0.2193 # params, taken from https://www.sciencedirect.com/science/article/pii/S0959652615005417
  removal_rate_washing = c(0.54, 0.363, 0.322, 0.58, 0.5, 0.3)
  
  for(i in 1:length(hm_concentration_ori)){
    hm_concentration_ori[i] = hm_concentration_ori[i]*(1-ratio_washing) + ratio_washing*hm_concentration_ori[i]*(1-removal_rate_washing[i])
  }
  return(hm_concentration_ori)
} 

# combustion type, either 'pc', 'cfb' or 'sf'
# check the excel: "release_rate_boiler.xslx" for detailed releasing rate, here in the model, we use an 
# the function has been built based on the facts: 
# 1, china has a total of more than 3000 CFB boiler units, the largest number of such units in the world.
# 2, after 2012, there is a stricter pollution requirements for new built coal power plants in China -> cfb has less emission compared to pc
# 3, stoker fired boiler is mainly for local or industrial heat or heat and power generating plants, and less efficient but flexible
release_rate <- function(MW, year){
  # decide the boiler type from MW and year 
  type = NA
  if(is.na(MW) | is.na(year)){
    type = 'cfb'
  }
  else{
    if(MW>=100 & year >=2012){
      type = 'cfb'
    }
    if(MW>=100 & year < 2012){
      type = sample(c('cfb','pc'), size = 1, prob = c(1, 1))
    }
    if(MW<100 & year >= 2012){
      type = sample(c('cfb', 'sf'), size = 1, prob = c(1, 1))
    }
    if(MW<100 & year < 2012){
      type = sample(c('cfb', 'sf', 'pc'), size = 1, prob = c(1, 1, 1))
    }
  }
  
  # decide the release rate according to boiler type
  if(type == 'cfb'){
    release_rate = c(0.756, 0.7733, 0.915, 0.813, 0.9892, 0.9805)         # cfb boiler 
  }
  if(type == 'pc'){
    release_rate = c(0.9846, 0.9625, 0.9494, 0.845, 0.995, 0.9622)         # pc boiler   https://www.sciencedirect.com/science/article/pii/S0959652615005417
  }
  if(type == 'sf'){
    release_rate = c(0.7718, 0.3387, 0.4253, 0.3608, 0.8315, 0.8095)       # stoker fired
  }
  
  return(release_rate)
}

# Posttreatment: PM/SO2/NOx/Hg removal rate 
# Reference: Tian et al., 2010, 2011, 2012a, b 
# check the excel: "removal_rate_APCD.xslx" for detailed releasing rate
removal_rate_table = read_excel("apcd_removal_efficiency_extended.xlsx")
# sequence: ESP FF WFGD SCR (1 means exist, 0 means doesnt exist)
removal_rate_cal <- function(depm, desul, denox){
  apcd_setup = c(0, 0, 0, 0) 
  # decide the apcd combination 
  if(!is.na(depm)){
    if(depm == 'ESP'){
      apcd_setup[1] = 1
    }
    if(depm == 'FF'){
      apcd_setup[2] = 1
    }
  }
  if(is.na(depm)){
    apcd_setup[1] = 1
  }
  
  if(!is.na(desul)){
    if(desul == 'wet'){
      apcd_setup[3] = 1
    }
  }
  
  if(!is.na(denox)){
    if(denox == 'SCR/SMCR'){
      apcd_setup[4] = 1
    }
  }
  
  # ... to be added 

  
  removal_rate = c(1, 1, 1, 1, 1, 1) # initial removal rates for As, Pb, Cd, Cr 
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
  return(removal_rate)
}




# calculate the total heavy metal emission (unit:g)
elec_genaration$As_emission = NA
elec_genaration$Pb_emission = NA
elec_genaration$Cd_emission = NA
elec_genaration$Cr_emission = NA
elec_genaration$Hg_emission = NA
elec_genaration$Se_emission = NA

for(i in 1:nrow(elec_genaration)){
  print(i)
  if(!is.na(elec_genaration$coal_consumption[i])){
    # hm orginal amount
    hm_ori_amount = elec_genaration$coal_consumption[i]*hm_concentration(elec_genaration$STATE[i], elec_genaration$coaltype[i])
    
    # pretreatment 
    hm_amount = pre_treatment(hm_ori_amount)
    
    # combustion 
    hm_amount = hm_amount*release_rate(elec_genaration$MW[i], elec_genaration$YEAR[i])
    
    # posttreament 
    hm_amount = hm_amount*removal_rate_cal(elec_genaration$depm[i],elec_genaration$desul[i], elec_genaration$denox[i])
    
    # convert unit ug-> t
    hm_amount = hm_amount/1000000000000
    
    # assign 
    elec_genaration$As_emission[i] = hm_amount[1]
    elec_genaration$Pb_emission[i] = hm_amount[2]
    elec_genaration$Cd_emission[i] = hm_amount[3]
    elec_genaration$Cr_emission[i] = hm_amount[4]
    elec_genaration$Hg_emission[i] = hm_amount[5]
    elec_genaration$Se_emission[i] = hm_amount[6]
  }
}

index_as = which(!is.na(elec_genaration$As_emission))
sum(unlist(elec_genaration$As_emission)[index_as]) # As 
sum(unlist(elec_genaration$Pb_emission)[index_as]) # Pb 
sum(unlist(elec_genaration$Cd_emission)[index_as]) # Cd 
sum(unlist(elec_genaration$Cr_emission)[index_as]) # Cr 


write.xlsx(elec_genaration, "result_hm_emissions0731.xlsx")



