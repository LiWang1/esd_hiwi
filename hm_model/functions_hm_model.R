### functions used in the heavy metals 

## model to simulate the heavy metals emission for coal power plants 
# Model: hm_emission = E*Ratio*C*R*(1-PM)* (1-SO2)*(1-NOX)*(1-Hg)
# E is the electricity generation (kwh);
# Ratio is the coal consumption (g) per Kwh power generation;
# C is the provincial average concentration of one toxic heavy metal in feed coal (ug/g); 
# R is the average release ratio of one toxic heavy metal in flue gas compared with the element concentration in feed coal from pulverized-coal (PC) boilers, circulating fluidized-bed (CFB) boilers or stoker fired (SF) boilers (%); 
# PM, SO2 NOx and Hg represent the averaged fraction of one heavy metal co- benefit removed from flue gas by the conventional PM/SO2/NOX/Hg emission control devices (%), respectively.
# HTEs emissions in CFPPs differ on the basis of the combustion method, coal type, and coal cleaning technique.
hm_model <- function(elec_genaration){
  
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
    ratio_washing = 0.02 # params, taken from https://www.sciencedirect.com/science/article/pii/S0959652615005417
    removal_rate_washing = c(0.54, 0.363, 0.322, 0.58, 0.5, 0.3)
    
    for(i in 1:length(hm_concentration_ori)){
      hm_concentration_ori[i] = hm_concentration_ori[i]*(1-ratio_washing) + ratio_washing*hm_concentration_ori[i]*(1-removal_rate_washing[i])
    }
    return(hm_concentration_ori)
  } 
  
  # combustion type, either 'pc', 'cfb' or 'sf'
  # check the excel: "release_rate_boiler.xslx" for detailed releasing rate, here in the model, we use an 
  # the function has been built based on the facts: 
  # 1, cfb is only used for boiler of MW between 250-300
  # 2, compared to pc, cfb has a lower emission of SO2 and NOx, desul=dry 
  release_rate <- function(MW, desul){
    # decide the boiler type from MW and year 
    type = NA
    if(is.na(MW) | is.na(desul)){
      type = 'pc'
    }  # default type as pc 
    else{
      if(MW>=250 & MW<=300 & desul=='dry'){
        type = "cfb"
      }
      else{
        type = 'pc'
      }
    }
    
    # decide the release rate according to boiler type
    if(!is.na(type)){
      if(type == 'cfb'){
        release_rate = c(0.756, 0.7733, 0.915, 0.813, 0.9892, 0.9805)         # cfb boiler 
      }
      if(type == 'pc'){
        release_rate = c(0.9846, 0.9625, 0.9494, 0.845, 0.995, 0.9622)         # pc boiler   https://www.sciencedirect.com/science/article/pii/S0959652615005417
      }
      if(type == 'sf'){
        release_rate = c(0.7718, 0.3387, 0.4253, 0.3608, 0.8315, 0.8095)       # stoker fired
      }
    }
    
    return(release_rate)
  }
  

  # Posttreatment: PM/SO2/NOx/Hg removal rate 
  # Reference: Tian et al., 2010, 2011, 2012a, b 
  # check the excel: "removal_rate_APCD.xslx" for detailed releasing rate
  removal_rate_table = read_excel("apcd_removal_efficiency_extended.xlsx")
  removal_rate_table$FGD_dry = 0.8*removal_rate_table$WFGD  # 0.5 is a assumption 
  removal_rate_table$FGD_seawater = removal_rate_table$WFGD
  # dust default as -- ESP 
  index_esp_na = which(!is.na(elec_genaration$coal_consumption) & is.na(elec_genaration$depm))
  elec_genaration$depm[index_esp_na] = 'ESP'
  
  # sequence: ESP FF WFGD SCR (1 means exist, 0 means doesnt exist)
  removal_rate_cal <- function(depm, desul, denox, year){
    apcd_setup = c(0, 0, 0, 0, 0, 0, 0, 0) 
    # decide the apcd combination 
    
    if(!is.na(depm) & !is.na(desul) & !is.na(denox)) {
      if(depm == 'ESP'){
        apcd_setup[1] = 1
      }
      if(depm == 'FF'){
        apcd_setup[2] = 1
      }
      if(desul == 'wet' || desul == 'WFGD'){
        apcd_setup[3] = 1
      }
      if(denox == 'SCR/SMCR' || denox == 'SCR' || denox == 'SNCR+SCR'){
        apcd_setup[4] = 1
      }
      if(desul == 'dry' || desul == 'sea' || desul == 'ammonia' || desul == 'others'|| desul == 'sea water'){
        apcd_setup[5] = 1
      }
      if(depm == 'ESP1/2'){
        apcd_setup[6] = 1
      }
      if(desul == 'WFGD1/2'){
        apcd_setup[7] = 1
      }
      if(denox == 'SCR1/2'){
        apcd_setup[8] = 1
      }
      
    }
    else{
      if(is.na(year)){
        apcd_setup = c(1, 0, 1, 1, 0, 0, 0, 0) 
      }
      if(!is.na(year) & year>=2012){
        apcd_setup = c(1, 0, 1, 1, 0, 0, 0, 0) 
      }
      if(!is.na(year) & year<2012){
        apcd_setup = c(0, 0, 0, 0, 0, 1, 1, 1) 
      }
    }
    
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
    if(apcd_setup[5] == 1){
      removal_rate = removal_rate*(1-removal_rate_table$FGD_dry/100)
    }
    if(apcd_setup[6] == 1){
      removal_rate = removal_rate*(1-removal_rate_table$`ESP1/2`/100)
    }
    if(apcd_setup[7] == 1){
      removal_rate = removal_rate*(1-removal_rate_table$`WFGD1/2`/100)
    }
    if(apcd_setup[8] == 1){
      removal_rate = removal_rate*(1-removal_rate_table$`SCR1/2`/100)
    }
    return(removal_rate)
  }
  
  # calculate the distributions among different phases
  # e.g., 
  # fly ash: 80% 
  # bottom ash: 15% 
  # if wet FGD: gypsum: 4% 
  phase_distribution <- function(amount){
    # e.g., 
    ratio_fly =    c(  .8,  .8,  .8,  .8,  .8, .8)
    ratio_bottom = c( .15, .15, .15, .15, .15, .15)
    ratio_waste =  c( .04, .04, .04, .04, .04, .04)
    
    amount_fly = amount*ratio_fly
    amount_bottom = amount*ratio_bottom
    amount_waste = amount*ratio_waste
    
    amount_matrix = as.data.frame(cbind(amount_fly,amount_bottom, amount_waste))
    
    return(amount_matrix)
  }
  
  # calculate the total heavy metal emission (unit:g)
  elec_genaration$As_emission = NA
  elec_genaration$Pb_emission = NA
  elec_genaration$Cd_emission = NA
  elec_genaration$Cr_emission = NA
  elec_genaration$Hg_emission = NA
  elec_genaration$Se_emission = NA
  for(i in 1:nrow(elec_genaration)){
    #print(i)
    if(!is.na(elec_genaration$coal_consumption[i])){
      # hm orginal amount
      hm_ori_amount = elec_genaration$coal_consumption[i]*hm_concentration(elec_genaration$STATE[i], elec_genaration$coaltype[i])
      #print(hm_ori_amount)
      # pretreatment 
      hm_amount = pre_treatment(hm_ori_amount)
      
      # heavy metals in fly ash, bottom ash and waste 
      distr_matrix = phase_distribution(hm_amount)/1000000000000
      fly_ash_amount = distr_matrix$amount_fly
      bottom_ash_amount = distr_matrix$amount_bottom
      waste_amount = distr_matrix$amount_waste
      
      # combustion 
      hm_amount = hm_amount*release_rate(elec_genaration$MW[i],  elec_genaration$desul[i])
      
      # posttreament 
      hm_amount = hm_amount*removal_rate_cal(elec_genaration$depm[i],elec_genaration$desul[i], elec_genaration$denox[i], elec_genaration$YEAR[i])
      
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
  
  return(elec_genaration)
}


## connect cec data  
cec_for_hm <- function(cec, elec_genaration){
  elec_genaration$coal_consumption_rate = NA
  elec_genaration$coaltype_cec = NA
  elec_genaration$depm = NA
  elec_genaration$desul = NA
  elec_genaration$denox = NA
  
  
  # dust removal 
  index_ff = which(stri_detect_fixed(cec$depm, '袋'))
  index_esp = which(!stri_detect_fixed(cec$depm, '袋'))
  cec$depm[index_ff] = 'FF'
  cec$depm[index_esp] = 'ESP'
  
  # nox removal  SCR，SMCR
  index_scr_smcr = which(stri_detect_fixed(cec$denox, 'SCR') | stri_detect_fixed(cec$denox, 'SMCR'))
  index_none = which(stri_detect_fixed(cec$denox, '无脱硝'))
  index_others =  which(!stri_detect_fixed(cec$denox, 'SCR') & !stri_detect_fixed(cec$denox, 'SMCR') &
                          !stri_detect_fixed(cec$denox, '无脱硝'))
  cec$denox[index_scr_smcr] = 'SCR'
  cec$denox[index_none] = 'none'
  cec$denox[index_others] = 'others'
  
  # s removal 
  index_wet = which(stri_detect_fixed(cec$desul, '湿'))
  index_dry = which(stri_detect_fixed(cec$desul, '干'))
  index_sea = which(stri_detect_fixed(cec$desul, '海水'))
  index_others = which(!stri_detect_fixed(cec$desul, '湿') & !stri_detect_fixed(cec$desul, '干') & !stri_detect_fixed(cec$desul, '海水'))
  
  cec$desul[index_wet] = 'WFGD'
  cec$desul[index_dry] = 'dry'
  cec$desul[index_sea] = 'sea water'
  cec$desul[index_others] = 'others'
  
  
  
  for( i in 1:nrow(elec_genaration)){
    if(!is.na(elec_genaration$cec_plant_code_test[i])){
      # assume one factory uses on type of coal
      index = which(cec$plant_code == elec_genaration$cec_plant_code_test[i])
      print(index)
      if(length(index)>=1){
        index_ = index[1]
        elec_genaration$coaltype_cec[i] = cec$coaltype_en[index_]
        elec_genaration$depm[i] = cec$depm[index_]
        elec_genaration$desul[i] = cec$desul[index_]
        elec_genaration$denox[i] = cec$denox[index_]
      }
    }
    
    if(!is.na(cec$coal_consumption[i])){
      index = which(elec_genaration$cec_plant_code_test==cec$plant_code[i] &
                      elec_genaration$cec_unit_code_test == cec$unit_name[i])
      elec_genaration$coal_consumption_rate[index] = cec$coal_consumption[i]
    }
  }
  
  elec_genaration$coaltype = NA
  index = which(is.na(elec_genaration$coaltype))
  elec_genaration$coaltype[index] = elec_genaration$coaltype_cec[index]
  index = which(is.na(elec_genaration$coaltype))
  elec_genaration$coaltype[index] = elec_genaration$coaltype_wiki[index]
  return(elec_genaration)
}


## keyword extraction 
source("keyword_extraction.R")


## connect desul device data 
desuldevice_for_hm <- function(desul_data, datainput_hm_models){
  index_nona = which(!is.na(desul_data$seq))
  desul_data = desul_data[index_nona, ]
  
  datainput_hm_models$PLANT =  gsub("hwasu", "huasu", datainput_hm_models$PLANT )     
  datainput_hm_models$CITY = tolower(datainput_hm_models$CITY)
  datainput_hm_models$city_plant = paste(datainput_hm_models$PLANT, datainput_hm_models$CITY)
  
  datainput_hm_models$desul_devices = NA
  count = 0 
  desul_data$check = NA
  for(i in 1:nrow(desul_data)){
    print(i)
    #i = 3287
    #for(i in 1:50){
    keyword = keyword_extract(desul_data$company_name[i])
    keyword_company = keyword$company_info[1]
    keyword_key = keyword$keyword_info
    keyword_num = keyword$num_info
    keyword_state = desul_data$state[i]
    keyword_year = desul_data$year[i]
    keyword_WM = desul_data$MW[i]
    keyword_unit = desul_data$unit[i]
    
    # keyword
    index_key = 1:nrow(datainput_hm_models)
    if(is.na(keyword_key)){
      index_key = integer(0)
    } # there must be key information
    if(!is.na(keyword_key)){
      for(j in 1:length(keyword_key)){
        pattern = pinyin_trans(keyword_key[j])
        index_match = which(stri_detect_fixed(datainput_hm_models$city_plant,pattern))
        index_key = intersect(index_match, index_key)
      }
    }
    
    #company 
    index_company = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_company)){
      index_comany = which(stri_detect_fixed(datainput_hm_models$COMPANY,keyword_company))
    }
    index_com_plant = intersect(index_key, index_company)
    
    #num
    index_num = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_num)){
      index_num = which(stri_detect_fixed(datainput_hm_models$PLANT,keyword_num))
    }
    
    # unit_num
    index_unit = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_unit)){
      index_unit = which(stri_detect_fixed(datainput_hm_models$UNIT,keyword_unit))
    }
    
    #year & WM & state
    index_state = which(datainput_hm_models$STATE ==keyword_state)
    index_year = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_year)){
      index_year = which(abs(datainput_hm_models$YEAR-keyword_year)<=1)
    }
    if(!is.na(keyword_year)){
      index_year_strict = which(abs(datainput_hm_models$YEAR-keyword_year)<1)
    }
    
    index_WM = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_WM)){
      index_WM = which(abs(datainput_hm_models$MW-keyword_WM)<=50)
    }
    index_year_MW_state = intersect(intersect(index_year, index_WM), index_state)
    
    
    index_trial0 = intersect(intersect(index_key, index_year_strict), index_WM)
    if(length(index_trial0)==1){
      datainput_hm_models$desul_devices[index_trial1]=desul_data$desul_category[i]
      count = count +1
      desul_data$check[i] =1 
    }
    
    index_trial1 = intersect(index_com_plant,index_year_MW_state)
    if(length(index_trial1)>=1){
      datainput_hm_models$desul_devices[index_trial1]=desul_data$desul_category[i]
      count = count +1
      desul_data$check[i] =1 
    }
    
  }
  
  index_na_desul = which(!is.na(datainput_hm_models$desul_devices))
  datainput_hm_models$desul[index_na_desul] = datainput_hm_models$desul_devices[index_na_desul]
  
  
  # assume one plant use the same desul device 
  plant_data = datainput_hm_models[which(!is.na(datainput_hm_models$desul) & datainput_hm_models$status_2016=='Operating'),]
  plant_data = plant_data[!duplicated(plant_data[,c('PLANT','COMPANY')]),] 
  for(j in 1:nrow(datainput_hm_models)){
    if(is.na(datainput_hm_models$desul[j]) & datainput_hm_models$status_2016[j]=='Operating'){
      index = which(datainput_hm_models$PLANT[j]==plant_data$PLANT & datainput_hm_models$COMPANY[j]==plant_data$COMPANY)
      if(length(index)==1){
        datainput_hm_models$desul[j] = plant_data$desul[index]
      }
    } 
  }
  
  # check how many companies get connected 
  index_non_connect = which(is.na(desul_data$check))
  unique(desul_data$company_name[index_non_connect])
  return(datainput_hm_models)
  #return(desul_data)
}


## connect denox device data 
denoxdevice_for_hm <- function(denox_data, datainput_hm_models){
  datainput_hm_models$denox_devices = NA
  count = 0 
  denox_data$check = NA
  for(i in 1:nrow(denox_data)){
    #for(i in 1:50){
    print(i)
    #i = 3287
    
    keyword = keyword_extract(denox_data$company_name[i])
    keyword_company = keyword$company_info[1]
    keyword_key = keyword$keyword_info
    keyword_num = keyword$num_info
    keyword_state = denox_data$state[i]
    keyword_year = denox_data$year[i]
    keyword_WM = denox_data$MW[i]
    
    # keyword
    index_key = 1:nrow(datainput_hm_models)
    if(is.na(keyword_key)){
      index_key = integer(0)
    } # there must be key information
    if(!is.na(keyword_key)){
      for(j in 1:length(keyword_key)){
        pattern = pinyin_trans(keyword_key[j])
        index_match = which(stri_detect_fixed(datainput_hm_models$city_plant,pattern))
        index_key = intersect(index_match, index_key)
      }
    }
    
    #company 
    index_company = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_company)){
      index_comany = which(stri_detect_fixed(datainput_hm_models$COMPANY,keyword_company))
    }
    index_com_plant = intersect(index_key, index_company)
    
    #num
    index_num = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_num)){
      index_num = which(stri_detect_fixed(datainput_hm_models$PLANT,keyword_num))
    }
    
    
    
    #year & WM & state
    index_state = which(datainput_hm_models$STATE ==keyword_state)
    index_year = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_year)){
      index_year = which(abs(datainput_hm_models$YEAR-keyword_year)<=1)
    }
    if(!is.na(keyword_year)){
      index_year_strict = which(abs(datainput_hm_models$YEAR-keyword_year)<1)
    }
    
    index_WM = 1:nrow(datainput_hm_models)
    if(!is.na(keyword_WM)){
      index_WM = which(abs(datainput_hm_models$MW-keyword_WM)<=50)
    }
    index_year_MW_state = intersect(intersect(index_year, index_WM), index_state)
    
    
    index_trial0 = intersect(intersect(index_key, index_year_strict), index_WM)
    if(length(index_trial0)==1){
      datainput_hm_models$denox_devices[index_trial1]=denox_data$denox[i]
      count = count +1
      denox_data$check[i] =1 
    }
    
    index_trial1 = intersect(index_com_plant,index_year_MW_state)
    if(length(index_trial1)>=1){
      datainput_hm_models$denox_devices[index_trial1]=denox_data$denox[i]
      count = count +1
      denox_data$check[i] =1 
    }
    
  }
  
  index_na_denox = which(!is.na(datainput_hm_models$denox_devices))
  datainput_hm_models$denox[index_na_denox] = datainput_hm_models$denox_devices[index_na_denox]
  
  length( which(is.na(datainput_hm_models$denox)& datainput_hm_models$status_2016=='Operating'))
  
  # assume one plant use the same denox device 
  plant_data = datainput_hm_models[which(!is.na(datainput_hm_models$denox) & datainput_hm_models$status_2016=='Operating'),]
  plant_data = plant_data[!duplicated(plant_data[,c('PLANT','COMPANY')]),] 
  for(j in 1:nrow(datainput_hm_models)){
    if(is.na(datainput_hm_models$denox[j]) & datainput_hm_models$status_2016[j]=='Operating'){
      index = which(datainput_hm_models$PLANT[j]==plant_data$PLANT & datainput_hm_models$COMPANY[j]==plant_data$COMPANY)
      if(length(index)==1){
        datainput_hm_models$denox[j] = plant_data$denox[index]
      }
    } 
  }
  
  # check how many companies get connected 
  index_non_connect = which(is.na(denox_data$check))
  unique(denox_data$company_name[index_non_connect])
  return(datainput_hm_models)
}


## coal consumption rate regression model
regre_coal_rate <- function(elec_genaration){
  index_cum_ratio = which(!is.na(elec_genaration$coal_consumption_rate) &!is.na(elec_genaration$eletricity_generation))
  index_predict = which(is.na(elec_genaration$coal_consumption_rate) & !is.na(elec_genaration$eletricity_generation))
  
  coal_consupmtion_rate = elec_genaration$coal_consumption_rate
  # capacity factor 
  wm_factor <- cut(elec_genaration$MW, breaks=c(0,600,1100), labels=c(1:2))
  wm_factor
  
  # year factor 
  year_factor <- cut(elec_genaration$YEAR, breaks=c(0,2000,2012,2020), labels=c(1:3))
  year_factor
  
  # location factor 
  location_factor = factor(elec_genaration$STATE) 
  summary(res<-lm(elec_genaration$coal_consumption_rate[index_cum_ratio]~location_factor[index_cum_ratio]))
  # inner mongolia\ningxia\shanxi\xinjiang appears to consume more 
  loc_factor <- function(state){
    index_state = which(state %in% c('inner mongolia', 'ningxia', 'shanxi', 'xinjiang'))
    state[1:length(state)] = 'betterstate'
    state[index_state] = 'worsestate'
    return(state)
  }
  location_factor = loc_factor(elec_genaration$STATE)
  
  # data 
  df = data.frame(coal_consupmtion_rate, wm_factor, year_factor, location_factor)
  
  # train
  res<-lm(coal_consupmtion_rate~wm_factor+year_factor+location_factor, data =df[index_cum_ratio, ])
  print(summary(res))
  
  # predict 
  pred = predict(res, newdata = df[index_predict, ])
  na_pred = which(is.na(pred))
  pred[na_pred] = 312 
  
  elec_genaration$coal_consumption_rate[index_predict] = pred
  return(elec_genaration)
}


## for NAs of APCDs
fill_NA_devices <- function(status, year, WM, device, cat, scenario){
  index_below = which(status=="Operating" & !is.na(year) & year<2012 & is.na(device))
  index_above =  which(status=="Operating" & !is.na(year) & year>=2012 & is.na(device))
  index_na =  which(status=="Operating" & is.na(year) & is.na(device))
  
  index_smallunits = which(status=="Operating" & !is.na(WM) & WM<100 & is.na(device))
  index_bigunits =  which(status=="Operating" & !is.na(WM) & WM<100 & is.na(device))
  index_wm_na =  which(status=="Operating" & is.na(WM) & is.na(device))
  
  if(scenario == 'guideline'){
    device[index_above] = cat
    device[index_na] = cat
    device[index_below] =paste(cat, "1/2", sep="")
  }
  if(scenario == 'optimistic'){
    device[index_above] = cat
    device[index_na] = cat
    device[index_below] = cat
  }
  if(scenario == 'pessimistic'){
    device[index_above] = paste(cat, "1/2", sep="")
    device[index_na] = paste(cat, "1/2", sep="")
    device[index_below] = paste(cat, "1/2", sep="")
  }
  
  if(scenario == 'smallunits' ){
    device[index_bigunits] = cat
    device[index_wm_na] = 'none'
    device[index_smallunits] = 'none'
  }
  return(device)
}

## wastewater model 
# assumptions made: 
# 1) the hm removal efficiency of pre-treatment for WFGD 
# 2) whether there is post treatments for certain units (the result is randomly sampled)
# pre-treatment: neutralization - flocculation - precipitation 
# post-treatment: 2.1) to atomize the high salinity wastewater and direct them into the flue gas 
# channel, the water get vaporated and the heavy metals can be captured by the dust removal device; 
# 2.2) To make use of the heat in the plant to evaporate the water and the crystals are kept and treated.
# 3) the hm removal efficiency of pre-treatment for sea water FGD
# 4) the sea water FGD doesn't have any other post treatment
# 5) for other FGD methods, they don't produce wastewater

hm_wastewater_model <- function(hm_data){
  # calculate the ratios between the amount in the wastewater and that in the flue gas
  removal_rate_table = read_excel("apcd_removal_efficiency_extended.xlsx")
  removal_rate_table$FGD_dry = 0.8*removal_rate_table$WFGD  # 0.5 is a assumption 
  removal_rate_table$FGD_seawater = removal_rate_table$WFGD
  ratio_wfgd = removal_rate_table$WFGD/(100-removal_rate_table$WFGD)
  ratio_sea = removal_rate_table$FGD_seawater/(100-removal_rate_table$FGD_seawater)
  
  # initiate 
  hm_data$ww_kg = NA 
  hm_data$post_treatment = NA 
  
  hm_data$As_ww_cc = NA 
  hm_data$Pb_ww_cc = NA 
  hm_data$Cd_ww_cc = NA
  hm_data$Cr_ww_cc = NA 
  hm_data$Hg_ww_cc = NA 
  hm_data$Se_ww_cc = NA 
  
  hm_data$As_ww_out = NA
  hm_data$Pb_ww_out = NA 
  hm_data$Cd_ww_out = NA
  hm_data$Cr_ww_out = NA 
  hm_data$Hg_ww_out = NA 
  hm_data$Se_ww_out = NA 
  
  
  for(i in 1:nrow(hm_data)){
    # WFGD 
    if((hm_data$desul[i] == 'WFGD' || hm_data$desul[i] == 'WFGD1/2') & hm_data$status_2016[i]=="Operating"){
      # amount of wastewater 15-20kg/Kwh, only for units with WFGD
      hm_data$ww_kg[i] = hm_data$elec_gene_kwh[i]*17.5
      
      # pre-treatment
      # t-- mg
      hm_data$As_ww_cc[i] = ratio_wfgd[1]*unlist(hm_data$As_emission[i])*1000000000
      hm_data$Pb_ww_cc[i] = ratio_wfgd[2]*unlist(hm_data$Pb_emission[i])*1000000000
      hm_data$Cd_ww_cc[i] = ratio_wfgd[3]*unlist(hm_data$Cd_emission[i])*1000000000
      hm_data$Cr_ww_cc[i] = ratio_wfgd[4]*unlist(hm_data$Cr_emission[i])*1000000000
      hm_data$Hg_ww_cc[i] = ratio_wfgd[5]*unlist(hm_data$Hg_emission[i])*1000000000
      hm_data$Se_ww_cc[i] = ratio_wfgd[6]*unlist(hm_data$Se_emission[i])*1000000000
      
      # 3) triple tank: neutralization -- flocculation -- precipitation, assume 70% removal rate
      removal_efficiency = 0.95
      hm_data$As_ww_out[i] = hm_data$As_ww_cc[i]*(1-removal_efficiency)
      hm_data$Pb_ww_out[i] = hm_data$Pb_ww_cc[i]*(1-removal_efficiency)
      hm_data$Cd_ww_out[i] = hm_data$Cd_ww_cc[i]*(1-removal_efficiency)
      hm_data$Cr_ww_out[i] = hm_data$Cr_ww_cc[i]*(1-removal_efficiency)
      hm_data$Hg_ww_out[i] = hm_data$Hg_ww_cc[i]*(1-removal_efficiency)
      hm_data$Se_ww_out[i] = hm_data$Se_ww_cc[i]*(1-removal_efficiency)
      
      # 4) post-treatment
      hm_data$post_treatment[i] = sample(c(0, 1), 1, replace = FALSE, prob = c(0.2, 0.8))
      if(hm_data$post_treatment[i]==1){
        hm_data$As_ww_out[i] = 0
        hm_data$Pb_ww_out[i] = 0
        hm_data$Cd_ww_out[i] = 0
        hm_data$Cr_ww_out[i] = 0
        hm_data$Hg_ww_out[i] = 0
        hm_data$Se_ww_out[i] = 0
      }
    }
    
    # seawater
    if(hm_data$desul[i] == 'sea' & hm_data$status_2016[i]=="Operating"){
      # amount of wastewater 15-20kg/Kwh, only for units with WFGD
      hm_data$ww_kg[i] = hm_data$elec_gene_kwh[i]*20
      
      # pre-treatment
      # t-- mg
      hm_data$As_ww_out[i] = ratio_sea[1]*unlist(hm_data$As_emission[i])*1000000000
      hm_data$Pb_ww_out[i] = ratio_sea[2]*unlist(hm_data$Pb_emission[i])*1000000000
      hm_data$Cd_ww_out[i] = ratio_sea[3]*unlist(hm_data$Cd_emission[i])*1000000000
      hm_data$Cr_ww_out[i] = ratio_sea[4]*unlist(hm_data$Cr_emission[i])*1000000000
      hm_data$Hg_ww_out[i] = ratio_sea[5]*unlist(hm_data$Hg_emission[i])*1000000000
      hm_data$Se_ww_out[i] = ratio_sea[6]*unlist(hm_data$Se_emission[i])*1000000000
    }
    
    # else 
    if(hm_data$desul[i] != 'sea' & hm_data$desul[i] != 'WFGD' & hm_data$desul[i] != 'WFGD1/2' & hm_data$status_2016[i]=="Operating"){
      hm_data$As_ww_out[i] = 0
      hm_data$Pb_ww_out[i] = 0
      hm_data$Cd_ww_out[i] = 0
      hm_data$Cr_ww_out[i] = 0
      hm_data$Hg_ww_out[i] = 0
      hm_data$Se_ww_out[i] = 0
    }
  }
  return(hm_data)
}

# to calculate the absolute difference of two point coordinates value.  
abs_dist <- function(lat, lng, lat_vec, lng_vec){
  lat_vec[which(is.na(lat_vec))] = 0 
  lng_vec[which(is.na(lng_vec))] = 0 
  
  dist = rep(NA, length(lat_vec))
  for(i in 1:length(lat_vec)){
    dist[i] = abs(lat_vec[i] - lat) + abs(lng_vec[i]-lng)
  }
  return(dist)
}

# to calculate the geographical distance between two points 
distance <- function(lat1, lng1, lat2, lng2){
  dist = rep(NA, length(lat1))
  for(i in 1:length(lat1)){
    #print(i)
    if(!is.na(lat1[i]) & !is.na(lng1[i]) & !is.na(lat2[i]) & !is.na(lng2[i])){
      dist[i]=distm(c(lng1[i], lat1[i]), c(lng2[i], lat2[i]), fun = distHaversine)
    }
  }
  return(dist)
} 
