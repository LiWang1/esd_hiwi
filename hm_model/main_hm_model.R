library(readxl)
library(openxlsx)
library(data.table)
library(stringi)
library(EnvStats) # triangular distribution 
library(pinyin)
library(translateR)
library(jiebaR)
library(XML)
library(RCurl)
library(rjson)
library(geosphere)
source("functions_hm_model.R")

### data preparation 
setwd("~/Desktop/hiwi_esd/Air pollution project/Own_power_plant_database")
processdata = read_excel("processeddata0818.xlsx")

# connect cec
cec <- read_excel("all_cec.xlsx")
cec = cec[!duplicated(cec[,c('plant_code','unit_name')]),]      # unique the dataframe
processdata = cec_for_hm(cec,processdata)

# connect desul_data 
desul_data = read_excel('desul_devices_0811.xlsx')
processdata = desuldevice_for_hm(desul_data, processdata)
#desul_data = desuldevice_for_hm(desul_data, processdata)
# connect denox_data
denox_data = read_excel('denox_devices_0818.xlsx')
processdata = denoxdevice_for_hm(denox_data, processdata)

# to further fill in the NAs
processdata$depm = fill_NA_devices(processdata$status_2016, processdata$YEAR, processdata$depm, "ESP",'guideline')
processdata$desul = fill_NA_devices(processdata$status_2016, processdata$YEAR, processdata$desul, "WFGD",'guideline')
processdata$denox = fill_NA_devices(processdata$status_2016, processdata$YEAR, processdata$denox, "SCR",'guideline')

# coal consumption rate 
processdata_ = regre_coal_rate(processdata)


### run the model 
hm_results = hm_model(processdata_)

### validate the model: reference data from (2010): https://pubs.acs.org/doi/10.1021/es404730j  & (2017) https://www.sciencedirect.com/science/article/abs/pii/S1309104220300313#bib35
index_as = which(!is.na(hm_results$As_emission))
sum(unlist(hm_results$As_emission)[index_as]) # As  335.45
sum(unlist(hm_results$Pb_emission)[index_as]) # Pb  705.45
sum(unlist(hm_results$Cd_emission)[index_as]) # Cd  13.34
sum(unlist(hm_results$Cr_emission)[index_as]) # Cr  505.03
sum(unlist(hm_results$Hg_emission)[index_as]) # Hg  118.54
sum(unlist(hm_results$Se_emission)[index_as]) # Se  459.4  

index_guizhou = which(hm_results$STATE=='guizhou' & !is.na(hm_results$As_emission))
sum(unlist(hm_results$Cd_emission)[index_guizhou]) # Cd  0.513 

### add wastewater model 
hm_results = hm_wastewater_model(hm_results)
