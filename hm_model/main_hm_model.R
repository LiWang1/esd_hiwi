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
# smallunits
smallunits = read_excel("smallunits0922.xlsx")
smallunits$STATE[which(smallunits$STATE=='corps')] = 'xinjiang'

# add status 
smallunits$status_2016[smallunits$YEAR>=1990] = 'Operating'
smallunits$status_2016[smallunits$YEAR<1990] = 'Not Operating'
smallunits$status_2016[which(is.na(smallunits$status_2016))] = 'Not Operating'
smallunits_ = smallunits[, -37]

processdata = rbind(processdata, smallunits_)

# connect cec
cec <- read_excel("all_cec.xlsx")
cec = cec[!duplicated(cec[,c('plant_code','unit_name')]),]      # unique the dataframe
processdata = cec_for_hm(cec,processdata)


# connect desul_data 
desul_data = read_excel('desul_devices_0811.xlsx')
processdata = desuldevice_for_hm(desul_data, processdata)
processdata$desul[which(processdata$COMPANY=='smallunits')] = smallunits$desul

# connect denox_data
denox_data = read_excel('denox_devices_0818.xlsx')
processdata = denoxdevice_for_hm(denox_data, processdata)

model <- function(data, scenario){
  # to fill in the NAs based on the scenario
  data$depm = fill_NA_devices(data$status_2016, data$YEAR, data$MW, data$depm, "ESP", scenario)
  data$desul = fill_NA_devices(data$status_2016, data$YEAR, data$MW, data$desul, "WFGD", scenario)
  data$denox = fill_NA_devices(data$status_2016, data$YEAR, data$MW, data$denox, "SCR", scenario)
  
  # coal consumption rate 
  data = regre_coal_rate(data)
  
  # run the model 
  hm_results = hm_model(data)
  
  return(hm_results)
}

hm_results = model(processdata, 'guideline')

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

### task 1: 
# calculate the dist 
processdata$city_lat = as.numeric(processdata$city_lat)
processdata$city_lng = as.numeric(processdata$city_lng)
processdata$distance_m = distance(processdata$lat, processdata$lng, processdata$city_lat, processdata$city_lng)
processdata$distance_m[which(processdata$status_2016=='Not Operating')]=NA
processdata$distance_m[which(processdata$status_2016=='Operating' & processdata$distance_m>100000)] = NA # for the distance too far

# regression analysis: MW ~ distance
# Conclusion: it shows that small units has a larger distance to the city center
index = which(!is.na(processdata$distance_m))
wm_factor <- cut(processdata$MW, breaks=c(0,100,500, 1100), labels=c(1:3))
res<-lm(processdata$distance_m[index]~wm_factor[index])
summary(res)
mean(processdata$distance_m[which(processdata$MW<20 & !is.na(processdata$distance_m))])

### task2: to find if all the small units doesnt overlap
for(j in 1:nrow(smallunits_)){
  if(!is.na(smallunits_$lat[j]) & !is.na(smallunits_$lng[j])){
    dist = abs_dist(smallunits_$lat[j], smallunits_$lng[j], processdata$lat, processdata$lng)
    index_min = which.min(dist)
    if(!is.na(processdata$YEAR[index_min]) & !is.na(processdata$MW[index_min]) & !is.na(smallunits_$YEAR[j]) & !is.na(smallunits_$MW[j])){
      if(processdata$STATE[index_min]==smallunits_$STATE[j] & processdata$YEAR[index_min] == smallunits_$YEAR[j]
         & processdata$MW[index_min] == smallunits_$MW[j]){
        print(j)
      }
    }
  }
}


