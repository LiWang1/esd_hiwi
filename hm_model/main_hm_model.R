setwd("~/Desktop/hiwi_esd/Air pollution project/Own_power_plant_database")

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
library(maps)
source("functions_hm_model.R")


 
### data preparation 
processdata = read_excel("processeddata0818.xlsx")
# smallunits
#smallunits = read_excel("smallunits0922.xlsx")
smallunits = read_excel("smallunits1019.xlsx")
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

### to add population density data
pop_ds = read.csv("gpw_v4_admin_unit_center_points_population_estimates_rev11_chn.csv")
processdata = addpopulationdensity(processdata, pop_ds) 
# ? we need a default value here for NAs 

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


### LCIA
hm_results$As_emission = unlist(hm_results$As_emission)
hm_results$Cd_emission = unlist(hm_results$Cd_emission)
hm_results$Cr_emission = unlist(hm_results$Cr_emission)
hm_results$Hg_emission = unlist(hm_results$Hg_emission)
hm_results$Pb_emission = unlist(hm_results$Pb_emission)
hm_results$Se_emission = unlist(hm_results$Se_emission)

hm_results$As_impact = NA
hm_results$Cd_impact = NA
hm_results$Cr_impact = NA
hm_results$Hg_impact = NA
hm_results$Pb_impact = NA
hm_results$Se_impact = NA

# add influence
for(i in 1:nrow(hm_results)){
#for(i in 1:10){
  if(hm_results$status_2016[i]=="Operating" & !is.na(hm_results$As_emission[i])){
    #print(i)
    # calculate the characterization factor
    charac_factors = cf_cal(hm_results$STATE[i], hm_results$pop_density[i])
    # calculate the impact (t-kg)
    impact = 1000*lcia(hm_results$As_emission[i], hm_results$Cd_emission[i], hm_results$Cr_emission[i], hm_results$Hg_emission[i], hm_results$Pb_emission[i], 
                       hm_results$Se_emission[i], charac_factors) # Emission to urban air(total)
    
    hm_results$As_impact[i]= sum(impact[1:2])
    hm_results$Cd_impact[i] = impact[3]
    hm_results$Cr_impact[i] = sum(impact[4:5])
    hm_results$Hg_impact[i] = impact[6]
    hm_results$Pb_impact[i] = impact[7]
    hm_results$Se_impact[i] = impact[8]
  }
}
# total impact  
sum(hm_results$Cr_impact[which(!is.na(hm_results$Cr_impact))])
sum(hm_results$As_impact[which(!is.na(hm_results$As_impact))])
sum(hm_results$Cd_impact[which(!is.na(hm_results$Cd_impact))])
sum(hm_results$Se_impact[which(!is.na(hm_results$Cd_impact))])
sum(hm_results$Pb_impact[which(!is.na(hm_results$Cd_impact))])
sum(hm_results$Hg_impact[which(!is.na(hm_results$Hg_impact))])


### correlation analysis 
## dist ~ mw 
# calculate the dist 
processdata$city_lat = as.numeric(processdata$city_lat)
processdata$city_lng = as.numeric(processdata$city_lng)
processdata$distance_m = distance(processdata$lat, processdata$lng, processdata$city_lat, processdata$city_lng)
processdata$distance_m[which(processdata$status_2016=='Not Operating')]=NA
processdata$distance_m[which(processdata$status_2016=='Operating' & processdata$distance_m>100000)] = NA # for the distance too far

# regression analysis: MW ~ distance (Conclusion: it shows that small units has a larger distance to the city center)
index = which(!is.na(processdata$distance_m) & processdata$distance_m<50000)
wm_factor <- cut(processdata$MW, breaks=c(0, 100, 500, 1100), labels=c(1:3))
res_dist<-lm(processdata$distance_m[index]~wm_factor[index])
summary(res_dist)

par(mfrow=c(2, 2))
mean(processdata$distance_m[which(processdata$MW<=100 & !is.na(processdata$distance_m) )])
hist(processdata$distance_m[which(processdata$MW<=50 & !is.na(processdata$distance_m))], breaks = 50)

mean(processdata$distance_m[which(processdata$MW>100 &processdata$MW<=500 & !is.na(processdata$distance_m) )])
hist(processdata$distance_m[which(processdata$MW>100 &processdata$MW<=500 & !is.na(processdata$distance_m) )], breaks = 50)

mean(processdata$distance_m[which(processdata$MW>500 &processdata$MW<=1100 & !is.na(processdata$distance_m) )])
hist(processdata$distance_m[which(processdata$MW>500 &processdata$MW<=1100 & !is.na(processdata$distance_m) )], breaks = 50)

## pop_ds ~ mw or year
index_mw = which(!is.na(processdata$pop_density))
index_year = which(!is.na(processdata$pop_density) & !is.na(processdata$YEAR))

# mw
mw_factor <- cut(processdata$MW, breaks=c(0, 100, 300, 600, 1100), labels=c(1:4))
mw_factor
res_mw<-lm(processdata$pop_density[index_mw]~mw_factor[index_mw])
summary(res_mw)
hist(processdata$pop_density[which(processdata$MW<=100 & !is.na(processdata$pop_density))], breaks = 50, main=paste('pop_density MW<100'))
hist(processdata$pop_density[which(processdata$MW>100 &processdata$MW<=300 & !is.na(processdata$pop_density))], breaks = 50,  main=paste('pop_density 100<MW<300'))
hist(processdata$pop_density[which(processdata$MW>300 &processdata$MW<=600 & !is.na(processdata$pop_density))], breaks = 50,  main=paste('pop_density 300<MW<600'))
hist(processdata$pop_density[which(processdata$MW>600 &processdata$MW<=1100 & !is.na(processdata$pop_density))], breaks = 50,  main=paste('pop_density 600<MW<1100'))

# year 
year_factor <- cut(processdata$YEAR, breaks=c(0,2000, 2012, 2020), labels=c(1:3))
year_factor
res_year <-lm(processdata$pop_density[index_year]~year_factor[index_year])
summary(res_year)
hist(processdata$pop_density[which(processdata$YEAR<=2000 & !is.na(processdata$pop_density))], breaks = 50, main=paste('pop_density year<2000'))
hist(processdata$pop_density[which(processdata$YEAR>2000 &processdata$YEAR<=2012 & !is.na(processdata$pop_density))], breaks = 50,  main=paste('pop_density 2000<year<2012'))
hist(processdata$pop_density[which(processdata$YEAR>2012 &processdata$YEAR<=2020 & !is.na(processdata$pop_density))], breaks = 50,  main=paste('pop_density MW>2012'))


### to find if all the small units doesnt overlap
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
} # the overlapped ones have been removed
par(mfrow=c(1, 1))
### to visualize the locations of coal power plant units 
map("world", regions="China")
index = which(!is.na(processdata$lat) & !is.na(processdata$lng)) 
lat = processdata$lat[index]
lng = processdata$lng[index]
points(x = lng, y = lat, col = "red")

