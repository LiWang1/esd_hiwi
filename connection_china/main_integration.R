library(stringi)
library(readxl)
library(openxlsx)
library(pinyin)
library(translateR)
library(jiebaR)
library(XML)
library(sp)
library(RCurl)
library(rjson)
library(geosphere) 
source("function_integration.R")
Sys.setlocale(category="LC_ALL", locale = "en_US.UTF-8")  # display chinese properly 

### preprocess "pp_CN_Mar_2020.csv" database
plant_data = read.csv("pp_CN_Mar_2020.csv", sep=";", stringsAsFactors = FALSE)
plant_data = preprocess(plant_data)

### connect to cec
cec <- read_excel("all_cec.xlsx")
plant_data = cec_connect(cec, plant_data)

### conncet to yearbook
yearbook <- read_excel("yearbook_2017.xlsx")
plant_data = yearbook_connect(yearbook, plant_data)

### connect to wiki 
wiki <- read_excel("wiki_info_.xlsx")
plant_data = wiki_connect(wiki, plant_data)
# Note: "coodinates_plants.xlsx" also have some coordinates but partially overlapped
coordinates <- read_excel("coodinates_plants.xlsx")
plant_data = coordinate_connect(coordinates, plant_data)


### integration of different data
## using hours ---------------------------------------------------------------------------
plant_data$using_hours_2016 = NA
# cec 
plant_data$using_hours_2016[which(plant_data$cec_using_hours>0)] = plant_data$cec_using_hours[which(plant_data$cec_using_hours>0)]
# yearbook 
yearbook_usinghours = unique(plant_data$yearbook_plant_code[which(!is.na(plant_data$yearbook_plant_code))])
plant_data$cec_using_hours[which(is.na(plant_data$cec_using_hours))]=0
for(i in yearbook_usinghours){
  index = which(plant_data$yearbook_plant_code == i)
  index
  total_hours = sum(as.numeric(plant_data$yearbook_using_hours[index]))
  #
  cec_hours = sum(plant_data$cec_using_hours[index])
  num = length(which(plant_data$cec_using_hours[index]==0))
  rest_ave = (total_hours - cec_hours)/num
  #
  index_na = which(is.na(plant_data$using_hours_2016))
  index_assign=intersect(index,index_na)
  plant_data$using_hours_2016[index_assign] = rest_ave
}

# others 
# if MW >= 1000   4672hrs 
plant_data$using_hours_2016[which(is.na(plant_data$using_hours_2016) & plant_data$MW>=1000)] = 4672
# if 600 =< MW < 1000 4172hrs
plant_data$using_hours_2016[which(is.na(plant_data$using_hours_2016) & plant_data$MW>=600 & plant_data$MW<1000)] = 4172
# if 300 =< MW < 600 3876hrs
plant_data$using_hours_2016[which(is.na(plant_data$using_hours_2016) & plant_data$MW>=300 & plant_data$MW<600)] = 3876
# if 100 =< MW < 300 3759hrs
plant_data$using_hours_2016[which(is.na(plant_data$using_hours_2016) & plant_data$MW>=100 & plant_data$MW<300)] = 3759
# if MW < 100 4275 hrs
plant_data$using_hours_2016[which(is.na(plant_data$using_hours_2016) & plant_data$MW<100)] = 4275
# remove erroreous using hours, assign the rest to be 4275 hous (annual report)
plant_data$using_hours_2016[which(abs(plant_data$using_hours_2016)>8670)]=4275

## calculate the electricity generation with using hours
plant_data$elec_gene_kwh = plant_data$using_hours_2016*plant_data$MW*1000 # unit: KWh
# correct several units with sgcc elec generation data
index_sgcc = which(!is.na(plant_data$sgcc))
plant_data$elec_gene_kwh[index_sgcc] = plant_data$sgcc[index_sgcc]


## assign operation status of the units -----------------------------------------------------------
index = which(!is.na(plant_data$yearbook_plant_code) | (!is.na(plant_data$cec_plant_code_test)) 
              | (!is.na(plant_data$CEC_PLANT_CODE)) | grepl('Operating', plant_data$plant_status_wiki, fixed = TRUE))
plant_data$status_2016=NA
plant_data$status_2016[index] = 'Operating'
plant_data$status_2016[which(is.na(plant_data$status_2016))] = 'Not Operating'

# check if the electricity generation is closed to the real one
sum(plant_data$elec_gene_kwh[index])/1000000000000  # 3.906 is the number 

# correction for the small units
index_small_units_1 = which(plant_data$MW <100 & plant_data$status_2016 == "Not Operating" 
                            & plant_data$YEAR>=2000 & plant_data$YEAR<=2016 & is.na(plant_data$wiki_code))
plant_data$status_2016[index_small_units_1] = 'Operating'

# correct for old big units 
index_oldbig_units_notoperating = which(plant_data$MW >100 & plant_data$status_2016 == "Operating" & 
                                          plant_data$YEAR<1995 & is.na(plant_data$CEC_UNIT_CODE) & 
                                          is.na(plant_data$cec_unit_code_test) & is.na(plant_data$wiki_code))
plant_data$status_2016[index_oldbig_units_notoperating] = "Not Operating"








