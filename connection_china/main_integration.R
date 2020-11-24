library(stringi)
library(readxl)
library(openxlsx)
library(pinyin)
library(translateR)
library(jiebaR)
library(XML)
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

