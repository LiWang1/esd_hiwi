## 1）read the data 
library(readxl)
cec <- read_excel("all_cec.xlsx")
cec = cec[!duplicated(cec[,c('plant_code','unit_name')]),]      # unique the dataframe

cec$unit_name = as.numeric(cec$unit_name)
cec$unit_name[which(cec$unit_name>10)] = cec$unit_name[which(cec$unit_name>10)] %% 10 

cec$state_en = google_trans(cec$state)

  #tolower(translate(content.vec = cec$state ,google.api.key =APIkey,source.lang = "zh-CN", target.lang="en"))
cec$state_en = gsub(" province", "", cec$state_en)
cec$state_en = gsub(" uygur autonomous region", "", cec$state_en)
cec$state_en = gsub(" hui autonomous region", "", cec$state_en)
cec$state_en = gsub(" autonomous region", "", cec$state_en)


cec$short_name = gsub("富拉尔基","华电富拉尔基",cec$short_name)
cec$short_name = gsub("穗恒运","恒运",cec$short_name)
cec$short_name = gsub("裕华","华电河北裕华",cec$short_name)
cec$short_name = gsub("达赉湖","满洲里",cec$short_name)
cec$short_name = gsub("太仓协鑫","协鑫太仓",cec$short_name)

cec$year_num =as.numeric(format(as.Date(cec$year, format="%Y-%m-%d"),"%Y"))
cec$check_status = 0

for(j in 1:nrow(cec)){
  #j = 71
  #j = 285
  #j = 194
  seg = seg_chs(cec$short_name[j]) # segment chinese 
  seg
  state = cec$state_en           # state information 
  
  
  num_info = vector()
  keyword_info = vector()
  company_info = vector()
  
  for(i in seg){
    ## 1) filter out information from the company names:
    # 1.1) redundant info
    index_redundant = which(stri_detect_fixed(redundant_words, i))# dont have to do anything. 
    
    # 1.2) state info 
    index_state = which(state_words==i)
    
    # 1.3) num info 
    index_num = which(stri_detect_fixed(num_words, i))
    num_info = append(num_info, num_words_ch[index_num])
    
    # 1.4) company info 
    index_company = which(company_words==i)
    company_info = append(company_info, company_words[index_company])
    
    # 1.5) keyword info
    if(length(index_redundant)+length(index_state)+length(index_num)+length(index_company) == 0){
      keyword_info = append(keyword_info, i)
    }
  }
  # 2.1) state_info index 
  index_state = which(plant_data$STATE ==state[j])
  
  # 2.2) company_info index
  company_info = unique(company_info)
  company_info
  company_info = pinyin_trans(company_info)
  
  # e.g., stri_detect_fixed(c("江苏省", "江苏省"),"江苏") TRUE TRUE
  index_company = vector()
  if(length(company_info) == 1){
    index_company = which(stri_detect_fixed(plant_data$plant_company,company_info))
  }
  if(length(company_info) > 1){
    index_company = which(stri_detect_fixed(plant_data$plant_company,company_info[1])
                          | stri_detect_fixed(plant_data$plant_company,company_info[2]))
  }

  
  # 2.3) keyword_info index
  keyword_info = unique(keyword_info)
  keyword_info
  keyword_info = pinyin_trans(keyword_info)
  
  index_key = vector()
  if(length(keyword_info) == 1){
    index_key = which(stri_detect_fixed(plant_data$plant_company,keyword_info))
  }
  if(length(keyword_info) >1){
    index_key = which(stri_detect_fixed(plant_data$plant_company,keyword_info[1])
                      | stri_detect_fixed(plant_data$plant_company,keyword_info[2]))
  }
  
  # 2.4) num_info index 
  num_info= unique(num_info)
  index_num = vector()
  if(length(num_info) >= 1){
    index_num = which(stri_detect_fixed(plant_data$PLANT,num_info[1]))
  }
  
  # 2.5) WM_info index 
  index_wm = vector()
  if(!is.na(cec$MW[j])){
    index_wm = which(abs(plant_data$MW-cec$MW[j])<=50)
  }
  #if(is.na(cec$MW[j])){
  #  index_wm = 1:nrow(plant_data)
  #}
  
  
  # 2.6) year_info index 
  index_year = vector()
  if(!is.na(cec$year_num[j])){
    index_year = which(abs(plant_data$YEAR-cec$year_num[j])<=1)
  }
  #if(is.na(cec$year_num[j])){
   # index_year = 1:nrow(plant_data)
  #}
  
  index_year_wm = intersect(index_wm,index_year)
  
  if(length(index_num)==0){
    index_cor = intersect(uniqueon(index_key, index_company), index_state)
    if(length(unique(plant_data$plant_company[index_cor]))==1){
      plant_data$cec_plant_code_test[index_cor] = cec$plant_code[j]
    }
    else{
      #index_cor = intersect(index_cor, index_year_wm)
      index_cor = intersect(uniqueon(index_key, index_company), intersect(index_state,index_year_wm))
      plant_data$cec_plant_code_test[index_cor] = cec$plant_code[j]
    }
  }
  
  if(length(index_num)>0){
    index_cor = intersect(uniqueon(index_key, index_company), intersect(index_state, index_num))
    if(length(unique(plant_data$plant_company[index_cor]))==1){
      plant_data$cec_plant_code_test[index_cor] = cec$plant_code[j]
    }
    else{
      index_cor = intersect(uniqueon(index_key, index_company), intersect(index_state, index_num))
      index_cor = intersect(index_cor, index_year_wm)
      plant_data$cec_plant_code_test[index_cor] = cec$plant_code[j]
    }
  }
  
  if(length(index_cor)>0){
    cec$check_status[j] = 1   # check how many cec information used
  }
  
}

# user rate: 
sum(cec$check_status ==1)/length(cec$check_status)
both_fill = which(!is.na(plant_data$CEC_PLANT_CODE) & !is.na(plant_data$cec_plant_code_test))
# accurate rate: 
sum(plant_data$CEC_PLANT_CODE[both_fill]==plant_data$cec_plant_code_test[both_fill])/length(both_fill)

