library(readxl)
yearbook <- read_excel("yearbook_2017.xlsx")

# preprocessing 
yearbook$yb_plant_code = 1:nrow(yearbook) 

yearbook$state_en = google_trans(yearbook$state)
yearbook$state_en = gsub(" province", "", yearbook$state_en)
yearbook$state_en = gsub(" district", "", yearbook$state_en)

yearbook$short_name = gsub('[0-9]+', '', yearbook$company_name)

# initialized
plant_data$yearbook_plant_code = NA
plant_data$yearbook_unique_checking = NA
yearbook$check_status = 0 

for(j in 1:nrow(yearbook)){

  #j =92
  seg = seg_chs(yearbook$short_name[j]) # segment chinese 
  seg
  
  num_info = vector()
  keyword_info = vector()
  company_info = vector()
  
  for(i in seg){
    ## 1) filter out information from the company names:
    index_state = which(state_words==i)
    # 1.1) num info 
    index_num = which(stri_detect_fixed(num_words, i))
    num_info = append(num_info, num_words_ch[index_num])
    
    # 1.2) company info 
    index_company = which(company_words==i)
    company_info = append(company_info, company_words[index_company])
    
    # 1.3) keyword info
    if(length(index_state)+length(index_num)+length(index_company) == 0){
      keyword_info = append(keyword_info, i)
    }
  }
  
  ## 2) connect the information 
  
  # 2.1) state_info index 
  index_state = which(plant_data$STATE == yearbook$state_en[j])
  
  # 2.2) company_info index
  company_info = unique(company_info)
  company_info = pinyin_trans(company_info)
  company_info
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
  index_key = vector()
  keyword_info = pinyin_trans(keyword_info)
  if(length(keyword_info) == 1){
    index_key = which(stri_detect_fixed(plant_data$plant_company,keyword_info))
  }
  if(length(keyword_info) >1){
    index_key = which(stri_detect_fixed(plant_data$plant_company_extend,keyword_info[1])
                      & stri_detect_fixed(plant_data$plant_company_extend,keyword_info[2]))
  }

  
  # 2.4) num_info index 
  num_info= unique(num_info)
  num_info
  index_num = vector()
  if(length(num_info) >= 1){
    index_num = which(stri_detect_fixed(plant_data$PLANT,num_info[1]))
  }

  
  ## 3) find qualified index
  if(length(index_num)==0){
    index_qualified = intersect(uniqueon(index_key, index_company), index_state)
    if(length(unique(plant_data$plant_company[index_qualified])) ==1){
      plant_data$yearbook_plant_code[index_qualified] = yearbook$yb_plant_code[j]
      plant_data$yearbook_using_hours[index_qualified] = yearbook$using_hours[j]
      yearbook$check_status[j] =1
    }
    if(length(unique(plant_data$plant_company[index_qualified])) >1){
      plant_data$yearbook_unique_checking[index_qualified] = yearbook$yb_plant_code[j]
      yearbook$check_status[j] =0.5
    }
  }
  
  if(length(index_num)!=0){
    index_qualified = intersect(uniqueon(index_key, index_company), intersect(index_state, index_num))
    if(length(unique(plant_data$plant_company[index_qualified])) ==1){
      plant_data$yearbook_plant_code[index_qualified] = yearbook$yb_plant_code[j]
      plant_data$yearbook_using_hours[index_qualified] = yearbook$using_hours[j]
      yearbook$check_status[j] = 1
    }
    if(length(unique(plant_data$plant_company[index_qualified])) >1){
      plant_data$yearbook_unique_checking[index_qualified] = yearbook$yb_plant_code[j]
      yearbook$check_status[j] = 0.5
    }
  }
}

# 5) ratio of yearbook being used
print(sum(yearbook$check_status> 0.0)/length(yearbook$check_status))
print(sum(yearbook$check_status>= 0.6)/length(yearbook$check_status))



