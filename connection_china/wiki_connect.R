plant_data$unit_num = substrRight(plant_data$UNIT, 2)
plant_data$unit_num = as.numeric(gsub("\\D", "", plant_data$unit_num))

wiki <- read_excel("wiki_info_.xlsx")

# preprocessing 
wiki$wiki_code = 1:nrow(wiki) 
wiki$State = tolower(wiki$State)
wiki$unit = tolower(wiki$unit)
wiki$unit = gsub("[[:punct:]]", "", wiki$unit)
wiki$unit = correct_pinyin(wiki$unit)
wiki$year = as.numeric(wiki$year)

# initialized
plant_data$wiki_code = NA
wiki$check_status = 0 

for(j in 1:nrow(wiki)){
  #j =1224
  seg = seg_chs(wiki$chinese_name[j]) # segment chinese 
  seg
  
  num_info = vector()
  keyword_info = vector()
  keyword_info_ = vector()
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
  index_state = which(plant_data$STATE == wiki$State[j])
  
  # 2.2) company_info index
  company_info = unique(company_info)
  company_info = pinyin_trans(company_info)
  company_info
  index_company = 1:nrow(plant_data)
  if(length(company_info) == 1){
    index_company = which(stri_detect_fixed(plant_data$COMPANY,company_info))
  }
  if(length(company_info) > 1){
    index_company = which(stri_detect_fixed(plant_data$COMPANY,company_info[1])
                          | stri_detect_fixed(plant_data$COMPANY,company_info[2]))
  }
  
  # 2.3) keyword_info index
  keyword_info = unique(keyword_info)
  keyword_info
  index_key = 1:nrow(plant_data)
  keyword_info = pinyin_trans(keyword_info)
  
  # filter useful keyword_info
  if(length(keyword_info)>0){
    for(m in 1:length(keyword_info)){
      if(grepl(keyword_info[m], wiki$unit[j], fixed = TRUE)){
        keyword_info_ = append(keyword_info_, keyword_info[m])
      }
    }
  }
  
  keyword_info_
  
  if(length(keyword_info_) == 1){
    index_key = which(stri_detect_fixed(plant_data$PLANT,keyword_info_))
  }
  if(length(keyword_info_) >1){
    index_key = which(stri_detect_fixed(plant_data$PLANT,keyword_info_[1])
                      & stri_detect_fixed(plant_data$PLANT,keyword_info_[2]))
  }
  
  
  
  # 2.4) num_info index 
  num_info= unique(num_info)
  num_info
  index_num = 1:nrow(plant_data)
  if(length(num_info) >= 1){
    index_num = which(stri_detect_fixed(plant_data$PLANT,num_info[1]))
  }
  
  # WM and year information 
  index_wm_year = vector()
  if(!is.na(wiki$year[j])){
    index_wm_year = which(abs(plant_data$MW-wiki$MW[j])<=70 & abs(plant_data$YEAR-wiki$year[j])<=3)
  }
  if(is.na(wiki$year[j])){
    index_wm_year = which(abs(plant_data$MW-wiki$MW[j])<30)
  }
  
  # unit_num 
  index_unit_num = which(plant_data$unit_num==wiki$unit_num[j])
  
  
  
  ## 3) find qualified index
  index_qualified = intersect(intersect(intersect(index_state, index_num), 
                                        intersect(index_key, index_wm_year)), index_unit_num)
  if(length(index_qualified)==1){
    print(index_qualified)
    plant_data$wiki_code[index_qualified] = wiki$wiki_code[j]
    wiki$check_status[j] = 1
  }
  if(length(index_qualified)>1){
    index_qualified = intersect(intersect(intersect(index_state, index_company), 
                                          intersect(index_key, index_wm_year)), intersect(index_num,index_unit_num))
    plant_data$wiki_code[index_qualified] = wiki$wiki_code[j]
    wiki$check_status[j] = 1
  }
  if(length(index_qualified)<1){
    # in case of incorrect plant_numbers 
    index_qualified_1 = intersect(intersect(intersect(index_state, index_company), 
                                          intersect(index_key, index_wm_year)), index_unit_num)
    if(length(index_qualified_1)==1){
      plant_data$wiki_code[index_qualified_1] = wiki$wiki_code[j]
      wiki$check_status[j] = 1
    }
    
    # in case of wrong unit number
    index_qualified_2 = intersect(intersect(intersect(index_state, index_company), 
                                            intersect(index_key, index_wm_year)), index_num)
    if(length(index_qualified_2)==1){
      plant_data$wiki_code[index_qualified_2] = wiki$wiki_code[j]
      wiki$check_status[j] = 1
    }
    if(length(index_qualified_2)==2 & index_qualified_2[2]-index_qualified_2[1]<5){
      plant_data$wiki_code[index_qualified_2] = wiki$wiki_code[j]
      wiki$check_status[j] = 1
    }
  }
}

sum(wiki$check_status)



