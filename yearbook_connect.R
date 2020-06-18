

library(readxl)
yearbook <- read_excel("yearbook_2017.xlsx")
yearbook$yb_plant_code = 1:nrow(yearbook) 

##  0) preprocessing 
# 0.1) yearbook
yearbook$state_en = tolower(translate(content.vec = yearbook$state ,google.api.key =APIkey,source.lang = "zh-CN", target.lang="en"))
yearbook$short_name = gsub('[0-9]+', '', yearbook$company_name)
yearbook$short_name = gsub("\\s*\\([^\\)]+\\)","",yearbook$short_name)
yearbook$check_status = 0 
yearbook$state_en = gsub(" province", "", yearbook$state_en)
yearbook$state_en = gsub(" district", "", yearbook$state_en)




for(j in 1:nrow(yearbook)){
  #j = 71
  seg = seg_chs(yearbook$short_name[j]) # segment chinese 
  state = yearbook$state_en           # state information 
  seg
  
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
  
  ## 2) connect the information 
  
  # 2.1) state_info index 
  index_state = which(plant_data$STATE ==state[j])
  
  # 2.2) company_info index
  company_info = unique(company_info)
  company_info = pinyin_trans(company_info)
  company_info
  # e.g., stri_detect_fixed(c("江苏省", "江苏省"),"江苏") TRUE TRUE
  if(length(company_info) == 1){
    index_company = which(stri_detect_fixed(plant_data$plant_company,company_info))
  }
  if(length(company_info) > 1){
    index_company = which(stri_detect_fixed(plant_data$plant_company,company_info[1])
                          | stri_detect_fixed(plant_data$plant_company,company_info[2]))
  }
  if(length(company_info) == 0){
    index_company = 1:nrow(plant_data)
  }
  
  # 2.3) keyword_info index
  keyword_info = unique(keyword_info)
  keyword_info
  keyword_info = pinyin_trans(keyword_info)
  if(length(keyword_info) == 1){
    index_key = which(stri_detect_fixed(plant_data$plant_company,keyword_info))
  }
  if(length(keyword_info) >1){
    index_key = which(stri_detect_fixed(plant_data$plant_company_extend,keyword_info[1])
                      & stri_detect_fixed(plant_data$plant_company_extend,keyword_info[2]))
  }
  if(length(keyword_info) == 0){
    index_key = 1:nrow(plant_data)
  }
  
  # 2.4) num_info index 
  num_info= unique(num_info)
  num_info
  if(length(num_info) >= 1){
    index_num = which(stri_detect_fixed(plant_data$plant_company,num_info[1]))
  }
  if(length(num_info) == 0){
    index_num = 1:nrow(plant_data)
  }
  
  ## 3) find qualified index
  # 3.1) qualified index 
  index_qualified = intersect(intersect(index_key, index_num), intersect(index_company, index_state))
  
  # 3.2) maybe qualified index with company issues 
  index_com_issue = setdiff(intersect(intersect(index_key, index_num),index_state), index_qualified) 
  
  # 3.3) maybe qualified index with num issues  
  index_num_issue = setdiff(intersect(index_key, intersect(index_company, index_state)), index_qualified)
  
  
  
  ## 4) check uniqueness 
  # num_encoding: 
  # 1: match 
  # 0.5: information match but multiple companies 
  # 0.8: all information but company_info match
  # 0.3:  all information but company_info match + multiple companies 
  # 0.4: number issue 
  
  # 4.1) qualified index 
  if(length(unique(plant_data$plant_company[index_qualified])) ==1){
    plant_data$yearbook_plant_code[index_qualified] = yearbook$yb_plant_code[j]
    yearbook$check_status[j] =1
  }
  if(length(unique(plant_data$plant_company[index_qualified])) >1){
    plant_data$yearbook_unique_checking[index_qualified] = yearbook$yb_plant_code[j]
    yearbook$check_status[j] =0.5
  }
  
  # 4.2) for companies names that doesnt match 
  index_com_issue = setdiff(intersect(intersect(index_key, index_num),index_state), index_qualified) 
  
  if(length(unique(plant_data$plant_company[index_com_issue])) ==1){
    plant_data$yearbook_plant_code[index_com_issue] = yearbook$yb_plant_code[j]
    yearbook$check_status[j] = 0.8
  }
  if(length(unique(plant_data$plant_company[index_com_issue])) >1){
    plant_data$yearbook_unique_checking[index_com_issue] = yearbook$yb_plant_code[j]
    yearbook$check_status[j] =0.3
  }
  
  # 4.3) for numbers that doesnt match 
  index_num_issue = setdiff(intersect(index_key, intersect(index_company, index_state)), index_qualified)
  
  if(length(unique(plant_data$plant_company[index_num_issue])) ==1){
    plant_data$yearbook_plant_code[index_num_issue] = yearbook$yb_plant_code[j]
    yearbook$check_status[j] = 0.7
  }
  if(length(unique(plant_data$plant_company[index_num_issue])) >1){
    plant_data$yearbook_unique_checking[index_num_issue] = yearbook$yb_plant_code[j]
    yearbook$check_status[j] =0.2
  }
}


# 5) ratio of yearbook being used
print(sum(yearbook$check_status>0.0)/length(yearbook$check_status))
print(sum(yearbook$check_status>=0.7)/length(yearbook$check_status))


  
