library(readxl)
coordinates <- read_excel("coodinates_plants.xlsx")
coordinates$state_en = google_trans(coordinates$state)

plant_data$coordinates = NA

target_data = coordinates

for(j in 1:nrow(target_data)){

  seg = seg_chs(target_data$chinese_name[j]) # segment chinese 
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
  index_state = which(plant_data$STATE == target_data$state_en[j])
  
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
  index_num = 1:nrow(plant_data)
  if(length(num_info) >= 1){
    index_num = which(stri_detect_fixed(plant_data$PLANT,num_info[1]))
  }
  
  index_qualified = intersect(index_key,intersect(index_num,index_state))
  plant_data$coordinates[index_qualified] = target_data$coordinates[j]
}
