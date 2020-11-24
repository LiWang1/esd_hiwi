# pinyin translation: 
mypy <- pydic(method = 'toneless', dic = "pinyin2")
mypy[["荥"]] = "xing"
pinyin_trans <- function(data){
  pinyin = py(data, dic = mypy, sep = '') 
  pinyin = data.frame(keyName=names(pinyin), value=pinyin, row.names=NULL, stringsAsFactors = FALSE)$value
  return(pinyin)
}

# google translation: 
APIkey= ""
google_trans <- function(chs){
  chs = tolower(translate(content.vec = chs ,google.api.key =APIkey, source.lang = "zh-CN", target.lang="en"))
  return(chs)
}

# google map api 
api_geocoding = ""
get_coordinates_google <- function(addr, api_key){
  tryCatch(
    expr = {
      url_address = paste('https://maps.googleapis.com/maps/api/geocode/json?address=', addr,'&key=', api_key, sep = '')
      map_info = fromJSON(getURL(URLencode(url_address)))
      lat = map_info$results[[1]]$geometry$location$lat
      lng = map_info$results[[1]]$geometry$location$lng
      lat_max = map_info$results[[1]]$geometry$bounds$northeast$lat
      lng_max = map_info$results[[1]]$geometry$bounds$northeast$lng
      lat_min = map_info$results[[1]]$geometry$bounds$southwest$lat
      lng_min = map_info$results[[1]]$geometry$bounds$southwest$lng
      return(c(lat,lng,lat_min,lng_min,lat_max,lng_max))
    },
    error = function(e){ 
      # Do this if an error is caught...
      print(addr)
    },
    warning = function(w){
      # Do this if an warning is caught...
      print(addr)
    }
  )
}

# baidu api 
options(digits=12)      # keep the digit for the coordinates
api_key_baidu = ''  # 6000 queries/day for free
get_coordinates_baidu <- function(address, api_key){
  url_address = paste('http://api.map.baidu.com/place/v2/suggestion?address=', address, '&output=json&ak=', api_key,sep = '')
  map_info = fromJSON(getURLContent(url_address))
  
  # extract 
  status = map_info$status              # status == 0 means query successfully
  lat_ = map_info$result$location$lat   # latitude
  lng_ = map_info$result$location$lng   # longitude
  
  # check and assign
  lat = NA
  lng = NA
  if(status==0){
    lat = lat_
    lng = lng_
  }
  return(c(lng, lat))
}

# chinese segement
readLines("stop.txt")
mixseg = worker(type = 'mix', stop_word = "stop.txt")
# add to libraries 
user_words = c('康巴什','酒钢','马鞍山',"三百门","吉林","长春","田家庵","广西","北海",'辽宁', "大连", "天生港", "粤电", "台山", "集团", "陈家港", "白音华", "霍煤", "鸿骏"
               ,"华能",'大唐','浙能','神华','国能','国华','国电', '华电', '鲁能','国投','华润','粤电','中电投',"射阳港", "淮沪", "平圩","三厂","热","协鑫", "太仓"
               ,'华能','福能','申能','京能','皖能','协鑫', "上电", "华阳", "万kW", "华淮", "同华", "轩岗", "燃机电厂","深能","黔桂", "恒益", "同煤", "中煤")
for(i in user_words){
  new_user_word(mixseg,i,"n")
}
seg_chs <-function(words){
  seg = segment(words, mixseg)
  return(seg)
}

# speicial comibination 
uniqueon <- function(index_key, index_company){
  if(length(index_key)>0 & length(index_company)>0){
    index = intersect(index_key, index_company)
    if(length(index)==0){
      index = index_key
    }
  }
  if(length(index_key)==0){
    index = index_company
  }
  if(length(index_company)==0){
    index = index_key
  }
  if(length(index_company)==0 & length(index_key)==0){
    index = vector()
  }
  return(index)
}

# words library: 
redundant_words = c("分公司","中国","棉纺织","发电厂", "有限公司","发电","火电厂", '电厂', "总厂","热电", "燃机电厂",
                    "铝电","热","公司","有限", "新厂", "电子股份", "自备", "集团公司", "责任","集团", "股份",'矿业','泥矸石',
                    "电力","煤电", "发展", "厂", "国际","控股", "能源", "号机组", "一期", "二期", "燃机", "电力公司","环保","汉东",'汽车',
                    "三期","三四期", "一二期","第", "大机组","资源","铝材", "电业", "铝业", "有限责任", "热电厂", "北方", "万kW", "MW","神皖" )

state_words = c("北京", "湖南","天津", "广东", "河北", "广西", "山西","海南", "内蒙古", "内蒙","重庆",
                "辽宁","四川", "吉林","贵州","黑龙江","龙江", "云南","上海","西藏", "江苏", "陕西", "浙江",
                "甘肃","安徽","青海", "福建","宁夏","江西","新疆","山东","台湾", "河南", "香港","湖北","澳门")

company_words = c('万基',"华能",'大唐','浙能','神华','国能','国华','国电', '华电', '鲁能','国投','华润','粤电','中电',"赣能",'中石化',
                  '中电投','华能','福能','申能','京能','皖能','协鑫', "上电", "华阳", "中煤", "建投", "深能", "国网","同煤", "百年","黔桂",'皖能')

num_words = c("第四","第三", "第二", "第一", "C", "A", "B","D", 'a', 'b', 'c', 'd',
              '4', '3', '2', '1', '四', '三', '二', '一', '四厂', '三厂', '二厂', '一厂',"三热", "二热","一热", "包二", '哈三', '外一', '外二', '外三')
num_words_ch = c("4","3", "2", "1", "c", "a", "b","d", 'a', 'b', 'c', 'd', 
                 '4', '3', '2', '1', '4', '3', '2', '1', '4', '3', '2', '1', '3', '2', '1',"baotou-2", "haerbin-3", "waigaoqiao -1", "waigaoqiao -2", "waigaoqiao -3")


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

correct_pinyin <-  function(correct_file,m){
  for(i in 1:nrow(correct_file)){
    m = gsub(correct_file$original[i], correct_file$corrected[i], m) 
  }
  return(m)
}

correct_pinyin_company <-  function(correct, data){
  for(i in 1:nrow(correct)){
    index = which(stri_detect_fixed(data, correct$original[i]))
    data[index] = correct$corrected[i]
  }
  return(data)
}

## preproprocess original database 
preprocess <- function(plant_data){
  plant_data$PLANT = tolower(plant_data$PLANT)
  plant_data$PLANT = gsub("'", "", plant_data$PLANT)
  plant_data$COMPANY = tolower(plant_data$COMPANY)
  plant_data$STATE = tolower(plant_data$STATE)
  plant_data$STATE = gsub("ningxia hui", "ningxia", plant_data$STATE)     # ningxia
  
  # number of the plants 
  plant_data$UNIT = gsub("-V", "-5", plant_data$UNIT) 
  plant_data$UNIT = gsub("-IV", "-4", plant_data$UNIT) 
  plant_data$UNIT = gsub("-III", "-3", plant_data$UNIT) # so "-II" wont detected in "-III" 
  plant_data$UNIT = gsub("-II", "-2", plant_data$UNIT) 
  plant_data$UNIT = gsub("-I", "-1", plant_data$UNIT) 
  
  nums = c("-5", "-4", "-3", "-2", "-1")
  for(i in nums){
    index = which(stri_detect_fixed(plant_data$UNIT, i) & !stri_detect_fixed(plant_data$PLANT, i))
    plant_data$PLANT[index] = paste(plant_data$PLANT[index], i) 
  }
  
  # english-> pinyin issues: 
  # 1) plant names 
  correct_file = read_excel(' pinyin_correction.xlsx')
  plant_data$PLANT = correct_pinyin(correct_file,plant_data$PLANT)
  
  # 2) company names 
  correct_file_company = read_excel('pinyin_correction_company.xlsx')
  plant_data$COMPANY = correct_pinyin_company(correct_file_company, plant_data$COMPANY)
  
  # cec
  plant_data$cec_plant_code_test=NA 
  plant_data$cec_unit_code_test= NA
  plant_data$cec_using_hours = NA
  plant_data$coal_consumption_rate = NA
  
  # yearbook
  plant_data$yearbook_plant_code = NA
  plant_data$yearbook_unique_checking = NA
  plant_data$yb_plant_code=NA 
  plant_data$yb_unique_checking=NA
  plant_data$yearbook_using_hours = NA 
  
  # wiki
  plant_data$unit_num = substrRight(plant_data$UNIT, 2)
  plant_data$unit_num = as.numeric(gsub("\\D", "", plant_data$unit_num))
  plant_data$wiki_code = NA
  
  plant_data$plant_company = paste(tolower(plant_data$COMPANY), tolower(plant_data$PLANT))
  plant_data$plant_company_extend = paste(tolower(plant_data$COMPANY), tolower(plant_data$PLANT), 
                                          tolower(iconv(plant_data$CITY,"WINDOWS-1252","UTF-8")))
  return(plant_data)
}

## connect cec data
cec_connect <- function(cec, plant_data){
  ## preprocess cec data 
  cec = cec[!duplicated(cec[,c('plant_code','unit_name')]),]      # unique the dataframe
  cec$check_status = 0
  cec$unit_check_status = 0
  
  ## connect cec code to plant database code  
  for(j in 1:nrow(cec)){
    #j = 727 # 华能内蒙包二"
    #j = 1081 #"中电投江西景德镇"
    
    seg = seg_chs(cec$short_name[j]) # segment chinese 
    seg
    
    
    num_info = vector()
    keyword_info = vector()
    company_info = vector()
    
    for(i in seg){
      ## 1) filter out useful information from the company names:
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
    
    # 2.1) state_info index 
    index_state = which(plant_data$STATE ==cec$state_en[j])
    
    # 2.2) company_info index
    company_info = unique(company_info)
    company_info
    company_info = pinyin_trans(company_info)
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
    num_info
    index_num = vector()
    if(length(num_info) >= 1){
      index_num = which(stri_detect_fixed(plant_data$PLANT,num_info[1]))
    }
    
    # 2.5) WM_info index 
    index_wm = vector()
    if(!is.na(cec$MW[j])){
      index_wm = which(abs(plant_data$MW-cec$MW[j])<=30)
    }
    
    # 2.6) year_info index 
    index_year = vector()
    if(!is.na(cec$year_num[j])){
      index_year = which(abs(plant_data$YEAR-cec$year_num[j])<=1)
    }
    
    index_year_wm = intersect(index_wm,index_year) # index that matches year and wm info
    
    # without num info
    if(length(index_num)==0){
      index_cor = intersect(uniqueon(index_key, index_company), index_state)
      if(length(unique(plant_data$plant_company[index_cor]))==1){
        plant_data$cec_plant_code_test[index_cor] = cec$plant_code[j]
      }
      
      else{
        index_cor = intersect(uniqueon(index_key, index_company), intersect(index_state,index_year_wm))
        plant_data$cec_plant_code_test[index_cor] = cec$plant_code[j]
      }
    }
    
    # with num info
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
  print("1")
  ## connect cec info to plant database
  for( i in 1:nrow(plant_data)){
    if(!is.na(plant_data$cec_plant_code_test[i]) & (!is.na(plant_data$cec_unit_code_test[i]))){
      index = which(cec$plant_code == plant_data$cec_plant_code_test[i] & cec$unit_name == plant_data$cec_unit_code_test[i])
      if(length(index)==1){
        # connect using hours
        plant_data$cec_using_hours[i] = cec$using_hours[index]
        # connect coal consumption 
        plant_data$coal_consumption_rate[i] = cec$coal_consumption[index]
      }
    }
  }
  print('2')
  ## check the ratio of used cec data 
  for(i in 1:nrow(plant_data)){
    if(!is.na(plant_data$cec_plant_code_test[i])){
      index_code = which((plant_data$cec_plant_code_test[i]==cec$plant_code)
                         &abs(plant_data$MW[i]-cec$MW)<100
                         &abs(plant_data$YEAR[i]-cec$year_num)<=1
                         &cec$unit_check_status==0)
      index=min(index_code)
      plant_data$cec_unit_code_test[i] = cec$unit_name[index] 
      cec$unit_check_status[index] = 1
    }
  } 
  ratio= sum(cec$unit_check_status)/sum(cec$check_status)
  print(ratio)
  print('3')
  return(plant_data)
}

## connect yearbook data
yearbook_connect <- function(yearbook, plant_data){
  yearbook$short_name = gsub('[0-9]+', '', yearbook$company_name)
  yearbook$check_status = 0 
  
  ## connect yearbook data to database 
  for(j in 1:nrow(yearbook)){
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
  
  ## connect yearbook information
  
  ## check ratio of used yearbook data
  print(sum(yearbook$check_status> 0.0)/length(yearbook$check_status))
  print(sum(yearbook$check_status>= 0.6)/length(yearbook$check_status))
  return(plant_data)
}

## connect wiki data
wiki_connect <- function(wiki, plant_data){
  wiki$wiki_code = 1:nrow(wiki) 
  wiki$State = tolower(wiki$State)
  wiki$unit = tolower(wiki$unit)
  wiki$unit = gsub("[[:punct:]]", "", wiki$unit)
  correct_file = read_excel(' pinyin_correction.xlsx')
  wiki$unit = correct_pinyin(correct_file, wiki$unit)
  wiki$year = as.numeric(wiki$year)
  wiki$check_status = 0 
  
  ## connect wiki data to database 
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
      #print(index_qualified)
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
  
  ## check the ratio of used wiki data
  ratio = sum(wiki$check_status)/nrow(wiki)
  print(ratio)
  return(plant_data)
}








