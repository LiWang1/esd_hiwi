# pinyin translation: 
library(pinyin)
mypy <- pydic(method = 'toneless', dic = "pinyin2")
pinyin_trans <- function(data){
  pinyin = py(data, dic = mypy, sep = '') 
  pinyin = data.frame(keyName=names(pinyin), value=pinyin, row.names=NULL, stringsAsFactors = FALSE)$value
  return(pinyin)
}

# google translation: 
APIkey= "AIzaSyCFtOytfyyzseR7FrHiYxFv91LtMvZSAXM"
library(translateR)
google_trans <- function(chs){
  chs = tolower(translate(content.vec = chs ,google.api.key =APIkey,source.lang = "zh-CN", target.lang="en"))
  return(chs)
}

# chinese segement
library("jiebaR")
mixseg = worker()
# add to libraries 
user_words = c("三百门","吉林","长春","田家庵","广西","北海",'辽宁', "大连", "天生港", "粤电", "台山", "集团", "陈家港", "白音华", "霍煤", "鸿骏"
               ,"华能",'大唐','浙能','神华','国能','国华','国电', '华电', '鲁能','国投','华润','粤电','中电投',"射阳港", "淮沪", "平圩","三厂"
               ,'华能','福能','申能','京能','皖能','协鑫', "上电", "华阳", "万kW", "华淮", "同华", "轩岗", "燃机电厂","深能", "恒益", "同煤", "中煤")
for(i in user_words){
  new_user_word(mixseg,i,"n")
}
seg_chs <-function(words){
  seg = segment(words, mixseg)
  return(seg)
}

# words library: 
redundant_words = c("分公司","中国","棉纺织","发电厂", "有限公司","发电","火电厂", '电厂', "总厂","热电", "燃机电厂",
                    "铝电","热","公司","有限", "新厂", "电子股份", "自备", "集团公司", "责任","集团", "股份",
                    "电力","煤电", "发展", "厂", "国际","控股", "能源", "号机组", "一期", "二期", "燃机", "电力公司","环保","汉东",
                    "三期","三四期", "一二期","第", "大机组","资源","铝材", "电业", "铝业", "有限责任", "热电厂", "北方", "万kW", "MW" )

state_words = c("北京", "湖南","天津", "广东", "河北", "广西", "山西","海南", "内蒙古", "内蒙","重庆",
                "辽宁","四川", "吉林","贵州","黑龙江","龙江", "云南","上海","西藏", "江苏", "陕西", "浙江",
                "甘肃","安徽","青海", "福建","宁夏","江西","新疆","山东","台湾", "河南", "香港","湖北","澳门")

company_words = c("华能",'大唐','浙能','神华','国能','国华','国电', '华电', '鲁能','国投','华润','粤电','中电',"赣能",
                  '中电投','华能','福能','申能','京能','皖能','协鑫', "上电", "华阳", "中煤", "建投", "深能", "国网","同煤" )

num_words = c("第四","第三", "第二", "第一", "C", "A", "B","D", 'a', 'b', 'c', 'd',
              '4', '3', '2', '1', '四', '三', '二', '一', '四厂', '三厂', '二厂', '一厂')
num_words_ch = c("4","3", "2", "1", "c", "a", "b","d", 'a', 'b', 'c', 'd', 
                 '4', '3', '2', '1', '4', '3', '2', '1', '4', '3', '2', '1')


############## start
data_connect <- function(yearbook, plant_data){
  
  ##  0) preprocessing 
  # 0.1) yearbook
  yearbook$short_name = gsub('[0-9]+', '', yearbook$company_name)
  yearbook$short_name = gsub("\\s*\\([^\\)]+\\)","",yearbook$short_name)
  yearbook$check_status = 0 
  yearbook$state_en = gsub(" province", "", yearbook$state_en)
  yearbook$state_en = gsub(" district", "", yearbook$state_en)
  
  
  # 0.2) plant_data
  plant_data$plant_company = paste(tolower(plant_data$COMPANY), tolower(plant_data$PLANT))
  plant_data$plant_company_extend = paste(tolower(plant_data$COMPANY), tolower(plant_data$PLANT), 
                                          tolower(iconv(plant_data$CITY,"WINDOWS-1252","UTF-8")))
  plant_data$yearbook_plant_code = NA
  plant_data$yearbook_unique_checking = NA
  
  
  
  for(j in 1:nrow(yearbook)){
    #j = 71
    seg = seg_chs(yearbook$short_name[j]) # segment chinese 
    state = yearbook$state_en           # state information 
    
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
}
