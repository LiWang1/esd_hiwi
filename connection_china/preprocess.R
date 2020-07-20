# The code is to preprocessing PLANT_DATA
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

# display chinese properly
Sys.setlocale(category="LC_ALL", locale = "en_US.UTF-8")

# preprocess PLANT_DATA
plant_data = read.csv("pp_CN_Mar_2020.csv", sep=";", stringsAsFactors = FALSE)
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
correct_pinyin <-  function(correct_file,m){
  for(i in 1:nrow(correct_file)){
    m = gsub(correct_file$original[i], correct_file$corrected[i], m)
  }
  return(m)
}
plant_data$PLANT = correct_pinyin(correct_file,plant_data$PLANT)

# 2) company names
correct_file_company = read_excel('pinyin_correction_company.xlsx')
correct_pinyin_company <-  function(correct, data){
  for(i in 1:nrow(correct)){
    index = which(stri_detect_fixed(data, correct$original[i]))
    data[index] = correct$corrected[i]
  }
  return(data)
}
plant_data$COMPANY = correct_pinyin_company(correct_file_company, plant_data$COMPANY)

#
plant_data$cec_plant_code_test=NA
plant_data$cec_unit_code_test= NA
plant_data$cec_using_hours = NA
plant_data$yb_plant_code=NA
plant_data$yb_unique_checking=NA
plant_data$yb_using_horus = NA

plant_data$plant_company = paste(tolower(plant_data$COMPANY), tolower(plant_data$PLANT))
plant_data$plant_company_extend = paste(tolower(plant_data$COMPANY), tolower(plant_data$PLANT),
                                        tolower(iconv(plant_data$CITY,"WINDOWS-1252","UTF-8")))


# pinyin translation:
mypy <- pydic(method = 'toneless', dic = "pinyin2")
mypy[["荥"]] = "xing"
pinyin_trans <- function(data){
  pinyin = py(data, dic = mypy, sep = '')
  pinyin = data.frame(keyName=names(pinyin), value=pinyin, row.names=NULL, stringsAsFactors = FALSE)$value
  return(pinyin)
}

# google translation:
APIkey= "AIz..."
google_trans <- function(chs){
  chs = tolower(translate(content.vec = chs ,google.api.key =APIkey,source.lang = "zh-CN", target.lang="en"))
  return(chs)
}

# google map api
api_geocoding = "AIz..."
get_coordinates_google <- function(addr, api_key){
  tryCatch(
    expr = {
      url_address = paste('https://maps.googleapis.com/maps/api/geocode/json?address=', addr,'&key=', api_key, sep = '')
      map_info = fromJSON(getURL(URLencode(url_address)))
      lat = map_info$results[[1]]$geometry$location$lat
      lng = map_info$results[[1]]$geometry$location$lng
      return(c(lat, lng))
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
api_key_baidu = 'fWm...'  # 6000 queries/day for free
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
