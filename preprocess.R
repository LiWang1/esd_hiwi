############# preprocessing PLANT_DATA
library(stringi)
## display chinese properly 
Sys.setlocale(category="LC_ALL", locale = "en_US.UTF-8")

## preprocess PLANT_DATA
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
plant_data$PLANT = gsub("shangdu", "shangdou", plant_data$PLANT)  # 上都 mypy[["都"]] = du 
plant_data$PLANT = gsub("leqing", "yueqing", plant_data$PLANT)    # 乐清 mypy[["乐"]] = le 
plant_data$PLANT = gsub("pingwei", "pingxu", plant_data$PLANT)    # 平圩 mypy[["圩"]] = wei
plant_data$PLANT = gsub("suzhou", "xiuzhou", plant_data$PLANT)    # 宿州  mypy[["宿"]] = su
plant_data$PLANT = gsub("bengbu", "bangbu", plant_data$PLANT)     # 蚌埠  mypy[["蚌"]] = beng
plant_data$PLANT = gsub("manas", "manasi", plant_data$PLANT)     # 
plant_data$PLANT = gsub("harbin", "haerbin", plant_data$PLANT)     # 
plant_data$PLANT = gsub("lueyang", "lveyang", plant_data$PLANT)     # 
plant_data$PLANT = gsub("hailar", "hailaer", plant_data$PLANT)    
plant_data$PLANT = gsub("turpan", "tulufan", plant_data$PLANT)    
plant_data$PLANT = gsub("aksu", "akesu", plant_data$PLANT) 
plant_data$PLANT = gsub("alar", "alaer", plant_data$PLANT) 
plant_data$PLANT = gsub("ewenki", "ewenke", plant_data$PLANT) 
plant_data$PLANT = gsub("korla", "kela", plant_data$PLANT) 
plant_data$PLANT = gsub("urumqi", "wulumuqi", plant_data$PLANT) 
plant_data$PLANT = gsub("ordos", "eerduosi", plant_data$PLANT) 
plant_data$PLANT = gsub("sanshui/hengyi", "hengyi", plant_data$PLANT) 

########################
# company names issues 

# SPIC/CHINA POWER INTL DEV LTD/china power investment -> 中电投 -> zhongdiantou  
index_spic1 = which(stri_detect_fixed(plant_data$COMPANY, "spic"))
index_spic2 = which(stri_detect_fixed(plant_data$COMPANY, "china power intl"))
index_spic3 = which(stri_detect_fixed(plant_data$COMPANY, "china power investment"))
index_spic = unique(c(index_spic1, index_spic2, index_spic3))
plant_data$COMPANY[index_spic] = "zhongdiantou"

# shenergy -> 申能 -> shenneng
index_shen = which(stri_detect_fixed(plant_data$COMPANY, "shenergy"))
plant_data$COMPANY[index_shen] = "shenneng"

# fujian energy -> 福能 -> funeng
index_fu = which(stri_detect_fixed(plant_data$COMPANY, "fujian energy"))
plant_data$COMPANY[index_fu] = "funeng"

# beijing energy ->京能 -> jingneng
index_jing = which(stri_detect_fixed(plant_data$COMPANY, "beijing energy"))
plant_data$COMPANY[index_jing] = "jingneng"

# beijing energy ->京能 -> jingneng
index_jing = which(stri_detect_fixed(plant_data$COMPANY, "beijing energy"))
plant_data$COMPANY[index_jing] = "jingneng"

# wenergy ->皖能 -> wanneng
index_jing = which(stri_detect_fixed(plant_data$COMPANY, "wenergy"))
plant_data$COMPANY[index_jing] = "wanneng"

# JIAXING POWER/ZHEJIANG PROV ENERGY GROUP CO -> 浙能 -> zheneng
index_zn1 = which(stri_detect_fixed(plant_data$COMPANY, "jiaxing power"))
index_zn2 = which(stri_detect_fixed(plant_data$COMPANY, "zhejiang prov energy"))
index_zn = unique(c(index_zn1, index_zn2))
plant_data$COMPANY[index_zn] = "zheneng"

# datong coal mine group co ltd -> 同煤 -> tongmei
index_tm = which(stri_detect_fixed(plant_data$COMPANY, "datong coal mine group co ltd"))
plant_data$COMPANY[index_tm] = "tongmei"

# china natl energy invest group -> 国电/国华/国网-> guodian/guohua/guowang
index_guo = which(stri_detect_fixed(plant_data$COMPANY, "china natl energy invest group"))
plant_data$COMPANY[index_guo] = "guodian/guohua/guowang/shenhua"
# 
index_guo_yue = which(stri_detect_fixed(plant_data$COMPANY, "guodian/guohua/guowang/shenhua")
                      & stri_detect_fixed(tolower(plant_data$STATE), "guangdong"))
plant_data$COMPANY[index_guo_yue] = "guodian/guohua/guowang/shenhua/yuedian"

# sdic -> 国投 -> guotou
index_guotou = which(stri_detect_fixed(plant_data$COMPANY, "sdic"))
plant_data$COMPANY[index_guotou] = "guotou"

# gcl-poly energy holdings ltd -> 协鑫 -> xiexin
index_guotou = which(stri_detect_fixed(plant_data$COMPANY, "gcl-poly energy holdings ltd"))
plant_data$COMPANY[index_guotou] = "xiexin"

# china resources power holdings -> 华润 -> huarun
# china res power north china
index_hr1 = which(stri_detect_fixed(plant_data$COMPANY, "china resources power holdings"))
index_hr2 = which(stri_detect_fixed(plant_data$COMPANY, "china res power"))
index_hr = unique(c(index_hr1, index_hr2))
plant_data$COMPANY[index_hr] = "huarun"

# north united power co ltd  -> 华能 -> huaneng
index_nu = which(stri_detect_fixed(plant_data$COMPANY, "north united power co ltd"))
plant_data$COMPANY[index_nu] = "huaneng"


# shandong century elec dev co  -> 华能 -> yantai bainian 
index_yantai = which(stri_detect_fixed(plant_data$COMPANY, "shandong century elec dev co"))
plant_data$COMPANY[index_yantai] = "yantai bainian"

# guangdong energy group /yudean /guangdong elec power/CHEUNG KONG INFRASTRUCTURE -> 粤电 -> yuedian
index_y1 = which(stri_detect_fixed(plant_data$COMPANY, "guangdong energy group"))
index_y2 = which(stri_detect_fixed(plant_data$COMPANY, "yudean"))
index_y3 = which(stri_detect_fixed(plant_data$COMPANY, "guangdong elec power"))
index_yd = unique(c(index_y1, index_y2, index_y3))
plant_data$COMPANY[index_yd] = "yuedian"
######################

plant_data$plant_company = paste(tolower(plant_data$COMPANY), tolower(plant_data$PLANT))
plant_data$plant_company_extend = paste(tolower(plant_data$COMPANY), tolower(plant_data$PLANT), 
                                        tolower(iconv(plant_data$CITY,"WINDOWS-1252","UTF-8")))

plant_data$cec_plant_code_test=NA 
plant_data$cec_unit_code_test= NA
plant_data$cec_unique_checking = NA
plant_data$yearbook_plant_code=NA 
plant_data$yearbook_unique_checking=NA



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

detect_string <- function(long, short){
  if(length(short)>0){
    index = which(stri_detect_fixed(long, short))
  }
  if(short==""){
    index = 1:nrow(long)
  }
  return(index)
}

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
                    "铝电","热","公司","有限", "新厂", "电子股份", "自备", "集团公司", "责任","集团", "股份",
                    "电力","煤电", "发展", "厂", "国际","控股", "能源", "号机组", "一期", "二期", "燃机", "电力公司","环保","汉东",
                    "三期","三四期", "一二期","第", "大机组","资源","铝材", "电业", "铝业", "有限责任", "热电厂", "北方", "万kW", "MW" )

state_words = c("北京", "湖南","天津", "广东", "河北", "广西", "山西","海南", "内蒙古", "内蒙","重庆",
                "辽宁","四川", "吉林","贵州","黑龙江","龙江", "云南","上海","西藏", "江苏", "陕西", "浙江",
                "甘肃","安徽","青海", "福建","宁夏","江西","新疆","山东","台湾", "河南", "香港","湖北","澳门")

company_words = c("华能",'大唐','浙能','神华','国能','国华','国电', '华电', '鲁能','国投','华润','粤电','中电',"赣能",
                  '中电投','华能','福能','申能','京能','皖能','协鑫', "上电", "华阳", "中煤", "建投", "深能", "国网","同煤", "百年" )

num_words = c("第四","第三", "第二", "第一", "C", "A", "B","D", 'a', 'b', 'c', 'd',
              '4', '3', '2', '1', '四', '三', '二', '一', '四厂', '三厂', '二厂', '一厂',"三热", "二热","一热")
num_words_ch = c("4","3", "2", "1", "c", "a", "b","d", 'a', 'b', 'c', 'd', 
                 '4', '3', '2', '1', '4', '3', '2', '1', '4', '3', '2', '1', '3', '2', '1')









