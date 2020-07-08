# R code to extract lat and lng for locations from baidu map 

library(XML)
library(RCurl)
library("rjson")
library(geosphere)

options(digits=12)                            # keep the digit for the coordinates
#api_key = 'AU3BZ-X7S6X-B6D47-7PTQG-I76LJ-7XBW5' # tercent 10000 /day
#keyword ='电厂'
api_key = 'fWmM8qGuIoCAQxLII2sB5RWgQKyaBKNc'  # 6000 queries/day for free

# //GET请求示例，注意参数值要进行URL编码
# https://apis.map.qq.com/ws/place/v1/suggestion/?region=北京&keyword=美食&key=OB4BZ-D4W3U-B7VVO-4PJWW-6TKDJ-WPB77
# http://api.map.baidu.com/place/v2/suggestion?query=天安门&region=北京&city_limit=true&output=json&ak=你的ak

get_coordinates <- function(address, api_key){
  # query
  keyword = "发电厂"
  city = "岳阳"
  url_address = paste('http://api.map.baidu.com/place/v2/suggestion?query=', keyword,'&region=', city, '&city_limit=true&output=json&ak=', api_key,sep = '')
  #url_address = paste('https://apis.map.qq.com/ws/place/v1/suggestion/?region=', city, '&keyword=', keyword, '&key', api_key,sep = '')
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
# http://api.map.baidu.com/geocoding/v3/?address=滨州魏桥棉纺织集团热电厂&output=json&ak=fWmM8qGuIoCAQxLII2sB5RWgQKyaBKNc
# e.g., https://apis.map.qq.com/ws/geocoder/v1/?address=
m1 = get_coordinates("大唐托克托发电", api_key)
m1

m2 = get_coordinates("北京清华大学", api_key)

# calculate the distance to the city center 
distm (m1, m2, fun = distHaversine)

