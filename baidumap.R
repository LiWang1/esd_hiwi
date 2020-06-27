# R code to extract lat and lng for locations from baidu map 

library(XML)
library(RCurl)
library("rjson")
library(geosphere)

options(digits=12)                            # keep the digit for the coordinates
api_key = ''  # 6000 queries/day for free

get_coordinates <- function(address, api_key){
  # query
  url_address = paste('http://api.map.baidu.com/geocoding/v3/?address=', address, '&output=json&ak=', api_key, sep = '')
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

# e.g., 
m1 = get_coordinates("北京", api_key)
m2 = get_coordinates("北京清华大学", api_key)

# calculate the distance to the city center 
distm (m1, m2, fun = distHaversine)

