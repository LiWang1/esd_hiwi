# to calculate the using hours from CEC data
result = read_excel("N_2016.xlsx")

result$unsch_stop_hours[which(is.na(result$unsch_stop_hours))] = 0
# calculate the using hours from cec N file: 
using_hours <- function(f1_score, use_std_coeff, use_real_coeff, nsche_hours){
  exposing_rate = (f1_score*5 - 15)/(use_real_coeff/use_std_coeff*100-100)
  #print(exposing_rate)
  sche_hours = (90 - use_std_coeff)*5*24
  hours_ready = 365*24-sche_hours-nsche_hours
  hours_use = hours_ready*exposing_rate*use_real_coeff/100
  return(hours_use)
}

result$using_hours = using_hours(result$f1, result$use_std_coeff, result$use_real_coeff, result$unsch_stop_hours)
mean(result$using_hours)

# asssign to database 
for(i in 1:nrow(result)){
  index = which(plant_data$cec_plant_code_test==result$plant_code[i] 
                & plant_data$cec_unit_code_test == result$unit_code[i])
  plant_data$cec_using_hours[index] = result$using_hours[i]
}
