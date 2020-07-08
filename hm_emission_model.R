### model to simulate the heavy metals emission for coal power plants 

# 1) electricity generation 
ele_total   # available 

# 2) total coal consumption 
coal_total = ele_total*coal_consump_rate  # coal_consump_rate is partially available

# 3) based on the characteristics of the input coal, we can calculate the amount for each heavy metals.
a_total = coal_total*a_ratio              # only average value available
# take China as an example, these are averaged values and they can differ based on what kind of coal they used
#https://www.academia.edu/9641565/Geochemistry_of_trace_elements_in_Chinese_coals_A_review_of_abundances_genetic_types_impacts_on_human_health_and_industrial_utilization
# Cr 16 ug/g
# As 4 ug/g
# Cd 0.3 ug/g
# Pb 16.64ug/g  


# 4) heavy metal emission 
# emission_factor is depended on: 
# a) volativity: the more volatile, the bigger the factor
# b) air pollution protection devices: the better the devices, the higher removal rate and thus lower emission factor

removal_rate <- function(volatility, devices, age_plant){
  # param@volatility: the more volatile, the lower removal rate; 
  # param@devices: the more and the more advanced the devices, the better removal rate; 
  # param@age_plant: the new plants should have better performances in general. 
}

# e.g., from: https://www.sciencedirect.com/science/article/pii/S0378382003001747?via%3Dihub
# plants with Particulate capture device and Wet FGD have:
# Cd: 98.9-99.9% removal rate 
# Pb: 95-98% removal rate
# Mn: 99.9% removal rate

metal_a_emission = a_total*(1-removal_rate_a)      # for some metals in China, emission factors available
