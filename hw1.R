rm(list = ls())  #Clearing Environment

##Q1- Loading Data into Data Frame
cali_employees <- read.table(file = "http://tinyurl.com/pdxdgrx",
                             header = TRUE,
                             sep = "\t",
                             stringsAsFactors = FALSE)
###

##Q1-No. of employees having total wages > 10000
high_pay_logical <- cali_employees$total_wages >100000
high_pays <- cali_employees$total_wages[high_pay_logical]
print(length(high_pays))
###

##Q2-No. of employees having total wages > 100000 and entity_county = Los Angeles
high_pay_logical <- cali_employees$total_wages >100000
county_LA_logical <- cali_employees$entity_county=="Los Angeles"
county_LA_high_pays_logical <- high_pay_logical & county_LA_logical
county_LA_high_pays <- cali_employees$entity_county[county_LA_high_pays_logical]
print(length(county_LA_high_pays))
###

##Q2-No. of employees having total wages > 100000 and entity_county = Los Angeles (Alternative)
high_pay_logical <- cali_employees$total_wages >100000
county_LA_logical <- cali_employees$entity_county=="Los Angeles"
county_LA_high_pays_logical <- high_pay_logical & county_LA_logical
county_LA_high_pays_logical_length <- length(county_LA_high_pays_logical[county_LA_high_pays_logical==TRUE])
print (county_LA_high_pays_logical_length)
###

##Q3-No. of Distinct position values holding by people having total wages > 100000
position_distinct <- unique(cali_employees$position[high_pay_logical])
print(length(position_distinct))
###

##Q4-Position of the highest total wage
high_wage_logical <- cali_employees$total_wages==max(cali_employees$total_wages)
position_high_wage <- cali_employees$position[high_wage_logical]
print(position_high_wage)
###

##Q4- Corrected
total_wages_sorted <- cali_employees$total_wages[order(cali_employees$total_wages)]
positions_sorted <- cali_employees$position[order(cali_employees$total_wages)]
print(positions_sorted[length(positions_sorted)])
###

##Q5-Position of Highest range between max_classification_salary and min_classification_salary
high_salary_range <- max(cali_employees$max_classification_salary - cali_employees$min_classification_salary)
high_salary_range_logical <- high_salary_range == (cali_employees$max_classification_salary - cali_employees$min_classification_salary)
position_high_salary_range <- cali_employees$position[high_salary_range_logical]
print(position_high_salary_range)
###

##Q5-Corrected
range <-  (cali_employees$max_classification_salary - cali_employees$min_classification_salary)
range_sorted <- range[order(range)]
position_sorted <- cali_employees$position[order(range)]
print(position_sorted[length(position_sorted)])
###

##Q6-Selective Replacement- Replace Top 5% and bottom 5% of total wages  with NA
top_bottom_5 <- quantile(cali_employees$total_wages,c(0.05,0.95))
top_5_logical <- cali_employees$total_wages <= top_bottom_5[1]
bottom_5_logical <- cali_employees$total_wages >= top_bottom_5[2]
top_bottom_5_logical <- top_5_logical | bottom_5_logical
cali_employees$total_wages[top_bottom_5_logical==TRUE] <- NA
# Count of Number of NAs
print(length(which(is.na(cali_employees$total_wages))))
#10534
###