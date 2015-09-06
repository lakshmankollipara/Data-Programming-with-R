rm(list = ls())  #Clearing Environment
cali_employees <- read.table(file = "http://tinyurl.com/pdxdgrx",
                             header = TRUE,
                             sep = "\t",
                             stringsAsFactors = FALSE)

high_pay_logical <- cali_employees$total_wages >100000
county_LA_logical <- cali_employees$entity_county=="Los Angeles"

county_LA_high_pays_logical <- high_pay_logical & county_LA_logical
temp <- length(county_LA_high_pays_logical[county_LA_high_pays_logical==TRUE])
print (temp)
county_LA_high_pays <- cali_employees$entity_county[county_LA_high_pays_logical]
print(length(county_LA_high_pays))

position_distinct <- unique(cali_employees$position[high_pay_logical])
print(length(position_distinct))

high_wage_logical <- cali_employees$total_wages==max(cali_employees$total_wages)
position_high_wage <- cali_employees$position[high_wage_logical]
print(position_high_wage)

high_salary_range <- max(cali_employees$max_classification_salary - cali_employees$min_classification_salary)
high_salary_range_logical <- highest_range_salary == (cali_employees$max_classification_salary - cali_employees$min_classification_salary)
position_high_salary_range <- cali_employees$position[high_salary_range_logical]
print(position_high_salary_range)

rm(list = ls())  #Clearing Environment
cali_employees <- read.table(file = "http://tinyurl.com/pdxdgrx",
                             header = TRUE,
                             sep = "\t",
                             stringsAsFactors = FALSE)

top_bottom_5 <- quantile(cali_employees$total_wages,c(0.05,0.95))
print(top_bottom_5)
top_5_logical <- cali_employees$total_wages <= 252
bottom_5_logical <- cali_employees$total_wages >= 95053.4
top_bottom_5_logical <- top_5_logical | bottom_5_logical
cali_employees$total_wages[top_bottom_5_logical==TRUE] <- NA
print(length(which(is.na(cali_employees$total_wages))))
##10534

rm(list = ls())  #Clearing Environment
cali_employees <- read.table(file = "http://tinyurl.com/pdxdgrx",
                             header = TRUE,
                             sep = "\t",
                             stringsAsFactors = FALSE)

percent_5 <- as.integer(0.05 * length(cali_employees$total_wages))
top_5_values <- tail(sort(cali_employees$total_wages), percent_5)
bottom_5_values <- head(sort(cali_employees$total_wages), percent_5)
top_5_values_logical <- is.element(cali_employees$total_wages,top_5_values)
bottom_5_values_logical <- is.element(cali_employees$total_wages,bottom_5_values)
top_bottom_5_logical <- top_5_values_logical | bottom_5_values_logical
cali_employees$total_wages[top_bottom_5_logical==TRUE] <- NA
print(length(which(is.na(cali_employees$total_wages))))
##10533
