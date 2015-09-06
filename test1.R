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
