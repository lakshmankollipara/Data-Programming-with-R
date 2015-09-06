rm(list = ls())  #Clearing Environment

##Q1- Loading Data into Data Frame
expr <- read.table(file = "http://teaching.cgrb.oregonstate.edu/CGRB/summer_15/_downloads/expr_long_2_fixed.txt",
                   header = TRUE,
                   sep = "\t",
                   stringsAsFactors = FALSE)
###
##Deciding good IDs
good_ids <- expr$id[expr$expression > median(expr$expression)]

##Filtering out Good IDs
##Works perfectly fine and ofcourse better way to do the job.
##%in% takes each element on LHS and check the presence in RHS. Returns a logical vector, from which row_selector selects True rows from data frame.
expr1 <- expr[expr$id %in% good_ids, ]
###
## Doesnt give error.
##But, Gives vague results, takes first row in expr$id and checks with first row in good_ids, second with second and so on.
expr2 <- expr[expr$id == good_ids, ]
###
##Doesnt give error.
##But gives empty output, Row selector should be a row name or row number or a logical vector.
## But, here we passed a list of ids which is invalid.
expr3 <- expr[good_ids, ]
###
##Works fine.Does the same job as of expr1, subset is used for non standard evaluation
##Which means, the condition "id %in% good_ids" is verified and a logical vector is created.
##And applies that logical vector to expr.
expr4 <- subset(expr, id %in% good_ids)
###
## Doesnt give error.subset does same job as in expr 4
##But, Gives vague results, takes first row in expr$id and checks with first row in good_ids, second with second and so on. Whic is not apt in this case.
expr5 <- subset(expr, id == good_ids)
###
##Throws an error. Because subset accepts only an expression as its 2nd argument and creates a logical vector.
##But Here, we passed a character list which is not an expression to convert into logical vector
print("ERROR:") #expr6 <- subset(expr, good_ids)
###
##Q2
##Function numeric_only takes data frame as argument and applies is.numeric on on all columns and returns a data frame with only numeric values.
numeric_only <- function(df){

  sslist <- lapply(df,is.numeric)
  ssvec <- unlist(sslist)
  return (df[ssvec])
}

df1 <- data.frame(id = c("PRQ", "XL2", "BB4"), val = c(23,45.6,62))
df2 <- data.frame(srn = c(4461,5144), name = c("Mel","Ben"), age = c(27,24))
numeric_only(df1)
numeric_only(df2)
###
##Q3
##ttest_pval function runs for 10000(for each value in the alist, runs once) times and calculates 10000 p-values as a list, which is converted to vector and represented as histogram.
##Here x doesnt do anything, this function works fine, even if we remove x.
##x will have some importance if we use x in the function definition.
##In this case, If we don't give x, It considers elements of alist as mean1 values passed to the function i.e ttest_pval(1) calls funtion with mean1 as 1.
## So, we need x, such that elements of sequence wont mess up with mean1 value.
##If we change mean1 = 0.1 in lapply, It calls the function ttest_pval on alist with mean1 = 0.1 every time instead of mean1 = 0.0 in function arguments.
ttest_pval <- function(x, mean1 =0, mean2 =0){
 vec1<- rnorm(100,mean = mean1, sd =1)
 vec2<- rnorm(100,mean = mean2, sd =1)
 ttest <- t.test(vec1,vec2,paired = TRUE)
 return (ttest[[3]])
}
alist <- as.list(seq(1:10000))
pvals <- lapply(alist, ttest_pval, mean1 = 0.0, mean2 = 0.0)
pvals1 <- lapply(alist, ttest_pval, mean1 = 0.1, mean2 = 0.0)
pvals <- unlist(pvals)
pvals1 <- unlist(pvals1)
hist(pvals)
hist(pvals1)
###
##Q4
##Loading Data into cali_employees
cali_employees <- read.table(file = "http://tinyurl.com/pdxdgrx",
                             header = TRUE,
                             sep = "\t",
                             stringsAsFactors = FALSE)

library(dplyr)

##Q4a
##Used order() instead of comparing with Max() because == in numeric sometimes gives incorrect results
##mean_total_wages function takes data frame as input and returns mean of total wages of Lecturer- Range 1 employees
mean_total_wages <- function(df){
  total_wages <- df$total_wages[df$position == "Lecturer - Academic Year, Range 1"]
  mean_total_wages <- mean(total_wages)
  return(data.frame(mean_total_wages = mean_total_wages))
}
data_by_county <- group_by(cali_employees, entity_county)
total_wages_by_County_NaN <- do(data_by_county,mean_total_wages(.))
total_wages_by_County <- total_wages_by_County_NaN[!is.nan(total_wages_by_County_NaN$mean_total_wages), ]
print(total_wages_by_County)
sorted_wages <- total_wages_by_County$mean_total_wages[order(total_wages_by_County$mean_total_wages)]
sorted_counties <- total_wages_by_County$entity_county[order(total_wages_by_County$mean_total_wages)]
print(sorted_counties[length(sorted_counties)])
###
##Q4b
cali_emp <- subset(cali_employees,position == "Lecturer - Academic Year, Range 1")
data_by_county <- group_by(cali_emp, entity_county)
total_wages_by_County <- summarise(data_by_county, mean_total_wages = mean(total_wages))
print(total_wages_by_County)
sorted_wages <- total_wages_by_County$mean_total_wages[order(total_wages_by_County$mean_total_wages)]
sorted_counties <- total_wages_by_County$entity_county[order(total_wages_by_County$mean_total_wages)]
print(sorted_counties[length(sorted_counties)])
###