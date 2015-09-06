rm(list = ls())  #Clearing Environment

##Function_Paired_test
##Arguments_Data Frame
##Return Value_Data Frame with P_value & Test name
##Definition- Runs Shapiro Test on Dataframe columns and eterines which test to perform (t.test() or wilcox.test())
paired_test <- function(df)
{
  diff <- (df[[1]] - df[[2]])
  if(shapiro.test(diff)[[2]] < 0.1)
  {
    x <- wilcox.test(df[[1]], df[[2]], paired = TRUE)[[3]]
    y<- "wilcox_median"
    z <- data.frame(pvalue = x, test = y)
    return (z)
  }else
  {
    x <- t.test(df[[1]], df[[2]], paired = TRUE)[[3]]
    y <- "ttest_mean"
    z <- data.frame(pvalue = x, test = y)
    return (z)
  }
}
## End of function_Paired_test
##Data Frame with two normally distributed columns
df_norm <- data.frame(col1 = rnorm(100, mean = 5, sd = 2), col2 = rnorm(100, mean = 10, sd = 2))
res_norm <- paired_test(df_norm)

##Data Frame with two non-normally distributed columns
df_nonnorm <- data.frame(firstcol = rexp(100, rate = 2.5), secondcol = rexp(100, rate = 1.3))
res_nonnorm <- paired_test(df_nonnorm)

print(res_norm)
print(res_nonnorm)