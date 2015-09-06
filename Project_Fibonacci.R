rm(list = ls())##Clearing Environment

##Fibonacci_Iterative_using Array
fib_array <- function(n){
  fibvals <- numeric(n)
  if(n == 1){
    fibvals[1] <- 0
  }else if(n ==2){
    fibvals[1] <- 0
    fibvals[2] <- 1
  }else{
    fibvals[1] <- 0
    fibvals[2] <- 1
    for (i in 3:n) { 
      fibvals[i] <- fibvals[i-1]+fibvals[i-2]
    }
  }
  return(fibvals)
}

##Fibonacci_using Recursion
fib_recursive <- function(n){
  fibvals <- numeric(n)
  fib_recursive_cal <- function(n){ 
    if (n == 1) return(0) 
    if (n == 2) return(1) 
    return (fib_recursive_cal(n - 1) + fib_recursive_cal(n - 2)) 
  }
  for(i in 1:n)
  {
    fibvals[i] <- fib_recursive_cal(i)
  } 
  return(fibvals)
}

##Fibonacci using Recursion and Memoization
##Golbal vector to store results of sub problem
storage <- numeric(10000)
storage[1:length(storage)] <- NA
fib_recursive_mem <- function(n){
  temp <- numeric()
  if(is.na(storage[n])){ 
    if (n == 1) {
      storage[n] <<- 0
      return(0)
    } else if (n == 2){
      storage[1] <<- 0
      storage[n] <<- 1
      return(1)
    } 
    storage[n] <<- (fib_recursive_mem(n - 1) + fib_recursive_mem(n - 2)) 
    #print(storage[!is.na(storage)])
  }
  return(storage[n])
}

##fib_Iterative_without_array
fib_iterative <-function(n){
  first <- 0
  second <- 1
  fibvals <- numeric(n)
  for(i in 1:n){
    if(i == 1){
      next_e <- 0
    }
    else if(i ==2){
      next_e <- 1
    }
    else{
      next_e <- first + second
      first <- second
      second <- next_e
    }
    fibvals[i] <- next_e 
  }
  return(fibvals)
}


library(microbenchmark)
elapsed_time <- function(data_by_input, fib_array, fib_iterative, fib_recursive_mem){
  n <- data_by_input$input
  elapsed <- microbenchmark(fib_array(n),fib_iterative(n),fib_recursive_mem(n),unit = "ms", times = 1)
  ret <- data.frame(run_time = elapsed$time, algorithm = as.character(elapsed$expr))
  return(ret)
}
library(dplyr)
run_time <- data.frame(input = seq(from = 100, to = 2000, by = 100))
data_by_input <- group_by(run_time, input)
runtime_data <- do(data_by_input,elapsed_time(., fib_array,fib_iterative,fib_recursive_mem))
#print(runtime_data)
runtime_log_data <- data.frame(input= log10(runtime_data$input), run_time = log10(runtime_data$run_time), algorithm = runtime_data$algorithm)
#print(runtime_log_data)
library(ggplot2)
p <- ggplot(runtime_log_data) + geom_point(aes(x = input, y = run_time, color = algorithm)) + 
  geom_smooth(aes(x = input, y = run_time, color = algorithm), method= "lm") 
plot(p)

elapsed_time_rec <- function(data_by_input_rec, fib_recursive){
  n <- data_by_input_rec$input
  elapsed_rec <- microbenchmark(fib_recursive(n),unit = "ms", times = 1)
  ret_rec <- data.frame(run_time = elapsed_rec$time, algorithm = as.character(elapsed_rec$expr))
  return(ret_rec)
}
library(dplyr)
run_time_rec <- data.frame(input = seq(from = 5, to = 40, by = 5))
data_by_input_rec <- group_by(run_time_rec, input)
runtime_data_rec <- do(data_by_input_rec,elapsed_time_rec(., fib_recursive))
library(ggplot2)
p_rec <- ggplot(runtime_data_rec) + geom_point(aes(x = input, y = run_time, color = algorithm)) + 
  geom_smooth(aes(x = input, y = run_time, color = algorithm), method= "loess", span = 1) 
plot(p_rec)