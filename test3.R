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
storage <- numeric(10)
storage[1:length(storage)] <- NA
fib_recursive <- function(n){
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
    storage[n] <<- (fib_recursive(n - 1) + fib_recursive(n - 2)) 
    #temp <- c(temp,fibvals[n])
    #print(temp)
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

##Calling function
#fib_array(10)
#fib_recursive(10)
#fib_iterative(10)
#input_size <- seq(from = 10, to = 30, by = 10)
#print(input_size)
#fib_array_n <- lapply(input_size,fib_array)
#fib_iterative_n <- lapply(input_size,fib_iterative)
#print(fib_array_n)
#print(fib_iterative_n)

#fib_recursive_n <- lapply(input_size,fib_recursive)
#print(fib_recursive_n)


library(microbenchmark)
elapsed_time <- function(data_by_input, fib_array, fib_iterative, fib_recursive){
  n <- data_by_input$input
  #print(n)
  elapsed <- microbenchmark(fib_array(n),fib_iterative(n),fib_recursive(n),unit = "ms", times = 1)
  ret <- data.frame(run_time = elapsed$time, algorithm = as.character(elapsed$expr))
  return(ret)
}
library(dplyr)
run_time <- data.frame(input = seq(from = 100, to = 2000, by = 100))
data_by_input <- group_by(run_time, input)
runtime_data <- do(data_by_input,elapsed_time(., fib_array,fib_iterative,fib_recursive))
print(runtime_data)
runtime_log_data <- data.frame(input= log10(runtime_data$input), run_time = log10(runtime_data$run_time), algorithm = runtime_data$algorithm)
print(runtime_log_data)
library(ggplot2)
p <- ggplot(runtime_log_data) + geom_point(aes(x = input, y = run_time, color = algorithm)) + 
  geom_smooth(aes(x = input, y = run_time, color = algorithm), method= "lm") 
plot(p)



######
print(run_time)
library(microbenchmark)
for(i in size){
  elapsed <- microbenchmark(fib_array(i),fib_iterative(i),fib_recursive(i),unit = "ms", times = 1)
  #elapsed <- microbenchmark(fib_array(i),fib_iterative(i),unit = "ms", times = 1)
  elapsed <- unclass(elapsed)
  elapsed1 <- data.frame(Input_Size = i, Algorithm = elapsed$expr, Run_Time = elapsed$time)
  run_time[nrow(run_time)+1, ] <- c(log10(i), log10(elapsed$time[1]), elapsed$expr[1])
  run_time[nrow(run_time)+1, ] <- c(log10(i), log10(elapsed$time[2]), elapsed$expr[2])
  run_time[nrow(run_time)+1, ] <- c(log10(i), log10(elapsed$time[3]), elapsed$expr[3])
}
print(run_time)
run_time_log_data <- data.frame(input = log10(run_time$input), run_time = log10(run_time$))
library(ggplot2)
p <- ggplot(runtime_data) + geom_line(aes(x = Input, y = Run_Time, color = Algorithm)) +
  geom_smooth(aes(x = Input, y = Run_Time, color = Algorithm))
plot(p)
