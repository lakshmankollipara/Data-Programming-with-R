rm(list =ls())
storage <- numeric(100)
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
    #temp <- c(temp,storage[n])
    #print(temp)
  }
  return(storage[n])
}
fib_recursive(50)
print(storage)