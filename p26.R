# [1] 983
# user  system elapsed 
# 0.084   0.012   0.095 

#only look at primes, composites will have the same number of recurring digits
# number of recuring decimals cannot be more than the number-1. Count backwards from 1000

main <- function(){
    pmt <- proc.time()
    library(numbers)
    p <- sort(Primes(1000), decreasing = TRUE)
    maxrd <- 0
    for(i in p){
        rd <- recurringDigits(i)
        if(rd>maxrd){
            maxrd <- rd
            maxPrime <- i
        }
        if(maxrd>i){break}
    }
    
    print(maxPrime)
    print(proc.time()-pmt)
}


recurringDigits <- function(d){
    x <- 1
    remainders <- c(x)
    repeat{
        x <- (x*10)%%d
        if(x %in% remainders){break}
        remainders <- c(remainders,x)
    }
    return(length(remainders) - which(remainders==x) + 1)
}