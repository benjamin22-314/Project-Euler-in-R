# [1] "997651 can be written as the sum of 543 consecutive primes"
# user  system elapsed 
# 0.342   0.000   0.340 

main <- function(){
    pmt <- proc.time()
    library(numbers)

    consecLen <- 0
    num <- 0
    upper <- 1e6
    x <- Primes(upper)
    
    #length should be at least 21 (from question)
    x <- revisePrimes(22,x,upper)
    
    for(i in seq(length(x))){
        if((length(x)-i)<consecLen){break}
        y <- cumsum(x[i:length(x)])
        y <- y[y<upper] #order is the same, just the end is dropped
        if(consecLen < max(which(isPrime(y)))){
            consecLen <- max(which(isPrime(y)))
            num <- y[consecLen]
            x <- revisePrimes(consecLen,x,upper)
        }
    }
    
    print(paste(num,"can be written as the sum of",consecLen,"consecutive primes"))
    print(proc.time()-pmt)
}

#Every time we get a new consecLen, we can lower the upper bound.
#
revisePrimes <- function(a,x,upper){
    if(a>=length(x)){return(x)}
    z <- rev(x)
    for(i in seq(length(z))){
        if(sum(z[i:(i+a)])<upper){break}
    }
    x <- rev(z[i:length(z)])
}
