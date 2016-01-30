# [1] "a*b = -59231"
# user  system elapsed 
# 32.434   0.000  32.444 

main <- function(){
    library(pracma)
    pmt <- proc.time()
    aa <- seq(-999,999,2) #a has to be odd
    bb <- primes(1000) #b must be a prime
    maxCount <- 0
    maxA <- 0
    maxB <- 0
    for(a in aa){
        for(b in bb[1+a+bb>0]){
            n <- 0
            while(isP(n^2 + a*n + b)){
                n <- n+1
            }
            if(n>maxCount){
                maxCount <- n
                maxA <- a
                maxB <- b
            }
        }
    }
    print(paste("a*b =",maxA*maxB))
    print(proc.time()-pmt)
}

isP <- function(n){
    if(n<1){return(FALSE)}
    return(isprime(n))
}