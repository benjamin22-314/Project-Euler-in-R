# [1] 5777
# user  system elapsed 
# 0.083   0.000   0.082


# isprime(x) (gmp) returns 2 if abs(x) is prime, 0 otherwise

pmt <- proc.time()
library(pracma)

squares <- c(1,4,9)

i <- 33
repeat{
    i <- i+2
    if(isprime(i)){next}
    while(squares[length(squares)]*2<i){
        squares <- c(squares,(length(squares)+1)^2)
    }
    if(sum(isprime(i-2*squares[squares<(i*0.5)]))==0){break}
    
}

print(i)
print(proc.time()-pmt)