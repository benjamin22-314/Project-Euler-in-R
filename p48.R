#9110846700
# 0.36 seconds, not too shabby :)

main <- function(){
    pmt <- proc.time()
    upto <- 1000
    tots <- 0
    for(i in seq(upto)){
        tots <- tots + bigSelfPower(i)
        tots <- tots%%1e10
    }
    print(tots)
    proc.time()-pmt
}

bigSelfPower <- function(x){
    #x^x only last ten digits
    sum <- 1
    for(i in seq(x)){
        sum <- sum*x
        sum <- sum%%1e10
    }
    sum
}



# Big Integer ('bigz') :
#     [1] 9110846700
# user  system elapsed 
# 0.020   0.000   0.019
# using gmp big integer package
main2 <- function(){
    pmt <- proc.time()
    library(gmp)
    
    x <- (sum(as.bigz(1:1000)^(1:1000))%%1e10)
    
    print(x)
    print(proc.time()-pmt)
    
}

