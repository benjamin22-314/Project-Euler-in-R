# [1] 153
# user  system elapsed 
# 0.081   0.000   0.080 


main <- function(){
    pmt <- proc.time()
    upto <- 1000
    count <- 0
    #there is a pattern. 
    # 3/2 7/5 17/12 41/29 99/70
    # n(k+1) = n(k) + 2d(k)
    # d(k+1) = n(k) + d(k) OR d(k+1) = n(k+1) - d(k)
    #where n is numerator, d is demoninator and d(k) is denominator subscript k
    n <- 3
    n <- gmp::as.bigz(n)
    d <- 2
    d <- gmp::as.bigz(d)
    for(i in 2:(upto)){
        n <- n + 2*d
        d <- n - d
        #print(paste("n / d =",n,"/",d))
        if(ceiling(log(n,10)) > ceiling(log(d,10))){
            count <- count + 1
        }
    }
    print(count)
    print(proc.time()-pmt)
}

