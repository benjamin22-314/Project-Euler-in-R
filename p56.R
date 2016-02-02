# > main()
# [1] "i^j is max for i = 99  and j= 95"
# [1] "maximum digital sum is 972"
# user  system elapsed 
# 0.379   0.000   0.376

main <- function(){
    pmt <- proc.time()
    library(gmp)
    a <- (2:99)
    #b <- outer(a,a,"^") outer doesn't work for bigz
    maxi <- 0
    maxj <- 0
    maxx <- 0
    for(i in a){
        for(j in a){
            if(digSum(pow.bigz(i,j))>maxx){
                maxx <- digSum(pow.bigz(i,j))
                maxi <- i
                maxj <- j
            }
        }
    }

    print(paste("i^j is max for i =",maxi," and j=",maxj))
    print(paste("maximum digital sum is",digSum(pow.bigz(maxi,maxj))))
    print(proc.time()-pmt)
}

digSum <- function(num){
    sum(as.integer(strsplit(as.character(num),"")[[1]]))
}