# [1] 134043
# user  system elapsed 
# 6.753   0.004   6.754

main <- function(){
    pmt <- proc.time()
    library(numbers)
    i <- 1000
    ll <- 4
    count <- 0
    repeat{
        i <- i+1
        
        if(length(unique(factorize(i)))==ll){
            count <- count+1
        }else{
            count <- 0
        }
        
        if(count==ll){break}
    }
    
    print(i-ll+1)
    print(proc.time()-pmt)
}

