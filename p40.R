# [1] 1 5 3 7 2 1
# [1] 210
# user  system elapsed 
# 1.294   0.000   1.291

main <- function(){
    
    pmt <- proc.time()
    d <- 10
    sum <- 0
    dv <- c()
    i <- as.integer(1)
    repeat{
        l <- length(sepDig(i))
        sum <- sum + l
        if(sum>=d){
            #print(i)
            dv <- c(dv,sepDig(i)[l-(sum-d)])
            d <- d*10
        }
        if(d==1e7){break}
        #need to keep this as an integer or
#         Warning message:
#             In sepDig(i) : NAs introduced by coercion
        i <- as.integer(i+1)
    }   
    print(dv)
    print(prod(dv))
    print(proc.time()-pmt)
}

sepDig <- function(x){
    as.numeric(strsplit(as.character(x),"")[[1]])
}