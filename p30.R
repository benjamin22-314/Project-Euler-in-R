#     [1]   4150   4151  54748  92727  93084 194979
#     [1] 443839
#     user  system elapsed 
#     12.021   0.000  11.972 

main <- function(){
    pmt <- proc.time()
    q <- seq(11,1000000)
    is5V <- Vectorize(isFifthPowerSum)
    matches <- q[is5V(q)]        
    print(matches)
    print(sum(matches))
    print(proc.time()-pmt)
}

isFifthPowerSum <- function(x){
    if(x==sum(sepDig(x)^5)){
        TRUE
    }else{
        FALSE
    }
}

sepDig <- function(x){
    as.numeric(strsplit(as.character(x),"")[[1]])
}