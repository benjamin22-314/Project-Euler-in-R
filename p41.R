# [1] 7652413
# user  system elapsed 
# 0.334   0.000   0.333 
# Nine numbers cannot be done (1+2+3+4+5+6+7+8+9=45 => always dividable by 3)
# Eight numbers cannot be done (1+2+3+4+5+6+7+8=36 => always dividable by 3)


main <- function(){
    pmt <- proc.time()
    library(numbers)
    upper <- 7654321 #987654321 #largest 9 digit pandigital
    repeat{
        upper <- previousPrime(upper)
        if(isPan(upper)){
            print(upper)
            break
        }
    }
    print(proc.time()-pmt)
}

isPan <- function(z){
    y <- sepDig(z)
    ll <- seq(length(y))
    bool <- 0
    for(i in ll){
        if(i %in% y){
            bool <- bool + 1
        }
    }
    if(bool==length(y)){
        return(TRUE)
    }else{FALSE}
}

sepDig <- function(x){
    as.numeric(strsplit(as.character(x),"")[[1]])
}

