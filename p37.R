#found the first 10 quickly and took a long time for the 11th
#23     37     53     73    313    317    373    797   3137   3797 739397 
# [1] 748317
# user  system elapsed 
# 50.765   0.000  50.624

main <- function(){
    pmt <- proc.time()
    library(numbers)
    pri <- 7
    count <- 0
    myTruncatables <- vector(mode = "integer")
    repeat{
        pri <- nextPrime(pri)
        if(truncatable(pri)){
            myTruncatables <- c(myTruncatables, pri)
            count <- count + 1
            print(paste("count =",count))
            print(paste("prime =",pri))
            if(count==11){
                break
            }
        }
    }
    print(myTruncatables)
    print(sum(myTruncatables))
    
    proc.time()-pmt
}

#take prime, return TRUE if it' a left & right truncatable prime
truncatable <- function(a){
    b <- sepDig(a)
    l <- length(b)
    if( any(b[2:l]%%2==0) || any(b[2:l]==5) ){
        return(FALSE)
    }
    
    truncs <- vector(mode = "integer")
    for(i in 1:l){
        truncs <- c(truncs, as.numeric(paste(b[1:i],sep = "",collapse = "")))
        truncs <- c(truncs, as.numeric(paste(b[i:l],sep = "",collapse = "")))
    }
    if(sum(isPrime(truncs))==2*l){ #TRUE=1, if all true
        TRUE
    }else{FALSE}
}

sepDig <- function(x){
    as.numeric(strsplit(as.character(x),"")[[1]])
}