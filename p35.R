# [1] "Number of circular primes below one million is 55"
# user  system elapsed 
# 2.534   0.000   2.514 

main <- function(){
    pmt <- proc.time()
    library(pracma)
    p <- primes(1e6)
    p <- p[sapply(p, containsComposite)]
    p <- p[sapply(p,isCircular)]
    
    print(paste("Number of circular primes below one million is", length(p)+2))#plus 2 and 5
    print(proc.time()-pmt)    
}

containsComposite <- function(x){
    x <- as.numeric(unlist(strsplit(as.character(x),"")))
    if( any(x%%2==0) || any(x==5) ){
        return(FALSE)
    }
    return(TRUE)
}

isCircular <- function(x){
    x <- as.numeric(unlist(strsplit(as.character(x),"")))
    l <- length(x)
    x <- c(x,x)
    for(i in 2:(l) ){
        if(!isprime(as.numeric(paste(x[i:(i+l-1)],sep = "",collapse = "")))){return(FALSE)}
    }
    return(TRUE)
}