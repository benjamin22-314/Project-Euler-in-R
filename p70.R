# [1] "minRatio = 1.00070905112481 n = 8319823"
# user  system elapsed 
# 1.001   0.000   0.998


main <- function(){
    pmt <- proc.time()
    library(numbers)
    
    num <- 1e7
    upper <- ceiling(sqrt(1e7))+1000
    lower <- upper-2000
    x <- Primes(lower, upper)
    nSave <- 0
    minRatio <- 10
    for(i in seq(length(x)-1)){
        for(j in seq(i+1,length(x))){
            if( x[i]*x[j] < num){
                if( sepSortDig(x[i]*x[j]) == sepSortDig(sPhi(x[i],x[j])) ){
                    if( x[i]*x[j]/sPhi(x[i],x[j]) < minRatio){
                        nSave <- x[i]*x[j]
                        minRatio <- x[i]*x[j]/sPhi(x[i],x[j])
                        #print(paste("minRatio =", minRatio,"n =",nSave))
                    }
                }
            }
        }
    }
    print(paste("minRatio =", minRatio,"n =",nSave))
    print(proc.time()-pmt)
}

# special phi function for this problem. Not generalised
sPhi <- function(p1,p2){
    as.integer((p1-1)*(p2-1))
}

sepSortDig <- function(x){
    as.numeric(paste(sort(unlist(strsplit(as.character(x),split = "")), decreasing = TRUE),collapse = ""))
}