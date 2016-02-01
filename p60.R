
# [1] "The 5 prime pairs are 13 5197 5701 6733 8389"
# [1] "and they sum to 26033"
# user  system elapsed 
# 52.092   0.000  52.032 

#I changed upper <- 1e5, to see how long it would take
# [1] "The 5 prime pairs are 3 3119 9887 36263 48731"
# [1] "and they sum to 98003"
# user   system  elapsed 
# 1622.759    3.010 1629.926  (27 minutes)

main <- function(){
        pmt <- proc.time()
        library(numbers)
        upper <- 10000
        assign("myPrimes", Primes(upper)[-c(1,3)], envir = .GlobalEnv)
        
        for(pri in myPrimes){
                if(findPairs(pri)){break}
        }
        
        #findPairs(13)
        
        proc.time()-pmt
}

findPairs <- function(prime){
        qq <- myPrimes[mapply(checkPrimePair, prime, myPrimes)]
        #qq <- qq[qq>prime]
        #print(qq)
        len <- length(qq)
        for(i in 1:(len-3)){
                for(j in (i+1):(len-2)){
                      if(checkPrimePair(qq[i],qq[j])){
                               for(k in (j+1):(len-1)){
                                       if(checkPrimePair(qq[i],qq[k]) && checkPrimePair(qq[j],qq[k])){
                                               for(l in (k+1):len){
                                                       if(checkPrimePair(qq[i],qq[l]) && checkPrimePair(qq[j],qq[l]) && checkPrimePair(qq[k],qq[l])){
                                                               print(paste("The 5 prime pairs are",prime,qq[i],qq[j],qq[k],qq[l]))
                                                               print(paste("and they sum to",sum(prime,qq[i],qq[j],qq[k],qq[l])))
                                                               return(T)
                                                       }
                                               }
                                       }
                               }
                       } 
                }
        }
        return(F)
}


checkPrimePair <- function(x,y){#check if x and y are a prime pair set
        
        xy <- unSepDig(c(x,y))
        yx <- unSepDig(c(y,x))
        #print(xy)
        #print(yx)
        if(isPrime(xy) && isPrime(yx)){
                return(T)
        }else{
                return(F)
        }
}

sepDig <- function(x){
        as.numeric(strsplit(as.character(x),"")[[1]])
}
unSepDig <- function(x){
        as.numeric(paste(x,sep = "",collapse = ""))
}
