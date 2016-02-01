# [1] "spiral length of 26241"
# user  system elapsed 
# 10.540   0.176  10.718 

#688590081 highest number counted
# > sqrt(7e8)
# [1] 26457.51
# > sqrt(1e9)
# [1] 31622.78
#runs in half the time if high <- 3e4
# but theres no way to no the limit before the calculation.
library(numbers)
high <- 1e5
pris <- Primes(high)

main <- function(){
    pmt <- proc.time()
    library(pracma)
    
    a <- 1
    ps <- 0
    tots <- 1
    i <- 1
    repeat{
        for(j in 1:4){
            a <- a+2*i
            #print(a)
            if(j==4){#bottom diagonal is always a square
                tots <- tots+4    
                next
            }
            if(a<high){
                if(a %in% pris){
                    ps <- ps+1
                }    
            }else if(!any(a%%pris==0)){
                ps <- ps+1
            }
        }
        if( (ps/tots) < 0.1 ){break}
        i <- i+1
    }
    print(paste("spiral length of",i*2+1))
    print(proc.time()-pmt)
    
}
