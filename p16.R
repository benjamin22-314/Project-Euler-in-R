# [1] 1366
# user  system elapsed 
# 0.440   0.001   0.440 


main <- function(){
        pmt <- proc.time()
        power <- 1000
        
        fred <- rep(0,400) #2^1000 has 301 digits
        
        fred[length(fred)] <- 2
        
        for(i in 1:(power-1)){
                
                fred <- fred*2
                for(j in length(fred):1){
                        if(fred[j]>=10){
                                fred[j-1] <- fred[j-1] + 1
                                fred[j] <- fred[j] - 10
                               # print(fred)
                        }
                }
        }
       # print(fred)
        print(sum(fred))
        print(proc.time()-pmt)
}