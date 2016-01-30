# [1] 73682
# user  system elapsed 
# 0.01    0.00    0.01 

main <- function(){
    pmt <- proc.time()
        target <- 200
        coins <- c(1,2,5,10,20,50,100,200)
        ways <- rep(0,target+1)
        ways[1] <- 1
        
        for(coin in coins){
                for(i in seq(coin,target)){
                        ways[i+1] <- ways[i+1] + ways[i+1-coin]
                }
        }
        print(ways[target+1])
        print(proc.time()-pmt)
}