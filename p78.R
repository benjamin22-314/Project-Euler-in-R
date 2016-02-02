# [1] 55374
# user  system elapsed 
# 43.852   0.000  43.117 

#1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56
main <- function(){
        pmt <- proc.time()
        #get some pentagonal numbers, is 300 enough?
        num <- 300
        k <- vector(mode = "numeric")
        for(j in seq(num)){
                k <- c(k,0.5*j*(3*j-1))
                k <- c(k,0.5*-j*(3*-j-1))
        }
        sgn <- c(-1,1,1,-1)
        m <- 1e7
        p <- c(1) #p0=1
        n <- 1
        repeat{
                px <- 0
                i <- 1
                while(k[i]<=n){
                        px <- px + sgn[i%%4+1]*p[n-k[i]+1]
                        i <- i+1
                }
                p <- c(p,px%%m)
                if(px%%1e6==0){break}
                n <- n+1
        }
        
        print(n)
        print(proc.time()-pmt)
}
