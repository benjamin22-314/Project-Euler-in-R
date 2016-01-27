#main 
# [1] 837799
# user  system elapsed 
# 8.597   0.006   8.607 

# > main2()
# [1] 837799
# user  system elapsed 
# 13.162   0.038  13.206 

main <- function(){
    
    pmt <- proc.time()
    
    num <- 1e6
    x <- integer(num)
    x[1] <- 1
    for(i in 2:num){
        #print(i)
        n <- i
        terms <- 0
        repeat{
            if(n<i){
                x[i] <- x[n]+terms
                break
            }
            if(n%%2==0){
                n <- n/2
            }else{
                n <- 3*n + 1
            }
            terms <- terms+1
        }
    }
    print(which(x==max(x)))
    print(proc.time()-pmt)
    
}


main2 <- function(){
    
    pmt <- proc.time()
    
    num <- 1e6
    x <- integer(num)
    x[1] <- 1
    for(i in seq(3,1e6,2)){
        #print(i)
        n <- i
        terms <- 0
        repeat{
            #print(n)
            #print(x[n])
            if(!is.na(x[n])){
                if(x[n]!=0){
                    x[i] <- x[n]+terms
                    break
                }
            }
            
            if(n%%2==0){
                n <- n/2
            }else{
                n <- 3*n + 1
            }
            terms <- terms+1
        }
    }
    print(which(x==max(x)))
    print(proc.time()-pmt)
    
}


collatz <- function(n){
    terms <- 1
    repeat{
        if(n==1){return(terms)}
        if(n%%2==0){
            n <- n/2
        }else{
            n <- 3*n + 1
        }
        terms <- terms+1
    }
}