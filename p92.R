# [1] 8581146
# user  system elapsed 
# 1.816   0.000   1.787 

main <- function(){
    pmt <- proc.time()
    
    #squareDigits(9999999)=567
    #one we have all these, every number above will be one calculation only
    ub <- 567
    uvec <- vector(mode = "integer",length = ub)
    uvec[1] <- 1
    uvec[89] <- 89
    
    for(i in seq(2,ub)){
        x <- i
        vecs <- c()
        repeat{
            if(x>ub){
                x <- squareDigits(x)
                next
            }
            if(uvec[x]==0){
                vecs <- c(vecs,x)
                x <- squareDigits(x)
            }else{
                uvec[vecs] <- uvec[x]
                break
            }
        }
    }
    
    #make permutations instead of counting. It reduces the search space to 11440
    # a<=b<=c<=d<=e<=f<=g
    count <- 0
    for(a in 0:9){
        for(b in a:9){
            for(c in b:9){
                for(d in c:9){
                    for(e in d:9){
                        for(f in e:9){
                            for(g in f:9){
                                if(g==0){next()}
                                if(uvec[sum(c(a,b,c,d,e,f,g)^2)]==89){
                                    count <- count + factorial(7)/prod(factorial(table(c(a,b,c,d,e,f,g))))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
   
    print(count)
    print(proc.time()-pmt)
}

squareDigits <- function(x){
    sum(as.numeric(unlist(strsplit(as.character(x),"")))^2)
}