# [1] "x = 142857"
# user  system elapsed 
# 6.753   0.000   6.741

main <- function(){
    
    pmt <- proc.time()
    x <- 100
    repeat{
        x <- x+1
        if(!hasLeading1(x)){
            x <- 10^(length(strsplit(as.character(x),"")[[1]]))
            next
        }
        if(hasSameDigits(x,6*x)){
            if(hasSameDigits(x,5*x)){
                if(hasSameDigits(x,4*x)){
                    if(hasSameDigits(x,3*x)){
                        if(hasSameDigits(x,2*x)){
                            print(paste("x =",x))
                            break
                        }
                    }
                }
            }
        }
        
        #if(x%%100==0){print(x)}
    }
    print(proc.time()-pmt)
}

hasSameDigits <- function(a,b){
    a <- sort(as.integer(strsplit(as.character(a),"")[[1]]))
    b <- sort(as.integer(strsplit(as.character(b),"")[[1]]))
    identical(a,b)
}

hasLeading1 <- function(a){
    as.numeric(strsplit(as.character(a),"")[[1]])[1]==1
}
