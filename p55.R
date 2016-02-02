# "number of Lychrel Numbers is 249"
# user  system elapsed 
# 1.933   0.000   1.920 

main <- function(){
        pmt <- proc.time()
        upper <- 9999
        myCounts <- vector()
        for(i in seq(upper)){
                i <- sepDig(i)
                myCounts <- c(myCounts, getPalindroneCount(i))
        }
        print(paste("number of Lychrel Numbers is", length(myCounts[myCounts==50])))
        #print(myCounts)
        hist(myCounts, breaks=50)
        proc.time()-pmt
}


getPalindroneCount <- function(x){#x is a vector of digits
        upp <- 50
        for(count in seq(upp)){
                x <- vecAdd(x,rev(x))
                if(isPalindrone(x)){break}
        }
        count
}

isPalindrone <- function(x){#x is a vector of digits of the number
        all( x==rev(x) )
}

sepDig <- function(x){
        as.numeric(strsplit(as.character(as.bigz(x)),"")[[1]])
}
unSepDig <- function(x){
        as.numeric(paste(x,sep = "",collapse = ""))
}

#vector Addition
vecAdd <- function(x,y){
        lx <- length(x)
        ly <- length(y)
        if(lx>ly){
                y <- c(rep(0,lx-ly),y)
        }else if(lx<ly){
                x <- c(rep(0,ly-lx),x)
        }
        xpy <- x+y
#         print(paste("x =",x))
#         print(paste("y =",y))
#         print(paste("xpy =",xpy))
        if(length(xpy)==1){
                if(xpy[1]>9){
                        xpy[1] <- xpy[1]%%10
                        xpy <- c(1,xpy)
                }
                return(xpy)
        }
        for(i in length(xpy):2){
                if(xpy[i]>9){
                        xpy[i] <- xpy[i]%%10
                        xpy[i-1] <- xpy[i-1]+1
                }
        }
        if(xpy[1]>9){
                xpy[1] <- xpy[1]%%10
                xpy <- c(1,xpy)
        }
        xpy
}