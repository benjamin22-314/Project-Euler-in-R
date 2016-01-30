#        [,1] [,2]
# [1,]   16   64
# [2,]   19   95
# [3,]   26   65
# [4,]   49   98
# [1] 1/100
# user  system elapsed 
# 0.180   0.000   0.179 


main <- function(){
        pmt <- proc.time()
        library(MASS)
        dcfs <- vector(mode = "integer")
        for(a in 10:98){
                for(b in (a+1):99){
                        a1 <- sepDig(a)[1]
                        a2 <- sepDig(a)[2]
                        b1 <- sepDig(b)[1]
                        b2 <- sepDig(b)[2]
                        q <- a/b
                        if(q==(a1/b1) && a2==b2 && a%%10!=0){#|| q==(a1/b2) || q==(a2/b1) || q==(a2/b2) ){
                                dcfs <- rbind(dcfs,c(a,b))
                        } 
                        if(q==(a1/b2) && a2==b1 && a%%10!=0){
                                dcfs <- rbind(dcfs,c(a,b))
                        }
                        if(q==(a2/b1) && a1==b2 && a%%10!=0){
                                dcfs <- rbind(dcfs,c(a,b))
                        }
                        if(q==(a2/b2) && a1==b1 && a%%10!=0){
                                dcfs <- rbind(dcfs,c(a,b))
                        }
                }
        }
        print(dcfs)
        print(fractions(prod(dcfs[,1])/prod(dcfs[,2])))
        # 1/100
        proc.time()-pmt
}


sepDig <- function(x){
        xn <- as.numeric(strsplit(as.character(x),"")[[1]])
}