#5482660
# 37.4 seconds

penty <- function(){
        pmt <- proc.time()
        upto <- 10000 # num of numbers
        k <- 0
        for(p1 in seq(upto-1)){
                for(p2 in seq(p1+1,upto)){
                        #print(paste(p1,p2))
                        if( isPen(givePent(p2)-givePent(p1)) ){
                                if( isPen(givePent(p2)+givePent(p1)) ){
                                        k <- 1
                                        break
                                }
                        }
                }
                if(k){break}
        }
        print(givePent(p2)-givePent(p1))
        proc.time()-pmt
}

givePent <- function(n){
        n*(3*n - 1)/2
}

isPen <- function(num){
        ((1+sqrt(1+24*num))/6)%%1==0
}

sepDig <- function(x){
        as.numeric(strsplit(as.character(x),"")[[1]])
}
