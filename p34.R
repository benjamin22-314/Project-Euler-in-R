# [1]   145 40585
# [1] 40730
# user  system elapsed 
# 10.326   0.000  10.315 
# 0.45 seconds if I change the upper bound

main <- function(){
        pmt <- proc.time()
        a <- seq(11,50000)#1000000)
        b <- lapply(a,sepDigFacSum)
        print((a[a==b]))
        #145 and 40585
        print(sum(a[a==b]))
        #40730
        print(proc.time()-pmt)
}

sepDigFacSum <- function(x){
        sum(factorial(as.numeric(strsplit(as.character(x),"")[[1]])))
}