# [1] 31626
# user  system elapsed 
# 1.686   0.000   1.662 
#  220  284 1184 1210 2620 2924 5020 5564 6232 6368

main <- function(){
    pmt <- proc.time()
    library(gmp)
    s <- 1:10000
    www <- Vectorize(ww)
    x <- s[www(www(s))==s]
    #we still have numbers that are paired with themselves
    x <- x[www(x)!=x]
    #OK
    print(x)
    print(sum(x))
    print(proc.time()-pmt)
}

#works
ww <- function(n){
    sum(seq(n/2)[rep(n,n/2)%%seq(n/2)==0])
}