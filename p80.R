# [1] 40886
# user  system elapsed 
# 0.187   0.000   0.183 

main <- function(){
    pmt <- proc.time()
    count <- 0
    mySquares <- seq(10)^2
    for(i in seq(100)){
        #not clear in the question, but don't include squares.
        if(i %in% mySquares){next()}
        count <- count + digSum(i)
    }
    print(count)
    print(proc.time()-pmt)
}



digSum <- function(x){
    a <- strsplit(capture.output(sqrt(mpfr(x,350))),"")[[2]]
    a <- a[-c(1,2,3,4,6)]
    a <- sum(as.integer(a[1:100]))
}



