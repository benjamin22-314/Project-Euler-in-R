# [1] "76576500 is the 12375 th triangel number and the first to have over 500 divisors"
# user  system elapsed 
# 3.006   0.000   2.985

pmt <- proc.time()

numberFactors <- function(x) {
    library(gmp)
    #returns the number of factors of x
    x <- table(as.numeric(factorize(x)))
    #print(x)
    prod(x+1)
}

ss <- 0
i <- 0
repeat{
    i <- i+1
    ss <- ss+i
    if(numberFactors(ss)>500){break}
}

print(paste(ss,"is the",i,"th triangel number and the first to have over 500 divisors"))
print(proc.time()-pmt)