# [1] 4782
# user  system elapsed 
# 0.088   0.000   0.088 

pmt <- proc.time()
library(gmp)

f <- as.bigz(1)
ff <- f
i <- 2
repeat{
    i <- i+1
    fff <- add.bigz(f,ff)
    f <- ff
    ff <- fff
    if(ceiling(log(fff,10))==1000){break}
}

print(i)
print(proc.time()-pmt)