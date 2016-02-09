# [1] 510510
# user  system elapsed 
# -0.003   0.000  -0.002 

pmt <- proc.time()
library(pracma)
x <- primes(30)
y <- cumprod(x)
print(max(y[y<1e6]))
print(pmt-proc.time())