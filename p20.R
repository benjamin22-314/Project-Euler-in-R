# [1] 648
# user  system elapsed 
# 0.001   0.000   0.001

pmt <- proc.time()
library(gmp)

print(sum(as.numeric(unlist(strsplit(as.character(factorialZ(100)),split="")))))
print(proc.time()-pmt)


