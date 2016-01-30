# [1] 9183
# user  system elapsed 
# 0.00    0.00    0.01 

pmt <- proc.time()
ab <- length(unique(c(outer(2:100, 2:100, "^"))))

print(ab)
print(proc.time()-pmt)

