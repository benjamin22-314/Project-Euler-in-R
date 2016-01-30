# [1] 669171001
# user  system elapsed 
# 0.021   0.000   0.020 

#What is the sum of the numbers on the 
#well, 1001 by 1001 means 500 spirals

pmt <- proc.time()
spirals <- 500
nums <- c(1)

for(i in 1:spirals){
    for(j in 1:4){
        nums <- c(nums,nums[length(nums)]+2*i)
    }
}
print(sum(nums))
print(proc.time()-pmt)

