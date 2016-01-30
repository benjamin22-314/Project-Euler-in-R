# [1] 4179871
# user  system elapsed 
# 33.115   0.850  33.979 

main <- function(){
    pmt <- proc.time()        
    #find the abundant numbers between 1 and 28123
    len <- 28123
    nums <- 1:len
    #if abundant, add to list
    sums <- apply(as.matrix(nums), 1, d)
    nums[(sums/nums)<=1] <- 0
    nums <- nums[nums!=0]
    #now nums is a list of abundant numbers (starting with 12)
    
    #find all pairs of these numbers
    coms <- combn(nums,2)
    pairs <- coms[1,]+coms[2,]
    #don't forget to include pairing every number with itself
    pairs <- c(pairs, nums*2)
    #print(pairs)
    #find all numbers between 1:28123 not in pairs and sum them together
    temp <- 1:len
    temp <- temp[!temp%in%pairs]
    #find the sum
    #print(temp)
    print(sum(temp))
    print(proc.time()-pmt)
}


d <- function(x){
    
    xHalf <- floor(x/2)
    a <- seq(1:xHalf)
    x <- rep(x,xHalf)
    b <- x%%a #if b[i] is 0, then i is a divisor of x 
    factors <- a[b==0]
    sum(factors)
}