#1533776805
# 0.18 seconds
#forum; all Hex nums are triangle nums, but not all Triangle numbers are Hex nums

pmt <- proc.time()

isPen <- function(num){
        ((1+sqrt(1+24*num))/6)%%1==0
}

isHex <- function(num){
        ((1+sqrt(1+8*num))/4)%%1==0
}


count <- 285 #two triangle numbers before this 1,40755

repeat{
        count <- count+1
        t <- count*(count+1)/2
        if(isPen(t) && isHex(t)){
                break
        }
}
print(count)
print(t)
print(proc.time()-pmt)


