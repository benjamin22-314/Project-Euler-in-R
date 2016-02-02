# [1] "5027^3 = "127035954683"
# user  system elapsed 
# 0.636   0.004   0.639

main <- function(){
    pmt <- proc.time()
    
    matches <- 5
    i <- 1
    temp <- 0
    l <- 0
    repeat{
        i <- i+1
        print(paste("i =",i))
        #get my cubes of length i
        myCubes <- getCubicNumbers(i)
        
        myCubesDigitSort <- sapply(myCubes, sortCubeDigits)
        #how many have the same digits
        cubeTable <- table(myCubesDigitSort)
        
        if(length(names(cubeTable[cubeTable==matches]))){
            temp <- mapply(hasSameDigits,myCubes,names(cubeTable[cubeTable==matches]))
            sol <- min(myCubes[temp])
            break
        }
    }
    print(paste(sol^(1/3),"^3 = ",as.character(sol),sep = ""))
    print(proc.time()-pmt)
}

sortCubeDigits <- function(x){
    paste(sort(unlist(strsplit(as.character(x),split="")),decreasing = TRUE),collapse = "")
}

getCubicNumbers <- function(ndigit) {
    lower <- floor(10^((ndigit-1)/3))
    upper <- floor(10^(ndigit/3))
    cube <- (lower:upper)^3
    return(cube)
}

hasSameDigits <- function(a,b){
    a <- paste(sort(unlist(strsplit(as.character(a),split = ""))),collapse = "")
    b <- paste(sort(unlist(strsplit(as.character(b),split = ""))),collapse = "")
    a==b#identical(a,b)
}

