#121313 where the replaced digits are the first, third and fifth
# [1] "x = 121313"
# [1]  20303 121313 222323 323333 424343 525353 626363 727373 828383 929393
# [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
# user  system elapsed 
# 11.376   0.000  11.335 

main <- function(){
    pmt <- proc.time()
    library(numbers)
    x <- Primes(1e5,1e6)
    #print(proc.time()-pmt)
    blss <- bools6(6)
    #print(length(x))
    z <- sapply(x,digReps)
    xz <- x[z]
    #print(length(xz))
    for(k in seq(length(xz))){
        #if(k%%100==0){print(k)}
        if(check6s(xz[k],blss)){
            break
        }
    }
    print(proc.time()-pmt)
}

check6s <- function(x,bls){
    # bls <- bools6(6)
    for(i in seq(length(bls))){
        if(sum(isPrime(getReps(x,bls[i][[1]])))==8){
            #&&
            if(all(getReps(x,bls[i][[1]])[isPrime(getReps(x,bls[i][[1]]))]>100000)){
                print(paste("x =", x))
                print(getReps(x,bls[i][[1]]))
                print(isPrime(getReps(x,bls[i][[1]])))
                return(TRUE)
            }
        }
    }
    return(FALSE)
}


digReps <- function(x){
    y <- as.numeric(strsplit(as.character(x),"")[[1]])
    y <- y[1:(length(y)-1)]
    for(i in seq(0,3)){
        if(sum(y==i)>2){
            return(TRUE)
        }
    }
    return(FALSE)
}

bools6 <- function(num){
    library(combinat)
    bls <- list()
    bb <- rep(F,num)
    for(i in seq(num-1)){
        bb[i] <- T
        bls <- c(bls,unique(permn(bb)))
    }
    #print(bls)
    k <- vector(mode = "numeric")
    for(i in seq(length(bls))){
        if(bls[[i]][num]){
            k <- c(k,i)
        }
    }
    bls <- bls[-k]
    bls
}

check5s <- function(x){
    library(numbers)
    bls <- bools6(5)
    for(i in seq(length(bls))){
        if(sum(isPrime(getReps(x,bls[i][[1]])))==7){
            print(paste("x =", x))
            print(getReps(x,bls[i][[1]]))
            print(isPrime(getReps(x,bls[i][[1]])))
            return(TRUE)
        }
    }
    return(FALSE)
}

#seems to work well
getReps <- function(x,bool){
    x <- sepDig(x)
    sos <- vector(mode = "numeric")
    for(i in seq(0,9)){
        temp <- vector(mode = "numeric")
        for(j in seq(length(x))){
            if(bool[j]){
                temp <- c(temp,i)
            }else{
                temp <- c(temp,x[j])
            }
        }
        sos <- c(sos,as.numeric(paste(temp,sep = "",collapse = "")))
    }
    sos
}

