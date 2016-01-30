#872187
# 18.17 seconds

main <- function(){
        pmt <- proc.time()
        a <- seq(1,1000000)
        b <- sapply(a,isPal)
        c <- a[b] #base10 pals
        d <- sapply(c,myIntToBit)
        e <- sapply(d,isPal)
        print(sum(c[e]))
        print(proc.time()-pmt)
}

#can't handle large numbers
num2bit <- function(num){
        as.integer(paste(rev(as.integer(intToBits(num))), collapse=""))
}

#my home grown function for large numbers
myIntToBit <- function(num){
        vec <- vector(mode = "integer")
        repeat{
                vec <- c(vec, num%%2)
                num <- floor(num/2)
                if(num<2){break}
        }
        (paste(rev(c(vec,1)), collapse = ""))#used to return an int, but had rounding errors
}

isPal <- function(gate){
        gate <- as.character(gate)
        identical(gate, paste(rev(strsplit(gate, "")[[1]]), collapse=""))
}