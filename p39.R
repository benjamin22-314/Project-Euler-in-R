#p = 840
#no. of sols = 8
#time = 0.162 seconds

main <- function(){
        pmt <- proc.time()
        pp <- 1001 #less than p
        b <- seq(pp) #c=b
        bc <- outer(b,b,"getP")
        
        pbc <- isInt(bc)
        pss <- bc[which(pbc==1)]
        pss <- pss[pss<pp]
        print(table(pss)[table(pss)==max(table(pss))]/2)
        #p is top number, number of sols is half bottom number.
        proc.time()-pmt
}

#{20,48,52}, {24,45,51}, {30,40,50}
getP <- function(bb,cc){
        sqrt(bb^2 + cc^2) + bb + cc
}

getA <- function(bb,cc){
        sqrt(bb^2 + cc^2)
}

isInt <- function(x){# returns 0 for non-int and 1 for integers
        floor(floor(x)/x)
}