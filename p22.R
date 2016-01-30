#871198282
# user  system elapsed 
# 0.113   0.000   0.110

main <- function(){
    pmt <- proc.time()
    myNames <- scan("~/R/ProjectEuler/p022_names.txt", what="character",
                  sep = ",", na.strings = "")
    
    myNames <- strsplit(sort(unlist(myNames)),split = "")
    
    z <- 1:26
    names(z) <- LETTERS
    
    total <- sapply(myNames, function(i) sum(z[i]))
    total <- total*(1:length(total))
    
    print(sum(total))
    print(proc.time()-pmt)
    
}

