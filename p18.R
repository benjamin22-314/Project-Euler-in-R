# [1] 1074
# user  system elapsed 
# 0.038   0.000   0.037 


main <- function(){
    pmt <- proc.time()
    colNum <- 15
    x <- read.table("~/R/ProjectEuler/p018_triangle.txt", sep = " ", col.names=paste("V", 1:colNum), fill = TRUE)
    
    gir <- dim(x)[1]
    
    for(i in (gir-1):1){
        for(j in i:1){
            x[i,j] <- x[i,j] + max(x[i+1,j],x[i+1,j+1])
        }       
    }
    print(x[1,1])
    print(proc.time()-pmt)
}

