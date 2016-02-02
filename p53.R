# [1] 4075
# user  system elapsed 
# 0.400   0.000   0.399

main <- function(){
    pmt <- proc.time()
    library(combinat)
    count <- 0
    for(n in seq(100)){
        for(m in seq(100)){
            if(nCm(n,m)>1e6){
                count <- count+1
            }
        }
    }
    print(count)
    proc.time()-pmt
}