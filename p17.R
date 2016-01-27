# [1] 21124
# user  system elapsed 
# 0.031   0.000   0.030 

main <- function(){
        pmt <- proc.time()
        len <- 0
        
        one_19 <- c("","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen",
        "fourteen","fifteen","sixteen","seventeen","eighteen","nineteen")
        
        twenty_90 <- c("twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety")
        
        one_99 <- one_19
        
        for(i in twenty_90){
              for(j in one_19[1:10]){
                      one_99 <- c(one_99, paste(i,j,sep = "") )              
              }  
        }
        
        hundred <- "hundred"
        and <- "and"
        
        onehundred_999 <- one_99
        
        for(i in one_19[2:10]){
                for(j in one_99){
                        if(j==""){
                                 onehundred_999 <- c(onehundred_999, paste(i,hundred,sep = "") )            
                        }else{
                                onehundred_999 <- c(onehundred_999, paste(i,hundred,and,j,sep = "") )            
                        }
                }
        }
        
        onethousand <- c(onehundred_999,"onethousand")
        
        onethousand <- paste(onethousand,collapse = "")
        
        #nchar("string") returns length of string
        print(nchar(onethousand))
        
        print(proc.time()-pmt)
}