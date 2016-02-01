#162
#0.1 seconds
chamo <- function(){
        pmt <- proc.time()
        words <- scan("~/R/ProjectEuler/p042_words.txt", what="character",
                      sep = ",", na.strings = "")
        #they're all capitals, only letters
        wordVals <- sapply(words,wordValue)
        
        #print(wordVals[which(wordVals==(max(wordVals)))]) 
        #highest value is 145, SYLVESTER
        
        triangle <- triangleNums() #triange numbers up to 210 (length 20)
        
        print(length(wordVals[wordVals %in% triangle]))
        
        proc.time()-pmt
}

triangleNums <- function(){
        sapply(seq(20),function(x) 0.5*x*(x+1) )
}
#1   3   6  10  15  21  28  36  45  55  66  78  91 105 120 136 153 171 190 210

wordValue <- function(word){
        sum(sapply(sepDig(word),letterValue))
}

letterValue <- function(lett){
        which(lett==LETTERS)
}

sepDig <- function(x){
        strsplit(as.character(x),"")[[1]]
}