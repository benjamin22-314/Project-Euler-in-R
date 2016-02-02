# [1] "player 1 wins 376 times"
# user  system elapsed 
# 1.719   0.000   1.703 

main <- function(){
        pmt <- proc.time()
        cards <- scan("/home/ben/R/Euler/p054_poker.txt", what="character",
                      sep = ",", na.strings = "")
        #for each row, first 5 cards are player 1, last 5 are player 2
        #return(cards)
        rounds <- sapply(cards, whoWins, USE.NAMES = FALSE)
        #print(rounds)
        print(paste("player 1 wins", length(rounds[rounds==1]), "times"))
        proc.time()-pmt
}

#calculate who wins the hand. return 1 for p1 winning and 2 for p2 winning and 3 for a draw
whoWins <- function(hands){ #hands is the vector holding p1 and p2 's cards
        #hands <- "7C 5H KC QH JD AS KH 4C AD TS" #just for testing
        hands <- face2Num(splitChars(hands))
        p1 <- hands[1:10]
        p2 <- hands[11:20]
        
        #test for royal flush
        p1RoyalFlush <- isRoyalFlush(p1)
        p2RoyalFlush <- isRoyalFlush(p2)
        if( p1RoyalFlush[[1]] && !p2RoyalFlush[[1]] ){return(1)} #p1 wins
        if( !p1RoyalFlush[[1]] && p2RoyalFlush[[1]] ){return(2)} #p2 wins
        if( p1RoyalFlush[[1]] && p2RoyalFlush[[1]] ){ 
                print("draw")
                return(3)
        }
        
        #test for straight flush
        p1StraightFlush <- isStraightFlush(p1)
        p2StraightFlush <- isStraightFlush(p2)
        if( p1StraightFlush[[1]] && !p2StraightFlush[[1]] ){return(1)} #p1 wins
        if( !p1StraightFlush[[1]] && p2StraightFlush[[1]] ){return(2)} #p2 wins
        if( p1StraightFlush[[1]] && p2StraightFlush[[1]] ){
                if(p1StraightFlush[[2]]==p2StraightFlush[[2]]){
                        print("draw")
                        return(3)        
                }else if(p1StraightFlush[[2]]>p2StraightFlush[[2]]){return(1) #p1 wins
                }else if(p1StraightFlush[[2]]<p2StraightFlush[[2]]){return(2)} #p1 wins        
        }
        
        #test for four of a kind
        p1FourOfAKind <- isFourOfAKind(p1)
        p2FourOfAKind <- isFourOfAKind(p2)
        if( p1FourOfAKind[[1]] && !p2FourOfAKind[[1]] ){return(1)} #p1 wins
        if( !p1FourOfAKind[[1]] && p2FourOfAKind[[1]] ){return(2)} #p2 wins
        if( p1FourOfAKind[[1]] && p2FourOfAKind[[1]] ){
                if(p1FourOfAKind[[2]]==p2FourOfAKind[[2]]){
                        print("draw") #shouldn't be able to draw with only one pack
                        return(3)        
                }else if(p1FourOfAKind[[2]]>p2FourOfAKind[[2]]){return(1) #p1 wins
                }else if(p1FourOfAKind[[2]]<p2FourOfAKind[[2]]){return(2)} #p2 wins        
        }
        
        #test for full house
        p1FullHouse <- isFullHouse(p1)
        p2FullHouse <- isFullHouse(p2)
        if( p1FullHouse[[1]] && !p2FullHouse[[1]] ){return(1)} #p1 wins
        if( !p1FullHouse[[1]] && p2FullHouse[[1]] ){return(2)} #p2 wins
        if( p1FullHouse[[1]] && p2FullHouse[[1]] ){
                if(p1FullHouse[[2]]==p2FullHouse[[2]]){
                        print("draw") #shouldn't be able to draw with only one pack
                        return(3)        
                }else if(p1FullHouse[[2]]>p2FullHouse[[2]]){return(1) #p1 wins
                }else if(p1FullHouse[[2]]<p2FullHouse[[2]]){return(2)} #p2 wins        
        }
        
        #test for flush
        p1Flush <- isFlush(p1)
        p2Flush <- isFlush(p2)
        if( p1Flush[[1]] && !p2Flush[[1]]){return(1)} #p1 wins
        if( !p1Flush[[1]] && p2Flush[[1]]){return(2)} #p2 wins
        if( p1Flush[[1]] && p2Flush[[1]]){
                for(i in length(p1Flush[[2]]):1 ){
                        if(p1Flush[[2]][i]>p2Flush[[2]][i]){return(1)} #p1 wins
                        if(p1Flush[[2]][i]<p2Flush[[2]][i]){return(2)} #p2 wins
                }
                print("draw")
                return(3)
        }
        
        #test for straight
        p1Straight <- isStraight(p1)
        p2Straight <- isStraight(p2)
        if( p1Straight[[1]] && !p2Straight[[1]]){return(1)} #p1 wins
        if( !p1Straight[[1]] && p2Straight[[1]]){return(2)} #p2 wins
        if( p1Straight[[1]] && p2Straight[[1]]){
                if(p1Straight[[2]]>p2Straight[[2]]){return(1)} #p1 wins
                if(p1Straight[[2]]<p2Straight[[2]]){return(2)} #p2 wins
                print("draw")
                return(3)
        }
        
        #test for three of a kind
        p1ThreeOfAKind <- isThreeOfAKind(p1)
        p2ThreeOfAKind <- isThreeOfAKind(p2)
        if( p1ThreeOfAKind[[1]] && !p2ThreeOfAKind[[1]] ){return(1)} #p1 wins
        if( !p1ThreeOfAKind[[1]] && p2ThreeOfAKind[[1]] ){return(2)} #p2 wins
        if( p1ThreeOfAKind[[1]] && p2ThreeOfAKind[[1]] ){
                if(p1ThreeOfAKind[[2]]==p2ThreeOfAKind[[2]]){
                        print("draw") #shouldn't be able to draw with only one pack
                        return(3)        
                }else if(p1ThreeOfAKind[[2]]>p2ThreeOfAKind[[2]]){return(1) #p1 wins
                }else if(p1ThreeOfAKind[[2]]<p2ThreeOfAKind[[2]]){return(2)} #p2 wins        
        }
        
        #test for two pairs
        p1TwoPairs <- isTwoPairs(p1)
        p2TwoPairs <- isTwoPairs(p2)
        if( p1TwoPairs[[1]] && !p2TwoPairs[[1]] ){return(1)} #p1 wins
        if( !p1TwoPairs[[1]] && p2TwoPairs[[1]] ){return(2)} #p2 wins
        if( p1TwoPairs[[1]] && p2TwoPairs[[1]] ){
                #print(p1TwoPairs[[2]])
                #print(p2TwoPairs[[2]])
                for(i in length(p1TwoPairs[[2]]):1){
                        #print(p1TwoPairs[[2]][i])
                        #print(p2TwoPairs[[2]][i])
                        if(p1TwoPairs[[2]][i]>p2TwoPairs[[2]][i]){return(1)} #p1 wins
                        if(p1TwoPairs[[2]][i]<p2TwoPairs[[2]][i]){return(2)} #p2 wins
                }
                print("draw")
                return(3)
        }
        
        #test for one pair
        p1OnePair <- isOnePair(p1)
        p2OnePair <- isOnePair(p2)
        if( p1OnePair[[1]] && !p2OnePair[[1]] ){return(1)} #p1 wins
        if( !p1OnePair[[1]] && p2OnePair[[1]] ){return(2)} #p2 wins
        if( p1OnePair[[1]] && p2OnePair[[1]] ){
                #print(p1OnePair[[2]])
                #print(p2OnePair[[2]])
                for(i in length(p1OnePair[[2]]):1){
                        #print(p1OnePair[[2]][i])
                        #print(p2OnePair[[2]][i])
                        if(p1OnePair[[2]][i]>p2OnePair[[2]][i]){return(1)} #p1 wins
                        if(p1OnePair[[2]][i]<p2OnePair[[2]][i]){return(2)} #p2 wins
                }
                print("draw")
                return(3)
        }
        
        #test for highest cards
        p1HighCards <- highCards(p1)
        p2HighCards <- highCards(p2)
        for(i in length(p1HighCards):1){
                if(p1HighCards[i]>p2HighCards[i]){return(1)} #p1 wins
                if(p1HighCards[i]<p2HighCards[i]){return(2)} #p2 wins
        }
        print("draw")
        return(3)
        
        
}


highCards <- function(vec){
        nums <- sort(as.numeric(vec[seq(1,length(vec),2)]))
}


isOnePair <- function(vec){
        nums <- sort(as.numeric(vec[seq(1,length(vec),2)]))
        for(i in seq(2,14)){
                if(sum(nums==i)==2){
                        return(list(TRUE, c(nums[-which(nums %in% i)],i)))#return the numbers in ascending order 
                        #except the one pair number is at the end
                }
        }
        return(list(FALSE, numeric(0)))
}


isTwoPairs <- function(vec){
        nums <- sort(as.numeric(vec[seq(1,length(vec),2)]))
        returnvec <- vector()
        for(i in seq(2,14)){
                if(sum(nums==i)==2){
                        returnvec <- c(returnvec, i)#
                }
        }
        if(length(returnvec)==2){
                return(list(TRUE, c( nums[-which(nums %in% returnvec)], returnvec) ) ) #return the single
                # value followed by the two pairs ascending order
        }else{
                return(list(FALSE, numeric(0)))        
        }
        
}

# three cards of the same value.
isThreeOfAKind <- function(vec){
        nums <- sort(as.numeric(vec[seq(1,length(vec),2)]))
        for(i in seq(2,14)){
                if(sum(nums==i)==3){
                        return(list(TRUE, i))#return single value
                }
        }
        return(list(FALSE, numeric(0)))
}

#Three of a kind and a pair
isFullHouse <- function(vec){
        nums <- sort(as.numeric(vec[seq(1,length(vec),2)]))
        k <- list(FALSE, numeric(0))
        for(i in seq(2,14)){
                if(sum(nums==i)==3){
                        k <- list(TRUE, i)#store the value of three of a kind
                        break
                }
        }
        if(k[[1]]){
                xnums <- nums[nums!=k[[2]]]
                #print(xnums)
                #xvec should be length 2
                if(xnums[1]==xnums[2]){
                        return(list(TRUE, i))
                }
        }
        return(list(FALSE, numeric(0)))
}

# Four cards of the same value.
isFourOfAKind <- function(vec){
        nums <- sort(as.numeric(vec[seq(1,length(vec),2)]))
        for(i in seq(2,14)){
                if(sum(nums==i)==4){
                        return(list(TRUE, i))#return single value
                }
        }
        return(list(FALSE, numeric(0)))
}

# Ten, Jack, Queen, King, Ace, in same suit.
isRoyalFlush <- function(vec){
        #is a straight flush and contains an ace
       if(isStraightFlush(vec)[[1]] && any(as.numeric(vec[seq(1,length(vec),2)])==14)){
               return(TRUE)
       }else{return(FALSE)}
}

#All cards are consecutive values of same suit. 
isStraightFlush <- function(vec){
        x <- isStraight(vec)
        if( x[[1]] && isFlush(vec)[[1]]){
                return(list(TRUE, x[[2]]))#return highest num
        }else{
                return(list(FALSE, numeric(0)))
        }
}

#is it a straight (all in sequence)
isStraight <- function(vec){
        nums <- sort(as.numeric(vec[seq(1,length(vec),2)]))
        #print(nums)
        #print(nums[1])
        for(i in length(nums):2 ){
                if( (nums[i]-nums[i-1]) != 1){
                        return(list(FALSE, numeric(0)))
                }
        }
        return(list(TRUE, nums[length(nums)]))#return highest number
}

#is it a flush (same suit)
isFlush <- function(vec){
        suits <- vec[seq(2,length(vec),2)]
        if(all(suits==suits[1])){
                return(list(TRUE, sort(as.numeric(vec[seq(1,length(vec),2)]))) )#return nums, lowest to highest
        }else{
                return(list(FALSE, numeric(0)))
        }
}


#turn ten, jack, queen, king and ace into 10,11,12,13 and 14. (still characters)
face2Num <- function(vec){
        vec[vec=="T"] <- "10"
        vec[vec=="J"] <- "11"
        vec[vec=="Q"] <- "12"
        vec[vec=="K"] <- "13"
        vec[vec=="A"] <- "14"
        vec
}

splitChars <- function(chars){
        tmp <- as.vector(strsplit(chars, split = ""))
        tmp[[1]][tmp[[1]]!=" "]
}



tests <- function(){
        
        print(isFlush(face2Num(splitChars("JC AC AC JC JC")))[[1]])
        print(!isFlush(face2Num(splitChars("JD AC AC JC JC")))[[1]])
        print(isFullHouse(face2Num(splitChars("JD AC AC JC JC")))[[1]])
        print(isFullHouse(face2Num(splitChars("JD AC AC JC JC")))[[2]])
        print(!isFullHouse(face2Num(splitChars("JD 2C AC JC JC")))[[1]])
        print(!isFullHouse(face2Num(splitChars("JD AC AC 5C JC")))[[1]])
        print(isTwoPairs(face2Num(splitChars("AD AC 5S 9C 9H")))[[1]])
        print(isTwoPairs(face2Num(splitChars("AD AC 5S 9C 9H")))[[2]])
        print(isOnePair(face2Num(splitChars("AD AC 5S 9C 8H")))[[1]])
        print(isOnePair(face2Num(splitChars("AD AC 5S 9C 8H")))[[2]])
        #print(isOnePair(face2Num(splitChars("AD AC 5S 9C 8H"))))
        print(highCards(face2Num(splitChars("AD 3C 5S 9C 8H"))))
        print(whoWins("AD 3C 5S 9C 8H AC KC QC JC TC"))
        print(whoWins("AD 3C 5S 9C 8H 2C 2D 3C 3D TC"))
        print(whoWins("2C 2D 3C 3D TC AD 3C 5S 9C 8H"))
        print("pp")
        print(whoWins("2C 2D 3C 3D TC AD 2C 2S 8C 8H"))
        print(whoWins("2C 2D 8C 8D TC AD 2C 2S 8C 8H"))
        print("ww")
        print(whoWins("2C 2D 4C 8D TC AD 2C 5S 8C 8H"))
        print(whoWins("2C 2D 4C 5D TC TD 3C 5S 2C 2H"))
        print(whoWins("2C KD 4C AD TC TD 3C 5S AC 2H"))
        print(whoWins("TD 3C 5S AC 2H TD 3C 5S AC 2H"))
        
}