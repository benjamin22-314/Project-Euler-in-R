
pmt <- proc.time()
count <- 0

for(a in seq(9) ){
    b <- 1
    repeat{
        if(ceiling(log(a^b,10))==b){
            count <- count+1
            b <- b+1
        }else{break}
    }
}
#log(1^1,10)=0 so need to add 1
print(count+1)
print(proc.time()-pmt)