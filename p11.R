# [1] 70600674
# user  system elapsed 
# 0.120   0.000   0.115 


pmt <- proc.time()
x <- read.table("~/R/ProjectEuler/p11.txt")
#print(class(x))

#vertical, horizontal, diagonal right, diagonal left

largest <- 0

#horizontal and vertical
for(i in 1:20){
    for(j in 1:17){
        #horizontal
        a <- x[i,j]*x[i,j+1]*x[i,j+2]*x[i,j+3]
        #print(a)
        if(a>largest){
            largest <- a
        }
        #vertical
        b <- x[j,i]*x[j+1,i]*x[j+2,i]*x[j+3,i]
        #print(b)
        if(b>largest){
            largest <- b
        }
    }
}

#diagonals right and left
for(i in 1:17){
    for(j in 1:17){
        #diagonal right
        c <- x[i,j]*x[i+1,j+1]*x[i+2,j+2]*x[i+3,j+3]
        #print(c)
        if(c>largest){
            largest <- c
        }
        #diagonal left
        d <- x[i,21-j]*x[(i+1),21-(j+1)]*x[(i+2),21-(j+2)]*x[(i+3),21-(j+3)]
        #print(d)
        if(d>largest){
            largest <- d
        }
    }
}

print(largest)
print(proc.time()-pmt)

