# [1] 171
# user  system elapsed 
# 0.021   0.004   0.025 

pmt <- proc.time()

library(lubridate)

startDate <- ymd("1901/1/1")
endDate <- ymd("2000/12/31")
interv <- interval(startDate,endDate)
tlen <- time_length(interv,"day") #36524
dates <- seq(startDate, length = tlen, by = "day")
num <- length(dates[intersect(which(wday(dates)==7),which(mday(dates)==1))])

print(num)
print(proc.time()-pmt)