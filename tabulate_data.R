## Matthew Coates
## Counted

rm(list=ls())
library(data.table)
library(ggplot2)

d <- fread("M:/professional/race/thecounted-data/the-counted-2016.csv")
d2 <- fread("M:/professional/race/thecounted-data/the-counted-2015.csv")
d <- rbind(d,d2)
d[age=="Unknown",age:=NA]
d[age=="40s",age:="45"]
d[,age:=as.numeric(age)]

## tabulate age
tab <- copy(d)
tab[,n:=1]
setkey(tab,gender,raceethnicity,armed)
tab <- tab[,list(avg_age=mean(age,na.rm=T),n=sum(n)),by=key(tab)]
write.csv(tab,"M:/professional/race/thecounted-data/summary_counted.csv",row.names=F)

armed <- as.matrix(table(d$armed,d$raceethnicity))
tots <- apply(armed,MARGIN=2,sum)
test <- t(apply(armed,MARGIN=1,FUN=function(x){x/tots}))

