
rm(list=ls())
library(data.table)
library(ggplot2)

if (substr(Sys.info()["user"],4,5)=="33") {
  rootdir <- "M:/professional/race/thecounted-data/"
} 

## read in and format data from The Counted
d <- fread(paste0(rootdir,"the-counted-2016.csv"))
d2 <- fread(paste0(rootdir,"the-counted-2015.csv"))
d <- rbind(d,d2)
d[age=="Unknown",age:=NA]
d[age=="40s",age:="45"]
d[,age:=as.numeric(age)]


## read in standard life expectancy file
le <- fread(paste0(rootdir,"FINAL_min_pred_ex.csv"))
setnames(le,"Pred_ex","ylls")

## create YLLs
d <- merge(d,le,by="age",all.x=T)
if (nrow(d[!is.na(age) & is.na(ylls)]) > 0) stop("predex didn't merge for some ages")
write.csv(d,paste0(rootdir,"counted_ylls.csv"),row.names=F)

## tabulate age
tab <- copy(d)
tab[,n:=1]
setkey(tab,gender,raceethnicity,armed)
tab <- tab[,list(avg_age=mean(age,na.rm=T),n=sum(n),ylls=sum(ylls,na.rm=T)),by=key(tab)] ## FIXME: counting missings in n


##
armed <- as.matrix(table(d$armed,d$raceethnicity))
tots <- apply(armed,MARGIN=2,sum)
test <- t(apply(armed,MARGIN=1,FUN=function(x){x/tots}))


