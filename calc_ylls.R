
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

## read in standard life expectancy file and format
le <- fread(paste0(rootdir,"FINAL_min_pred_ex.csv"))
setnames(le,"Pred_ex","ylls")

## read in census file and format
cen <- fread(paste0(rootdir,"/census.csv"))
##cen <- melt(cen,id.vars=c("origin","race","sex","year"))
## for now, just make all-age rates
cen <- cen[,c("origin","race","sex","year","total_pop"),with=F]
cen[sex==1 & origin==0 & year==2015 & race==8]$total_pop




## create YLLs
d <- merge(d,le,by="age",all.x=T)
if (nrow(d[!is.na(age) & is.na(ylls)]) > 0) stop("predex didn't merge for some ages")

## create rates



#write.csv(d,paste0(rootdir,"counted_ylls.csv"),row.names=F)

## tabulate age
tab <- copy(d)
tab[,n:=1]
setkey(tab,raceethnicity,year)
tab <- tab[,list(avg_age=mean(age,na.rm=T),n=sum(n),ylls=sum(ylls,na.rm=T)),by=key(tab)] ## FIXME: counting missings in n


1000000*584/cen[sex==0 & origin==1 & year==2015 & race==1]$total_pop
## White, non-hispanic, both sexes, "white alone" 2.944 compared to 2.95


1000000*574/cen[sex==0 & origin==1 & year==2016 & race==1]$total_pop
## White, non-hispanic, both sexes, "white alone" 2.89 compared to 2.9


1000000*307/cen[sex==0 & origin==1 & year==2015 & race==2]$total_pop
## black, non-hispanic, both sexes, "black alone" 7.717 compared to 7.69


1000000*266/cen[sex==0 & origin==1 & year==2016 & race==2]$total_pop
## black, non-hispanic, both sexes, "black alone" 6.627 compared to 6.66


1000000*307/cen[sex==0 & origin==0 & year==2015 & race==2]$total_pop
## black, total hispanic, both sexes, "black alone" 7.231 compared to 7.69


1000000*266/cen[sex==0 & origin==0 & year==2016 & race==2]$total_pop
## black, total hispanic, both sexes, "black alone" 6.204 compared to 6.66

1000000*307/cen[sex==0 & origin==0 & year==2015 & race==8]$total_pop
## black, total hispanic, both sexes, "black alone or combination" 6.656 compared to 7.69


1000000*266/cen[sex==0 & origin==0 & year==2016 & race==8]$total_pop
## black, total hispanic, both sexes, "black alone or combination" 5.698 compared to 6.66

1000000*307/cen[sex==0 & origin==1 & year==2015 & race==8]$total_pop
## black, not hispanic, both sexes, "black alone or combination" 7.19 compared to 7.69


1000000*266/cen[sex==0 & origin==1 & year==2016 & race==8]$total_pop
## black, not hispanic, both sexes, "black alone or combination" 6.161 compared to 6.66

##
#armed <- as.matrix(table(d$armed,d$raceethnicity))
#tots <- apply(armed,MARGIN=2,sum)
#test <- t(apply(armed,MARGIN=1,FUN=function(x){x/tots}))


