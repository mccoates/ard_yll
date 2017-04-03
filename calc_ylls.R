
rm(list=ls())
library(tools)
library(data.table)
library(ggplot2)

if (substr(Sys.info()["user"],4,5)=="33") {
  rootdir <- "M:/professional/race/thecounted-data/"
} 

##############################################################
## read in and format data from The Counted
##############################################################
d <- fread(paste0(rootdir,"the-counted-2016.csv"))
d2 <- fread(paste0(rootdir,"the-counted-2015.csv"))
d <- rbind(d,d2)
d[age=="Unknown",age:=NA]
d[age=="40s",age:="45"] ## assumption for the one death in the age "40s"
d[,age:=as.numeric(age)]
setnames(d,c("raceethnicity","classification"),c("race","cod"))
d[,age_group:=cut(age,breaks=c(seq(from=0,to=80,by=5),110),right=F)]


##############################################################
## read in standard life expectancy file and format
##############################################################
le <- fread(paste0(rootdir,"FINAL_min_pred_ex.csv"))
setnames(le,"Pred_ex","ylls")


##############################################################
## read in census file and format
##############################################################
cen <- fread(paste0(rootdir,"/census.csv"))
## https://www.census.gov/population/projections/files/filelayout/NP2014_D1.pdf
cen <- cen[year %in% c(2015,2016)]
cen <- melt(cen,id.vars=c("origin","race","sex","year"))
setnames(cen,c("variable","value"),c("age","pop"))
cen[,age:=gsub("pop_","",age)]
cen[age=="total_pop",age:="All Ages"]
## recode some of the variables
cen[,origin:=factor(origin,labels=c("Total","Not Hispanic","Hispanic"))]
cen[,race:=factor(race,labels=c("All races","White alone","Black alone","AIAN alone","Asian alone","AHPI alone","Two or More Races",
                                "White alone or in combination","Black alone or in combination","AIAN alone or in combination",
                                "Asian alone or in combination","NHPI alone or in combination"))]
cen[,sex:=factor(sex,labels=c("both","male","female"))]
## recode races to match races from the counted
## the counted uses non-hispanic race-exclusive categories as denominators
## we will match their methods, but do sensitivity analyses to see if conclusions differ
## when different are assumptions are applied to solve the issue that the race categories are not consistent
## between the data sources

## Hispanic/Latino
## the counted has "hispanic/latino" as race rather than ethnicity, so taking "Hispanic" as denominator from census
hisp <- copy(cen[origin=="Hispanic" & race=="All races"])
hisp[,race:="Hispanic/Latino"]

## white, black, native american
wbn <- copy(cen[(race %in% c("Black alone","White alone","AIAN alone")) & origin == "Not Hispanic"])
wbn[,race:=gsub(" alone","",race)]
wbn[race=="AIAN",race:="Native American"]

## Asian/Pacific Islander
api <- copy(cen[(race %in% c("AHPI alone","Asian alone")) & origin == "Not Hispanic"])
setkey(api,origin,sex,year,age)
api <- api[,list(pop=sum(pop)),by=key(api)]
api[,race:="Asian/Pacific Islander"]

## No population for Arab-American, Unknown, or Other from census (One "Other" death in data, 39 Unknown, 7 Arab-American)
## These will count in all-race numbers, but we cannot calculate race-specific rates, only total counts
tot <- copy(cen[race == "All races" & origin=="Total"])
tot <- rbindlist(list(tot,api,wbn,hisp),use.names=T)
tot[,origin:=NULL]
tot[,age_group:=cut(as.numeric(as.character(age)),breaks=c(seq(from=0,to=80,by=5),110),right=F)]
tot[age=="All Ages",age_group:="All Ages"]
tot[,race:=as.character(race)]
tot[race=="All races",race:="All Races"]
tot[,sex:=toTitleCase(as.character(sex))]
tot[sex=="both",sex:="Both"]
setnames(tot,"sex","gender")

## add for both years, collapse to age groups
setkey(tot,race,gender,year,age_group)
tot <- tot[,list(pop=sum(pop)),by=key(tot)]
add <- copy(tot)
setkey(add,race,gender,age_group)
add <- add[,list(pop=sum(pop)),by=key(add)]
add[,year:="2015-2016"]
tot <- rbind(tot,add)

## create white/non-white binary populations
bin <- copy(cen[(race=="White alone" & origin == "Not Hispanic") | (race=="All races" & origin == "Total")])
bin[race=="White alone" & origin == "Not Hispanic",race:="White"]
bin[race=="All races" & origin == "Total",race:="All"]
bin[,origin:=NULL]
bin <- dcast.data.table(bin,sex+year+age~race,value.var="pop")
bin[,"Non-White":=All - White]
bin[,All:=NULL]
bin <- melt(bin,id.vars=c("sex","year","age"),value.name = "pop",variable.name = "race")
bin[,age_group:=cut(as.numeric(as.character(age)),breaks=c(seq(from=0,to=80,by=5),110),right=F)]
bin[age=="All Ages",age_group:="All Ages"]
bin[,race:=as.character(race)]
bin[,sex:=toTitleCase(as.character(sex))]
bin[sex=="both",sex:="Both"]
setnames(bin,"sex","gender")
setkey(bin,race,gender,year,age_group)
bin <- bin[,list(pop=sum(pop)),by=key(bin)]
add <- copy(bin)
setkey(add,race,gender,age_group)
add <- add[,list(pop=sum(pop)),by=key(add)]
add[,year:="2015-2016"]
bin <- rbind(bin,add)


######################################################
## create YLLs
######################################################
d <- merge(d,le,by="age",all.x=T)
if (nrow(d[!is.na(age) & is.na(ylls)]) > 0) stop("predex didn't merge for some ages")

## some ages unknown, we don't want to make assumptions about them, when making comparisons between age groups
## but we do want to count their YLLs in the "All Ages" category
## to make a conservative assumption, we will assign them the mean YLLs across the dataset
d[is.na(ylls),ylls:=mean(d$ylls,na.rm=T)]

########################################################
## collapse to counts by important variables
########################################################
tab <- copy(d)
setkey(tab,age_group,gender,race,year,cod,armed)
tab[,deaths:=1]
tab <- tab[,list(avg_age=mean(age),deaths=sum(deaths),ylls=sum(ylls)),by=key(tab)]

######################################################################################
## collapse to create both-gender, all-race, all-death type, all-armed counts, all-age
######################################################################################
## function to do the collapse
add_agg <- function(data,var,cats,aggname) {
  add <- copy(data)
  add <- add[,list(avg_age=weighted.mean(avg_age,w=deaths,na.rm=T),deaths=sum(deaths),ylls=sum(ylls)),by=c(cats)]
  add[,paste0(var):=aggname]
  return(add)
}
## dataframe to loop over for the aggregate name
aggloop <- data.frame(vars=c("age_group","gender","race","cod","year","armed"),
                      aggname=c("All Ages","Both","All Races","All Causes","2015-2016","Armed or Unarmed"),stringsAsFactors=F)
for (x in 1:nrow(aggloop)) {
  out <- add_agg(tab,var=aggloop$vars[x],cats=aggloop$vars[!aggloop$vars==aggloop$vars[x]],aggname=aggloop$aggname[x])
  tab <- rbind(tab,out)
}

####################################################################################
## merge populations, make rates
####################################################################################

dat <- merge(tot[,c("race","gender","year","age_group","pop"),with=F],tab,by=c("race","gender","year","age_group"),all.y=T)
dat[,death_rate:=deaths/pop*1000000]
dat[,yll_rate:=ylls/pop*1000000]
setnames(dat,c("deaths","ylls"),c("death_count","yll_count"))

#####################################################################
## make a table
#####################################################################
roundvars <- c("death_pct","death_rate","yll_pct","yll_rate","avg_age","pop_pct")
t1 <- copy(dat[year=="2015-2016" & age_group == "All Ages" & cod== "All Causes" & armed=="Armed or Unarmed" & gender=="Both"])
## Other category should include arab-american/Other/Unknown
t1[race %in% c("Arab-American","Other","Unknown"),race:="Other/Unknown"]
setkey(t1,race)
t1 <- t1[,list(death_count=sum(death_count),death_rate=weighted.mean(death_rate,w=pop),yll_count=sum(yll_count),
               yll_rate=weighted.mean(yll_rate,w=pop),avg_age=weighted.mean(avg_age,w=death_count),pop=sum(pop)),by=key(t1)]
t1[,death_pct:=death_count/sum(death_count)*200]
t1[,yll_pct:=yll_count/sum(yll_count)*200]
t1[,pop_pct:=pop/t1[race=="All Races"]$pop*100]
t1 <- t1[,c("race","pop_pct","death_count","death_pct","death_rate","yll_count","yll_pct","yll_rate","avg_age"),with=F]
for (r in roundvars) {
  t1[[r]] <- round(t1[[r]],1)
}
t1 <- t1[order(pop_pct,decreasing=T)]


t2 <- copy(dat[year=="2015-2016" & age_group == "All Ages" & race== "All Races" & cod== "All Causes" & armed=="Armed or Unarmed" & gender %in% c("Male","Female")])
## Other category should include arab-american/Other/Unknown
t2[,death_pct:=death_count/sum(death_count)*100]
t2[,yll_pct:=yll_count/sum(yll_count)*100]
t2[,pop_pct:=pop/sum(pop)*100]
t2 <- t2[,c("gender","pop_pct","death_count","death_pct","death_rate","yll_count","yll_pct","yll_rate","avg_age"),with=F]
for (r in roundvars) {
  t2[[r]] <- round(t2[[r]],1)
}
t2 <- t2[order(pop_pct,decreasing=T)]

 
t3 <- copy(dat[year=="2015-2016" & age_group != "All Ages" & race== "All Races" & cod== "All Causes" & armed=="Armed or Unarmed" & gender %in% c("Both")])
t3[,age:=substr(age_group,2,3)]
t3[,age:=as.numeric(as.character(gsub(",","",age)))]
t3[age < 15,age_grp:="<15 years"]
t3[age > 14 & age < 25,age_grp:="15 to 24 years"]
t3[age > 24 & age < 35,age_grp:="25 to 34 years"]
t3[age > 34 & age < 45,age_grp:="35 to 44 years"]
t3[age > 44 & age < 55,age_grp:="45 to 54 years"]
t3[age > 54 & age < 65,age_grp:="55 to 64 years"]
t3[age > 64,age_grp:="65+ years"]
setkey(t3,age_grp)
t3 <- t3[,list(death_count=sum(death_count),death_rate=weighted.mean(death_rate,w=pop),yll_count=sum(yll_count),
               yll_rate=weighted.mean(yll_rate,w=pop),avg_age=weighted.mean(avg_age,w=death_count),pop=sum(pop)),by=key(t3)]
## Other category should include arab-american/Other/Unknown
t3[,death_pct:=death_count/sum(death_count)*100]
t3[,yll_pct:=yll_count/sum(yll_count)*100]
t3[,pop_pct:=pop/sum(pop)*100]
t3 <- t3[,c("age_grp","pop_pct","death_count","death_pct","death_rate","yll_count","yll_pct","yll_rate","avg_age"),with=F]
for (r in roundvars) {
  t3[[r]] <- round(t3[[r]],1)
}
t3[,age_grp:=factor(age_grp,levels=c("<15 years","15 to 24 years","25 to 34 years","35 to 44 years","45 to 54 years","55 to 64 years","65+ years"))]
t3 <- t3[order(age_grp,decreasing=F)]

write.csv(t1,paste0(rootdir,"/race_table.csv"),row.names=F)
write.csv(t2,paste0(rootdir,"/sex_table.csv"),row.names=F)
write.csv(t3,paste0(rootdir,"/age_table.csv"),row.names=F)


######################################################################
## make figure
######################################################################

toplot <- copy(dat[year=="2015-2016" & race!="All Races" & (!is.na(age_group) & age_group != "All Ages") & cod== "All Causes" & armed=="Armed or Unarmed" & gender=="Both"])
toplot[,age:=substr(age_group,2,3)]
toplot[,age:=gsub(",","",age)]
setkey(toplot,age_group,age)
toplot<- toplot[,list(race=race,gender=gender,year=year,cod=cod,armed=armed,pop=pop,avg_age=avg_age,death_count=death_count,yll_count=yll_count,
             death_rate=death_rate,yll_rate=yll_rate,death_pct=death_count/sum(death_count),yll_pct=yll_count/sum(yll_count)),by=key(toplot)]
toplot[,race:=factor(race,levels=c("White","Black","Hispanic/Latino","Native American","Asian/Pacific Islander","Arab-American","Other","Unknown"))]

extra <- copy(dat[year=="2015-2016" & (!race %in% c("All Races","Unknown")) & (!is.na(age_group) & age_group != "All Ages") & cod== "All Causes" & armed=="Armed or Unarmed" & gender=="Both"])
extra[,age:=substr(age_group,2,3)]
extra[,age:=gsub(",","",age)]
setkey(extra,age_group,age)
extra<- extra[,list(race=race,gender=gender,year=year,cod=cod,armed=armed,pop=pop,avg_age=avg_age,death_count=death_count,yll_count=yll_count,
                      death_rate=death_rate,yll_rate=yll_rate,death_pct=death_count/sum(death_count),yll_pct=yll_count/sum(yll_count)),by=key(extra)]
extra[race!="White",race:="Non-White"]
setkey(extra,age_group,age,race,gender,year,cod,armed)
extra <- extra[,list(pop=sum(pop),avg_age=weighted.mean(avg_age,w=death_count),death_count=sum(death_count),yll_count=sum(yll_count)),by=key(extra)]
extra[,race:=factor(race,levels=c("White","Non-White"))]

## stacked bar

gg <- ggplot(toplot, aes(x = as.numeric(as.character(age)), y = yll_count,fill=race)) +
        geom_bar(stat='identity') + ylab("YLLs") + xlab("Age") + 
        scale_fill_brewer("Race/Ethnicity",type="qual",palette=7) +
        ggtitle("Arrest-Related YLLs by Age and Race, 2015-2016") + theme_bw()
gg

gg <- ggplot(extra, aes(x = as.numeric(as.character(age)), y = yll_count,fill=race)) +
  geom_bar(stat='identity') + ylab("YLLs") + xlab("Age") + 
  scale_fill_brewer("Race/Ethnicity",type="qual",palette=2) +
  ggtitle("Arrest-Related YLLs by Age and Race, 2015-2016") + theme_bw()
gg

gg <- ggplot(toplot, aes(x = as.numeric(as.character(age)), y = yll_pct*100,fill=race)) +
  geom_bar(stat='identity') + ylab("Percent oF YLLs") + xlab("Age") + 
  scale_fill_brewer("Race/Ethnicity",type="qual",palette=7) +
  ggtitle("Percent of Arrest-Related YLLs by Age and Race, 2015-2016") + theme_bw()
gg

## distributions
toplot[race!="White",race:="Non-White"]
setkey(toplot,race,gender,year,age_group,cod,armed)
toplot <- toplot[,list(avg_age=weighted.mean(avg_age,w=death_count),yll_count=sum(yll_count),death_count=sum(death_count)),by=key(toplot)]
toplot <- merge(toplot,bin[gender=="Both" & year=="2015-2016" & age_group != "All Ages"],by=c("race","gender","year","age_group"),all=T)
toplot[is.na(death_count),death_count:=0]
toplot[is.na(yll_count),yll_count:=0]
plotrate <- copy(toplot)
setkey(toplot,race,gender,year)
toplot <- toplot[,list(pct_ylls=yll_count/sum(yll_count),age_group=age_group,pct_pop=pop/sum(pop)),by=key(toplot)]
toplot <- melt(toplot,id.vars=c("race","gender","year","age_group"))
toplot[,lines:=paste0(race,"_",variable)]
toplot[,age:=substr(age_group,2,3)]
toplot[,age:=gsub(",","",age)]
toplot[,lines:=factor(lines,levels=c("Non-White_pct_ylls","Non-White_pct_pop","White_pct_ylls","White_pct_pop"),
                      labels=c("Non-White YLLs","Non-White Population","White YLLs","White Population"))]

gg <- ggplot(toplot,aes(x=as.numeric(as.character(age)),y=value*100,group=lines,color=lines)) + geom_line(size=1.5) +
  ylab("Percent") + xlab("Age") + 
  scale_color_manual("Distribution",values=c("firebrick4","firebrick1","dodgerblue4","dodgerblue")) +
  ggtitle("Distribution of Population and Arrest-Related YLLs in White \nand Non-White Populations, 2015-2016") + 
  theme_bw()
gg

plotrate[,yll_rate:=yll_count/pop*1000000]
plotrate[,age:=substr(age_group,2,3)]
plotrate[,age:=gsub(",","",age)]

gg <- ggplot(plotrate,aes(x=as.numeric(as.character(age)),y=yll_rate*100,group=race,color=race)) + geom_line(size=1.5) +
  ylab("YLL Rate (per million)") + xlab("Age") + 
  scale_color_manual("Distribution",values=c("firebrick3","dodgerblue3")) +
  ggtitle("Rate of Arrest-Related YLLs in White \nand Non-White Populations by Age, 2015-2016") + 
  theme_bw()
gg

