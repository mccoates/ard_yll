
rm(list=ls())
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
d[,age_group:=cut(age,breaks=c(seq(from=0,to=90,by=5)))]


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
api <- copy(cen[(race %in% c("AHPI alone" | "Asian alone")) & origin == "Not Hispanic"])
setkey(api,origin,sex,year,age)
api <- api[,list(pop=sum(pop)),by=key(api)]
api[,race:="Asian/Pacific Islander"]

## No population for Arab-American, Unknown, or Other from census (One "Other" death in data, 39 Unknown, 7 Arab-American)
## These will count in all-race numbers, but we cannot calculate race-specific rates, only total counts



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

########################################################
## merge age-specific, race-specific populations from census
########################################################




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
## re-merge aggregate populations to make sure aggregated properly
####################################################################################









