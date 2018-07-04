#####################################
### Premature Mortality Germany - Dependent variable
### Denise Laroze and Thomas Plümper
### September 2016
#####################################


library(foreign)
library(plyr)

bd<-"C:/Users/Denise Laroze Prehn/Dropbox/PMCL/Data"

#### Datasets
setwd(bd)

rm(list=ls())


death<-read.csv("INE/Muertes_2009.csv")
#death<-death[,c(1, 3, 4)]
#death<-rename(death,c("Total_deaths"="deaths"))
#death$age_group<-gsub(" ", ".", death$age_group, fixed = TRUE)
#death.w<-reshape(death, idvar = "code", timevar = "age_group", direction = "wide")


pop<-read.csv("INE/tot_pop.csv", sep=";")
g.pop<-ddply(pop, c("code", "edad"), summarise,
             pop.2002= sum(a2002, na.rm=T),
             pop.2003= sum(a2003, na.rm=T),  
             pop.2004= sum(a2004, na.rm=T),  
             pop.2005= sum(a2005, na.rm=T),
             pop.2006= sum(a2006, na.rm=T),   
             pop.2007= sum(a2007, na.rm=T),
             pop.2008= sum(a2008, na.rm=T),
             pop.2009= sum(a2009, na.rm=T),
             pop.2010= sum(a2010, na.rm=T),
             pop.2011= sum(a2011, na.rm=T),
             pop.2012= sum(a2012, na.rm=T),
             pop.2013= sum(a2013, na.rm=T),
             pop.2014= sum(a2014, na.rm=T),
             pop.2015= sum(a2015, na.rm=T),
             pop.2016= sum(a2016, na.rm=T)
             )


g.pop<-g.pop[,c("code", "edad", "pop.2009")]
g.pop<-rename(g.pop,c("pop.2009"="population"))
#g.pop$age_group<-gsub(" ", ".", g.pop$edad, fixed = TRUE)
pop.w<-reshape(g.pop, idvar = c("code"), timevar = "edad", direction = "wide")


pop.death.df<-merge(x=pop.w, y=death, by.x="code", by.y="Comuna", all=T)

pd<-as.data.frame(pop.death.df[,1])
names(pd)
colnames(pd)[1] <- "code"

pd$pd.0<-pop.death.df$muertes.men1/pop.death.df$population.0

for (i in 1:80){
  
  for(j in 1:nrow(pd)){
    pd[j, paste0("pd.", i)] <-pop.death.df[j, paste0("muertes.", i)]/ pop.death.df[j, paste0("population.", i)]
  }
  
 
}


#### Survival table

s_pop<-10000 # standard population
pd$s.0<- ifelse(!is.na(pd$pd.0), s_pop-(pd$pd.0*s_pop), s_pop )  

for (i in 1:80){
  col<-paste0("pd.", i)
  colpm1<-paste0("pd.", i-1)
  colsm1<-paste0("s.", i-1)
  for (j in 1:nrow(pd)){
    row<-j
    
    
    pd[j,paste0("s.", i)] <- ifelse(is.na(pd[row,col]), 
                                    pd[row, colsm1],
                                    pd[row, colsm1]-(pd[row, col]*pd[j,colsm1])
    )
  }
}


#View(pd[, c( "pd.0", "s.0",   "pd.1", "s.1", "pd.2", "s.2", "pd.3", "s.3")])



##### Premature Mortality
pd$pm_59_2009<-s_pop-pd$s.59
pd$pm_64_2009<-s_pop-pd$s.64
pd$pm_69_2009<-s_pop-pd$s.69
pd$pm_74_2009<-s_pop-pd$s.74



pd<-pd[, c("code", "pm_59_2009", "pm_64_2009", "pm_69_2009", "pm_74_2009" )]


write.csv(pd, file = "INE/pm2009.csv")
save(pd, file="INE/pm2009.RData")
