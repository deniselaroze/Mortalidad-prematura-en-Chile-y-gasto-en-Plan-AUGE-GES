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


death<-read.csv("INE/Muertes_2015.csv")
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


g.pop<-g.pop[,c("code", "edad", "pop.2015")]
g.pop<-rename(g.pop,c("pop.2015"="population"))
#g.pop$age_group<-gsub(" ", ".", g.pop$edad, fixed = TRUE)
pop.w<-reshape(g.pop, idvar = c("code"), timevar = "edad", direction = "wide")

# pop.w$pop0_1<-pop.w[, c(3) ]
# pop.w$pop2_4<-rowSums(pop.w[, c(4:7)], na.rm = T, dim=1)
# pop.w$pop5_9<-rowSums(pop.w[, c(8:12)], na.rm = T, dim=1)
# pop.w$pop10_14<-rowSums(pop.w[, c(13:17)], na.rm = T, dim=1)
# pop.w$pop15_19<-rowSums(pop.w[, c(18:22)], na.rm = T, dim=1)
# pop.w$pop20_24<-rowSums(pop.w[, c(23:27)], na.rm = T, dim=1)
# pop.w$pop25_29<-rowSums(pop.w[, c(28:32)], na.rm = T, dim=1)
# pop.w$pop30_34<-rowSums(pop.w[, c(33:37)], na.rm = T, dim=1)
# pop.w$pop35_39<-rowSums(pop.w[, c(38:42)], na.rm = T, dim=1)
# pop.w$pop40_44<-rowSums(pop.w[, c(43:47)], na.rm = T, dim=1)
# pop.w$pop45_49<-rowSums(pop.w[, c(48:52)], na.rm = T, dim=1)
# pop.w$pop50_54<-rowSums(pop.w[, c(53:57)], na.rm = T, dim=1)
# pop.w$pop55_59<-rowSums(pop.w[, c(58:62)], na.rm = T, dim=1)
# pop.w$pop60_64<-rowSums(pop.w[, c(63:67)], na.rm = T, dim=1)
# pop.w$pop65_69<-rowSums(pop.w[, c(68:72)], na.rm = T, dim=1)
# pop.w$pop70_74<-rowSums(pop.w[, c(73:77)], na.rm = T, dim=1)
# pop.w$pop75_79<-pop.w$population.75.bis.unter.80.Jahre
# pop.w$pop80_84<-pop.w$population.80.bis.unter.85.Jahre
# pop.w$pop85_p<-rowSums(pop.w[, c(80:81)], na.rm = T, dim=1)


pop.death.df<-merge(x=pop.w, y=death, by.x="code", by.y="Comuna", all=T)


#muertes<-paste0("muertes.", 0:75)
#population<-paste0("population.", 0:75)


pd<-pop.death.df[, c(1, 2)]
pd$pd.0<-pop.death.df$muertes.men1/pop.death.df$population.0

for (i in 1:80){
  pd[paste0("pd.", i)] <-pop.death.df[paste0("muertes.", i)]/ pop.death.df[paste0("population.", i)]
}

# pop.death.df$pd2_4<-(pop.death.df$muertes.o/pop.death.df$population.0)*4
# pop.death.df$pd5_9<-(pop.death.df$deaths.5.bis.unter.10.Jahre/pop.death.df$pop5_9)*5
# pop.death.df$pd10_14<-(pop.death.df$deaths.10.bis.unter.15.Jahre/pop.death.df$pop10_14)*5
# pop.death.df$pd15_19<-(pop.death.df$deaths.15.bis.unter.20.Jahre/pop.death.df$pop15_19)*5
# pop.death.df$pd20_24<-(pop.death.df$deaths.20.bis.unter.25.Jahre/pop.death.df$pop20_24)*5
# pop.death.df$pd25_29<-(pop.death.df$deaths.25.bis.unter.30.Jahre/pop.death.df$pop25_29)*5
# pop.death.df$pd30_34<-(pop.death.df$deaths.30.bis.unter.35.Jahre/pop.death.df$pop30_34)*5
# pop.death.df$pd35_39<-(pop.death.df$deaths.35.bis.unter.40.Jahre/pop.death.df$pop35_39)*5
# pop.death.df$pd40_44<-(pop.death.df$deaths.40.bis.unter.45.Jahre/pop.death.df$pop40_44)*5
# pop.death.df$pd45_49<-(pop.death.df$deaths.45.bis.unter.50.Jahre/pop.death.df$pop45_49)*5
# pop.death.df$pd50_54<-(pop.death.df$deaths.50.bis.unter.55.Jahre/pop.death.df$pop50_54)*5
# pop.death.df$pd55_59<-(pop.death.df$deaths.55.bis.unter.60.Jahre/pop.death.df$pop55_59)*5
# pop.death.df$pd60_64<-(pop.death.df$deaths.60.bis.unter.65.Jahre/pop.death.df$pop60_64)*5
# pop.death.df$pd65_69<-(pop.death.df$deaths.65.bis.unter.70.Jahre/pop.death.df$pop65_69)*5
# pop.death.df$pd70_74<-(pop.death.df$deaths.70.bis.unter.75.Jahre/pop.death.df$pop70_74)*5
# pop.death.df$pd75_79<-(pop.death.df$deaths.75.bis.unter.80.Jahre/pop.death.df$pop75_79)*5
# pop.death.df$pd80_84<-(pop.death.df$deaths.80.bis.unter.85.Jahre/pop.death.df$pop80_84)*5
# pop.death.df$pd85_p<-(pop.death.df$deaths.85.Jahre.und.mehr/pop.death.df$pop85_p)*5

####??? Survival table

s_pop<-10000 # standard population
pd$s.0<- ifelse(!is.na(pd$pd.0), s_pop-(pd$pd.0*s_pop), s_pop )  

for (i in 1:80){
  col<-paste0("pd.", i)
  cols<-paste0("s.", i)
  for (j in 1:nrow(pd)){
    row<-j
    colpm1<-paste0("pd.", i-1)
    colsm1<-paste0("s.", i-1)
    
    pd[j,paste0("s.", i)] <- ifelse(is.na(pd[row,col]), 
                                    pd[row, colsm1],
                                    pd[row, colsm1]-(pd[row, col]*pd[j,cols])
    )
  }
}



# s_1<-s_pop-(pop.death.df$pd0_1*s_pop)
# s_4<-s_1-(pop.death.df$pd2_4*s_1)
# s_9<-s_4-(pop.death.df$pd5_9*s_4)
# s_14<-s_9-(pop.death.df$pd10_14*s_9)
# s_19<-s_14-(pop.death.df$pd15_19*s_14)
# s_24<-s_19-(pop.death.df$pd20_24*s_19)
# s_29<-s_24-(pop.death.df$pd25_29*s_24)
# s_34<-s_29-(pop.death.df$pd30_34*s_29)
# s_39<-s_34-(pop.death.df$pd35_39*s_34)
# s_44<-s_39-(pop.death.df$pd40_44*s_39)
# s_49<-s_44-(pop.death.df$pd45_49*s_44)
# s_54<-s_49-(pop.death.df$pd50_54*s_49)
# s_59<-s_54-(pop.death.df$pd55_59*s_54)
# s_64<-s_59-(pop.death.df$pd60_64*s_59)
# s_69<-s_64-(pop.death.df$pd65_69*s_64)
# s_74<-s_69-(pop.death.df$pd70_74*s_69)



##### Premature Mortality
pd$pm_59<-s_pop-pd$s.59
pd$pm_64<-s_pop-pd$s.64
pd$pm_69<-s_pop-pd$s.69
pd$pm_74<-s_pop-pd$s.74



#Number of Standaradised deaths
ns_death_1<-s_pop-s_1
ns_death_4<-s_1-s_4
ns_death_9<-s_4-s_9
ns_death_14<-s_9-s_14
ns_death_19<-s_14-s_19
ns_death_24<-s_19-s_24
ns_death_29<-s_24-s_29
ns_death_34<-s_29-s_34
ns_death_39<-s_34-s_39
ns_death_44<-s_39-s_44
ns_death_49<-s_44-s_49
ns_death_54<-s_49-s_54
ns_death_59<-s_54-s_59
ns_death_64<-s_59-s_64
ns_death_69<-s_64-s_69
ns_death_74<-s_69-s_74

yl<-c(  ns_death_1*74 +  ns_death_4*72 + ns_death_9*67 + ns_death_14*62  + ns_death_19*57 + 
        ns_death_24*52  + ns_death_29*47 + ns_death_34*42  + ns_death_39*37 + 
        ns_death_44*32  + ns_death_49*27 + ns_death_54*22  + ns_death_59*17 + 
        ns_death_64*12  + ns_death_69*7 + ns_death_74*2)






# Years Lost





pop.death.df2<-pop.death.df[, c(1:3, 62:ncol(pop.death.df))]

pop.death.df2<-cbind(pop.death.df2, s_1, s_4, s_9, s_14, s_19, s_24, s_29, s_34, s_39, s_44, s_49, s_54, s_59, s_64, s_69, s_74, yl)



write.csv(pop.death.df2, file = "pop_death.csv")
save(pop.death.df2, file="pop_death.RData")
