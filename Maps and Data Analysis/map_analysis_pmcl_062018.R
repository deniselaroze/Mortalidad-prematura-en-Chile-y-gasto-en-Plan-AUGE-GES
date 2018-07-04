
####################################################
### Premature Mortality Chile - Maps
### Denise Laroze
### June 2018
####################################################



library(sp) 
library(foreign)
library(RColorBrewer)
library(maptools)
#library(geosphere)
library(stringr)
library(latticeExtra)
library(dplyr)
library(rgdal)
library(readstata13)
library(plyr)
library(raster)
library(stargazer)
library(ggplot2)

rm(list=ls())


#bd<-"C:/Users/Denise Laroze Prehn/Dropbox/PMCL"
bd<-"C:/Users/Denise Laroze P/Dropbox/PMCL"

setwd(bd)


#### Datasets
# General setting for script
v<-"_March2018"  # version number to store the plots

load("Maps and Data Analysis/PMCLmydf.Rdata")


CL<-readShapePoly("Maps and Data Analysis/DPA/DPA INE/Comuna.shp")
R<-readShapePoly("Maps and Data Analysis/DPA/DPA INE/Region.shp")


cl<-as.data.frame(CL)

coordinates<- coordinates(CL)


########################
##### Descriptive and Data management
#######################

#descriptives data
#plot(CL)
#attributes(CL@data)
#GB.b@data$LAD14NM

#summary(DE)
#str_count(DE@data$CCA_2)


###################################
#Merge the data with the shapefile
###################################
#Creating merging code and sorting

#map.df$mcode<-str_pad(map.df$code, 5, side = "left", pad = "0")
#mydf$code.maps<-str_pad(mydf$code, 8, side = "right", pad = "0") ### for 1990's data

mydf <- transform(mydf,
                  pm_59_2015_s = pm_59_2015 - mean(pm_59_2015, na.rm=T),
                  pm_64_2015_s = pm_64_2015 - mean(pm_64_2015, na.rm=T),   
                  pm_69_2015_s = pm_69_2015 - mean(pm_69_2015, na.rm=T),
                  pm_74_2015_s = pm_74_2015 - mean(pm_74_2015, na.rm=T),
                  pm_59_2004_s = pm_59_2004 - mean(pm_59_2004, na.rm=T),
                  pm_64_2004_s = pm_64_2004 - mean(pm_64_2004, na.rm=T),   
                  pm_69_2004_s = pm_69_2004 - mean(pm_69_2004, na.rm=T),
                  pm_74_2004_s = pm_74_2004 - mean(pm_74_2004, na.rm=T),
                  gespop2009_s= gespop2009 - mean(gespop2009, na.rm=T),
                  gespop2014_s= gespop2014 - mean(gespop2014, na.rm=T)   
                   
)


mydf <- mydf[order(mydf$code),] 

CL <- CL[order(CL@data$COMUNA),] 
#DE <- DE[order(DE@data$KREIS_KENN),] ### for 1990's data

CL@data<-merge(CL@data,mydf,by.x='COMUNA',by.y='code', all.x=T)

#write.csv(DE, file = paste0(bd, "/Data/maps_data.csv"))


#DE@data<-merge(DE@data,mydf,by.x='KREIS_KENN',by.y='code.maps', all.x=T) ### for 1990's data

cl<-as.data.frame(CL)
#CRS(CL, doCheckCRSArgs=TRUE)
cl$area_sqkm <- area(CL) / 1000000
cl$density2004<- cl$pop.2004/cl$area_sqkm
cl$density2015<- cl$pop.2015/cl$area_sqkm
cl$density2007<- cl$pop.2007/cl$area_sqkm


summary(CL)


###############
## Descriptives
###############
#AVPP
summary(cl$pm_64_2004_s)
summary(cl$pm_64_2015_s)
summary(cl$pm_69_2004_s)
summary(cl$pm_69_2015_s)
summary(cl$pm_74_2004_s)
summary(cl$pm_74_2015_s)


# ges
summary(cl$gespop2009_s)
summary(cl$gespop2014_s)

plot(cl$gespop2014)



###########################
### Descriptive statistics
###########################

View(cl[ CL$DESC_COMUN %in% c("LAS CONDES", "VITACURA"), c("DESC_COMUN", "pm_64_2004", "pm_64_2015")])



df<-cl[, c( "COMUNA",
            "pm_64_2004","pm_69_2004",  "pm_74_2004",
            "pm_64_2009", "pm_69_2009", "pm_74_2009",
            "pm_39_2015", "pm_49_2015",  "pm_59_2015",
            "pm_64_2015", "pm_69_2015","pm_74_2015",
            "pop.2004", "pop.2009", "pop.2015",
            "gespop2009", "density2007", "ingreso_promedio_2015" , 
            "salud_fonasa_2015", "carencia_edu_2015", "Pobreza_multi_2015")
]


df$COMUNA<-as.numeric(df$COMUNA)
###Summary table
cvars<-c( "Codigo Comuna", 
          "MP <65 años 2004","MP <70 años 2004", "MP <75 años 2004",
          "MP <65 años 2009", "MP <70 años 2009", "MP <75 años 2009",
          "MP <40 años 2015", "MP <50 años 2015", "MP <60 años 2015",
          "MP <65 años 2015", "MP <70 años 2015", "MP <75 años 2015",
          "Población Comunal 2004", "Población Comunal 2009", "Población Comunal 2015",
          "Gasto GES 2007-9", "Densidad pob. 2015", "Ingreso medio 2015", 
          "Prop. Fonasa 2015", "Carencia Educ. 2015", "Pobreza multicausal"
)

stargazer(df, covariate.labels=cvars, decimal.mark = ",",  digit.separator = ".")


minus<-mydf$pm_69_2004-mydf$pm_69_2015
summary(minus)

summary(mydf$gespop2009)
sd(mydf$gespop2009, na.rm=T)

hist(mydf$gespop2009)


### If levels premature mortality correlate with spending
# lm1<-lm(gespop2009 ~ pm_69_2004, data=mydf)
# summary(lm1)

###############################
### Data Analysis
###############################
#Main table die before 70

lm1<-lm(pm_69_2015 ~ gespop2009, data=cl)

lm2<-lm(pm_69_2015 ~ gespop2009 + density2007 + ingreso_promedio_2015, data=cl)

lm3<-lm(pm_69_2015 ~ gespop2009 + density2007 + ingreso_promedio_2015 + salud_fonasa_2015, data=cl)

lm4<-lm(pm_69_2015 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm5<-lm(pm_69_2015 ~ gespop2009  + density2007 + salud_fonasa_2015 + Pobreza_multi_2015 , data=cl)

lm6<-lm(pm_69_2015 ~ gespop2009:salud_fonasa_2015 + density2007 + salud_fonasa_2015, data=cl)


stargazer(lm1, lm2, lm3, lm4, lm5)

stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "Gasto AUGE/GES pc",  "Densidad pobl.", "Ingreso medio",
                              "Fonasa", "Carencia Educ", "Pobreza Multi",
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/table1_March2018.tex")

###########################################
#### Robustness tests
###########################################


#### Weighted by deaths caused by sicknesses sovered by in AUGE/GES
lm1<-lm(pm_69_2015*pon2011 ~ gespop2009, data=cl)

lm2<-lm(pm_69_2015*pon2011 ~ gespop2009 + density2007 + ingreso_promedio_2015, data=cl)

lm3<-lm(pm_69_2015*pon2011 ~ gespop2009 + density2007 + ingreso_promedio_2015 + salud_fonasa_2015, data=cl)

lm4<-lm(pm_69_2015*pon2011 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm5<-lm(pm_69_2015*pon2011 ~ gespop2009  + density2007 + salud_fonasa_2015 + Pobreza_multi_2015 , data=cl)

lm6<-lm(pm_69_2015*pon2011 ~ gespop2009:salud_fonasa_2015 + density2007 + salud_fonasa_2015, data=cl)


stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "Gasto AUGE/GES pc",  "Densidad pobl.", "Ingreso medio",
                              "Fonasa", "Carencia Educ", "Pobreza Multi",
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/table1_pon_March2018.tex")

### Eliminating outliers
cl2<-cl[cl$gespop2009<2000,]
lm1<-lm(pm_69_2015 ~ gespop2009, data=cl2)

lm2<-lm(pm_69_2015 ~ gespop2009 + density2007 + ingreso_promedio_2015, data=cl2)

lm3<-lm(pm_69_2015 ~ gespop2009 + density2007 + ingreso_promedio_2015 + salud_fonasa_2015, data=cl2)

lm4<-lm(pm_69_2015 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl2)

lm5<-lm(pm_69_2015 ~ gespop2009  + density2007 + salud_fonasa_2015 + Pobreza_multi_2015 , data=cl2)

lm6<-lm(pm_69_2015 ~ gespop2009:salud_fonasa_2015 + density2007 + salud_fonasa_2015, data=cl2)


stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "Gasto AUGE/GES pc",  "Densidad pobl.", "Ingreso medio",
                              "Fonasa", "Carencia Educ", "Pobreza Multi",
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/table_out_March2018.tex")



########## Table deaths <75

lm1<-lm(pm_74_2015 ~ gespop2009, data=cl)

lm2<-lm(pm_74_2015 ~ gespop2009 + density2007 + ingreso_promedio_2015, data=cl)

lm3<-lm(pm_74_2015 ~ gespop2009 + density2007 + ingreso_promedio_2015 + salud_fonasa_2015, data=cl)

lm4<-lm(pm_74_2015 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm5<-lm(pm_74_2015 ~ gespop2009  + density2007 + salud_fonasa_2015 + Pobreza_multi_2015 , data=cl)


stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "Gasto AUGE/GES pc",  "Densidad pobl.", "Ingreso medio",
                              "Fonasa", "Carencia Educ", "Pobreza Multi",
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/tablepm75_March2018.tex")


########## Table deaths <65

lm1<-lm(pm_64_2015 ~ gespop2009, data=cl)

lm2<-lm(pm_64_2015 ~ gespop2009 + density2007 + ingreso_promedio_2015, data=cl)

lm3<-lm(pm_64_2015 ~ gespop2009 + density2007 + ingreso_promedio_2015 + salud_fonasa_2015, data=cl)

lm4<-lm(pm_64_2015 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm5<-lm(pm_64_2015 ~ gespop2009  + density2007 + salud_fonasa_2015 + Pobreza_multi_2015 , data=cl)


stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "Gasto AUGE/GES pc",  "Densidad pobl.", "Ingreso medio",
                              "Fonasa", "Carencia Educ", "Pobreza Multi",
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/tablepm65_March2018.tex")


########## Table deaths different ages

lm1<-lm(pm_39_2015 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm2<-lm(pm_49_2015 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm3<-lm(pm_59_2015 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm4<-lm(pm_69_2015 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm5<-lm(pm_74_2015 ~ gespop2009  + density2007 + salud_fonasa_2015 + carencia_edu_2015 , data=cl)


stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "Gasto AUGE/GES pc",  "Densidad pobl.", 
                              "Fonasa", "Carencia Educ", 
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/table_mulipleages_June2018.tex")



####### Placebo tests

lm1<-lm(pm_64_2004 ~ gespop2009, data=cl)

lm2<-lm(pm_64_2004 ~ gespop2009 + density2007 + ingreso_promedio_2015, data=cl)

lm3<-lm(pm_64_2004 ~ gespop2009 + density2007 + ingreso_promedio_2015 + salud_fonasa_2015, data=cl)

lm4<-lm(pm_64_2004 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm5<-lm(pm_64_2004 ~ gespop2009  + density2007 + salud_fonasa_2015 + Pobreza_multi_2015 , data=cl)


stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "Gasto AUGE/GES pc",  "Densidad pobl.", "Ingreso medio",
                              "Fonasa", "Carencia Educ", "Pobreza Multi",
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/table_placebo_March2018.tex")


####### FD estimations 
lm1<-lm(pm_64_2015-pm_64_2004 ~ gespop2009, data=cl)

lm2<-lm(pm_64_2015-pm_64_2004 ~ gespop2009 + density2007 + ingreso_promedio_2015, data=cl)

lm3<-lm(pm_64_2015-pm_64_2004 ~ gespop2009 + density2007 + ingreso_promedio_2015 + salud_fonasa_2015, data=cl)

lm4<-lm(pm_64_2015-pm_64_2004 ~ gespop2009 + density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm5<-lm(pm_64_2015-pm_64_2004 ~ gespop2009  + density2007 + salud_fonasa_2015 + Pobreza_multi_2015 , data=cl)


stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "Gasto AUGE/GES pc",  "Densidad pobl.", "Ingreso medio",
                              "Fonasa", "Carencia Educ", "Pobreza Multi",
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/table_FD_March2018.tex")




##############################
### What explains GES spending
##############################


lm1<-lm(gespop2009 ~ pm_69_2004 , data=cl)

lm2<-lm(gespop2009 ~ pm_69_2004 + density2007 + ingreso_promedio_2015, data=cl)

lm3<-lm(gespop2009 ~ pm_69_2004 + density2007 + ingreso_promedio_2015 + salud_fonasa_2015, data=cl)

lm4<-lm(gespop2009 ~ pm_69_2004+ density2007  + salud_fonasa_2015 + carencia_edu_2015, data=cl)

lm5<-lm(gespop2009 ~ pm_69_2004  + density2007 + salud_fonasa_2015 + Pobreza_multi_2015 , data=cl)


stargazer(lm1, lm2, lm3, lm4, lm5, 
          dep.var.labels.include = F,
          #keep.stat = c("n"),
          #add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
          #               c("R squared", "", "", "", "", "", "", m.all.r2 )),
          covariate.labels=c( "MP 2004",  "Densidad pobl.", "Ingreso medio",
                              "Fonasa", "Carencia Educ", "Pobreza Multi",
                              "Constante"),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          keep.stat = c("n", "adj.rsq"),
          #column.labels   = c("OLS MP 2015", "AVPP"),
          out="Reports/tableGES_March2018.tex")





###################
##### Plotting data 
####################

###################
#### Plot set up
###################

#Changing onvervations with 0 pm to missing
CL$pm_69_2004_r<-CL$pm_69_2004
CL$pm_69_2004_r[CL$pm_69_2004==0]<-NA

r5<-c(5:9, 13)

cls<-CL[CL$REGION %in% r5,]


#colour<-colorRampPalette(brewer.pal(n = 9, name = "Greys"))(17)
colour.res<-rev(colorRampPalette(brewer.pal(n = 11, name = "RdBu"))(17))

x<-1
y<-20
cex.size<-1.5
xlims<- c( -80.45488, -66.41558+x )
ylims<-c(-56.53777, -17.49778+x)

xlims_cls<- c( -80.45488+6, -66.41558-3 )
ylims_cls<-c(-56.53777+16, -17.49778-13)

wd<-20
ht<-50
resol<-300
text.size<-15

#### Cut offs for plot
cut.off<-quantile(CL@data$pm_69_2015, prob = seq(0, 1, length = 11), type = 5, na.rm=T)
#min<-min(CL@data$ylmean2004_s, na.rm=T)-5
#max<-max(CL@data$ylmean2009_s, na.rm=T)+5
#cut.off<-seq(min, max , length = 11)
#cut.off<-round(cut.off, digits=0)
cut.off.r<-cut.off
cut.off.r[11]<-cut.off[11]+10
cut.off.r[1]<-0

#ges.cut.off<-seq(min(CL@data$gespop2014_s, na.rm=T) , max(CL@data$gespop2014_s, na.rm=T), length = 11)
ges.cut.off<-quantile(CL@data$gespop2014, prob = seq(0, 1, length = 11), type = 5, na.rm=T)
#minimum<-min(CL@data$gespop2014_s, na.rm=T)-5
#maximum<-max(CL@data$gespop2014_s, na.rm=T)+5
#ges.cut.off<-seq(minimum, maximum , length = 11)
ges.cut.off.r<-ges.cut.off
ges.cut.off.r[11]<-ges.cut.off[11]+10
ges.cut.off.r[1]<-0



ckey <- list(labels=list(cex=1, at=cut.off.r))
ckeyges <- list(labels=list(cex=1, at=ges.cut.off.r))






######################
#### PM Plots
######################

region.layer <- list("sp.points", R, col = "Black", first=F, lwd = 2.5)

png(filename=paste0(bd, "/Maps and Data Analysis/Plots/pm_69_2004", v, "deciles.png" ,""), width = wd, height = ht, units = 'cm', 
    res = resol)

pm2004<-spplot(CL, "pm_69_2004_r",
               col = "transparent", 
               main = list("Mortalidad prematura 2004", cex=cex.size),
               col.regions =colour.res, alpha=0.5, sp.na.omit=T, colorkey=ckey, at=cut.off.r,
               par.settings=list(fontsize=list(text=text.size)),
               xlim = xlims, ylim=ylims , sp.layout = region.layer
               
               
)

print(pm2004)#, position = c(.5,0,1,1)) 

dev.off()


png(filename=paste0(bd, "/Maps and Data Analysis/Plots/pm_69_2015", v, "deciles.png" ,""), width = wd, height = ht, units = 'cm', 
    res = resol)

pm2015<-spplot(CL, "pm_69_2015",
               col = "transparent",  main = list("Mortalidad prematura 2015", cex=cex.size), 
               col.regions =colour.res , sp.na.omit=T, colorkey=ckey, at=cut.off.r,
               par.settings=list(fontsize=list(text=text.size)),
               xlim = xlims, ylim=ylims , sp.layout = region.layer
               
)

print(pm2015)#, position = c(.5,0,1,1)) 
dev.off()




###########################
#### PM Plots 5-9th region
###########################

png(filename=paste0(bd, "/Maps and Data Analysis/Plots/pm_69_2004_cls", v, "deciles.png" ,""), width = wd, height = ht-20, units = 'cm', 
    res = resol)

pm2004<-spplot(cls, "pm_69_2004_r",
               col = "transparent", main = list("Mortalidad prematura 2004", cex=cex.size), 
               col.regions =colour.res , sp.na.omit=T, colorkey=ckey, at=cut.off.r,
               par.settings=list(fontsize=list(text=text.size)),
               xlim = xlims_cls, ylim=ylims_cls, sp.layout = region.layer
)


print(pm2004)#, position = c(.5,0,1,1)) 
dev.off()


png(filename=paste0(bd, "/Maps and Data Analysis/Plots/pm_69_2015_cls", v, "deciles.png" ,""), width = wd, height = ht-20, units = 'cm', 
    res = resol)

pm2015<-spplot(cls, "pm_69_2015",
               col = "transparent", main = list("Mortalidad prematura 2015", cex=cex.size), 
               col.regions =colour.res , sp.na.omit=T, colorkey=ckey, at=cut.off.r,
               par.settings=list(fontsize=list(text=text.size)),
               xlim = xlims_cls, ylim=ylims_cls, sp.layout = region.layer
               
)

print(pm2015)#, position = c(.5,0,1,1)) 
dev.off()



###################################
### GES plots
###################################


png(filename=paste0(bd, "/Maps and Data Analysis/Plots/ges2009", v, "deciles.png" ,""), width = wd+1.5, height = ht, units = 'cm', 
    res = resol)

ges2009<-spplot(CL, "gespop2009",
                col = "transparent", main = list("Media Gasto 2007-9", cex=cex.size), 
                col.regions =colour.res , sp.na.omit=T, colorkey=ckeyges, at=ges.cut.off.r,
                par.settings=list(fontsize=list(text=text.size)),
                xlim = xlims, ylim=ylims, sp.layout = region.layer
)

print(ges2009)#, position = c(.5,0,1,1)) 
dev.off()


png(filename=paste0(bd, "/Maps and Data Analysis/Plots/ges2014", v, "deciles.png" ,""), width = wd+1.5, height = ht, units = 'cm', 
    res = resol)

ges2014<-spplot(CL, "gespop2014",
                col = "transparent", main = list("Media Gasto 2010-14", cex=cex.size), 
                col.regions =colour.res , sp.na.omit=T, colorkey=ckeyges, at=ges.cut.off.r,
                par.settings=list(fontsize=list(text=text.size)),
                xlim = xlims, ylim=ylims, sp.layout = region.layer
                
)

print(ges2014)#, position = c(.5,0,1,1)) 
dev.off()







