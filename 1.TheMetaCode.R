#### meta analysis using metafor package
library(Hmisc);library(car);library(mgcv)
library(maps)
library(metafor)
#check that variables are factors or numeric
metadf <- Alldata[which(Alldata$metadata == "Yes"),]

str(metadf$Bioresponse)
metadf$Stress <- as.factor(metadf$Stress)
metadf$NativeNone <- as.factor(metadf$NativeNone)
metadf$SppType <- as.factor(metadf$SppType)
metadf$Climate <- as.factor(metadf$Climate)
metadf$Taxon <- as.factor(metadf$Taxon)
metadf$Trophicfunction <- as.factor(metadf$Trophicfunction)
metadf$TrophicLevel <- as.factor(metadf$TrophicLevel)
metadf$Ecosystem <- as.factor(metadf$Ecosystem)
metadf$StressAnomaly = as.numeric(as.character(metadf$StressAnomaly))
metadf$TreatmentMean = as.numeric(as.character(metadf$TreatmentMean))
metadf$SamplesizeC = as.numeric(as.character(metadf$SamplesizeC))
metadf$SamplesizeT = as.numeric(as.character(metadf$SamplesizeT))
metadf$Exptime = as.numeric(as.character(metadf$Exptime))

#"SMD" standardised mean difference
metadf <- escalc(measure="SMD",m1i=TreatmentMean,sd1i=TreatStd,n1i=SamplesizeT,m2i=`Control Mean`,sd2i=ControlStd,n2i=SamplesizeC,data=metadf)
metadf$yi <- metadf$yi*metadf2$Effect.direction ### change sign !!!!
#######    subset data
#for temp/sal data only
tempdf <- metadf[which(metadf$Stress == "Temp"),]
saldf  <- metadf[which(metadf$Stress == "Sal"),]
##   native and non native
nattemp <- tempdf[which(tempdf$NativeNone == "native"),]
nontemp <- tempdf[which(tempdf$NativeNone == "nns"),]
natsal <- subset(x=saldf, NativeNone=="native")
nonsal <- subset(x=saldf, NativeNone=="nns")

res.t <- rma(yi,vi,data=nonsal,method="REML");res.t 
res.t <- rma.mv(yi,vi,data=tempdf,method="REML");res.t #'rma' random-effects models ; "REML" HS" Viechtbauer (2005)
mod <-rma.mv(yi, vi, random = list(~1|Study, ~ 1|Bioresponse, ~1|Species), data=tempdf, method="REML");mod
mod <-rma.mv(yi, vi, random = list(~1|ArticleID/ExperimentID, ~ 1|BioProcess, ~1|Species), data=es1, method="REML")

mod <-rma.mv(yi, vi, random = list(~1|Study, ~ 1|Bioresponse), data=nonsal, method="REML");mod
mod <-rma.mv(yi, vi, random = list(~1|Study), data=nonsal, method="REML");mod
mod <-rma.mv(yi, vi, random = list(~1|Study), data=natsal, method="REML");mod

### see  FinalForest script for random effects models and forest plots
### see plotecode.r for forest polts with ggplot
#######################################################      go to ForestPlotsModerators.R & metafor_moderators.R to run the models

######    below is looking for significant difference between the subgroup outputs

res1 <- rma(yi,vi,subset=Ecosystem=="Marine",data=tempdf);res1#380
res2 <- rma(yi,vi,subset=Ecosystem=="Esturine",data=tempdf);res2#72

res1 <- rma(yi,vi,subset=NativeNone=="native",data=saldf);res1#380
res2 <- rma(yi,vi,subset=NativeNone=="nns",data=saldf);res2#72

res.t <- rma(yi,vi,data=tempdf,method="REML");res.t
res.s <- rma(yi,vi,data=saldf,method="REML");res.s

dat.comp <- data.frame(estimate = c(coef(res.t), coef(res.s)), stderror = c(res.t$se, res.s$se),## this coding tests for differences between the groupings
                       meta = c("Temperature","Salinity"), tau2 = round(c(res.t$tau2, res.s$tau2),3))

dat.comp <- data.frame(estimate = c(coef(res1), coef(res2)), stderror = c(res1$se, res2$se),## this coding tests for differences between the groupings
                       meta = c("Native","Non-native"), tau2 = round(c(res1$tau2, res2$tau2),3))
dat.comp
rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)## this coding tests for differences between the groupings, see http://www.metafor-project.org/doku.php/tips:comp_two_independent_estimates
with(dat.comp, round(c(zval = (estimate[1] - estimate[2])/sqrt(stderror[1]^2 + stderror[2]^2)), 3))



##########mixed effects  for the inclusion of moderators

### subgroup analysis for mixed effects and heterogeneity
res1 <- rma(yi,vi,mods= ~ factor(Bioresponse)-1,data=tempdf);res1 ###mixed effects model to explore this variable
res1 <- rma(yi,vi,mods= ~ factor(TrophicLevel)-1,data=saldf);res1
res1 <- rma(yi,vi,mods= ~ factor(Morph)-1,data=natsal);res1
res2 <- rma(yi,vi,mods= ~ factor(Morph)-1,data=tempdf);res2
res1 <- rma(yi,vi,mods= ~ factor(NativeNone)-1,data=tempdf);res1
res1 <- rma.mv(yi,vi,subset=Bioresponse=="Survival",data=nattemp);res1
res1 <- rma(yi,vi,mods= ~ factor(Climate)-1,data=nonsal);res1
#########sal data
res1 <- rma(yi,vi,mods= ~ factor(Ecosystem)-1,data=tempdf);res1 ###mixed effects model to explore this variable
res1 <- rma(yi,vi,mods= ~ factor(NativeNone)-1,data=saldf);res1
res1 <- rma(yi,vi,mods= ~ factor(Morph)-1,data=saldf);res1
res1 <- rma(yi,vi,mods= ~ factor(Biofouler)-1,data=saldf);res1
res1 <- rma(yi,vi,mods= ~ factor(Climate)-1,data=saldf);res1

#############meta regression of continuos variables 
rg1 <-rma(yi,vi,subset=Bioresponse=="Critical Biological Rate",mods= ~ Exptime, data=natsal);rg1
rg2 <-rma(yi,vi,subset=Bioresponse=="Critical Biological Rate",mods= ~ StressAnomaly, data=natsal);rg2
rg3 <-rma(yi,vi,subset=Bioresponse=="Critical Biological Rate",mods= ~ AbsolutLat, data=natsal);rg3

rg4 <-rma(yi,vi,subset=Bioresponse=="Critical Biological Rate",mods= ~ Exptime + StressAnomaly, data=natsal);rg4
rg5 <-rma(yi,vi,subset=Bioresponse=="Critical Biological Rate",mods= ~ AbsolutLat + StressAnomaly, data=natsal);rg5
rg6 <-rma(yi,vi,subset=Bioresponse=="Critical Biological Rate",mods= ~ AbsolutLat + Exptime, data=natsal);rg6
rg7 <-rma(yi,vi,subset=Bioresponse=="Critical Biological Rate",mods= ~ AbsolutLat + StressAnomaly + Exptime, data=natsal);rg7
AIC(rg1, rg2, rg3,rg4, rg5, rg6, rg7)
#rg7  5 2463.169
AIC(rg7)
#############meta regression of continuos variables  TEMP Data
rg1 <-rma(yi,vi,mods= ~ Exptime-1, data=nattemp);rg1
rg2 <-rma(yi,vi,mods= ~ StressAnomaly, data=nattemp);rg2
rg3 <-rma(yi,vi,mods= ~ AbsolutLat, data=nattemp);rg3
rg4 <-rma(yi,vi,mods= ~ Exptime + StressAnomaly + AbsolutLat, data=nattemp);rg4
rg5 <-rma(yi,vi,mods= ~ AbsolutLat + StressAnomaly, data=nattemp);rg5
rg6 <-rma(yi,vi,mods= ~ AbsolutLat + Exptime, data=nattemp);rg6
rg7 <-rma(yi,vi,mods= ~ StressAnomaly + Exptime, data=nattemp);rg7
AIC(rg1, rg2,rg3, rg4, rg5, rg6, rg7)

AIC(rg7)
#############meta regression of continuos variables  TEMP Data
rg1 <-rma(yi,vi,mods= ~ Exptime, data=nontemp);rg1
rg2 <-rma(yi,vi,mods= ~ StressAnomaly, data=nontemp);rg2
rg3 <-rma(yi,vi,mods= ~ AbsolutLat, data=nontemp);rg3
rg4 <-rma(yi,vi,mods= ~ Exptime + StressAnomaly + AbsolutLat, data=nontemp);rg4
rg5 <-rma(yi,vi,mods= ~ AbsolutLat + StressAnomaly, data=nontemp);rg5
rg6 <-rma(yi,vi,mods= ~ AbsolutLat + Exptime, data=nontemp);rg6
rg7 <-rma(yi,vi,mods= ~ StressAnomaly + Exptime, data=nontemp);rg7
AIC(rg1, rg2,rg3, rg4, rg5, rg6, rg7)

#############meta regression of continuos variables  TEMP Data
rg1 <-rma(yi,vi,mods= ~ Exptime, data=natsal);rg1
rg2 <-rma(yi,vi,mods= ~ StressAnomaly, data=natsal);rg2
rg3 <-rma(yi,vi,mods= ~ AbsolutLat, data=natsal);rg3
rg4 <-rma(yi,vi,mods= ~ Exptime + StressAnomaly + AbsolutLat, data=natsal);rg4
rg5 <-rma(yi,vi,mods= ~ AbsolutLat + StressAnomaly, data=natsal);rg5
rg6 <-rma(yi,vi,mods= ~ AbsolutLat + Exptime, data=natsal);rg6
rg7 <-rma(yi,vi,mods= ~ StressAnomaly + Exptime, data=natsal);rg7
AIC(rg1, rg2,rg3, rg4, rg5, rg6, rg7)

AIC(rg7)
#############meta regression of continuos variables  TEMP Data
rg1 <-rma(yi,vi,mods= ~ Exptime, data=nonsal);rg1
rg2 <-rma(yi,vi,mods= ~ StressAnomaly, data=nonsal);rg2
rg3 <-rma(yi,vi,mods= ~ AbsolutLat, data=nonsal);rg3
rg4 <-rma(yi,vi,mods= ~ Exptime + StressAnomaly + AbsolutLat, data=nonsal);rg4
rg5 <-rma(yi,vi,mods= ~ AbsolutLat + StressAnomaly, data=nonsal);rg5
rg6 <-rma(yi,vi,mods= ~ AbsolutLat + Exptime, data=nonsal);rg6
rg7 <-rma(yi,vi,mods= ~ StressAnomaly + Exptime, data=nonsal);rg7
AIC(rg1, rg2,rg3, rg4, rg5, rg6, rg7)

#############meta regression of continuos variables  TEMP Data
rg1 <-rma(yi,vi,mods= ~ Exptime, data=tempdf);rg1
rg2 <-rma(yi,vi,mods= ~ StressAnomaly, data=tempdf);rg2
rg3 <-rma(yi,vi,mods= ~ AbsolutLat, data=tempdf);rg3
rg4 <-rma(yi,vi,mods= ~ Exptime + StressAnomaly + AbsolutLat, data=tempdf);rg4
rg41 <-rma(yi,vi,mods= ~ Exptime + StressAnomaly + AbsolutLat +NativeNone, data=tempdf);rg41
rg5 <-rma(yi,vi,mods= ~ AbsolutLat + StressAnomaly, data=tempdf);rg5
rg6 <-rma(yi,vi,mods= ~ AbsolutLat + Exptime, data=tempdf);rg6
rg7 <-rma(yi,vi,mods= ~ StressAnomaly + Exptime, data=tempdf);rg7
AIC(rg1, rg2,rg3, rg4, rg5, rg6, rg7, rg41)
ddat=data.frame("estimate"=estimate, "variance"=variance, "sterror"=se, "Stress"=stress,"N"=sampsize, "Bioresponse"=labels)
write.csv(ddat,"metaOutputs.csv")

library(Hmisc);library(car);library(mgcv)
library(maps)
library(metafor)

metadf <- read_csv("metadf.csv")
View(metadf)

#check that variables are factors or numeric
str(data$Bioresponse)
data$Bioresponse <- as.factor(data$Bioresponse)
data$`Control Mean` = as.numeric(as.character(data$`Control Mean`))
data$TreatmentMean = as.numeric(as.character(data$TreatmentMean))
data$SamplesizeC = as.numeric(as.character(data$SamplesizeC))
data$SamplesizeT = as.numeric(as.character(data$SamplesizeT))
data$Exptime = as.numeric(as.character(data$Exptime))
str(data)
head(data)
summary(data$Bioresponse)


MA <- escalc(measure="SMD",m1i=TreatmentMean,sd1i=TreatStd,n1i=SamplesizeT,m2i=`Control Mean`,sd2i=ControlStd,n2i=SamplesizeC,data=data)
View(MA)

MA$yi <- MA$yi*MA$Effect.direction ### change sign !!!!

res.MA <- rma(yi,vi,data=MA,method="REML");res.MA  #random-effects models ; "HS" Viechtbauer (2005)

### subgroup analysis for Bioresponse - entire data
levels(data$Bioresponse)
res.response <- rma(yi,vi,mods= ~ factor(Bioresponse)-1,data=MA);res.response

res.cr <- rma.mv(yi,vi,subset=Bioresponse=="Critical Biological Rate",random = list(~1|Study, ~ 1|Species),data=nontemp);res.cr
res.fa <- rma(yi,vi,subset=Bioresponse=="Feeding Ability",random = list(~1|Study, ~ 1|Species),data=nontemp);res.fa
res.gr <- rma(yi,vi,subset=Bioresponse=="Growth",random = list(~1|Study, ~ 1|Species),data=nontemp);res.gr
res.mi <- rma(yi,vi,subset=Bioresponse=="Metabolic Indicators",data=MA);res.mi
res.oh <- rma(yi,vi,subset=Bioresponse=="Overall Health",data=MA);res.oh
res.rp <- rma(yi,vi,subset=Bioresponse=="Reproduction",data=MA);res.rp
res.su <- rma(yi,vi,subset=Bioresponse=="Survival",data=MA);res.su

## must be in Alphabet order
estimates <- c(coef(res.cr), coef(res.fa), coef(res.gr), coef(res.mi), coef(res.oh), coef(res.rp), coef(res.su))
variances <- c(vcov(res.cr), vcov(res.fa), vcov(res.gr), vcov(res.mi), vcov(res.oh), vcov(res.rp), vcov(res.su))
ddat=data.frame("estimate"=estimate, "variance"=variance, "sterror"=se, "Stress"=stress,"N"=sampsize, "Bioresponse"=labels)
write.csv(ddat,"metaOutputs.csv")

labels <- c("Critical Biological Rate (135)", "Feeding Ability (20)" , "Growth (155)", "Metabolic Indicators (45)","Overall Health (31)", "Reproduction (7)", "Survival (49)")
x11();
forest(estimates, variances, slab=labels,digit=0,annotate=F,xlab="Mean effect size for all data", ylim=c(0,10)) # "psize=1" size of mean box on forest plot                        
addpoly(res.MA, row=0.2, cex=1, atransf=F, mlab="RE Model (445)",annotate=F)

##investigate native and none native (nns)

#normality
tapply(MA$yi, MA$NativeNone, mean) 
tapply(MA$yi, MA$NativeNone, var)
#or 
tapply(MA$yi, MA$NativeNone, shapiro.test)
#or 
bartlett.test(MA$yi, MA$NativeNone)
#> than 0.05 accept the null = no difference in data

#for normal
t.test(MA$yi ~ MA$NativeNone, paired=F)
#for non-normal
wilcox.test(MA$yi ~ MA$NativeNone, paired=F)

#seperating data
nat <- subset(x=MA, NativeNone=="native")
none <- subset(x=MA, NativeNone=="nns")

res.MAnat <- rma(yi,vi,data=nat,method="REML"); res.MAnat
resresponse.nat<- rma(yi,vi,mods= ~ factor(Bioresponse)-1,data=nat);resresponse.nat

res.MAnone <- rma(yi,vi,data=none,method="REML"); res.MAnone
resresponse.none<- rma(yi,vi,mods= ~ factor(Bioresponse)-1,data=none);resresponse.none
