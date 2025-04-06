#   Final meta analysis code

###  -----------------running meta-analysis models and extracting data ---------------------------

####Install packages
library(tidyverse)
library(Hmisc);library(car);library(mgcv)
library(maps)
library(metafor)
#select meta-data
metadf <- Alldata[which(Alldata$metadata == "Yes"),]
#check that variables are factors or numeric
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
metadf$`Treatment var` = as.numeric(as.character(metadf$`Treatment var`))
metadf$`Control var` = as.numeric(as.character(metadf$`Control var`))
metadf$`Control Mean` = as.numeric(as.character(metadf$`Control Mean`))
metadf$SamplesizeC = as.numeric(as.character(metadf$SamplesizeC))
metadf$SamplesizeT = as.numeric(as.character(metadf$SamplesizeT))
metadf$Exptime = as.numeric(as.character(metadf$Exptime))
metadf$logstress <- log(metadf$StressAnomaly)
metadf$logtime <- log(metadf$Exptime)
#"SMD" standardised mean difference
metadf <- escalc(measure="SMD",m1i=TreatmentMean,sd1i=TreatStd,n1i=SamplesizeT,m2i=`Control Mean`,sd2i=ControlStd,n2i=SamplesizeC,data=metadf)
metadf$yi <- metadf$yi*metadf$Effect.direction ### change sign !!!!
#######    subset data
#   native and non native
natdf <- metadf[which(metadf$NativeNone == "native"),]
nondf <- metadf[which(metadf$NativeNone == "nns"),]
#for Temperature/Salinity data only
tempdf <- metadf[which(metadf$Stress == "Temperature"),]
saldf  <- metadf[which(metadf$Stress == "Salinity"),]
##   native and non native
nattemp <- tempdf[which(tempdf$NativeNone == "native"),]
nontemp <- tempdf[which(tempdf$NativeNone == "nns"),]
natsal <- subset(x=saldf, NativeNone=="native")
nonsal <- subset(x=saldf, NativeNone=="nns")
# correct groupings
# Temperature / sal data
nat <- metadf %>% filter(NativeNone == "native")
tCBR <- metadf%>%filter(Stress=="Temperature" & Bioresponse=="Critical Biological Rate")  
tMI <- metadf%>%filter(Stress=="Temperature" & Bioresponse=="Metabolic Indicators")  
tFA <- metadf%>%filter(Stress=="Temperature" & Bioresponse=="Feeding Ability") 
tG <- metadf%>%filter(Stress=="Temperature" & Bioresponse=="Growth") 
tOH <- metadf%>%filter(Stress=="Temperature" & Bioresponse=="Overall Health") 
tRep <- metadf%>%filter(Stress=="Temperature" & Bioresponse=="Reproduction") 
tRes <- metadf%>%filter(Stress=="Temperature" & Bioresponse=="Respiration") 
tS <- metadf%>%filter(Stress=="Temperature" & Bioresponse=="Survival")
# sal
sCBR <- metadf%>%filter(Stress=="Salinity" & Bioresponse=="Critical Biological Rate")  
sMI <- metadf%>%filter(Stress=="Salinity" & Bioresponse=="Metabolic Indicators")  
sG <- metadf%>%filter(Stress=="Salinity" & Bioresponse=="Growth") 
sRep <- metadf%>%filter(Stress=="Salinity" & Bioresponse=="Reproduction") 
sRes <- metadf%>%filter(Stress=="Salinity" & Bioresponse=="Respiration") 
sS <- metadf%>%filter(Stress=="Salinity" & Bioresponse=="Survival")
# Temperature native 
ntCBR <- metadf%>%filter(Stress=="Temperature" & NativeNone=="native" &Bioresponse=="Critical Biological Rate")  
ntMI <- metadf%>%filter(Stress=="Temperature" & NativeNone=="native" &Bioresponse=="Metabolic Indicators")  
ntG <- metadf%>%filter(Stress=="Temperature" & NativeNone=="native" &Bioresponse=="Growth") 
ntOH <- metadf%>%filter(Stress=="Temperature" & NativeNone=="native" &Bioresponse=="Overall Health") 
ntRep <- metadf%>%filter(Stress=="Temperature" & NativeNone=="native" &Bioresponse=="Reproduction") 
ntRes <- metadf%>%filter(Stress=="Temperature" & NativeNone=="native" &Bioresponse=="Respiration") 
ntS <- metadf%>%filter(Stress=="Temperature" & NativeNone=="native" &Bioresponse=="Survival")
# Temperature nns 
nntCBR <- metadf%>%filter(Stress=="Temperature" & NativeNone=="nns" &Bioresponse=="Critical Biological Rate")  
nntMI <- metadf%>%filter(Stress=="Temperature" & NativeNone=="nns" &Bioresponse=="Metabolic Indicators")  
nntG <- metadf%>%filter(Stress=="Temperature" & NativeNone=="nns" &Bioresponse=="Growth") 
nntOH <- metadf%>%filter(Stress=="Temperature" & NativeNone=="nns" &Bioresponse=="Overall Health") 
nntRep <- metadf%>%filter(Stress=="Temperature" & NativeNone=="nns" &Bioresponse=="Reproduction") 
nntRes <- metadf%>%filter(Stress=="Temperature" & NativeNone=="nns" &Bioresponse=="Respiration") 
nntS <- metadf%>%filter(Stress=="Temperature" & NativeNone=="nns" &Bioresponse=="Survival")

# sal native 
nsCBR <- metadf%>%filter(Stress=="Salinity" & NativeNone=="native" &Bioresponse=="Critical Biological Rate")  
nsMI <- metadf%>%filter(Stress=="Salinity" & NativeNone=="native" &Bioresponse=="Metabolic Indicators")  
nsG <- metadf%>%filter(Stress=="Salinity" & NativeNone=="native" &Bioresponse=="Growth") 
nsRep <- metadf%>%filter(Stress=="Salinity" & NativeNone=="native" &Bioresponse=="Reproduction") 
nsS <- metadf%>%filter(Stress=="Salinity" & NativeNone=="native" &Bioresponse=="Survival")
# Salinity nns 
nnsCBR <- metadf%>%filter(Stress=="Salinity" & NativeNone=="nns" &Bioresponse=="Critical Biological Rate")  
nnsMI <- metadf%>%filter(Stress=="Salinity" & NativeNone=="nns" &Bioresponse=="Metabolic Indicators")  
nnsG <- metadf%>%filter(Stress=="Salinity" & NativeNone=="nns" &Bioresponse=="Growth") 
nnsRep <- metadf%>%filter(Stress=="Salinity" & NativeNone=="nns" &Bioresponse=="Reproduction") 
nnsS <- metadf%>%filter(Stress=="Salinity" & NativeNone=="nns" &Bioresponse=="Survival")

# 1. Begin analysis            
# 1.1 Fig 1 - difference between responses under Temperature and salinity

# 1.1 Looking at bioresponses under Temperature

##  using Multivariate Meta-Analysis Models and subsetting for each response ~ study as a random factor (and ~ 1|Species)
#Critical Biological Rate
res1 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=tCBR);res1
predict(res1, newmods=(mean(tCBR$logstress)))
W <- diag(1/tCBR$vi)
X <- model.matrix(res1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res1I <- 100 * sum(res1$sigma2) / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P))) #gives statistic can be thought of as the overall  I sqared
#Feeding Ability
res2 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species),data=tFA);res2#not sig for stress anomly
W <- diag(1/tFA$vi)
X <- model.matrix(res2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res2I <- 100 * sum(res2$sigma2) / (sum(res2$sigma2) + (res2$k-res2$p)/sum(diag(P)));res2I #gives statistic can be thought of as the overall  I sqared
#MI
res3 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=tMI);res3
W <- diag(1/tMI$vi)
X <- model.matrix(res3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res3$sigma2) / (sum(res3$sigma2) + (res3$k-res3$p)/sum(diag(P)));res3I #gives statistic can be thought of as the overall  I sqared
#Growth
res4 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=tG);res4
W <- diag(1/tG$vi)
X <- model.matrix(res4)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res4I <- 100 * sum(res4$sigma2) / (sum(res4$sigma2) + (res4$k-res4$p)/sum(diag(P)));res4I #gives statistic can be thought of as the overall  I sqared
#Overall Health
res5 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logtime)+scale(AbsolutLat),data=tOH);res5
W <- diag(1/tOH$vi)
X <- model.matrix(res5)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res5I <- 100 * sum(res5$sigma2) / (sum(res5$sigma2) + (res5$k-res5$p)/sum(diag(P)));res5I
#Reproduction
res6 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=tRep);res6
W <- diag(1/tRep$vi)
X <- model.matrix(res6)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res6I <- 100 * sum(res6$sigma2) / (sum(res6$sigma2) + (res6$k-res6$p)/sum(diag(P)));res6I
#Respiration
res7 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=tRes);res7
W <- diag(1/tRes$vi)
X <- model.matrix(res7)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res7I <- 100 * sum(res7$sigma2) / (sum(res7$sigma2) + (res7$k-res7$p)/sum(diag(P)));res7I
#survival
res8 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=tS);res4
W <- diag(1/tS$vi)
X <- model.matrix(res8)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res8I <- 100 * sum(res8$sigma2) / (sum(res8$sigma2) + (res8$k-res8$p)/sum(diag(P)));res8I

######      creating a dataframe from the outputs of a model
estimate <- c((coef(res1)[1]), (coef(res2)[1]), (coef(res3)[1]), (coef(res4)[1]), (coef(res5)[1]), (coef(res6)[1]), (coef(res7)[1]), (coef(res8)[1]))
lower <- c((res1$ci.lb[1]), (res2$ci.lb[1]), (res3$ci.lb[1]), (res4$ci.lb[1]) ,(res5$ci.lb[1]), (res6$ci.lb[1]), (res7$ci.lb[1]), (res8$ci.lb[1]))
upper <- c((res1$ci.ub[1]), (res2$ci.ub[1]), (res3$ci.ub[1]), (res4$ci.ub[1]) ,(res5$ci.ub[1]), (res6$ci.ub[1]), (res7$ci.ub[1]), (res8$ci.ub[1]))
stress <- c("Temperature", "Temperature","Temperature", "Temperature", "Temperature","Temperature", "Temperature", "Temperature")
labels <- c("Critical Biological Rate","Feeding Ability" ,"Metabolic Indicators",  "Growth","Health","Reproduction","Respiration","Survival")
sig <- c("non", "non","sigred","non","sigred", "sigred", "non", "non")
samplesize <- c(res1$k,res2$k,res3$k,res4$k,res5$k,res6$k,res7$k,res8$k)
pval <- c(((res1$pval)[1]),((res2$pval)[1]),((res3$pval)[1]),((res4$pval)[1]),((res5$pval)[1]),((res6$pval)[1]),((res7$pval)[1]),((res8$pval)[1]))
I2 <- c(res1I,res2I,res3I,res4I,res5I,res6I,res7I,res8I)
stressmod <- c((coef(res1)[2]),0,(coef(res3)[2]),(coef(res4)[2]),0,(coef(res6)[2]),(coef(res7)[2]),(coef(res8)[2]) )
stressmodspval <- c(((res1$pval)[2]),0,((res3$pval)[2]),((res4$pval)[2]),0,((res6$pval)[2]),((res7$pval)[2]),((res8$pval)[2]) )
timemod <- c((coef(res1)[3]),0,(coef(res3)[3]),(coef(res4)[3]),(coef(res5)[2]),(coef(res6)[3]),(coef(res7)[3]),(coef(res8)[3]) )
timemodspval <- c(((res1$pval)[3]),0,((res3$pval)[3]),((res4$pval)[3]),((res5$pval)[2]),((res6$pval)[3]),((res7$pval)[3]),((res8$pval)[3]) )
ablatmod <- c((coef(res1)[4]),(coef(res2)[4]),(coef(res3)[4]),(coef(res4)[4]),(coef(res5)[3]),(coef(res6)[4]),(coef(res7)[4]),(coef(res8)[4]) )
ablatmodspval <- c(((res1$pval)[4]),((res2$pval)[4]),((res3$pval)[4]),((res4$pval)[4]),((res5$pval)[3]),((res6$pval)[4]),((res7$pval)[4]),((res8$pval)[4]) )
QEvalue <- c(res1$QE,res2$QE, res3$QE,res4$QE,res5$QE,res6$QE,res7$QE,res8$QE)
QEpval <- c(res1$QEp,res2$QEp,res3$QEp,res4$QEp,res5$QEp,res6$QEp,res7$QEp,res8$QEp)
QMvalue <- c(res1$QM,res2$QM,res3$QM,res4$QM,res5$QM,res6$QM,res7$QM,res8$QM)
QMpval <- c(res1$QMp,res2$QMp,res3$QMp,res4$QMp,res5$QMp,res6$QMp,res7$QMp,res8$QMp)

df =data.frame("estimate"=estimate,"upper"=upper, "lower"=lower,"stress"=stress,"Bioresponse"=labels, "significance"=sig, "n"=samplesize, "pval"=pval, "I2"=I2,
               "stressmod"= stressmod, "stressmodspval"=stressmodspval, "timemod"= timemod, "timemodspval"=timemodspval,"ablatmod"=ablatmod, "ablatmodspval"=ablatmodspval,
               "QEvalue"=QEvalue, "QEpval"=QEpval, "QMvalue"=QMvalue,"QMpval"=QMpval)


# 1.2 Looking at bioresponses under salinity
##  using Multivariate Meta-Analysis Models and subsetting for each response ~ study as a random factor (and ~ 1|Species)
##CBR
res1 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=sCBR);res1
W <- diag(1/sCBR$vi)
X <- model.matrix(res1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res1I <- 100 * sum(res1$sigma2) / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P))) #gives statistic can be thought of as the overall  I sqared
#MI
res2 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=sMI);res2
W <- diag(1/sMI$vi)
X <- model.matrix(res2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res2I <- 100 * sum(res2$sigma2) / (sum(res2$sigma2) + (res2$k-res2$p)/sum(diag(P)));res2I
#Growth
res3 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress),data=sG);res3#####
W <- diag(1/sG$vi)
X <- model.matrix(res3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res3$sigma2) / (sum(res3$sigma2) + (res3$k-res3$p)/sum(diag(P)))
#Reproduction
res4 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime),data=sRep);res4
W <- diag(1/sRep$vi)
X <- model.matrix(res4)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res4I <- 100 * sum(res4$sigma2) / (sum(res4$sigma2) + (res4$k-res4$p)/sum(diag(P)))
#Respiration
res5 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species),mods = ~scale(logstress),data=sRes);res5
W <- diag(1/sRes$vi)
X <- model.matrix(res5)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res5I <- 100 * sum(res5$sigma2) / (sum(res5$sigma2) + (res5$k-res5$p)/sum(diag(P)));res5I

#Survival
res6 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species),mods = ~scale(logstress),data=sS);res6
# predict(res6, newmods=(mean(sS$logstress)))
W <- diag(1/sS$vi)
X <- model.matrix(res6)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res6I <- 100 * sum(res6$sigma2) / (sum(res6$sigma2) + (res6$k-res6$p)/sum(diag(P)));res6I

#  export data into dataframe
estimate <- c((coef(res1)[1]), (coef(res2)[1]), (coef(res3)[1]), (coef(res4)[1]),(coef(res5)[1]), (coef(res6)[1]))
lower <- c((res1$ci.lb[1]), (res2$ci.lb[1]), (res3$ci.lb[1]),(res4$ci.lb[1]),(res5$ci.lb[1]),(res6$ci.lb[1]))
upper <- c((res1$ci.ub[1]), (res2$ci.ub[1]),(res3$ci.ub[1]),(res4$ci.ub[1]),(res5$ci.ub[1]),(res6$ci.ub[1]))
stress <- c("Salinity", "Salinity","Salinity", "Salinity", "Salinity","Salinity")
labels <- c("Critical Biological Rate","Metabolic Indicators", "Growth","Reproduction","Respiration","Survival")
sig <- c("sigblue", "non","sigblue","sigblue","sigblue", "sigblue")
samplesize <- c(res1$k,res2$k,res3$k,res4$k,res5$k,res6$k)
pval <- c(((res1$pval)[1]),((res2$pval)[1]),((res3$pval)[1]),((res4$pval)[1]),((res5$pval)[1]),((res6$pval)[1]))
I2 <- c(res1I,res2I,res3I,res4I,res5I,res6I)
stressmod <- c((coef(res1)[2]),(coef(res2)[2]),(coef(res3)[2]),(coef(res4)[2]),(coef(res5)[2]),(coef(res6)[2]))
stressmodspval <- c(((res1$pval)[2]),((res2$pval)[2]),((res3$pval)[2]),((res4$pval)[2]),((res5$pval)[2]),((res6$pval)[2]))
timemod <- c((coef(res1)[3]),(coef(res2)[3]),(coef(res3)[3]),(coef(res4)[3]),0,0)
timemodspval <- c(((res1$pval)[3]),((res2$pval)[3]),((res3$pval)[3]),((res4$pval)[3]),0,0)
ablatmod <- c((coef(res1)[4]),(coef(res2)[4]),(coef(res3)[4]),0,0,0)
ablatmodspval <- c(((res1$pval)[4]),((res2$pval)[4]),((res3$pval)[4]),0,0,0)
QEvalue <- c(res1$QE,res2$QE, res3$QE,res4$QE,res5$QE,res6$QE)
QEpval <- c(res1$QEp,res2$QEp,res3$QEp,res4$QEp,res5$QEp,res6$QEp)
QMvalue <- c(res1$QM,res2$QM,res3$QM,res4$QM,res5$QM,res6$QM)
QMpval <- c(res1$QMp,res2$QMp,res3$QMp,res4$QMp,res5$QMp,res6$QMp)

df2 =data.frame("estimate"=estimate,"upper"=upper, "lower"=lower,"stress"=stress,"Bioresponse"=labels, "significance"=sig, "n"=samplesize, "pval"=pval, "I2"=I2,  
            "stressmod"= stressmod, "stressmodspval"=stressmodspval, "timemod"= timemod, "timemodspval"=timemodspval, "ablatmod"=ablatmod, "ablatmodspval"=ablatmodspval, 
            "QEvalue"=QEvalue, "QEpval"=QEpval, "QMvalue"=QMvalue,"QMpval"=QMpval)

#  combine the dataframes to form the full set of data for ggplot
fullmod <- rbind(df,df2)
#  export
fullmod <- as.data.frame(fullmod)
write.csv(fullmod, "fullmodfinal.csv", row.names = FALSE)
Salmod <- as.data.frame(Salmod)
write.csv(Salmod, "Salmodfinal.csv", row.names = FALSE)
tempmod <- as.data.frame(tempmod)
write.csv(tempmod, "tempmodfinal.csv", row.names = FALSE)
##### 2 Fig  - difference between responses of native and nonnatives under Temperature and salinity

# 2.1 Looking at bioresponses of NATIVES under Temperature (natTemp data)

#Critical Biological Rate
res1 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(AbsolutLat),data=ntCBR);res1#Critical Biological Rate
W <- diag(1/ntCBR$vi)
X <- model.matrix(res1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res1I <- 100 * sum(res1$sigma2) / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P))) 
#MI
res2 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=ntMI);res2
W <- diag(1/ntMI$vi)
X <- model.matrix(res2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res2I <- 100 * sum(res2$sigma2) / (sum(res2$sigma2) + (res2$k-res2$p)/sum(diag(P)));res2I 
#Growth
res3 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(AbsolutLat),data=ntG);res3
W <- diag(1/ntG$vi)
X <- model.matrix(res3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res3$sigma2) / (sum(res3$sigma2) + (res3$k-res3$p)/sum(diag(P)));res3I 
#Overall Health
res4 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(AbsolutLat),data=ntOH);res4
W <- diag(1/ntOH$vi)
X <- model.matrix(res4)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res4I <- 100 * sum(res4$sigma2) / (sum(res4$sigma2) + (res4$k-res4$p)/sum(diag(P)));res4I
#Reproduction
res5 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=ntRep);res5
W <- diag(1/ntRep$vi)
X <- model.matrix(res5)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res5I <- 100 * sum(res5$sigma2) / (sum(res5$sigma2) + (res5$k-res5$p)/sum(diag(P)));res5I
#Respiration
res6 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=ntRes);res6
W <- diag(1/ntRes$vi)
X <- model.matrix(res6)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res6I <- 100 * sum(res6$sigma2) / (sum(res6$sigma2) + (res6$k-res6$p)/sum(diag(P)));res6I
#survival
res7 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=ntS);res7
W <- diag(1/ntS$vi)
X <- model.matrix(res7)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res7I <- 100 * sum(res7$sigma2) / (sum(res7$sigma2) + (res7$k-res7$p)/sum(diag(P)));res7I

######      creating a dataframe from the outputs of a model native Temperature
estimate <- c((coef(res1)[1]),  (coef(res2)[1]), (coef(res3)[1]), (coef(res4)[1]), (coef(res5)[1]), (coef(res6)[1]),(coef(res7)[1]))
lower <- c((res1$ci.lb[1]),  (res2$ci.lb[1]) , (res3$ci.lb[1]) ,(res4$ci.lb[1]) , (res5$ci.lb[1]) , (res6$ci.lb[1]),(res7$ci.lb[1]))
upper <- c((res1$ci.ub[1]),(res2$ci.ub[1]),(res3$ci.ub[1]),(res4$ci.ub[1]),(res5$ci.ub[1]),(res6$ci.ub[1]),(res7$ci.ub[1]))
stress <- c("Temperature", "Temperature","Temperature", "Temperature", "Temperature","Temperature", "Temperature")
labels <- c("Critical Biological Rate","Metabolic Indicators",  "Growth","Health","Reproduction","Respiration","Survival")
status <- c("Native","Native","Native","Native","Native", "Native","Native")
sig <-    c("non", "sigblack","sigblack","sigblack","sigblack", "non", "non")
samplesize <- c(res1$k,res2$k,res3$k,res4$k,res5$k,res6$k,res7$k)
pval <- c(((res1$pval)[1]),((res2$pval)[1]),((res3$pval)[1]),((res4$pval)[1]),((res5$pval)[1]),((res6$pval)[1]),((res7$pval)[1]))
I2 <- c(res1I,res2I,res3I,res4I,res5I,res6I,res7I)
stressmod <- c((coef(res1)[2]),(coef(res2)[2]),(coef(res3)[2]),(coef(res4)[2]),(coef(res5)[2]),(coef(res6)[2]),(coef(res7)[2]))
stressmodspval <- c(((res1$pval)[2]),((res2$pval)[2]),((res3$pval)[2]),((res4$pval)[2]),((res5$pval)[2]),((res6$pval)[2]),((res7$pval)[2]))
timemod <- c(0,(coef(res2)[3]),0,0,(coef(res5)[2]),(coef(res6)[3]),(coef(res7)[3]))
timemodspval <- c(0,((res2$pval)[3]),0,0,((res5$pval)[2]),((res6$pval)[3]),((res7$pval)[3]))
ablatmod <- c((coef(res1)[3]),(coef(res2)[4]),(coef(res3)[3]),(coef(res4)[3]),(coef(res5)[4]),(coef(res6)[4]),(coef(res7)[4]))
ablatmodspval <- c(((res1$pval)[3]),((res2$pval)[4]),((res3$pval)[3]),((res4$pval)[3]),((res5$pval)[4]),((res6$pval)[4]),((res7$pval)[4]))
QEvalue <- c(res1$QE,res2$QE, res3$QE,res4$QE,res5$QE,res6$QE,res7$QE)
QEpval <- c(res1$QEp,res2$QEp,res3$QEp,res4$QEp,res5$QEp,res6$QEp,res7$QEp)
QMvalue <- c(res1$QM,res2$QM,res3$QM,res4$QM,res5$QM,res6$QM,res7$QM)
QMpval <- c(res1$QMp,res2$QMp,res3$QMp,res4$QMp,res5$QMp,res6$QMp,res7$QMp)

df =data.frame("estimate"=estimate,"upper"=upper, "lower"=lower,"stress"=stress,"status"=status,"Bioresponse"=labels, "significance"=sig, "n"=samplesize, "pval"=pval, "I2"=I2, 
               "stressmod"= stressmod, "stressmodspval"=stressmodspval, "timemod"= timemod, "timemodspval"=timemodspval, "ablatmod"=ablatmod, "ablatmodspval"=ablatmodspval,  
              "QEvalue"=QEvalue, "QEpval"=QEpval, "QMvalue"=QMvalue,"QMpval"=QMpval)


# 2.2 Looking at bioresponses of NON_NATIVES under Temperature (nonTemperature data)

#Critical Biological Rate
res1 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nntCBR);res1
W <- diag(1/nntCBR$vi)
X <- model.matrix(res1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res1I <- 100 * sum(res1$sigma2) / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P))) 
#MI
res2 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logtime)+scale(AbsolutLat),data=nntMI);res2
W <- diag(1/nntMI$vi)
X <- model.matrix(res2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res2$sigma2) / (sum(res2$sigma2) + (res2$k-res2$p)/sum(diag(P)));res2I 
#Growth
res3 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nntG);res8
W <- diag(1/nntG$vi)
X <- model.matrix(res3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res3$sigma2) / (sum(res3$sigma2) + (res3$k-res3$p)/sum(diag(P)));res3I 
#Overall Health
res4 <- rma.mv(yi,vi,random = list(~1|Study), data=nntOH);res4
W <- diag(1/nntOH$vi)
X <- model.matrix(res4)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res4I <- 100 * sum(res4$sigma2) / (sum(res4$sigma2) + (res4$k-res4$p)/sum(diag(P)));res4I
#Reproduction
res5 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logtime),data=nntRep);res5
W <- diag(1/nntRep$vi)
X <- model.matrix(res5)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res5I <- 100 * sum(res5$sigma2) / (sum(res5$sigma2) + (res5$k-res5$p)/sum(diag(P)));res5I
#Respiration
res6 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nntRes);res6
W <- diag(1/nntRes$vi)
X <- model.matrix(res6)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res6I <- 100 * sum(res6$sigma2) / (sum(res6$sigma2) + (res6$k-res6$p)/sum(diag(P)));res6I
#survival
res7 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nntS);res7
W <- diag(1/nntS$vi)
X <- model.matrix(res7)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res7I <- 100 * sum(res7$sigma2) / (sum(res7$sigma2) + (res7$k-res7$p)/sum(diag(P)));res7I

######      creating a dataframe from the outputs of a model native Temperature
estimate <- c((coef(res1)[1]),(coef(res2)[1]),(coef(res3)[1]),(coef(res4)[1]),(coef(res5)[1]),(coef(res6)[1]),(coef(res7)[1]))
lower <- c((res1$ci.lb[1]),(res2$ci.lb[1]),(res3$ci.lb[1]),(res4$ci.lb[1]),(res5$ci.lb[1]),(res6$ci.lb[1]),(res7$ci.lb[1]))
upper <- c((res1$ci.ub[1]),(res2$ci.ub[1]),(res3$ci.ub[1]),(res4$ci.ub[1]),(res5$ci.ub[1]),(res6$ci.ub[1]),(res7$ci.ub[1]))
stress <- c("Temperature", "Temperature","Temperature", "Temperature", "Temperature","Temperature", "Temperature")
labels <- c("Critical Biological Rate","Metabolic Indicators",  "Growth","Health","Reproduction","Respiration","Survival")
status <- c("Non-Native","Non-Native","Non-Native","Non-Native","Non-Native", "Non-Native","Non-Native")
sig <-    c("non", "sigred","sigred","non","sigred", "sigred", "non")
samplesize <- c(res1$k,res2$k,res3$k,res4$k,res5$k,res6$k,res7$k)
pval <- c(((res1$pval)[1]),((res2$pval)[1]),((res3$pval)[1]),((res4$pval)[1]),((res5$pval)[1]),((res6$pval)[1]),((res7$pval)[1]))
I2 <- c(res1I,res2I,res3I,res4I,res5I,res6I,res7I)
stressmod <- c((coef(res1)[2]),0,(coef(res3)[2]),0,0,(coef(res6)[2]),(coef(res7)[2]))
stressmodspval <- c(((res1$pval)[2]),0,((res3$pval)[2]),0,0,((res6$pval)[2]),((res7$pval)[2]))
timemod <- c((coef(res1)[3]),(coef(res2)[2]),(coef(res3)[3]),0,(coef(res5)[2]),(coef(res6)[3]),(coef(res7)[3]))
timemodspval <- c(((res1$pval)[3]),((res2$pval)[2]),((res3$pval)[3]),0,((res5$pval)[2]),((res6$pval)[3]),((res7$pval)[3]))
ablatmod <- c((coef(res1)[4]),(coef(res2)[3]),(coef(res3)[4]),0,0,(coef(res6)[4]),(coef(res7)[4]))
ablatmodspval <- c(((res1$pval)[4]),((res2$pval)[3]),((res3$pval)[4]),0,0,((res6$pval)[4]),((res7$pval)[4]))
QEvalue <- c(res1$QE,res2$QE, res3$QE,res4$QE,res5$QE,res6$QE,res7$QE)
QEpval <- c(res1$QEp,res2$QEp,res3$QEp,res4$QEp,res5$QEp,res6$QEp,res7$QEp)
QMvalue <- c(res1$QM,res2$QM,res3$QM,res4$QM,res5$QM,res6$QM,res7$QM)
QMpval <- c(res1$QMp,res2$QMp,res3$QMp,res4$QMp,res5$QMp,res6$QMp,res7$QMp)

df2 =data.frame("estimate"=estimate,"upper"=upper, "lower"=lower,"stress"=stress,"status"=status,"Bioresponse"=labels, "significance"=sig, "n"=samplesize, "pval"=pval, "I2"=I2, 
                   "stressmod"= stressmod, "stressmodspval"=stressmodspval, "timemod"= timemod, "timemodspval"=timemodspval, "ablatmod"=ablatmod, "ablatmodspval"=ablatmodspval,  
                   "QEvalue"=QEvalue, "QEpval"=QEpval, "QMvalue"=QMvalue,"QMpval"=QMpval)

#  combine the dataframes to form the full set of data for ggplot
Tempmod <- rbind(df,df2)

######    below is looking for significant difference between the Temperature native/nonative for standard random effects metaanalysis
dat.comp <- data.frame(estimate = c(coef(res2), coef(res.)), stderror = c(res2$se, res.2$se),## this coding tests for differences between the groupings
                       meta = c("Temperature","Sal"), tau2 = round(c(res2$tau2, res.2$tau2),5))
dat.comp
rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)

dat.comp <- data.frame(estimate = c(pre5$pred, coef(res.5)), stderror = c(pre5$se, res.5$se),## this coding tests for differences between the groupings
                       meta = c("Native","Non-native"), tau2 =round(sum(pre3$sigma2), round(c(res2$tau2),3)))
dat.comp
rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)

dat.comp <- data.frame(estimate = c(pre3$pred, pre.3$pred), stderror = c(pre3$se, pre.3$se),## this coding tests for differences between the groupings
                       meta = c("Native","Non-native"), tau2 =round(sum(pre3$sigma2, pre.3$sigma2), 3))
dat.comp
rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)

# 2.3 Looking at bioresponses of NATIVES under salinity (natsal data)

#Critical Biological Rate
res1 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = scale(logstress),data=nsCBR);res1#Critical Biological Rate
res101 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nsCBR);res101
W <- diag(1/nsCBR$vi)
X <- model.matrix(res101)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res1I <- 100 * sum(res101$sigma2) / (sum(res101$sigma2) + (res101$k-res101$p)/sum(diag(P))) 
#MI
res2 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nsMI);res2
W <- diag(1/nsMI$vi)
X <- model.matrix(res2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res2I <- 100 * sum(res2$sigma2) / (sum(res2$sigma2) + (res2$k-res2$p)/sum(diag(P)));res2I 
#Growth
res3 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nsG);res3
W <- diag(1/nsG$vi)
X <- model.matrix(res3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res3$sigma2) / (sum(res3$sigma2) + (res3$k-res3$p)/sum(diag(P)));res3I 
#Reproduction
res4 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress),data=nsRep);res4
W <- diag(1/nsRep$vi)
X <- model.matrix(res4)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res4I <- 100 * sum(res4$sigma2) / (sum(res4$sigma2) + (res4$k-res4$p)/sum(diag(P)));res4I
#survival
res5 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species),data=nsS);res5 
res501 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress),data=nsS);res501
res502 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logtime),data=nsS);res502
res503 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(AbsolutLat),data=nsS);res503 
res505 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nsS);res505
W <- diag(1/nsS$vi)
X <- model.matrix(res505)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res5I <- 100 * sum(res505$sigma2) / (sum(res505$sigma2) + (res505$k-res505$p)/sum(diag(P)));res5I

######      creating a dataframe from the outputs of a model native Temperature
estimate <- c((coef(res1)[1]),(coef(res2)[1]),(coef(res3)[1]),(coef(res4)[1]),(coef(res5)[1]))
lower <- c((res1$ci.lb[1]),(res2$ci.lb[1]),(res3$ci.lb[1]),(res4$ci.lb[1]),(res5$ci.lb[1]))
upper <- c((res1$ci.ub[1]),(res2$ci.ub[1]),(res3$ci.ub[1]),(res4$ci.ub[1]),(res5$ci.ub[1]))
stress <- c("Salinity", "Salinity", "Salinity","Salinity", "Salinity")
labels <- c("Critical Biological Rate","Metabolic Indicators", "Growth","Reproduction","Survival")
status <- c("Native","Native","Native","Native","Native" )
sig <-    c("sigblack", "non","sigblack","sigblack","sigblack")
samplesize <- c(res1$k,res2$k,res3$k,res4$k,res5$k)
pval <- c(((res1$pval)[1]),((res2$pval)[1]),((res3$pval)[1]),((res4$pval)[1]),((res5$pval)[1]))
I2 <- c(res1I,res2I,res3I,res4I,res5I)
stressmod <- c((coef(res101)[2]),(coef(res2)[2]),(coef(res3)[2]),(coef(res4)[2]),(coef(res501)[2]))
stressmodspval <- c(((res101$pval)[2]),((res2$pval)[2]),((res3$pval)[2]),((res4$pval)[2]),((res501$pval)[2]))
timemod <- c((coef(res101)[3]),(coef(res2)[3]),(coef(res3)[3]),0,(coef(res502)[2]))
timemodspval <- c(((res101$pval)[3]),((res2$pval)[3]),((res3$pval)[3]),0,((res502$pval)[2]))
ablatmod <- c((coef(res101)[4]),(coef(res2)[4]),(coef(res3)[4]),0,(coef(res503)[2]))
ablatmodspval <- c(((res101$pval)[4]),((res2$pval)[4]),((res3$pval)[4]),0,((res503$pval)[2]))
QEvalue <- c(res101$QE,res2$QE, res3$QE,res4$QE,res505$QE)
QEpval <- c(res101$QEp,res2$QEp,res3$QEp,res4$QEp,res505$QEp)
QMvalue <- c(res101$QM,res2$QM,res3$QM,res4$QM,res505$QM)
QMpval <- c(res101$QMp,res2$QMp,res3$QMp,res4$QMp,res505$QMp)

df =data.frame("estimate"=estimate,"upper"=upper, "lower"=lower,"stress"=stress,"status"=status,"Bioresponse"=labels, "significance"=sig, "n"=samplesize, "pval"=pval, "I2"=I2, 
               "stressmod"= stressmod, "stressmodspval"=stressmodspval, "timemod"= timemod, "timemodspval"=timemodspval, "ablatmod"=ablatmod, "ablatmodspval"=ablatmodspval,  
               "QEvalue"=QEvalue, "QEpval"=QEpval, "QMvalue"=QMvalue,"QMpval"=QMpval)

# 2.4 Looking at bioresponses of NON_NATIVES under salinity (nonsal data)

##  using Multivariate Meta-Analysis Models and subsetting for each response ~ study as a random factor (and ~ 1|Species)
#Critical Biological Rate
res1 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nnsCBR);res8
W <- diag(1/nnsCBR$vi)
X <- model.matrix(res1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res1I <- 100 * sum(res1$sigma2) / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P))) 
#MI
res2 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species),data=nnsMI);res2
W <- diag(1/nnsMI$vi)
X <- model.matrix(res2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res2I <- 100 * sum(res2$sigma2) / (sum(res2$sigma2) + (res2$k-res2$p)/sum(diag(P)));res2I 
#Growth
res3 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=nnsG);res3
W <- diag(1/nnsG$vi)
X <- model.matrix(res3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res3$sigma2) / (sum(res3$sigma2) + (res3$k-res3$p)/sum(diag(P)));res3I 
#Reproduction
res4 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species),data=nnsRep);res4
W <- diag(1/nnsRep$vi)
X <- model.matrix(res4)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res4I <- 100 * sum(res4$sigma2) / (sum(res4$sigma2) + (res4$k-res4$p)/sum(diag(P)));res4I
#survival
res5<- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species), mods = ~scale(logtime),data=nnsS);res5
W <- diag(1/nnsS$vi)
X <- model.matrix(res5)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res5I <- 100 * sum(res5$sigma2) / (sum(res5$sigma2) + (res5$k-res5$p)/sum(diag(P)));res5I

######      creating a dataframe from the outputs of a model native Temperature
estimate <- c((coef(res1)[1]),(coef(res2)[1]), (coef(res3)[1]),(coef(res4)[1]),(coef(res5)[1]))
lower <- c((res1$ci.lb[1]),(res2$ci.lb[1]),(res3$ci.lb[1]),(res4$ci.lb[1]),(res5$ci.lb[1]))
upper <- c((res1$ci.ub[1]),(res2$ci.ub[1]),(res3$ci.ub[1]),(res4$ci.ub[1]),(res5$ci.ub[1]))
stress <- c("Salinity", "Salinity","Salinity", "Salinity", "Salinity")
labels <- c("Critical Biological Rate","Metabolic Indicators",  "Growth","Reproduction","Survival")
status <- c("Non-Native","Non-Native","Non-Native","Non-Native","Non-Native")
sig <-    c("non", "non","non","non","non")
samplesize <- c(res1$k,res2$k,res3$k,res4$k,res5$k)
pval <- c(((res1$pval)[1]),((res2$pval)[1]),((res3$pval)[1]),((res4$pval)[1]),((res5$pval)[1]))
I2 <- c(res1I,res2I,res3I,res4I,res5I)
stressmod <- c((coef(res1)[2]),0,(coef(res3)[2]),0,0)
stressmodspval <- c(((res1$pval)[2]),0,((res3$pval)[2]),0,0)
timemod <- c((coef(res1)[3]),0,(coef(res3)[3]),(coef(res4)[3]),(coef(res5)[2]))
timemodspval <- c(((res1$pval)[3]),0,((res3$pval)[3]),0,((res5$pval)[2]))
ablatmod <- c((coef(res1)[4]),0,(coef(res3)[4]),0,0)
ablatmodspval <- c(((res1$pval)[4]),0,((res3$pval)[4]),0,0)
QEvalue <- c(res1$QE,res2$QE, res3$QE,res4$QE,res5$QE)
QEpval <- c(res1$QEp,res2$QEp,res3$QEp,res4$QEp,res5$QEp)
QMvalue <- c(res1$QM,res2$QM,res3$QM,res4$QM,res5$QM)
QMpval <- c(res1$QMp,res2$QMp,res3$QMp,res4$QMp,res5$QMp)

df2 =data.frame("estimate"=estimate,"upper"=upper, "lower"=lower,"stress"=stress,"status"=status,"Bioresponse"=labels, "significance"=sig, "n"=samplesize, "pval"=pval, "I2"=I2, 
                "stressmod"= stressmod, "stressmodspval"=stressmodspval, "timemod"= timemod, "timemodspval"=timemodspval, "ablatmod"=ablatmod, "ablatmodspval"=ablatmodspval,  
                "QEvalue"=QEvalue, "QEpval"=QEpval, "QMvalue"=QMvalue,"QMpval"=QMpval)

#  combine the dataframes to form the full set of data for ggplot
Salmod <- rbind(df,df2)

######    below is looking for significant difference between the Temperature native/nonative
dat.comp <- data.frame(estimate = c(coef(res1), coef(res.1)), stderror = c(res1$se, res.1$se),## this coding tests for differences between the groupings
                       meta = c("Native","Non-native"), tau2 = round(c(res1$tau2, res.1$tau2),3))
rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)


########### life form Temperature
plnatTemperature <- metadf%>%filter(Stress=="Temperature" & Lifeform=="Plant" & NativeNone=="native")  
plnonTemperature <- metadf%>%filter(Stress=="Temperature" & Lifeform=="Plant" & NativeNone=="nns")  
annatTemperature <- metadf%>%filter(Stress=="Temperature" & Lifeform=="Animal" & NativeNone=="native")  
annonTemperature <- metadf%>%filter(Stress=="Temperature" & Lifeform=="Animal" & NativeNone=="nns")

plnatsal <- metadf%>%filter(Stress=="Sal" & Lifeform=="Plant" & NativeNone=="native")  
plnonsal <- metadf%>%filter(Stress=="Sal" & Lifeform=="Plant" & NativeNone=="nns")  
annatsal <- metadf%>%filter(Stress=="Sal" & Lifeform=="Animal" & NativeNone=="native")
annonsal <- metadf%>%filter(Stress=="Sal" & Lifeform=="Animal" & NativeNone=="nns")  

#plant  native Temperature
res1 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=plnatTemperature)
W <- diag(1/plnatTemperature$vi)
X <- model.matrix(res1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res1I <- 100 * sum(res1$sigma2) / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P)));res1I 
#plant  nns Temperature
res2 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=plnonTemperature);res2
W <- diag(1/plnonTemperature$vi)
X <- model.matrix(res2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res2I <- 100 * sum(res2$sigma2) / (sum(res2$sigma2) + (res2$k-res2$p)/sum(diag(P)));res2I
#animal  native Temperature
res3 <- rma.mv(yi,vi,random = list(~1|Study, ~1|Species,~1|Bioresponse),mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=annatTemperature);res3
W <- diag(1/annatTemperature$vi)
X <- model.matrix(res3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res3$sigma2) / (sum(res3$sigma2) + (res3$k-res3$p)/sum(diag(P)));res3I
#animal  nns Temperature
res4 <- rma.mv(yi,vi,random = list(~1|Study, ~1|Species),mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=annonTemperature);res4
W <- diag(1/annonTemperature$vi)
X <- model.matrix(res4)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res4I <- 100 * sum(res4$sigma2) / (sum(res4$sigma2) + (res4$k-res4$p)/sum(diag(P)));res4I

#trophic level salinity
#plant  native sal
res5 <- rma.mv(yi,vi,random = list(~1|Study, ~1|Species,~1|Bioresponse),mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=plnatsal);res5
W <- diag(1/plnatsal$vi)
X <- model.matrix(res5)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res5I <- 100 * sum(res5$sigma2) / (sum(res5$sigma2) + (res5$k-res5$p)/sum(diag(P)));res5I
#plant  nns sal
res6 <- rma.mv(yi,vi,random = list(~1|Study, ~1|Species),mods = ~scale(logstress)+scale(logtime),data=plnonsal);res6
W <- diag(1/plnonsal$vi)
X <- model.matrix(res6)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res6I <- 100 * sum(res6$sigma2) / (sum(res6$sigma2) + (res6$k-res6$p)/sum(diag(P)));res6I
#animal  native sal
res7 <- rma.mv(yi,vi,random = list(~1|Study, ~1|Species),mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=annatsal);res7
W <- diag(1/annatsal$vi)
X <- model.matrix(res7)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res7I <- 100 * sum(res7$sigma2) / (sum(res7$sigma2) + (res7$k-res7$p)/sum(diag(P)));res7I
#animal  nns sal
res8 <- rma.mv(yi,vi,random = list(~1|Study, ~1|Species, ~1|Bioresponse),mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=annonsal)
W <- diag(1/annonsal$vi)
X <- model.matrix(res8)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res8I <- 100 * sum(res8$sigma2) / (sum(res8$sigma2) + (res8$k-res8$p)/sum(diag(P)));res8I

#  extracting the data
estimate <- c((coef(res1)[1]), (coef(res2)[1]), (coef(res3)[1]), (coef(res4)[1]), (coef(res5)[1]), (coef(res6)[1]), (coef(res7)[1]), (coef(res8)[1]))
lower <- c((res1$ci.lb[1]), (res2$ci.lb[1]), (res3$ci.lb[1]), (res4$ci.lb[1]) ,(res5$ci.lb[1]), (res6$ci.lb[1]), (res7$ci.lb[1]), (res8$ci.lb[1]))
upper <- c((res1$ci.ub[1]), (res2$ci.ub[1]), (res3$ci.ub[1]), (res4$ci.ub[1]) ,(res5$ci.ub[1]), (res6$ci.ub[1]), (res7$ci.ub[1]), (res8$ci.ub[1]))
stress <-  c("Temperature","Temperature","Temperature","Temperature","Salinity","Salinity","Salinity","Salinity")
type <-  c("Plant","Plant","Animal","Animal","Plant","Plant","Animal","Animal")  
status <- c("Native", "Non-native","Native", "Non-native","Native", "Non-native","Native", "Non-native") 
sig <-  c("non","sigred","non","non","sigblack","non", "non","non")
samplesize <- c(res1$k,res2$k,res3$k,res4$k,res5$k,res6$k,res7$k,res8$k)
pval <- c(((res1$pval)[1]),((res2$pval)[1]),((res3$pval)[1]),((res4$pval)[1]),((res5$pval)[1]),((res6$pval)[1]),((res7$pval)[1]),((res8$pval)[1]))
I2 <- c(res1I,res2I,res3I,res4I,res5I,res6I,res7I,res8I)
stressmod <- c((coef(res1)[2]),0,(coef(res3)[2]),(coef(res4)[2]),0,(coef(res6)[2]),(coef(res7)[2]),(coef(res8)[2]) )
stressmodspval <- c(((res1$pval)[2]),((res2$pval)[2]),((res3$pval)[2]),((res4$pval)[2]),((res5$pval)[2]),((res6$pval)[2]),((res7$pval)[2]),((res8$pval)[2]) )
timemod <- c((coef(res1)[3]),(coef(res2)[3]),(coef(res3)[3]),(coef(res4)[3]),(coef(res5)[3]),(coef(res6)[3]),(coef(res7)[3]),(coef(res8)[3]) )
timemodspval <- c(((res1$pval)[3]),((res2$pval)[3]),((res3$pval)[3]),((res4$pval)[3]),((res5$pval)[3]),((res6$pval)[3]),((res7$pval)[3]),((res8$pval)[3]) )
ablatmod <- c((coef(res1)[4]),(coef(res2)[4]),(coef(res3)[4]),(coef(res4)[4]),(coef(res5)[4]),(coef(res6)[4]),(coef(res7)[4]),(coef(res8)[4]) )
ablatmodspval <- c(((res1$pval)[4]),((res2$pval)[4]),((res3$pval)[4]),((res4$pval)[4]),((res5$pval)[4]),((res6$pval)[4]),((res7$pval)[4]),((res8$pval)[4]) )
QEvalue <- c(res1$QE,res2$QE, res3$QE,res4$QE,res5$QE,res6$QE,res7$QE,res8$QE)
QEpval <- c(res1$QEp,res2$QEp,res3$QEp,res4$QEp,res5$QEp,res6$QEp,res7$QEp,res8$QEp)
QMvalue <- c(res1$QM,res2$QM,res3$QM,res4$QM,res5$QM,res6$QM,res7$QM,res8$QM)
QMpval <- c(res1$QMp,res2$QMp,res3$QMp,res4$QMp,res5$QMp,res6$QMp,res7$QMp,res8$QMp)

dat =data.frame("estimate"=estimate,"upper"=upper, "lower"=lower,"stress"=stress,"status"=status,"type"=type, "significance"=sig, "n"=samplesize, "pval"=pval, "I2"=I2, 
                "stressmod"= stressmod, "stressmodspval"=stressmodspval, "timemod"= timemod, "timemodspval"=timemodspval, "ablatmod"=ablatmod, "ablatmodspval"=ablatmodspval,  
                "QEvalue"=QEvalue, "QEpval"=QEpval, "QMvalue"=QMvalue,"QMpval"=QMpval)
#  export
OrgType <- as.data.frame(OrgType)
write.csv(OrgType, "OrgType.csv", row.names = FALSE)
######    below is looking for significant difference between the Temperature native/nonative
dat.comp <- data.frame(estimate = c(coef(res.sp), coef(res.nsp)), stderror = c(res.sp$se, res.nsp$se),## this coding tests for differences between the groupings
                       meta = c("Native","Non-native"), tau2 = round(c(res.sp$tau2, res.nsp$tau2),3))
rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)
  

########   Morph
AdnatTemperature <- metadf%>%filter(Stress=="Temperature" & Morph=="Adult" & NativeNone=="native")  
AdnonTemperature <- metadf%>%filter(Stress=="Temperature" & Morph=="Adult" & NativeNone=="nns")  
JuvnatTemperature <- metadf%>%filter(Stress=="Temperature" & Morph=="Juv" & NativeNone=="native")  
JuvnonTemperature <- metadf%>%filter(Stress=="Temperature" & Morph=="Juv" & NativeNone=="nns")
LarnatTemperature <- metadf%>%filter(Stress=="Temperature" & Morph=="Larval" & NativeNone=="native")
LarnonTemperature <- metadf%>%filter(Stress=="Temperature" & Morph=="Larval" & NativeNone=="nns")

Adnatsal <- metadf%>%filter(Stress=="Sal" & Morph=="Adult" & NativeNone=="native")  
Adnonsal <- metadf%>%filter(Stress=="Sal" & Morph=="Adult" & NativeNone=="nns")  
Juvnatsal <- metadf%>%filter(Stress=="Sal" & Morph=="Juv" & NativeNone=="native")
Juvnonsal <- metadf%>%filter(Stress=="Sal" & Morph=="Juv" & NativeNone=="nns") 
Larnatsal <- metadf%>%filter(Stress=="Sal" & Morph=="Larval" & NativeNone=="native")
Larnonsal <- metadf%>%filter(Stress=="Sal" & Morph=="Larval" & NativeNone=="nns") 

#adult  native Temperature
res1 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=AdnatTemperature);res1
W <- diag(1/AdnatTemperature$vi)
X <- model.matrix(res1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res1I <- 100 * sum(res1$sigma2) / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P)));res1I 
#adult  nns Temperature
res2 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=AdnonTemperature);res2
W <- diag(1/AdnonTemperature$vi)
X <- model.matrix(res2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res2I <- 100 * sum(res2$sigma2) / (sum(res2$sigma2) + (res2$k-res2$p)/sum(diag(P)));res2I
#juv  native Temperature
res3 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=JuvnatTemperature);res3
W <- diag(1/JuvnatTemperature$vi)
X <- model.matrix(res3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res3I <- 100 * sum(res3$sigma2) / (sum(res3$sigma2) + (res3$k-res3$p)/sum(diag(P)));res3I
#juv  nns Temperature
res4 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logtime)+scale(AbsolutLat),data=JuvnonTemperature);res4
W <- diag(1/JuvnonTemperature$vi)
X <- model.matrix(res4)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res4I <- 100 * sum(res4$sigma2) / (sum(res4$sigma2) + (res4$k-res4$p)/sum(diag(P)));res4I
#larval  native Temperature
res5 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=LarnatTemperature);res5
W <- diag(1/LarnatTemperature$vi)
X <- model.matrix(res5)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res5I <- 100 * sum(res5$sigma2) / (sum(res5$sigma2) + (res5$k-res5$p)/sum(diag(P)));res5I
#larval  nns Temperature
res6 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logtime)+scale(AbsolutLat),data=LarnonTemperature);res6
W <- diag(1/LarnonTemperature$vi)
X <- model.matrix(res6)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res6I <- 100 * sum(res6$sigma2) / (sum(res6$sigma2) + (res6$k-res6$p)/sum(diag(P)));res6I

# salinity
#adult  native sa;
res7 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=Adnatsal);res7
W <- diag(1/Adnatsal$vi)
X <- model.matrix(res7)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res7I <- 100 * sum(res7$sigma2) / (sum(res7$sigma2) + (res7$k-res7$p)/sum(diag(P)));res7I 
#adult  nns 
res8 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=Adnonsal);res8
W <- diag(1/Adnonsal$vi)
X <- model.matrix(res8)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res8I <- 100 * sum(res8$sigma2) / (sum(res8$sigma2) + (res8$k-res8$p)/sum(diag(P)));res8I
#juv  native 
res91 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logtime)+scale(AbsolutLat),data=Juvnatsal);res91
res9 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logtime),data=Juvnatsal);res9
W <- diag(1/Juvnatsal$vi)
X <- model.matrix(res9)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res9I <- 100 * sum(res9$sigma2) / (sum(res9$sigma2) + (res9$k-res9$p)/sum(diag(P)));res9I

#larval  native 
res11 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime),data=Larnatsal);res5
W <- diag(1/Larnatsal$vi)
X <- model.matrix(res11)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res11I <- 100 * sum(res11$sigma2) / (sum(res11$sigma2) + (res11$k-res11$p)/sum(diag(P)));res11I
#larval  nns 
res12 <- rma.mv(yi,vi,random = list(~1|Study, ~ 1|Species,~1|Bioresponse), mods = ~scale(logstress)+scale(logtime)+scale(AbsolutLat),data=Larnonsal);res12
W <- diag(1/Larnonsal$vi)
X <- model.matrix(res12)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
res12I <- 100 * sum(res12$sigma2) / (sum(res12$sigma2) + (res12$k-res12$p)/sum(diag(P)));res12I
#  extracting the data
estimate <- c((coef(res1)[1]), (coef(res2)[1]), (coef(res3)[1]), (coef(res4)[1]), (coef(res5)[1]), (coef(res6)[1]), (coef(res7)[1]), (coef(res8)[1]),(coef(res9)[1]),(coef(res11)[1]),(coef(res12)[1]))
lower <- c((res1$ci.lb[1]), (res2$ci.lb[1]), (res3$ci.lb[1]), (res4$ci.lb[1]) ,(res5$ci.lb[1]), (res6$ci.lb[1]), (res7$ci.lb[1]), (res8$ci.lb[1]),(res9$ci.lb[1]),(res11$ci.lb[1]),(res12$ci.lb[1]))
upper <- c((res1$ci.ub[1]), (res2$ci.ub[1]), (res3$ci.ub[1]), (res4$ci.ub[1]) ,(res5$ci.ub[1]), (res6$ci.ub[1]), (res7$ci.ub[1]), (res8$ci.ub[1]),(res9$ci.ub[1]),(res11$ci.ub[1]),(res12$ci.ub[1]))
stress <-  c("Temperature","Temperature","Temperature","Temperature","Temperature","Temperature","Salinity","Salinity","Salinity","Salinity","Salinity")
type <-  c("Adult","Adult","Juvenile","Juvenile","Larval","Larval","Adult","Adult","Juvenile","Larval","Larval")  
status <- c("Native", "Non-native","Native", "Non-native","Native", "Non-native","Native", "Non-native","Native", "Native", "Non-native") 
sig <-  c("non","non","non","non","sigblack","non", "sigblack","sigred","non","sigblack","non")
samplesize <- c(res1$k,res2$k,res3$k,res4$k,res5$k,res6$k,res7$k,res8$k,res9$k,res11$k,res12$k)
pval <- c(((res1$pval)[1]),((res2$pval)[1]),((res3$pval)[1]),((res4$pval)[1]),((res5$pval)[1]),((res6$pval)[1]),((res7$pval)[1]),((res8$pval)[1]),((res9$pval)[1]),((res11$pval)[1]),((res12$pval)[1]))
I2 <- c(res1I,res2I,res3I,res4I,res5I,res6I,res7I,res8I,res9I,res11I,res12I)
stressmod <- c((coef(res1)[2]),0,(coef(res3)[2]),(coef(res4)[2]),0,(coef(res6)[2]),(coef(res7)[2]),(coef(res8)[2]),(coef(res9)[2]),(coef(res11)[2]),(coef(res12)[2]))
stressmodspval <- c(((res1$pval)[2]),((res2$pval)[2]),((res3$pval)[2]),((res4$pval)[2]),((res5$pval)[2]),((res6$pval)[2]),((res7$pval)[2]),((res8$pval)[2]),((res9$pval)[2]),((res11$pval)[2]),((res12$pval)[2]))
timemod <- c((coef(res1)[3]),(coef(res2)[3]),(coef(res3)[3]),(coef(res4)[3]),(coef(res5)[3]),(coef(res6)[3]),(coef(res7)[3]),(coef(res8)[3]),(coef(res9)[3]),(coef(res11)[3]),(coef(res12)[3]))
timemodspval <- c(((res1$pval)[3]),((res2$pval)[3]),((res3$pval)[3]),((res4$pval)[3]),((res5$pval)[3]),((res6$pval)[3]),((res7$pval)[3]),((res8$pval)[3]),((res9$pval)[3]),((res11$pval)[3]),((res12$pval)[3]))
ablatmod <- c((coef(res1)[4]),(coef(res2)[4]),(coef(res3)[4]),(coef(res4)[4]),(coef(res5)[4]),(coef(res6)[4]),(coef(res7)[4]),(coef(res8)[4]),(coef(res91)[3]),(coef(res11)[4]),(coef(res12)[4]))
ablatmodspval <- c(((res1$pval)[4]),((res2$pval)[4]),((res3$pval)[4]),((res4$pval)[4]),((res5$pval)[4]),((res6$pval)[4]),((res7$pval)[4]),((res8$pval)[4]),((res91$pval)[3]),((res11$pval)[4]),((res12$pval)[4]))
QEvalue <- c(res1$QE,res2$QE, res3$QE,res4$QE,res5$QE,res6$QE,res7$QE,res8$QE,res9$QE,res11$QE,res12$QE)
QEpval <- c(res1$QEp,res2$QEp,res3$QEp,res4$QEp,res5$QEp,res6$QEp,res7$QEp,res8$QEp,res9$QEp,res11$QEp,res12$QEp)
QMvalue <- c(res1$QM,res2$QM,res3$QM,res4$QM,res5$QM,res6$QM,res7$QM,res8$QM,res9$QM,res11$QM,res12$QM)
QMpval <- c(res1$QMp,res2$QMp,res3$QMp,res4$QMp,res5$QMp,res6$QMp,res7$QMp,res8$QMp,res9$QMp,res11$QMp,res12$QMp)

dat =data.frame("estimate"=estimate,"upper"=upper, "lower"=lower,"stress"=stress,"status"=status,"type"=type, "significance"=sig, "n"=samplesize, "pval"=pval, "I2"=I2, 
                "stressmod"= stressmod, "stressmodspval"=stressmodspval, "timemod"= timemod, "timemodspval"=timemodspval, "ablatmod"=ablatmod, "ablatmodspval"=ablatmodspval,  
                "QEvalue"=QEvalue, "QEpval"=QEpval, "QMvalue"=QMvalue,"QMpval"=QMpval)
#  export
life <- as.data.frame(life)
write.csv(life, "life.csv", row.names = FALSE)

#  how to figure out th heterogeneity values for each of the sub groups  see https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
#  I^2 for Multilevel and Multivariate Models
#  Multilevel Models

W <- diag(1/subset(natsal,natsal$Bioresponse == "Critical Biological Rate")$vi)
X <- model.matrix(res1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res1$sigma2) / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P))) #gives statistic can be thought of as the overall  I sqared
# the total variance can be attributed to between- and within-cluster heterogeneity separately, see below
100 * res1$sigma2 / (sum(res1$sigma2) + (res1$k-res1$p)/sum(diag(P)))






