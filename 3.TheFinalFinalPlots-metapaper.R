########     FINAL plots
library(tidyverse)
###### stress vs magnitude first onE is sized to be inserted into fig 1
table(metadf$Stress)
metadf$Stress <- as.factor(metadf$Stress)
levels(metadf$Stress) <- c("Salinity", "Temperature")
metadf$Stress = factor(metadf$Stress,levels(metadf$Stress)[c(2,1)]) ###changing the order

d <- metadf %>%
  ggplot(aes(x = Stress, y = StressAnomaly)) +
  geom_violin(trim=FALSE, fill="gray") +
  geom_boxplot(width=0.05)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black",size=10),axis.text.y = element_text(color="black", size=10))+
  scale_y_continuous(breaks=seq(0,30,5))+ 
  labs(y ="Intensity") + labs(x ="")
d
#######   revisions   stess intensity
metadf %>%
  ggplot(aes(x = Stress, y = StressAnomaly, fill=NativeNone)) +
  geom_violin(trim=FALSE, fill="gray") +
  geom_boxplot(width=0.05)+
  theme_classic()+
  theme(strip.text = element_text(size=16),strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(color="black",size=16),axis.text.y = element_text(color="black", size=16), 
        text = element_text(colour="black",size=16))+
  scale_y_continuous(breaks=seq(0,30,5))+ 
  labs(y ="Intensity") + labs(x ="")+scale_fill_manual(name="", values = c("Native" = "black", "Non-native" = "red")) + 
  facet_wrap(.~NativeNone, strip.position = "bottom")

########  ##############    Life stages
table(metadf$Morph)
metadf$Morph <- as.factor(metadf$Morph)
levels(metadf$Morph) <- c("Adult", "Juvenile", "Larval")
metadf$Morph = factor(metadf$Morph,levels(metadf$Morph)[c(3,2,1)]) ###changing the order

a <-  metadf %>%    ###   Life stages distribution of effect
  ggplot(aes(x = Morph, y = yi)) +
  geom_violin(trim=FALSE, fill="gray") +
  geom_boxplot(width=0.05)+
  theme_classic()+ 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  theme(text = element_text(size=18))+
  theme(axis.text.x = element_text(color="black", size=16, angle=45, hjust=1, vjust=1),axis.text.y = element_text(color="black", size=16))+
  #theme(aspect.ratio=1)+  ##for a square shape (irrespective of the data being plotted)
  scale_y_continuous(breaks=seq(-20,5,5))+ 
  labs(y=expression("Hedges' "*italic("d")))+ labs(x ="")
a
########    Forest of life stages into sub catagories
life <- Covariates2[which(Covariates2$type == "Life stages"),]
levels(life$status)
life$status <- as.factor(life$status)
levels(life$status)<- c("Native", "Non-native") #change names
levels(life$covar)
life$covar <- as.factor(life$covar)

levels(life$stress)
life$stress <- as.factor(life$stress)
life$stress = factor(life$stress,levels(life$stress)[c(2,1)]) ###changing the order

plt3 <- ggplot(life) + 
  geom_errorbar(aes(x=covar,ymin=lower, ymax=upper, width = 0.2, col=status), size=1,position = position_dodge(width = 0.4))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = covar, colour=status, fill=sig), pch = 21, size=4, position = position_dodge(width = 0.4)) +
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("")) +
  xlab("")+
  theme_classic() +
  scale_y_continuous(breaks=seq(-4,2,1)) +
  theme(text = element_text(size=16))+
  theme(strip.text = element_text(size=16),
        strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(legend.position=c(.13,.95),legend.text = element_text( size = 12)) + 
  theme(axis.text.x = element_text(color="black", size=16, angle=45, hjust=1, vjust=),axis.text.y = element_text(color="black", size=16))+
  scale_color_manual(name="", values=c("black", "red"))+
  scale_fill_manual(name="", values=c("white", "black", "red"), guide = "none")+
  facet_wrap(.~stress, strip.position = "bottom")
plt3 
##########3   trying to annotate
dat_text <- data.frame(
  label = c("239", "71","77","8", "34","23","62","27","12", "1","7","14"),
  stress   = c("Temperature","Temperature","Temperature","Temperature","Temperature","Temperature",
               "Salinity","Salinity","Salinity", "Salinity","Salinity","Salinity"),
  x     = c(0.65,   1.25, 1.7, 2.25,  2.7, 3.3,  0.7, 1.3,  1.75,  2.2, 2.8,  3.25),
  y     = c(-0.2, 0.2,-0.2,  0.8, -2.8 , 0.7,  -1.3,    -0.8, -3.1,  0.2, -1.8, -1.8))

plt4 <- plt3 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),size=4)
plt4
###arranging plots##
library(ggpubr)
ggarrange(a,plt4, 
          labels = c("a", "b"),  nrow=1)
#############################################################  ABLAT
####                 ########################################   frequency denstiy graph vs ablat  
levels(metadf$NativeNone)
metadf$NativeNone <- as.factor(metadf$NativeNone)
levels(metadf$NativeNone)<- c("Native", "Non-native") #change names   aes(y = ..count..), 

metadf %>%
  ggplot(aes(x = AbsolutLat, fill = NativeNone)) +
  geom_density(alpha = 0.2)+
  theme_coding()+ 
  theme(legend.position=c(.8,.8)) + 
  scale_fill_manual(name="", values = c("Native" = "black", "Non-native" = "brown3")) + 
  scale_x_continuous(breaks=seq(0,90,10),expand=c(0,0),limits=c(0,91)) +
  scale_y_continuous(breaks=seq(0,0.05,0.01),expand=c(0,0))+ 
  labs(y ="Density\n") + labs(x ="\nAbsolute latitude")

#####################################        taxa plots
levels(metadf$NativeNone)
metadf$NativeNone <- as.factor(metadf$NativeNone)
levels(metadf$NativeNone)<- c("Native", "Non-native")

levels(metadf$Phylum3)
metadf$Phylum3 <- as.factor(metadf$Phylum3)
metadf$Phylum3 <- factor(metadf$Phylum3,levels(metadf$Phylum3)[c(11,8,2,4,7,3,6,10,1,12,5,9)])#y = (..count..)/sum(..count..)*100


ggplot(metadf, aes(fill=NativeNone, x=Phylum3, y = (..count..)/sum(..count..)*100)) + 
  geom_bar(position="stack", stat="count",colour = "black")+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,30,5)) +
  theme_coding()+
  theme(legend.position=c(.2,.8), legend.title = element_blank(), plot.margin = unit(c(0.1, 0.5, 0.5, 0.1), units = , "cm"))+
  labs(x = "", y = "Species (%)\n")+scale_fill_manual(name="", values = c("Native" = "grey50", "Non-native" = "brown3")) +
  annotation_custom(text1,xmin=4,xmax=4,ymin=-5,ymax=-5) +  geom_vline(xintercept = 8.5, col = 1, lty = 1) +
  annotation_custom(text2,xmin=10.5,xmax=10.5,ymin=-5,ymax=-5)+ coord_cartesian(clip = "off")

library(grid)
text1 <- textGrob("Animals", gp=gpar(fontsize=12, fontface="bold"))
text2 <- textGrob("Plants", gp=gpar(fontsize=12, fontface="bold"))

#####################        climate types forest plot
clim <- dat
clim <- Covariates2[which(Covariates2$type == "Climate"),]
levels(clim$status)
clim$status <- as.factor(clim$status)
levels(clim$status)<- c("Native", "Non-native") #change names
levels(clim$covar)
clim$covar <- as.factor(clim$covar)
clim$covar = factor(clim$covar,levels(clim$covar)[c(2,1,4,3)]) ###changing the order
levels(clim$stress)
clim$stress <- as.factor(clim$stress)
clim$stress = factor(clim$stress,levels(clim$stress)[c(2,1)]) ###changing the order
# levels(clim$covar)
# levels(clim$covar)<- c("Polar", "Temperate", "Warm temperate", "Tropical") #change names

plt1 <- ggplot(clim) + 
  geom_errorbar(aes(x=covar,ymin=lower, ymax=upper, width = 0.2, col=status), size = 1,position = position_dodge(width = 0.4))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = covar, colour=status), pch = 21, size=4, position = position_dodge(width = 0.4)) +
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("Effect")) +
  xlab("")+
  theme_classic() +
  theme(strip.text = element_text(size=18),
        strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(text = element_text(size=18))+ #theme(plot.margin = unit(c(0, 11, 0, 0.5), "cm"))+
  theme(legend.position=c(.15,.95),legend.text = element_text( size = 13)) + 
  theme(axis.text.x = element_text(color="black", size=16, angle=45, hjust=1, vjust=1),axis.text.y = element_text(color="black", size=18))+
  scale_y_continuous(breaks=seq(-4,1,1)) +
  scale_color_manual(name="", values=c("black", "red"))+
  #scale_fill_manual(name="", values=c("white", "black", "red"), guide = "none")+   fill=sig
  facet_wrap(.~stress, strip.position = "bottom")
plt1


######   annotate
dat_text <- data.frame(
  label = c("33","130","39","81","48","106","15","9","33","30","39", "2", "10"),
  stress   = c("Temperature","Temperature","Temperature","Temperature","Temperature","Temperature","Temperature",
               "Salinity","Salinity","Salinity", "Salinity","Salinity","Salinity"),
  x     = c(0.75, 1.55, 2.35, 3.2, 3.35,    3.6,   4.3,  0.8, 1.7, 2.3,  2.7, 3.25,  4.3),
  y     = c(-2.5, 0.2, -0.25, -0.2, 0.95 , -0.5, 0.5, -1.7, -1.25, -1.1, -2.35, -1.7, 0.1))

pt2 <- plt1 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),size=4)
pt2
###arranging plots##
library(ggpubr)
ggarrange(pt2,c, 
          labels = c("a", "b"),  nrow=1)

############################################      PLANTS AND ANIMALS FOREST PLOT


## subgroup meta-analysis
res1 <- rma(yi,vi,subset=Lifeform=="Animal",data=nattemp);res1
res2 <- rma(yi,vi,subset=Lifeform=="Plant",data=nattemp);res2
res1 <- rma(yi,vi,subset=Lifeform=="Animal",data=nontemp);res1
res2 <- rma(yi,vi,subset=Lifeform=="Plant",data=nontemp);res2
res1 <- rma(yi,vi,subset=Lifeform=="Animal",data=natsal);res1
res2 <- rma(yi,vi,subset=Lifeform=="Plant",data=natsal);res2
res1 <- rma(yi,vi,subset=Lifeform=="Animal",data=nonsal);res1
res2 <- rma(yi,vi,subset=Lifeform=="Plant",data=nonsal);res2
###   plotting 
library(tidyverse)
OrgType <- dat
levels(OrgType$Stress)
OrgType$Stress <- as.factor(OrgType$Stress)
OrgType$Stress = factor(OrgType$Stress,levels(OrgType$Stress)[c(2,1)]) ###changing the order
# levels(OrgType$Status)
# OrgType$Status <- as.factor(OrgType$Status)
# levels(OrgType$Status)<- c("Native", "Non-native") #change names
levels(OrgType$sig)
OrgType$sig <- as.factor(OrgType$sig)
levels(OrgType$type)
OrgType$type <- as.factor(OrgType$type)
levels(OrgType$type)<- c("Animal", "Plant") #change names


plt1 <- ggplot(OrgType) + 
  geom_errorbar(aes(x=type,ymin=lower, ymax=upper, width = 0.2, col=Status), size=1, position = position_dodge(width = 0.4))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = type, colour=Status,fill=sig), pch = 21, size=4, position = position_dodge(width = 0.4)) +
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("                           Hedges' "*italic("d"))) +
  xlab("")+
  # theme_bw()+ theme(panel.grid.major.x=element_blank(),                                          
  #                   panel.grid.minor.x=element_blank(),
  #                   panel.grid.minor.y=element_blank(),
  #                   panel.grid.major.y=element_blank())+
  theme_classic()+
  theme(text = element_text(size=18))+
  theme(strip.text = element_text(size=16),
        strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(legend.position=c(.1,.9),legend.text = element_text(size = 14)) + 
  theme(axis.text.x = element_text(color="black", size=16, angle=45, hjust=1, vjust=),axis.text.y = element_text(color="black", size=16))+
  scale_color_manual(name="", values=c("black", "red"))+
  scale_fill_manual(name="", values=c("white", "black", "red"), guide = "none")+
  facet_wrap(.~Stress, strip.position = "bottom")
plt1

##     Annotate this graph
## create a data table to combine
dat_text <- data.frame(
  label = c("123", "64","227", "38", "35", "35", "46", "7"),
  Stress   = c("Temperature","Temperature","Temperature","Temperature",  ## make sure you assign this to the right variable
               "Salinity","Salinity","Salinity", "Salinity"),
  x     = c(0.82,  1.18,  2.,   2.15,   0.85,  1.17, 1.97,   2.15),
  y     = c(-0.1, 0.2, -0.2, 0.9,  -1.7 , -1, -1.8, -0.3))

plt1 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),size=4)

#####################################################     ECOSYSTEM
levels(Ecosystem$stress)
Ecosystem$stress <- as.factor(Ecosystem$stress)
Ecosystem$stress = factor(Ecosystem$stress,levels(Ecosystem$stress)[c(2,1)]) ###changing the order
levels(Ecosystem$sig)
Ecosystem$sig <- as.factor(Ecosystem$sig)

plt1 <- ggplot(Ecosystem) +           ####  entire ecostsyem
  geom_errorbar(aes(x=ecosystem,ymin=lower, ymax=upper, width = 0.2, col=stress))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = ecosystem, colour=stress, fill=sig), pch = 21, size=2) +
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("Hedges' "*italic("d"))) +
  xlab("")+
  theme_classic() +
  theme(text = element_text(size=16))+
  theme(strip.text = element_text(size=16),
        strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(legend.position=c(.07,.99),legend.text = element_text( size = 14)) + 
  theme(axis.text.x = element_text(color="black", size=14, angle=45, hjust=1, vjust=),axis.text.y = element_text(color="black", size=12))+
  scale_color_manual(name="", values=c("red", "blue"))+
  scale_fill_manual(name="", values=c("white", "blue"), guide = "none")+
  facet_wrap(.~stress, strip.position = "bottom")

plt1
##     Annotate this graph
dat_text <- data.frame(
  label = c("72", "380","49", "74"),
  stress   = c("Temperature","Temperature",  ## make sure you assign this to the right variable
               "Salinity","Salinity"),
  x     = c(0.9,  1.9,    0.9,  1.9),
  y     = c(-0.5, 0.1,   -2.2 ,  -0.8))

plt1 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),size=3)

#######    ecsystem temp/sal  nat/non
levels(Eco$stress)
Eco$stress <- as.factor(Eco$stress)
Eco$stress = factor(Eco$stress,levels(Eco$stress)[c(2,1)]) ###changing the order
levels(Eco$status)
Eco$status <- as.factor(Eco$status)
levels(Eco$sig)
Eco$sig <- as.factor(Eco$sig)

plt1 <- ggplot(Eco) + 
  geom_errorbar(aes(x=ecosystem,ymin=lower, ymax=upper, width = 0.2, col=status), position = position_dodge(width = 0.3))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = ecosystem, colour=status, fill=sig), pch = 21, size=2, position = position_dodge(width = 0.3)) +
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("Hedges' "*italic("d"))) +
  xlab("")+
  theme_classic() +
  theme(text = element_text(size=16))+
  theme(strip.text = element_text(size=16),
        strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(legend.position=c(.07,.95),legend.text = element_text( size = 14)) + 
  theme(axis.text.x = element_text(color="black", size=14, angle=45, hjust=1, vjust=),axis.text.y = element_text(color="black", size=12))+
  scale_color_manual(name="", values=c("black", "red"))+
  scale_fill_manual(name="", values=c("white", "black", "red"), guide = "none")+
  facet_wrap(.~stress, strip.position = "bottom")
plt1
##     Annotate this graph
## create a data table to combine
dat_text <- data.frame(
  label = c("61", "11","289", "91", "43", "6", "38", "36"),
  stress   = c("Temperature","Temperature","Temperature","Temperature",  ## make sure you assign this to the right variable
               "Salinity","Salinity","Salinity", "Salinity"),
  x     = c(0.85,  1.15,  1.8,   2.15,   0.85,  1.15,   1.85,   2.15),
  y     = c(-0.9, 0.8,    -0.1,   0.4,    -2.3 , -2,    -1,  -0.7))

plt1 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),size=3)
