# ----------  meta-analysis plots

library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(cowplot)
library(tidyverse)
###   code for bioresponse plots ####
###  reorder the levels
levels(fullmod$Bioresponse)
fullmod$Bioresponse <- as.factor(fullmod$Bioresponse)
fullmod$Bioresponse <- factor(fullmod$Bioresponse,levels(fullmod$Bioresponse)[c(8,7,6,4,2,3,5,1)])## reorder the levels for the all data
levels(fullmod$significance)
fullmod$significance <- as.factor(fullmod$significance)
# levels(fullmod$shaded)
# fullmod$shaded <- as.factor(fullmod$shaded)
levels(fullmod$stress)
fullmod$stress <- as.factor(fullmod$stress)
fullmod$stress <-  factor(fullmod$stress,levels(fullmod$stress)[c(2,1)])
levels(fullmod$stress) <- c("Warming", "Freshening")# changing name

##   full plot fig.1 ####
p1 <- ggplot(fullmod) + 
  geom_errorbar(aes(x=Bioresponse,ymin=lower, ymax=upper, width = 0.2, col=stress),size=1,  position = position_dodge(width = 0.5))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = Bioresponse, colour=stress, fill=significance), pch = 21, size=4, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("                     Hedges' "*italic("d"))) +
  xlab("")+
  theme_coding()+
  scale_y_continuous(breaks=seq(-5,3,2), expand = c(0,0),limits=c(-6,4)) +
  theme(legend.position=c(.3,.9),legend.text = element_text(size = 13), 
        legend.title = element_blank(),legend.key = element_rect(fill="white", colour="white")) + 
  scale_color_manual(values=c("red", "blue"))+ scale_size_continuous(guide="none")+
  scale_fill_manual(values=c("white", "blue", "red"), guide = "none")
p1
# adding in the little numbers for the sample sizes
q <- p1 + annotate("text", x = 0.6, y = -0.8, label = "37", colour = "black", size=4)+ 
  annotate("text", x = 1.4, y = -3, label = "22", colour = "black", size=4)+
  annotate("text", x = 1.6, y = -0.5, label = "20", colour = "black", size=4) +
  annotate("text", x = 2.5, y = -1.3, label = "12", colour = "black", size=4) +
  annotate("text", x = 2.6, y = -0.9, label = "38", colour = "black", size=4)+
  annotate("text", x = 3.4, y = -0.9, label = "7", colour = "black", size=4)+
  annotate("text", x = 3.7, y = -0.3, label = "31", colour = "black", size=4)+
  annotate("text", x = 4.7, y = 0.4, label = "20", colour = "black", size=4) +
  annotate("text", x = 5.5, y = -0.3, label = "190", colour = "black", size=4)+
  annotate("text", x = 6.5, y = -2., label = "32", colour = "black", size=4)+
  annotate("text", x = 6.6, y = 0.4, label = "30", colour = "black", size=4)+
  annotate("text", x = 7.4, y = 0.3, label = "17", colour = "black", size=4)+
  annotate("text", x = 7.6, y = -0.1, label = "86", colour = "black", size=4)+
  annotate("text", x = 8.41, y = -1.0, label = "37", colour = "black", size=4)
q


##   fig2 ####
tdata <- Tempmod
levels(tdata$Bioresponse)
tdata$Bioresponse <- as.factor(tdata$Bioresponse)
#tdata$Bioresponse <- factor(tdata$Bioresponse,levels(tdata$Bioresponse)[c(9,5,8,7,6,3,2,4,1)])## reorder the levels for the temp data
tdata$Bioresponse <- factor(tdata$Bioresponse,levels(tdata$Bioresponse)[c(7,6,5,3,2,4,1)])## reorder the levels for the temp data
levels(tdata$status)
tdata$status <- as.factor(tdata$status)
tdata$status <-  factor(tdata$status,levels(tdata$status)[c(2,1)])
# levels(tdata$sig)
# tdata$sig <- as.factor(tdata$sig)

##                           Temp
p2 <- ggplot(tdata) + 
  geom_errorbar(aes(x=Bioresponse,ymin=lower, ymax=upper, width = 0.2, col=status), size=1,position = position_dodge(width = 0.5))+
  geom_point(aes(y = estimate, x = Bioresponse, colour=status, fill=significance),pch=21, size=4, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab("") +
  xlab("")+
  theme_coding()+
  theme(axis.text.y = element_blank())+
  scale_y_continuous(breaks=seq(-5,3,2), expand = c(0,0),limits=c(-6,4)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ 
  theme(legend.position=c(.4,.9),legend.text = element_text(size = 13), legend.title = element_blank(),legend.key = element_rect(fill="white", colour="white")) + 
  scale_color_manual(values=c("black", "red"))+ scale_size_continuous(guide=F)+
  scale_fill_manual(values=c("white", "black", "red"), guide = "none")+ annotate("text", x = 5.8, y = -5.5, label = "Warming", colour = "black", size=5)

p2
f <- p2 +
  annotate("text", x = 0.6, y = -1, label = "29", colour = "black", size=4)+ 
  annotate("text", x = 1.4, y = 0.2, label = "8", colour = "black", size=4)+
  annotate("text", x = 1.6, y = 0.3, label = "13", colour = "black", size=4) +
  annotate("text", x = 2.4, y = -2., label = "7", colour = "black", size=4) +
  annotate("text", x = 2.6, y = -1.4, label = "24", colour = "black", size=4)+
  annotate("text", x = 3.4, y = 0.5, label = "14", colour = "black", size=4)+
  annotate("text", x = 3.6, y = -0.6, label = "29", colour = "black", size=4)+
  annotate("text", x = 4.4, y = -0.2, label = "2", colour = "black", size=4) +
  annotate("text", x = 4.55, y = -0.7, label = "148", colour = "black", size=4)+
  annotate("text", x = 5.4, y = 1.2, label = "42", colour = "black", size=4)+
  annotate("text", x = 5.6, y = 0.7, label = "21", colour = "black", size=4)+
  annotate("text", x = 6.4, y = 1.3, label = "9", colour = "black", size=4)+
  annotate("text", x = 6.6, y = -0.1, label = "66", colour = "black", size=4)+
  annotate("text", x = 7.4, y = 0.5, label = "20", colour = "black", size=4)

f 
  

##     salinity  

sdata <- Salmod
levels(sdata$Bioresponse)
sdata$Bioresponse <- as.factor(sdata$Bioresponse)
sdata$Bioresponse <- factor(sdata$Bioresponse,levels(sdata$Bioresponse)[c(5,4,2,3,1)])## reorder for salinity
levels(sdata$status)
sdata$status <-as.factor(sdata$status)
# levels(sdata$sig)
# sdata$sig <- as.factor(sdata$sig)

p3 <- ggplot(sdata) + 
  geom_errorbar(aes(x=Bioresponse,ymin=lower, ymax=upper, width = 0.2, col=status), size=1,position = position_dodge(width = 0.5))+
  geom_point(aes(y = estimate, x = Bioresponse, colour=status, fill=significance),pch=21, size=4, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab("") +
  xlab("")+
  theme_coding()+  
  theme(axis.text.y = element_blank())+
  scale_y_continuous(breaks=seq(-5,3,2), expand = c(0,0),limits=c(-6,4)) +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+ scale_size_continuous(guide=F)+
  scale_fill_manual(values=c("white", "black", "red"), guide = "none")+ annotate("text", x = 4.4, y = -5.5, label = "Freshening", colour = "black", size=5)
p3
r <- p3 + annotate("text", x = 0.6, y = -2.2, label = "9", colour = "black", size=4)+ 
  annotate("text", x = 1.4, y = -0.3, label = "3", colour = "black", size=4)+
  annotate("text", x = 1.6, y = -1, label = "5", colour = "black", size=4) +
  annotate("text", x = 2.4, y = -0.2, label = "2", colour = "black", size=4) +
  annotate("text", x = 2.6, y = -1.2, label = "19", colour = "black", size=4)+
  annotate("text", x = 3.4, y = -2, label = "13", colour = "black", size=4)+
  annotate("text", x = 3.6, y = 0.5, label = "13", colour = "black", size=4)+
  annotate("text", x = 4.4, y = 0.2, label = "4", colour = "black", size=4) +
  annotate("text", x = 4.6, y = -1.8, label = "25", colour = "black", size=4)+
  annotate("text", x = 5.4, y = -0.2, label = "8", colour = "black", size=4)

r

###arranging plots##
library(ggpubr)
###arranging plots##
library(ggpubr)
ggarrange(q, f, r, widths = c(1,.9,.7),
          labels = c("a", "b", "c"), 
          nrow = 1)


##       taxa plots ####
levels(metadf$NativeNone)
metadf$NativeNone <- as.factor(metadf$NativeNone)
levels(metadf$NativeNone)<- c("Native", "Non-native")

levels(metadf$Phylum3)
metadf$Phylum3 <- as.factor(metadf$Phylum3)
metadf$Phylum3 <- factor(metadf$Phylum3,levels(metadf$Phylum3)[c(11,8,2,4,7,3,6,10,1,12,5,9)])#y = (..count..)/sum(..count..)*100



pt1 <- ggplot(metadf, aes(fill=NativeNone, x=Phylum3, y = (..count..)/sum(..count..)*100)) + 
  geom_bar(position="stack", stat="count",colour = "grey50")+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,30,5),limits=c(0,31)) +
  theme_coding1()+
  theme(legend.position=c(.2,.8), legend.title = element_blank(), plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm"))+
  labs(x = "", y = "Species (%)")+scale_fill_manual(name="", values = c("Native" = "grey50", "Non-native" = "brown3"))+
  geom_vline(xintercept = 8.5, col = 1, lty = 1) +
  annotate("text", x = 1.4, y = 30, label = "Animals", colour = "black", size=5)+
  annotate("text", x = 9.3, y = 30, label = "Plants", colour = "black", size=5)
pt1

# ABLAT ####
## frequency denstiy graph vs ablat  
levels(metadf$NativeNone)
metadf$NativeNone <- as.factor(metadf$NativeNone)
levels(metadf$NativeNone)<- c("Native", "Non-native") #change names   aes(y = ..count..), 
library(wesanderson)
names(wes_palettes)


pt2 <- metadf %>%
  ggplot(aes(x = AbsolutLat, fill = NativeNone, colour = NativeNone)) +
  geom_density(alpha = 0.2)+
  theme_coding2()+ 
  theme(legend.position="none")+
  scale_fill_manual(name="", values = c("Native" = "black", "Non-native" = "#990000")) + 
  #scale_fill_manual(name="",values = wes_palette("BottleRocket2", n = 2))+
  scale_colour_manual(name="", values = c("Native" = "grey50", "Non-native" = "#990000")) + 
  #scale_colour_manual(name="",values = wes_palette("BottleRocket2", n = 2))+
  scale_x_continuous(breaks=seq(0,90,10),expand=c(0,0),limits=c(0,92)) +
  scale_y_continuous(breaks=seq(0,0.05,0.01),expand=c(0,0))+ 
  labs(y ="Density") + labs(x ="")
pt2
library(ggpubr)
ggarrange(plt1, plt2, 
          labels = c("a", "b"), widths = c(.7,1),
          nrow = 1)
library(cowplot)
plot_grid(pt1, pt2, nrow = 1, align="h",labels = c("a", "b"))


# Life history stages ####
##   Forest of life stages into sub catagories
life <- dat
levels(life$status)
life$status <- as.factor(life$status)
levels(life$status)<- c("Native", "Non-native") #change names
levels(life$sig)
life$status <- as.factor(life$status)

levels(life$stress)
life$stress <- as.factor(life$stress)
life$stress = factor(life$stress,levels(life$stress)[c(2,1)]) ###changing the order
levels(life$stress) <- c("Warming", "Freshening")# changing name

plt3 <- ggplot(life) + 
  geom_errorbar(aes(x=type,ymin=lower, ymax=upper, width = 0.2, col=status), size=1,position = position_dodge(width = 0.3))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = type, colour=status, fill=significance), pch = 21, size=4, position = position_dodge(width = 0.3)) +
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("                             Hedges' "*italic("d"))) +
  xlab("")+
  theme_classic() +
  scale_y_continuous(breaks=seq(-7,3,2), expand = c(0,0),limits=c(-7.5,4.2)) +
  theme(text = element_text(size=18))+
  theme(strip.text = element_text(size=16),
        strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(legend.position=c(.2,.95),legend.text = element_text( size = 12,face="italic")) + 
  theme(axis.text.x = element_text(size=18, angle=45, hjust=1, vjust=),axis.text.y = element_text(color="black", size=16))+
  scale_color_manual(name="", values=c("black", "grey60"))+
  scale_fill_manual(name="", values=c("white", "black", "grey60"), guide = "none")+
  facet_wrap(.~stress, strip.position = "bottom")
plt3 
##########3   trying to annotate
dat_text <- data.frame(
  label = c("239", "71","77","8", "34","23","62","27","12", "7","14"),
  stress   = c("Warming","Warming","Warming","Warming","Warming","Warming",
               "Freshening","Freshening","Freshening", "Freshening","Freshening"),
  x     = c(0.65,   1.25, 1.7, 2.25,  2.7, 3.3,  0.7, 1.3,  1.75,  2.8,  3.3),
  y     = c(-0.2, 0.2,-0.2,  0.8, -2.2 , 0.9,  -0.8,    -0.8, -2.,   -1.8, -0.2))
levels(dat_text$stress)
dat_text$stress <- as.factor(dat_text$stress)
dat_text$stress = factor(dat_text$stress,levels(dat_text$stress)[c(2,1)]) ###changing the order

fig1 <- plt3 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),size=4)
fig1
## OrgType -  plant and animals ####

OrgType <- dat
levels(OrgType$stress)
OrgType$stress <- as.factor(OrgType$stress)
OrgType$stress = factor(OrgType$stress,levels(OrgType$stress)[c(2,1)]) ###changing the order
levels(OrgType$stress) <- c("Warming", "Freshening")# changing name
# levels(OrgType$Status)
# OrgType$Status <- as.factor(OrgType$Status)
# levels(OrgType$Status)<- c("Native", "Non-native") #change names
levels(OrgType$sig)
OrgType$sig <- as.factor(OrgType$sig)
levels(OrgType$type)
OrgType$type <- as.factor(OrgType$type)
levels(OrgType$type)<- c("Animal", "Plant") #change names

pt3 <- ggplot(OrgType) + 
  geom_errorbar(aes(x=type,ymin=lower, ymax=upper, width = 0.2, col=status), size=1, position = position_dodge(width = 0.3))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = type, colour=status,fill=significance), pch = 21, size=4, position = position_dodge(width = 0.3)) +
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  #ylab(expression("Hedges' "*italic("d"))) +
  ylab("")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(breaks=seq(-7,3,2), expand = c(0,0),limits=c(-7.5,4.2)) +
  theme(text = element_text(size=18))+
  theme(strip.text = element_text(size=16),
        strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=18, angle=45, hjust=1, vjust=),axis.text.y = element_text(color="black", size=16))+
  scale_color_manual(name="", values=c("black", "grey60"))+
  scale_fill_manual(name="", values=c("white", "black", "grey60"), guide = "none")+
  facet_wrap(.~stress, strip.position = "bottom")+
  theme(axis.text.y = element_blank())
pt3
##     Annotate this graph
## create a data table to combine
dat_text <- data.frame(
  label = c("123", "64","227", "38", "35", "35", "46", "7"),
  stress   = c("Warming","Warming","Warming","Warming",  ## make sure you assign this to the right variable
               "Freshening","Freshening","Freshening", "Freshening"),
  x     = c(0.7,  1.2,  1.68,   2.22,   0.8,  1.2, 1.75,   2.2),
  y     = c(-0.1, 0.2, -0.2, 2,  -1.1 , -1.3, -4.4, -0.3))

fig2 <- pt3 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),size=4)
fig2

library(ggpubr)
ggarrange(fig1, fig2, 
          labels = c("a", "b"), widths = c(1,1),
          nrow = 1)
library(cowplot)
plot_grid(fig1, fig2,  nrow = 1, align="h",rel_widths = c(1,1), labels = c("a", "b"))


##STRESS anomly   -   violin plots ####
##stress vs magnitude first onE is sized to be inserted into fig 1
table(metadf$Stress)
metadf$Stress <- as.factor(metadf$Stress)
metadf$Stress = factor(metadf$Stress,levels(metadf$Stress)[c(2,1)]) ###changing the order
levels(metadf$Stress) <- c("Warming", "Freshening")# changing name

##   revisions   stess intensity
pt4 <- metadf %>%
  ggplot(aes(x = Stress, y = StressAnomaly, fill=NativeNone)) +
  geom_violin(trim=FALSE, fill="gray") +
  geom_boxplot(width=0.05)+
  theme_classic()+
  theme(strip.text = element_text(size=16),strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=16, angle=45, vjust=1, hjust=1),axis.text.y = element_text(color="black", size=16), 
        text = element_text(colour="black",size=18))+
  scale_y_continuous(breaks=seq(0,30,5))+ 
  labs(y ="Intensity") + labs(x ="")+scale_fill_manual(name="", values = c("Native" = "black", "Non-native" = "red")) + 
  facet_wrap(.~NativeNone, strip.position = "bottom")
pt4


library(ggpubr)
ggarrange(plt1, plt2, 
          labels = c("a", "b"), widths = c(.6,1),
          nrow = 1)
library(cowplot)
plot_grid(pt1, pt2, pt3, nrow = 1, align="h",rel_widths = c(.8, 1,1), labels = c("a", "b","c"))


plot_grid(pt1, pt2,pt4, pt5, nrow = 2, ncol =2, align="hv",#rel_widths = c(1,1), 
          labels = c("a", "b","c","d"))

## ECOsystem  #### 
#, y = (..count..)/sum(..count..)*100)
levels(metadf$Ecosystem)
metadf$Ecosystem <- as.factor(metadf$Ecosystem)
levels(metadf$Ecosystem)<- c("Estuarine", "Marine") #change names
levels(metadf$Stress)
metadf$Stress <- as.factor(metadf$Stress)
levels(metadf$Stress) <- c("Warming", "Freshening")# changing name

pt5 <- ggplot(metadf, aes(fill=NativeNone, x=Ecosystem, y = (..count..)/sum(..count..)*100)) + 
  geom_bar(position="stack", stat="count")+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,70,10),limits=c(0,71)) +
  theme_classic()+
  theme(strip.text = element_text(size=16),strip.background = element_rect(fill="white", colour="white",size=1))+
  theme(axis.text.x = element_text(size=16,angle=45, vjust=1, hjust=1),axis.text.y = element_text(color="black", size=16), 
                        text = element_text(colour="black",size=18))+
  theme(legend.position=c(.7,.88), legend.title = element_blank(), legend.text = element_text(size=14, face="italic"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm"))+
  labs(x = "", y = "Species (%)")+scale_fill_manual(name="", values = c("Native" = "grey50", "Non-native" = "brown3"))+
  facet_wrap(.~Stress, strip.position = "bottom")
pt5

## world map of points ####
##      Review paper map
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")        
library(ggspatial)
#The package rnaturalearth provides a map of countries of the entire world. 
#Use ne_countries to pull country data and choose the scale (rnaturalearthhires is necessary for scale = "large"). 
#The function can return sp classes (default) or directly sf classes, as defined in the argument returnclass:
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
## world map of points 
map1 <- ggplot(data = world) + 
  geom_sf(color = "grey 50", fill = "darkseagreen1")+
  coord_sf(xlim = c(-180, 185), ylim = c(-70, 82), expand = FALSE)+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0), panel.background = element_rect(fill = "aliceblue"))+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=12))+ xlab("Longitude") + ylab("Latitude") 

map1 + geom_point(aes(Longitude,Latitude),color = "black", size = 3, position=position_jitter(width=2, height=2), data=metadf)
