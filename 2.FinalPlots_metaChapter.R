library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(cowplot)
library(tidyverse)
#############################################   code for bioresponse plots


####  reorder the place of the levels
tdata <- Tempmod
levels(tdata$Bioresponse)
tdata$Bioresponse <- as.factor(tdata$Bioresponse)
tdata$Bioresponse <- factor(tdata$Bioresponse,levels(tdata$Bioresponse)[c(9,5,8,7,6,3,2,4,1)])## reorder the levels for the temp data
levels(tdata$status)
tdata$status <- as.factor(tdata$status)
tdata$status <-  factor(tdata$status,levels(tdata$status)[c(2,1)])
levels(tdata$sig)
tdata$sig <- as.factor(tdata$sig)

sdata <- Salmod
levels(sdata$Bioresponse)
sdata$Bioresponse <- as.factor(sdata$Bioresponse)
sdata$Bioresponse <- factor(sdata$Bioresponse,levels(sdata$Bioresponse)[c(7,4,6,5,2,3,1)])## reorder for salinity
levels(sdata$status)
sdata$status <-as.factor(sdata$status)
levels(sdata$status)
levels(sdata$sig)
sdata$sig <- as.factor(sdata$sig)


levels(fullmod$Bioresponse)
fullmod$Bioresponse <- as.factor(fullmod$Bioresponse)
fullmod$Bioresponse <- factor(fullmod$Bioresponse,levels(fullmod$Bioresponse)[c(8,7,6,4,2,3,5,1)])## reorder the levels for the all data
levels(fullmod$significance)
fullmod$sig <- as.factor(fullmod$significance)
levels(fullmod$shaded)
fullmod$shaded <- as.factor(fullmod$shaded)
levels(fullmod$stress)
fullmod$stress <- as.factor(fullmod$stress)

#####   full plot fig.1
p <- ggplot(fullmod) + 
  geom_errorbar(aes(x=Bioresponse,ymin=lower, ymax=upper, width = 0.2, col=stress),size=1,  position = position_dodge(width = 0.5))+ ##, size=dat$type
  geom_point(aes(y = estimate, x = Bioresponse, colour=stress, fill=significance), pch = 21, size=4, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("Hedges' "*italic("d"))) +
  xlab("")+
  theme_coding()+
  ggtitle("")+
  scale_y_continuous(breaks=seq(-4,2.5,1), expand = c(0,0),limits=c(-4.3,2.5)) +
  theme(legend.position = "none")+
  scale_color_manual(values=c("red", "blue"))+ scale_size_continuous(guide=F)+
  scale_fill_manual(values=c("white", "red", "blue"))+
  coord_flip()
p
# adding in the little numbers for the sample sizes
q <- p + annotate("text", x = 10.1, y = -1, label = "33", colour = "black", size=4)+ annotate("text", x = 10.35, y = 0.2, label = "86", colour = "black", size=4) +
  annotate("text", x = 8.65, y = 0.35, label = "17", colour = "black", size=4) +annotate("text", x = 9.35, y = 0.9, label = "30", colour = "black", size=4) +
  annotate("text", x = 8.35, y = 0.1, label = "190", colour = "black", size=4) +annotate("text", x = 8.15, y = -2.3, label = "32", colour = "black", size=4)+
  annotate("text", x = 7.25, y = 0.3, label = "20", colour = "black", size=4) + annotate("text", x = 6.25, y = -0.4, label = "31", colour = "black", size=4)+
  annotate("text", x = 5.35, y = -1.1, label = "38", colour = "black", size=4) + annotate("text", x = 4.6, y = -0.95, label = "7", colour = "black", size=4)+
  annotate("text", x = 3.65, y = -0.85, label = "12", colour = "black", size=4) + annotate("text", x = 4.35, y = -0.5, label = "20", colour = "black", size=4)+
  annotate("text", x = 2.65, y = -3, label = "22", colour = "black", size=4) + annotate("text", x = 3.35, y = -0.8, label = "37", colour = "black", size=4)+
  annotate("text", x = 2.25, y = 0.1, label = "452", colour = "black", size=4) + annotate("text", x = 1.25, y = -1.5, label = "123", colour = "black", size=4)
q  
    #### adding a polygon
r <- q + annotate("rect", xmin = 0, xmax = 2.4, ymin = -Inf, ymax = Inf, 
           alpha = .1)

r
#### attempting to add polygons but above worked
q + geom_rect(
  aes(xmin = start, xmax = end, fill = fullmod$shaded), 
  ymin = -Inf, ymax = Inf, alpha = 0.2)
#xmin= and xmax=Inf
q + annotate("rect", xmin = 9.5, xmax = 10.5, ymin = -4.2, ymax = 1.5, 
             alpha = .1)
plot.new()
p + polygon(xx,yy, col="yellow")

xx = c(9.5, 9.5, 10, 10)
yy = c(-4, 2, 2, -4)

#### adding a plot inside a plot
r + annotation_custom(ggplotGrob(d), xmin = 3, xmax = 7.8, 
                       ymin = -4.2, ymax = -2.4)

r + annotation_custom(ggplotGrob(d), xmin = 0.1, xmax = 4.8, 
                      ymin = 0.2, ymax = 1.9)


##########                            below are the native and nonnative               axis.text.y = element_text(face = "bold"))+
##########                   temp as a single figure
p1 <- ggplot(tdata) + 
  geom_errorbar(aes(x=Bioresponse,ymin=lower, ymax=upper, width = 0.2, col=status), position = position_dodge(width = 0.3))+
  geom_point(aes(y = estimate, x = Bioresponse, colour=status, fill=significance),pch=21, size=2, position = position_dodge(width = 0.3)) + 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab("") +
  xlab("")+
  ggtitle("Temperature")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ 
  ylim(-5,3)+
  theme(legend.position=c(.1,.95),legend.text = element_text(size = 18), legend.title = element_blank(),legend.key = element_rect(fill="white", colour="white")) + 
  scale_color_manual(values=c("black", "red"))+ scale_size_continuous(guide=F)+
  scale_fill_manual(values=c("white", "black", "red"), guide = "none")+
    theme(plot.title = element_text(size=16, face = "bold"),axis.title = element_text(size=14, face = "bold"),
          axis.text = element_text(size=14), axis.text.y = element_text(face = c('bold', 'bold','plain','plain','plain','plain','plain','plain','plain'))) +
  coord_flip()
p1
f1 <- p1 + annotate("text", x = 8.8, y = 0.15, label = "66", colour = "black", size=3)+ annotate("text", x = 9.2, y = 0.45, label = "20", colour = "black", size=3)+
  annotate("text", x = 7.8, y = 1, label = "21", colour = "black", size=3) +annotate("text", x = 8.2, y = 1.05, label = "9", colour = "black", size=3)+
  annotate("text", x = 6.75, y = -0.3, label = "148", colour = "black", size=3) + annotate("text", x = 7.25, y = 1.05, label = "42", colour = "black", size=3)+
  annotate("text", x = 5.75, y = -0.3, label = "29", colour = "black", size=3) + annotate("text", x = 6.3, y = -0.1, label = "2", colour = "black", size=3)+
  annotate("text", x = 5.1, y = -1.9, label = "24", colour = "black", size=3) + annotate("text", x = 5.2, y = 0.3, label = "14", colour = "black", size=3)+
  annotate("text", x = 4.3, y = -2.6, label = "7", colour = "black", size=3) + annotate("text", x = 3.7, y = 0.5, label = "13", colour = "black", size=3)+
  annotate("text", x = 2.7, y = -1, label = "29", colour = "black", size=3) + annotate("text", x = 3.3, y = -0.1, label = "8", colour = "black", size=3)+
  annotate("text", x = 2.25, y = 0.5, label = "350", colour = "black", size=3) + annotate("text", x = 1.25, y = -0.2, label = "102", colour = "black", size=3)
f2 <- f1 +  annotate("rect", xmin = 0, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = .1)
f2

##################################################################################    Salinity fig native/nns
p2 <- ggplot(sdata) + 
  geom_errorbar(aes(x=Bioresponse,ymin=lower, ymax=upper, width = 0.2, col=status), position = position_dodge(width = 0.3))+
  geom_point(aes(y = estimate, x = Bioresponse, colour=status, fill=sig),pch=21, size=2, position = position_dodge(width = 0.3)) + 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("Hedges' "*italic("d"))) +
  xlab("")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+  
  theme(legend.position=c(.1,.95),legend.text = element_text(size = 18), legend.title = element_blank(), legend.key = element_rect(fill="white", colour="white")) + 
  ggtitle("Salinity")+
  ylim(-7,3)+
  scale_color_manual(values=c("black", "red"))+ scale_size_continuous(guide=F)+
  scale_fill_manual(values=c("white", "black", "red"), guide = "none")+
  theme(plot.title = element_text(size=16, face = "bold"),axis.title = element_text(size=16, face = "bold"),axis.text = element_text(size=14),
        axis.text.y = element_text(face = c('bold', 'bold','plain','plain','plain','plain','plain','plain','plain'))) +
  coord_flip()

f3 <- p2 + annotate("text", x = 7.05, y = -1.25, label = "25", colour = "black", size=3)+ annotate("text", x = 7.2, y = -.35, label = "8", colour = "black", size=3)+
  annotate("text", x = 5.8, y = 0.45, label = "13", colour = "black", size=3) + annotate("text", x = 6.2, y =  0.2, label = "4", colour = "black", size=3)+
  annotate("text", x = 4.8, y = -2.4, label = "19", colour = "black", size=3) + annotate("text", x = 5.2, y = -2.1, label = "13", colour = "black", size=3)+
  annotate("text", x = 3.8, y = -1.15, label = "5", colour = "black", size=3) + annotate("text", x = 4.2, y = -0.7, label = "2", colour = "black", size=3)+
  annotate("text", x = 2.8, y = -3.2, label = "9", colour = "black", size=3) + annotate("text", x = 3.2, y = -1.95, label = "3", colour = "black", size=3)+
  annotate("text", x = 2.1, y = -0.7, label = "42", colour = "black", size=3) + annotate("text", x = 1.1, y = -1.55, label = "81", colour = "black", size=3)


f4 <- f3 +  annotate("rect", xmin = 0, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = .1)
f4

#  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = .1)+
 # annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = .1)
#  theme(plot.title = element_text(size=16, face = "bold"),axis.title = element_text(size=16, face = "bold"),axis.text = element_text(size=14),
#axis.text.y = element_text(face = c('bold', 'bold','plain','plain','plain','plain','plain','plain','plain','plain'))) +  
#  coord_flip() 

#                          COMBINED FIGURES
##################################          below is code for the combined figures for temp/sal  nat/nns

  p1 <- ggplot(tdata) + 
  geom_errorbar(aes(x=Bioresponse,ymin=lower, ymax=upper, width = 0.2, col=status), size=1,position = position_dodge(width = 0.5))+
  geom_point(aes(y = estimate, x = Bioresponse, colour=status, fill=significance),pch=21, size=4, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab("") +
  xlab("")+
  #ggtitle("Temperature")+
  theme_coding()+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ 
  ylim(-7,4)+
  theme(legend.position=c(.11,.79),legend.text = element_text(size = 18), legend.title = element_blank(),legend.key = element_rect(fill="white", colour="white")) + 
  scale_color_manual(values=c("black", "red"))+ scale_size_continuous(guide=F)+
  scale_fill_manual(values=c("white", "black", "red"), guide = "none")+
  theme(plot.title = element_text(size=16, face = "bold"),axis.title = element_text(size=18, face = "bold"),axis.text = element_text(size=18),
        axis.text.y = element_text(face = c('bold', 'bold','plain','plain','plain','plain','plain','plain','plain'))) +
  coord_flip()
p1
f1 <- p1 + annotate("text", x = 8.5, y = 0.15, label = "66", colour = "black", size=4)+ annotate("text", x = 9.4, y = 0.5, label = "20", colour = "black", size=4)+
  annotate("text", x = 7.5, y = 0.8, label = "21", colour = "black", size=4) +annotate("text", x = 8.5, y = 1.05, label = "9", colour = "black", size=4)+
  annotate("text", x = 7.3, y = -0.35, label = "148", colour = "black", size=4) + annotate("text", x = 6.7, y = 1.2, label = "42", colour = "black", size=4)+
  annotate("text", x = 5.55, y = -0.5, label = "29", colour = "black", size=4) + annotate("text", x = 6.4, y = -0.3, label = "2", colour = "black", size=4)+
  annotate("text", x = 5.25, y = -1.9, label = "24", colour = "black", size=4) + annotate("text", x = 5.5, y = 0.25, label = "14", colour = "black", size=4)+
  annotate("text", x = 4.4, y = -2.65, label = "7", colour = "black", size=4) + annotate("text", x = 3.5, y = 0.5, label = "13", colour = "black", size=4)+
  annotate("text", x = 2.5, y = -1, label = "29", colour = "black", size=4) + annotate("text", x = 3.45, y = -0.2, label = "8", colour = "black", size=4)+
  annotate("text", x = 1.55, y = 0.55, label = "350", colour = "black", size=4) + annotate("text", x = 1.45, y = -0.25, label = "102", colour = "black", size=4)+
  annotate("text", x = 9.2, y = -6.5, label = "Temperature", colour = "black", size=7)

f2 <- f1 +  annotate("rect", xmin = 0, xmax = 2.4, ymin = -Inf, ymax = Inf, alpha = .1)
f2

#################################      salinity
  p2 <- ggplot(sdata) + 
  geom_errorbar(aes(x=Bioresponse,ymin=lower, ymax=upper, width = 0.2, col=status), size=1,position = position_dodge(width = 0.5))+
  geom_point(aes(y = estimate, x = Bioresponse, colour=status, fill=significance),pch=21, size=4, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope=0, colour = "darkgray", linetype="dashed") +
  ylab(expression("Hedges' "*italic("d"))) +
  xlab("")+
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+  
  #ggtitle("Salinity")+
  ylim(-7,4)+
  scale_color_manual(values=c("black", "red"))+ scale_size_continuous(guide=F)+
  scale_fill_manual(values=c("white", "black", "red"), guide = "none")+
  theme(plot.title = element_text(size=18, face = "bold"),axis.title = element_text(size=18, face = "bold"),axis.text = element_text(size=18),
        axis.text.y = element_text(face = c('bold', 'bold','plain','plain','plain','plain','plain','plain','plain'))) +
  coord_flip()
p2
f3 <- p2 + annotate("text", x = 7.2, y = -1.2, label = "25", colour = "black", size=4)+ annotate("text", x = 6.8, y = -.35, label = "8", colour = "black", size=4)+
  annotate("text", x = 5.5, y = 0.4, label = "13", colour = "black", size=4) + annotate("text", x = 6.4, y = 0.28, label = "4", colour = "black", size=4)+
  annotate("text", x = 4.5, y = -2.4, label = "19", colour = "black", size=4) + annotate("text", x = 5.5, y = -2.1, label = "13", colour = "black", size=4)+
  annotate("text", x = 3.5, y = -1.15, label = "5", colour = "black", size=4) + annotate("text", x = 4.5, y = -0.75, label = "2", colour = "black", size=4)+
  annotate("text", x = 2.5, y = -3.2, label = "9", colour = "black", size=4) + annotate("text", x = 3.45, y = -1.95, label = "3", colour = "black", size=4)+
  annotate("text", x = 2.3, y = -0.7, label = "42", colour = "black", size=4) + annotate("text", x = 1.3, y = -1.55, label = "81", colour = "black", size=4)+
  annotate("text", x = 7.25, y = -6.8, label = "Salinity", colour = "black", size=7)

f4 <- f3 +  annotate("rect", xmin = 0, xmax = 2.4, ymin = -Inf, ymax = Inf, alpha = .1)
f4

###arranging plots##
library(ggpubr)
ggarrange(p1, p2, heights = c(1.35,1.1), #heights= vector for colmn heights
          labels = c("a", "b"), 
          ncol = 1)


##################################### world map of points 
####### make sure to be on the right directory
###to change the points
#### or keep itin and change the name
metadf$Phylum2 <- as.factor(metadf$Phylum2)
levels(metadf$Phylum2)  # get structure
table(metadf$Phylum2)

metadf$Phylum2 = factor(metadf$Phylum2,levels(metadf$Phylum2)[c(7,8,4,2,5,3,1,6,9)])# rearrange

shapevector.Taxon <- c( "deeppink1", "blue","black", "yellow", "coral", "springgre", "green", "blueviolet", "violetred4")
                 
route <- geom_point(aes(Longitude,Latitude),color = shapevector.Taxon[metadf$Taxon], size = 1.25, data=metadf)

wrld <- readOGR(".","ne_110m_admin_0_countries")
base <- ggplot(wrld, aes(x = long, y = lat)) +  
  theme_classic()+ theme(axis.title = element_text(size=18),axis.text = element_text(size=18))+
  scale_x_continuous(expand = c(0,0),breaks=seq(-180,180,40),limits = c(-180, 185)) + ## change limits to zoom in or out
  scale_y_continuous(expand = c(0,0),breaks=seq(-90,90,20),limits = c(-70, 85)) + 
  labs(x ="\nLongitude")+ 
  labs(y=expression("Latitude"))

wrld <- c(geom_polygon(aes(group=group), size = 0.05, colour= "grey 50", fill="darkseagreen1", data=wrld, alpha=1)) 
route <- geom_point(aes(Longitude,Latitude),color = shapevector.Taxon[metadf$Phylum2], size = 3, position=position_jitter(width=2, height=2), data=metadf)
base +  wrld + route

### legend - plots seperatlry
plot.new()
legend("center", pch = 16,pt.cex =4.5,
       col = shapevector.Taxon,
       legend=c("Macrophyte (318)" ,   "Mollusca (110)"  ,"Chordata (79)","Arthropoda (23)", "Cnidaria (18)","Bryozoa (12)", "Annelida (10)" ,  
                "Echinodermata (3)",   "Porifera (2)"),
       cex=2, bty="n")

geom_









