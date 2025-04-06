#FinalPlotsReview
library(rgdal)
library(ggplot2)
library(png)
library(tidyr)
library(dplyr)
library(scales)
library(ggthemes)
library(maps)
library(mapproj)
library(rgdal)
library(cowplot)
library(gridExtra)
library(grid)
library(ggpubr)
# hjust = -0.5  adjust the horizontal pos of each label, more negative move label further to the right
##vjust = 1.5   adjust the vertical pos of each label, more positive move label further down
#### cumulative frequency papers
str(CumFreqyears)
#CumFreqyears$timeyear <- as.factor(CumFreqyears$timeyear) do not change to facotr


##  ------   SEE PLOTS on script review_manip_language.R ---------------------------------

yrplt <- ggplot() + geom_line(aes(x=CumFreqyears$timeyear, y=CumFreqyears$Temperature, group=1), alpha = 0.3,size =1, colour="darkred") + ### group=1 means the points connect
  geom_ribbon(aes(x=CumFreqyears$timeyear, ymax=CumFreqyears$t.upp, ymin=CumFreqyears$t.lower), alpha = 0.6,fill="darkred")+
  geom_line(aes(x=CumFreqyears$timeyear, y=CumFreqyears$Salinity, group=1), size =1, color="steelblue")+
  geom_ribbon(aes(x=CumFreqyears$timeyear, ymax=CumFreqyears$s.upp, ymin=CumFreqyears$s.low), fill="steelblue")+
  theme_coding()+
  labs(x = "", y="Cumulative frequency of papers\n") +
  scale_y_continuous(expand = c(0,0),breaks=seq(0,128,20),limits = c(0, 128))+ 
  scale_x_continuous(expand = c(0,0),breaks=seq(2001,2019,2), limits = c(2003,2019))+ 
  theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),text = element_text(colour="black",size=14))

fig2 <- yrplt + annotation_custom(ggplotGrob(mapPlt), xmin = 2003, xmax = 2016, 
                            ymin = 30, ymax = 150)
fig2
####       CHECK THIS WEBSITE   https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# or
#pivot
cum_long <- CumFreqyears %>% 
  gather(key = "Stress", value = "paps", Temperature, Salinity)
#plot
ggplot(cum_long,aes(x=timeyear, y=paps, color=Stress,fill=Stress))+
  geom_line()
##################################### world map of points 
####### make sure to be on the right directory
###to change the points
#### or keep itin and change the name
table(Alldata$Phylum2)
taxadata <- Alldata[-which(Alldata$Phylum2 == "Comm"),] #### removing community points
table(taxadata$Phylum2)
taxadata$Phylum2 <- as.factor(taxadata$Phylum2)
levels(taxadata$Phylum2)  # get structure
taxadata$Phylum2 = factor(taxadata$Phylum2,levels(taxadata$Phylum2)[c(7,8,4,2,5,3,1,9,6)])# rearrange

####  new mapping with tidyverse ####
world <- map_data("world")
str(taxadata)
levels(taxadata$Phylum2)
levels(taxadata$Phylum2) <- c("Macrophyte (347)" ,   "Mollusca (158)"  ,"Chordata (85)","Arthropoda (30)", "Cnidaria (20)","Bryozoa (12)", "Annelida (10)" ,  
                              "Porifera (8)", "Echinodermata (3)")# changing name
levels(taxadata$Phylum2) <- c("Macrophyte" ,   "Mollusca"  ,"Chordata ","Arthropoda ", "Cnidaria ","Bryozoa ", "Annelida " ,  
                              "Porifera ", "Echinodermata ")# changing name

mapPlt <- ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),color = "grey", fill = "grey", size = 0.1) +
  coord_sf(ylim = c(-65.2, 88), expand = FALSE)+
  theme_void()+
  geom_jitter(data = taxadata,aes(Longitude, Latitude),size = 1) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm"))
  #theme(legend.position=c(0.15, 0.35))+
  # scale_color_manual(values = c("burlywood", "blue","black", "yellow", "coral", "darkorange", 
  #                               "green", "deeppink1", "violetred4"))+
  #theme(legend.title = element_blank(),legend.text = element_text(size=12))
  # #annotation_scale(location = "tr",bar_cols = c("grey60", "white"), text_family = "mono")+
  # #annotation_north_arrow(location = "tr", which_north = "true",
  #                        pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
  #                        style = north_arrow_nautical(fill = c("grey40", "white"),line_col = "grey20"))
mapPlt

### bar plot for number of stressors variables papers
levels(Studies$Stress2)
Studies$Stress2 <- as.factor(Studies$Stress2)
table(Studies$Stress2)
levels(Studies$Stress2) <- c("Freshening &other", "Temperature", "Temp &other",
                             "Temp &pH", "Temp &pH &other",
                             "Temp &Fresh &other")
Studies$Stress2 = factor(Studies$Stress2,levels(Studies$Stress2)[c(2,3,4,5,1,6)])
table(Studies$Stress2)
levels(Studies$Stress2) <- c("Temperature","Temp & other" ,"Temp & pH","Temp & pH\n & other"  ,  "Freshening\n & other" , "Temp & Fresh\n & other")
#d <-position_dodge(width=0.2)

pt1 <-ggplot(Studies, aes(x=factor(Stress2))) + 
  geom_bar(stat="count",fill = "grey 50")+  
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  theme(axis.text.x = element_text(angle= 45,hjust=1, vjust=1,color="grey50", size=18, face="bold"),
        axis.text.y = element_text(color="grey50", size=18, face="bold"))+
  scale_y_continuous(expand = c(0,0), breaks=seq(0,60,20), limits=c(0,65))+
  theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
pt1
#position=d
#angle = -12,
##  PLOT for number of species measured per study
levels(Studies$Species)
table(Studies$Species)
Studies$Species <- as.factor(Studies$Species)
levels(Studies$Species) <- c("1 Species", "2 Species", "3 Species", "4 Species", "5+ Species")

pt2 <-ggplot(Studies, aes(x=factor(Species))) + 
  geom_bar(stat="count",fill="grey 50") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,60,20), limits=c(0,65))+ 
  theme(axis.text.x = element_text(color="grey50", size=18, angle = 45,hjust=1, vjust=1,face="bold"),
        axis.text.y = element_text(color="grey50", size=18,face="bold"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
pt2
###            Bioresponses
## i made a data set from the data in excel
str(Res)
levels(Res$Response)
table(Res$Response)
Res$Response <- as.factor(Res$Response)
Res$Response = factor(Res$Response,levels(Res$Response)[c(2,7,4,3,1,6,5,8)])
levels(Res$Response) <- c("Biomolecular", "Survival" , "Physiology", "Community", "Behaviour & feeding ",
                         "Reproduction" ,"Range shifts","Symbiont" )

pt3 <-ggplot(Res, aes(x= Response,  y=pap)) + geom_bar(fill="grey50", stat="identity") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,110,20), limit=c(0,110))+ 
  theme(axis.text.x = element_text(color="grey50", size=16, angle =45, hjust=1, vjust=1,face="bold"),
        axis.text.y = element_text(color="grey50", size=18,face="bold")) +theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
pt3

##############   Lab and field
table(Studies$Experimenttype2)
Studies$Experimenttype2 <- as.factor(Studies$Experimenttype2)
levels(Studies$Experimenttype2)
Studies$Experimenttype2 = factor(Studies$Experimenttype2,levels(Studies$Experimenttype2)[c(2,5,3,4,1)])# rearrange
levels(Studies$Experimenttype2) <- c("Laboratory","Lab & modelling","Lab & field","Lab with field\n observations", "Field")

pt4 <-ggplot(Studies, aes(x=factor(Experimenttype2))) + 
  geom_bar(stat="count",fill="grey 50") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,110,20), limits=c(0,110))+ 
  theme(axis.text.x = element_text(color="grey50", size=18, angle = 45,hjust=1, vjust=1, face="bold"),
        axis.text.y = element_text(color="grey50", size=18,face="bold"))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
pt4
# arranging plots ####

p1 <- plot_grid(pt1, pt2, pt3, pt4,
                labels = c("(a)", "(b)", "(c)", "(d)"), 
                align="hv", hjust=-0.5, vjust = 1 ) ### aigns in both direction
p2 <- grid.arrange(
  arrangeGrob(p1, 
              bottom=grid::textGrob(label = expression(""), gp= gpar(fontsize=20, fontface="bold", col="black")),
              left=grid::textGrob(label = expression("                      Number of Papers"), rot=90, gp= gpar(fontsize=20, fontface="bold", col="black")))) 
p2

##############   SCENARIOS
table(Studies$Scenario)
Studies$Scenario <- as.factor(Studies$Scenario)
levels(Studies$Scenario)
Studies$Scenario = factor(Studies$Scenario,levels(Studies$Scenario)[c(2,5,1,6,4,3,7)])# rearrange
levels(Studies$Scenario)

pt1 <- ggplot(Studies, aes(x=factor(Scenario))) + 
  geom_bar(stat="count",fill = "grey 50") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  theme(axis.text.x = element_text(color="grey50",size=16, angle =45, hjust=1,vjust=1,face="bold"),
        axis.text.y = element_text(color="grey50", size=18,face="bold"))+
  scale_y_continuous(expand = c(0,0), breaks=seq(0,90,20), limits=c(0,95)) +theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
pt1

###### pre exposure adjustment
levels(Studies$Intensity)
Studies$Intensity <- as.factor(Studies$Intensity)
table(Studies$Intensity)
Studies$Intensity = factor(Studies$Intensity,levels(Studies$Intensity)[c(5,1,2,4,6,3)])# rearrange

  
pt2 <- ggplot(Studies, aes(x=factor(Intensity))) + 
  geom_bar(stat="count",fill = "grey 50") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  theme(axis.text.x = element_text(color="grey50",size=16, angle =45, hjust=1,vjust=1,face="bold"),
        axis.text.y = element_text(color="grey50", size=18,face="bold"))+
  scale_y_continuous(expand = c(0,0), breaks=seq(0,90,20), limits=c(0,95)) +theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
pt2

#############3     type of exposure    TYPE
levels(Studies$`Type (constant, gradual change, ramping, flucuating)`)
Studies$`Type (constant, gradual change, ramping, flucuating)` <- as.factor(Studies$`Type (constant, gradual change, ramping, flucuating)`)
table(Studies$`Type (constant, gradual change, ramping, flucuating)`)
levels(Studies$`Type (constant, gradual change, ramping, flucuating)`) <- c("Constant" ,  "Diurnal", "Seasonal"  ,  "Ramping"   , "Unknown"  )


pt3 <- ggplot(Studies, aes(x=factor(`Type (constant, gradual change, ramping, flucuating)`))) + 
  geom_bar(stat="count",fill = "grey 50") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  theme(axis.text.x = element_text(color="grey50",size=16, angle =45, hjust=1,vjust=1,face="bold"),
        axis.text.y = element_text(color="grey50", size=18,face="bold"))+
  scale_y_continuous(expand = c(0,0), breaks=seq(0,90,20), limits=c(0,95)) +theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
pt3

######################     PSEUDO REPLICATION
table(Studies$`Pseudo-rep`)
levels(Studies$`Pseudo-rep`)
Studies$`Pseudo-rep` <- as.factor(Studies$`Pseudo-rep`)
Studies$`Pseudo-rep` = factor(Studies$`Pseudo-rep`, levels(Studies$`Pseudo-rep`)[c(1,3,2)])

pt4 <- ggplot(Studies, aes(x=factor(`Pseudo-rep`))) + 
  geom_bar(stat="count",fill = "grey 50") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  theme(axis.text.x = element_text(color="grey50",size=16, angle =45, hjust=1,vjust=1,face="bold"),
        axis.text.y = element_text(color="grey50", size=18,face="bold"))+
  scale_y_continuous(expand = c(0,0), breaks=seq(0,100,20), limits=c(0,95)) +theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
pt4

############     COMBINING THER GRAPHS INTO ONE

p1 <- plot_grid(pt1, pt2, pt3, pt4,
                labels = c("(a)", "(b)", "(c)", "(d)"), 
                align="hv", hjust=-0.5, vjust = 1 ) ### aigns in both direction
p2 <- grid.arrange(
  arrangeGrob(p1, 
              bottom=grid::textGrob(label = expression(""), gp= gpar(fontsize=20, fontface="bold", col="black")),
              left=grid::textGrob(label = expression("                     Number of Papers"), rot=90, gp= gpar(fontsize=20, fontface="bold", col="black")))) 
p2




##########################                 magnitude of experimental stress
table(Alldata$Stress)
Alldata$Stress <- as.factor(Alldata$Stress)
levels(Alldata$Stress) <- c("Salinity", "Temperature")
Alldata$Stress = factor(Alldata$Stress,levels(Alldata$Stress)[c(2,1)]) ###changing the order

p1 <- Alldata %>%
  ggplot(aes(x = Stress, y = StressAnomaly, fill=Stress)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.05,alpha=0.8, fill = "grey80")+ 
  theme_coding()+theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(0,30,5)) + scale_fill_manual(values=c("brown3", "lightblue"))+
  labs(y ="Intensity of treatments (oC / PSU)\n") + labs(x ="")

##########################                 Durantion of experimental stress
table(Alldata$Stress)
Alldata$Stress <- as.factor(Alldata$Stress)
levels(Alldata$Stress) <- c("Salinity", "Temperature")
Alldata$Stress = factor(Alldata$Stress,levels(Alldata$Stress)[c(2,1)]) ###changing the order    

short  <- Alldata %>% filter(Exptime <= 300 )  #removing outliers

p2 <- short %>%
  ggplot(aes(x = Stress, y = Exptime, fill=Stress)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.05,alpha=0.8, fill = "grey80")+
  theme_coding()+theme(legend.position = "none")+ theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.5), units = , "cm"))+
  scale_y_continuous(breaks=seq(0,900,50))+ scale_fill_manual(values=c("brown3", "lightblue"))+
  labs(y ="Duration of treatments (d)\n") + labs(x ="")

#########   combining 
library(ggpubr)
ggarrange(p1,p2, 
          labels = c("a", "b"),  nrow=1, widths = c(1,1.2))
#################   magnitude os stress bars   first is bars, then a density plot, then violin plots

levels(alldata$Stress)
alldata$Stress <- as.factor(alldata$Stress)
levels(alldata$Stress)<- c("Salinity", "Temperature")
alldata$Stress = factor(alldata$Stress,levels(alldata$Stress)[c(2,1)])
barlines <- "black" ### add balines so there is more definotion 
######################now plot together in an arrangment!!!!!!!!!
ggplot(Alldata, aes(x= StressAnomaly)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)*100), binwidth=1, alpha=0.5, colour = barlines, position="identity") +
  theme_classic() +
  theme(text = element_text(size=16)) + theme(legend.position = "none") +
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  scale_y_continuous(expand = c(0,0), breaks=seq(0,15,5))+ 
  labs(x = "Magnitude of temperature &\n salinity manipulation (degrees/PSU)\n", y=expression("\nFrequency of experiments "*italic("%"))) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0,30,2)) + 
  coord_flip()  +facet_grid(.~Stress) 

#theme(panel.background = element_rect(fill='white', colour='black'))####adds a closed box around the graph
#scales = "free_x"    would alter the scales    breaks=seq(0,15,5)   breaks=seq(0,30,5) 

ggplot(alldata, aes(x= StressAnomaly)) +
  geom_density(aes(fill=factor(Stress), y =..count..), alpha=0.5) +
  theme_classic() +
  theme(text = element_text(size=16)) + theme(legend.position = "none") +
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  scale_y_continuous(expand = c(0,0))+ 
  labs(x = "Magnitude of temperature &\n salinity manipulation (degrees/PSU)\n", y=expression("\nNumber of experiments "*italic("%"))) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0,30,2)) + 
  coord_flip() 

ggplot(alldata, aes(x= Stress, y=StressAnomaly)) +
  geom_violin(trim=FALSE, fill="gray") +
  geom_boxplot(width=0.05)+
  theme_classic() +
  theme(text = element_text(size=16))+
  theme(axis.text.x = element_text(color="black",size=14),axis.text.y = element_text(color="black", size=14))+
  scale_y_continuous(breaks=seq(0,30,5))+ 
  labs(y ="Intensity") + labs(x ="Stress")

############################################ bias plots
##### plots investigating charcteristicas of the experimental studies
library(tidyverse)

########################### bar plot for number of stressors variables papers
levels(Studies$Stress2)
Studies$Stress2 <- as.factor(Studies$Stress2)
table(Studies$Stress2)
levels(Studies$Stress2) <- c("Freshening (&other)", "Temperature", "Temp (&other)",
                             "Temp & pH", "Temp &pH &other",
                             "Temp &Fresh (&other)")
Studies$Stress2 = factor(Studies$Stress2,levels(Studies$Stress2)[c(2,3,4,5,1,6)])
d <-position_dodge(width=0.2)
ggplot(Studies, aes(x=factor(Stress2))) + 
  geom_bar(stat="count",fill = "grey 50")+  
  theme_classic(base_size = 20) +
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = -10,color="black", size=20),
        axis.text.y = element_text(color="black", size=20))+
  labs(x="Climate variables", y="Number of papers\n") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,60,5), limits=c(0,60))+
  theme(axis.title.x = element_blank())
#position=d
#angle = -12,
##########################  PLOT for number of species measured per study
levels(Studies$Species)
Studies$Species <- as.factor(Studies$Species)
levels(Studies$Species) <- c("1 Species", "2 Species", "3 Species", "4 Species", "5+ Species")
ggplot(Studies, aes(x=factor(Species))) + 
  geom_bar(stat="count",fill="grey 50") +
  theme_classic(base_size = 20) + 
  theme(text = element_text(size=20))+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,60,5), limits=c(0,60))+ 
  theme(axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(color="black", size=12))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

########  plot for species and interations
levels(Studies$`Spp interactions`)
Studies$`Spp interactions` <- as.factor(Studies$`Spp interactions`)
Studies$`Spp interactions` = factor(Studies$`Spp interactions`,levels(Studies$`Spp interactions`)[c(2,3,1)])
ggplot(Studies, aes(x=Species, fill = `Spp interactions`)) + 
  geom_bar() +
  theme_classic(base_size = 20) + 
  theme(text = element_text(size=20))+
  scale_fill_manual(name="Experimental\n organisation", values = c("Individual" = "grey 50", "Interacting" = "grey 25", "Community" = "honeydew3"))+
  theme(legend.position=c(.65, .7),legend.text = element_text(colour="black", size = 20)) + 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,60,5), limits=c(0,60))+ 
  theme(axis.text.x = element_text(angle = -10,color="black", size=20),
        axis.text.y = element_text(color="black", size=20))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

plt2 <- ggplot(Studies, aes(x=factor(Species))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "grey 50")+
  theme_classic(base_size = 14) + 
  theme(text = element_text(size=16))+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,40,5), limits=c(0,40))+ 
  theme(axis.text.x = element_text(color="black", angle = -12, size=15),
        axis.text.y = element_text(color="black", size=15))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

###################             Discipline of the research papers
##3#####################i made a data set from the data in excel
str(Disc)
levels(Disc$Discipline)
Res$Response <- as.factor(Res$Response)
Res$Response = factor(Res$Response,levels(Res$Response)[c(2,7,4,3,1,6,5,8)])
ggplot(Res, aes(x= Response,  y=pap)) + geom_bar(fill="grey 50", stat="identity") +
  theme_classic(base_size = 20) +
  theme(text = element_text(size=20))+
  labs(x = "Responses measured", y="Number of papers\n") +
  scale_y_continuous(expand = c(0,0),breaks=seq(0,80,10), limit=c(0,80))+ 
  theme(axis.text.x = element_text(angle = -10, color="black", size=20),
        axis.text.y = element_text(color="black", size=18)) +theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())

###################             Bioresponses / discipline
##3#####################i made a data set from the data in excel
str(design)
levels(design$expdesign)
table(design$expdesign)
design$expdesign <- as.factor(design$expdesign)
design$expdesign = factor(design$expdesign,levels(design$expdesign)[c(4,2,1,3)])
ggplot(design, aes(x= expdesign,  y=pap)) + geom_bar(fill="grey 50", stat="identity") +
  theme_classic(base_size = 20) +
  theme(text = element_text(size=20))+
  labs(x = "Responses measured", y="Number of papers\n") +
  scale_y_continuous(expand = c(0,0),breaks=seq(0,70,10), limit=c(0,70))+ 
  theme(axis.text.x = element_text(color="black", size=20),
        axis.text.y = element_text(color="black", size=18)) +theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())

###### pre exposure adjustment
levels(Studies$Intensity)
Studies$Intensity <- as.factor(Studies$Intensity)
table(Studies$Intensity)
Studies$Intensity = factor(Studies$Intensity,levels(Studies$Intensity)[c(5,1,2,4,3,6)])

ggplot(Studies, aes(x=factor(Intensity))) + 
  geom_bar(stat="count",fill = "grey 50")  +
  theme_classic() +
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(color="black",size=20),
        axis.text.y = element_text(color="black", size=20))+
  labs(x="", y="Number of papers\n") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,90,10), limit=c(0,92)) +theme(axis.title.x = element_blank())

###### pre exposure adjustment
levels(Studies$`Type (constant, gradual change, ramping, flucuating)`)
Studies$`Type (constant, gradual change, ramping, flucuating)` <- as.factor(Studies$`Type (constant, gradual change, ramping, flucuating)`)
table(Studies$`Type (constant, gradual change, ramping, flucuating)`)
#Studies$`Type (constant, gradual change, ramping, flucuating)` = factor(Studies$`Type (constant, gradual change, ramping, flucuating)`,
                                                                        #levels(Studies$`Type (constant, gradual change, ramping, flucuating)`)[c(5,1,2,6,4,3)])

ggplot(Studies, aes(x=factor(`Type (constant, gradual change, ramping, flucuating)`))) + 
  geom_bar(stat="count",fill = "grey 50") +
  theme_classic() +
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(color="black",size=20),
        axis.text.y = element_text(color="black", size=20))+
  labs(x="", y="") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,90,10), limits=c(0,90)) +theme(axis.title.x = element_blank())


###### stated scenarios
levels(Studies$Scenario)
Studies$Scenario <- as.factor(Studies$Scenario)
table(Studies$Scenario)
Studies$Scenario = factor(Studies$Scenario,levels(Studies$Scenario)[c(2,7,5,1,4,8,3,6)])
ggplot(Studies, aes(x=factor(Scenario))) + 
  geom_bar(stat="count",fill = "grey 50") +
  theme_classic() +
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle= -10,color="black",size=20),
        axis.text.y = element_text(color="black", size=20))+
  labs(x="", y="") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,70,10), limits=c(0,70)) +theme(axis.title.x = element_blank())

##### number of experiments that cover intensities   
#### i dont think you need this as you can just mention the number of papers without incremental rises
rampdata <- Alldata[-which(Alldata$IntTime == "Month"),] 

ggplot(rampdata, aes(x=factor(IntTime))) + 
  geom_bar(stat="count",fill = "grey 50")  +
  theme_classic() +
  theme(text = element_text(size=16))+
  theme(axis.text.x = element_text(color="black",size=14),
        axis.text.y = element_text(color="black", size=14))+
  labs(x="\nExperiments using gradual increments of applying stress", y="Number of experiments/n") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,400,50))  


##### world map of points 
###to change the points
taxadf <- alldata[-which(alldata$Phylum2 == "Comm"),] #### removing community points
#### or keep itin and change the name
levels(alldata$Phylum2) <- c("Annelida" , "Arthropoda","Bryozoa",  "Chordata",   "Cnidaria" , 
                     "Community", "Echinodermata", "Macrophyte", "Mollusca" ,"Porifera")

alldata$Phylum2 = factor(alldata$Phylum2,levels(alldata$Phylum2)[c(8,9,4,2,5,6,3,1,10,7)])
alldata$Phylum2 <- as.factor(alldata$Phylum2)
table(alldata$Phylum2)

shapevector.Taxon <- c( "blueviolet", "deeppink4","black", "yellow", "coral", "orange", "green", "darkorchid", "violetred4", 
                        "pink")
route <- geom_point(aes(Longitude,Latitude),color = shapevector.Taxon[alldata$Phylum2], size = 1.25, data=alldata)

wrld <- readOGR(".","ne_110m_admin_0_countries")
base <- ggplot(wrld, aes(x = long, y = lat)) +  
  theme_classic()+
  scale_x_continuous(expand = c(0,0),breaks=seq(-180,180,40),limits = c(-180, 180)) +
  scale_y_continuous(expand = c(0,0),breaks=seq(-90,90,20),limits = c(-90, 90)) + 
  labs(x ="\nLongitude")+ 
  labs(y=expression("Latitude"))
wrld <- c(geom_polygon(aes(group=group), size = 0.05, colour= "grey 50", fill="light green", data=wrld, alpha=1))
route <- geom_point(aes(Longitude,Latitude),color = "black", size = 1.25, data=alldata)
base +  wrld + route

### legend - plots seperatlry
plot.new()
legend("center", pch = 16,
       col = shapevector.Taxon,
       legend=c("Macrophyte (347)" ,   "Mollusca (158)"  ,"Chordata (85)","Arthropoda (30)", "Cnidaria (20)", "Community (18)", "Bryozoa (12)", "Annelida (10)" ,  
                "Porifera (8)",   "Echinodermata (3)"),
       cex=1, bty="n")
####### map based on meta data seperated by native and non
table(MA$NativeNone)
### change the order
MA$NativeNone = factor(MA$NativeNone,levels(MA$NativeNone)[c(2,1)])
shapevector.natnon <- c( "red", "black")
wrld <- readOGR(".","ne_110m_admin_0_countries")
base <- ggplot(wrld, aes(x = long, y = lat)) +  
  theme_classic()+
  scale_x_continuous(expand = c(0,0),breaks=seq(-180,180,40),limits = c(-180, 180)) +
  scale_y_continuous(expand = c(0,0),breaks=seq(-90,90,20),limits = c(-90, 90)) + 
  labs(x ="\nLongitude")+ 
  labs(y=expression("Latitude"))
wrld <- c(geom_polygon(aes(group=group), size = 0.05, colour= "grey 50", fill="light green", data=wrld, alpha=1))
route <- geom_point(aes(Longitude,Latitude),color = shapevector.natnon[MA$NativeNone], size = 1.25, data=MA)
base +  wrld + route

### legend - plots seperatlry
plot.new()
legend("center", pch = 16,
       col = shapevector.natnon,
       legend=c("Non-native  (146)" ,   "Native         (429)"),
       cex=2, bty="n")


###type of exp
ggplot(Alldata)+ 
  geom_point(mapping = aes(x = Exptime, y = StressAnomaly, colour = factor(`Type of ex`)))+
  theme_classic()+
  theme(panel.background = element_rect(fill='white', colour='black'), text = element_text(size=16)) +
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  theme(legend.position=c(.9, .7), legend.text = element_text(colour="black", size = 16)) +  ##legend
  scale_x_continuous(breaks=seq(0,500,20))+ 
  scale_y_continuous(breaks=seq(0,30,2))+
  scale_color_manual(name="Type\nof experiment", values = c("Field" = "green", "Lab" = "blue")) + 
  labs(x ="Experimental exposure (days)")+ 
  labs(y=expression("Degree climatic stress (PSU/oC)"))

##########################  PLOT for number of species measured per study
levels(Studies$Species)
Studies$Species <- as.factor(Studies$Species)
levels(Studies$Species) <- c("1 Species", "2 Species", "3 Species", "4 Species", "5 Species", "6+ Species")

ggplot(Studies, aes(x=factor(Species))) + 
  geom_bar(stat="count",fill="grey 50") +
  theme_classic(base_size = 14) + 
  theme(text = element_text(size=16))+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,40,5))+ 
  theme(axis.text.x = element_text(color="black", angle = -12, size=15),
        axis.text.y = element_text(color="black", size=15))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

###################             Bioresponses 
##3#####################i made a data set from the data in excel
str(biores)
levels(biores$Bioresponse)
biores$Bioresponse <- as.factor(biores$Bioresponse)

ggplot(biores, aes(x= Bioresponse,  y=papers)) + geom_bar(fill="grey 50", stat="identity") +
  theme_classic(base_size = 14) +
  theme(text = element_text(size=16))+
  labs(x = "Responses measured", y="Percentage of papers") +
  scale_y_continuous(expand = c(0,0),breaks=seq(0,80,5))+ 
  theme(axis.text.x = element_text(angle = -20, color="black", size=15),
        axis.text.y = element_text(color="black", size=15)) +theme(axis.title.x = element_blank())

##### number of experiments that cover intensities
#### i dont think you need this as you can just mention the number of papers without incremental rises
rampdata <- Alldata[-which(Alldata$IntTime == "Month"),] 

ggplot(rampdata, aes(x=factor(IntTime))) + 
  geom_bar(stat="count",fill = "grey 50")  +
  theme_classic() +
  theme(text = element_text(size=16))+
  theme(axis.text.x = element_text(color="black",size=14),
        axis.text.y = element_text(color="black", size=14))+
  labs(x="\nExperiments using gradual increments of applying stress", y="Number of experiments/n") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,400,50))  


##### world map of points 
wrld <- readOGR(".","ne_110m_admin_0_countries")
base <- ggplot(wrld, aes(x = long, y = lat)) +  
  theme_classic()+
  scale_x_continuous(expand = c(0,0),limits = c(-180, 180)) +
  scale_y_continuous(expand = c(0,0),limits = c(-90, 90)) + 
  labs(x ="Longitude")+ 
  labs(y=expression("Latitude"))
wrld <- c(geom_polygon(aes(group=group), size = 0.05, colour= "grey 50", fill="light green", data=wrld, alpha=1))
route <- geom_point(aes(Longitude,Latitude),color = "black", size = 1.25, data=Alldata)
base +  wrld + route


###type of exp
ggplot(tempdf)+ 
  geom_point(mapping = aes(x = Exptime, y = StressAnomaly, colour = factor(Type.of.ex)))+
  theme_classic()+
  theme(panel.background = element_rect(fill='white', colour='black'), text = element_text(size=16)) +
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  theme(legend.position=c(.9, .7), legend.text = element_text(colour="black", size = 16)) +  ##legend
  scale_x_continuous(breaks=seq(0,500,20))+ 
  scale_y_continuous(breaks=seq(0,30,2))+
  scale_color_manual(name="Type\nof experiment", values = c("Field" = "green", "Lab" = "blue")) + 
  labs(x ="Experimental exposure (days)")+ 
  labs(y=expression("Degree climatic stress (PSU/oC)"))

library(tidyverse)


#####################  taxa plots bar plot ####
taxa <- alldata[-which(alldata$Phylum2 == "Comm"),] #### removing community points
plt3 <- ggplot (taxa, aes(x=factor(Phylum2))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "grey 50")  + 
  theme_classic()+
  theme(text = element_text(size=16))+
  theme(axis.text.x = element_text(color="black", angle = -15,size=14),
        axis.text.y = element_text(color="black", size=14))+
  labs(x="\nTaxa", y="Percentage of Experiments\n")+ 
  scale_y_continuous(expand = c(0,0),breaks=seq(0,60,5), limits = c(0,60))+
  theme(axis.title.x = element_blank())


##################### bar plot for number of native none native papers
levels(Studies$`native/nns`)
Studies$`native/nns` <- as.factor(Studies$`native/nns`)
Studies$natnon = factor(Studies$`native/nns`,levels(Studies$`native/nns`)[c(2,3,1)])

ggplot(Studies, aes(x=factor(natnon))) + 
  geom_bar(stat="count",fill = "grey 50")  +
  theme_classic() +
  theme(text = element_text(size=16))+
  theme(axis.text.x = element_text(color="black",size=14),
        axis.text.y = element_text(color="black", size=14))+
  labs(x="\nNative or Non-native", y="Percentage of papers\n") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,90,10))  

#scale_x_discrete(limits=c("Both", "Native", "Non-native"))

#### Stress anomaly & duration scatterplot
##experimental duration against Stress anomlay 
# first remove tempsal
## check if its a factor
levels(stressdf$Stress) ###  single stress data frame
stressdf$Stress <- as.factor(stressdf$Stress)
levels(stressdf$Stress)<- c("Salinity", "Temperature")

stressdf$proano= log(stressdf$StressAnomaly)
stressdf$proano= (stressdf$StressMax/stressdf$Ambient)

ggplot(stressdf)+ 
  geom_point(mapping = aes(x = Exptime, y = proano, colour = factor(Stress)))+
  theme_classic()+
  theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  theme(legend.position=c(.9, .7), legend.text = element_text(colour="black", size = 16)) +  ##legend
  scale_x_continuous(breaks=seq(0,500,20))+ 
  scale_y_continuous()+
  scale_color_manual(name="Type\nof Stress", values = c("Salinity" = "Blue", "Temperature" = "red")) + 
  labs(x ="\nExperimental exposure (days)")+ 
  labs(y="Degree climatic stress (PSU/oC)\n")



##Intensity duration against intensity degrees
#to remove factors in a variabless see seperating data
levels(Alldata$IntTime)
Alldata$IntTime <- as.factor(Alldata$IntTime)
ram <- Alldata[-which(Alldata$IntTime == "None"),] 
rampdata <- ram[-which(ram$IntTime == "Month"),] 
rampdata$inttim = factor(rampdata$IntTime,levels(rampdata$IntTime)[c(3,2,1,7,6)]) ###changing the order

ggplot(rampdata)+ 
  theme_classic()+
  geom_point(mapping = aes(x = IntensityDurationd, y = Intensitydegrees, colour = factor(inttim))) +
  theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  theme(legend.position=c(.8, .8), legend.text = element_text(colour="black", size = 16)) + 
  scale_color_manual(name="Stress risen per", 
                     values = c("Minute"= "Green", "Hour" = "Black", "Day" = "Red", "Week" = "Pink" , "Salinity" = "Blue")) + 
  scale_x_continuous(breaks=seq(0,20,2))+ 
  scale_y_continuous(breaks=seq(0,6,1))+
  labs(x ="\nDuration of stress increase (days)")+ 
  labs(y="Stress increase\n")


##PLOT for SAL stress anomly metaregression
levels(saldf$NativeNone)
saldf$NativeNone <- as.factor(saldf$NativeNone)
levels(saldf$NativeNone)<- c("Native", "Non-native") #change names

ggplot(saldf)+ 
  geom_point(mapping = aes(x = StressAnomaly, y = yi, colour = factor(NativeNone))) + 
  theme_classic()+
  theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  theme(legend.position=c(.8, .9),legend.text = element_text(colour="black", size = 16)) + 
  scale_color_manual(name="Seperated by", values = c("Native" = "black", "Non-native" = "red")) + 
  scale_x_continuous(breaks=seq(0,30,2)) +
  geom_abline(intercept = -1.16, slope = 0.08, colour = "red") + 
  geom_abline(intercept = 2.87, slope = -0.23, colour = "black") + 
  labs(y=expression("Hedges' "*italic("d")))+ labs(x ="\nDegree of salinity stress (PSU)")

# Calculate slope and intercept of line of best fit IM NOT SURE IF THIS WORKS
coef(lm(saldf$yi ~ saldf$AbsolutLat))

##PLOT for Ablat
levels(metadf$NativeNone)
metadf$NativeNone <- as.factor(metadf$NativeNone)
levels(metadf$NativeNone)<- c("Native", "Non-native") #change names

ggplot(metadf)+ 
  geom_point(mapping = aes(x = StressAnomaly, y = yi, colour = factor(NativeNone))) + 
  theme_classic()+
  theme(text = element_text(size=16)) + 
  theme(legend.position=c(.8, .9),legend.text = element_text(colour="black", size = 16)) + 
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  scale_color_manual(name="Seperated by", values = c("Native" = "black", "Non-native" = "red")) + 
  scale_x_continuous(breaks=seq(0,30,2)) +
  geom_abline(intercept = 0.92, slope = 0.01, colour = "red") + 
  geom_abline(intercept = -0.18, slope = 0.01, colour = "black") + 
  labs(y=expression("Hedges' "*italic("d")))+ labs(x ="\nAbsolute latitude")


##PLOT for Ablat
str(MA$AbsolutLat)
MA$AbsolutLat = as.numeric(as.character(MA$AbsolutLat))
levels(MA$Stress)
MA$Stress <- as.factor(MA$Stress)
levels(MA$Stress) <- c("Salinity", "Temperature")

ggplot(MA)+ 
  geom_point(mapping = aes(x = AbsolutLat, y = yi, colour = factor(Stress))) + 
  theme_classic()+
  theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(color="black", size=14),axis.text.y = element_text(color="black", size=14))+
  theme(legend.position=c(.9, .8),legend.text = element_text(colour="black", size = 16)) +
  scale_color_manual(name="Type\nof Stress", values = c("Salinity" = "Blue", "Temperature" = "red")) + 
  scale_x_continuous(breaks=seq(0,60,10)) +
  geom_abline(intercept = -4.78, slope = 0.09, colour = "blue") + 
  geom_abline(intercept = -0.64, slope = 0.02, colour = "red") + 
  labs(y=expression("Hedges' "*italic("d")))+ labs(x ="\nAbsolute latitude")



