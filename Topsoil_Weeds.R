#METADATA for EmData----------
#EmData = R-File for Emergence Data Analysis & Visualization.
#It contains data on emergence of plant species on restoration sites (Details in Pawel Waryszak 's PhD thesis)
#TST = Time since Topsoil transfer, 0.5 = spring 2012 (half a year after transfer) & 1.5 = Spring 2013
#newTST - relates to when the seedling was first recorded (important for survial chapter)
#count4m2 = total Count per 4 square meters vegetation survey plot.
#count1m2 = totCnt/4 to express it per 1 square m.
#cluster = block of 4m2 plots, here called "sub"

#Plotting Weeds: Effects of Treatments on weed densities YEAR I:=======
library(tidyverse)
#setwd("~/OneDrive/Documents/Murdoch/All Chapters Together/Manuscripts/Germination Manuscript")
data<- read.csv("TopEmergenceGood.csv")
names(data)

#Group by treatmet and sampling units:
#YEAR I
year1data<-data[data$TST== 0.5 & data$nat=="invasive",] #TST = time since transfer
length(levels(year1data$su))# 853 =too many levels, as it remembers all of them
year1data$su <-factor(year1data$su) #factor is the way to drop emtpy levels
length(levels(year1data$su))#673 = YAY!
#Let us compute plat densities per plot (su), per site-treatment:
d1.depth<-summarise(group_by(year1data,su,Transdepth),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("DISPERSAL"))
dim(d1.depth)#674   4
colnames(d1.depth)[2]<- "Site.Treatment"

d1.rip<-summarise(group_by(year1data,su,rip),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("ABIOTIC"))
dim(d1.depth)#674   4
colnames(d1.rip)[2]<- "Site.Treatment"

d1.fence<-summarise(group_by(year1data,su,fence),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("BIOTIC"))
dim(d1.fence)#674   4
colnames(d1.fence)[2]<- "Site.Treatment"

d1<-bind_rows(d1.depth,d1.rip,d1.fence)
dim(d1)#2022    4

#We need to split su into site, cluster,plot:
weeds1 <- d1  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)

dim(weeds1)#2022    7 =  YAY! These are densities of weeds in year one per 1m2

#Group by treatmet and sampling units:
#YEAR II:
year2data<-data[data$TST== 1.5 & data$nat=="invasive",] #TST = time since transfer
length(levels(year2data$su))# 853 =too many levels, as it remembers all of them
year2data$su <-factor(year2data$su) #factor is the way to drop emtpy levels
length(levels(year2data$su))#846 = 8 missing =  NAY! These are densities of weeds in year one per 1m2,
# There were Some empty plots, not infested. We skip them as all plots were infested
#Some weeds were not recorded early in the veg season.

#Let us compute plat densities per plot (su), per site-treatment:
d2.depth<-summarise(group_by(year2data,su,Transdepth),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("DISPERSAL"))
dim(d2.depth)#847   4
colnames(d2.depth)[2]<- "Site.Treatment"

d2.rip<-summarise(group_by(year2data,su,rip),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("ABIOTIC"))
dim(d2.depth)#847   4
colnames(d2.rip)[2]<- "Site.Treatment"

d2.fence<-summarise(group_by(year2data,su,fence),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("BIOTIC"))
dim(d2.fence)#847   4
colnames(d2.fence)[2]<- "Site.Treatment"

d2<-bind_rows(d2.depth,d2.rip,d2.fence)
dim(d2)#2541    4

#We need to split su into site, cluster,plot:
weeds2 <- d2  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)

#WEEDS Densities both years:============
dim(weeds1)#2022    7
names(weeds1)
weeds1$Year<-"one"
as.data.frame(weeds1)

dim(weeds2)#2541    7
names(weeds2)
as.data.frame(weeds2)
weeds2$Year<-"two"

identical(names(weeds1), names(weeds2)) # TRUE - YAY! after computing sums
#we can merge two years together now
weeds1and2<-rbind(weeds1,weeds2)#does not like it:
str(weeds1and2)#4563 obs. of  8 vari

#Compute 95% CI using summarySE:
library(Rmisc)
all.weeds<-summarySE(weeds1and2, measurevar="Weed.Density.1m2",
                     groupvars=c("Site.Treatment","Filter","Year"))
all.weeds$nat<-"invasive" #it looks like summarySE does not like dplyr produce -> NAs produced
all.weeds
all.weeds<-all.weeds[1:12,]
all.weeds

#GGPLOT:
pd <- position_dodge(.5)#setting the gaps between elements on graph
all.weeds1<-ggplot(all.weeds, aes(x=Year, y=Weed.Density.1m2, shape=Year, color= Year, fill=Year))
all.weeds2<-all.weeds1 +geom_errorbar(aes(ymin=Weed.Density.1m2-ci, ymax=Weed.Density.1m2+ci),width=.50,position=pd,size=1.4)
all.weeds3<-all.weeds2+ geom_point(position=pd,size=6)
all.weeds3a<-all.weeds3 + geom_line(position=pd)+ scale_colour_manual(values = c("brown", "black"))# +scale_shape_manual(values=22) #
all.weeds4<-all.weeds3a+facet_grid(.~Filter+Site.Treatment)+theme_bw()
all.weeds4


#with grid bigger fond +8:
all.weeds5<- all.weeds4 +theme(axis.text.x=element_blank(),
                               axis.text.y=element_text(size=20),
                               axis.title.x=element_blank(),
                               axis.title.y=element_text(size=28),
                               panel.grid.minor.x = element_blank(),
                               strip.text=element_text(size=24),
                               legend.position = "none")

all.weeds5
all.weeds7<- all.weeds5 +  scale_y_continuous("Weed Density (1m\u00B2)")
all.weeds7


#NATIVE DENSITIES (FINAL YEAR AUT 2014) for ESA2017 poster:======
#LOAD Final Densities DATA
data<- read.csv("FinalDensityAll.csv")#Off computer directory:
str(data)#'data.frame':  855 obs. of  17 variables::
range(data$sum1m2)#[1] 0 - 7.25
#Main  Site-Scale Treatments MEANS +- SE:
#summarizing data on final density.
library(tidyverse)
siteScale<-data[data$treat=="control",] #control referes to all site-treatments (Rip, fence and transdepth)
b1 <- ddply(siteScale, c("Transdepth"), summarise,
            N    = length(sum1m2),
            mean = mean(sum1m2),
            sd   = sd(sum1m2),
            se   = sd / sqrt(N)
)
b1
colnames(b1)[1]<-"Treatment"
b1$Filter<- "DISPERSAL"
#
b2 <- ddply(siteScale, c("rip"), summarise,
            N    = length(sum1m2),
            mean = mean(sum1m2),
            sd   = sd(sum1m2),
            se   = sd / sqrt(N)
)
b2
colnames(b2)[1]<-"Treatment"
b2$Filter<- "ABIOTIC"

#
b3 <- ddply(siteScale, c("fence"), summarise,
            N    = length(sum1m2),
            mean = mean(sum1m2),
            sd   = sd(sum1m2),
            se   = sd / sqrt(N)
)
colnames(b3)[1]<-"Treatment"
b3$Filter<- "BIOTIC"

bar<-rbind(b1,b2,b3)
bar$Time<-"two"
bar

#GGPLOT:
pd<-position_dodge(0.9)
f<-ggplot(bar, aes(x=Time,shape = Time ,fill=Treatment, y=mean, width=.75))
f1<-f + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.4, position=pd)
f2<-f1 + ylab("Native Densities (1m\u00B2)") #+ggtitle("End Mean Densities of Woody Perennials (Autumn 2014)")
f3 <- f2 + geom_point(position=pd,size=8) + geom_line() 
f4<-f3 +scale_shape_manual(values= 22) # 0 = empty square
f5<-f4 +facet_grid(.~Filter+Treatment)+ theme_bw()
f5
f6 <- f5+theme(axis.text.x=element_blank(),
               axis.text.y=element_text(size=20),
               axis.title.x=element_blank(),
               axis.title.y=element_text(size=28),
               panel.grid.minor.x = element_blank(),
               strip.text=element_text(size=24),
               legend.position = "none")

f6
