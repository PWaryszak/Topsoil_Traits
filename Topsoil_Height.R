h<-read.csv("height.csv")
str(h)#Data on height pulled out for woody only for Autumn2014/tst=2.0
#953 obs. of  32 variables:
library(tidyverse)

#All treatments Combinations============
g <- summarise(group_by_(h,"su", "comb2", "plot2"),sum = sum(hgt))#first summing the densities by sampling unit(su)
g  
SumStats<-summarise(group_by_(g, "comb2", "plot2"), mean=mean(sum), sd=sd(sum), N = length(sum), se   = sd / sqrt(N))
SumStats #computed means +-SE

#ggplot:
pd <- position_dodge(0.3) #
# Plot <-ggplot(SumStats, aes(x=rip, y=mean, fill= nat , width=.75)) + 
g1<-ggplot(SumStats, aes(x=plot2, y=mean , fill=plot2, width=.75))
g2<-g1 + geom_bar(position=pd, stat="identity")+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se,color="black"),width=.4, position=pd, color ="black")
g2 
g3<- g2 + facet_grid(.~comb2)
g3
g4<-g3 + theme(axis.text.x=element_text(size=10,angle=90),
          axis.text.y=element_text(size=19),
          axis.title.y=element_text(angle=90,size=20),
          axis.title.x=element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text=element_text(size=28),
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(face = "italic",size=20))
  
g4

#Single treatments============
#plot2 codes control for all site level treatmetns
#Subset controls, transform to one variable called Treatment:

siteTreat<- h[h$plot2 == "control",]
plotTreat<- h[h$plot2 != "control",]
dim(siteTreat)+dim(plotTreat)#953  64
dim(h)#953 = ALL GOOD. Subsetting went well

#Merge 4 treatment columns into one variable column:
siteTreatVar1<- siteTreat[ ,c("Transdepth", "hgt")]
colnames(siteTreatVar1)[1]<-"Treatment"

siteTreatVar2<- siteTreat[ ,c("rip",  "hgt")]
colnames(siteTreatVar2)[1]<-"Treatment"

siteTreatVar3<- siteTreat[ ,c("fence",  "hgt")]
colnames(siteTreatVar3)[1]<-"Treatment"

plotTreatVar<- plotTreat[ ,c("plot2", "hgt")]
colnames(plotTreatVar)[1]<-"Treatment"

h2<-rbind(siteTreatVar1,siteTreatVar2,siteTreatVar3,plotTreatVar)

h2$hgt2 <- h2$hgt/100 #order the data by mean
SumStats2<-summarise(group_by_(h2,"Treatment"), mean=mean(hgt2), sd=sd(hgt2), N = length(hgt2), se   = sd / sqrt(N))
SumStats2 #computed means +-SE
SumStats2 <- SumStats2[ order(SumStats2$mean),] #order the data by mean
SumStats2
SumStats3<-SumStats2[SumStats2$Treatment != "shade.semi",]#removing  shade.semi = vandalized shade treatment
levels(droplevels(SumStats3$Treatment))#hange levels as per order to look ascending on figure:
SumStats3$Treatment<- factor(SumStats3$Treatment, levels=SumStats3$Treatment[order(SumStats3$mean)], ordered=TRUE)

#ggplot:
pd <- position_dodge(0.3) #
g1<-ggplot(SumStats3, aes(x=Treatment, y=mean))
g2<-g1 + geom_bar(position=pd, stat="identity")
g3<- g2 + ylab("Plant Height (m)")+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.4, position=pd)

g4<-g3 + theme(axis.text.x=element_text(size=12),
               axis.text.y=element_text(size=18),
               axis.title.y=element_text(size=20),
               axis.title.x=element_text(size=20),
               legend.position = "none")

g4# + theme_classic()
