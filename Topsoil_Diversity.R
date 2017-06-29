#DATA:========
#Correlation between density and Richness indices:
#write.table(data,file="FD.all.seasons.outcome.csv",sep=",", row.names=FALSE)
#setwd("C:/Users/31757326/Desktop/MURDOCHWorkNov15/Thesis/CH6_Traits/Analysis/FD")
#File "FD.all.seasons.outcome.csv" is a wide species matrix with FDis column computed before in "FD" pckg.
if (!require(tidyverse)) library(tidyverse) # load this package if not done yet 
if (!require(FD)) library(FD) # load this package if not done yet 
if (!require(vegan)) library(vegan) # load this package if not done yet  
if (!require(lme4)) library(lme4) # load this package if not done yet

#Data is stored on my page as per url below. FD.all stands for "Functional diversity indices already computed for All 4 seasons"
FD.all<-read.csv(url("https://sites.google.com/site/pawelwaryszak/forestdale-experiment/chapter-6/FD.all.seasons.outcome.csv?attredirects=0&d=1"))
#FD.all<-read.csv("FD.all.seasons.outcome.csv") #optionally after downloading csv file to working directory
str(FD.all)#2925 obs. of 204 variables:
names(FD.all)
FD.all$DIVshannon<-diversity(FD.all[,12:199])
FD.all$DIVsimpson<-diversity(FD.all[,12:199], index= "simpson")
FD.all$Density<-rowSums(FD.all[,12:199])

#Drawing correlation between Density and Diversity indices======
#install.packages("GGally")
library(GGally)
#All Sites:
#Function to return points and geom_smooth
#Source of this advice: https://stackoverflow.com/questions/35085261/how-to-use-loess-method-in-ggallyggpairs-using-wrap-function
my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}

# Default loess curve    
ggpairs(data[1:4], lower = list(continuous = my_fn))

pm <- ggpairs(FD.all[,c("DIVshannon","DIVsimpson", "Density")],
              lower = list(continuous = my_fn),
              title= "Correlations between diversity indices & densities")
pm

cool_theme<-theme(legend.position = "none", 
                    plot.title = element_text(lineheight=1.2, face="bold",size=16, hjust = 0.5),
                    panel.grid.major = element_blank(), 
                    axis.ticks = element_blank(), 
                    panel.border = element_rect(linetype = "dashed", colour = "black", fill = NA))

pm2<- pm + cool_theme 
pm2 
ggsave(filename="DivDensityCorrelation_AllSeasons.jpeg", dpi=600) 


#Spring 2012 native plants only:=========
#Spring2012 = first emergence season after topsoil transfer

#Subsetting natives:
#Trait data that fits our plant data
traitURL <- "https://sites.google.com/site/pawelwaryszak/forestdale-experiment/chapter-6/traits.all_19oct15.csv?attredirects=0&d=1"
traits <- read.csv(url(traitURL)) # read in data for traits Ind <- c(as.character(traits[traits[,2]=="native",1]), "count") # index of native species and text 'count'
names(traits)#list of recorded traits

#Spring2012 natives only:
NatSpr12<-FD.all[FD.all$season=="Spring2012",]
str(NatSpr12)#673 obs. of  207 variables:

Ind <- c(as.character(traits[traits[,4]=="native",1]), "count") # index of native species and text 'count'
colInd <- which(colnames(spr12) %in% Ind) # index of column numbers that contain the text from Ind
natives<-spr12[,colInd] # subset columns of all.species
str(natives)#"data.frame" =  673 obs. of  122 variables:

#re-computing diversity indices to natives (Seems OK):
NatSpr12$SWien.Diversity<-diversity(NatSpr12[,12:199])
NatSpr12$Simps.Diversity<-diversity(NatSpr12[,12:199], index= "simpson")
NatSpr12$Density<-rowSums(NatSpr12[,12:199])
NatSpr12$Richness<-specnumber(NatSpr12[,12:199])

#Computing mean + SD Richness per treatment:
##Site-scale Topsoil Treatments:
RipRichnessSpr12<-summarise(group_by(NatSpr12,rip), MeanRichness=mean(Richness),
SDRichness=sd(Richness),n=length(Richness))%>% mutate(Treat = rip)
RipRichnessSpr12
#######rip MeanRichness SDRichness NumberRichness
#   ripped     16.11573   5.833628            337
# unripped     27.57440   6.938996            336
TransdepthRichnessSpr12<-summarise(group_by(NatSpr12,Transdepth), MeanRichness=mean(Richness),
                            SDRichness=sd(Richness),n=length(Richness))%>% mutate(Treat = Transdepth)
TransdepthRichnessSpr12
#####Transdepth MeanRichness SDRichness
#       deep     23.45401   8.839094
#    shallow     20.21429   8.036962

fenceRichnessSpr12<-summarise(group_by(NatSpr12,fence), MeanRichness=mean(Richness),
                            SDRichness=sd(Richness),n=length(Richness))%>% mutate(Treat = fence)
fenceRichnessSpr12
######fence MeanRichness SDRichness NumberRichness
# fenced     22.06029   8.621346            481
#   open     21.27604   8.529091            192

plot2RichnessSpr12<-summarise(group_by(NatSpr12,plot2), MeanRichness=mean(Richness),
                              SDRichness=sd(Richness),n=length(Richness)) %>% mutate(Treat = plot2)
plot2RichnessSpr12
##Plot-scale Topsoil Treatments:
#CTRL refers to all site-scale treatments together,
#as plot treatments were nested within them:

#plot2 MeanRichness SDRichness NumberRichness
#   CTRL     21.47113   8.792343            433
#   HERB     22.10417   9.563316             48
#   PLAS     22.75000   9.327379             48
#   SHAD     21.62500   8.554726             16
#   SHSE     24.78125   5.678109             32
#   SMOK     22.43750   7.031559             48
#   SMPL     21.45833   8.052906             48

#BIND THEM TOGETHER BY Treat:
r<-RipRichnessSpr12[ , c("MeanRichness", "SDRichness","n","Treat")]
t<-TransdepthRichnessSpr12[ , c("MeanRichness", "SDRichness","n","Treat")]
f<-fenceRichnessSpr12[ , c("MeanRichness", "SDRichness","n","Treat")]
p<-plot2RichnessSpr12[ , c("MeanRichness", "SDRichness","n","Treat")]

#SORT and see how richness changes with Treat:
all.treatments<- rbind(r,t,f,p)
all.treatments$Trt<-casefold(all.treatments$Treat, upper=TRUE)#capitalizing
sort1 <- arrange(all.treatments,desc(MeanRichness))
sort1
#QUICK PLOT:
#dev.off() #if error re-occur
n1 <- ggplot(sort1, aes(x = (reorder(Trt, - MeanRichness)), y = MeanRichness, fill=Treat))
n2 <- n1 + geom_bar(stat="identity",colour="black") 
n2 + scale_x_discrete(label=function(x) abbreviate(x, minlength=4))
