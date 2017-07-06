#LOAD LIBRARIES AND DATA:======
library(vegan)
library(MASS)
library(tidyverse)
setwd("C:/Users/poles/OneDrive/R/PhD.Thesis/CH6.Traits/FD")

#DATA:
BigDF2<-read.csv("big2trait.csv")#the entire data frame (site,plot,trait)
str(BigDF2)#data.frame':  787500 obs. of  39 variables,791058 obs. of  38 variables with REMNANT DATA
big2.du<-BigDF2[BigDF2$comb2=="deep.unripped",]#subsetting only the most abundant treatment assigned to remnant too
du.ctrl<-big2.du[big2.du$plot2=="CTRL",]
str(du.ctrl)#133837 obs. of  38 variables:

#subsetting  only few columns that will be needed for vegan:
a<-du.ctrl[, c( "specCode", "count","su","season", "comb2","plot2")]
a$binary<-ifelse(a$count > 0, 1,0)#turning count int binary presence/absence data
names(a)# "specCode","count","su","season","comb2" ,"plot2", "binary"  
str(a)#data.frame':	133837 obs. of  7 variables:

#NMDS per Morgan's comment on thesis======
# = Comparing "start" (spr12) and "end" season (aut14):
#We work on deep.unripped (a) only as it was most successful treatment in terms of native densities
levels(a$season)
a1<-a[a$season== "Spring2012" | a$season== "Autumn2014",]
str(a1)#data.frame':	58949 obs. of  7 variables:
a1<- a1[ c("specCode","su","season", "binary")]

a1.wide<-tidyr::spread(a1, specCode, binary, fill = TRUE)#producing species matrix...
dim(a1.wide)#233 255
species<-a1.wide[,3:255]

#Remove Singletons:
occur.cols<-apply(species,2,sum)#sum species occurances in each column
a1.good.matrix<-species[ , !occur.cols <= 1 ] #removing all 0or1-sum columns.
dim(a1.good.matrix) #233 124/160, columns removed.
rows1<-rowSums(a1.good.matrix)
range(rows1)#0 48

veg<-a1.good.matrix #this a good subset of the species only matrix
env<-as.data.frame(a1.wide[,"season"])#it subsets main treatments for env fit matrix
colnames(env1)[1] <- "season"
veg.env<-cbind(veg,env)

#removing zero rows
veg.env$RowSum<-rowSums(veg)
range(veg.env$RowSum)#0 48
str(veg.env)#233 obs. of  126 variables:

veg.env1<-subset(veg.env, veg.env$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.
str(veg.env1)#209 obs. of  126 variables:

#Computing MDS:
MDS <- metaMDS(veg.env1[ , 1:124], distance = "bray")#computing distances
plot(MDS$points[,1:2])
env1<- as.data.frame(veg.env1[,125])#env data
colnames(env1)[1]<-"Season"
veg1<- veg.env1[ , 1:124]

coordinates<-as.data.frame(MDS$points[,1:2])
veg.nmds<-cbind(coordinates, env1)
dim(veg.nmds)#209 3
veg.nmds$Season<- factor(veg.nmds$Season, levels = c("Spring2012", "Autumn2014"))
#Vegan MDS in GGPLOT:
sp1<-ggplot(data = veg.nmds, aes(MDS1, MDS2)) + geom_point(aes(color = season), size= 4)
sp2<-sp1+theme_bw()+ggtitle("NMDS for start & end season")
sp2

#Two MDS points form outliers that cause messy clustering on sp2 plot
#Let us find them and remove them
names(veg.nmds)
veg.nmds2<-veg.nmds[- which.max(veg.nmds$MDS1) , ]
dim(veg.nmds2)#208 3
x<-veg.nmds2[- which.min(veg.nmds2$MDS1), ]

dim(x)# 207 3
#plot veg.nmds3 in GGPLOT:
nmds3plot<-ggplot(x, aes(MDS1, MDS2))+ geom_point(aes(color = Season, shape =Season), size= 4)+theme_bw()
nmds3plot
x3<-nmds3plot+theme(  axis.text.y=element_text(size=18),
                        axis.title.x=element_blank(),
                        axis.text.x=element_text(size=18),
                        axis.title.y=element_blank(),
                        panel.grid.minor.x = element_blank(),
                        legend.position = "bottom",
                        legend.text = element_text(size = 14),
                        legend.title = element_text(size = 14),
                        strip.text.x=element_text(size=28),
                        plot.title = element_text(lineheight=1.2, face="bold",size=26))

x4<- x3 +scale_color_manual(values = c("green", "blue"))+ scale_shape_manual(values=c(17,15))
x4

#on Producing eclipse:
#https://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo


#autumns only: 
###########
veg4<-subset(veg, veg$RowSum > 4)#that is minimum 5 in row
dim(veg4)#374 283
range(veg4$RowSum)#5 27
#Computing MDS:
MDS4=metaMDS(veg4[1:282])#computing distances
plot(MDS4$points[,1:2])
cut.env4<-rownames(veg4)#producing extra factors that fit veg4 for color-coding the plot
ab.env4<-ab.matrix[cut.env4,]#subsetting only the rows computed in FD
dim(ab.env4)#374 287
ab.env.matrix4<-ab.env4[,c(1:5)]
names(ab.env.matrix4)#"count"  "su"     "season" "comb2"  "plot2" 


#NMDS on both remnnant and topsoil restoration sites=======
#Remnant Banksia Woodland data:
REMNspecies <- read.csv("REMNspecies.csv")
names(REMNspecies)
#changing from wide to long format to fit big2trait format:
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
b <- gather(REMNspecies,specCode,count, acacpulc:xantprei)
str(b)#2424 obs. of  6 variables:
b$binary<-ifelse(b$count > 0, 1,0)#new column with 0,1 values only
range(b$binary)# 0 1
names(b)
ab<-rbind(a,b)#joining remnant data with my topsoil data
ab.matrix<-tidyr::spread(ab, specCode, binary)#producing species matrix...
head(ab.matrix)
ab.matrix[is.na(ab.matrix)] <- 0 #replaces NA=s with zero
range(ab.matrix$count)#[1]   0 272
dim(ab.matrix)#4844  287

#######
#VEGAN:
veg<-ab.matrix[,c(6:287)] #this subsets the species only matrix
env<-ab.matrix[,c("comb2","plot2","season")]#it subsets main treatments for env fit matrix
#removing zero rows
veg$RowSum<-rowSums(veg)
range(veg$RowSum)#0 27
str(veg)#4844 obs. of  282 variables:
veg1<-subset(veg, veg$RowSum != 0)#'data.frame':  4291 obs. of  283 variables: M
#MDS runs slowely with veg1 with too many low rows. Worth looking at: 
veg2<-subset(veg, veg$RowSum > 2)#
veg3<-subset(veg, veg$RowSum > 3)#
veg4<-subset(veg, veg$RowSum > 4)#
#Computing MDS:
############
MDS=metaMDS(veg1[1:282])#computing distances
plot(MDS$points[,1:2])
cut.env<-rownames(veg1)#producing extra factors that fit veg1 for color-coding the plot
ab.env<-ab.matrix[cut.env,]#subsetting only the rows computed in FD
dim(ab.env)
ab.env.matrix<-ab.env[,c(1:5)]
names(ab.env.matrix)

coordinates<-as.data.frame(MDS$points[,1:2])
veg.nmds<-cbind(veg1,coordinates, season=ab.env$season)
names(veg.nmds)
veg.nmds$stage<-veg.nmds$season
#Seperating season in two:
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="aut11"] <- "Ref.Aut2011"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="spr11"] <- "Ref.Spr2011"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="Spring2012"] <- "Off.Spr2012"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="Autumn2013"] <- "Off.Aut2013"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="Spring2013"] <- "Off.Spr2013"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="Autumn2014"] <- "Off.Aut2014"

#Vegan MDS in GGPLOT:
#all
sp1<-ggplot(data = veg.nmds, aes(MDS1, MDS2)) + geom_point(aes(color = stage), size= 4)
sp2<-sp1+theme_bw()+ggtitle("NMDS for all seasons")
sp2
#########
#autumns only: 
###########
veg4<-subset(veg, veg$RowSum > 4)#that is minimum 5 in row
dim(veg4)#374 283
range(veg4$RowSum)#5 27
#Computing MDS:
MDS4=metaMDS(veg4[1:282])#computing distances
plot(MDS4$points[,1:2])
cut.env4<-rownames(veg4)#producing extra factors that fit veg4 for color-coding the plot
ab.env4<-ab.matrix[cut.env4,]#subsetting only the rows computed in FD
dim(ab.env4)#374 287
ab.env.matrix4<-ab.env4[,c(1:5)]
names(ab.env.matrix4)#"count"  "su"     "season" "comb2"  "plot2" 

coordinates4<-as.data.frame(MDS4$points[,1:2])
veg.nmds4<-cbind(veg4,coordinates4, season=ab.env4$season)
names(veg.nmds4)
veg.nmds4$Stage<-veg.nmds4$season
#Seperating season in two:
levels(veg.nmds4$Stage)[levels(veg.nmds4$Stage)=="aut11"] <- "Ref.Aut2011"
levels(veg.nmds4$Stage)[levels(veg.nmds4$Stage)=="spr11"] <- "Ref.Spr2011"
levels(veg.nmds4$Stage)[levels(veg.nmds4$Stage)=="Spring2012"] <- "Off.Spr2012"
levels(veg.nmds4$Stage)[levels(veg.nmds4$Stage)=="Autumn2013"] <- "Off.Aut2013"
levels(veg.nmds4$Stage)[levels(veg.nmds4$Stage)=="Spring2013"] <- "Off.Spr2013"
levels(veg.nmds4$Stage)[levels(veg.nmds4$Stage)=="Autumn2014"] <- "Off.Aut2014"

veg.aut<-with(veg.nmds4, veg.nmds4[Stage == "Ref.Aut2011" | Stage == "Off.Aut2013" | Stage == "Off.Aut2014", ])
dim(veg.aut)#50 287
sp1<-ggplot(data = veg.aut, aes(MDS1, MDS2)) + geom_point(aes(color = Stage), size= 4)
sp2<-sp1+theme_bw()+ggtitle("NMDS in Autumn")
sp2
#1 outliers removed:
min(veg.aut$MDS1)
v1<-veg.aut[-which.min(veg.aut$MDS1),]
dim(v1)#49 287
v1$Stage <- factor(v1$Stage, levels = c("Ref.Aut2011", "Off.Aut2013", "Off.Aut2014"))
sp1<-ggplot(v1, aes(MDS1, MDS2)) + geom_point(aes(color = Stage, shape=Stage), size= 6)
sp2<-sp1+theme_classic()
sp2
sp3 <- sp2 + theme(axis.text.y=element_text(size=18),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(size=18),
                  axis.title.y=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.position = "bottom",
                  legend.text = element_text(size = 14),
                  legend.title = element_text(size = 14),
                  strip.text.x=element_text(size=28),
                  plot.title = element_text(lineheight=1.2, face="bold",size=26))

sp3
#ggsave(filename="nmdsVeg4b.pdf", dpi=600, width=140, height=140, unit= "mm") 
#ggsave(filename="nmdsVeg4b.jpeg", dpi=600, width=140, height=140, unit= "mm") 
#above is used in Trait Chapter as fig 3:
################


#Statistical test of NMDS ordination :
#mrpp(releves,don1$SelectionETR,distance="bray")
#########
#springs only: 
#############
veg.spr<-with(veg.nmds4, veg.nmds4[Stage == "Ref.Spr2011" | Stage == "Off.Spr2012" | Stage == "Off.Spr2013", ])
dim(veg.spr)#462 287
spspr1<-ggplot(data = veg.spr, aes(MDS1, MDS2)) + geom_point(aes(color = Stage), size= 4)
spspr2<-spspr1+theme_bw()+ggtitle("NMDS in Autumn")
spspr2
veg.spr$Stage <- factor(veg.spr$Stage, levels = c("Ref.Spr2011", "Off.Spr2012", "Off.Spr2013"))
spspr1<-ggplot(veg.spr, aes(MDS1, MDS2)) + geom_point(aes(color = Stage, shape=Stage), size= 4)
spspr2<-spspr1+theme_bw()
spspr3 <- spspr2 + theme(  axis.text.y=element_text(size=18),
                     axis.title.x=element_blank(),
                     axis.text.x=element_text(size=18),
                     axis.title.y=element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     legend.position = "bottom",
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14),
                     strip.text.x=element_text(size=28),
                     plot.title = element_text(lineheight=1.2, face="bold",size=26))

spspr3
ggsave(filename="nmdsVeg4Spring.pdf", dpi=600, width=140, height=140, unit= "mm") 
ggsave(filename="nmdsVeg4SpringB.jpeg", dpi=600, width=140, height=140, unit= "mm") 

#############
#All more than or equal 4 (n>3):
veg3<-subset(veg, veg$RowSum > 3)#
dim(veg3)#544 283
MDS3=metaMDS(veg3[1:282])#computing distances
plot(MDS3$points[,1:2])
cut.env3<-rownames(veg3)#producing extra factors that fit veg1 for color-coding the plot
ab.env3<-ab.matrix[cut.env3,]#subsetting only the rows computed in FD
dim(ab.env3)
ab.env.matrix3<-ab.env3[,c(1:5)]
names(ab.env.matrix3)

coordinates3<-as.data.frame(MDS3$points[,1:2])
veg.nmds3<-cbind(veg3,coordinates3, season=ab.env3$season)
names(veg.nmds3)
veg.nmds3$stage<-veg.nmds3$season
#Seperating season in two:
levels(veg.nmds3$stage)[levels(veg.nmds3$stage)=="aut11"] <- "Ref.Aut2011"
levels(veg.nmds3$stage)[levels(veg.nmds3$stage)=="spr11"] <- "Ref.Spr2011"
levels(veg.nmds3$stage)[levels(veg.nmds3$stage)=="Spring2012"] <- "Off.Spr2012"
levels(veg.nmds3$stage)[levels(veg.nmds3$stage)=="Autumn2013"] <- "Off.Aut2013"
levels(veg.nmds3$stage)[levels(veg.nmds3$stage)=="Spring2013"] <- "Off.Spr2013"
levels(veg.nmds3$stage)[levels(veg.nmds3$stage)=="Autumn2014"] <- "Off.Aut2014"

#Vegan MDS in GGPLOT:
#all
veg.nmds3$stage <- factor(veg.nmds3$stage, levels = c("Ref.Spr2011","Ref.Aut2011","Off.Spr2012", "Off.Aut2013","Off.Spr2013", "Off.Aut2014"))

sp1.veg3<-ggplot(data = veg.nmds3, aes(MDS1, MDS2)) + geom_point(aes(color = stage,shape=stage), size= 4)
sp2.veg3<-sp1.veg3+theme_bw()+ggtitle("NMDS for all seasons\n Sp Matrix RowSums > 3")
sp2.veg3
sp3.veg3 <- sp2.veg3 + theme(  axis.text.y=element_text(size=22),
                     axis.title.x=element_blank(),
                     axis.text.x=element_text(size=20),
                     axis.title.y=element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     legend.position = "bottom",
                     legend.text = element_text(size = 16),
                     legend.title = element_text(size = 16),
                     strip.text.x=element_text(size=28),
                     plot.title = element_text(lineheight=1.2, face="bold",size=26))

sp3.veg3

#Statistical test of NMDS ordination :
#mrpp(releves,don1$SelectionETR,distance="bray")
