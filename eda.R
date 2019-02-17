library(nlme)
library(readr)
library(vcd)
library(reshape2)
library(lme4)
# load data
Collection <- read_csv("ABMI_Soil_Substrate_Data_2007-2017/A_T23A_Soil_Collection_8696278229856835266.csv")
Disturbance <- read_csv("ABMI_Soil_Substrate_Data_2007-2017/A_T23B_Soil_Disturbance_6115164298572665863.csv")
Mineral <- read_csv("ABMI_Soil_Substrate_Data_2007-2017/A_T25_Mineral_Soil_408583918077381328.csv")
# transfering string data type into factors
Collection <- as.data.frame(unclass(Collection),check.names = FALSE)
Disturbance <- as.data.frame(unclass(Disturbance),check.names = FALSE)
Mineral <- as.data.frame(unclass(Mineral),check.names = FALSE)
# remove missing data
Collection <- Collection[!is.na(Collection$`ABMI Site`),]
Disturbance <- Disturbance[!is.na(Disturbance$`ABMI Site`),]
Mineral <- Mineral[!is.na(Mineral$`ABMI Site`),]
Mineral <- Mineral[!Mineral$`Total Carbon (Percent of Dry Weight)`=='VNA',]
Mineral <- Mineral[Mineral$Quadrant!='DNC',]
Mineral = droplevels(Mineral)
#
#Test whether the carbon in four direction are same
# rotation 1
Mineral1 = Mineral[Mineral$Rotation=='Rotation 1',]
# rotation 2
Mineral2 = Mineral[Mineral$Rotation=='Rotation 2',]
M1 = matrix(NA,length(unique(Mineral1$`ABMI Site`)),4)
colnames(M1) = levels(Mineral1$Quadrant)
rownames(M1) = sort(unique(Mineral1$`ABMI Site`))
M2 = matrix(NA,length(unique(Mineral2$`ABMI Site`)),4)
colnames(M2)<-levels(Mineral2$Quadrant)
for(i in rownames(M1)){
  for (j in levels(Mineral$Quadrant)) {
    Carbon = Mineral1[which(Mineral1$`ABMI Site`==i & Mineral1$Quadrant==j),]$`Total Carbon (Percent of Dry Weight)`
    if(length(Carbon)!=0){
      M1[i,j] = mean(Carbon)
    }
  }
}
### First test whether there is a difference within the four quadrant.
# M1.new=as.matrix(na.omit(M1))
# max=apply(M1.new, 1, max)
# min=apply(M1.new, 1, min)
# # diff = max - min
# M1.new=cbind(M1.new,diff=max-min)
# # using a t-test to test whether the four direction are different.
# t.test(M1.new[,'diff'])

#1. Merge three tables for round1
Collection1 = Collection[Collection$Rotation=='Rotation 1',]
#identify columns contain soil core sample
n1=grep("Soil Core Sample", colnames(Collection1))
#romove those columns
Collection1_uni=unique(Collection1[ ,-c(n1)])
# spread mineral
Disturbance1 = Disturbance[Disturbance$Rotation=='Rotation 1',]
Disturbance1.human = Disturbance1[Disturbance1$`Human or Natural Disturbance`=='Human',]
names(Disturbance1.human)[8]<-'Human'
Disturbance1.human$`Human or Natural Disturbance` <- NULL
# merge the collection unique table and mineral table
A=merge(x = Collection1_uni[,-c(4:5)], y = Mineral1[,-c(4:7)], by = c("Rotation","ABMI Site","Year","Quadrant"), all = TRUE)
# remove missing data
A=A[!is.na(A$`Total Carbon (Percent of Dry Weight)`),]
A$`Total Carbon (Percent of Dry Weight)` = as.numeric(A$`Total Carbon (Percent of Dry Weight)`)
A = A[A$`Slope Position`!='DNC',]
# table 2 cannot be merged because there are multiple levels of disturbance type corresponding to one quardant. i.e one observation, but there are plenty of predictors.
All=A[,c(2:10,15:16)]
All = droplevels(All)
# replace colume name to enable the code more clear.
colnames(All)[1] <- 'site'
colnames(All)[4] <- 'nutrient'
colnames(All)[5] <- 'species'
colnames(All)[6] <- 'stage'
colnames(All)[7] <- 'percent'
colnames(All)[8] <- 'position'
colnames(All)[9] <- 'direction'
colnames(All)[10] <- 'carbon'
# change the slope direction into appropriate category
All$slope = NA
All$direction[All$direction=='VNA'] <- NA
All$direction <- as.numeric(All$direction)
All$slope[which(All$direction < 22.5 | All$direction > 337.5)] <- 'N'
All$slope[which(All$direction > 22.5 & All$direction < 67.5)] <- 'NE'
All$slope[which(All$direction > 67.5 & All$direction < 112.5)] <- 'E'
All$slope[which(All$direction > 112.5 & All$direction < 157.5)] <- 'SE'
All$slope[which(All$direction > 157.5 & All$direction < 202.5)] <- 'S'
All$slope[which(All$direction > 202.5 & All$direction < 247.5)] <- 'SW'
All$slope[which(All$direction > 247.5 & All$direction < 292.5)] <- 'W'
All$slope[which(All$direction > 292.5 & All$direction < 337.5)] <- 'NW'
All$slope[is.na(All$direction)] <- 'FLAT'
All$direction <- NULL
All$slope <- as.factor(All$slope)
levels(All$slope)

##############################MODEL FITTING################################
library('lattice')
library('kader')
library(MASS)
library(nlme)
library(lme4)
library(sjPlot)
require(car)



# correlation plot to visualize any linear association between carbon and else
DF=All[,c(9,1:8,10,11)]
DF[] <- lapply(DF,as.integer)
sjp.corr(DF)
# visualize the data
boxplot(carbon ~ position,col="lightyellow",xlab='Position',All)
boxplot(carbon ~ slope,col="lightblue",xlab='Direction',All)
interaction.plot(All$position,All$slope,All$carbon,col = 2:9, lty = 1)

#FULL model
lmer1 = lmer(logit(carbon/100) ~ position*slope + (1|site),data = All)
# reduced model 1 (no interaction)
lmer2 = lmer(logit(carbon/100) ~ position+slope+ (1|site),data = All)
# reduced model 2 (no slope direction)
lmer3 = lmer(logit(carbon/100) ~ position + (1|site),data = All)
# reduced model 3 (no position)
lmer4 = lmer(logit(carbon/100) ~ slope + (1|site),data = All)
anova(lmer1,lmer2,lmer3,lmer4)
anova(lmer1,lmer2)# interaction effect is not significant
anova(lmer2,lmer3)# slope direction effect is not significant 
anova(lmer2,lmer4)# position effect is very significant
anova(lmer4,lmer3)# same conclusion
rsquare(lmer2)

lm1=lm(logit(carbon/100) ~ position+Year,data = All)
summary(lm1)
logLik(lm1)
lm1=lm(logit(carbon/100) ~ position+Year+site,data = All)
summary(lm1)
lm1=lm(logit(carbon/100) ~ position+Year+as.factor(site),data = All)
summary(lm1)
# the variation between site to site should be considered in the model
# adding the effect of Year
lmer2.y = lmer(logit(carbon/100) ~ position + Year + slope + (1|site),data = All)
lmer3.y = lmer(logit(carbon/100) ~ position + Year + (1|site),data = All)
anova(lmer2,lmer2.y)
anova(lmer3,lmer3.y) # it impoves the model fit significantly
