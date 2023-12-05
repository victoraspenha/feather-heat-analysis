##########################################
rm(list=ls())

## Needed packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(lme4)
library(psych)
library(regclass)
library(car)
library(BBmisc)
library(lmerTest)
library(multcomp)
library(emmeans)
library(Rmisc)
library(MuMIn)

## Getting data
data <- read.csv("./data.csv")
str(data)

## Correcting columns
## Type of feather
unique(data$Type)
data$Type <- as.factor(data$Type)

## Site
unique(data$Capture.Site)
data[which(data$Capture.Site == "Rural"),"Capture.Site"] <- "rural"
data$Capture.Site <- as.factor(data$Capture.Site)

## Scorer
data$Photo.Scorer.s.Initials <- as.factor(data$Photo.Scorer.s.Initials)

## Trial
data$Trial <- as.factor(data$Trial)

## Bird ID
data$Bird.ID.. <- as.factor(data$Bird.ID..)

## Changing the dataframe
## Making an average per trial
matriz <- read.csv("./matriz.csv")

for(i in 1:dim(matriz)[1]){
  filter <- data[which(data$Bird.ID.. == matriz$Bird.ID..[i]),]
  if(length(unique(filter$Type)) > 1){
    tipo <- unique(filter$Type)
    for(k in 1:length(tipo)){
      filter2 <- filter[which(filter$Type == tipo[k]),]
      if(length(unique(filter2$Capture.Site)) > 1){
        sitios <- unique(filter2$Capture.Site)
        for(m in 1:length(sitios)){
          filter3 <- filter2[which(filter2$Capture.Site == sitios[m]),]
          matriz[matriz$Bird.ID.. == unique(filter$Bird.ID..) & matriz$Type == unique(filter2$Type) & matriz$Capture.Site == unique(filter3$Capture.Site),c(4:36)] <- colMeans(filter3[,c(6:38)])
        }
      }
      else{
        matriz[matriz$Bird.ID.. == unique(filter$Bird.ID..) & matriz$Type == unique(filter2$Type),c(4:36)] <- colMeans(filter2[,c(6:38)])
      }
    }
  }
  else{
    matriz[matriz$Bird.ID.. == unique(filter$Bird.ID..),c(4:36)] <- colMeans(filter[,c(6:38)])
  }
}
## Final dataframe
matriz$Capture.Site <- as.factor(matriz$Capture.Site)
levels(matriz$Capture.Site)[2] <- "rural"

## Changing site: too little rural birds: urban x non-urban
matriz$Site <- NA
matriz[which(matriz$Capture.Site == "urban"),"Site"] <- "urban"
matriz[which(is.na(matriz$Site)),"Site"] <- "non-urban"

## Transoforming classes
matriz$Type <- as.factor(matriz$Type)
matriz$Bird.ID.. <- as.factor(matriz$Bird.ID..)
matriz$Site <- as.factor(matriz$Site)

## Variable distributions
hist(log(matriz$RealMax))
hist(matriz$RealMin)
hist((matriz$Up1))
hist(matriz$Down1)

####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
## Repeated measures
## Making a long dataframe
long_data <- matriz %>%
  gather(key = "time", value = "score", Temp_0s:Temp_120s) %>% 
  convert_as_factor(Type, time)

## Remove unwanted columns
colnames(long_data)
long_data <- long_data[,-c(4:11)]

## Ordering the levels to make a correct graph
long_data$time <- factor(long_data$time, levels = c("Temp_0s", "Temp_5s", "Temp_10s", "Temp_15s", "Temp_20s", "Temp_25s", "Temp_30s", "Temp_35s", 
                                                    "Temp_40s", "Temp_45s", "Temp_50s", "Temp_55s", "Temp_60s", "Temp_65s", "Temp_70s", "Temp_75s", 
                                                    "Temp_80s", "Temp_85s", "Temp_90s", "Temp_95s", "Temp_100s", "Temp_105s", "Temp_110s", "Temp_115s", "Temp_120s"))

####################################################################################################################################################################
## Models: heating phase
long_data$time <- gsub("Temp_", "", long_data$time)
long_data$time <- gsub("s", "", long_data$time)
long_heating <- long_data[1:260,]

levels(long_heating$time)
long_heating$time <- factor(long_heating$time, levels = c("0","5","10","15","20","25","30","35","40","45","50","55","60"))

## Model
colnames(long_heating)
model_repeated_heating <- lmer(score ~ Type + time*Site + (1|Bird.ID..), long_heating)
model_repeated_heating_null <- lmer(score ~ 1 + (1|Bird.ID..), long_heating)
anova(model_repeated_heating, model_repeated_heating_null)
drop1(model_repeated_heating, test = "Chisq")
summary(model_repeated_heating)

## Pairwise
model <- aov(score~factor(time)*Site+time*Site+Error(factor(Bird.ID..)), data = long_heating)
summary(model)
emm<-emmeans(model, ~ factor(time)*Site)
write.csv(pairs(emm),"./Emms.csv")

####################################################################################################################################################################
## Models: Cooling phase
long_cooling <- long_data[which(long_data$time == "60")[1]:dim(long_data)[1],]

levels(long_cooling$time)
long_cooling$time <- factor(long_cooling$time, levels = c("60", "65","70","75","80","85","90","95","100","105","110","115","120"))

## Model
colnames(long_cooling)
model_repeated_cooling <- lmer(score ~ Type + time*Site + (1|Bird.ID..), long_cooling)
model_repeated_cooling_null <- lmer(score ~ 1 + (1|Bird.ID..), long_cooling)
anova(model_repeated_cooling, model_repeated_cooling_null)
drop1(model_repeated_cooling, test = "Chisq")
summary(model_repeated_cooling)

## Pairwise
model <- aov(score~factor(time)*Site+Error(factor(Bird.ID..)), data = long_cooling)
summary(model)
emm<-emmeans(model, ~ factor(time)*Site)

#####################
## Plotting results
ggplot(long_cooling, aes(x = Type, y = predict(model_repeated_cooling))) + geom_boxplot() + theme_bw() + ylab("Temperature (Celcius)") + xlab("Feather type")

## Plotting figure
long_data$mean <- (long_data$RealMax+long_data$RealMin)/2
plotar <- summarySE(long_data, measurevar="score", groupvars=c("time","Site"))
plotar$time <- as.integer(as.character(plotar$time))
annotation <- data.frame(
  x = c(30,90),
  y = c(21,21),
  label = c("HEATING", "COOLING"))

## Plotting paper plot
ggplot(plotar, aes(y=score, x=time, col=Site)) + 
  geom_errorbar(aes(ymin=score-se, ymax=score+se), width=.1) + 
  geom_line() + 
  scale_color_manual(values=c('red','burlywood4')) +
  scale_y_continuous(breaks=c(22,24,26,28,30,32,34,36)) + 
  geom_point() + scale_x_continuous(breaks = plotar$time) +
  ylab("Feather Temperature (Celsius)") + xlab("Time (seconds)") + 
  theme_classic() +
  geom_rect(mapping = aes(xmin = 0, xmax = 60, ymin = 20, ymax = 38),
            alpha = 0.005, fill = "red", color = NA) +
  geom_rect(mapping = aes(xmin = 60, xmax = 120, ymin = 20, ymax = 38),
            alpha = 0.005, fill = "blue", color = NA) + 
  geom_text(data=annotation, aes( x=x, y=y, label=label),
            color="black", 
            size=7 ,fontface="bold" )
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
## Repeatability results
## Intra trial repeatability
intratrail <- read.csv("./intraTrialRep.csv")
str(intratrail)

intratrail$Band <- as.factor(intratrail$Band)
unique(intratrail$Type)
intratrail$Type <- as.factor(intratrail$Type)
intratrail$Time <- as.factor(intratrail$Time)
colnames(intratrail)

## Model
model1 <- lmer(First ~ Second + (1|Band) + (1|Type) + (1|Time), intratrail)
summary(model1)
r.squaredGLMM(model1)

####################################################################################################################################################################
## intra-picture repeatability
intrapic <- read.csv("./IntraPictureRep.csv")
str(intrapic)

intrapic$ID <- as.factor(intrapic$ID)
unique(intrapic$Type)
intrapic$Type <- as.factor(intrapic$Type)
intrapic$Time <- as.factor(intrapic$Time)
intrapic$Trial <- as.factor(intrapic$Trial)

## Model
model1 <- lmer(Value1 ~ Value2 + (1|ID) + (1|Type) + (1|Time) + (1|Trial), intrapic)
summary(model1)
r.squaredGLMM(model1)

####################################################################################################################################################################
## Intra-observer
intraobs <- read.csv("./IntraObsRep.csv")
str(intraobs)

intraobs$ID <- as.factor(intraobs$ID)
unique(intraobs$ID)
intraobs$Type <- as.factor(intraobs$Type)
intraobs$Time <- as.factor(intraobs$Time)
intraobs$Trial <- as.factor(intraobs$Trial)
intraobs$Measurer <- as.factor(intraobs$Measurer)

## Model
model1 <- lmer(Value1 ~ Value2 + (1|ID) + (1|Time) + (1|Trial), intraobs)
summary(model1)
r.squaredGLMM(model1)

####################################################################################################################################################################
## Intra-observer
interobs <- read.csv("./inter-obs.csv")
str(interobs)

interobs$ID <- as.factor(interobs$ID)
unique(interobs$ID)
interobs$Type <- as.factor(interobs$Type)
interobs$Time <- as.factor(interobs$Time)
interobs$Trial <- as.factor(interobs$Trial)

## Model
model1 <- lmer(Value ~ Value2 + (1|ID) + (1|Time) + (1|Trial), interobs)
summary(model1)
r.squaredGLMM(model1)

## End of code