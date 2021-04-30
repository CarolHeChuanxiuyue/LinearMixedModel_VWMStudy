## ---------------------------
##
## Script name: Exp1_SupplementaryAnalysis
##
## Purpose of script: Based on the main analysis, other analyses done for Experiment 1. (need to run main analysis first)
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2020-12-21
##
## Email: carol.hcxy@gmail.com
##
## ---------------------------


## ---------------------------

## set working directory for Mac and PC

setwd("~/CarolHe/")      # Carol's working directory (mac)
setwd("C:/Users/carolhe/")    # if using PC

## ---------------------------

## load up the packages
## use install.packages() to install new packages
library(tidyverse)
library(reshape2)
library(lme4) #linear mixed model
library(rstatix) #chi-squared tests for mixed model

#####----------Linear Mixed Model with MRT----------#####

Exp1_spab_2 <- merge(Exp1_psy,Exp1_dpr_long,by="subject")%>%
  mutate(SA=(scale(PF,center = T,scale = T)+
               scale(CC,center = T,scale = T)+
               scale(MRT,center=T,scale=T))/3)%>%
  mutate(rotation_s=scale(rotation_num,center=T,scale=T))%>%
  select(subject,rotation_s,symmetry,dp,SA)

Exp1_spab_2$subject <- as.factor(Exp1_spab_2$subject)

Exp1.lmm3 <- lmer(dp ~ rotation_s * symmetry+SA+SA:symmetry+SA:rotation_s+ (1|subject), data = Exp1_spab_2, REML = F)
summary(Exp1.lmm3)
Anova(Exp1.lmm3)

#####----------Linear Mixed Model with separated PF/CC/MRT----------#####

Exp1_spab_3 <- merge(Exp1_psy,Exp1_dpr_long,by="subject")%>%
  mutate(rotation_s=scale(rotation_num,center=T,scale=T))%>%
  mutate(PF_s=scale(PF,center=T,scale=T))%>%
  mutate(CC_s=scale(CC,center=T,scale=T))%>%
  mutate(MRT_s=scale(MRT,center=T,scale=T))%>%
  select(subject,rotation_s,symmetry,dp,PF_s,CC_s,MRT_s)

Exp1_spab_3$subject <- as.factor(Exp1_spab_3$subject)


Exp1.lmm4 <- lmer(dp ~ rotation_s * symmetry+PF_s+CC_s+ (1|subject), data = Exp1_spab_3, REML = F)
summary(Exp1.lmm4)
Anova(Exp1.lmm4)


#####----------Response Time (RT) of Correct Trials----------#####


## remove incorrect trials
Exp1_rt_correct <- combined_df_noTimeOut[combined_df_noTimeOut$accuracy==1,]

## proportion of the removed trials
(nrow(combined_df)-nrow(Exp1_rt_correct))/nrow(combined_df)

## aggregate to get mean response time
TIME <- aggregate(Exp1_rt_correct$time,
                  list(subject=Exp1_rt_correct$subject,
                       rotation=Exp1_rt_correct$rotation,
                       startsym=Exp1_rt_correct$startSym,
                       change=Exp1_rt_correct$change), 
                  mean)

names(TIME)[5] <- "RT"

## merge with spatial ability scores
Exp1_spabRT <- merge(TIME,Exp1_psy,by="subject")%>%
  mutate(SA=(scale(PF,center = T,scale = T)+
               scale(CC,center = T,scale = T))/2)%>%
  mutate(rotation_s=scale(rotation,center=T,scale=T))%>%
  select(subject,rotation_s,startsym,change,RT,SA)


Exp1_spabRT$change <- as.factor(Exp1_spabRT$change)
Exp1_spabRT <- within(Exp1_spabRT,change <- relevel(change,ref="0"))
Exp1_spabRT <- within(Exp1_spabRT,startsym <- relevel(startsym,ref="as"))

#####----------Response Time Linear Mixed Model----------#####

Exp1.lmmRT <- lmer(RT ~ rotation_s*startsym*change+ (1|subject), data = Exp1_spabRT, REML = F)
summary(Exp1.lmmRT)
Anova(Exp1.lmmRT)
confint(Exp1.lmmRT)

#####----------Gender Differences----------#####

## get gender information
Exp1_Gender <- combined_df[,c("subject","sex")]
Exp1_psy_sex <- unique(merge(Exp1_psy,Exp1_Gender,by="subject"))

## gender distribution
table(Exp1_psy_sex$sex)

## test gender differences
t.test(sym.Dp~sex,data=Exp1_psy_sex)
t.test(as.Dp~sex,data=Exp1_psy_sex)
t.test(PF~sex,data=Exp1_psy_sex)
t.test(CC~sex,data=Exp1_psy_sex)
t.test(MRT~sex,data=Exp1_psy_sex)