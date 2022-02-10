## ---------------------------
##
## Script name: Exp2_SupplementaryAnalysis
##
## Purpose of script: Based on the main analysis, other analyses done for Experiment 2 (need to run main analysis first)
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2020-12-23
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
library(lsr) # effect size

#####----------Linear Mixed Model II----------#####

## without verbal intelligence and general intelligence
Exp2.lmm3 <- lmer(dp ~ rotation_s * symmetry+SA+SA:rotation_s+symmetry:SA+ (1|subject), data = Exp2_spab)
summary(Exp2.lmm3)
Anova(Exp2.lmm3)

## compare it with the main model reported in the paper
anova(Exp2.lmm3,Exp2.lmm1)

#####----------Response Time (RT) of Correct Trials----------#####

## remove incorrect trials
Exp2_rt_correct <- combined_df_noTimeOut[combined_df_noTimeOut$accuracy==1,]

## proportion of the removed trials
(nrow(combined_df)-nrow(Exp2_rt_correct))/nrow(combined_df)

## aggregate to get mean response time
TIME <- aggregate(Exp2_rt_correct$time,
                  list(subject=Exp2_rt_correct$subject,
                       rotation=Exp2_rt_correct$rotation,
                       startsym=Exp2_rt_correct$startSym,
                       change=Exp2_rt_correct$change), 
                  mean)

names(TIME)[5] <- "RT"

## merge with spatial ability scores
Exp2_spabRT <- merge(TIME,Exp2_psy,by="subject")%>%
  mutate(SA=(scale(PF,center = T,scale = T)+
               scale(CC,center = T,scale = T))/2)%>%
  mutate(rotation_s=scale(rotation,center=T,scale=T))%>%
  select(subject,rotation_s,startsym,change,RT,SA)

Exp2_spabRT$change <- as.factor(Exp2_spabRT$change)
Exp2_spabRT <- within(Exp2_spabRT,change <- relevel(change,ref="0"))
Exp2_spabRT <- within(Exp2_spabRT,startsym <- relevel(startsym,ref="as"))

#####----------Response Time Linear Mixed Model----------#####

Exp2.lmmRT <- lmer(RT ~ rotation_s*startsym*change+ (1|subject), data = Exp2_spabRT, REML = F)
summary(Exp2.lmmRT)
Anova(Exp2.lmmRT)
confint(Exp2.lmmRT)


#####----------Gender Differences----------#####
Exp2_Gender <- unique(combined_df[,c("subject","sex")])

Exp2_psy_sex <- unique(merge(Exp2_psy,Exp2_Gender,by="subject"))

table(Exp2_psy_sex$sex)

t.test(CC~sex,data=Exp2_psy_sex)
t.test(PP~sex,data=Exp2_psy_sex)
t.test(VR~sex,data=Exp2_psy_sex)
t.test(RAPM~sex,data=Exp2_psy_sex)

lsr::cohensD(CC~sex,data=Exp2_psy_sex)
