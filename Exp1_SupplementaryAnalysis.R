## ---------------------------
##
## Script name: Exp1_SupplementaryAnalysis
##
## Purpose of script: Based on the main analysis, other analyses done for Experiment 1. (need to run main analysis first)
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2020-12-21 / Major Update: 2022 -02-10
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

#####----------Mental Rotation Analysis----------#####

## read MRT scores:
Exp1_MRT <- read.spss('Exp1.sav',to.data.frame = T)%>%
  select(subject,MRT_M,MRT_O)

## combined with other spatial measures
Exp1_psy <- merge(Exp1_psy,Exp1_MRT, by = "subject")

## combined with overall d'
re_Exp1_sdt_2<- 
  recast(Exp1_sdt[,c("subject","change","acc")], 
         subject ~ change, 
         id.var = c("subject","change"),
         measure.var = "acc",
         fun.agg = mean)

names(re_Exp1_sdt_2) <- c("subject","same","diff")

re_Exp1_sdt_2$f<- ifelse(re_Exp1_sdt_2$same==1,1/120,1-re_Exp1_sdt_2$same)
re_Exp1_sdt_2$h <- ifelse(re_Exp1_sdt_2$diff==1,1-(1/120),re_Exp1_sdt_2$diff)

Exp1_all <- re_Exp1_sdt_2%>%
  mutate_at(vars(contains('f'),contains('h')),
            list(z=~qnorm(.)))%>%
  mutate(dp = h_z-f_z)%>%
  select(subject,dp)%>%
  merge(.,Exp1_psy,by="subject")

## correlations between all measures
cor(select(Exp1_all,-subject))
cor.test(Exp1_all$dp,Exp1_all$PF)
cor.test(Exp1_all$PF,Exp1_all$MRT_M)
cor.test(Exp1_all$CC,Exp1_all$MRT_M)
cor.test(Exp1_all$CC,Exp1_all$PF)
cor.test(Exp1_all$MRT_M,Exp1_psy$MRT_O)
## combining two MRT tasks
Exp1_all$MRT <- (scale(Exp1_all$MRT_M,center = T, scale = T) + scale(Exp1_all$MRT_O,center = T, scale = T))/2


########gender differences

## get gender information
Exp1_Gender <- unique(combined_df[,c("subject","sex")])
Exp1_all <- unique(merge(Exp1_all,Exp1_Gender,by="subject"))

## gender distribution
table(Exp1_all$sex)

## test gender differences
t.test(PF~sex,data=Exp1_all)
t.test(CC~sex,data=Exp1_all)
t.test(MRT~sex,data=Exp1_all)
t.test(dp~sex, data=Exp1_all)

########linear mixed model with MRT
Exp1_spab_2 <- merge(select(Exp1_all,-dp),Exp1_dpr_long,by="subject")%>%
  mutate(SA=(scale(PF,center = T,scale = T)+
               scale(CC,center = T,scale = T)+MRT)/3)%>%
  mutate(rotation_s=scale(rotation_num,center=T,scale=T))%>%
  select(subject,rotation_s,symmetry,dp,SA)

Exp1_spab_2$subject <- as.factor(Exp1_spab_2$subject)

Exp1.lmm2 <- lmer(dp ~ 
                   rotation_s * symmetry+SA+SA:symmetry+SA:rotation_s+
                   ((rotation_s * symmetry)|subject), 
                 data = Exp1_spab_2, 
                 REML = F)
summary(Exp1.lmm2)
effectsize::eta_squared(Exp1.lmm2)
Anova(Exp1.lmm2)

#####----------Linear Mixed Model with separated PF/CC/MRT----------#####

Exp1_spab_3 <- merge(select(Exp1_all,-dp),Exp1_dpr_long,by="subject")%>%
  mutate(rotation_s=scale(rotation_num,center=T,scale=T))%>%
  mutate(PF_s=scale(PF,center=T,scale=T))%>%
  mutate(CC_s=scale(CC,center=T,scale=T))%>%
  select(subject,rotation_s,symmetry,dp,PF_s,CC_s,MRT)

Exp1_spab_3$subject <- as.factor(Exp1_spab_3$subject)


Exp1.lmm3 <- lmer(dp ~ 
                    rotation_s * symmetry+PF_s+CC_s+
                    ((rotation_s * symmetry)|subject), 
                  data = Exp1_spab_3, 
                  REML = F)
summary(Exp1.lmm3)
effectsize::eta_squared(Exp1.lmm3)
Anova(Exp1.lmm3)

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
Exp1_spabRT <- merge(TIME,Exp1_all,by="subject")%>%
  mutate(SA=(scale(PF,center = T,scale = T)+
               scale(CC,center = T,scale = T))/2)%>%
  mutate(rotation_s=scale(rotation,center=T,scale=T))%>%
  select(subject,rotation_s,startsym,change,RT,SA)


Exp1_spabRT$change <- as.factor(Exp1_spabRT$change)
Exp1_spabRT <- within(Exp1_spabRT,change <- relevel(change,ref="0"))
Exp1_spabRT <- within(Exp1_spabRT,startsym <- relevel(startsym,ref="as"))

#####----------Response Time Linear Mixed Model----------#####

Exp1.lmmRT <- lmer(RT ~ rotation_s*startsym*change+SA+ ((rotation_s*startsym*change)|subject), data = Exp1_spabRT, REML = F)
summary(Exp1.lmmRT)
effectsize::eta_squared(Exp1.lmmRT)
Anova(Exp1.lmmRT)



