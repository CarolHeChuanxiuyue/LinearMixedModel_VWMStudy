## ---------------------------
##
## Script name: Exp2_SupplementaryAnalysis
##
## Purpose of script: Based on the main analysis, other analyses done for Experiment 2 (need to run main analysis first)
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2020-12-23 / Major Updated: 2022-02-10
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
Exp2.lmm2 <- lmer(dp ~ rotation_s * symmetry+SA+SA:rotation_s+symmetry:SA+ ((rotation_s * symmetry)|subject), data = Exp2_spab)
summary(Exp2.lmm2)

## compare it with the main model reported in the paper
anova(Exp2.lmm2,Exp2.lmm)

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
Exp2_spabRT$startsym <- as.factor(Exp2_spabRT$startsym)
Exp2_spabRT <- within(Exp2_spabRT,change <- relevel(change,ref="0"))
Exp2_spabRT <- within(Exp2_spabRT,startsym <- relevel(startsym,ref="as"))

#####----------Response Time Linear Mixed Model----------#####

Exp2.lmmRT <- lmer(RT ~ rotation_s*startsym*change+SA+
                     ((rotation_s*startsym*change)|subject), 
                   data = Exp2_spabRT, REML = F)
summary(Exp2.lmmRT)
effectsize::eta_squared(Exp2.lmmRT)
Anova(Exp2.lmmRT)



#####----------Gender Differences----------#####
Exp2_Gender <- unique(combined_df[,c("subject","sex")])

Exp2_psy_sex <- unique(merge(Exp2_psy,Exp2_Gender,by="subject"))

table(Exp2_psy_sex$sex)

t.test(CC~sex,data=Exp2_psy_sex)
t.test(PF~sex,data=Exp2_psy_sex)
t.test(VR~sex,data=Exp2_psy_sex)
t.test(RAPM~sex,data=Exp2_psy_sex)

lsr::cohensD(CC~sex,data=Exp2_psy_sex)


#####----------individual differences in all measures----------#####
Exp2_data <- combined_df

ggplot(Exp1_data,aes(x=time))+
  geom_density(data=Exp2_data,aes(x=time),fill="orange",alpha=0.5)+
  labs(x="Response Time in second",y="density")+
  theme_bw(base_size=30)

Exp2_RT_indiv <- Exp2_data%>%
  group_by(subject) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(time, na.rm = TRUE),
    sd = sd(time, na.rm = TRUE),
    se = sd/sqrt(count)
  )

ggplot(Exp1_RT_indiv,aes(x=sd))+
  geom_density(alpha=0.5,fill="blue")+
  geom_density(data=Exp2_RT_indiv,aes(x=sd),fill="orange",alpha=0.5)+
  labs(x="RT-individual standard deviation",y="density")+
  theme_bw(base_size=30)

t.test(Exp1_RT_indiv$sd,Exp2_RT_indiv$sd)
t.test(Exp1_RT_indiv$mean,Exp2_RT_indiv$mean)

ggplot(Exp1_RT_indiv,aes(x=mean))+
  geom_density(alpha=0.5,fill="blue")+
  geom_density(data=Exp2_RT_indiv,aes(x=mean),fill="orange",alpha=0.5)+
  labs(x="RT-individual average",y="density")+
  theme_bw(base_size=30)

psych::describe(Exp2_RT_indiv$mean)

## recast Exp1 Structure Change Detection data
re_Exp2_sdt<- 
  recast(Exp2_sdt, 
         subject ~ change, 
         id.var = c("subject", "change"),
         measure.var = "acc",
         fun.aggregate = mean)

names(re_Exp2_sdt) <- c("subject","same","different")

Exp2_indiv_trait <- re_Exp2_sdt %>%
  mutate_at("same",
            list(false=~ifelse(.==1,1/40,1-.)))%>%
  mutate_at("different",
            list(hit=~ifelse(.==1,1-(1/40),.)))%>%
  mutate_at(vars(contains('false'),contains('hit')),
            list(z=~qnorm(.)))%>%
  mutate(dp = hit_z - false_z)%>%
  select(subject,dp)%>%
  merge(Exp2_indiv,by="subject")

library(PerformanceAnalytics)
chart.Correlation(
  Exp2_indiv_trait[,-1],
  histogram = TRUE,
)

cor.test(Exp2_indiv_trait$SA,Exp2_indiv_trait$RTmean)

#####----------# of practice repeats----------#####

practicerun <- combined_df%>%group_by(subject)%>%
  dplyr::summarise(
    run = mean(nPracticeRuns, na.rm = TRUE),
  )

hist(practicerun$run)
unique(practicerun$run)