## ---------------------------
##
## Script name: Exp2_MainAnalysis
##
## Purpose of script: Experiment 2 data cleaning and EDA and linear mixed model development
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2020-12-22
##
## Email: carol.hcxy@gmail.com
## 
## Content: 
##      1. Load Experiment 2 Data
##      2. Experiment 2 Data Cleaning
##      3. Accuracy Descriptive Statistics
##      4. Response Time Descriptive Statistics
##      5. d' Calculation
##      6. d' Descriptive Statistics
##      7. Bias Calculation
##      8. Bias Descriptive Statistics
##      9. Spatial Ability
##      10. Spatial Ability Descriptive Statistics
##      11. Correlation comparisons
##      12. Linear Regression
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
library(foreign) # read sav data
library(psych) # descriptive statistics
library(lme4) #linear mixed model
library(rstatix) #chi-squared tests for mixed model


## ---------------------------

#####----------Load Experiment 2 Data----------#####

## read txt files
txt_files_ls <-  list.files(
  path=getwd(), 
  pattern = "*.txt")

txt_files_df <- lapply(txt_files_ls, 
                       function(x) 
                         { 
                         tmp <- try(read.table(
                           file = x, 
                           header = TRUE, 
                           sep=";")) 
                         if (!inherits(tmp, 
                                       'try-error')) 
                           tmp
                         }
                       )

## combine all txt files together
combined_df <- do.call("rbind", 
                       lapply(txt_files_df, 
                              as.data.frame)
                       )


#####----------Experiment 2 Data Cleaning----------#####

## finds -99999 values in the accuracy column and sets them to NA so that we can omit them and saving a new data frame
combined_df$accuracy[combined_df$accuracy == -99999] <- 0

## finds timed-out trials and sets them to NA
combined_df$time[combined_df$time == -1] <- NA

## no dual task trials set as NA
combined_df$dualTaskAcc[combined_df$dualTaskAcc == -99] <- NA

## ignore clockwise versus counterclockwise
combined_df$rotation <- with(combined_df, ifelse(rotation <= 0, -rotation, rotation))

## performance on the concurrent verbal task:
v_df <- na.omit(combined_df)  
v_perf <- aggregate(v_df$dualTaskAcc, 
                    list(subject=v_df$subject), 
                    mean)
names(v_perf) [2] <- "VerbTaskAcc"


## performance on the structure detection task:
Exp2_sdt <- aggregate(combined_df$accuracy,
                  list(subject=combined_df$subject,
                       rotation=combined_df$rotation,
                       startsym=combined_df$startSym,
                       change=combined_df$change), 
                  mean)

names(Exp2_sdt)[5] <- "acc"


## poor performance in the concurrent verbal task
v_perf <- v_perf[v_perf$VerbTaskAcc>=0.8,]

## exclude low verb_acc participants 
Exp2_sdt <- Exp2_sdt[Exp2_sdt$subject %in% v_perf$subject,]

#####------Accuracy Descriptive Statistics-----#####

Exp2_descrp <- Exp2_sdt%>%
  group_by(rotation,startsym,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(acc, na.rm = TRUE),
    se = sd(acc, na.rm = TRUE)/sqrt(count)
  )

#####------Response Time Descriptive Statistics-----#####

## remove time-out trials
combined_df<- combined_df[combined_df$subject %in% v_perf$subject,]
combined_df_noTimeOut <- combined_df[complete.cases(combined_df[, "time"]),]


TIME <- aggregate(combined_df_noTimeOut$time,
                  list(subject=combined_df_noTimeOut$subject,
                       rotation=combined_df_noTimeOut$rotation,
                       startsym=combined_df_noTimeOut$startSym,
                       change=combined_df_noTimeOut$change), 
                  mean)

names(TIME)[5] <- "RT"

TIME$startsym <-recode(TIME$startsym,
                       'as'='asymmetrical',
                       'sym'='symmetrical')
TIME$change <-recode(TIME$change,
                     '0' = 'no change',
                     '1' = 'change')
## descriptive statistics
Exp2_RT_summary <- TIME%>%
  group_by(rotation,startsym,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RT, na.rm = TRUE),
    sd = sd(RT, na.rm = TRUE),
    se = sd/sqrt(count)
  )
## visualization - line graph with error bars
#jpeg("Exp2_rt_line.jpeg", width = 8, height = 4.5, units = 'in', res = 300)
ggplot(Exp2_RT_summary,aes(x=rotation,y=mean,color=change,shape=change))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=10,position=position_dodge(1))+
  ylim(0.5,1.5)+
  ylab('response time(s)')+
  scale_x_continuous(name ="rotation",breaks=c(10,60,120),limits=c(0,130))+
  facet_wrap(~startsym)+
  theme_classic(base_size = 20)+
  scale_color_manual(values=c('#999999','#E69F00'))
#dev.off()

#####----------d' Calculation----------#####

## recast Exp2 Structure Change Detection data
re_Exp2_sdt<- 
  recast(Exp2_sdt, 
         subject ~ rotation + startsym + change, 
         id.var = c("subject", "rotation",  "startsym", "change"))

names(re_Exp2_sdt) <- c("subject","r10_as_s","r10_as_d","r10_sym_s","r10_sym_d","r60_as_s","r60_as_d","r60_sym_s","r60_sym_d","r120_as_s","r120_as_d","r120_sym_s","r120_sym_d")

Exp2_fh <- re_Exp2_sdt %>%
  mutate_at(c("r10_as_s","r10_sym_s",
              "r60_as_s","r60_sym_s",
              "r120_as_s","r120_sym_s"),
            list(f=~ifelse(.==1,1/40,1-.)))%>%
  mutate_at(vars(contains('_d')),
            list(h=~ifelse(.==1,1-(1/40),.)))%>%
  mutate_at(vars(contains('_f'),contains('_h')),
            list(z=~qnorm(.)))

Exp2_fh <- Exp2_fh%>%
  mutate(r10.as.dp=r10_as_d_h_z-r10_as_s_f_z)%>%
  mutate(r10.sym.dp=r10_sym_d_h_z-r10_sym_s_f_z)%>%
  mutate(r60.as.dp=r60_as_d_h_z-r60_as_s_f_z)%>%
  mutate(r60.sym.dp=r60_sym_d_h_z-r60_sym_s_f_z)%>%
  mutate(r120.as.dp=r120_as_d_h_z-r120_as_s_f_z)%>%
  mutate(r120.sym.dp=r120_sym_d_h_z-r120_sym_s_f_z)

Exp2_dpr <- Exp2_fh%>%
  select(subject,contains('dp'))

## wide to long
Exp2_dpr_long <- reshape(Exp2_dpr,
                         direction = 'long',
                         idvar = 'subject',
                         varying = c(2:7),
                         timevar='rotation',
                         times=c('r10', 'r60','r120'),
                         v.names=c('as.dp', 'sym.dp'))

Exp2_dpr_long <- gather(Exp2_dpr_long,
                        symmetry,
                        dp,
                        as.dp:sym.dp,
                        factor_key = T)

Exp2_dpr_long$rotation <- as.factor(Exp2_dpr_long$rotation)
Exp2_dpr_long$subject <- as.factor(Exp2_dpr_long$subject)
Exp2_dpr_long$symmetry <-
  recode(Exp2_dpr_long$symmetry,
         'as.dp'='asymmetrical',
         'sym.dp'='symmetrical')
Exp2_dpr_long$rotation <-
  recode(Exp2_dpr_long$rotation,
         'r10'='10-degree',
         'r60'='60-degree',
         'r120'='120-degree')

Exp2_dpr_long <-Exp2_dpr_long%>%
  mutate(rotation = forcats::fct_relevel(rotation, c("10-degree", "60-degree", "120-degree")))%>%
  mutate(rotation_num = dplyr::case_when(
    rotation == '10-degree'  ~ 10,
    rotation == '60-degree'  ~ 60,
    rotation == '120-degree'  ~ 120))

#####--------d' Descriptive Statistics--------#####

Exp2_dpr_summary <- Exp2_dpr_long%>%
  group_by(rotation_num, symmetry) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(dp, na.rm = TRUE),
    se = sd(dp, na.rm = TRUE)/sqrt(count)
  )

## visualize dprime by symmetry and rotation
#jpeg("Exp2_dpr_line.jpeg", width = 8, height = 4.5, units = 'in', res = 300)
ggplot(Exp2_dpr_summary,aes(x=rotation_num,y=mean,color=symmetry,shape=symmetry))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=10,position=position_dodge(0))+
  ylim(0,4)+
  ylab('d\'')+
  scale_x_continuous(name ="rotation",breaks=c(10,60,120),limits=c(0,130))+
  theme_classic(base_size = 20)+
  scale_color_manual(values=c('#999999','#E69F00'))
#dev.off()

#####----------Bias Calculation----------#####

Exp2_bias <- Exp2_fh %>%
  mutate(r10.as.b=exp(-1*(r10_as_d_h_z^2-r10_as_s_f_z^2)*0.5))%>%
  mutate(r10.sym.b=exp(-1*(r10_sym_d_h_z^2-r10_sym_s_f_z^2)*0.5))%>%
  mutate(r60.as.b=exp(-1*(r60_as_d_h_z^2-r60_as_s_f_z^2)*0.5))%>%
  mutate(r60.sym.b=exp(-1*(r60_sym_d_h_z^2-r60_sym_s_f_z^2)*0.5))%>%
  mutate(r120.as.b=exp(-1*(r120_as_d_h_z^2-r120_as_s_f_z^2)*0.5))%>%
  mutate(r120.sym.b=exp(-1*(r120_sym_d_h_z^2-r120_sym_s_f_z^2)*0.5))%>%
  select(subject,r10.as.b,r10.sym.b,r60.as.b,r60.sym.b,r120.as.b,r120.sym.b)

## wide to long
Exp2_bias_long <- reshape(Exp2_bias,
                          direction = 'long',
                          idvar = 'subject',
                          varying = c(2:7),
                          timevar='rotation',
                          times=c('r10', 'r60','r120'),
                          v.names=c('as.b', 'sym.b'))
Exp2_bias_long <- gather(Exp2_bias_long,
                         symmetry,
                         bias,
                         as.b:sym.b,
                         factor_key = T)

Exp2_bias_long$rotation <- as.factor(Exp2_bias_long$rotation)
Exp2_bias_long$subject <- as.factor(Exp2_bias_long$subject)
Exp2_bias_long <-Exp2_bias_long%>%
  mutate(rotation = 
           forcats::fct_relevel(rotation, 
                                c("r10", "r60", "r120")))

#####----------Bias Descriptive Statistics----------#####

Exp2_bias_long%>%
  group_by(rotation, symmetry) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(bias, na.rm = TRUE),
    se = sd(bias, na.rm = TRUE)/sqrt(count)
  )

#####----------Spatial Ability----------#####

Exp2 <- read.spss('Exp2.sav',to.data.frame = T)
Exp2_psy <- Exp2%>%
  select(subject,PF,CC,VR,RAPM)

## merge spatial ability score with structure detection score
Exp2_psy <- re_Exp2_sdt%>%
  merge(Exp2_psy,.,by="subject")%>%
  mutate(all.Acc.s=rowSums(.[c("r10_sym_s","r60_sym_s","r120_sym_s","r10_as_s","r60_as_s","r120_as_s")])/6)%>%
  mutate(all.Acc.d=rowSums(.[c("r10_sym_d","r60_sym_d","r120_sym_d","r10_as_d","r60_as_d","r120_as_d")])/6)%>%
  mutate(sym.Acc.s=rowSums(.[c("r10_sym_s","r60_sym_s","r120_sym_s")])/3)%>%
  mutate(sym.Acc.d=rowSums(.[c("r10_sym_d","r60_sym_d","r120_sym_d")])/3)%>%
  mutate(as.Acc.s=rowSums(.[c("r10_as_s","r60_as_s","r120_as_s")])/3)%>%
  mutate(as.Acc.d=rowSums(.[c("r10_as_d","r60_as_d","r120_as_d")])/3)%>%
  mutate(all.Acc.f_z=ifelse(all.Acc.s==1,qnorm(1/240),qnorm(1-all.Acc.s)))%>%
  mutate(all.Acc.h_z=ifelse(all.Acc.d==1,qnorm(1-1/240),qnorm(all.Acc.d)))%>%
  mutate(sym.Acc.f_z=ifelse(sym.Acc.s==1,qnorm(1/120),qnorm(1-sym.Acc.s)))%>%
  mutate(sym.Acc.h_z=ifelse(sym.Acc.d==1,qnorm(1-1/120),qnorm(sym.Acc.d)))%>%
  mutate(as.Acc.f_z=ifelse(as.Acc.s==1,qnorm(1/120),qnorm(1-as.Acc.s)))%>%
  mutate(as.Acc.h_z=ifelse(as.Acc.d==1,qnorm(1-1/120),qnorm(as.Acc.d)))%>%
  mutate(all.Dp=all.Acc.h_z-all.Acc.f_z)%>%
  mutate(sym.Dp=sym.Acc.h_z-sym.Acc.f_z)%>%
  mutate(as.Dp=as.Acc.h_z-as.Acc.f_z)%>%
  select(subject,all.Dp,sym.Dp,as.Dp,PF,CC,VR,RAPM)

#####--Spatial Ability Descriptive Statistics---#####

psych::describe(Exp2_psy[,-1])

## correlations between different spatial measures
cor.test(Exp2_psy$sym.Dp,Exp2_psy$PF)
cor.test(Exp2_psy$as.Dp,Exp2_psy$PF)
cor.test(Exp2_psy$sym.Dp,Exp2_psy$CC)
cor.test(Exp2_psy$as.Dp,Exp2_psy$CC)

#####----------Correlation Comparisons----------#####
diff.corr <- function( r1, n1, r2, n2 ){
  
  Z1 <- 0.5 * log( (1+r1)/(1-r1) )
  Z2 <- 0.5 * log( (1+r2)/(1-r2) )
  
  diff   <- Z1 - Z2
  SEdiff <- sqrt( 1/(n1 - 3) + 1/(n2 - 3) )
  diff.Z  <- diff/SEdiff
  cat( "diff.Z", diff.Z , "\n" )
  
  p <- 2*pnorm( abs(diff.Z), lower=F)
  cat( "Two-tailed p-value", p , "\n" )
}

diff.corr( r1=0.41, n1=42, r2=0.25, n2=42)


#####----------Linear Regression----------#####

## data preparation
Exp2_spab <- merge(Exp2_psy,
                   Exp2_dpr_long,
                   by="subject")%>%
  mutate(SA=(scale(PF,center = T,scale = T)+
              scale(CC,center = T,scale = T))/2)%>%
  mutate(rotation_s=scale(rotation_num,
                          center=T,scale=T))%>%
  mutate(VR_s=scale(VR,center=TRUE,scale=TRUE))%>%
  mutate(RAPM_s=scale(RAPM,center=TRUE,scale=TRUE))%>%
  select(subject,rotation_s,symmetry,dp,SA,VR_s,RAPM_s)

Exp2_spab$subject <- as.factor(Exp2_spab$subject)
Exp2_spab$symmetry <- as.factor(Exp2_spab$symmetry)
Exp2_spab$symmetry <- relevel(Exp2_spab$symmetry,ref= 'asymmetrical')

## linear mixed model (subject as a random factor)

Exp2.lmm1 <- lmer(dp ~ 
                    rotation_s * symmetry+
                    SA+SA:symmetry+SA:rotation_s+
                    VR_s+RAPM_s+
                    (1|subject), 
                  data = Exp2_spab, 
                  REML = F)

summary(Exp2.lmm1)
Anova(Exp2.lmm1)
confint(Exp2.lmm1)

## random slope model
Exp2.lmm2 <- lmer(dp ~ rotation_s+symmetry+ (1+symmetry|subject)+rotation_s:symmetry, data = Exp2_spab, REML = F)
summary(Exp2.lmm2)
Anova(Exp2.lmm2)

cor.test(coef(Exp2.lmm2)[[1]]$symmetrysymmetrical,
         Exp2_psy$PF)
cor.test(coef(Exp2.lmm2)[[1]]$symmetrysymmetrical,
         Exp2_psy$CC)
cor.test(coef(Exp2.lmm2)[[1]]$symmetrysymmetrical,
         Exp2_psy$VR)
cor.test(coef(Exp2.lmm2)[[1]]$symmetrysymmetrical,
         Exp2_psy$RAPM)




