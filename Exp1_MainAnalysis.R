## ---------------------------
##
## Script name: Symmetry_r_script
##
## Purpose of script: 
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2020-12-20
##
## Email: carol.hcxy@gmail.com
##
## ---------------------------


## ---------------------------

## set working directory for Mac and PC

setwd("~/CarolHe/")      # Carol's working directory (mac)
setwd("C:/Users/carolhe/")    # Tim's working directory (PC)

## ---------------------------

## load up the packages we will need:  (uncomment as required)
## if never used the following packages before, use install.packages() to install
library(tidyverse)
library(reshape2)
library(foreign) # read sav data
library(psych) # descriptive statistics
library(lme4) #linear mixed model
library(rstatix)


## ---------------------------

#####----------Load Experiment 1 Data----------#####

## read txt files
txt_files_ls <-  list.files(
  path=getwd(), 
  pattern = "*.txt")

txt_files_df <- lapply(txt_files_ls, 
                       function(x) 
                         { 
                         tmp <- try(read.table(file = x, 
                                               header = TRUE, 
                                               sep=";")) 
                         if (!inherits(tmp, 'try-error')) 
                           tmp
                         }
                       )

## combine all txt files together
combined_df <- do.call("rbind", 
                       lapply(txt_files_df, as.data.frame)
                       )


#####----------Experiment 1 Data Cleaning----------#####

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
Exp1_sdt <- aggregate(combined_df$accuracy,
                  list(subject=combined_df$subject,
                       rotation=combined_df$rotation,
                       startsym=combined_df$startSym,
                       change=combined_df$change), 
                  mean)

names(Exp1_sdt)[5] <- "acc"


## poor performance in the concurrent verbal task
v_perf <- v_perf[v_perf$VerbTaskAcc>=0.8,]

## exclude low verb_acc participants 
Exp1_sdt <- Exp1_sdt[Exp1_sdt$subject %in% v_perf$subject,]

#####------Accuracy Descriptive Statistics-----#####

Exp1_descrp <- Exp1_sdt%>%
  group_by(rotation,startsym,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(acc, na.rm = TRUE),
    se = sd(acc, na.rm = TRUE)/sqrt(count)
  )

#####----------d' calculation----------#####

## recast Exp1 Structure Change Detection data
re_Exp1_sdt<- 
  recast(Exp1_sdt, 
         subject ~ rotation + startsym + change, 
         id.var = c("subject", "rotation",  "startsym", "change"))

names(re_Exp1_sdt) <- c("subject","r10_as_s","r10_as_d","r10_sym_s","r10_sym_d","r60_as_s","r60_as_d","r60_sym_s","r60_sym_d","r120_as_s","r120_as_d","r120_sym_s","r120_sym_d")

Exp1_dpr <- re_Exp1_sdt %>%
  mutate_at(c("r10_as_s","r10_sym_s",
              "r60_as_s","r60_sym_s",
              "r120_as_s","r120_sym_s"),
            list(f=~ifelse(.==1,1/40,1-.)))%>%
  mutate_at(vars(contains('_d')),
            list(h=~ifelse(.==1,1-(1/40),.)))%>%
  mutate_at(vars(contains('_f'),contains('_h')),
            list(z=~qnorm(.)))

Exp1_dpr <- Exp1_dpr%>%
  mutate(r10.as.dp=r10_as_d_h_z-r10_as_s_f_z)%>%
  mutate(r10.sym.dp=r10_sym_d_h_z-r10_sym_s_f_z)%>%
  mutate(r60.as.dp=r60_as_d_h_z-r60_as_s_f_z)%>%
  mutate(r60.sym.dp=r60_sym_d_h_z-r60_sym_s_f_z)%>%
  mutate(r120.as.dp=r120_as_d_h_z-r120_as_s_f_z)%>%
  mutate(r120.sym.dp=r120_sym_d_h_z-r120_sym_s_f_z)%>%
  select(subject,contains('dp'))

## wide to long
Exp1_dpr_long <- reshape(Exp1_dpr,
                         direction = 'long',
                         idvar = 'subject',
                         varying = c(2:7),
                         timevar='rotation',
                         times=c('r10', 'r60','r120'),
                         v.names=c('as.dp', 'sym.dp'))

Exp1_dpr_long <- gather(Exp1_dpr_long,
                        symmetry,
                        dp,
                        as.dp:sym.dp,
                        factor_key = T)

Exp1_dpr_long$rotation <- as.factor(Exp1_dpr_long$rotation)
Exp1_dpr_long$subject <- as.factor(Exp1_dpr_long$subject)
Exp1_dpr_long$symmetry <-
  recode(Exp1_dpr_long$symmetry,
         'as.dp'='asymmetrical',
         'sym.dp'='symmetrical')
Exp1_dpr_long$rotation <-
  recode(Exp1_dpr_long$rotation,
         'r10'='10-degree',
         'r60'='60-degree',
         'r120'='120-degree')

Exp1_dpr_long <-Exp1_dpr_long%>%
  mutate(rotation = forcats::fct_relevel(rotation, c("10-degree", "60-degree", "120-degree")))%>%
  mutate(rotation_num = dplyr::case_when(
    rotation == '10-degree'  ~ 10,
    rotation == '60-degree'  ~ 60,
    rotation == '120-degree'  ~ 120))

#####--------d' Descriptive Statistics--------#####

Exp1_dpr_summary <- Exp1_dpr_long%>%
  group_by(rotation_num, symmetry) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(dp, na.rm = TRUE),
    se = sd(dp, na.rm = TRUE)/sqrt(count)
  )

## visualize dprime by symmetry and rotation
#jpeg("Exp1_dpr_line.jpeg", width = 8, height = 4.5, units = 'in', res = 300)
ggplot(Exp1_dpr_summary,aes(x=rotation_num,y=mean,color=symmetry,shape=symmetry))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=10,position=position_dodge(0))+
  ylim(0,4)+
  ylab('d\'')+
  scale_x_continuous(name ="rotation",breaks=c(10,60,120),limits=c(0,130))+
  theme_classic(base_size = 20)+
  scale_color_manual(values=c('#999999','#E69F00'))
#dev.off()


#####----------Spatial Ability----------#####

Exp1 <- read.spss('Exp1.sav',to.data.frame = T)
Exp1_psy <- Exp1%>%
  select(subject,PF,CC,MRT_M,MRT_O)%>%
  mutate(MRT=MRT_M+MRT_O)

## merge spatial ability score with structure detection score
Exp1_psy <- re_Exp1_sdt%>%
  merge(Exp1_psy,.,by="subject")%>%
  mutate(sym.Acc.s=rowSums(.[c("r10_sym_s","r60_sym_s","r120_sym_s")])/3)%>%
  mutate(sym.Acc.d=rowSums(.[c("r10_sym_d","r60_sym_d","r120_sym_d")])/3)%>%
  mutate(as.Acc.s=rowSums(.[c("r10_as_s","r60_as_s","r120_as_s")])/3)%>%
  mutate(as.Acc.d=rowSums(.[c("r10_as_d","r60_as_d","r120_as_d")])/3)%>%
  mutate(sym.Acc.f_z=ifelse(sym.Acc.s==1,qnorm(1/120),qnorm(1-sym.Acc.s)))%>%
  mutate(sym.Acc.h_z=ifelse(sym.Acc.d==1,qnorm(1-1/120),qnorm(sym.Acc.d)))%>%
  mutate(as.Acc.f_z=ifelse(as.Acc.s==1,qnorm(1/120),qnorm(1-as.Acc.s)))%>%
  mutate(as.Acc.h_z=ifelse(as.Acc.d==1,qnorm(1-1/120),qnorm(as.Acc.d)))%>%
  mutate(sym.Dp=sym.Acc.h_z-sym.Acc.f_z)%>%
  mutate(as.Dp=as.Acc.h_z-as.Acc.f_z)%>%
  select(subject,sym.Dp,as.Dp,PF,CC,MRT_M,MRT_O,MRT)

#####--Spatial Ability Descriptive Statistics---#####

psych::describe(Exp1_psy[,-1])

## correlations between different spatial measures
cor.test(Exp1_psy$MRT_M,Exp1_psy$MRT_O)
cor.test(Exp1_psy$PF,Exp1_psy$MRT)
cor.test(Exp1_psy$CC,Exp1_psy$MRT)

#####----------linear regression----------#####

## data preparation
Exp1_spab <- merge(Exp1_psy,
                   Exp1_dpr_long,
                   by="subject")%>%
  mutate(SA=(scale(PF,center = T,scale = T)+
              scale(CC,center = T,scale = T))/2)%>%
  mutate(rotation_s=scale(rotation_num,
                          center=T,scale=T))%>%
  select(subject,rotation_s,rotation_num,symmetry,dp,SA)

Exp1_spab$subject <- as.factor(Exp1_spab$subject)
Exp1_spab$symmetry <- as.factor(Exp1_spab$symmetry)
Exp1_spab$symmetry <- relevel(Exp1_spab$symmetry,ref= 'symmetrical')

## linear mixed model (subject as a random factor)

Exp1.lmm1 <- lmer(dp ~ 
                    rotation_s * symmetry+
                    SA+SA:symmetry+SA:rotation_s+
                    (1|subject), 
                  data = Exp1_spab, 
                  REML = F)

summary(Exp1.lmm1)
Anova(Exp1.lmm1)
confint(Exp1.lmm1)

## random slope model
Exp1.lmm2 <- lmer(dp ~ rotation_s+symmetry+ (1+symmetry|subject)+rotation_s:symmetry, data = Exp1_spab, REML = F)
summary(Exp1.lmm2)
Anova(Exp1.lmm2)

cor.test(coef(Exp1.lmm2)[[1]]$symmetrysymmetrical,
         Exp1_psy$PF)
cor.test(coef(Exp1.lmm2)[[1]]$symmetrysymmetrical,
         Exp1_psy$CC)
cor.test(coef(Exp1.lmm2)[[1]]$symmetrysymmetrical,
         Exp1_psy$MRT)






