## ---------------------------
##
## Script name: Exp1_MainAnalysis
##
## Purpose of script: Experiment 1 data cleaning and EDA and linear mixed model development and evaluation
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2020-12-20 / Major Updated: 2022-02-10
##
## Email: carol.hcxy@gmail.com
## 
## Content: 
##      1. Power Analysis - Simulation
##      2. Load Experiment 1 Data
##      3. Experiment 1 Data Cleaning
##      4. Accuracy Descriptive Statistics
##      5. Response Time Descriptive Statistics
##      6. d' Calculation
##      7. d' Descriptive Statistics
##      8. Bias Calculation
##      9. Bias Descriptive Statistics
##      10. Spatial Ability
##      11. Spatial Ability Descriptive Statistics
##      12. Linear Regression
##      13. Bayes Factor
## ---------------------------


## ---------------------------

## set working directory for Mac and PC

setwd("~/CarolHe/")      # Carol's working directory (mac)
setwd("C:/Users/carolhe/")    # if using PC

## ---------------------------

## load up the packages
## use install.packages() to install new packages
library(tidyverse)
library(simr) #power analysis
library(reshape2)
library(foreign) # read sav data
library(psych) # descriptive statistics
library(splithalf) #reliability
library(lme4) #linear mixed model
library(rstatix) #chi-squared tests for mixed model
library(effectsize) # effect size for mixed model
library(BayesFactor)



## ---------------------------
## ---------------------------


#####----------Power Analysis----------#####
set.seed(7)
subj <- factor(1:42)
with_group1 <- c("sym","asym")
with_group2 <- c(-1.18,0.07,1.25)
obs_cov <- rnorm(42, 0, 1) 

subj_full <- rep(subj,6)
spatial_full <- rep(obs_cov,6)
wit_grp1_full <- rep(rep(with_group1,each=42),3)
wit_grp2_full <- rep(rep(with_group2,each=84),1)
covars <- data.frame(id=subj_full, sa = spatial_full,symmetry=wit_grp1_full,rotation=wit_grp2_full)

## Random intercepts for participants
V2 <- list(0.3)
## residual variance
res <- 0.3
#assume effect size d=.2 (small effect size partial 0.01)
d <- .2
print(paste("partial eta square: ",d^2/(d^2+4)))

## Intercept and slopes for symmetry, rotation, sym:rot
coeffi <- d/sqrt(sum(res,V2[[1]]))
fixed <- c(1.5,coeffi,coeffi,coeffi,coeffi)

model <- makeLmer(y ~ symmetry*rotation + sa + (1|id), fixef=fixed, VarCorr=V2, sigma=res, data=covars)
model

sim_interaction <- powerSim(model, nsim=100, test = fcompare(y~rotation+symmetry + sa + (1|id)))
sim_interaction

sim_sa <- powerSim(model, nsim=100, test = fcompare(y~rotation*symmetry + (1|id)))
sim_sa

#####----------Load Experiment 1 Data----------#####

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
Exp1_RT_summary <- TIME%>%
  group_by(rotation,startsym,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RT, na.rm = TRUE),
    sd = sd(RT, na.rm = TRUE),
    se = sd/sqrt(count)
  )
## visualization - line graph with error bars
#jpeg("Exp1_rt_line.jpeg", width = 8, height = 4.5, units = 'in', res = 300)
ggplot(Exp1_RT_summary,aes(x=rotation,y=mean,color=change,shape=change))+
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

## recast Exp1 Structure Change Detection data
re_Exp1_sdt<- 
  recast(Exp1_sdt, 
         subject ~ rotation + startsym + change, 
         id.var = c("subject", "rotation",  "startsym", "change"))

names(re_Exp1_sdt) <- c("subject","r10_as_s","r10_as_d","r10_sym_s","r10_sym_d","r60_as_s","r60_as_d","r60_sym_s","r60_sym_d","r120_as_s","r120_as_d","r120_sym_s","r120_sym_d")

Exp1_fh <- re_Exp1_sdt %>%
  mutate_at(c("r10_as_s","r10_sym_s",
              "r60_as_s","r60_sym_s",
              "r120_as_s","r120_sym_s"),
            list(f=~ifelse(.==1,1/40,1-.)))%>%
  mutate_at(vars(contains('_d')),
            list(h=~ifelse(.==1,1-(1/40),.)))%>%
  mutate_at(vars(contains('_f'),contains('_h')),
            list(z=~qnorm(.)))

Exp1_fh <- Exp1_fh%>%
  mutate(r10.as.dp=r10_as_d_h_z-r10_as_s_f_z)%>%
  mutate(r10.sym.dp=r10_sym_d_h_z-r10_sym_s_f_z)%>%
  mutate(r60.as.dp=r60_as_d_h_z-r60_as_s_f_z)%>%
  mutate(r60.sym.dp=r60_sym_d_h_z-r60_sym_s_f_z)%>%
  mutate(r120.as.dp=r120_as_d_h_z-r120_as_s_f_z)%>%
  mutate(r120.sym.dp=r120_sym_d_h_z-r120_sym_s_f_z)

Exp1_dpr <- Exp1_fh%>%
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

#####----------Bias Calculation----------#####

Exp1_bias <- Exp1_fh %>%
  mutate(r10.as.b=exp(-1*(r10_as_d_h_z^2-r10_as_s_f_z^2)*0.5))%>%
  mutate(r10.sym.b=exp(-1*(r10_sym_d_h_z^2-r10_sym_s_f_z^2)*0.5))%>%
  mutate(r60.as.b=exp(-1*(r60_as_d_h_z^2-r60_as_s_f_z^2)*0.5))%>%
  mutate(r60.sym.b=exp(-1*(r60_sym_d_h_z^2-r60_sym_s_f_z^2)*0.5))%>%
  mutate(r120.as.b=exp(-1*(r120_as_d_h_z^2-r120_as_s_f_z^2)*0.5))%>%
  mutate(r120.sym.b=exp(-1*(r120_sym_d_h_z^2-r120_sym_s_f_z^2)*0.5))%>%
  select(subject,r10.as.b,r10.sym.b,r60.as.b,r60.sym.b,r120.as.b,r120.sym.b)

## wide to long
Exp1_bias_long <- reshape(Exp1_bias,
                          direction = 'long',
                          idvar = 'subject',
                          varying = c(2:7),
                          timevar='rotation',
                          times=c('r10', 'r60','r120'),
                          v.names=c('as.b', 'sym.b'))
Exp1_bias_long <- gather(Exp1_bias_long,
                         symmetry,
                         bias,
                         as.b:sym.b,
                         factor_key = T)

Exp1_bias_long$rotation <- as.factor(Exp1_bias_long$rotation)
Exp1_bias_long$subject <- as.factor(Exp1_bias_long$subject)
Exp1_bias_long <-Exp1_bias_long%>%
  mutate(rotation = 
           forcats::fct_relevel(rotation, 
                                c("r10", "r60", "r120")))

#####----------Bias Descriptive Statistics----------#####

Exp1_bias_long%>%
  group_by(rotation, symmetry) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(bias, na.rm = TRUE),
    se = sd(bias, na.rm = TRUE)/sqrt(count)
  )

#####----------Spatial Ability----------#####
#####-------correct answers------#####

########## paper folding
pp_correct <- c('a','d','b','d','b',
                'e','a','c','e','e',
                'c','b','a','e','b',
                'a','e','d','d','c')

########## cube comparisons
cc_correct <- c('d','d','d','s','d',
                's','s','s','s','d',
                'd','s','s','s','s',
                's','d','d','d','d',
                'd','s','d','s','s',
                'd','s','s','d','s',
                'd','d','s','d','d',
                's','d','d','d','s',
                'd','d')

########## verbal reasoning
vr_correct <- c('a','h','c','f','c',
                'k','c','g','b','j',
                'a','g','e','g','d',
                'j','e','j','b','k',
                'a','g','e','j','c',
                'k','c','g','c','h',
                'd','g','b','k','c',
                'j','c','j','e','f')

########## scoring functions
scoring <- function(ans,rubrics,penalty){
  score = NULL
  score[1] = ans[1]
  for (i in c(1:(length(ans)-1))){
    score[i+1] = ifelse(ans[i+1]==0,0,
                        ifelse(ans[i+1]== rubrics[i],1,penalty))
    
  }
  return(score)
}
########## valid total score for each participant
valid_score <- function(data,valid_participants){
  data$V1 <- as.numeric((data$V1))
  data_valid <- data[data$V1%in%valid_participants$subject,]
  data_valid <- as.data.frame(apply(data_valid,2,as.numeric))
  data_valid['total'] <- apply(data_valid[,c(2:length(data))],1,sum)
  return(data_valid)
}

########## paper folding
##########>>>>> data reading and scoring
C1PP<- read.csv("3C1PP.csv")
C1PP[C1PP=='']<- 0

test <- as.data.frame(t(apply(C1PP,1,scoring,rubrics=pp_correct,penalty=-0.25)))
data_valid <- valid_score(test,valid_participants = v_perf)

##########>>>>> descriptive stats
Exp1_pp_tot <- data_valid[,c(1,22)]
psych::describe(Exp1_pp_tot$total)

##########>>>>> permutation based split-half reliability
data_valid_long <- gather(data_valid[,c(1:21)], trialID, score, V2:V21, factor_key=TRUE)
colnames(data_valid_long)[1] <- 'subject'

set.seed(123)
splithalf(data=data_valid_long,
          outcome = "accuracy",
          score = "average",
          halftype = "random",
          permutations = 5000,
          var.ACC = "score",
          var.trialnum = "trialID",
          var.participant = "subject",
          average="mean")

########## cube comparisons
##########>>>>> data reading and scoring
C1CC<- read.csv("3C1CC.csv")
C1CC[C1CC=='']<- 0

test <- as.data.frame(t(apply(C1CC,1,scoring,rubrics=cc_correct,penalty=-1)))
data_valid <- valid_score(test,valid_participants = v_perf)

##########>>>>> descriptive stats
Exp1_cc_tot <- data_valid[,c(1,44)]
psych::describe(Exp1_cc_tot$total)

##########>>>>> permutation based split-half reliability
data_valid_long <- gather(data_valid[,c(1:43)], trialID, score, V2:V43, factor_key=TRUE)
colnames(data_valid_long)[1] <- 'subject'

splithalf(data=data_valid_long,
          outcome = "accuracy",
          score = "average",
          halftype = "random",
          permutations = 5000,
          var.ACC = "score",
          var.trialnum = "trialID",
          var.participant = "subject",
          average="mean")

Exp1_psy <- Exp1_pp_tot%>%
  merge(.,Exp1_cc_tot,by='V1')
colnames(Exp1_psy) <- c('subject','PF','CC')
cor.test(Exp1_psy$PF,Exp1_psy$CC)

#####----------Linear Regression----------#####

Exp1_data <- combined_df

Exp1_RT_indiv <- Exp1_data[Exp1_data$accuracy==1,]%>%
  group_by(subject) %>%
  dplyr::summarise(
    count = n(),
    RTmean = mean(time, na.rm = TRUE),
    RTsd = sd(time, na.rm = TRUE),
  )

Exp1_indiv <- merge(Exp1_RT_indiv,Exp1_psy,by="subject")%>%
  mutate(SA=(scale(PF,center = T,scale = T)+
               scale(CC,center = T,scale = T))/2)%>%
  select(subject,RTmean,RTsd,SA)
  

## data preparation
Exp1_spab <- merge(Exp1_indiv,
                   Exp1_dpr_long,
                   by="subject")%>%
  mutate(rotation_s=scale(rotation_num,
                          center=T,scale=T))%>%
  select(subject,rotation_s,rotation_num,symmetry,dp,SA)

Exp1_spab$subject <- as.factor(Exp1_spab$subject)
Exp1_spab$symmetry <- as.factor(Exp1_spab$symmetry)
Exp1_spab$symmetry <- relevel(Exp1_spab$symmetry,ref= 'asymmetrical')

## linear mixed model (subject as a random factor)

#(link)[https://cran.r-project.org/web/packages/effectsize/vignettes/from_test_statistics.html]

Exp1.lmm <- lmer(dp ~ 
                    rotation_s * symmetry+SA+SA:symmetry+SA:rotation_s+
                    ((rotation_s * symmetry)|subject), 
                  data = Exp1_spab, 
                  REML = F)
summary(Exp1.lmm)
effectsize::eta_squared(Exp1.lmm)
Anova(Exp1.lmm)

#####----------Dprime Bayes Factor----------#####

set.seed(123)
bf1 = lmBF(dp ~ rotation_s * symmetry+
               SA+SA:symmetry+SA:rotation_s+subject, data=Exp1_spab, whichRandom = "subject")
bf2 = lmBF(dp ~ rotation_s * symmetry+
             SA+SA:rotation_s+subject, data=Exp1_spab, whichRandom = "subject")

bf3 = lmBF(dp ~ rotation_s * symmetry+
             SA+SA:symmetry+subject, data=Exp1_spab, whichRandom = "subject")
bf1/bf2
bf1/bf3


