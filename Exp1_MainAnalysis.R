## ---------------------------
##
## Script name: Exp1_MainAnalysis
##
## Purpose of script: Experiment 1 data cleaning and EDA and linear mixed model development
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2020-12-20
##
## Email: carol.hcxy@gmail.com
## 
## Content: 
##      1. Load Experiment 1 Data
##      2. Experiment 1 Data Cleaning
##      3. Accuracy Descriptive Statistics
##      4. Response Time Descriptive Statistics
##      5. d' Calculation
##      6. d' Descriptive Statistics
##      7. Bias Calculation
##      8. Bias Descriptive Statistics
##      9. Spatial Ability
##      10. Spatial Ability Descriptive Statistics
##      11. Linear Regression
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

Exp1 <- read.spss('Exp1.sav',to.data.frame = T)
Exp1_psy <- Exp1%>%
  select(subject,PF,CC,MRT_M,MRT_O)%>%
  mutate(MRT=MRT_M+MRT_O)

##new:
Exp1_psy <- re_Exp1_sdt%>%
  merge(df1_c,.,by="subject")%>%
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
  select(subject,sym.Dp,as.Dp,pf_score,PF_SAV,cc_score,CC_SAV)

##new^

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

#####----------Linear Regression----------#####

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
Exp1_spab$symmetry <- relevel(Exp1_spab$symmetry,ref= 'asymmetrical')

## linear mixed model (subject as a random factor)

#(link)[https://cran.r-project.org/web/packages/effectsize/vignettes/from_test_statistics.html]


Exp1.lmm1 <- lmer(dp ~ 
                    rotation_s * symmetry+
                    SA+SA:symmetry+SA:rotation_s+
                    (1|subject), 
                  data = Exp1_spab, 
                  REML = F)
summary()

Exp1.lmmr <- lmer(dp ~ 
                    rotation_s * symmetry+
                    SA+SA:symmetry+SA:rotation_s+SA:symmetry:rotation_s+
                    (1|subject), 
                  data = Exp1_spab, 
                  REML = F)
summary(Exp1.lmmr)

Exp1_spab['low_SA'] = Exp1_spab$SA<median(Exp1_spab$SA)
Exp1.lmmr <- lmer(dp ~ 
                    rotation_s * symmetry+
                    low_SA+low_SA:symmetry+low_SA:rotation_s+
                    (rotation_s*symmetry|subject), 
                  data = Exp1_spab, 
                  REML = F)
summary(Exp1.lmmr)
Anova(Exp1.lmmr, type = 3)

install.packages("afex")
library(afex)

aov_fit <- aov_car(dp ~ rotation_s * symmetry+
                     SA+SA:symmetry+SA:rotation_s + Error(subject/(rotation_s*symmetry)),
                   data = Exp1_spab,observed = c("SA"),factorize = FALSE,
                   anova_table = list(correction = "none", es = "pes")
)
aov_fit
eta_squared(aov_fit)
head(Exp1_spab)
Exp1_spab$rotation_num <- as.factor(Exp1_spab$rotation_num)
aov_fit4 <- aov_4(dp ~ rotation_num * symmetry+
        SA+SA:symmetry+SA:rotation_num + ((rotation_num * symmetry)|subject),
      data = Exp1_spab,observed = c("SA"),factorize = FALSE)
aov_fit4
eta_squared(aov_fit4)

#####----------Dprime Bayes Factor----------#####
library(BayesFactor)
bf = lmBF(dp ~ rotation_s*symmetry+subject, data=Exp1_spab, whichRandom="subject")
bf
plot(bf)


bf1 = lmBF(dp ~ rotation_s * symmetry+
               SA+SA:symmetry+SA:rotation_s+subject, data=Exp1_spab, whichRandom = "subject")
bf2 = lmBF(dp ~ rotation_s * symmetry+
             SA+SA:rotation_s+subject, data=Exp1_spab, whichRandom = "subject")
bf3 = lmBF(dp ~ rotation_s * symmetry+
             SA+SA:symmetry+subject, data=Exp1_spab, whichRandom = "subject")
bf2/bf1
bf3/bf1



library(effectsize)
effectsize::F_to_eta2(
  f = aov_fit$anova_table$F,
  df = aov_fit$anova_table$`num Df`,
  df_error = aov_fit$anova_table$`den Df`
)

powerSim(aov_fit,fixed("SA:symmetry","lr"),nsim=1000)
powerSim(aov_fit,fixed("SA:rotation_s","lr"),nsim=1000)
powerSim(aov_fit,fixed("SA","lr"),nsim=1000)
powerSim(aov_fit,fixed("symmetry","lr"),nsim=10)

library(lme4)
library(rstatix)
## subjects clustered by their spatial ability
Exp1.lmm1a <- lmer(dp ~ 
                     rotation_s * symmetry+ SA + SA:symmetry + SA:rotation_s+
                     ((rotation_s*symmetry)|subject), 
                   data = Exp1_spab, 
                   REML = F)
summary(Exp1.lmm1a)
powerSim(Exp1.lmm1a,fixed("SA","lr"),nsim=100)
powerSim(Exp1.lmm1a,fixed("symmetrysymmetrical:SA","lr"),nsim=100)
fixef(Exp1.lmm1a)['symmetrysymmetrical:SA']
fixef(Exp1.lmm1a)['symmetrysymmetrical:SA'] <- 0.2
powerSim(Exp1.lmm1a,fixed("symmetrysymmetrical:SA","t"),nsim=100) 

Anova(Exp1.lmm1a)
install.packages('simr')
library('simr')
powerSim(Exp1.lmm1a,fixed("SA:symmetry","chisq"),nsim=1000) 

simmodel <- extend(Exp1.lmm1a,along='subject',n=100)
simmodel2 <- extend(Exp1.lmm1a,within='SA+symmetry',n=100)
pc2 <- powerCurve(Exp1.lmm1a,test=fixed("SA","chisq"),along="subject", breaks=c(20,30,40))
print(pc2)
plot(pc2)
powerSim(Exp1.lmm1a,nsim=100,test=fixed("SA","chisq"))

fixef(Exp1.lmm1a)['symmetrysymmetrical:SA'] <- 0.2
simmodel2 <- extend(Exp1.lmm1a,within='SA+symmetry',n=50)
pc2 <- powerCurve(simmodel2,test=fixed("symmetrysymmetrical:SA","chisq"), 
                  along ="subject", breaks=c(20,30,40,50))
print(pc2)
plot(pc2)

# install mixedpower

install.packages("cli")
install.packages("devtools")

# load library
library(mixedpower)

model <-Exp1.lmm1 # which model do we want to simulate power for?
data <- Exp1_spab # data used to fit the model
fixed_effects <- c("rotation_s", "symmetry","SA") # all fixed effects specified in FLPmodel
simvar <- "subject" # which random effect do we want to vary in the simulation?

steps <- c(20, 30, 40, 50, 60) # which sample sizes do we want to look at?
critical_value <- 2 # which t/z value do we want to use to test for significance?
n_sim <- 1000 # how many single simulations should be used to estimate power?

Exp1_spab$subject <- as.numeric(Exp1_spab$subject)

power_FLP <- mixedpower(model = Exp1.lmmr, data = Exp1_spab,
                        fixed_effects = c("rotation_s", "symmetry","low_SA"),
                        simvar = "subject", steps = c(20,30,40),
                        critical_value = 2, n_sim = 1000)
power_FLP

summary(Exp1.lmm1)
#install.packages('effectsize')
library(effectsize)
#install.packages('lmerTest')
library(lmerTest)
anova(Exp1.lmm1)
effectsize::F_to_eta2(
  f = c(33.36, 282.84,9.00,19.14,0.65,1.36),
  df = 1,
  df_error = c(210,210,42,210,210)
)

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


subj <- factor(1:42)
with_group1 <- c("sym","asym")
with_group2 <- c(-1.18,0.07,1.25)

subj_full <- rep(subj,6)
wit_grp1_full <- rep(rep(with_group1,each=42),3)
wit_grp2_full <- rep(rep(with_group2,each=84),1)
covars <- data.frame(id=subj_full, symmetry=wit_grp1_full,rotation=wit_grp2_full)


nsub <-  42
## Random intercepts for participants
V2 <- list(0.3)
## residual variance
res <- 0.3
#assume effect size d=.2 (large)
d <- .2
d^2/(d^2+4)
coeffi <- d/sqrt(sum(res,V2[[1]]))
## Intercept and slopes for symmetry, rotation, sym:rot
fixed <- c(1.5,coeffi,coeffi,coeffi)

model <- makeLmer(y ~ symmetry*rotation + (1|id), fixef=fixed, VarCorr=V2, sigma=res, data=covars)
model

sim_interaction <- powerSim(model, nsim=100, test = fcompare(y~rotation+symmetry + (1|id)))
sim_interaction
