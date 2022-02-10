setwd("~/Documents/Research/Third Year Project/Chemistry/Symmetry/SymmetryDataAnalysis/RawData/OpenScienceFiles")

library(tidyverse)

##>>>>>>>>>>>>>>>>>>>>Psychometric scoring

#####----------correct answers----------#####

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

#####----------Experiment 1----------#####
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

library(splithalf)
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

#####----------Experiment 2---------#####
########## paper folding
##########>>>>> data reading and scoring
C2PP<- read.csv("3C2PP.csv")
C2PP[C2PP=='']<- 0

test <- as.data.frame(t(apply(C2PP,1,scoring,rubrics=pp_correct,penalty=-0.25)))
data_valid <- valid_score(test,valid_participants = v_perf)

##########>>>>> descriptive stats
Exp2_pp_tot <- data_valid[,c(1,22)]
psych::describe(Exp2_pp_tot$total)

##########>>>>> permutation based split-half reliability
data_valid_long <- gather(data_valid[,c(1:21)], trialID, score, V2:V21, factor_key=TRUE)
colnames(data_valid_long)[1] <- 'subject'

library(splithalf)
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
C2CC<- read.csv("3C2CC.csv")
C2CC[C2CC=='']<- 0

test <- as.data.frame(t(apply(C2CC,1,scoring,rubrics=cc_correct,penalty=-1)))
data_valid <- valid_score(test,valid_participants = v_perf)

##########>>>>> descriptive stats
Exp2_cc_tot <- data_valid[,c(1,44)]
psych::describe(Exp2_cc_tot$total)

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

cor.test(Exp2_cc_tot$total,Exp2_pp_tot$total)
########## verbal reasoning
##########>>>>> data reading and scoring
C2VR<- read.csv("3C2VR.csv")
C2VR[C2VR=='']<- 0
test <- as.data.frame(t(apply(C2VR,1,scoring,rubrics=vr_correct,penalty=-0.25)))
data_valid <- valid_score(test,valid_participants = v_perf)
##########>>>>> descriptive stats
Exp2_vr_tot <- data_valid[,c(1,42)]
psych::describe(Exp2_vr_tot$total)

##########>>>>> permutation based split-half reliability
data_valid_long <- gather(data_valid[,c(1:41)], trialID, score, V2:V41, factor_key=TRUE)
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

########## Raven's
##########>>>>> data reading and scoring
Exp2_raven_raw <- read.csv("3C2raven.csv")
Exp2_raven_tot <- Exp2_raven_raw[Exp2_raven_raw$subject%in%v_perf$subject,]
colnames(Exp2_raven_tot)[1] <- 'V1'
psych::describe(Exp2_raven_tot$RAPM)

#put all data frames into list
psy_list <- list(Exp2_cc_tot, Exp2_pp_tot, Exp2_vr_tot,Exp2_raven_tot)

#merge all data frames in list
Exp2_psy <- psy_list %>% reduce(full_join, by='V1')
colnames(Exp2_psy) <- c('subject','CC','PP','VR','RAPM')
Exp2_psy <- Exp2_psy%>%
  mutate(SA=(scale(PP,center = T,scale = T)+
               scale(CC,center = T,scale = T))/2)
cor.test(Exp2_psy$SA,Exp2_psy$VR)
cor.test(Exp2_psy$SA,Exp2_psy$RAPM)





library(effectsize)
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
aov_fit4 <- aov_4(dp ~ rotation_s * symmetry+
                    SA+SA:symmetry+SA:rotation_s + ((rotation_s * symmetry)|subject),
                  data = Exp1_spab,observed = c("SA"),factorize = FALSE)
aov_fit4
eta_squared(aov_fit4)

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
