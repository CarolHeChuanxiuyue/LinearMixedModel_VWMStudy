setwd("~/Documents/Research/Third Year Project/Chemistry/Symmetry/SymmetryDataAnalysis/reliability")

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
  score_total <- apply(data_valid[,c(2:length(data))],1,sum)
  return(list(data_valid, score_total))
}

#####----------Experiment 1----------#####
########## paper folding
##########>>>>> data reading and scoring
C1PP<- read.csv("3C1PP.csv")
C1PP[C1PP=='']<- 0

test <- as.data.frame(t(apply(C1PP,1,scoring,rubrics=pp_correct,penalty=-0.25)))
valid_score <- valid_score(test,valid_participants = v_perf)

##########>>>>> descriptive stats
Exp1_pp_tot <- valid_score[[2]]
psych::describe(Exp1_pp_tot)

##########>>>>> permutation based split-half reliability
data_valid_long <- gather(valid_score[[1]], trialID, score, V2:V21, factor_key=TRUE)
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
valid_score <- valid_score(test,valid_participants = v_perf)

##########>>>>> descriptive stats
Exp1_cc_tot <- valid_score[[2]]
psych::describe(Exp1_cc_tot)

##########>>>>> permutation based split-half reliability
data_valid_long <- gather(valid_score[[1]], trialID, score, V2:V43, factor_key=TRUE)
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
valid_score <- valid_score(test,valid_participants = v_perf)

########## cube comparisons
##########>>>>> data reading and scoring
C2CC<- read.csv("3C2CC.csv")
C2CC[C2CC=='']<- 0


