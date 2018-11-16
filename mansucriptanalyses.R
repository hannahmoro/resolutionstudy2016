#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#manuscript analyses
#10/24/2018
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Initializing libraries --------------------------------------------------
library(ggplot2) 
library(tidyverse)    
library(dplyr)
library(plyr) 
library(xtable) 
library(psych)
library(corrplot)
library(GGally)
library(lme4)
library(broom)
library(MuMIn)
library(caret)



# Importing data -------------------------------------------------- 

resLIWC <- read.csv('LIWCresForHannah.csv')

#adding random ID numbers
set.seed(1)
#assigning each person a random ID
study_ID <- data.frame(study_ID = sample(1:length(unique(resLIWC$MturkID)), replace = FALSE), MturkID = unique(resLIWC$MturkID))
#adding IDs to dataset and removing Mturk ID
resLIWC <- merge(study_ID, resLIWC, by = "MturkID", all = TRUE) %>% select(-MturkID)

resLIWC <- resLIWC %>% 
#  dplyr::select(MturkID, resNum, t4Achieve, t4Success, t4Status, selfControl, t1Commit, t1Confident, t1Effort, 
#                pronoun,  Dic, nonflu, article, assent, netspeak, i, ppron, verb, adj, adverb, conj, negate, 
#                swear, number, quant, posemo, negemo, social, tentat, certain, health, ingest, achieve, reward, risk, 
#                focuspast, focuspresent, focusfuture, time, work, leisure, home, money, WC) %>%  
  #removing person with duplicate resolutions
  filter(study_ID != 389) %>% 
  #removing people whose resolutions were incoherent
  filter(study_ID != 133)


#creating new variables ####

#mturk as character
resLIWC$MturkID <- as.character(resLIWC$MturkID)

#recoding resnum
resLIWC$resNum <- as.character(resLIWC$resNum)
resLIWC$resNum[resLIWC$resNum == "NYR1"] <- 1
resLIWC$resNum[resLIWC$resNum == "NYR2"] <- 2
resLIWC$resNum[resLIWC$resNum == "NYR3"] <- 3
resLIWC$resNum[resLIWC$resNum == "NYR4"] <- 4
resLIWC$resNum[resLIWC$resNum == "NYR5"] <- 5

#creating a new id variable
resLIWC$resid <- paste(resLIWC$MturkID, resLIWC$resNum, sep="_")

#making LIWC variables count not proportion but saving prop variable
#pronoun
resLIWC$pronoun_prop <- resLIWC$pronoun
resLIWC$pronoun <- round(resLIWC$pronoun*resLIWC$WC*1/100, digits = 2)
#article
resLIWC$article_prop <- resLIWC$article
resLIWC$article <- round(resLIWC$article*resLIWC$WC*1/100, digits = 2)
#i
resLIWC$i_prop <-resLIWC$i
resLIWC$i <- round(resLIWC$i*resLIWC$WC*1/100, digits = 2)
#ppron
resLIWC$ppron_prop <-resLIWC$ppron
resLIWC$ppron <- round(resLIWC$ppron*resLIWC$WC*1/100, digits = 2)
#verb
resLIWC$verb_prop <-resLIWC$verb
resLIWC$verb <- round(resLIWC$verb*resLIWC$WC*1/100, digits = 2)
#adj
resLIWC$adj_prop <-resLIWC$adj
resLIWC$adj <- round(resLIWC$adj*resLIWC$WC*1/100, digits = 2)
#adverb
resLIWC$adverb_prop <-resLIWC$adverb
resLIWC$adverb <- round(resLIWC$adverb*resLIWC$WC*1/100, digits = 2)
#conj
resLIWC$conj_prop <-resLIWC$conj
resLIWC$conj <- round(resLIWC$conj*resLIWC$WC*1/100, digits = 2)
#negate
resLIWC$negate_prop <-resLIWC$negate
resLIWC$negate <- round(resLIWC$var*resLIWC$WC*1/100, digits = 2)
#number
resLIWC$number_prop <-resLIWC$number
resLIWC$number <- round(resLIWC$number*resLIWC$WC*1/100, digits = 2)
#quant
resLIWC$quant_prop <-resLIWC$quant
resLIWC$quant <- round(resLIWC$quant*resLIWC$WC*1/100, digits = 2)
#posemo
resLIWC$posemo_prop <-resLIWC$posemo
resLIWC$posemo <- round(resLIWC$posemo*resLIWC$WC*1/100, digits = 2)
#negemo
resLIWC$negemo_prop <-resLIWC$negemo
resLIWC$negemo <- round(resLIWC$negemo*resLIWC$WC*1/100, digits = 2)
#social
resLIWC$social_prop <-resLIWC$social
resLIWC$social <- round(resLIWC$social*resLIWC$WC*1/100, digits = 2)
#tentat
resLIWC$tentat_prop <-resLIWC$tentat
resLIWC$tentat <- round(resLIWC$tentat*resLIWC$WC*1/100, digits = 2)
#certain
resLIWC$certain_prop <-resLIWC$certain
resLIWC$certain <- round(resLIWC$certain*resLIWC$WC*1/100, digits = 2)
#health
resLIWC$health_prop <-resLIWC$health
resLIWC$health <- round(resLIWC$health*resLIWC$WC*1/100, digits = 2)
#ingest
resLIWC$ingest_prop <-resLIWC$ingest
resLIWC$ingest <- round(resLIWC$ingest*resLIWC$WC*1/100, digits = 2)
#achieve
resLIWC$achieve_prop <-resLIWC$achieve
resLIWC$achieve <- round(resLIWC$achieve*resLIWC$WC*1/100, digits = 2)
#reward
resLIWC$reward_prop <-resLIWC$reward
resLIWC$reward <- round(resLIWC$reward*resLIWC$WC*1/100, digits = 2)
#risk
resLIWC$risk_prop <-resLIWC$risk
resLIWC$risk <- round(resLIWC$risk*resLIWC$WC*1/100, digits = 2)
#focuspast
resLIWC$focuspast_prop <-resLIWC$focuspast
resLIWC$focuspast <- round(resLIWC$focuspast*resLIWC$WC*1/100, digits = 2)
#focuspresent
resLIWC$focuspresent_prop <-resLIWC$focuspresent
resLIWC$focuspresent <- round(resLIWC$focuspresent*resLIWC$WC*1/100, digits = 2)
#focusfuture
resLIWC$focusfuture_prop <-resLIWC$focusfuture
resLIWC$focusfuture <- round(resLIWC$focusfuture*resLIWC$WC*1/100, digits = 2)
#time
resLIWC$time_prop <-resLIWC$time
resLIWC$time <- round(resLIWC$time*resLIWC$WC*1/100, digits = 2)
#work
resLIWC$work_prop <-resLIWC$work
resLIWC$work <- round(resLIWC$work*resLIWC$WC*1/100, digits = 2)
#leisure
resLIWC$leisure_prop <-resLIWC$leisure
resLIWC$leisure <- round(resLIWC$leisure*resLIWC$WC*1/100, digits = 2)
#home
resLIWC$home_prop <-resLIWC$home
resLIWC$home <- round(resLIWC$home*resLIWC$WC*1/100, digits = 2)
#money
resLIWC$money_prop <-resLIWC$money
resLIWC$money <- round(resLIWC$money*resLIWC$WC*1/100, digits = 2)


#creating a binary success variable
resLIWC$t4Success_01[resLIWC$t4Status == 6 & resLIWC$t4Status != "NA"] <- 1 
resLIWC$t4Success_01[resLIWC$t4Status == 7 & resLIWC$t4Status != "NA"] <- 1 
resLIWC$t4Success_01[resLIWC$t4Status == 1 & resLIWC$t4Status != "NA"] <- 0
resLIWC$t4Success_01[resLIWC$t4Status == 2 & resLIWC$t4Status != "NA"] <- 0
resLIWC$t4Success_01[resLIWC$t4Status == 3 & resLIWC$t4Status != "NA"] <- 0
resLIWC$t4Success_01[resLIWC$t4Status == 4 & resLIWC$t4Status != "NA"] <- 0
resLIWC$t4Success_01[resLIWC$t4Status == 5 & resLIWC$t4Status != "NA"] <- 0

#seeing what attrition is related to
#first creating an attrition variable for t4
resLIWC$t4completed <- 0
resLIWC$t4completed[resLIWC$t4Status != "NA"] <- 1
#looking at how many resolutions were completed
table(resLIWC$t4completed)


#making binary variables ####

#pronoun
resLIWC$pronoun_01[resLIWC$pronoun == 0 & resLIWC$pronoun != "NA"] <- 0
resLIWC$pronoun_01[resLIWC$pronoun > 0 & resLIWC$pronoun != "NA"] <- 1
#article
resLIWC$article_01[resLIWC$article == 0 & resLIWC$article != "NA"] <- 0
resLIWC$article_01[resLIWC$article > 0 & resLIWC$article != "NA"] <- 1
#i
resLIWC$i_01[resLIWC$i == 0 & resLIWC$i != "NA"] <- 0
resLIWC$i_01[resLIWC$i > 0 & resLIWC$i != "NA"] <- 1
#ppron
resLIWC$ppron_01[resLIWC$ppron == 0 & resLIWC$ppron != "NA"] <- 0
resLIWC$ppron_01[resLIWC$ppron > 0 & resLIWC$ppron != "NA"] <- 1
#verb
resLIWC$verb_01[resLIWC$verb == 0 & resLIWC$verb != "NA"] <- 0
resLIWC$verb_01[resLIWC$verb > 0 & resLIWC$verb != "NA"] <- 1
#adj
resLIWC$adj_01[resLIWC$adj == 0 & resLIWC$adj != "NA"] <- 0
resLIWC$adj_01[resLIWC$adj > 0 & resLIWC$adj != "NA"] <- 1
#adverb
resLIWC$adverb_01[resLIWC$adverb == 0 & resLIWC$adverb != "NA"] <- 0
resLIWC$adverb_01[resLIWC$adverb > 0 & resLIWC$adverb != "NA"] <- 1
#conj
resLIWC$conj_01[resLIWC$conj == 0 & resLIWC$conj != "NA"] <- 0
resLIWC$conj_01[resLIWC$conj > 0 & resLIWC$conj != "NA"] <- 1
#negate
resLIWC$negate_01[resLIWC$negate == 0 & resLIWC$negate != "NA"] <- 0
resLIWC$negate_01[resLIWC$negate > 0 & resLIWC$negate != "NA"] <- 1
#number
resLIWC$number_01[resLIWC$number == 0 & resLIWC$number != "NA"] <- 0
resLIWC$number_01[resLIWC$number > 0 & resLIWC$number != "NA"] <- 1
#quant
resLIWC$quant_01[resLIWC$quant == 0 & resLIWC$quant != "NA"] <- 0
resLIWC$quant_01[resLIWC$quant > 0 & resLIWC$quant != "NA"] <- 1
#posemo
resLIWC$posemo_01[resLIWC$posemo == 0 & resLIWC$posemo != "NA"] <- 0
resLIWC$posemo_01[resLIWC$posemo > 0 & resLIWC$posemo != "NA"] <- 1
#negemo
resLIWC$negemo_01[resLIWC$negemo == 0 & resLIWC$negemo != "NA"] <- 0
resLIWC$negemo_01[resLIWC$negemo > 0 & resLIWC$negemo != "NA"] <- 1
#social
resLIWC$social_01[resLIWC$social == 0 & resLIWC$social != "NA"] <- 0
resLIWC$social_01[resLIWC$social > 0 & resLIWC$social != "NA"] <- 1
#tentat
resLIWC$tentat_01[resLIWC$tentat == 0 & resLIWC$tentat != "NA"] <- 0
resLIWC$tentat_01[resLIWC$tentat > 0 & resLIWC$tentat != "NA"] <- 1
#certain
resLIWC$certain_01[resLIWC$certain == 0 & resLIWC$certain != "NA"] <- 0
resLIWC$certain_01[resLIWC$certain > 0 & resLIWC$certain != "NA"] <- 1
#health
resLIWC$health_01[resLIWC$health == 0 & resLIWC$health != "NA"] <- 0
resLIWC$health_01[resLIWC$health > 0 & resLIWC$health != "NA"] <- 1
#ingest
resLIWC$ingest_01[resLIWC$ingest == 0 & resLIWC$ingest != "NA"] <- 0
resLIWC$ingest_01[resLIWC$ingest > 0 & resLIWC$ingest != "NA"] <- 1
#achieve
resLIWC$achieve_01[resLIWC$achieve == 0 & resLIWC$achieve != "NA"] <- 0
resLIWC$achieve_01[resLIWC$achieve > 0 & resLIWC$achieve != "NA"] <- 1
#reward
resLIWC$reward_01[resLIWC$reward == 0 & resLIWC$reward != "NA"] <- 0
resLIWC$reward_01[resLIWC$reward > 0 & resLIWC$reward != "NA"] <- 1
#risk
resLIWC$risk_01[resLIWC$risk == 0 & resLIWC$risk != "NA"] <- 0
resLIWC$risk_01[resLIWC$risk > 0 & resLIWC$risk != "NA"] <- 1
#focuspast
resLIWC$focuspast_01[resLIWC$focuspast == 0 & resLIWC$focuspast != "NA"] <- 0
resLIWC$focuspast_01[resLIWC$focuspast > 0 & resLIWC$focuspast != "NA"] <- 1
#focuspresent
resLIWC$focuspresent_01[resLIWC$focuspresent == 0 & resLIWC$focuspresent != "NA"] <- 0
resLIWC$focuspresent_01[resLIWC$focuspresent > 0 & resLIWC$focuspresent != "NA"] <- 1
#focusfuture
resLIWC$focusfuture_01[resLIWC$focusfuture == 0 & resLIWC$focusfuture != "NA"] <- 0
resLIWC$focusfuture_01[resLIWC$focusfuture > 0 & resLIWC$focusfuture != "NA"] <- 1
#time
resLIWC$time_01[resLIWC$time == 0 & resLIWC$time != "NA"] <- 0
resLIWC$time_01[resLIWC$time > 0 & resLIWC$time != "NA"] <- 1
#work
resLIWC$work_01[resLIWC$work == 0 & resLIWC$work != "NA"] <- 0
resLIWC$work_01[resLIWC$work > 0 & resLIWC$work != "NA"] <- 1
#leisure
resLIWC$leisure_01[resLIWC$leisure == 0 & resLIWC$leisure != "NA"] <- 0
resLIWC$leisure_01[resLIWC$leisure > 0 & resLIWC$leisure != "NA"] <- 1
#home
resLIWC$home_01[resLIWC$home == 0 & resLIWC$home != "NA"] <- 0
resLIWC$home_01[resLIWC$home > 0 & resLIWC$home != "NA"] <- 1
#money
resLIWC$money_01[resLIWC$money == 0 & resLIWC$money != "NA"] <- 0
resLIWC$money_01[resLIWC$money > 0 & resLIWC$money != "NA"] <- 1


#adding Hannah/Christine Resolved Codes ####
#one participant completed the survey multiple times so am revoming them
weight_cthm <- read.csv("weightcodes.csv", header = TRUE, stringsAsFactors = FALSE) %>% filter(MturkID != "A1LB4EJUIYDMYJ")
finances_cthm <- read.csv("financescodes.csv", header = TRUE, stringsAsFactors = FALSE) %>% filter(MturkID != "A1LB4EJUIYDMYJ")
exercise_cthm <- read.csv("exercisecodes.csv", header = TRUE, stringsAsFactors = FALSE) %>% filter(MturkID != "A1LB4EJUIYDMYJ")
subuse_cthm <- read.csv("subusecodes.csv", header = TRUE, stringsAsFactors = FALSE) %>% filter(MturkID != "A1LB4EJUIYDMYJ")
specificity_cthm <- read.csv("specificitycodes.csv", header = TRUE, stringsAsFactors = FALSE) %>% filter(MturkID != "A1LB4EJUIYDMYJ")
approach_cthm <- read.csv("approachcodes.csv", header = TRUE, stringsAsFactors = FALSE) %>% filter(MturkID != "A1LB4EJUIYDMYJ")
freq_cthm <- read.csv("freqcodes.csv", header = TRUE, stringsAsFactors = FALSE) %>% filter(MturkID != "A1LB4EJUIYDMYJ")

#relabeling resolution numbering variable
names(weight_cthm) <- c("X", "MturkID", "1", "2", "3", "4", "5")
names(finances_cthm) <- c("X", "MturkID", "1", "2", "3", "4", "5")
names(exercise_cthm) <- c("X", "MturkID", "1", "2", "3", "4", "5")
names(subuse_cthm) <- c("X", "MturkID", "1", "2", "3", "4", "5")
names(specificity_cthm) <- c("X", "MturkID", "1", "2", "3", "4", "5")
names(approach_cthm) <- c("X", "MturkID", "1", "2", "3", "4", "5")
names(freq_cthm) <- c("X", "MturkID", "1", "2", "3", "4", "5")
#wide to long for each 
weight_cthm_long <- select(weight_cthm %>% gather("resNum", "weight_cthm", 3:7), - X) 
finances_cthm_long <- select(finances_cthm %>% gather("resNum", "finances_cthm", 3:7), - X)
exercise_cthm_long <- select(exercise_cthm %>% gather("resNum", "exercise_cthm", 3:7), - X)
subuse_cthm_long <- select(subuse_cthm %>% gather("resNum", "subuse_cthm", 3:7), - X)
specificity_cthm_long <- select(specificity_cthm %>% gather("resNum", "specificity_cthm", 3:7), - X)
approach_cthm_long <- select(approach_cthm %>% gather("resNum", "approach_cthm", 3:7), - X)
freq_cthm_long <- select(freq_cthm %>% gather("resNum", "freq_cthm", 3:7), - X)

#now merging
#making a new ID variable 
weight_cthm_long$resid <- paste(weight_cthm_long$MturkID, weight_cthm_long$resNum, sep="_")
finances_cthm_long$resid <- paste(finances_cthm_long$MturkID, finances_cthm_long$resNum, sep="_")
exercise_cthm_long$resid <- paste(exercise_cthm_long$MturkID, exercise_cthm_long$resNum, sep="_")
subuse_cthm_long$resid <- paste(subuse_cthm_long$MturkID, subuse_cthm_long$resNum, sep="_")
specificity_cthm_long$resid <- paste(specificity_cthm_long$MturkID, specificity_cthm_long$resNum, sep="_")
approach_cthm_long$resid <- paste(approach_cthm_long$MturkID, approach_cthm_long$resNum, sep="_")
freq_cthm_long$resid <- paste(freq_cthm_long$MturkID, freq_cthm_long$resNum, sep="_")

#removing other other id variables to keep it simple
weight_cthm_long <- weight_cthm_long %>% select(-MturkID, -resNum)
finances_cthm_long <- finances_cthm_long %>% select(-MturkID, -resNum)
exercise_cthm_long <- exercise_cthm_long %>% select(-MturkID, -resNum)
subuse_cthm_long <- subuse_cthm_long %>% select(-MturkID, -resNum)
specificity_cthm_long <- specificity_cthm_long %>% select(-MturkID, -resNum)
approach_cthm_long <- approach_cthm_long %>% select(-MturkID, -resNum)
freq_cthm_long <- freq_cthm_long %>% select(-MturkID, -resNum)

#removing NAs
#weight_cthm_long <- weight_cthm_long %>% filter(weight_cthm_long$weight_cthm != "NA")
#finances_cthm_long <- finances_cthm_long %>% filter(finances_cthm_long$finances_cthm != "NA")
#exercise_cthm_long <- exercise_cthm_long %>% filter(exercise_cthm_long$exercise_cthm != "NA")
#subuse_cthm_long <- subuse_cthm_long %>% filter(subuse_cthm_long$subuse_cthm != "NA")
#specificity_cthm_long <- specificity_cthm_long %>% filter(specificity_cthm_long$specificity_cthm != "NA")
#approach_cthm_long <- approach_cthm_long %>% filter(approach_cthm_long$approach_cthm != "NA")
#freq_cthm_long <- freq_cthm_long %>% filter(freq_cthm_long$freq_cthm != "NA")

#saving a copy in case I mess up
resLIWC_copy <- resLIWC

#merging
resLIWC <- left_join(resLIWC, weight_cthm_long, by = "resid")
resLIWC <- left_join(resLIWC, finances_cthm_long, by = "resid") 
resLIWC <- left_join(resLIWC, exercise_cthm_long, by = "resid") 
resLIWC <- left_join(resLIWC, subuse_cthm_long, by = "resid") 
resLIWC <- left_join(resLIWC, specificity_cthm_long, by = "resid") 
resLIWC <- left_join(resLIWC, approach_cthm_long, by = "resid") 
resLIWC <- left_join(resLIWC, freq_cthm_long, by = "resid") 




#adding Carrie codes ####

##******* something is wrong with these files, the ids differ and the merging isn't working*****

#making a file just with the resolutions
#resolutiontext_forcodes <- read.csv("weightcodes_CH.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = ".") %>% select(MturkID, NYR1, NYR2, NYR3, NYR4, NYR5)
#removing text 
#weight_ch <- read.csv("weightcodes_CH.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = ".") %>% select(-NYR1, -NYR2, -NYR3, -NYR4, -NYR5)
#finances_ch <- read.csv("financescodes_CH.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = ".") %>% select(-NYR1, -NYR2, -NYR3, -NYR4, -NYR5)
#exercise_ch <- read.csv("exercisecodes_CH.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = ".") %>% select(-NYR1, -NYR2, -NYR3, -NYR4, -NYR5)
#subuse_ch <- read.csv("subusecodes_CH.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = ".") %>% select(-NYR1, -NYR2, -NYR3, -NYR4, -NYR5)
#specificity_ch <- read.csv("specificitycodes_CH.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = ".") %>% select(-NYR1, -NYR2, -NYR3, -NYR4, -NYR5)
#approach_ch <- read.csv("approachcodes_CH.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = ".") %>% select(-NYR1, -NYR2, -NYR3, -NYR4, -NYR5)
#freq_ch <- read.csv("freqcodes_CH.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = ".") %>% select(-NYR1, -NYR2, -NYR3, -NYR4, -NYR5)

#relabeling resolution numbering variable
#names(weight_ch) <- c("MturkID", "1", "2", "3", "4", "5")
#names(finances_ch) <- c("MturkID", "1", "2", "3", "4", "5")
#names(exercise_ch) <- c("MturkID", "1", "2", "3", "4", "5")
#names(subuse_ch) <- c("MturkID", "1", "2", "3", "4", "5")
#names(specificity_ch) <- c("MturkID", "1", "2", "3", "4", "5")
#names(approach_ch) <- c("MturkID", "1", "2", "3", "4", "5")
#names(freq_ch) <- c("MturkID", "1", "2", "3", "4", "5")

#wide to long for each 
#weight_ch_long <- weight_ch %>% gather("resNum", "weight_ch", 2:6) 
#finances_ch_long <- finances_ch %>% gather("resNum", "finances_ch", 2:6)
#exercise_ch_long <-exercise_ch %>% gather("resNum", "exercise_ch", 2:6)
#subuse_ch_long <- subuse_ch %>% gather("resNum", "subuse_ch", 2:6)
#specificity_ch_long <- select(specificity_ch %>% gather("resNum", "specificity_ch", 2:6))
#approach_ch_long <- approach_ch %>% gather("resNum", "approach_ch", 2:6)
#freq_ch_long <- freq_ch %>% gather("resNum", "freq_ch", 2:6)


#now merging
#allcodes_ch <- left_join(weight_ch_long, finances_ch_long, by = c("MturkID", "resNum")) %>% 
#  left_join(exercise_ch_long, by = c("MturkID", "resNum")) %>% 
#  left_join(subuse_ch_long, by = c("MturkID", "resNum")) %>%
#  left_join(approach_ch_long, by = c("MturkID", "resNum")) %>% 
#  left_join(freq_ch_long, by = c("MturkID", "resNum")) %>% left_join(specificity_ch_long, by = c("MturkID", "resNum"))





#looking at variance of linguistic features
describe(resLIWC)

#removing zero and near-zero variance variables
#using caret which looks at frequency of
#most prevalent value relative to second most and
#the percent of unique values
nzv <- nearZeroVar(resLIWC, saveMetrics = TRUE)
nzv[nzv$nzv,]

#removing NZV variables
nzv <- nearZeroVar(resLIWC)
resLIWC_allvars <- resLIWC
resLIWC <- resLIWC[, -nzv]
p_orig <- dim(resLIWC_allvars)
p_filtered <- dim(resLIWC)
num_filtered_nzv <- p_orig[2] - p_filtered[2]
#num_filtered_nzv is the number of near zero variance filtered

#now looking at multicolinearity
#just looking at non-character vars
resLIWC_num <- resLIWC[, !sapply(resLIWC, is.character)] %>% na.omit() %>% select(-t4completed)

resCor <- cor(resLIWC_num, use = "pairwise.complete.obs")
summary(resCor[upper.tri(resCor)])

#a corr of .9928!
highlyCorRes <- findCorrelation(resCor, cutoff = .85)
filteredRes <- resLIWC_num[,-highlyCorRes]
resCor2 <- cor(filteredRes, use = "everything")
summary(resCor2[upper.tri(resCor2)])

#removed a bunch
p_orig <- dim(resLIWC)
p_filtered <- dim(filteredRes)
num_filtered_cor <- p_orig[2] - p_filtered[2]
num_filtered_cor 
#34!

#finding the list of the variables that were removed by this process
allvarnames <- names(resLIWC_allvars)
varsdropped_nzv <- names(resLIWC)
varsdropped_nzv_cor <- names(filteredRes)

dropped_nzv <- setdiff(allvarnames, varsdropped_nzv)
dropped_nzv
dropped_cor <- setdiff(varsdropped_nzv, varsdropped_nzv_cor)
dropped_cor

#looking at what finances and specificity codes were correlated with
contentcode_df <- resLIWC %>% select(contains("_cthm"))
cor(contentcode_df)
#oh it's just the binary versions of the code...

names(filteredRes)


#testing whether attrition was related to anything ####

glmer_attrition <- glmer(t4completed ~ selfControl + t1Commit + t1Confident + t1Effort +  pronoun +  Dic + article + i + ppron + verb + adj + adverb + conj + negate + number + quant + posemo + negemo + social + tentat + certain + health + ingest + achieve + reward + risk + focuspast + focuspresent + focusfuture + time + work + leisure + home + money + WC + (1|MturkID), data = resLIWC, family = binomial, nAGQ = 10)

print(glmer_attrition, corr = FALSE)

#producing se around the estimates
se <- sqrt(diag(vcov(glmer_attrition)))
(tab <- cbind(Est = fixef(glmer_attrition), LL = fixef(glmer_attrition) -1.96*se, UL = fixef(glmer_attrition) + 1.96*se))

#producing odds ratios
glmer_attrition_or <- exp(glmer_attrition)

#plot 
plot(glmer_attrition)
summary(glmer_attrition)
logistic1 <- tidy(glmer_attrition)
logistic1_b <- glance(glmer_attrition)

#post-hoc ttest
attrition_ttest <-  t.test(selfControl ~ t4completed, data = resLIWC, var.equal = TRUE)
table()
#mean and sd of completed
mean(resLIWC$selfControl[resLIWC$t4completed == 1])
sd(resLIWC$selfControl[resLIWC$t4completed == 1])
#mean and sd of not completed
mean(resLIWC$selfControl[resLIWC$t4completed == 0])
sd(resLIWC$selfControl[resLIWC$t4completed == 0])


#since the variables are non-normal, also doing chisqs of linguistic variables
#chisqs of raw variables ####
#making tables
aProTbl <- table(resLIWC$pronoun, resLIWC$t4completed)
aDicTbl <- table(resLIWC$Dic, resLIWC$t4completed)
aArtTbl <- table(resLIWC$article, resLIWC$t4completed)
aITbl <- table(resLIWC$i, resLIWC$t4completed)
aPpronTbl <- table(resLIWC$ppron, resLIWC$t4completed)
aVerbTbl <- table(resLIWC$verb, resLIWC$t4completed)
aAdjTbl <- table(resLIWC$adj, resLIWC$t4completed)
aAdverbTbl <- table(resLIWC$adverb, resLIWC$t4completed) 
aConjTbl <- table(resLIWC$conj, resLIWC$t4completed) 
aNegateTbl <- table(resLIWC$negate, resLIWC$t4completed)
aNumberTbl <- table(resLIWC$number, resLIWC$t4completed)
aQuantTbl <- table(resLIWC$quant, resLIWC$t4completed)
aPosemoTbl <- table(resLIWC$posemo, resLIWC$t4completed) 
aNegemoTbl <- table(resLIWC$negemo, resLIWC$t4completed) 
aSocialTbl <- table(resLIWC$social, resLIWC$t4completed) 
aTentatTbl <- table(resLIWC$tentat, resLIWC$t4completed) 
aCertainTbl <- table(resLIWC$certain, resLIWC$t4completed) 
aHealthTbl <- table(resLIWC$health, resLIWC$t4completed) 
aIngestTbl <- table(resLIWC$ingest, resLIWC$t4completed) 
aAchieveTbl <- table(resLIWC$achieve, resLIWC$t4completed) 
aRewardTbl <- table(resLIWC$reward, resLIWC$t4completed) 
aRiskTbl <- table(resLIWC$risk, resLIWC$t4completed) 
aPastTbl <- table(resLIWC$focuspast, resLIWC$t4completed) 
aPresentTbl <- table(resLIWC$focuspresent, resLIWC$t4completed) 
aFutureTbl <- table(resLIWC$focusfuture, resLIWC$t4completed) 
aTimeTbl <- table(resLIWC$time, resLIWC$t4completed) 
aWorkTbl <- table(resLIWC$work, resLIWC$t4completed) 
aLeisureTbl <- table(resLIWC$leisure, resLIWC$t4completed) 
aHomeTbl <- table(resLIWC$home, resLIWC$t4completed) 
aMoneyTbl <- table(resLIWC$money, resLIWC$t4completed) 
aWcTbl <- table(resLIWC$WC, resLIWC$t4completed)
#chisqs 
#most of these have errors
chisq.test(aProTbl)
chisq.test(aDicTbl)
chisq.test(aArtTbl)
chisq.test(aITbl)
chisq.test(aPpronTbl)
chisq.test(aVerbTbl)
chisq.test(aAdjTbl)
chisq.test(aAdverbTbl)
chisq.test(aConjTbl)
chisq.test(aNegateTbl)
chisq.test(aNumberTbl)
chisq.test(aQuantTbl)
chisq.test(aPosemoTbl)
chisq.test(aNegemoTbl)
chisq.test(aSocialTbl)
chisq.test(aTentatTbl)
chisq.test(aCertainTbl)
chisq.test(aHealthTbl)
chisq.test(aIngestTbl)
chisq.test(aAchieveTbl)
chisq.test(aRewardTbl)
chisq.test(aRiskTbl)
chisq.test(aPastTbl)
chisq.test(aPresentTbl)
chisq.test(aFutureTbl)
chisq.test(aTimeTbl)
chisq.test(aWorkTbl)
chisq.test(aLeisureTbl)
chisq.test(aHomeTbl)
chisq.test(aMoneyTbl)
chisq.test(aWcTbl)

#chisqs of binary variables ####
#making tables
aProTbl_01 <- table(resLIWC$pronoun_01, resLIWC$t4completed)
#aDicTbl_01 <- table(resLIWC$Dic_01, resLIWC$t4completed)
aArtTbl_01 <- table(resLIWC$article_01, resLIWC$t4completed)
aITbl_01 <- table(resLIWC$i_01, resLIWC$t4completed)
aPpronTbl_01 <- table(resLIWC$ppron_01, resLIWC$t4completed)
aVerbTbl_01 <- table(resLIWC$verb_01, resLIWC$t4completed)
aAdjTbl_01 <- table(resLIWC$adj_01, resLIWC$t4completed)
aAdverbTbl_01 <- table(resLIWC$adverb_01, resLIWC$t4completed) 
aConjTbl_01 <- table(resLIWC$conj_01, resLIWC$t4completed) 
aNegateTbl_01 <- table(resLIWC$negate_01, resLIWC$t4completed)
aNumberTbl_01 <- table(resLIWC$number_01, resLIWC$t4completed)
aQuantTbl_01 <- table(resLIWC$quant_01, resLIWC$t4completed)
aPosemoTbl_01 <- table(resLIWC$posemo_01, resLIWC$t4completed) 
aNegemoTbl_01 <- table(resLIWC$negemo_01, resLIWC$t4completed) 
aSocialTbl_01 <- table(resLIWC$social_01, resLIWC$t4completed) 
aTentatTbl_01 <- table(resLIWC$tentat_01, resLIWC$t4completed) 
aCertainTbl_01 <- table(resLIWC$certain_01, resLIWC$t4completed) 
aHealthTbl_01 <- table(resLIWC$health_01, resLIWC$t4completed) 
aIngestTbl_01 <- table(resLIWC$ingest_01, resLIWC$t4completed) 
aAchieveTbl_01 <- table(resLIWC$achieve_01, resLIWC$t4completed) 
aRewardTbl_01 <- table(resLIWC$reward_01, resLIWC$t4completed) 
aRiskTbl_01 <- table(resLIWC$risk_01, resLIWC$t4completed) 
aPastTbl_01 <- table(resLIWC$focuspast_01, resLIWC$t4completed) 
aPresentTbl_01 <- table(resLIWC$focuspresent_01, resLIWC$t4completed) 
aFutureTbl_01 <- table(resLIWC$focusfuture_01, resLIWC$t4completed) 
aTimeTbl_01 <- table(resLIWC$time_01, resLIWC$t4completed) 
aWorkTbl_01 <- table(resLIWC$work_01, resLIWC$t4completed) 
aLeisureTbl_01 <- table(resLIWC$leisure_01, resLIWC$t4completed) 
aHomeTbl_01 <- table(resLIWC$home_01, resLIWC$t4completed) 
aMoneyTbl_01 <- table(resLIWC$money_01, resLIWC$t4completed) 
#aWcTbl_01 <- table(resLIWC$WC, resLIWC$t4completed)

#chisqs 
aProTbl_01_test <- chisq.test(aProTbl_01)
#aDicTbl_01_test <- chisq.test(aDicTbl_01)
aArtTbl_01_test <- chisq.test(aArtTbl_01)
aITbl_01_test <- chisq.test(aITbl_01)
aPpronTbl_01_test <- chisq.test(aPpronTbl_01)
aVerbTbl_01_test <- chisq.test(aVerbTbl_01)
aAdjTbl_01_test <- chisq.test(aAdjTbl_01)
aAdverbTbl_01_test <- chisq.test(aAdverbTbl_01)
aConjTbl_01_test <- chisq.test(aConjTbl_01)
aNegateTbl_01_test <- chisq.test(aNegateTbl_01)
aNumberTbl_01_test <- chisq.test(aNumberTbl_01)
aQuantTbl_01_test <- chisq.test(aQuantTbl_01)
aPosemoTbl_01_test <- chisq.test(aPosemoTbl_01)
aNegemoTbl_01_test <- chisq.test(aNegemoTbl_01)
aSocialTbl_01_test <- chisq.test(aSocialTbl_01)
aTentatTbl_01_test <- chisq.test(aTentatTbl_01)
aCertainTbl_01_test <- chisq.test(aCertainTbl_01)
aHealthTbl_01_test <- chisq.test(aHealthTbl_01)
aIngestTbl_01_test <- chisq.test(aIngestTbl_01)
aAchieveTbl_01_test <- chisq.test(aAchieveTbl_01)
aRewardTbl_01_test <- chisq.test(aRewardTbl_01)
aRiskTbl_01_test <- chisq.test(aRiskTbl_01)
aPastTbl_01_test <- chisq.test(aPastTbl_01)
aPresentTbl_01_test <- chisq.test(aPresentTbl_01)
aFutureTbl_01_test <- chisq.test(aFutureTbl_01)
aTimeTbl_01_test <- chisq.test(aTimeTbl_01)
aWorkTbl_01_test <- chisq.test(aWorkTbl_01)
aLeisureTbl_01_test <- chisq.test(aLeisureTbl_01)
aHomeTbl_01_test <- chisq.test(aHomeTbl_01)
aMoneyTbl_01_test <- chisq.test(aMoneyTbl_01)
#aWcTbl_01_test <- chisq.test(aWcTbl_01)

#making a table of all the chi-sq tests

BinaryLIWCAttritionChiSq_Results <- data.frame(Variable = c("Pronoun", "Article", "I", "Personal Pronoun","Verb", "Adjective", "Adverb", "Conjunctions", "Negate", "Number", "Quant", "Positive Emotion", "Negative Emotion", "Social", "Tentative", "Certain", "Health", "Ingest", "Achieve", "Reward", "Risk", "Focus Past", "Focus Present", "Focus Future", "Time", "Work", "Leisure", "Home", "Money"),
                         Zero = I(c((aProTbl_01[,1] + aProTbl_01[,2])[1],
                                    (aArtTbl_01[,1] + aArtTbl_01[,2])[1],
                                    (aITbl_01[,1] + aITbl_01[,2])[1],
                                    (aPpronTbl_01[,1] + aPpronTbl_01[,2])[1], 
                                    (aVerbTbl_01[,1] + aVerbTbl_01[,2])[1], 
                                    (aAdjTbl_01[,1] + aAdjTbl_01[,2])[1], 
                                    (aAdverbTbl_01[,1] + aAdverbTbl_01[,2])[1], 
                                    (aConjTbl_01[,1] + aConjTbl_01[,2])[1], 
                                    (aAdjTbl_01[,1] + aNegateTbl_01[,2])[1],
                                    (aNumberTbl_01[,1] + aNumberTbl_01[,2])[1],
                                    (aQuantTbl_01[,1] + aQuantTbl_01[,2])[1],
                                    (aPosemoTbl_01[,1] + aPosemoTbl_01[,2])[1],
                                    (aNegemoTbl_01[,1] + aNegemoTbl_01[,2])[1],
                                    (aSocialTbl_01[,1] + aSocialTbl_01[,2])[1],
                                    (aTentatTbl_01[,1] + aTentatTbl_01[,2])[1],
                                    (aCertainTbl_01[,1] + aCertainTbl_01[,2])[1],
                                    (aHealthTbl_01[,1] + aHealthTbl_01[,2])[1],
                                    (aIngestTbl_01[,1] + aIngestTbl_01[,2])[1], 
                                    (aAchieveTbl_01[,1] + aAchieveTbl_01[,2])[1],
                                    (aRewardTbl_01[,1] + aRewardTbl_01[,2])[1],
                                    (aRiskTbl_01[,1] + aRiskTbl_01[,2])[1],
                                    (aPastTbl_01[,1] + aPastTbl_01[,2])[1],
                                    (aPresentTbl_01[,1] + aPresentTbl_01[,2])[1],
                                    (aFutureTbl_01[,1] + aFutureTbl_01[,2])[1],
                                    (aTimeTbl_01[,1] + aTimeTbl_01[,2])[1],
                                    (aWorkTbl_01[,1] + aWorkTbl_01[,2])[1],
                                    (aLeisureTbl_01[,1] + aLeisureTbl_01[,2])[1],
                                    (aHomeTbl_01[,1] + aHomeTbl_01[,2])[1], 
                                    (aMoneyTbl_01[,1] + aMoneyTbl_01[,2])[1])),
                         OnePlus = I(c((aProTbl_01[,1] + aProTbl_01[,2])[2],
                                    (aArtTbl_01[,1] + aArtTbl_01[,2])[2],
                                    (aITbl_01[,1] + aITbl_01[,2])[2],
                                    (aPpronTbl_01[,1] + aPpronTbl_01[,2])[2], 
                                    (aVerbTbl_01[,1] + aVerbTbl_01[,2])[2], 
                                    (aAdjTbl_01[,1] + aAdjTbl_01[,2])[2], 
                                    (aAdverbTbl_01[,1] + aAdverbTbl_01[,2])[2], 
                                    (aConjTbl_01[,1] + aConjTbl_01[,2])[2], 
                                    (aAdjTbl_01[,1] + aNegateTbl_01[,2])[2],
                                    (aNumberTbl_01[,1] + aNumberTbl_01[,2])[2],
                                    (aQuantTbl_01[,1] + aQuantTbl_01[,2])[2],
                                    (aPosemoTbl_01[,1] + aPosemoTbl_01[,2])[2],
                                    (aNegemoTbl_01[,1] + aNegemoTbl_01[,2])[2],
                                    (aSocialTbl_01[,1] + aSocialTbl_01[,2])[2],
                                    (aTentatTbl_01[,1] + aTentatTbl_01[,2])[2],
                                    (aCertainTbl_01[,1] + aCertainTbl_01[,2])[2],
                                    (aHealthTbl_01[,1] + aHealthTbl_01[,2])[2],
                                    (aIngestTbl_01[,1] + aIngestTbl_01[,2])[2], 
                                    (aAchieveTbl_01[,1] + aAchieveTbl_01[,2])[2],
                                    (aRewardTbl_01[,1] + aRewardTbl_01[,2])[2],
                                    (aRiskTbl_01[,1] + aRiskTbl_01[,2])[2],
                                    (aPastTbl_01[,1] + aPastTbl_01[,2])[2],
                                    (aPresentTbl_01[,1] + aPresentTbl_01[,2])[2],
                                    (aFutureTbl_01[,1] + aFutureTbl_01[,2])[2],
                                    (aTimeTbl_01[,1] + aTimeTbl_01[,2])[2],
                                    (aWorkTbl_01[,1] + aWorkTbl_01[,2])[2],
                                    (aLeisureTbl_01[,1] + aLeisureTbl_01[,2])[2],
                                    (aHomeTbl_01[,1] + aHomeTbl_01[,2])[2], 
                                    (aMoneyTbl_01[,1] + aMoneyTbl_01[,2])[2])),
                          ChiSq = I(c(aProTbl_01_test$statistic, aArtTbl_01_test$statistic, aITbl_01_test$statistic, aPpronTbl_01_test$statistic, aVerbTbl_01_test$statistic, aAdjTbl_01_test$statistic, aAdverbTbl_01_test$statistic, aConjTbl_01_test$statistic, aNegateTbl_01_test$statistic, aNumberTbl_01_test$statistic, aQuantTbl_01_test$statistic, aPosemoTbl_01_test$statistic, aNegemoTbl_01_test$statistic, aSocialTbl_01_test$statistic, aTentatTbl_01_test$statistic, aCertainTbl_01_test$statistic, aHealthTbl_01_test$statistic, aIngestTbl_01_test$statistic, aAchieveTbl_01_test$statistic, aRewardTbl_01_test$statistic, aRiskTbl_01_test$statistic, aPastTbl_01_test$statistic, aPresentTbl_01_test$statistic, aFutureTbl_01_test$statistic, aTimeTbl_01_test$statistic, aWorkTbl_01_test$statistic, aLeisureTbl_01_test$statistic, aHomeTbl_01_test$statistic, aMoneyTbl_01_test$statistic)),
                          pValue = I(c(aProTbl_01_test$p.value, aArtTbl_01_test$p.value, aITbl_01_test$p.value, aPpronTbl_01_test$p.value, aVerbTbl_01_test$p.value, aAdjTbl_01_test$p.value, aAdverbTbl_01_test$p.value, aConjTbl_01_test$p.value, aNegateTbl_01_test$p.value, aNumberTbl_01_test$p.value, aQuantTbl_01_test$p.value, aPosemoTbl_01_test$p.value, aNegemoTbl_01_test$p.value, aSocialTbl_01_test$p.value, aTentatTbl_01_test$p.value, aCertainTbl_01_test$p.value, aHealthTbl_01_test$p.value, aIngestTbl_01_test$p.value, aAchieveTbl_01_test$p.value, aRewardTbl_01_test$p.value, aRiskTbl_01_test$p.value, aPastTbl_01_test$p.value, aPresentTbl_01_test$p.value, aFutureTbl_01_test$p.value, aTimeTbl_01_test$p.value, aWorkTbl_01_test$p.value, aLeisureTbl_01_test$p.value, aHomeTbl_01_test$p.value, aMoneyTbl_01_test$p.value)))
                          
colnames(BinaryLIWCAttritionChiSq_Results) <- c("LIWC Variable", "Total n None","Total n One or More", "Chi Square Attrition", "p Value")

#rounding variables
BinaryLIWCAttritionChiSq_Results$`Chi Square Attrition` <- round(BinaryLIWCAttritionChiSq_Results$`Chi Square Attrition`, digits = 2)
BinaryLIWCAttritionChiSq_Results$`p Value` <- round(BinaryLIWCAttritionChiSq_Results$`p Value`, digits = 3)

#exporting
write.csv(BinaryLIWCAttritionChiSq_Results, "BinaryLIWCAttritionChiSq_Results.csv", row.names = FALSE)


#### Characterizing resolutions ####

#basic propertiesv###

basicprop <- resLIWC %>% select(pronoun, Dic, article, i, ppron, verb, adj, adverb, conj, negate, number, quant, WC)
basicprop_descriptives <- describe(basicprop)
basicprop_descriptives <- basicprop_descriptives %>% select(-trimmed, -mad, -range, -se, - vars, - median, - n)
#rounding
basicprop_descriptives$mean <- round(basicprop_descriptives$mean, digits = 2)
basicprop_descriptives$sd <- round(basicprop_descriptives$sd, digits = 2)
basicprop_descriptives$skew <- round(basicprop_descriptives$skew, digits = 2)
basicprop_descriptives$kurtosis <- round(basicprop_descriptives$kurtosis, digits = 2)
#making a new column
basicprop_descriptives$description <- c("pronouns", "prop in dictionary", "articles", "uses of \"i\"", "personal pronouns", "verbs", "adjectives", "adverbs", "conjunctions", "negations", "numbers", "quantities", "word count")
basicprop_descriptives  <- basicprop_descriptives[c(7, 1, 2, 3, 4, 5, 6)]

#saving descriptives table
write.csv(basicprop_descriptives, "basicprop_descriptives.csv")

#counting percentages of resolutions that were totally coded
histogram(resLIWC$Dic)
prop.table(table(resLIWC$Dic))
prop.table(table(resLIWC$Dic[resLIWC$t4completed ==1]))

#goal properties ###

goalprop <- resLIWC %>% select(contains("cthm"))

table(goalprop$weight_cthm, useNA = "always")
table(goalprop$finances_cthm, useNA = "always")
table(goalprop$exercise_cthm, useNA = "always")
table(goalprop$subuse_cthm, useNA = "always")
table(goalprop$specificity_cthm, useNA = "always")
table(goalprop$approach_cthm, useNA = "always")
table(goalprop$freq_cthm, useNA = "always")


prop.table(table(goalprop$weight_cthm, useNA = "always"))
prop.table(table(goalprop$finances_cthm, useNA = "always"))
prop.table(table(goalprop$exercise_cthm, useNA = "always"))
prop.table(table(goalprop$subuse_cthm, useNA = "always"))
prop.table(table(goalprop$specificity_cthm, useNA = "always"))
prop.table(table(goalprop$approach_cthm, useNA = "always"))
prop.table(table(goalprop$freq_cthm, useNA = "always"))

codinginfo <- resLIWC %>% select(resid) %>% filter(specificity_cthm == "NA")

#### success logistic regression

success_basicprop01_1 <- glmer(t4Success_01 ~ pronoun_01 +  article_01 + i_01 + ppron_01 + verb_01 + adj_01 + 
  adverb_01 + conj_01 + negate_01 + number_01 + quant_01 + WC +
  (1|MturkID), data = resLIWC, family = binomial(link=logit), nAGQ = 10)
#need to change something as that didn't converge
success_basicprop01_2 <- update(success_basicprop01_1, control=glmerControl(optimizer="bobyqa"))

#basic property results
summary(success_basicprop01_2)
#r2
r.squaredGLMM(success_basicprop01_2)

##second model

success_humancodes_1 <- glmer(t4Success_01 ~ pronoun_01 +  article_01 + i_01 + ppron_01 + verb_01 + adj_01 + 
                                 adverb_01 + conj_01 + negate_01 + number_01 + quant_01 + WC +
                                #adding human content codes
                                weight_cthm + finances_cthm + exercise_cthm + subuse_cthm +
                                specificity_cthm + approach_cthm + freq_cthm +
                                 (1|MturkID), data = resLIWC, family = binomial(link=logit), 
                              control=glmerControl(optimizer="bobyqa"), nAGQ = 10)

#model didn't converge... trying with collapsed categories for codes with small observations
#recoding so that it's approach (1) versus all else (0)
resLIWC$approach_cthm_01 <- resLIWC$approach_cthm
resLIWC$approach_cthm_01[resLIWC$approach_cthm > 1 & resLIWC$approach_cthm != "NA"] <- 0
#recoding so that it's frequency specified (1) versus not (0)
resLIWC$freq_cthm_01 <- resLIWC$freq_cthm
resLIWC$freq_cthm_01[resLIWC$freq_cthm > 0 & resLIWC$freq_cthm != "NA"] <- 1
#recoding so that it's objective, specific (1) versus all else (0)
resLIWC$specificity_cthm_01 <-  resLIWC$specificity_cthm
resLIWC$specificity_cthm_01[resLIWC$specificity_cthm == 1 & resLIWC$specificity_cthm != "NA"] <- 0
resLIWC$specificity_cthm_01[resLIWC$specificity_cthm == 2 & resLIWC$specificity_cthm != "NA"] <- 1
#recoding so that it's finances mentioned (1) versus not (0)
resLIWC$finances_cthm_01 <- resLIWC$finances_cthm
resLIWC$finances_cthm_01[resLIWC$finances_cthm > 0 & resLIWC$finances_cthm != "NA"] <- 1
#recoding so that it's substances/behaviors (1) versus not (0)
resLIWC$subuse_cthm_01 <- resLIWC$subuse_cthm
resLIWC$subuse_cthm_01[resLIWC$subuse_cthm > 0 & resLIWC$subuse_cthm != "NA"] <- 1


#trying again
success_humancodes_2 <- glmer(t4Success_01 ~ pronoun_01 +  article_01 + i_01 + ppron_01 + verb_01 + adj_01 + 
                                adverb_01 + conj_01 + negate_01 + number_01 + quant_01 + WC +
                                #adding human content codes
                                weight_cthm + finances_cthm_01 + exercise_cthm + subuse_cthm_01 +
                                specificity_cthm_01 + approach_cthm_01 + freq_cthm_01 +
                                (1|MturkID), data = resLIWC, family = binomial(link=logit), 
                              control=glmerControl(optimizer="bobyqa"), nAGQ = 10)

#human code results
summary(success_humancodes_2)
#r2
r.squaredGLMM(success_humancodes_2,success_basicprop01_2)
#formal test
anova(success_humancodes_2,success_basicprop01_2)

#model 3
success_computercodes_1 <- glmer(t4Success_01 ~ pronoun_01 +  article_01 + i_01 + ppron_01 + verb_01 + adj_01 + 
                                adverb_01 + conj_01 + negate_01 + number_01 + quant_01 + WC +
                                #adding human content codes
                                weight_cthm + finances_cthm_01 + exercise_cthm + subuse_cthm_01 +
                                specificity_cthm_01 + approach_cthm_01 + freq_cthm_01 +
                                #adding LIWC content codes
                                posemo_01 + negemo_01 + social_01 + tentat_01 + certain_01 +
                                health_01 + ingest_01 + achieve_01 + reward_01 + risk_01 +
                                focuspast_01 + focuspresent_01 + focusfuture_01 + time_01 +
                                work_01 + leisure_01 + home_01 + money_01 +
                                (1|MturkID), data = resLIWC, family = binomial(link=logit), 
                              control=glmerControl(optimizer="bobyqa"), nAGQ = 10)

#model failed to converge
#seeing if some categories are too empty
#removing tentat 79 v 1003
#removing certain 72 v 1010
#focus past 45 v 1037
#focus future 1011 v 71
#home 76 v 1006

success_computercodes_2 <- glmer(t4Success_01 ~ pronoun_01 +  article_01 + i_01 + ppron_01 + verb_01 + adj_01 + 
                                   adverb_01 + conj_01 + negate_01 + number_01 + quant_01 + WC +
                                   #adding human content codes
                                   weight_cthm + finances_cthm_01 + exercise_cthm + subuse_cthm_01 +
                                   specificity_cthm_01 + approach_cthm_01 + freq_cthm_01 +
                                   #adding LIWC content codes
                                   posemo_01 + negemo_01 + social_01 +
                                   health_01 + ingest_01 + achieve_01 + reward_01 + risk_01 +
                                   focuspresent_01 + time_01 +
                                   work_01 + leisure_01 + money_01 +
                                   (1|MturkID), data = resLIWC, family = binomial(link=logit), 
                                 control=glmerControl(optimizer="bobyqa"), nAGQ = 10)
#model still failed to converge
#looking at overlap of variables
resLIWC_logisticvars <- resLIWC %>% select(pronoun_01,  article_01, i_01, ppron_01, verb_01, adj_01, 
                                                     adverb_01, conj_01, negate_01, number_01, quant_01, WC,
                                                     weight_cthm, finances_cthm_01, exercise_cthm, subuse_cthm_01,
                                                     specificity_cthm_01, approach_cthm_01, freq_cthm_01,
                                                     posemo_01, negemo_01, social_01,
                                                     health_01, ingest_01, achieve_01, reward_01, risk_01,
                                                     focuspresent_01, time_01,
                                                     work_01, leisure_01, money_01)

corr.test(resLIWC_logisticvars)
corrs <- cor(resLIWC_logisticvars)
corrs[corrs>.7]

#LIWC code results
summary(success_computercodes_2)
#r2
r.squaredGLMM(success_computercodes_2,success_humancodes_2)
#formal test
anova(success_computercodes_2,success_humancodes_2)



#saving results


write.csv(x = resLIWC, file = "theoreticalLIWC_successdata.csv")
