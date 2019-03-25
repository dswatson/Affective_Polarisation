#BES 2014-2018 Wave 1-13 Cleaning Script

#colnames(BES2015_W3_v4.7) <- BES2015_W3_v4.7[1,] # the first row will be the header
#BES2015_W3_v4.7 = BES2015_W3_v4.7[-1,]

#Wave 1

library(data.table)
library(tidyverse)
library(haven)

####The "core" weights weight a smaller, more representative sample 
#### of respondents (~21,000 respondents) in each wave, hence the "NAs".
####The "new" weights (from wave 10 onwards), a YouGov weighting schema is used
### Each wave may be treated as a cross-sectional groupo

BES_2014_W1_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W1_v7.7.csv",
                          stringsAsFactors = FALSE) 
BES_2014_W1_OG <- as.data.table(BES_2014_W1_OG)

BES_2014_W1 <- BES_2014_W1_OG %>%
  select(c("id", "wt_core_W1", "age", "gender", "partyId", 
           "partyIdStrength", "polAttention", "likeCon", "likeLab", 
           "likeLD", "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2014, wave = 1) %>%
  rename(wt_core = wt_core_W1)%>%
  select(14, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)%>%
as.data.table()

BES_2014_W1$partyId[BES_2014_W1$partyId == 1] <- "Con"
BES_2014_W1$partyId[BES_2014_W1$partyId == 2] <- "Lab"
BES_2014_W1$partyId[BES_2014_W1$partyId == 3] <- "LD"
BES_2014_W1$partyId[BES_2014_W1$partyId == 4] <- "SNP"
BES_2014_W1$partyId[BES_2014_W1$partyId == 5] <- "PC"
BES_2014_W1$partyId[BES_2014_W1$partyId == 6] <- "UKIP"
BES_2014_W1$partyId[BES_2014_W1$partyId == 7] <- "Green"
BES_2014_W1$partyId[BES_2014_W1$partyId == 8] <- "BNP"
BES_2014_W1$partyId[BES_2014_W1$partyId == 9] <- "Other"
BES_2014_W1$partyId[BES_2014_W1$partyId == 10] <- NA_integer_

#WAVE2#

BES_2014_W2_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W2_v6.7.csv", 
                          stringsAsFactors = FALSE) 
BES_2014_W2_OG <- as.data.table(BES_2014_W2_OG)
#colnames(BES2015_W3_v4.7) <- BES2015_W3_v4.7[1,] # the first row will be the header
#BES2015_W3_v4.7 = BES2015_W3_v4.7[-1,]

BES_2014_W2 <- BES_2014_W2_OG %>%
  select(c("id", "wt_core_W2", "Age", "gender", "partyId", 
           "partyIdStrength", "polAttention", "likeCon", "likeLab", 
           "likeLD", "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2014, wave = 2) %>%
  rename(age = Age, wt_core = wt_core_W2)%>%
  select(14, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)%>%
  as.data.table()

BES_2014_W2$partyId[BES_2014_W2$partyId == 1] <- "Con"
BES_2014_W2$partyId[BES_2014_W2$partyId == 2] <- "Lab"
BES_2014_W2$partyId[BES_2014_W2$partyId == 3] <- "LD"
BES_2014_W2$partyId[BES_2014_W2$partyId == 4] <- "SNP"
BES_2014_W2$partyId[BES_2014_W2$partyId == 5] <- "PC"
BES_2014_W2$partyId[BES_2014_W2$partyId == 6] <- "UKIP"
BES_2014_W2$partyId[BES_2014_W2$partyId == 7] <- "Green"
BES_2014_W2$partyId[BES_2014_W2$partyId == 8] <- "BNP"
BES_2014_W2$partyId[BES_2014_W2$partyId == 9] <- "Other"
BES_2014_W2$partyId[BES_2014_W2$partyId == 10] <- NA_integer_

#as.character(BES_2014_W2)
#BES_2014_W2[BES_2014_W2 == " "] <- NA_integer_
#as.factor(BES_2014_W2)
#BES_2014_W2 = BES_2014_W2[-1,]

#Wave 3

BES_2014_W3_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W3_v4.7.csv", 
                          stringsAsFactors = FALSE) 
BES_2014_W3_OG <- as.data.table(BES_2014_W3_OG)

BES_2014_W3 <- BES_2014_W3_OG %>%
  select(c("id", "wt_core_W3", "Age", "gender", "partyId", 
           "partyIdStrength", "polAttention", "likeCon", "likeLab", 
           "likeLD", "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2014, wave = 3) %>%
  rename(age = Age, wt_core = wt_core_W3)%>%
  select(14,15,1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,12, 13)%>%
as.data.table()

BES_2014_W3$partyId[BES_2014_W3$partyId == 1] <- "Con"
BES_2014_W3$partyId[BES_2014_W3$partyId == 2] <- "Lab"
BES_2014_W3$partyId[BES_2014_W3$partyId == 3] <- "LD"
BES_2014_W3$partyId[BES_2014_W3$partyId == 4] <- "SNP"
BES_2014_W3$partyId[BES_2014_W3$partyId == 5] <- "PC"
BES_2014_W3$partyId[BES_2014_W3$partyId == 6] <- "UKIP"
BES_2014_W3$partyId[BES_2014_W3$partyId == 7] <- "Green"
BES_2014_W3$partyId[BES_2014_W3$partyId == 8] <- "BNP"
BES_2014_W3$partyId[BES_2014_W3$partyId == 9] <- "Other"
BES_2014_W3$partyId[BES_2014_W3$partyId == 10] <- NA_integer_


#Wave4

BES_2015_W4_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W4_v3.7.csv", 
                          stringsAsFactors = FALSE) 
BES_2015_W4_OG <- as.data.table(BES_2015_W4_OG)

BES_2015_W4 <- BES_2015_W4_OG %>%
  select(c("id", "wt_core_W4", "Age", "gender", "partyId", 
           "partyIdStrength", "polAttention", "likeCon", "likeLab", 
           "likeLD", "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2015, wave = 4) %>%
  rename(age = Age, wt_core = wt_core_W4)%>%
  select(14, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)%>%
as.data.table()

BES_2015_W4$partyId[BES_2015_W4$partyId == 1] <- "Con"
BES_2015_W4$partyId[BES_2015_W4$partyId == 2] <- "Lab"
BES_2015_W4$partyId[BES_2015_W4$partyId == 3] <- "LD"
BES_2015_W4$partyId[BES_2015_W4$partyId == 4] <- "SNP"
BES_2015_W4$partyId[BES_2015_W4$partyId == 5] <- "PC"
BES_2015_W4$partyId[BES_2015_W4$partyId == 6] <- "UKIP"
BES_2015_W4$partyId[BES_2015_W4$partyId == 7] <- "Green"
BES_2015_W4$partyId[BES_2015_W4$partyId == 8] <- "BNP"
BES_2015_W4$partyId[BES_2015_W4$partyId == 9] <- "Other"
BES_2015_W4$partyId[BES_2015_W4$partyId == 10] <- NA_integer_


#Wave5
BES_2015_W5_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W5_v1.0.csv", 
                          stringsAsFactors = FALSE) 
BES_2015_W5_OG <- as.data.table(BES_2015_W5_OG)

BES_2015_W5 <- BES_2015_W5_OG %>%
  select(c("id", "wt_core_W5", "Age", "gender", "partyIdW4", 
             "partyIdStrengthW4", "likeCon", "likeLab", "likeLD",
             "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2015, wave = 5, polAttention= NA_integer_)%>%
  rename(age = Age, 
         partyIdStrength = partyIdStrengthW4, 
         partyId = partyIdW4, 
         wt_core = wt_core_W5) %>%
  select(13, 14, 1, 2, 3, 4, 5, 6, 15, 7, 8, 9, 10,11, 12)%>%
as.data.table()

BES_2015_W5$partyId[BES_2015_W5$partyId == 1] <- "Con"
BES_2015_W5$partyId[BES_2015_W5$partyId == 2] <- "Lab"
BES_2015_W5$partyId[BES_2015_W5$partyId == 3] <- "LD"
BES_2015_W5$partyId[BES_2015_W5$partyId == 4] <- "SNP"
BES_2015_W5$partyId[BES_2015_W5$partyId == 5] <- "PC"
BES_2015_W5$partyId[BES_2015_W5$partyId == 6] <- "UKIP"
BES_2015_W5$partyId[BES_2015_W5$partyId == 7] <- "Green"
BES_2015_W5$partyId[BES_2015_W5$partyId == 8] <- "BNP"
BES_2015_W5$partyId[BES_2015_W5$partyId == 9] <- "Other"
BES_2015_W5$partyId[BES_2015_W5$partyId == 10] <- NA_integer_

#Wave 6

BES_2015_W6_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W6_v3.6.csv", 
                          stringsAsFactors = FALSE) 
BES_2015_W6_OG <- as.data.table(BES_2015_W6_OG)

BES_2015_W6 <- BES_2015_W6_OG %>%
  select(c("id", "wt_core_W6", "Age", "gender", "partyId", 
           "partyIdStrength", "polAttention","likeCon", "likeLab", "likeLD",
           "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2015, wave = 6) %>%
  rename(wt_core = wt_core_W6, age = Age)%>%
  select(14, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)%>%
as.data.table()

BES_2015_W6$partyId[BES_2015_W6$partyId == 1] <- "Con"
BES_2015_W6$partyId[BES_2015_W6$partyId == 2] <- "Lab"
BES_2015_W6$partyId[BES_2015_W6$partyId == 3] <- "LD"
BES_2015_W6$partyId[BES_2015_W6$partyId == 4] <- "SNP"
BES_2015_W6$partyId[BES_2015_W6$partyId == 5] <- "PC"
BES_2015_W6$partyId[BES_2015_W6$partyId == 6] <- "UKIP"
BES_2015_W6$partyId[BES_2015_W6$partyId == 7] <- "Green"
BES_2015_W6$partyId[BES_2015_W6$partyId == 8] <- "BNP"
BES_2015_W6$partyId[BES_2015_W6$partyId == 9] <- "Other"
BES_2015_W6$partyId[BES_2015_W6$partyId == 10] <- NA_integer_

#Wave 7
BES_2016_W7_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W7_v1.3.csv", 
                          stringsAsFactors = FALSE) 
BES_2016_W7_OG <- as.data.table(BES_2016_W7_OG)

BES_2016_W7 <- BES_2016_W7_OG %>%
  select(c("id", "wt_core_W7", "age", "gender", "partyId", 
            "polAttention","likeCon", "likeLab", "likeLD",
           "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2016, wave = 7, partyIdStrength = NA_integer_) %>%
  rename(wt_core = wt_core_W7) %>%
  select(13, 14, 1, 2, 3, 4, 5, 15, 6, 7, 8, 9, 10,11, 12)%>% 
as.data.table()

BES_2016_W7$partyId[BES_2016_W7$partyId == 1] <- "Con"
BES_2016_W7$partyId[BES_2016_W7$partyId == 2] <- "Lab"
BES_2016_W7$partyId[BES_2016_W7$partyId == 3] <- "LD"
BES_2016_W7$partyId[BES_2016_W7$partyId == 4] <- "SNP"
BES_2016_W7$partyId[BES_2016_W7$partyId == 5] <- "PC"
BES_2016_W7$partyId[BES_2016_W7$partyId == 6] <- "UKIP"
BES_2016_W7$partyId[BES_2016_W7$partyId == 7] <- "Green"
BES_2016_W7$partyId[BES_2016_W7$partyId == 8] <- "BNP"
BES_2016_W7$partyId[BES_2016_W7$partyId == 9] <- "Other"
BES_2016_W7$partyId[BES_2016_W7$partyId == 10] <- NA_integer_

#Wave 8
BES_2016_W8_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W8_v1.6-1.csv", 
                          stringsAsFactors = FALSE) 
BES_2016_W8_OG <- as.data.table(BES_2016_W8_OG)

BES_2016_W8 <- BES_2016_W8_OG %>%
  select(c("id", "wt_core_W8", "age", "gender", "partyId", 
           "partyIdStrength", "polAttention","likeCon", "likeLab", "likeLD",
           "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2016, wave = 8) %>%
  rename(wt_core = wt_core_W8) %>%
  select(14, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)%>%
as.data.table()

BES_2016_W8$partyId[BES_2016_W8$partyId == 1] <- "Con"
BES_2016_W8$partyId[BES_2016_W8$partyId == 2] <- "Lab"
BES_2016_W8$partyId[BES_2016_W8$partyId == 3] <- "LD"
BES_2016_W8$partyId[BES_2016_W8$partyId == 4] <- "SNP"
BES_2016_W8$partyId[BES_2016_W8$partyId == 5] <- "PC"
BES_2016_W8$partyId[BES_2016_W8$partyId == 6] <- "UKIP"
BES_2016_W8$partyId[BES_2016_W8$partyId == 7] <- "Green"
BES_2016_W8$partyId[BES_2016_W8$partyId == 8] <- "BNP"
BES_2016_W8$partyId[BES_2016_W8$partyId == 9] <- "Other"
BES_2016_W8$partyId[BES_2016_W8$partyId == 10] <- NA_integer_

#Wave 9

BES_2016_W9_OG <-read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W9_v1.2.csv", 
                stringsAsFactors = FALSE) 
BES_2016_W9_OG <- as.data.table(BES_2016_W9_OG)

BES_2016_W9 <- BES_2016_W9_OG %>%
  select(c("id", "wt_core_W9", "age", "gender", "partyId", 
           "partyIdStrength", "likeCon", "likeLab", "likeLD",
           "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2016, wave = 9, polAttention= NA_integer_) %>%
  rename(wt_core = wt_core_W9) %>%
  select(13, 14, 1, 2, 3, 4, 5, 6, 15, 7, 8, 9, 10, 11,12)%>%
as.data.table()

BES_2016_W9$partyId[BES_2016_W9$partyId == 1] <- "Con"
BES_2016_W9$partyId[BES_2016_W9$partyId == 2] <- "Lab"
BES_2016_W9$partyId[BES_2016_W9$partyId == 3] <- "LD"
BES_2016_W9$partyId[BES_2016_W9$partyId == 4] <- "SNP"
BES_2016_W9$partyId[BES_2016_W9$partyId == 5] <- "PC"
BES_2016_W9$partyId[BES_2016_W9$partyId == 6] <- "UKIP"
BES_2016_W9$partyId[BES_2016_W9$partyId == 7] <- "Green"
BES_2016_W9$partyId[BES_2016_W9$partyId == 8] <- "BNP"
BES_2016_W9$partyId[BES_2016_W9$partyId == 9] <- "Other"
BES_2016_W9$partyId[BES_2016_W9$partyId == 10] <- NA_integer_

#Wave10

BES_2016_W10_OG <- read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W10_v0.6.csv", 
                           stringsAsFactors = FALSE) 
BES_2016_W10_OG <- as.data.table(BES_2016_W10_OG)

BES_2017_W10 <- BES_2017_W10_OG %>%
  select(c("id", "wt_new_", "age", "gender", "partyId", 
           "partyIdStrength","polAttention", "likeCon", "likeLab", "likeLD",
           "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2016, wave = 10) %>%
  rename(wt_core = wt_new_) %>%
  select(14, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)%>%
as.data.table()

BES_2016_W10$partyId[BES_2016_W10$partyId == 1] <- "Con"
BES_2016_W10$partyId[BES_2016_W10$partyId == 2] <- "Lab"
BES_2016_W10$partyId[BES_2016_W10$partyId == 3] <- "LD"
BES_2016_W10$partyId[BES_2016_W10$partyId == 4] <- "SNP"
BES_2016_W10$partyId[BES_2016_W10$partyId == 5] <- "PC"
BES_2016_W10$partyId[BES_2016_W10$partyId == 6] <- "UKIP"
BES_2016_W10$partyId[BES_2016_W10$partyId == 7] <- "Green"
BES_2016_W10$partyId[BES_2016_W10$partyId == 8] <- "BNP"
BES_2016_W10$partyId[BES_2016_W10$partyId == 9] <- "Other"
BES_2016_W10$partyId[BES_2016_W10$partyId == 10] <- NA_integer_

#Wave11

BES_2017_W11_OG <- read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W11_v1.2.csv", 
                            stringsAsFactors = FALSE) 
BES_2017_W11_OG <- as.data.table(BES_2017_W11_OG)
BES_2017_W11 <- BES_2017_W11_OG %>%
  select(c("id", "wt_new_W11", "age", "gender", "partyId", 
           "partyIdStrength","polAttention", "likeCon", "likeLab", "likeLD",
           "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2017, wave = 11) %>%
  rename(wt_core = wt_new_W11) %>%
  select(14, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)%>%
  as.data.table()

BES_2017_W11$partyId[BES_2017_W11$partyId == 1] <- "Con"
BES_2017_W11$partyId[BES_2017_W11$partyId == 2] <- "Lab"
BES_2017_W11$partyId[BES_2017_W11$partyId == 3] <- "LD"
BES_2017_W11$partyId[BES_2017_W11$partyId == 4] <- "SNP"
BES_2017_W11$partyId[BES_2017_W11$partyId == 5] <- "PC"
BES_2017_W11$partyId[BES_2017_W11$partyId == 6] <- "UKIP"
BES_2017_W11$partyId[BES_2017_W11$partyId == 7] <- "Green"
BES_2017_W11$partyId[BES_2017_W11$partyId == 8] <- "BNP"
BES_2017_W11$partyId[BES_2017_W11$partyId == 9] <- "Other"
BES_2017_W11$partyId[BES_2017_W11$partyId == 10] <- NA_integer_

#Wave 12

BES_2017_W12_OG <- read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2015_W12_v1.2.csv", 
                            stringsAsFactors = FALSE) 
BES_2017_W12_OG <- as.data.table(BES_2017_W12_OG)

BES_2017_W12 <- BES_2017_W12_OG %>%
  select(c("id", "wt_new_W12", "age", "gender", "partyId", 
           "partyIdStrength","likeCon", "likeLab", "likeLD",
           "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2017, wave = 12, polAttention = NA_integer_) %>%
  rename(wt_core = wt_new_W12) %>%
  select(13, 14, 1, 2, 3, 4, 5, 6, 15, 7, 8, 9, 10, 11,12)%>%
  as.data.table()

BES_2017_W12$partyId[BES_2017_W12$partyId == 1] <- "Con"
BES_2017_W12$partyId[BES_2017_W12$partyId == 2] <- "Lab"
BES_2017_W12$partyId[BES_2017_W12$partyId == 3] <- "LD"
BES_2017_W12$partyId[BES_2017_W12$partyId == 4] <- "SNP"
BES_2017_W12$partyId[BES_2017_W12$partyId == 5] <- "PC"
BES_2017_W12$partyId[BES_2017_W12$partyId == 6] <- "UKIP"
BES_2017_W12$partyId[BES_2017_W12$partyId == 7] <- "Green"
BES_2017_W12$partyId[BES_2017_W12$partyId == 8] <- "BNP"
BES_2017_W12$partyId[BES_2017_W12$partyId == 9] <- "Other"
BES_2017_W12$partyId[BES_2017_W12$partyId == 10] <- NA_integer_

#Wave13

BES_2017_W13_OG <- read.csv("~/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/BES2017_W13_v1.2.csv", 
                            stringsAsFactors = FALSE) 
BES_2017_W13_OG <- as.data.table(BES_2017_W13_OG)

BES_2017_W13 <- BES_2017_W13_OG %>%
  select(c("id", "wt_new_W13", "age", "gender", "partyId", 
           "partyIdStrength", "polAttention", "likeCon", "likeLab", "likeLD",
           "likeSNP", "likePC", "leftRight")) %>%
  mutate(year = 2017, wave = 13) %>%
  rename(wt_core = wt_new_W13) %>%
  select(14, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)%>%
as.data.table()

BES_2017_W13$partyId[BES_2017_W13$partyId == 1] <- "Con"
BES_2017_W13$partyId[BES_2017_W13$partyId == 2] <- "Lab"
BES_2017_W13$partyId[BES_2017_W13$partyId == 3] <- "LD"
BES_2017_W13$partyId[BES_2017_W13$partyId == 4] <- "SNP"
BES_2017_W13$partyId[BES_2017_W13$partyId == 5] <- "PC"
BES_2017_W13$partyId[BES_2017_W13$partyId == 6] <- "UKIP"
BES_2017_W13$partyId[BES_2017_W13$partyId == 7] <- "Green"
BES_2017_W13$partyId[BES_2017_W13$partyId == 8] <- "BNP"
BES_2017_W13$partyId[BES_2017_W13$partyId == 9] <- "Other"
BES_2017_W13$partyId[BES_2017_W13$partyId == 10] <- NA_integer_

#Merging all datasets together

BES_2014_2017 <- bind_rows(BES_2014_W1, BES_2014_W2, BES_2014_W3, BES_2015_W4,
                           BES_2015_W4, BES_2015_W5, BES_2015_W6, BES_2015_W6,
                           BES_2016_W7, BES_2016_W8, BES_2016_W9,
                           BES_2017_W11, BES_2017_W12, BES_2017_W13)

#Isolate NAs in datasets while preserving the values of weights
tempBES <-  BES_2014_2017 %>%
  select(-wt_core)%>% 
as.data.table()

tempBES[tempBES == 9999] <- NA_integer_

colnames(BES_2014_2017)




