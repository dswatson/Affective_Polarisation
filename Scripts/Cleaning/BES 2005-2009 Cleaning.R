#-----------------------------------------------------------------------------------------#
# BES 2005-2009 Cleaning Script
#-----------------------------------------------------------------------------------------#

library(data.table)
library(tidyverse)
library(ggplot2)
library(haven)

BES_2005_2009_OG <- read_sav("~/Downloads/2005-2009BES6WavePanel.sav")
BES_2005_2009_OG <- as.data.table(BES_2005_2009_OG)

## ! NEED TO TURN LABELLED DATA TO NUMERIC ! ##

#Recode year of birth variable as NA
BES_2005_2009_OG[pre_q148 == 89, pre_q148 := NA_integer_]
BES_2005_2009_OG[cam_q83 == 89, cam_q83 := NA_integer_]
BES_2005_2009_OG[post_q66 == 89, post_q66 := NA_integer_]
BES_2005_2009_OG[oyo_q100 %in% c(90,999), oyo_q100 := NA_integer_]
BES_2005_2009_OG[y08_q92 == 90, y08_q92 := NA_integer_]
BES_2005_2009_OG[y09_q83 == 93, y09_q83 := NA_integer_]

BES_2005_W1_OG <- BES_2005_2009_OG %>%
  select(c("besid","pre_w8", starts_with("pre"))) %>%
as.data.table() 
BES_2005_W1 <- BES_2005_W1_OG %>%
  rename(id = besid,
          wt_core = pre_w8,
          y_birth = pre_q148,
          gender = pre_q180,
          partyId = pre_q29,
          partyIdStrength = pre_q33,
          polAttention = pre_q141,
          likeCon = pre_q85,
          likeLab = pre_q84,
          likeLD = pre_q86,
          likeSNP = pre_q89,
          likePC = pre_q90)%>%
  select(id, wt_core, y_birth, gender, partyId, partyIdStrength,
         polAttention, likeCon, likeLab, likeLD, likeSNP, likePC)%>%
  mutate(year = 2005, wave = 1, x = 1899, y_birth_new = !is.na(as.integer(y_birth)) + x, 
         age = year - y_birth_new) %>%
  select(13, 14, 1, 2, 17, 4, 7, 5, 6, 8, 9, 10, 11, 12)%>%
as.data.table() 


BES_2005_W2_OG<- BES_2005_2009_OG %>%
  select(c("besid","w8_full", starts_with("cam"))) %>%
as.data.table() 
BES_2005_W2 <-  BES_2005_W2_OG %>%
  rename(id = besid,
           wt_core = w8_full,
           y_birth = cam_q83,
           gender = cam_q85,
           partyId = cam_q18,
           partyIdStrength = cam_q22,
           polAttention = cam_q64,
           likeCon = cam_q50,
           likeLab = cam_q49,
           likeLD = cam_q51,
           likeSNP = cam_q52,
           likePC = cam_q50)%>%
  mutate(y_birth, year = 2005, wave = 2, x = 1899, y_birth_new = !is.na(as.integer(y_birth)) + x, 
         age = year - y_birth_new) %>%
  select(13, 14, 1, 2, 17, 4, 7, 5, 6, 8, 9, 10, 11, 12)%>%
  as.data.table() 

BES_2005_W3_OG<- BES_2005_2009_OG %>%
  select(c("besid","post_w8", starts_with("post")))%>%
as.data.table() 
BES_2005_W3 <- BES_2005_W3_OG %>%
  rename(id = besid,
         wt_core = post_w8,
         y_birth = post_q66,
         gender = post_q69,
         partyId = post_q13,
         partyIdStrength = post_q17,
         polAttention = post_q49,
         likeCon = post_q36,
         likeLab = post_q35,
         likeLD = post_q37,
         likeSNP = post_q38,
         likePC = post_q39)%>%
  mutate(year = 2005, wave = 3, x = 1899, y_birth_new = !is.na(as.integer(y_birth)) + x, 
         age = year - y_birth_new) %>%
  select(13, 14, 1, 2, 17, 4, 7, 5, 6, 8, 9, 10, 11, 12)%>%
  as.data.table() 

BES_2006_W1_OG<- BES_2005_2009_OG %>%
  select(c("besid","oyo_w8", starts_with("oyo"))) %>%
as.data.table() 
BES_2006_W1 <- BES_2006_W1_OG %>%
  rename(id = besid,
         wt_core = oyo_w8,
         y_birth = oyo_q100,
         gender = oyo_q102,
         partyId = oyo_q57,
         partyIdStrength = oyo_q61,
         polAttention = oyo_q61,
         likeCon = oyo_q68,
         likeLab = oyo_q67,
         likeLD = oyo_q69,
         likeSNP = oyo_q70,
         likePC = oyo_q71)%>%
  mutate(year = 2006, wave = 1, x = 1899, y_birth_new = !is.na(as.integer(y_birth)) + x, 
         age = year - y_birth_new) %>%
  select(13, 14, 1, 2, 17, 4, 7, 5, 6, 8, 9, 10, 11, 12)%>%
  as.data.table() 

BES_2008_W1_OG<- BES_2005_2009_OG %>%
  select(c("besid","y08_w8_p", starts_with("y08"))) %>%
as.data.table()
BES_2008_W1 <- BES_2008_W1_OG %>%
  rename(id = besid,
         wt_core = y08_w8_p,
         y_birth = y08_q92,
         gender = y08_q94,
         partyId = y08_q27,
         partyIdStrength = y08_q31,
         polAttention = y08_q63,
         likeCon = y08_q38,
         likeLab = y08_q37,
         likeLD = y08_q39,
         likeSNP = y08_q45,
         likePC = y08_q46)%>%
  mutate(year = 2008, wave = 1, x = 1899, y_birth_new = !is.na(as.integer(y_birth)) + x, 
         age = year - y_birth_new) %>%
  select(13, 14, 1, 2, 17, 4, 7, 5, 6, 8, 9, 10, 11, 12)%>%
  as.data.table() 

BES_2009_W1_OG<- BES_2005_2009_OG %>%
  select("besid", "y09_w8_p", starts_with("y09")) %>%
  mutate_if(
    is.labelled,
    funs(as_factor(.))
  ) %>%
as.data.table() 
BES_2009_W1 <- BES_2009_W1_OG %>%
  rename(id = besid,
       wt_core = y09_w8_p,
       d_birth = y09_q83,
       gender = y09_q85,
       partyId = y09_q15,
       partyIdStrength = y09_q19,
       polAttention = y09_q46,
       likeCon = y09_q27,
       likeLab = y09_q26,
       likeLD = y09_q28,
       likeSNP = y09_q29,
       likePC = y09_q30)%>%
  mutate(year = 2009, wave = 1, x = 1899, y_birth_new = !is.na(as.integer(d_birth)) + x, 
         age = year - y_birth_new) %>%
  select(13, 14, 1, 2, 17, 4, 7, 5, 6, 8, 9, 10, 11, 12)%>%
  as.data.table() 

BES_2005_2009 <- bind_rows(BES_2005_W1, BES_2005_W2, BES_2005_W3, BES_2006_W1, BES_2008_W1, BES_2009_W1)

