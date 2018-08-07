#-----------------------------------------------------------------------------------------#
# CSES 1997-2005-2015 Cleaning Script
#-----------------------------------------------------------------------------------------#

library(data.table)
library(tidyverse)

#-----------------------------------------------------------------------------------------#
# CSES Module 1 (1997) 
#-----------------------------------------------------------------------------------------#

#STEP1: Identify variables of interest and renaming columns#
load("/Users/nahemamarchal/Dropbox/1. DPHIL/4. Papers/IPP2018/CSES/Datasets/cses1.rdata")
cses1_clean <- as.data.table(cses1_clean)
cses1_clean <- cses1 %>%
  select (A1005, A1010_1, A1010_2, A1010_3, A1006_NAM, A1008, A2001, A2002, 
          A2003, A2007, A2023, A2024, A2025, A3004, A3005_1, A3010, A3011,
          A3012, A3020_A, A3020_B, A3020_C, A3020_D, A3020_E, A3031, A3032_A,
          A3032_B, A3032_C, A3032_D, A3032_E, A5004_A, A5004_B, A5004_C, 
          A5004_D, A5004_E) %>%
  rename(id = A1005,
         sample_wt = A1010_1,
         dem_wt = A1010_2,
         pol_wt = A1010_3,
         country = A1006_NAM ,
         year = A1008 ,
         age = A2001 ,
         gender = A2002,
         education = A2003,
         employment = A2007,
         polq1 = A2023,
         polq2 = A2024, 
         polq3 = A2025, 
         closeness = A3004, 
         partyId1 = A3005_1,
         closeness2 = A3010,
         partyId2 = A3011,
         partyIdStrength = A3012, 
         likeLab = A3020_A ,  
         likeCon = A3020_B,  
         likeLD = A3020_C,
         likeSNP = A3020_D,
         likePC = A3020_E,
         leftRight = A3031,
         lrLab = A3032_A, 
         lrCon = A3032_B,
         lrLD = A3032_C, 
         lrSNP = A3032_D, 
         lrPC =A3032_E,
         lrLab_real = A5004_A, 
         lrCon_real = A5004_B,
         lrLD_real =A5004_C, 
         lrSNP_real = A5004_D, 
         lrPC_real = A5004_E
         ) %>%
  filter(country == "Great Britain") %>%
  as.data.table()

#STEP 2: Highlighting missing and special values#
cses1_clean[age == 999, age := NA_integer_]
cses1_clean[gender  == 9 , gender := NA_integer_]
cses1_clean[employment  == 99, employment := NA_integer_]
cses1_clean[education  == 99, education := NA_integer_]
cses1_clean[polq1 == 99, polq1 := NA_integer_] 
cses1_clean[polq2== 99, polq2 := NA_integer_] 
cses1_clean[polq3 == 99, polq3 := NA_integer_] 
cses1_clean[closeness %in% c(9,0), closeness := NA_integer_] 
cses1_clean[partyId1 %in% c(99,00), partyId1 := NA_integer_] 
cses1_clean[partyId2 %in% c(99,00), partyId2 := NA_integer_] 
cses1_clean[closeness2 %in% c(9,0), closeness2 := NA_integer_]
cses1_clean[partyIdStrength %in% c(9,0), partyIdStrength:= NA_integer_]
cses1_clean[likeLab == 99, likeLab := NA_integer_] 
cses1_clean[likeCon == 99, likeCon := NA_integer_] 
cses1_clean[likeLD == 99, likeLD := NA_integer_] 
cses1_clean[likeSNP == 99, likeSNP := NA_integer_] 
cses1_clean[likePC == 99, likePC := NA_integer_] 
cses1_clean[leftRight == 99, leftRight := NA_integer_] 
cses1_clean[lrCon  == 99, lrCon  := NA_integer_] 
cses1_clean[lrLab  == 99, lrLab  := NA_integer_] 
cses1_clean[lrLD  == 99, lrLD  := NA_integer_] 
cses1_clean[lrSNP == 99, lrSNP  := NA_integer_] 
cses1_clean[lrPC  == 99, lrCon  := NA_integer_] 
cses1_clean[lrCon_real== 99, lrCon_real  := NA_integer_] 
cses1_clean[lrLab_real == 99, lrLab_real  := NA_integer_] 
cses1_clean[lrLD_real == 99, lrLD_real   := NA_integer_] 
cses1_clean[lrSNP_real == 99, lrSNP_real  := NA_integer_] 
cses1_clean[lrPC_real  == 99, lrPC_real  := NA_integer_] 


#STEP3: Transform variables#
#Turning inconsistent answers into 0#
cses1_clean$closeness[cses1_clean$closeness== 5] <- 0
cses1_clean$closeness2[cses1_clean$closeness2== 5] <- 0

#Create a single variable "closeness_new" by merging the values of closeness and closeness2
cses1_clean$closeness_new <- cses1_clean$closeness
cses1_clean[cses1_clean$closeness2== 1 ,]$closeness_new <- 1

#Inspecting data set to check how many respondents are partisans
summary(cses1_clean)
cses1_clean <- as.tibble(cses1_clean)
not_close <- which(is.na(cses1_clean$closeness))
cses1_clean[not_close, ]

#Create 2 separate data tables for partisans and non-partisans
cses1_partisan <- cses1_clean [-not_close,]
cses1_partisan<- as.data.table(cses1_partisan)

#Create political knowledge composite variable
cses1_partisan[polq1 %in% c(2,8), polq1 := 0]
cses1_partisan[polq2 %in% c(2,8), polq2 := 0] 
cses1_partisan[polq3 %in% c(2,8), polq3 := 0]
cses1_partisan <- cses1_partisan %>%
  mutate(polknowledge = polq1 + polq2 + polq3)%>%
  
#Create a single variable "partyId_new" by merging the values of partyId1 and partyId 2
  mutate (partyId = ifelse(is.na(partyId1), partyId2, partyId1))%>%
  as.data.table() 

#Recode education variable
cses1_partisan$education <- as.character(cses1_partisan$education)
cses1_partisan[education == '1', education := '0_new']
cses1_partisan[education == '2', education := '1_new']
cses1_partisan[education == '3', education := '2_new']
cses1_partisan[education %in% c('4','5'), education := '3_new']
cses1_partisan[education %in% c('6','7'), education := '4_new']
cses1_partisan[education == '8', education := '5_new']
cses1_partisan$education <- gsub( '_new', '', cses1_partisan$education)
cses1_partisan$education <- as.integer(cses1_partisan$education)


#Recode employment variable
cses1_partisan$employment <- as.character(cses1_partisan$employment)
cses1_partisan[employment %in% c('10', '9', '8', '4'), employment := '0_new']
cses1_partisan[employment == '5', employment := '1_new']
cses1_partisan[employment == '7', employment := '2_new']
cses1_partisan[employment == '6', employment := '3_new']
cses1_partisan[employment %in% c('2', '3'), employment := '4_new']
cses1_partisan[employment == '1', employment := '5_new']
cses1_partisan$employment <- gsub('_new', '', cses1_partisan$employment)
cses1_partisan$employment <- as.integer(cses1_partisan$employment)


#Rename political parties
cses1_partisan$partyId[cses1_partisan$partyId== 0] <- NA_character_
cses1_partisan$partyId[cses1_partisan$partyId== 99] <- NA_character_
cses1_partisan$partyId[cses1_partisan$partyId== 1] <- "Con"
cses1_partisan$partyId[cses1_partisan$partyId== 2] <- "Lab"
cses1_partisan$partyId[cses1_partisan$partyId== 3] <- "LD"
cses1_partisan$partyId[cses1_partisan$partyId== 4] <- "SNP"
cses1_partisan$partyId[cses1_partisan$partyId== 5] <- "PC"
cses1_partisan$partyId[cses1_partisan$partyId== 7] <- "Other"

#cses1_partisan <- subset(cses1_partisan, select =c("year", "id", "sample_wt", "age", "gender", "education", "employment", "polknowledge", "closeness_new",
                                                   #"partyId", "partyIdStrength", "likeCon",  "likeLab", "likeLD", 
                                                  # "likeSNP", "likePC", "leftRight"))

#Remove inconsistent answers to the "Do you feel close to a political party?" questions        
cses1_partisan <- cses1_partisan %>%    
  filter (closeness_new!=6)%>%
  as.data.table() 

#-----------------------------------------------------------------------------------------#
# CSES Module 2 (2005) 
#-----------------------------------------------------------------------------------------#


#STEP1: Selecting relevant variables and renaming columns#

load("/Users/nahemamarchal/Dropbox/1. DPHIL/4. Papers/IPP2018/CSES/Datasets/cses2.rdata")
cses2_clean <- as.data.table(cses2_clean)

cses2_clean <- cses2 %>%
  select (B1005, B1010_1, B1010_2, B1010_3, B1006_NAM, B1008, B2001, B2002, B2003, 
          B2010, B3047_1, B3047_2, B3047_3, B3028, B3029_1, B3034, B3035, 
          B3036, B3037_A, B3037_B, B3037_C, B3037_D, B3037_E, B3045, B3038_A ,
          B3038_B, B3038_C, B3038_D, B3038_E, B5018_A ,B5018_B, B5018_C, B5018_D, B5018_E) %>%
  rename(id = B1005,
         sample_wt = B1010_1,
         dem_wt = B1010_2,
         pol_wt = B1010_3,
         country = B1006_NAM,
         year = B1008 ,
         age = B2001 ,
         gender = B2002,
         education = B2003,
         employment = B2010,
         polq1 = B3047_1,
         polq2 = B3047_2, 
         polq3 = B3047_3, 
         closeness = B3028, 
         partyId1 =  B3029_1,
         closeness2 = B3034,
         partyId2 = B3035,
         partyIdStrength = B3036, 
         likeLab = B3037_A ,  
         likeCon = B3037_B,  
         likeLD = B3037_C,
         likeSNP =  B3037_D,
         likePC =  B3037_E,
         leftRight = B3045,
         lrLab = B3038_A ,
         lrCon = B3038_B,
         lrLD = B3038_C, 
         lrSNP = B3038_D, 
         lrPC =B3038_E,
         lrLab_real =  B5018_A ,
         lrCon_real =  B5018_B, 
         lrLD_real = B5018_C, 
         lrSNP_real =  B5018_D, 
         lrPC_real =  B5018_E) %>%
  filter(country == "Great Britain") %>%
  as.data.table()

#Highlighting missing values
cses2_clean[age %in% c(999,997), age := NA_integer_]
cses2_clean[gender  %in% c(9, 7 ), gender := NA_integer_]
cses2_clean[employment  %in% c(99, 97), employment := NA_integer_]
cses2_clean[education  %in% c(99, 97), education := NA_integer_]
cses2_clean[polq1 %in% c(9,7), polq1 := NA_integer_] 
cses2_clean[polq2 %in% c(9,7), polq2 := NA_integer_] 
cses2_clean[polq3 %in% c(9,7), polq3 := NA_integer_] 
cses2_clean[closeness %in% c(9, 7), closeness := NA_integer_] 
cses2_clean[partyId1 %in% c(99, 97), partyId1 := 0] 
cses2_clean[closeness2 %in% c(9, 7), closeness2 := NA_integer_]
cses2_clean[partyId2 %in% c(99, 97), partyId2 := 0] 
cses2_clean[partyIdStrength %in% c(9, 7), partyIdStrength:= NA_integer_]
cses2_clean[likeLab %in% c(99, 97), likeLab := NA_integer_] 
cses2_clean[likeCon %in% c(99, 97), likeCon := NA_integer_] 
cses2_clean[likeLD %in% c(99, 97), likeLD := NA_integer_] 
cses2_clean[likeSNP %in% c(99, 97), likeSNP := NA_integer_] 
cses2_clean[likePC %in% c(99, 97), likePC := NA_integer_] 
cses2_clean[leftRight %in% c(99, 97), leftRight := NA_integer_] 
cses2_clean[lrLab  %in% c(99, 97), lrLab  := NA_integer_] 
cses2_clean[lrLD  %in% c(99, 97), lrLD  := NA_integer_] 
cses2_clean[lrSNP %in% c(99, 97), lrSNP  := NA_integer_] 
cses2_clean[lrPC  %in% c(99, 97), lrCon  := NA_integer_] 
cses2_clean[lrCon_real== 99, lrCon_real  := NA_integer_] 
cses2_clean[lrLab_real == 99, lrLab_real  := NA_integer_] 
cses2_clean[lrLD_real == 99, lrLD_real   := NA_integer_] 
cses2_clean[lrSNP_real == 99, lrSNP_real  := NA_integer_] 
cses2_clean[lrPC_real  == 99, lrPC_real  := NA_integer_] 


#Reassign values
cses2_clean$closeness[cses2_clean$closeness== 2] <- 0
cses2_clean$closeness2[cses2_clean$closeness2== 2] <- 0

#Create a single variable "closeness_new" by merging the values of closeness and closeness2
cses2_clean$closeness_new <- cses2_clean$closeness
cses2_clean[cses2_clean$closeness2== 1 ,]$closeness_new <- 1

#Create 2 separate datatables for partisans and non-partisans
not_close <- which(is.na(cses2_clean$closeness))
cses2_clean[not_close, ]
cses2_partisan <- cses2_clean [-not_close,]
cses2_partisan<- as.data.table(cses2_partisan)

#Create political knowledge composite variable
cses2_partisan[polq1 %in% c(8,2), polq1 := 0]
cses2_partisan[polq2 %in% c(8,2), polq2 := 0] 
cses2_partisan[polq3 %in% c(8,2), polq3 := 0]
cses2_partisan <- cses2_partisan %>%
  mutate(polknowledge = polq1 + polq2 + polq3)%>%
  
#Create a single variable "partyId_new" by merging the values of partyId and partyId 2
  mutate (partyId = ifelse(cses2_partisan$partyId2 == 0, cses2_partisan$partyId1, cses2_partisan$partyId2))%>%
  as.data.table()    

#Recode education variable
cses2_partisan$education <- as.character(cses2_partisan$education)
cses2_partisan[education == '1', education := '0_new']
cses2_partisan[education == '2', education := '1_new']
cses2_partisan[education == '3', education := '2_new']
cses2_partisan[education %in% c('4','5'), education := '3_new']
cses2_partisan[education %in% c('6','7'), education := '4_new']
cses2_partisan[education == '8', education := '5_new']
cses2_partisan$education <- gsub( '_new', '', cses2_partisan$education)
cses2_partisan$education <- as.integer(cses2_partisan$education)


#Recode employment variable
cses2_partisan$employment <- as.character(cses2_partisan$employment)
cses2_partisan[employment %in% c('10', '9', '8', '4'), employment := '0_new']
cses2_partisan[employment == '5', employment := '1_new']
cses2_partisan[employment == '7', employment := '2_new']
cses2_partisan[employment == '6', employment := '3_new']
cses2_partisan[employment %in% c('2', '3'), employment := '4_new']
cses2_partisan[employment == '1', employment := '5_new']
cses2_partisan$employment <- gsub('_new', '', cses2_partisan$employment)
cses2_partisan$employment <- as.integer(cses2_partisan$employment)

#Rename political parties
cses2_partisan$partyId[cses2_partisan$partyId== 0] <- NA_character_
cses2_partisan$partyId[cses2_partisan$partyId== 99] <- NA_character_
cses2_partisan$partyId[cses2_partisan$partyId== 1] <- "Lab"
cses2_partisan$partyId[cses2_partisan$partyId== 2] <- "Con"
cses2_partisan$partyId[cses2_partisan$partyId== 3] <- "LD"
cses2_partisan$partyId[cses2_partisan$partyId== 4] <- "SNP"
cses2_partisan$partyId[cses2_partisan$partyId== 5] <- "PC"
cses2_partisan$partyId[cses2_partisan$partyId== 6] <- "Other"


# mutate (closeness_new = ifelse(cses2_clean$closeness2 == "YES", cses2_clean$closeness2,
# ifelse(is.na(cses2_clean$closeness2), cses2_clean$closeness, cses2_clean$closeness2))) %>%
#cses2_partisan <- subset(cses2_partisan, select =c("year", "id", "sample_wt", "age", "gender", "education", "employment", "polknowledge", "closeness_new",
                                                   #"partyId", "partyIdStrength",  "likeCon", "likeLab","likeLD",
                                                   #"likeSNP", "likePC", "leftRight"))




#-----------------------------------------------------------------------------------------#
# CSES Module 4 (2015) 
#-----------------------------------------------------------------------------------------#

#STEP1: Selecting relevant variables and renaming columns#
load("/Users/nahemamarchal/Dropbox/1. DPHIL/4. Papers/IPP2018/CSES/Datasets/cses4.rdata")
cses4_clean <- as.data.table(cses4_clean)
cses4_clean <- cses4 %>%
  select (D1005, D1010_1, D1010_2, D1010_3, D1006_NAM, D1008, D2001_Y, D2002, D2003, 
          D2010, D3025_1_A, D3025_2_A, D3025_3_A, D3018_1, D3018_2, D3018_3, D3018_4, 
          D3011_B , D3011_A, D3011_D, D3011_E, D3011_G, D3014, D3013_B, D3013_A, D3013_D, 
          D3013_E, D3013_G, D5017_B , D5017_A , D5017_D, D5017_E, D5017_G) %>%
  rename(id = D1005,
         sample_wt = D1010_1,
         dem_wt = D1010_2,
         pol_wt = D1010_3,
         country = D1006_NAM ,
         year = D1008 ,
         age_year = D2001_Y ,
         gender = D2002,
         education = D2003,
         employment = D2010,
         polq1 = D3025_1_A,
         polq2 = D3025_2_A, 
         polq3 = D3025_3_A, 
         closeness = D3018_1, 
         closeness2 = D3018_2,
         partyId = D3018_3,
         partyIdStrength = D3018_4, 
         likeLab = D3011_B ,  
         likeCon = D3011_A,  
         likeLD = D3011_D , 
         likeSNP = D3011_E,
         likePC = D3011_G,
         leftRight = D3014,
         lrLab =  D3013_B  ,
         lrCon =   D3013_A,
         lrLD =   D3013_D, 
         lrSNP =   D3013_E, 
         lrPC =  D3013_G,
         lrLab_real =   D5017_B , 
         lrCon_real =   D5017_A , 
         lrLD_real = D5017_D, 
         lrSNP_real =  D5017_E, 
         lrPC_real =  D5017_G) %>%
  filter(country == "Great Britain") %>%
  as.data.table()
#this turns your dataframe into a datatable again

#STEP 2: Highlighting missing values#

#DF [i, j, by]
#:= NA_integer_] reassign a value in datatable

cses4_clean[age_year %in% c(9999,9997), age_year := NA_integer_]
cses4_clean[gender  %in% c(9, 7), gender := NA_integer_]
cses4_clean[employment  %in% c(99, 97), employment := NA_integer_]
cses4_clean[education  %in% c(99, 97), education := NA_integer_]
cses4_clean[polq1 %in% c(9, 7), polq1 := NA_integer_] 
cses4_clean[polq2 %in% c(9, 7), polq2 := NA_integer_] 
cses4_clean[polq3 %in% c(9, 7), polq3 := NA_integer_] 
cses4_clean[closeness %in% c(9, 7), closeness := NA_integer_] 
cses4_clean[partyId %in% c(99, 97), partyId := NA_integer_] 
cses4_clean[closeness2 %in% c(9, 7), closeness2 := NA_integer_]
cses4_clean[partyIdStrength %in% c(9, 7), partyIdStrength:= NA_integer_]
cses4_clean[likeLab %in% c(99, 97), likeLab := NA_integer_] 
cses4_clean[likeCon %in% c(99, 97), likeCon := NA_integer_] 
cses4_clean[likeLD %in% c(99, 97), likeLD := NA_integer_] 
cses4_clean[likeSNP %in% c(99, 97), likeSNP := NA_integer_] 
cses4_clean[likePC %in% c(99, 97), likePC := NA_integer_] 
cses4_clean[leftRight%in% c(99, 97), leftRight := NA_integer_] 
cses4_clean[lrLab  %in% c(99, 97), lrLab  := NA_integer_] 
cses4_clean[lrLD  %in% c(99, 97), lrLD  := NA_integer_] 
cses4_clean[lrSNP %in% c(99, 97), lrSNP  := NA_integer_] 
cses4_clean[lrPC  %in% c(99, 97), lrCon  := NA_integer_] 
cses4_clean[lrCon_real== 99, lrCon_real  := NA_integer_] 
cses4_clean[lrLab_real == 99, lrLab_real  := NA_integer_] 
cses4_clean[lrLD_real == 99, lrLD_real   := NA_integer_] 
cses4_clean[lrSNP_real == 99, lrSNP_real  := NA_integer_] 
cses4_clean[lrPC_real  == 99, lrPC_real  := NA_integer_] 

#STEP3: Transform variables#

#Create age variable
#dplyr
cses4_clean <- cses4_clean %>%
  mutate(age = year - age_year) %>%
  as.data.table()

#Reassign values
cses4_clean$closeness[cses4_clean$closeness== 5] <- 0
cses4_clean$closeness2[cses4_clean$closeness2== 5] <- 0


#Create a single variable "closeness_new" by merging the values of closeness and closeness2
cses4_clean$closeness_new <- cses4_clean$closeness
cses4_clean[cses4_clean$closeness2== 1 ,]$closeness_new <- 1

not_close <- which(is.na(cses4_clean$closeness))
cses4_clean[not_close, ]

#Create 2 separate tibbles for partisans and non-partisans
cses4_partisan <- cses4_clean [-not_close,]
cses4_partisan<- as.data.table(cses4_partisan)

#Create political knowledge composite variable
cses4_partisan[polq1 %in% c(8,5), polq1 := 0]
cses4_partisan[polq2 %in% c(8,5), polq2 := 0] 
cses4_partisan[polq3 %in% c(8,5), polq3 := 0]
cses4_partisan <- cses4_partisan %>%
  mutate(polknowledge = polq1 + polq2 + polq3)%>%
  as.data.table()  

#Recode education variable
cses4_partisan$education <- as.character(cses4_partisan$education)
cses4_partisan[education == '96', education := '0_new']
cses4_partisan[education == '2', education := '1_new']
cses4_partisan[education %in% c('3','4'), education := '2_new']
cses4_partisan[education == '5', education := '3_new']
cses4_partisan[education == '6', education := '4_new']
cses4_partisan[education %in% c('7','8'), education := '5_new']
cses4_partisan$education <- gsub( '_new', '', cses4_partisan$education)
cses4_partisan$education <- as.integer(cses4_partisan$education)


#Recode employment variable
cses4_partisan$employment <- as.character(cses4_partisan$employment)
cses4_partisan[employment %in% c('8','9', '11', '12'), employment := '0_new']
cses4_partisan[employment == '5', employment := '1_new']
cses4_partisan[employment == '7', employment := '2_new']
cses4_partisan[employment == '6', employment := '3_new']
cses4_partisan[employment %in% c('2', '3'), employment := '4_new']
cses4_partisan[employment == '1', employment := '5_new']
cses4_partisan$employment <- gsub('_new', '', cses4_partisan$employment)
cses4_partisan$employment <- as.integer(cses4_partisan$employment)

#Rename political parties
cses4_partisan$partyId[cses4_partisan$partyId== 0] <- NA_character_
cses4_partisan$partyId[cses4_partisan$partyId== 99] <- NA_character_
cses4_partisan$partyId[cses4_partisan$partyId== 1] <- "Con"
cses4_partisan$partyId[cses4_partisan$partyId== 2] <- "Lab"
cses4_partisan$partyId[cses4_partisan$partyId== 3] <- "UKIP"
cses4_partisan$partyId[cses4_partisan$partyId== 4] <- "LD"
cses4_partisan$partyId[cses4_partisan$partyId== 5] <- "SNP"
cses4_partisan$partyId[cses4_partisan$partyId== 6] <- "GP"
cses4_partisan$partyId[cses4_partisan$partyId== 7] <- "PC"

#mutate (closeness_new = ifelse(cses4_clean$closeness2 == "YES", cses4_clean$closeness2,
#ifelse(is.na(cses4_clean$closeness2), cses4_clean$closeness, cses4_clean$closeness2))) %>%
#cses4_partisan <- subset(cses4_partisan, select =c("year", "id", "sample_wt","age", "gender", "education", "employment", "polknowledge", "closeness_new",
                                                   #"partyId", "partyIdStrength", "likeLab", "likeCon", "likeLD","likeSNP", "likePC", "leftRight"))





