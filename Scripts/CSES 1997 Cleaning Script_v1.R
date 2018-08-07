#-----------------------------------------------------------------------------------------#
# CSES 1997-2005-2015 Cleaning Script
#-----------------------------------------------------------------------------------------#

library(data.table)
library(tidyverse)
library(memisc)

#-----------------------------------------------------------------------------------------#
# CSES Module 1 (1997) 
#-----------------------------------------------------------------------------------------#

#STEP1: Identify variables of interest and renaming columns#
load("/Users/nahemamarchal/Dropbox/1. DPHIL/4. Papers/IPP2018/CSES/Datasets/cses1.rdata")
cses1_clean <- as.data.table(cses1_clean)

cses1_clean <- cses1 %>%
  select (A1005, A1010_1, A1010_2, A1010_3, A1006_NAM, A1008, A2001, A2002, 
          A2003, A2007, A2023, A2024, A2025, A3004, A3005_1, A3010, A3011,
          A3012, A3020_A, A3020_B, A3020_C,A3020_D, A3020_E, A3031, A3032_A,
          A3032_B, A3032_C, A3032_D, A3032_E, A5004_A, A5004_B, A5004_C, 
          A5004_D, A5004_E
          ) %>%
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
         closest_party1 = A3005_1,
         closeness2 = A3010,
         closest_party2 = A3011,
         degree_closeness = A3012, 
         likeLab = A3020_A ,  
         likeCon = A3020_B,  
         likeLD = A3020_C,
         likeSNP = A3020_D,
         likePC = A3020_E,
         leftRight = A3031,
         lrCon = A3032_A,
         lrLab = A3032_B, 
         lrLD = A3032_C, 
         lrSNP = A3032_D, 
         lrPC =A3032_E,
         lrCon_real = A5004_A, 
         lrLab_real = A5004_B, 
         lrLD_real =A5004_C, 
         lrSNP_real = A5004_D, 
         lrPC_real = A5004_E
         ) %>%
  filter(country == "Great Britain") %>%
  as.data.table()

#for (i in 1:n_samples) {
  #ifelse(cses1_clean$[i] %in% c(999), 
  #}


#STEP 2: Highlighting missing and special values#
cses1_clean[age == 999, age := NA_integer_]
cses1_clean[gender  == 9 , gender := NA_integer_]
cses1_clean[employment  == 99, employment := NA_integer_]
cses1_clean[education  == 99, education := NA_integer_]
cses1_clean[polq1 == 99, polq1 := NA_integer_] 
cses1_clean[polq2== 99, polq2 := NA_integer_] 
cses1_clean[polq3 == 99, polq3 := NA_integer_] 
cses1_clean[closeness %in% c(9,0), closeness := NA_integer_] 
cses1_clean[closest_party1 %in% c(99,00), closest_party1 := NA_integer_] 
cses1_clean[closest_party2 %in% c(99,00), closest_party2 := NA_integer_] 
cses1_clean[closeness2 %in% c(9,0), closeness2 := NA_integer_]
cses1_clean[degree_closeness %in% c(9,0), degree_closeness:= NA_integer_]
cses1_clean[likeLab == 99, likeLab := NA_integer_] 
cses1_clean[likeCon == 99, likeCon := NA_integer_] 
cses1_clean[likeLD == 99, likeLD := NA_integer_] 
cses1_clean[likeSNP == 99, likeSNP := NA_integer_] 
cses1_clean[likePC == 99, likePC := NA_integer_] 
cses1_clean[leftRight == 99, leftRight := NA_integer_] 
cses1_clean[lrCon  == 99, lrCon  := NA_integer_] 
cses1_clean[lrCon  == 99, lrCon  := NA_integer_] 
lrCon 
lrLab
lrLD  
lrSNP 
lrPC 
lrCon_real 
lrLab_real 
lrLD_real  
lrSNP_real 
lrPC_real

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
  
#Create a single variable "closest_party_new" by merging the values of closest_party1 and closest_party 2
  mutate (closest_party = ifelse(is.na(closest_party1), closest_party2, closest_party1))%>%
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
cses1_partisan$closest_party[cses1_partisan$closest_party== 0] <- NA
cses1_partisan$closest_party[cses1_partisan$closest_party== 99] <- NA
cses1_partisan$closest_party[cses1_partisan$closest_party== 1] <- "Con"
cses1_partisan$closest_party[cses1_partisan$closest_party== 2] <- "Lab"
cses1_partisan$closest_party[cses1_partisan$closest_party== 3] <- "LD"
cses1_partisan$closest_party[cses1_partisan$closest_party== 4] <- "SNP"
cses1_partisan$closest_party[cses1_partisan$closest_party== 5] <- "PC"
cses1_partisan$closest_party[cses1_partisan$closest_party== 7] <- "Other"

#Create relevant subset

cses1_partisan <- subset(cses1_partisan, select =c("year", "id", "sample_wt", "age", "gender", "education", "employment", "polknowledge", "closeness_new",
                                                   "closest_party", "degree_closeness", "likeCon",  "likeLab", "likeLD", 
                                                   "likeSNP", "likePC", "leftRight"))

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
          B3036, B3037_A, B3037_B, B3037_C, B3037_D, B3037_E, B3045) %>%
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
         closest_party1 =  B3029_1,
         closeness2 = B3034,
         closest_party2 = B3035,
         degree_closeness = B3036, 
         likeLab = B3037_A ,  
         likeCon = B3037_B,  
         likeLD = B3037_C,
         likeSNP =  B3037_D,
         likePC =  B3037_E,
         leftRight = B3045 ) %>%
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
cses2_clean[closest_party1 %in% c(99, 97), closest_party1 := 0] 
cses2_clean[closeness2 %in% c(9, 7), closeness2 := NA_integer_]
cses2_clean[closest_party2 %in% c(99, 97), closest_party2 := 0] 
cses2_clean[degree_closeness %in% c(9, 7), degree_closeness:= NA_integer_]
cses2_clean[likeLab %in% c(99, 97), likeLab := NA_integer_] 
cses2_clean[likeCon %in% c(99, 97), likeCon := NA_integer_] 
cses2_clean[likeLD %in% c(99, 97), likeLD := NA_integer_] 
cses2_clean[likeSNP %in% c(99, 97), likeSNP := NA_integer_] 
cses2_clean[likePC %in% c(99, 97), likePC := NA_integer_] 
cses2_clean[leftRight %in% c(99, 97), leftRight := NA_integer_] 


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
  #Create a single variable "closest_party_new" by merging the values of closest_party and closest_party 2
  mutate (closest_party = ifelse(cses2_partisan$closest_party2 == 0, cses2_partisan$closest_party1, cses2_partisan$closest_party2))%>%
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
cses2_partisan$closest_party[cses2_partisan$closest_party== 0] <- NA
cses2_partisan$closest_party[cses2_partisan$closest_party== 99] <- NA
cses2_partisan$closest_party[cses2_partisan$closest_party== 1] <- "Lab"
cses2_partisan$closest_party[cses2_partisan$closest_party== 2] <- "Con"
cses2_partisan$closest_party[cses2_partisan$closest_party== 3] <- "LD"
cses2_partisan$closest_party[cses2_partisan$closest_party== 4] <- "SNP"
cses2_partisan$closest_party[cses2_partisan$closest_party== 5] <- "PC"
cses2_partisan$closest_party[cses2_partisan$closest_party== 6] <- "Other"


# mutate (closeness_new = ifelse(cses2_clean$closeness2 == "YES", cses2_clean$closeness2,
# ifelse(is.na(cses2_clean$closeness2), cses2_clean$closeness, cses2_clean$closeness2))) %>%
cses2_partisan <- subset(cses2_partisan, select =c("year", "id", "sample_wt", "age", "gender", "education", "employment", "polknowledge", "closeness_new",
                                                   "closest_party", "degree_closeness",  "likeCon", "likeLab","likeLD",
                                                   "likeSNP", "likePC", "leftRight"))




#-----------------------------------------------------------------------------------------#
# CSES Module 4 (2015) 
#-----------------------------------------------------------------------------------------#

#STEP1: Selecting relevant variables and renaming columns#
load("/Users/nahemamarchal/Dropbox/1. DPHIL/4. Papers/IPP2018/CSES/Datasets/cses4.rdata")
cses4_clean <- as.data.table(cses4_clean)

cses4_clean <- cses4 %>%
  select (D1005, D1010_1, D1010_2, D1010_3, D1006_NAM, D1008, D2001_Y, D2002, D2003, D2010, D3025_1_A, D3025_2_A, D3025_3_A, D3018_1, D3018_2, D3018_3, D3018_4, 
          D3011_A , D3011_B, D3011_C, D3014) %>%
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
         closest_party = D3018_3,
         degree_closeness = D3018_4, 
         likeLab = D3011_A ,  
         likeCon = D3011_B,  
         likeLD = D3011_D , 
         likeSNP = D3011_E,
         likePC = D3011_G,
         leftRight = D3014 ) %>%
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
cses4_clean[closest_party %in% c(99, 97), closest_party := NA_integer_] 
cses4_clean[closeness2 %in% c(9, 7), closeness2 := NA_integer_]
cses4_clean[degree_closeness %in% c(9, 7), degree_closeness:= NA_integer_]
cses4_clean[likeLab %in% c(99, 97), likeLab := NA_integer_] 
cses4_clean[likeCon %in% c(99, 97), likeCon := NA_integer_] 
cses4_clean[likeLD %in% c(99, 97), likeLD := NA_integer_] 
cses4_clean[likeSNP %in% c(99, 97), likeSNP := NA_integer_] 
cses4_clean[likePC %in% c(99, 97), likePC := NA_integer_] 
cses4_clean[leftRight%in% c(99, 97), leftRight := NA_integer_] 

#STEP3: Transform variables#

#Create age variable
#dplyr
cses4_clean <- cses4_clean %>%
  mutate(age = year - age_year) %>%
  as.data.table()

#data table
#cses4_clean[, age := year - age_year
#][, mean_age_by_country := mean(age), by = country
#][]

# grouping in dplyr
#cses4_clean <- cses4_clean %>%
#group_by(country) %>%
#mutate(mean_age_by_country = mean(age)) %>%
#ungroup(.)

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
cses4_partisan$closest_party[cses4_partisan$closest_party== 0] <- NA
cses4_partisan$closest_party[cses4_partisan$closest_party== 99] <- NA
cses4_partisan$closest_party[cses4_partisan$closest_party== 1] <- "Con"
cses4_partisan$closest_party[cses4_partisan$closest_party== 2] <- "Lab"
cses4_partisan$closest_party[cses4_partisan$closest_party== 3] <- "UKIP"
cses4_partisan$closest_party[cses4_partisan$closest_party== 4] <- "LD"
cses4_partisan$closest_party[cses4_partisan$closest_party== 5] <- "SNP"
cses4_partisan$closest_party[cses4_partisan$closest_party== 6] <- "GP"
cses4_partisan$closest_party[cses4_partisan$closest_party== 7] <- "PC"

#mutate (closeness_new = ifelse(cses4_clean$closeness2 == "YES", cses4_clean$closeness2,
#ifelse(is.na(cses4_clean$closeness2), cses4_clean$closeness, cses4_clean$closeness2))) %>%
cses4_partisan <- subset(cses4_partisan, select =c("year", "id", "sample_wt","age", "gender", "education", "employment", "polknowledge", "closeness_new",
                                                   "closest_party", "degree_closeness", "likeLab", "likeCon", "likeLD","likeSNP", "likePC", "leftRight"))





