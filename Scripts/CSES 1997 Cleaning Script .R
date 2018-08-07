library(data.table)
library(tidyverse)
library(ggplot2)

#STEP1: Identify variables of interest and renaming columns#
load("/Users/nahemamarchal/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/cses1.rdata")
cses1_clean <- as.data.table(cses1_clean)

cses1_clean <- cses1 %>%
  select (A1005, A1010_1, A1010_2, A1010_3, A1006_NAM, A1008, A2001, A2002, A2003, A2007, A2023, A2024, A2025, A3004, A3005_1, 
          A3010, A3011, A3012, A3020_A, A3020_B, A3020_C,A3020_D, A3020_E, A3031) %>%
  rename(id = A1005,
         sample_wt = A1010_1,
         dem_wt = A1010_2,
         pol_wt = A1010_3,
         country = A1006_NAM ,
         year = A1008 ,
         age = A2001 ,
         gender = A2002,
         education_level = A2003,
         employment_stat = A2007,
         polquest_1 = A2023,
         polquest_2 = A2024, 
         polquest_3 = A2025, 
         closeness = A3004, 
         closest_party1 = A3005_1,
         closeness2 = A3010,
         closest_party2 = A3011,
         degree_closeness = A3012, 
         feelings_labour = A3020_A ,  
         feelings_conser = A3020_B,  
         feelings_libdem = A3020_C,
         feelings_SNP = A3020_D,
         feelings_PC = A3020_E,
         left_right_scale = A3031) %>%
  filter(country == "Great Britain") %>%
  as.data.table()

#STEP 2: Highlighting missing and special values#
cses1_clean[age %in% c(999), age := NA_integer_]
cses1_clean[gender  %in% c(9), gender := NA_integer_]
cses1_clean[employment_stat  %in% c(99), employment_stat := NA_integer_]
cses1_clean[education_level  %in% c(99), education_level := NA_integer_]
cses1_clean[polquest_1 %in% c(9), polquest_1 := NA_integer_] 
cses1_clean[polquest_2 %in% c(9), polquest_2 := NA_integer_] 
cses1_clean[polquest_3 %in% c(9), polquest_3 := NA_integer_] 
cses1_clean[closeness %in% c(9,0), closeness := NA_integer_] 
cses1_clean[closest_party1 %in% c(99,00), closest_party1 := NA_integer_] 
cses1_clean[closest_party2 %in% c(99,00), closest_party2 := NA_integer_] 
cses1_clean[closeness2 %in% c(9,0), closeness2 := NA_integer_]
cses1_clean[degree_closeness %in% c(9,0), degree_closeness:= NA_integer_]
cses1_clean[feelings_labour %in% c(99), feelings_labour := NA_integer_] 
cses1_clean[feelings_conser %in% c(99), feelings_conser := NA_integer_] 
cses1_clean[feelings_libdem %in% c(99), feelings_libdem := NA_integer_] 
cses1_clean[feelings_SNP %in% c(99), feelings_SNP := NA_integer_] 
cses1_clean[feelings_PC %in% c(99), feelings_PC := NA_integer_] 
cses1_clean[left_right_scale %in% c(99), left_right_scale := NA_integer_] 

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
cses1_partisan[polquest_1 %in% c(2,8), polquest_1 := 0]
cses1_partisan[polquest_2 %in% c(2,8), polquest_2 := 0] 
cses1_partisan[polquest_3 %in% c(2,8), polquest_3 := 0]
cses1_partisan <- cses1_partisan %>%
  mutate(polknowledge = polquest_1 + polquest_2 + polquest_3)%>%
  
#Create a single variable "closest_party_new" by merging the values of closest_party1 and closest_party 2
  mutate (closest_party = ifelse(is.na(closest_party1), closest_party2, closest_party1))%>%
  as.data.table() 

#Recode education variable
cses1_partisan$education_level <- as.character(cses1_partisan$education_level)
cses1_partisan[education_level == '1', education_level := '0_new']
cses1_partisan[education_level == '2', education_level := '1_new']
cses1_partisan[education_level == '3', education_level := '2_new']
cses1_partisan[education_level %in% c('4','5'), education_level := '3_new']
cses1_partisan[education_level %in% c('6','7'), education_level := '4_new']
cses1_partisan[education_level == '8', education_level := '5_new']
cses1_partisan$education_level <- gsub( '_new', '', cses1_partisan$education_level)
cses1_partisan$education_level <- as.integer(cses1_partisan$education_level)


#Recode employment variable

cses1_partisan$employment_stat <- as.character(cses1_partisan$employment_stat)
cses1_partisan[employment_stat %in% c('10', '9', '8', '4'), employment_stat := '0_new']
cses1_partisan[employment_stat == '5', employment_stat := '1_new']
cses1_partisan[employment_stat == '7', employment_stat := '2_new']
cses1_partisan[employment_stat == '6', employment_stat := '3_new']
cses1_partisan[employment_stat %in% c('2', '3'), employment_stat := '4_new']
cses1_partisan[employment_stat == '1', employment_stat := '5_new']
cses1_partisan$employment_stat <- gsub('_new', '', cses1_partisan$employment_stat)
cses1_partisan$employment_stat <- as.integer(cses1_partisan$employment_stat)


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

cses1_partisan <- subset(cses1_partisan, select =c("year", "id", "sample_wt", "age", "gender", "education_level", "employment_stat", "polknowledge", "closeness_new",
                                                   "closest_party", "degree_closeness", "feelings_conser",  "feelings_labour", "feelings_libdem", 
                                                   "feelings_SNP", "feelings_PC", "left_right_scale"))

#Remove inconsistent answers to the "Do you feel close to a political party?" questions        
cses1_partisan <- cses1_partisan %>%    
  filter (closeness_new!=6)%>%
  as.data.table() 









