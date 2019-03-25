###### CREATING A SUBSET WITH RELEVANT VARIABLES ######

library(data.table)
library(tidyverse)
library(memisc)

#STEP1: Selecting relevant variables and renaming columns#

load("/Users/nahemamarchal/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/cses2.rdata")
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
         education_level = B2003,
         employment_stat = B2010,
         polquest_1 = B3047_1,
         polquest_2 = B3047_2, 
         polquest_3 = B3047_3, 
         closeness = B3028, 
         closest_party1 =  B3029_1,
         closeness2 = B3034,
         closest_party2 = B3035,
         degree_closeness = B3036, 
         feelings_labour = B3037_A ,  
         feelings_conser = B3037_B,  
         feelings_libdem = B3037_C,
         feelings_SNP =  B3037_D,
         feelings_PC =  B3037_E,
         left_right_scale = B3045 ) %>%
  filter(country == "Great Britain") %>%
  as.data.table()

#Highlighting missing values

cses2_clean[age %in% c(999,997), age := NA_integer_]
cses2_clean[gender  %in% c(9, 7 ), gender := NA_integer_]
cses2_clean[employment_stat  %in% c(99, 97), employment_stat := NA_integer_]
cses2_clean[education_level  %in% c(99, 97), education_level := NA_integer_]
cses2_clean[polquest_1 %in% c(9,7), polquest_1 := NA_integer_] 
cses2_clean[polquest_2 %in% c(9,7), polquest_2 := NA_integer_] 
cses2_clean[polquest_3 %in% c(9,7), polquest_3 := NA_integer_] 
cses2_clean[closeness %in% c(9, 7), closeness := NA_integer_] 
cses2_clean[closest_party1 %in% c(99, 97), closest_party1 := 0] 
cses2_clean[closeness2 %in% c(9, 7), closeness2 := NA_integer_]
cses2_clean[closest_party2 %in% c(99, 97), closest_party2 := 0] 
cses2_clean[degree_closeness %in% c(9, 7), degree_closeness:= NA_integer_]
cses2_clean[feelings_labour %in% c(99, 97), feelings_labour := NA_integer_] 
cses2_clean[feelings_conser %in% c(99, 97), feelings_conser := NA_integer_] 
cses2_clean[feelings_libdem %in% c(99, 97), feelings_libdem := NA_integer_] 
cses2_clean[feelings_SNP %in% c(99, 97), feelings_SNP := NA_integer_] 
cses2_clean[feelings_PC %in% c(99, 97), feelings_PC := NA_integer_] 
cses2_clean[left_right_scale %in% c(99, 97), left_right_scale := NA_integer_] 


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
cses2_partisan[polquest_1 %in% c(8,2), polquest_1 := 0]
cses2_partisan[polquest_2 %in% c(8,2), polquest_2 := 0] 
cses2_partisan[polquest_3 %in% c(8,2), polquest_3 := 0]
cses2_partisan <- cses2_partisan %>%
  mutate(polknowledge = polquest_1 + polquest_2 + polquest_3)%>%
#Create a single variable "closest_party_new" by merging the values of closest_party and closest_party 2
  mutate (closest_party = ifelse(cses2_partisan$closest_party2 == 0, cses2_partisan$closest_party1, cses2_partisan$closest_party2))%>%
as.data.table()    

#Recode education variable
cses2_partisan$education_level <- as.character(cses2_partisan$education_level)
cses2_partisan[education_level == '1', education_level := '0_new']
cses2_partisan[education_level == '2', education_level := '1_new']
cses2_partisan[education_level == '3', education_level := '2_new']
cses2_partisan[education_level %in% c('4','5'), education_level := '3_new']
cses2_partisan[education_level %in% c('6','7'), education_level := '4_new']
cses2_partisan[education_level == '8', education_level := '5_new']
cses2_partisan$education_level <- gsub( '_new', '', cses2_partisan$education_level)
cses2_partisan$education_level <- as.integer(cses2_partisan$education_level)


#Recode employment variable
cses2_partisan$employment_stat <- as.character(cses2_partisan$employment_stat)
cses2_partisan[employment_stat %in% c('10', '9', '8', '4'), employment_stat := '0_new']
cses2_partisan[employment_stat == '5', employment_stat := '1_new']
cses2_partisan[employment_stat == '7', employment_stat := '2_new']
cses2_partisan[employment_stat == '6', employment_stat := '3_new']
cses2_partisan[employment_stat %in% c('2', '3'), employment_stat := '4_new']
cses2_partisan[employment_stat == '1', employment_stat := '5_new']
cses2_partisan$employment_stat <- gsub('_new', '', cses2_partisan$employment_stat)
cses2_partisan$employment_stat <- as.integer(cses2_partisan$employment_stat)

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
cses2_partisan <- subset(cses2_partisan, select =c("year", "id", "sample_wt", "age", "gender", "education_level", "employment_stat", "polknowledge", "closeness_new",
                                             "closest_party", "degree_closeness",  "feelings_conser", "feelings_labour","feelings_libdem",
                                             "feelings_SNP", "feelings_PC", "left_right_scale"))




#cses2_clean <- cses2_clean %>% 
#select (country, year, age, gender, education_level, employment_stat, polknowledge, closeness ,closeness2, closeness_new, 
#closest_party_new, degree_closeness, feelings_labour, feelings_conser, feelings_libdem, left_right_scale,
#-polquest_1, -polquest_2, -polquest_3, -closest_party, -closest_party2) %>%
#as.data.table()   
#N=860
