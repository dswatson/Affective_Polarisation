###### CREATING A SUBSET WITH RELEVANT VARIABLES ######

library(data.table)
library(tidyverse)

#STEP1: Selecting relevant variables and renaming columns#
load("/Users/nahemamarchal/Dropbox/1. DPHIL/4. Papers/IPP2018 AffectPol UK/Datasets/cses4.rdata")
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
         education_level = D2003,
         employment_stat = D2010,
         polquest_1 = D3025_1_A,
         polquest_2 = D3025_2_A, 
         polquest_3 = D3025_3_A, 
         closeness = D3018_1, 
         closeness2 = D3018_2,
         closest_party = D3018_3,
         degree_closeness = D3018_4, 
         feelings_labour = D3011_A ,  
         feelings_conser = D3011_B,  
         feelings_libdem = D3011_D , 
         left_right_scale = D3014 ) %>%
  filter(country == "Great Britain") %>%
as.data.table()
#this turns your dataframe into a datatable again

#STEP 2: Highlighting missing values#

#DF [i, j, by]
#:= NA_integer_] reassign a value in datatable

cses4_clean[age_year %in% c(9999,9997), age_year := NA_integer_]
cses4_clean[gender  %in% c(9, 7), gender := NA_integer_]
cses4_clean[employment_stat  %in% c(99, 97), employment_stat := NA_integer_]
cses4_clean[education_level  %in% c(99, 97), education_level := NA_integer_]
cses4_clean[polquest_1 %in% c(9, 7), polquest_1 := NA_integer_] 
cses4_clean[polquest_2 %in% c(9, 7), polquest_2 := NA_integer_] 
cses4_clean[polquest_3 %in% c(9, 7), polquest_3 := NA_integer_] 
cses4_clean[closeness %in% c(9, 7), closeness := NA_integer_] 
cses4_clean[closest_party %in% c(99, 97), closest_party := NA_integer_] 
cses4_clean[closeness2 %in% c(9, 7), closeness2 := NA_integer_]
cses4_clean[degree_closeness %in% c(9, 7), degree_closeness:= NA_integer_]
cses4_clean[feelings_labour %in% c(99, 97), feelings_labour := NA_integer_] 
cses4_clean[feelings_conser %in% c(99, 97), feelings_conser := NA_integer_] 
cses4_clean[feelings_libdem %in% c(99, 97), feelings_libdem := NA_integer_] 
cses4_clean[left_right_scale%in% c(99, 97), left_right_scale := NA_integer_] 

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
cses4_partisan[polquest_1 %in% c(8,5), polquest_1 := 0]
cses4_partisan[polquest_2 %in% c(8,5), polquest_2 := 0] 
cses4_partisan[polquest_3 %in% c(8,5), polquest_3 := 0]
cses4_partisan <- cses4_partisan %>%
  mutate(polknowledge = polquest_1 + polquest_2 + polquest_3)%>%
as.data.table()  

#Recode education variable
cses4_partisan$education_level <- as.character(cses4_partisan$education_level)
cses4_partisan[education_level == '96', education_level := '0_new']
cses4_partisan[education_level == '2', education_level := '1_new']
cses4_partisan[education_level %in% c('3','4'), education_level := '2_new']
cses4_partisan[education_level == '5', education_level := '3_new']
cses4_partisan[education_level == '6', education_level := '4_new']
cses4_partisan[education_level %in% c('7','8'), education_level := '5_new']
cses4_partisan$education_level <- gsub( '_new', '', cses4_partisan$education_level)
cses4_partisan$education_level <- as.integer(cses4_partisan$education_level)


#Recode employment variable
cses4_partisan$employment_stat <- as.character(cses4_partisan$employment_stat)
cses4_partisan[employment_stat %in% c('8','9', '11', '12'), employment_stat := '0_new']
cses4_partisan[employment_stat == '5', employment_stat := '1_new']
cses4_partisan[employment_stat == '7', employment_stat := '2_new']
cses4_partisan[employment_stat == '6', employment_stat := '3_new']
cses4_partisan[employment_stat %in% c('2', '3'), employment_stat := '4_new']
cses4_partisan[employment_stat == '1', employment_stat := '5_new']
cses4_partisan$employment_stat <- gsub('_new', '', cses4_partisan$employment_stat)
cses4_partisan$employment_stat <- as.integer(cses4_partisan$employment_stat)

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
cses4_partisan <- subset(cses4_partisan, select =c("year", "id", "sample_wt","age", "gender", "education_level", "employment_stat", "polknowledge", "closeness_new",
                                                   "closest_party", "degree_closeness", "feelings_labour", "feelings_conser", "feelings_libdem", "left_right_scale"))



#datatable
#cses4_clean[, polknowledge := polquest_1 + polquest_2 + polquest_3]


#Creating relevant subset
#cses4_clean <- cses4_clean %>%
  #select(-polquest_1, -polquest_2, -polquest_3, -age_year)%>% 
#as.data.table()

View(cses4_partisan)
