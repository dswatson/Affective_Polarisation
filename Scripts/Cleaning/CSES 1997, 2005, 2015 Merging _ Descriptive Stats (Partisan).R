
library(dplyr)
library(oii)
library(ggplot2)

#Merge 3 momdules into one
CSES_OG <- bind_rows(cses1_partisan, cses2_partisan, cses4_partisan)

#Isolating the "Don't know" responses
CSES_OG$closeness_new[CSES_OG$closeness_new== 8] <- -8
CSES_OG$closest_party[CSES_OG$closest_party== 98] <- -98
CSES_OG$closest_party[CSES_OG$closest_party== 9] <- -9
CSES_OG$degree_closeness[CSES_OG$degree_closeness== 8] <- -8
CSES_OG$feelings_labour[CSES_OG$feelings_labour== 98] <- -98
CSES_OG$feelings_conser[CSES_OG$feelings_conser== 98] <- -98
CSES_OG$feelings_libdem[CSES_OG$feelings_libdem== 98] <- -98
CSES_OG$feelings_SNP[CSES_OG$feelings_SNP== 98] <- -98
CSES_OG$feelings_PC[CSES_OG$feelings_PC== 98] <- -98
CSES_OG$left_right_scale[CSES_OG$left_right_scale== 98] <- -98

#Create a subset of the original dataset without the "Don't knows"
CSES <- CSES_OG %>%
  filter(degree_closeness >=0 | is.na(degree_closeness), closeness_new>=0 | is.na(closeness_new), 
         closest_party>=0 | is.na(closest_party), feelings_conser>=0 | is.na(feelings_conser), 
         feelings_labour>=0 | is.na(feelings_labour), feelings_libdem >=0 | is.na (feelings_libdem),
         feelings_SNP >=0 | is.na (feelings_SNP),feelings_PC >=0 | is.na (feelings_PC),
         left_right_scale>=0 | is.na(left_right_scale)) %>%
#addig | is.na maintains the NAs in the rows, otherwise, conditional filtering in dplyr deletes them
as.data.table ()     

#Create a separate list with common variables to merge data frame with BES

CSES <- CSES[, c("year", "id", "sample_wt", "age", "gender", "closest_party", "degree_closeness", 
                 "feelings_conser","feelings_labour", "feelings_libdem","feelings_SNP",
                 "feelings_PC", "left_right_scale"), with = FALSE]
CSES %>%
  rename (wt_core = sample_wt,
          closest_party = partyId,
          degree_closeness = partyIdStrength,
          likeCon = feelings_conser,
          likeLab = feelings_labour,
          likeLD = feelings_libdem,
          likeSNP = feelings_SNP,
          likePC = feelings_PC,
          leftRight = left_right_scale)%>%
as.data.table()
                        
#Summary statistics
oii.summary(CSES$age, extended = TRUE)
ggplot(data = CSES, mapping = aes(x = age))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 5)+
  labs (x = "Age")

oii.freq(CSES$gender)
ggplot(data = CSES, mapping = aes(x = gender))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 1)+
  labs (x = "Gender")

oii.summary(CSES$education_level, extended = TRUE)
oii.freq(CSES$education_level)
ggplot(data = CSES, mapping = aes(x = education_level))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 1)+
  labs (x = "Education")

oii.summary(CSES$employment_stat, extended = TRUE)
oii.freq(CSES$employment_stat)
ggplot(data = CSES, mapping = aes(x = employment_stat))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 1)+
  labs (x = "Employment")

#Data visualisation

#Comparing the distribution of my variables

#Combining plots
#par(mfrow = c(2,3))
#AGE#
p1 <- ggplot(data = CSES_partisan, mapping = aes(x = age))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 5)+
  labs (x = "Age_combined")

p2 <- ggplot(data = cses1_clean, mapping = aes(x = cses1_clean$age))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 5)+
  labs (x = "Age_module1")

p3 <- ggplot(data = cses2_clean, mapping = aes(x = cses2_clean$age))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 5)+
  labs (x = "Age_module2")

p4 <- ggplot(data = cses4_clean, mapping = aes(x = cses4_clean$age))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 5)+
  labs (x = "Age_module4")
#Combining ggplots
grid.arrange(p1, p2, p3, p4, nrow =2)

#GENDER#

p1 <- ggplot(data = CSES_partisan, mapping = aes(x = gender))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 1)+
  labs (x = "Gender_combined")

p2 <- ggplot(data = cses1_clean, mapping = aes(x = cses1_clean$gender))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 1)+
  labs (x = "Gender_module1")

p3 <- ggplot(data = cses2_clean, mapping = aes(x = cses2_clean$gender))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 1)+
  labs (x = "Gender_module2")

p4 <- ggplot(data = cses4_clean, mapping = aes(x = cses4_clean$gender))+
  geom_histogram(fill= "steelblue2", color = "black", binwidth = 1)+
  labs (x = "Gender_module4")
#Combining ggplots
grid.arrange(p1, p2, p3, p4, nrow =2)






#Showing multiple plots next to one another
#hist(CSES_partisan$age, breaks = 20, main = "Histogram age", xlab = "Age")



