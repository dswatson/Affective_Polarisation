library(dplyr)
library(car)
library(Hmisc)
library(mctest)
library(VIM)
library(mice)
library(miceadds)

#----------------------------------------------------------------------------------------#
# LOAD DATA + TRANSFORMATION
#----------------------------------------------------------------------------------------#

load("/Users/nahema/Dropbox/1. DPHIL/Papers/IPP2018/NewImputedData.Rdata")
#load new_data_20 (long data frame containing 20 imputed datasets)

new <- new_data_20 %>%
  mutate(male = if_else(gender ==1, 1, 0)) %>%
  mutate(extremity = (equalIncome + immigOpp)/2)

#recode ideological extremity variable
new$extremity[ new$extremity<6 & new$extremity>4] <- '0_new'
new$extremity[ (new$extremity<= 4 & new$extremity>3) & (new$extremity>= 6 & new$extremity<7)] <- '1_new'
new$extremity[ (new$extremity<= 3 & new$extremity>2) & (new$extremity>= 7 & new$extremity<8)] <- '2_new'
new$extremity[ (new$extremity<= 2 & new$extremity>1) & (new$extremity>= 8 & new$extremity<9)] <- '3_new'
new$extremity[ (new$extremity<= 2 & new$extremity>1) & (new$extremity>= 8 & new$extremity<9)] <- '3_new'
new$extremity[ (new$extremity<= 1 & new$extremity>=0) & (new$extremity>= 9 & new$extremity<=10)] <- '4_new'
new$extremity <- gsub('_new', '', new$extremity)
new$extremity <- as.integer(new$extremity)

new <- as.mids(new) #back to mids object

#----------------------------------------------------------------------------------------#
# REGRESSION MODEL 1
#H2: Ideological sorting will only motivate affective polarisation among the most politically attentive 
#H2: H3: The relationship between partisanship and affective polarisation will be moderated by age 
#----------------------------------------------------------------------------------------#
#Here, to answer H2, we want to check the interaction effects of sorted x polAtt (both continuous)
#To answer H3, we want to analyse the interaction effects of as.factor(partyId) x age (categorical & continuous)
#You might want to run 2 separate models to test these two hypothesese. I will leave it to your judgment

model2_bis <- with(data = new, exp =lm(phi~ sorted + as.factor(partyId) + age + strong_partisan + male+ education + 
                                         polAtt + income_quintile, weights = weight))
summary(pool(model2_bis))

#estimate    std.error  statistic         df      p.value
#(Intercept)          1.57562494 0.1489045077  10.581446   38.51551 0.000000e+00
#sorted               1.05451940 0.0621846897  16.957862   43.48960 0.000000e+00
#as.factor(partyId)2 -0.38567317 0.0192552789 -20.029477  247.94786 0.000000e+00
#as.factor(partyId)3 -1.88591874 0.0245081521 -76.950671  655.35869 0.000000e+00
#age                  0.01199969 0.0004736274  25.335711  231.54648 0.000000e+00
#strong_partisan      2.79495348 0.0178162486 156.876655 2108.05476 0.000000e+00
#male                -0.23175152 0.0149883568 -15.462103  308.27814 0.000000e+00
#education           -0.07788349 0.0070783352 -11.003080   81.27758 0.000000e+00
#polAtt               0.13031881 0.0046981344  27.738416   57.01776 0.000000e+00
#income_quintile     -0.04417991 0.0095026236  -4.649233   33.76908 3.537903e-06
pool.r.squared(model2_bis)
#R^2 0.2129112 0.2089203 0.2169194 NaN

# Testing H2: add interaction term btw sorted and polAtt
model2_bis_h2 <- with(data = new, 
                      exp = lm(phi ~ sorted * polAtt + 
                               as.factor(partyId) + age + strong_partisan + 
                               male + education + income_quintile, 
                               weights = weight))
summary(pool(model2_bis_h2))
#estimate    std.error  statistic         df      p.value
#(Intercept)          2.98642251 0.3598836720   8.298300   49.87108 2.220446e-16
#sorted               0.40336679 0.1610546753   2.504533   53.39577 1.233335e-02
#polAtt              -0.06996226 0.0422005373  -1.657852   73.51249 9.749026e-02
#as.factor(partyId)2 -0.38528267 0.0192183297 -20.047667  252.68293 0.000000e+00
#as.factor(partyId)3 -1.88668766 0.0244398094 -77.197315  690.96709 0.000000e+00
#age                  0.01197667 0.0004748127  25.223990  225.69219 0.000000e+00
#strong_partisan      2.79626078 0.0177966715 157.122684 2193.49040 0.000000e+00
#male                -0.23190035 0.0149629773 -15.498276  314.21370 0.000000e+00
#education           -0.07831258 0.0070535879 -11.102517   82.49349 0.000000e+00
#income_quintile     -0.04406469 0.0095301581  -4.623710   33.63392 3.988552e-06
#sorted:polAtt        0.09267923 0.0188712335   4.911138   82.74737 9.723185e-07

# Very significant interaction effect
pool.r.squared(model2_bis_h2)
#          est     lo 95     hi 95 fmi
# R^2 0.213124 0.2091607 0.2171043 NaN

# Testing H3: add interaction term btw partyId and age
model2_bis_h3 <- with(data = new, 
                      exp = lm(phi ~ 0 + age:as.factor(partyId) + 
                               as.factor(partyId) + sorted + strong_partisan + 
                               male + education + polAtt + income_quintile, 
                               weights = weight))

#estimate    std.error   statistic         df      p.value
#as.factor(partyId)1      0.260045867 0.1519478145   1.7114156   41.31354 8.714799e-02
#as.factor(partyId)3     -0.084046768 0.1573548829  -0.5341224   53.62077 5.933118e-01
#as.factor(partyId)2      1.600285149 0.1514578666  10.5658767   43.98707 0.000000e+00
#sorted                   1.170318999 0.0599376804  19.5255971   46.82269 0.000000e+00
#strong_partisan          2.796345932 0.0177081135 157.9132602 2159.76186 0.000000e+00
#male                    -0.201478893 0.0149317661 -13.4933063  308.61507 0.000000e+00
#education               -0.077397993 0.0071121424 -10.8825145   78.19158 0.000000e+00
#polAtt                   0.130718066 0.0047056159  27.7791617   55.91045 0.000000e+00
#income_quintile         -0.041557785 0.0094834089  -4.3821568   33.61965 1.231379e-05
#age:as.factor(partyId)1  0.031893424 0.0007049361  45.2430014  273.66569 0.000000e+00
#age:as.factor(partyId)2 -0.002876251 0.0006805152  -4.2265786  233.46374 2.471465e-05
#age:as.factor(partyId)3  0.001299717 0.0012104421   1.0737542  457.02561 2.830528e-01

# Ummmm this makes a *bit* of a difference!
#est     lo 95     hi 95 fmi
#R^2 0.7930595 0.7909509 0.7951496 NaN

anova(model2_bis_h3, model2_bis)
#test statistic df1      df2 df.com p.value       riv
#1 ~~ 2  526.6554   4 371.0499 174444       0 0.7726064


#-----------------------------------------------------------------#
# REGRESSION MODEL 2
#H4: Attitude extremity on policy issues will drive affective polarisation
#-----------------------------------------------------------------#
#Here, we just need to beef up the interpretation of the model in the  manuscript

model3_bis <- with(data = new, exp =lm(phi~ as.factor(partyId) + extremity + age + 
                                         strong_partisan + male+ education + polAtt + income_quintile, 
                                       weights = weight))

#summary(pool(model3_bis))

#estimate    std.error  statistic         df      p.value
#(Intercept)          3.74770663 0.0668888329  56.028884   41.80912 0.000000e+00 
#as.factor(partyId)2 -0.15852913 0.0178287379  -8.891775  179.59871 0.000000e+00
#as.factor(partyId)3 -1.71584769 0.0256747851 -66.830070  252.52152 0.000000e+00
#extremity           -0.01147861 0.0036777920  -3.121061   44.30121 1.840122e-03
#age                  0.01195687 0.0004844232  24.682699  193.80947 0.000000e+00
#strong_partisan      2.74378332 0.0180000520 152.431966 1353.51260 0.000000e+00
#male                -0.23389570 0.0150222088 -15.569994  308.62420 0.000000e+00
#education           -0.06763867 0.0070220808  -9.632283   84.43106 0.000000e+00
#polAtt               0.12795700 0.0047857003  26.737361   54.59187 0.000000e+00
#income_quintile     -0.03827211 0.0097301261  -3.933362   32.86400 8.801131e-05
#pool.r.squared(model3_bis)