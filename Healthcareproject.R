rm(list=ls())

hops<-read.csv('C:/Users/asp-/Desktop/hospitalcosts.csv')

#Attribute	Description
#Age 	      Age of the patient discharged
#Female 	  A binary variable that indicates if the patient is female
#Los	      Length of stay in days
#Race 	    Race of the patient (specified numerically)
#Totchg	    Hospital discharge costs
#Aprdrg 	  All Patient Refined Diagnosis Related Groups

head(hops) # gives us the first 6 rows of the dataset

# AGE FEMALE LOS RACE TOTCHG APRDRG
# 1  17      1   2    1   2660    560
# 2  17      0   2    1   1689    753
# 3  17      1   7    1  20060    930
# 4  17      1   1    1    736    758
# 5  17      1   1    1   1194    754
# 6  17      0   0    1   3305    347

colSums(is.na(hops)) # returns number of NA's in each column

# AGE FEMALE    LOS   RACE TOTCHG APRDRG 
#   0      0      0      1      0      0 

hops<-na.omit(hops) # Omits NA values

colSums(is.na(hops)) 

# AGE FEMALE    LOS   RACE TOTCHG APRDRG 
#   0      0      0      0      0      0 

str(hops)

#   $ AGE   : int  17 17 17 17 17 17 17 16 16 17 ...
#   $ FEMALE: int  1 0 1 1 1 0 1 1 1 1 ...
#   $ LOS   : int  2 2 7 1 1 0 4 2 1 2 ...
#   $ RACE  : int  1 1 1 1 1 1 1 1 1 1 ...
#   $ TOTCHG: int  2660 1689 20060 736 1194 3305 2205 1167 532 1363 ...
#   $ APRDRG: int  560 753 930 758 754 347 754 754 753 758 ...


hops$RACE<-as.factor(hops$RACE) #converts RACE into factor variable
hops$FEMALE<-as.factor(hops$FEMALE) #converts FEMALE into factor variable

str(hops)
#   $ AGE   : int  17 17 17 17 17 17 17 16 16 17 ...
#   $ FEMALE: Factor w/ 2 levels "0","1": 2 1 2 2 2 1 2 2 2 2 ...
#   $ LOS   : int  2 2 7 1 1 0 4 2 1 2 ...
#   $ RACE  : Factor w/ 6 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#   $ TOTCHG: int  2660 1689 20060 736 1194 3305 2205 1167 532 1363 ...
#   $ APRDRG: int  560 753 930 758 754 347 754 754 753 758 ...


#######################################################################
# QUES 1. To record the patient statistics, the agency wants to find the 
#         age category of people who frequent the hospital and has the
#         maximum expenditure.


######################################################################
#  1(a) the age category of people who frequent the hospital
hist(hops$AGE)

library(dplyr)
a<-summary(as.factor(hops$AGE))
a

#   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 306  10   1   3   2   2   2   3   2   2   4   8  15  18  25  29  29  38
# This implies that infant or age category between 0-1 has frequently 
# visited the hospital. 

######################################################################
#  1(b) the age category of people who has the maximum expenditure.

d<- summarise(group_by(hops,AGE),totalcharge=sum(TOTCHG))
d
arrange(d,desc(totalcharge))
arrange(d,desc(totalcharge))[1,]
 
#     AGE         totalcharge
#     <int>              <int>
# 1     0              676962
# This implies that age group between 0-1 has the maximum expenditure.

######################################################################
# QUES 2. In order of severity of the diagnosis and treatments and to find out the 
#         expensive treatments, the agency wants to find the diagnosis-
#         related group that has maximum hospitalization and expenditure. 

# 2(a)   the agency wants to find the diagnosis-related group that 
#        has maximum hospitalization. 


summary(as.factor(hops$APRDRG))
which.max(summary(as.factor(hops$APRDRG)))
# 640 
# 44 


#    APRDRG 
#  1    640  
#  This implies that the group 640 has the maximum hospitalization.

####################################################################

# 2(b)   the agency wants to find the diagnosis-related group that 
#        has maximum expenditure.


library(sqldf)
b<-sqldf('select APRDRG,sum(TOTCHG) as TOTCHG from hops group by APRDRG')
b
arrange(b,desc(TOTCHG))
arrange(b,desc(TOTCHG))[1,]

#        APRDRG  TOTCHG
# 1         640  436822
# This implies that group number 640 has the maximum expenditure.


#######################################################################
# QUES 3. To make sure that there is no malpractice, the agency needs to 
#         analyze if the race of the patient is related to the hospitalization 
#         costs.

# In this case we'll be using anova test.
# ANOVA dependent variable: TOTCHG
# ANOVA independent variable: RacE
# Ho: There is no effect of RACE on TOTCHG.
# Ha: There is effect of RACE on TOTCHG.

model1<-aov(TOTCHG~RACE,data = hops)
summary(model1)
#              Df    Sum Sq  Mean Sq F value Pr(>F)
# RACE          5 1.859e+07  3718656   0.244  0.943
# Residuals   493 7.524e+09 15260687 

pvalue<-0.943
alpha<-0.05
pvalue < alpha  #whenever p_value is less than alpha; we reject the null hypothesis

# [1] FALSE
# We do not reject the null hypothesis,Ho.
# There is no effect of race on the hospitalization cost.


###############################################################################
# QUES. 4. To properly utilize the costs, the agency has to analyze the severity 
# of the hospital costs by age and gender for the proper allocation of resources.

# In this case we'll be using anova test.
# ANOVA dependent variable: TOTCHG
# ANOVA independent variables: AGE, FEMALE
# Ho: There is no effect of AGE AND FEMALE on TOTCHG.
# Ha: There is effect of AGE AND FEMALE on TOTCHG.


model2<-aov(TOTCHG~ AGE+FEMALE,data = hops)
summary(model2)
#              Df    Sum Sq   Mean Sq F value  Pr(>F)   
# AGE           1 1.297e+08 129749266   8.759 0.00323 **
# FEMALE        1 6.522e+07  65219972   4.403 0.03638 * 
# Residuals   496 7.347e+09  14812787 

pvalue_age<-0.00323
pvalue_female<-0.03638
alpha<-0.05
pvalue_age < alpha  # [1] TRUE
pvalue_female < alpha  # [1] TRUE

# Result_age: We reject the null hypothesis. There is significant affect of age 
#             on hospital costs .
# Result_female: We reject the null hypothesis. There is significant affect of
#                gender on hospital costs .



##################################################################################
# QUES. 5. Since the length of stay is the crucial factor for inpatients, the 
#          agency wants to find if the length of stay can be predicted from age,
#          gender, and race. 


#  Linear Regression
#  Dependent variable: LOS
#  Independent vaiables: AGE, FEMALE, AND RACE 
#  Ho: There is no linear relationship between dependent and independent variables
#  Ha: There is linear relationship between dependent and independent variables
#  y = b0 + b1x1 + b2x2 + b3x3

# Ho: b1 = b2 = b3 = 0
# Ha: at least one b != 0
model3<-lm(LOS~AGE+FEMALE+RACE,data=hops)
summary(model3) # p-value: 0.7432
pvalue <- 0.7432
alpha <-  0.05
pvalue < alpha # [1] FALSE 
#  whenever pvalue > alpha , we do not reject the null hypothesis. This implies 
#  that length of stay can not be predicted from age, female, and race.


##########################################################################
# QUES. 6. To perform a complete analysis, the agency wants to find the variable 
#          that mainly affects hospital costs.

#  Linear Regression
#  Dependent variable: TOTCHG
#  Independent vaiables: AGE, FEMALE, LOS, RACE,AND APRDRG
#  Ho: There is no linear relationship between dependent and independent variables
#  Ha: There is linear relationship between dependent and independent variables
#  y = b0 + b1x1 + b2x2 + b3x3 + b4x4 + b5x5
#  Ho: b1 = b2 = b3 = b4 = b5 = 0
#  Ha: at least one b != 0


model4<-lm(TOTCHG~ .,data=hops)
summary(model4) # p-value: < 2.2e-16
pvalue <- 2.2e-16
alpha <- 0.05
pvalue < alpha  # TRUE, whenever p_value is less than alpha; we reject the 
                # null hypothesis

# Rej the null hypothesis and there is linear relationship between dependent 

# and independent variables

# at least one b != 0 - We dont know which b is NOT zero

###############################################################
# Ho: b1 = 0; AGE has no effect on TOTCHG
# Ha: b1 != 0; AGE has effect on TOTCHG
str(hops)
pvalue<-7.02e-14
alpha<-0.05
pvalue < alpha   # [1] TRUE
#  since this is true so we can reject the null hypothesis.
#  this implies that age is a significant variable 

###############################################################

# Ho: b2 = 0; FEMALE1 has no effect on TOTCHG
# Ha: b2 != 0; FEMALE1 has effect on TOTCHG


pvalue<-0.115
alpha<-0.05  
pvalue < alpha # [1] FALSE, since this is false so we do not reject the null hypothesis
# do not reject Ho, FEMALE1 has no effect on TOTCHG.
# FEMALE1 is not a significant variable to work with.

###############################################################

# Ho: b3 = 0; LOS has no effect on TOTCHG
# Ha: b3 != 0; LOS has effect on TOTCHG

pvalue<-2e-16
alpha<-0.05  
pvalue < alpha  # [1] TRUE
# since this is true so we can reject the null hypothesis.
# this implies that LOS is a significant variable.



###############################################################

# Ho: b5 = 0; APRDRG has no effect on TOTCHG
# Ha: b5 != 0; APRDRG has effect on TOTCHG

pvalue<-2e-16
alpha<-0.05  
pvalue < alpha  # [1] TRUE
# since this is true so we can reject the null hypothesis.
# this implies that APRDRG is a significant variable.

Conclusion:
variables AGE, LOS AND APRDRG are significant enough to work with and they are affecting the TOTCHG.
 


