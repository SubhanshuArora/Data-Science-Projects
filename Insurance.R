#File is imported as SMC
SMC <- read.csv("C:/Users/subha/Downloads/1559641988_insurance_factor_identification/Insurance_factor_identification.csv")
View(SMC)

#Task 1:

#The committee is interested to know each field of the 
#data collected through descriptive analysis to gain 
#basic insights into the data set and to prepare for 
#further analysis. 

summary(SMC)

#Analysis:
#The result shows the maximum, minimum and mean values
#of every variable.
#We can see that the maximum number of claims are 3338
#and the mean claims are 51.87.

#Task 2:

#The committee has decided to find whether payment is 
#related to no. of claims and no. of insured policy years
#They also want to visualize the results for better 
#understanding. 

# CORRELATION

C1=cor(SMC$Claims,SMC$Payment)
C1*100
# The correlation b/w no. of Claims and payment is 99.54.

C2=cor(SMC$Insured,SMC$Payment)
C2*100
# The correlation b/w Insured years and payment is 93.32

# Using Linear Regression to build linear model
# Dependent variable= no. of Claims,Insured years
# Independent variable = Payment

fit = lm(SMC$Payment~SMC$Claims+SMC$Insured)
fit
summary(fit)

#Analysis:
#Since p-value of no. of Claims and Insured years is less, 
#this means both plays a significant role in Payment.

#Graphical Visualization

plot(SMC$Claims,SMC$Payment)
#From the graph, we can see that there is a linear 
#correlation of 99% with both variables

plot(SMC$Insured,SMC$Payment)
#From the graph, we can see that there is a linear 
#correlation of 93% among Insured years and payments.

# Task 3:

#Independent variables: Insured,Claims,Make,Bonus,Zone 
#& Kilometers
#Dependent variable: Payment

names(SMC)
fit1 = lm(Payment~.,data=SMC)
fit1
summary(fit1)

#Since the p value of Bonus and Make is very high, 
#therefore neglecting these variables.
# Adjusted R squared =0.9952

fit2= lm(Payment~Kilometres+Zone+Insured+Claims,data=SMC)
fit2
summary(fit2)

#Analysis:
#This new model (fit 2) has variables with lesser p values,
#Hence we can conclude that the Payment is affected by 
#distance,location,Insured years and Claims.

#Task 4:

# Using aggregate function :

aggregate(SMC[,5:7],by=list(SMC$Zone),
          FUN=mean,na.rm=TRUE)

# We can see that Zone 4 has higher number of claims
# and high number of insured vehicle and that is why
# payment is also high.

aggregate(SMC[,5:7],by=list(SMC$Bonus),
          FUN=mean,na.rm=TRUE)

# All the variables are high for Bonus group 7.

aggregate(SMC[,5:7],by=list(SMC$Kilometres),
          FUN=mean,na.rm=TRUE)

#Analysis:
# We can see that 1st group of Kilometres has more 
#insured vehicles than group 2 while group 2 has more 
#number of claims and so is total payment. Also group 5 
#has lowest number of insured vehicles.

#Task 5:
#The committee needs to find whether the insured amount, 
#zone, kilometer, bonus, or make affects the claim 
#rates and to what extent.

#Independent variables: Insured, Make, Bonus, Zone & Kilometers
#Dependent variable: Claims

fit3=lm(Claims~Kilometres+Zone+Make+Bonus+Insured,data=SMC)
summary(fit3)

#Neglecting Kilometres

fit4=lm(Claims~Zone+Make+Bonus+Insured,data=SMC)
summary(fit4)

#Analysis:
#We have built a linear model "fit3" using Linear 
#Regression and we can see that p-value of the variables
#: Zone, Bonus , Make, Kilometres and Insured years is 
#very low. So these variables are having a major effect 
#on total number of Claims. 





