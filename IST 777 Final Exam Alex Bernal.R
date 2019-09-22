#IST 777 FInal Exam - Alex Bernal
#comment block Cntrl + Shft + C

library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data
library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')
library('ztable')     # format tables for reporting

#1. What proportion of public schools reported vaccination data?


#sData <- allSchoolsReportStatusClean
sData <- allSchoolsReportStatus
head(sData)

#write.csv(allSchoolsReportStatus,"C:\\Users\\User\\Desktop\\allSchoolsReportStatus.csv", row.names = FALSE)

dfr <- sData %>%             
  mutate(pubpriv = as.factor(pubpriv)     # numeric values to factor
         , reported = as.ordered(reported))  # numeric to ordered factor (like a grade)

dfr_prop <- dfr %>% 
  count(pubpriv, reported) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too

as.data.frame(dfr_prop)           # strip tbl_df() properties to print

# pubpriv reported    n       prop
# 1 PRIVATE        N  252 0.03414172
# 2 PRIVATE        Y 1397 0.18926975
# 3  PUBLIC        N  148 0.02005148
# 4  PUBLIC        Y 5584 0.75653705

#2. What proportion of private schools reported vaccination data?

summary(dfr)
# pubpriv     reported
# PRIVATE:1649   N: 400  
# PUBLIC :5732   Y:6981  



#3. How have U.S. vaccination rates varied over time?

#export data and append years
#write.csv(usVaccines,"C:\\Users\\User\\Desktop\\usVaccines.csv", row.names = FALSE)

vData <- usVaccinesClean
head(vData)


boxplot(usVaccines2)
summary(vData)
View(vData)
cor(vData[,2:6])

plot.ts(vData$DTP1)
plot.ts(vData$HepB_BD)
plot.ts(vData$Pol3)
plot.ts(vData$Hib3)
plot.ts(vData$MCV1)


#4. Are there any notable patterns in U.S. vaccination rates over time?
co2series <- ts(vData[,2:6], start=c(1980,1), frequency=12)
plot(co2series)

dec1 <- decompose(co2series[,"DTP1"])
dec2 <- decompose(co2series[,"HepB_BD"])
dec3 <- decompose(co2series[,"Pol3"])
dec4 <- decompose(co2series[,"Hib3"])
dec5 <- decompose(co2series[,"MCV1"])

plot(dec1)
plot(dec2)
plot(dec3)
plot(dec4)
plot(dec5)

sites <- data.frame(dec1$random, dec2$random, dec3$random, dec4$random, dec5$random)
sites <- sites[complete.cases(sites),]
sites <- ts(sites,start=c(1980,1),frequency=12)

str(sites)
summary(sites)
plot(sites)

acf(sites)

library("tseries")
cor(sites)
#Weak correlation between the random components
library("changepoint")
cpvar1 <- cpt.var((sites[,1]))

summary(cpvar1)
plot(cpvar1)

cpvar2 <- cpt.var((sites[,2]))
summary(cpvar2)
plot(cpvar2)

cpvar3 <- cpt.var((sites[,3]))
summary(cpvar3)
plot(cpvar3)

cpvar4 <- cpt.var((sites[,4]))
summary(cpvar4)
plot(cpvar4)

cpvar5 <- cpt.var((sites[,5]))
summary(cpvar5)
plot(cpvar5)


adf.test(sites[, 1])
adf.test(sites[, 2])
adf.test(sites[, 3])
adf.test(sites[, 4])
adf.test(sites[, 5])


#levels of noise and seasonality vary among the three sites
#Trend is similar across the three sites





#5. How do vaccination rates (for individual vaccines) in California districts compare with
#overall US vaccination rates?
#export data and append years
write.csv(districts,"C:\\Users\\User\\Desktop\\districts.csv", row.names = FALSE)



#6. Among districts, how are the vaccination rates for individual vaccines related? In other
#words, if students are missing one vaccine are they missing all of the others?


head(sDataJoin)

library(e1071)
#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(PctUpToDate ~ WithoutDTP + WithoutPolio + WithoutMMR + WithoutHepB, data=sDataJoin)
#What does the model say? Print the model summary
print(Naive_Bayes_Model)


summary(Naive_Bayes_Model)

head(NBsData)
#Fitting the Naive Bayes model
Naive_Bayes_Model2=naiveBayes(MissingAny ~ MissingDTP + MissingPolio + MissingMeasles + MissingHep, data=NBsData)
#What does the model say? Print the model summary
Naive_Bayes_Model2

modelPred <- predict(Naive_Bayes_Model2, NBsData)
  

#Public vs. Private School Comparisons:
#7. What was the breakdown of reporting vs. non-reporting for public schools?


#8. What was the breakdown of reporting vs. non-reporting for private schools?


#9. Was there any credible difference in overall reporting proportions between public and
#private schools?

# pubpriv reported    n       prop
# 1 PRIVATE        N  252 0.03414172
# 2 PRIVATE        Y 1397 0.18926975
# 3  PUBLIC        N  148 0.02005148
# 4  PUBLIC        Y 5584 0.75653705


RepPubPriv <- c(1397, 5587)
NpubPriv <- c(1649, 5732)

#install.packages("devtools")
#library(BayesianFirstAid)


prop.test(RepPubPriv, NpubPriv)


install.packages("devtools")
library(devtools)
install_github("rasmusab/bayesian_first_aid")

library(BayesianFirstAid)
fit <-bayes.prop.test(RepPubPriv, NpubPriv)
plot(fit)
summary(fit)
diagnositcs(fit)



#  Predictive Analyses:
#  (For all of these analyses, use PctChildPoverty, PctFreeMeal, PctFamilyPoverty, Enrolled, and
#   TotalSchools as predictors. Transform variables as necessary to improve prediction and/or
#   interpretability.)

#10. What variables predict whether or not a district's reporting was complete?


head(sDataJoin)

sDOut <- glm(formula = complete ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, family = binomial(), data = sDataJoin)
summary(sDOut)
anova(sDOut, test="Chisq")


# the wider the gap between null deviance and residual deviance shows out our model
# is doing againt the null model. 

# 
# Call:
#   glm(formula = complete ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + 
#         Enrolled + TotalSchools, family = binomial(), data = sDataJoin)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.7576   0.2356   0.2928   0.3608   2.2462  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       3.8665273  0.4730562   8.174    3e-16 ***
#   PctChildPoverty   0.0363563  0.0309062   1.176 0.239457    
# PctFreeMeal      -0.0194700  0.0108264  -1.798 0.072117 .  
# PctFamilyPoverty -0.0506706  0.0381356  -1.329 0.183948    
# Enrolled          0.0021242  0.0006381   3.329 0.000872 ***
#   TotalSchools     -0.2084117  0.0565684  -3.684 0.000229 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 323.23  on 699  degrees of freedom
# Residual deviance: 283.04  on 694  degrees of freedom
# AIC: 295.04
# 
# Number of Fisher Scoring iterations: 6



# Perform Boruta search
library(caret)
varImp(sDOut)
# Overall
# PctChildPoverty  1.176346
# PctFreeMeal      1.798380
# PctFamilyPoverty 1.328697
# Enrolled         3.328781
# TotalSchools     3.684243




# Import the random forest library and fit a model

library(randomForest)
fit_rf = randomForest(complete ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, data = sDataJoin)
# Create an importance based on mean decreasing gini
importance(fit_rf)
# IncNodePurity
# PctChildPoverty       4.513721
# PctFreeMeal           6.333098
# PctFamilyPoverty      4.629761
# Enrolled              9.078441
# TotalSchools          6.833434
varImpPlot(fit_rf,type=2)


#11. What variables predict the percentage of all enrolled students with completely up-todate vaccines?

head(sDataJoin)

regOut <- lm(PctUpToDate ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, data=sDataJoin)
summary(regOut)
anova(regOut, test="Chisq")
varImp(regOut)
#anova to analyze table of deviance


varImp(regOut)

fit_rf2 = randomForest(PctUpToDate ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, data = sDataJoin)
# Create an importance based on mean decreasing gini
importance(fit_rf2)
varImpPlot(fit_rf2,type=2)


#12. What variables predict the percentage of all enrolled students with belief exceptions?
head(sDataJoin)
BelOut <- lm(PctBeliefExempt ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, data=sDataJoin)
summary(BelOut)
anova(BelOut, test="Chisq")
varImp(BelOut)



library(randomForest)
fit_rf3 = randomForest(PctBeliefExempt ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, data = sDataJoin)
# Create an importance based on mean decreasing gini
importance(fit_rf3)
# IncNodePurity
# PctChildPoverty       4.513721
# PctFreeMeal           6.333098
# PctFamilyPoverty      4.629761
# Enrolled              9.078441
# TotalSchools          6.833434

varImpPlot(fit_rf3,type=2)


# Mean Decrease gini by importance measures how much each feature contributes to the homogeneity in the data.
#if purity is high, then its most important

  
#13. What's the big picture, based on all of the foregoing analyses? The staff member in the
#state legislator's office is interested to know if certain school districts need financial aid
#to improve both their vaccination rates and their reporting compliance.
#What have youlearned that might inform this question?



library(mlbench)
library(e1071)
mN <-   naiveBayes(PctBeliefExempt ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, data = sDataJoin)
plot(mN)
