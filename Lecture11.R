
# Heteroscedatsicity test (BP test)

data(hprice1, package='wooldridge')

View(hprice1)

# Estimate model

reg <- lm(price~lotsize+sqrft+bdrms, data=hprice1)
reg

# Automatic BP test

library(lmtest)
bptest(reg)



# Heteroscedatsicity test (White test)

# Estimate model
reg <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice1)
reg

# BP test
library(lmtest)
bptest(reg)

# White test
bptest(reg, ~ fitted(reg) + I(fitted(reg)^2) )

#the null hypothesis is homoskedasticity and we cant rejct null 


# Weighted least square estimator


#Woolridge example 8.6


data(k401ksubs, package='wooldridge')

View(k401ksubs)

# OLS (only for singles: fsize==1)

lm(nettfa ~ inc + I((age-25)^2) + male + e401k, 
   data=k401ksubs, subset=(fsize==1))


#we assume that variance is proportional to income, so use 1/inc as weight 
# WLS
lm(nettfa ~ inc + I((age-25)^2) + male + e401k, weight=1/inc, 
   data=k401ksubs, subset=(fsize==1))

# we can also use heteroscedasticity robust statistics to account for the fact that
# our variance function might be misspecified

# WLS
wlsreg <- lm(nettfa ~ inc + I((age-25)^2) + male + e401k, 
             weight=1/inc, data=k401ksubs, subset=(fsize==1))

# non-robust results
library(lmtest); library(car)
coeftest(wlsreg)

# robust results (Refined White SE:)
coeftest(wlsreg,hccm)



## Heteroscedasticity:using gpa3 data

data(gpa3, package='wooldridge')
View(gpa3)

# load packages (which need to be installed!)
library(lmtest); library(car)

# Estimate model (only for spring data)
reg <- lm(cumgpa~sat+hsperc+tothrs+female+black+white, 
          data=gpa3, subset=(spring==1))
summary(reg)



# for the default homescedasticity-based SE:
coeftest(reg)
# Refined White heteroscedasticity-robust SE:
coeftest(reg, vcov=hccm)

# F-Tests using different variance-covariance formulas:
myH0 <- c("black","white")

# Ususal VCOV:for the dafault homo based cov matrix
linearHypothesis(reg, myH0)

# Refined White VCOV
linearHypothesis(reg, myH0, vcov=hccm)

# Classical White VCOV
linearHypothesis(reg, myH0, vcov=hccm(reg,type="hc0"))




###Chapter 15: Instrumental Variables and 2SLS

## Start with example 15.1 from Wooldridge, sect 15.1

library(AER);library(stargazer)
data(mroz, package='wooldridge')
View(mroz)


#check if there is any missing observation
is.na(mroz$wage) 
which (is.na(mroz$wage))


# create a new dataset without missing data; restrict to non-missing wage observations
oursample <- subset(mroz, !is.na(wage))
str(oursample)
is.na(oursample$wage) 


# OLS regression model
reg.ols <-   lm(log(wage) ~ educ, data=oursample)

# IV automatically 
reg.iv <- ivreg(log(wage) ~ educ | fatheduc, data=oursample) 

# Pretty regression table
stargazer(reg.ols,reg.iv, type="text")

####################################################################

## If we add more exogenogenous variable
## Example 15.4; ivreg command and | symbol

data(card, package='wooldridge')
View(card)

#Educ is assumed to be endogenous

## nearc4: dummy variable for whether someone grew up near a fouryear
#college  as an instrumental variable for education

#other standard controls: experience, a black dummy variable, dummy variables
#for living in an SMSA and living in the South, and a full set of regional dummy variables
#and an SMSA dummy for where the man was living in 1966

## For valid instrument, it must be uncorrelated with the error term in the wage equation

# Checking for relevance: reduced form
# run regression; educ on all exog variables including instrument nearc4



redf<-lm(educ ~ nearc4+exper+I(exper^2)+black+smsa+south+smsa66+reg662+
           reg663+reg664+reg665+reg666+reg667+reg668+reg669, data=card)

summary(redf)


#people who lived near a college in 1966 had, on average, 
#about one-third of a year more education than those who did not grow up near a college.



# OLS

ols<-lm(log(wage)~educ+exper+I(exper^2)+black+smsa+south+smsa66+reg662+
          reg663+reg664+reg665+reg666+reg667+reg668+reg669, data=card)
summary(ols)

# IV estimation
iv <-ivreg(log(wage)~educ+exper+I(exper^2)+black+smsa+south+smsa66+
             reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669 
           | nearc4+exper+I(exper^2)+black+smsa+south+smsa66+
             reg662+reg663+reg664+reg665+reg666+reg667+reg668+reg669
           , data=card)


# Pretty regression table of selected coefficients
stargazer(redf,ols,iv,type="text",
          keep=c("ed","near","exp","bl"),keep.stat=c("n","rsq"))




#######################################################################################

## 2SLS method: Example 15.5

# restrict to non-missing wage observations
oursample <- subset(mroz, !is.na(wage))

lm(log(wage)~educ+exper+I(exper^2)+motheduc+fatheduc, data=card)

# 1st stage: reduced form

stage1 <- lm(educ~exper+I(exper^2)+motheduc+fatheduc, data=oursample)

summary(stage1)
fitted(stage1)


# 2nd stage
man.2SLS<-lm(log(wage)~fitted(stage1)+exper+I(exper^2), data=oursample)

# Automatic 2SLS estimation

aut.2SLS<-ivreg(log(wage)~educ+exper+I(exper^2) 
                | motheduc+fatheduc+exper+I(exper^2) , data=oursample)

# Pretty regression table

stargazer(stage1,man.2SLS,aut.2SLS,type="text",keep.stat=c("n","rsq"))



# Example 15.7: Testing for exogenity of the regressors
# Using the control function approach
# first stage is same as before,
# But second stage adds the first stage residuals to the original list of regressors

# restrict to non-missing wage observations

oursample <- subset(mroz, !is.na(wage))

# 1st stage: reduced form

stage1<-lm(educ~exper+I(exper^2)+motheduc+fatheduc, data=oursample)

r1 <- resid(stage1)

# 2nd stage
stage2<-lm(log(wage)~educ+exper+I(exper^2)+r1,data=oursample)

# results including t tests
coeftest(stage2)


## Testing overidentifying restrictions, see section 15.5 for details

# IV regression

summary( reg.2sls <- ivreg(log(wage) ~ educ+exper+I(exper^2)
                           | exper+I(exper^2)+motheduc+fatheduc,data=oursample) )
resi_2sls <- resid(reg.2sls)

# Auxiliary regression

res.aux <-  lm(resi_2sls ~ exper+I(exper^2)+motheduc+fatheduc
               , data=oursample) 


# Calculations for test
( r2 <- summary(res.aux)$r.squared )
( n <- nobs(res.aux) )
( teststat <- n*r2 )
( pval <- 1-pchisq(teststat,1) )






In-class-practice: IVreg


# load the CigarettesSW data set using AER package and get an overview of the data set
library(AER)
data("CigarettesSW")
summary(CigarettesSW)

# compute real per capita prices
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

#  compute the sales tax
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

# check the correlation between sales tax and price
cor(CigarettesSW$salestax, CigarettesSW$price)

# generate a subset for the year 1995
c1995 <- subset(CigarettesSW, year == "1995")

# perform the first stage regression
cig_s1 <- lm(log(rprice) ~ salestax, data = c1995)

coeftest(cig_s1, vcov = vcovHC, type = "HC1")

# inspect the R^2 of the first stage regression
summary(cig_s1)$r.squared

# store the predicted values
lcigp_pred <- cig_s1$fitted.values

# run the stage 2 regression
cig_s2 <- lm(log(c1995$packs) ~ lcigp_pred)
coeftest(cig_s2, vcov = vcovHC)

# perform TSLS using 'ivreg()'
cig_ivreg <- ivreg(log(packs) ~ log(rprice) | salestax, data = c1995)

coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")


# An example of a complete data analysis/data science project 
#A comparision of socio-economic determinants of suicide rates by year and country
#This complete dataset pulled from the following four other datasets linked by time and place
#United Nations Development Program. (2018). Human development index (HDI)
#World development indicators: GDP (current US$) by country:1985 to 2016
#[Szamil]. (2017). Suicide in the Twenty-First Century [dataset]
#World Health Organization. (2018). Suicide prevention

library(tidyverse)
library(psych)
library(effsize)

#import xlsx data using import dataset icon/ command

str(df)
head(df)

#Checking the number of missing value in particular variable.

sapply(df, function(x) mean(is.na(df)))

#Those missing values that appear here are acceptable. 
#It's not that much. Only 6% each variable


#First Step: performing statistical analysis: GDP and Suicide
#Since we are going to find the correlation between suicide_no and gdp_for_year
#We have to check the assumption whether varibales are normally distributed


summary(df$suicides_no)
psych::skew(df$suicides_no)


df %>%
  ggplot(aes(x = suicides_no ))+
  geom_histogram(fill = "blue" , alpha = .50)



#Skewness is so high (10.35179). We have to pick either to transform this variable and pick Pearson as a method to 
#perform correlation test. Because in order to use Pearson as a method, your variable should be normally distributed.
#Or alternative solution, I don't have to transform this variable, but I have to pick Kendall or Spearman as a method
#to perform correlation test. Let look at gdp_for_year variable and find its skewness.

summary(df$`gdp_for_year ($)`)
psych::skew(df$`gdp_for_year ($)` , na.rm = TRUE)

df %>%
  ggplot(aes(x = `gdp_for_year ($)`))+
  geom_histogram(fill = "red" , alpha = .50)

#Since both variables are not normally distributed (Their skewness are higher than 1). Therefore 
#We will use Spearman as a method to find the correlation between those two variable.

(cor1 <-  corr.test(df$suicides_no , df$`gdp_for_year ($)` , method = "spearman"))

#Let look at the confidence interval 
#and see if there is any correlation in population or not
#Looking for confident interval
cor1$ci.adj

cohen.d(df$suicides_no , df$`gdp_for_year ($)`)

#Cohen's d testing is consider to be the important test to perform in everytime 
#before we really make the conclusion. Effect size on 
#the above correlation is -0.4335046. It's considering to be small effect size. 

df %>%
  ggplot(aes(x = `gdp_for_year ($)` , y = suicides_no))+
  geom_jitter(alpha = .30 , color = "blue")+
  geom_smooth(method = 'lm' ,color = "red")


#Examine the realtionship between HDI(Human Development Index) and Suicide

#we must check the assumption and find the value of skewness 
#in order to pick the optimum method to perform any test

#Checking the HDI assumption
summary(df$`HDI for year`)
psych::skew(df$`HDI for year`)


df %>%
  ggplot(aes(x = `HDI for year`))+
  geom_histogram(fill = "green" , alpha = .50)

# Transforming suicides_no variable
df <- df %>%
  mutate(suicides_no_log = log(suicides_no + 1))

psych::skew(df$suicides_no_log)

df %>%
  ggplot(aes(x = suicides_no_log))+
  geom_histogram(fill = "red" , alpha = .50)


# finding the correlation
cor.test(df$suicides_no_log , df$`HDI for year` , method = "pearson")

#There is a significant correlation between suicides_no and HDI  
#But the correlation matrix itself is relatively small.
#Only 0.1801759. Even in population, it's lies between 0.1593597 and 0.2008318.

cohen.d(df$suicides_no_log , df$`HDI for year` , na.rm = TRUE)

#effect size of this correlation is so large (the optimum scale for
#the effect size is 0.80, but this correlation is 1.233637)
#Therefore, we can say that there is a significantly very small positive correlation between suicides_no and HDI for year.
df %>%
  ggplot(aes(x = `HDI for year` , y = suicides_no_log))+
  geom_jitter(color = "palegreen3" , alpha = .30)+
  geom_smooth(method = "lm", color="orangered3")

#Analyse data for different eneration 
#Showing difference in generation may also cause the number of suicide rate itself

df %>% 
  ggplot(aes(x = generation , y = suicides_no , fill = generation))+
  geom_boxplot(alpha = .50)+
  coord_cartesian(ylim = c(0,500))

options(scipen = 999)
gen.aov <- aov(suicides_no ~ generation , data = df)
summary(gen.aov)
#there is significant different between the generation group
#But it's not telling us about which group have the significantly highest suicides rate.

pairwise.t.test(df$suicides_no , as.vector(df$generation))

#we can say that Boomers generation is significant highest generation in term of suicide rate

#From correlation to regression analysis
summary(df)
library(gridExtra)
plt1 <- df %>%
  ggplot(aes(x = `gdp_for_year ($)`))+
  geom_histogram(alpha = .50 , fill = "red")+
  xlab("GDP per year in Dollar")
plt2 <- df %>%
  ggplot(aes(x = population))+
  geom_histogram(alpha = .50 , fill = "green")+
  xlab("Population")
plt3 <- df %>%
  ggplot(aes(x = `gdp_per_capita ($)`))+
  geom_histogram(alpha = .50 , fill = "blue")+
  xlab("GDP per capital in Dollar")
plt4 <- df %>%
  ggplot(aes(x = `suicides/100k pop`))+
  geom_histogram(alpha = .50 , fill = "purple")+
  xlab("Suicides / 100k of population")

grid.arrange(plt1 , plt2 , plt3 , plt4 , nrow = 2)


#Transforming variable
df <- df %>%
  mutate(gdp_for_year_log = log(`gdp_for_year ($)`),
         gdp_per_capital_log = log(`gdp_per_capita ($)`),
         population_log = log(population),
         suicides_per_100k_pop_log = log(`suicides/100k pop` + 1))

#Now plot variables after transformation

plt1 <- df %>%
  ggplot(aes(x = gdp_for_year_log))+
  geom_histogram(alpha = .50 , fill = "red")+
  xlab("GDP per year in Dollar")
plt2 <- df %>%
  ggplot(aes(x = population_log))+
  geom_histogram(alpha = .50 , fill = "green")+
  xlab("Population")
plt3 <- df %>%
  ggplot(aes(x = gdp_per_capital_log))+
  geom_histogram(alpha = .50 , fill = "blue")+
  xlab("GDP per capital in Dollar")
plt4 <- df %>%
  ggplot(aes(x = suicides_per_100k_pop_log))+
  geom_histogram(alpha = .50 , fill = "purple")+
  xlab("Suicides / 100k of population")

grid.arrange(plt1 , plt2 , plt3 , plt4 , nrow = 2)

reg_1 <- lm(data = df, suicides_no_log ~ `HDI for year`)
summary(reg_1)
reg_2 <- lm(data = df , suicides_no_log ~ `HDI for year` + gdp_for_year_log)
summary(reg_2)

reg_3 <- lm(data = df , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex)
summary(reg_3)

reg_4 <- lm(data = df , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex + age)
summary(reg_4)

reg_5 <- lm(data = df , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex + age + generation)
summary(reg_5)


reg_6 <- lm(data = df , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex + age +  gdp_per_capital_log)
summary(reg_6)

#To evaluate the model whether it's doing a great job or not
#One of the assumption we can check is the residual distribution
#We expected to be normally distributed.

ggplot(data = as.data.frame(reg_6$residuals), aes(x = reg_6$residuals))+
  geom_histogram(alpha = .60 , fill = "yellowgreen")

#We have to order the cause/determinants of suicide rate from high to low
#Since the Estimate column above did not tell us much about which regressor
#contribute the most due to the different in the scale between variable
#We have to convert each regressor to standard scale(standardized)
library(lm.beta)
summary(lm.beta(reg_6))

require(car)
vif(reg_6)

require(stargazer)

stargazer(list(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6),type="text",keep.stat=c("n","rsq"))





