### Topic 04 : Linear restrictions: F test

## Example 01 (pls read details from Woolridge section 4.5, page 124)

## Import your major league baseball players data: mlb1

data(mlb1, package='wooldridge')
View(mlb1)

# first we calculate the critical value (CV) for alpha=1% using the F distribution with 3 and 347 d.f.:

qf(1-0.01, 3,347)

# Now estimate the Unrestricted OLS regression model:

res.ur <- lm(log(salary) ~ years+gamesyr+bavg+hrunsyr+rbisyr, data=mlb1)
summary(res.ur)



# Next estimate the Restricted OLS regression model:
res.r <- lm(log(salary) ~ years+gamesyr, data=mlb1)
summary(res.r)

# Extract R2 from both models:


( r2.ur <- summary(res.ur)$r.squared )
( r2.r <- summary(res.r)$r.squared )

# Now calculate F statistic:(you can get more details about F-statistic formula on page 126)

( F <- (r2.ur-r2.r) / (1-r2.ur) * 347/3 )

## Now compare your calculated F value with your critical F value and take decision whether your H0 is rejected or not

# You may also take decision based on p value = 1-cdf of the appropriate F distribution:

options(scipen = 999)
1-pf(F, 3,347)


## Now we will examine the same F test  using car package

# Unrestricted OLS regression:

res.ur <- lm(log(salary) ~ years+gamesyr+bavg+hrunsyr+rbisyr, data=mlb1)

# Load package "car" (which has to be installed on the computer)

library(car)

# F test

myH0 <- c("bavg","hrunsyr","rbisyr")

linearHypothesis(res.ur, myH0)



## Now we will examine more complicated null hypothesis  using car package
## Assume a sports reporter claims that the batting average plays no role, and
## the number of home runs has twice the impact as the number of runs batted in
## So, our null hypothesis is  H0= beta.bavg=0, beta.hrunsyr=2*beta.rbisyr

myH0 <- c("bavg", "hrunsyr=2*rbisyr")

linearHypothesis(res.ur, myH0)


## Reporting regression results as you see in empirical papers/published article (see section 4.6 and example 4.10, page 135)

data(meap93, package='wooldridge')
View(meap93)

# define new variable within data frame (wooldridge page 135)

meap93$benefits_salary <- meap93$benefits / meap93$salary

# Estimate three different models

model1<- lm(log(salary) ~ benefits_salary                       , data=meap93)
model2<- lm(log(salary) ~ benefits_salary+log(enroll)+log(staff), data=meap93)
model3<- lm(log(salary) ~ benefits_salary+log(enroll)+log(staff)+droprate+gradrate, data=meap93)


# Load package and display table of results
install.packages("stargazer")
library(stargazer)
stargazer(list(model1,model2,model3),type="text",keep.stat=c("n","rsq"))





### Topic 02 : Linear restrictions: LM test

## Example 5.3 (pls read details from Woolridge section 5.2, and page 178)

data(crime1, package='wooldridge')

View(crime1)

regmodel <- lm(narr86 ~ pcnv+ptime86+qemp86+avgsen+tottime, data=crime1)
summary(regmodel)


# 1. Estimate restricted model:
restr <- lm(narr86 ~ pcnv+ptime86+qemp86, data=crime1)

# 2. Regression of residuals from restricted model:
utilde <- resid(restr)
LMreg <- lm(utilde ~ pcnv+ptime86+qemp86+avgsen+tottime, data=crime1)
# R-squared:
(r2 <- summary(LMreg)$r.squared )

# 3. Calculation of LM test statistic:
LM <- r2 * nobs(LMreg)
LM

# 4. Critical value from chi-squared distribution, alpha=10%:
qchisq(1-0.10, 2)

# Alternative to critical value: p value
1-pchisq(LM, 2)

# Alternative: automatic F test (see above)
library(car)
unrestr <- lm(narr86 ~ pcnv+ptime86+qemp86+avgsen+tottime, data=crime1)
linearHypothesis(unrestr, c("avgsen=0","tottime=0"))





## Standardization: Beta coefficient
## Woolridge, page 189, Example 6.1


data(hprice2, package='wooldridge')

# Estimate model with standardized variables:


lm(scale(price) ~ 0+scale(nox)+scale(crime)+scale(rooms)+
     scale(dist)+scale(stratio), data=hprice2)
hprice2$price

scale(hprice2$price)


## Log functional form (6.2)

## Woolridge, page 191

data(hprice2, package='wooldridge')

# Estimate model with logs:
lm(log(price)~log(nox)+rooms, data=hprice2)


## Woolridge, example 6.2 page 162


data(hprice2, package='wooldridge')

View(hprice2)
res <- lm(log(price)~log(nox)+log(dist)+rooms+I(rooms^2)+
            stratio,data=hprice2)
summary(res)

# Using poly(...):
res <- lm(log(price)~log(nox)+log(dist)+poly(rooms,2,raw=TRUE)+
            stratio,data=hprice2)
summary(res)



## Woolridge, example 6.3 page 201


data(attend, package='wooldridge')

# Estimate model with interaction effect:
(myres<-lm(stndfnl~atndrte*priGPA+ACT+I(priGPA^2)+I(ACT^2), data=attend))

# Estimate for partial effect at priGPA=2.59:
b <- coef(myres)
b["atndrte"] + 2.59*b["atndrte:priGPA"] 

# Test partial effect for priGPA=2.59:
library(car)
linearHypothesis(myres,c("atndrte+2.59*atndrte:priGPA"))



## Woolridge, example 6.5 page 210 (Confidence interval, Prediction)


data(gpa2, package='wooldridge')

View(gpa2)



# Regress and report coefficients
reg <- lm(colgpa~sat+hsperc+hsize+I(hsize^2),data=gpa2)
reg

# Generate data set containing the regressor values for predictions
cvalues <- data.frame(sat=1200, hsperc=30, hsize=5)

# Point estimate of prediction
predict(reg, cvalues)

# Point estimate and 95% confidence interval
predict(reg, cvalues, interval = "confidence")

# Define three sets of regressor variables
cvalues <- data.frame(sat=c(1200,900,1400), hsperc=c(30,20,5), 
                      hsize=c(5,3,1))
cvalues
# Point estimates and 99% confidence intervals for these
predict(reg, cvalues, interval = "confidence", level=0.99)


## Woolridge, example 6.5 page 203 (Prediction Intervals)



# Regress (as before)
reg <- lm(colgpa~sat+hsperc+hsize+I(hsize^2),data=gpa2)

# Define three sets of regressor variables (as before)
cvalues <- data.frame(sat=c(1200,900,1400), hsperc=c(30,20,5), 
                      hsize=c(5,3,1))

# Point estimates and 95% prediction intervals for these
predict(reg, cvalues, interval = "prediction")



# Point estimates and 99% prediction intervals for these
predict(reg, cvalues, interval = "prediction", level = 0.99)






## Effects plots for non-linear specifications

# Repeating the regression from Example 6.2:
data(hprice2, package='wooldridge')

res <- lm( log(price) ~ log(nox)+log(dist)+rooms+I(rooms^2)+stratio,
           data=hprice2)

# Predictions: Values of the regressors:
# rooms = 4-8, all others at the sample mean:
X <- data.frame(rooms=seq(4,8),nox=5.5498,dist=3.7958,stratio=18.4593)

# Calculate predictions and confidence interval:
pred <- predict(res, X, interval = "confidence")

# Table of regressor values, predictions and CI:
cbind(X,pred)

# Plot 
matplot(X$rooms, pred, type="l", lty=c(1,2,2))




# Automatic effects plot using the package "effects" (please install package effects first)


library(effects)
plot( effect("rooms",res) )




