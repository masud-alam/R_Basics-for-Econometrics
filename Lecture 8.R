##Topic2: Multiple regression: Inference (chapt 4)
## t-test and p-values, and hypothesis testing

# Example 4.3(woolridge)

# CV for alpha=5% and 1% using the t distribution with 137 d.f.:


alpha <- c(0.05, 0.01)
qt(1-alpha/2, 137)

# Critical values for alpha=5% and 1% using the normal approximation:


qnorm(1-alpha/2)
require(wooldridge)
data("sleep75")
View(sleep75)
write.csv(sleep75,"sleep75.csv")


getwd()
# Store results under "sumres" and display full table:


( sumres <- summary( lm(colGPA ~ hsGPA+ACT+skipped, data=gpa1) ) )

# Manually confirm the formulas: Extract coefficients and SE


regtable <- sumres$coefficients
bhat <- regtable[,1]
bhat

se   <- regtable[,2]
se
# Reproduce t statistic


( tstat <- bhat / se )

options(scipen = 999)

# Reproduce p value

( pval  <- 2*pt(-abs(tstat),137) )


## t-test and p-values, and hypothesis testing(one sided)

# Example 4.1(woolridge)

# CV for alpha=5% and 1% using the t distribution with 522 d.f.:


alpha <- c(0.05, 0.01)
qt(1-alpha, 522)

# Critical values for alpha=5% and 1% using the normal approximation:


qnorm(1-alpha)

# OLS regression:

summary( lm(log(wage) ~ educ+exper+tenure, data=wage1) )


### Topic3 : Making confidence interval from multiple linear regression model (page 139)

## Example 01


# OLS regression:
myreg <- lm(log(rd) ~ log(sales)+profmarg, data=rdchem)

# Regression output:
summary(myreg)

# 95% CI:
confint(myreg)

# 99% CI:
confint(myreg, level=0.99)


### Topic 04 : Linear restrictions: F test

## Example 01 (pls read details from Woolridge section 4.5)

## Import your major league baseball players data: mlb1 (page 144)

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
