## Review and Extension of simple OLS----
rm(list=ls())



# Woolridge Book: page 33, example 2.4


data(wage1, package='wooldridge')

View(wage1)
# OLS regression:


summary(wage1$wage)
sd(wage1$wage)
regmodel2 <- lm(wage ~ educ, data=wage1)
plot(wage1$wage,wage1$educ,col="darkgreen")
abline(regmodel2,lwd=3,col="red")
plot(predict(regmodel2),residuals(regmodel2))
plot(predict(regmodel2),rstudent(regmodel2))
par(mfrow=c(2,2))
plot(regmodel2, pch=23 ,bg='orange',cex=2)
plot(hatvalues(regmodel2))
which.max(hatvalues(regmodel2))
plot(resid(regmodel2), rstudent(regmodel2), pch=23, bg='blue', cex=3)
plot(rstandard(regmodel2), rstudent(regmodel2), pch=23, bg='blue', cex=3)
qqnorm(rstandard(regmodel2), pch=23, bg='red', cex=2)
qqnorm(rnorm(500), pch=23, bg='red', cex=2)
abline(0, 1)

plot(fitted(regmodel2), sqrt(abs(rstandard(regmodel2))), pch=23, bg='red', ylim=c(0,1))

plot(fitted(regmodel2), resid(regmodel2), pch=23, bg='red', cex=2)
abline(h=0, lty=2)
plot(dffits(regmodel2), pch=23, bg='orange', cex=2, ylab="DFFITS")
plot(cooks.distance(regmodel2), pch=23, bg='orange', cex=2, ylab="Cook's distance")

# abline(regmodel2$coef[1], regmodel2$coef[2], col = "darkred", lwd = 3)
summary(regmodel2)

# Woolridge Book: page 34, example 2.5

#first import data
data(vote1, package='wooldridge')
str(vote1)
View(vote1)

# OLS regression (parentheses for immediate output):
( VOTEres <- lm(voteA ~ shareA, data=vote1) )

# scatter plot with regression line:
with(vote1, plot(shareA, voteA, col="darkgreen"))
abline(VOTEres$coef[1], VOTEres$coef[2], col = "darkred", lwd = 3)
summary(VOTEres)

#Extension of ceo salary and roe regression model----

data( ceosal1, package='wooldridge')
View(ceosal1)
# extract variables as vectors:
sal <- ceosal1$salary
roe <- ceosal1$roe

# regression with vectors:
CEOregres <- lm( sal ~ roe)

# obtain predicted values and residuals (topic 2.3, page 35)
sal.hat <- fitted(CEOregres)
u.hat <- resid(CEOregres)

# Wooldridge, Table 2.2, page 36: 
cbind(roe, sal, sal.hat, u.hat)[1:15,]


#Extension of wage and education regression model, wooldridge example 2.7, page 37

WAGEregres <- lm(wage ~ educ, data=wage1)

# obtain coefficients, predicted values and residuals
b.hat <- coef(WAGEregres)
wage.hat <- fitted(WAGEregres)
u.hat <- resid(WAGEregres)


# Confirm property (1): eqn 2.30, page 36
mean(u.hat)

# Confirm property (2): eqn 2.31
cor(wage1$educ , u.hat)

# Confirm property (3): eqn 2.32
mean(wage1$wage)

#now we get the same result using our regression model

b.hat[1] + b.hat[2] * mean(wage1$educ) 


#Goodnes of fit(R2) example 2.8, page 38 & 39----



CEOregres <- lm( salary ~ roe, data=ceosal1 )

# Calculate predicted values & residuals:
sal.hat <- fitted(CEOregres)
u.hat <- resid(CEOregres)

# Calculate R^2 in three different ways:
sal <- ceosal1$salary
var(sal.hat) / var(sal)
1 - var(u.hat) / var(sal)
cor(sal, sal.hat)^2



# Estimate log-level model----
lm( log(wage) ~ educ, data=wage1 )

lm( log(salary) ~ log(sales), data=ceosal1 )

# regression through the origin, with conatsnt only---- 

reg1 <- lm(salary~0+roe, data=ceosal1)
summary(reg1)

reg2 <- lm(salary~1,data = ceosal1)
summary(reg2)

mean(ceosal1$salary)

#Now compare three regression model-----
windows()
plot(ceosal1$roe,ceosal1$salary,col="darkblue",ylim = c(0,4000),xlab="Return on Equity", ylab="CEO Salary")

abline(reg1,lwd=2,lty=1, col="red")
abline(reg2,lwd=2,lty=2, col="olivedrab")
abline(CEOregres,lwd=2,lty=3,col="mediumvioletred")

legend("topleft",c("regression through the origin","constant only","Regression with intercept"),
       col=c("red","olivedrab","mediumvioletred"),lwd=2,lty = 1:3)




### Chapter 3: Multiple regression-----

#Example 3.1

data(gpa1, package='wooldridge')

# Just obtain parameter estimates:
lm(colGPA ~ hsGPA+ACT, data=gpa1)

# Store results under "GPAres" and display full table:
GPAres <- lm(colGPA ~ hsGPA+ACT, data=gpa1)
summary(GPAres)

data(wage1, package='wooldridge')

# OLS regression:
summary( lm(log(wage) ~ educ+exper+tenure, data=wage1) )
# OLS regression:
summary( lm(log(wage) ~ educ, data=wage1) )




data(k401k, package='wooldridge')

# OLS regression:
summary( lm(prate ~ mrate+age, data=k401k) )



data(crime1, package='wooldridge')

# Model without avgsen:
summary( lm(narr86 ~ pcnv+ptime86+qemp86, data=crime1) )

# Model with avgsen:
summary( lm(narr86 ~ pcnv+avgsen+ptime86+qemp86, data=crime1) )


#estimating the impact of ommited variable----


# Parameter estimates for full and simple model:
beta.hat <- coef( lm(colGPA ~ ACT+hsGPA, data=gpa1) )
beta.hat

# Relation between regressors:
delta.tilde <- coef(lm(hsGPA ~ ACT, data=gpa1) )
delta.tilde

# Omitted variables formula for beta1.tilde:
beta.hat["ACT"] + beta.hat["hsGPA"]*delta.tilde["ACT"]

# Actual regression with hsGPA omitted:
lm(colGPA ~ ACT, data=gpa1)




# Full estimation results including automatic SE :
res <- lm(colGPA ~ hsGPA+ACT, data=gpa1)
summary(res)

# Extract SER (instead of calculation via residuals)
( SER <- summary(res)$sigma )

# regressing hsGPA on ACT for calculation of R2 & VIF
( R2.hsGPA  <- summary( lm(hsGPA~ACT, data=gpa1) )$r.squared )
( VIF.hsGPA <- 1/(1-R2.hsGPA) )

# manual calculation of SE of hsGPA coefficient:
n <- nobs(res)
sdx <- sd(gpa1$hsGPA) * sqrt((n-1)/n)  # (Note: sd() uses the (n-1) version)
( SE.hsGPA <- 1/sqrt(n) * SER/sdx  * sqrt(VIF.hsGPA) )




data(wage1, package='wooldridge')

# OLS regression:
lmres <- lm(log(wage) ~ educ+exper+tenure, data=wage1)

# Regression output:
summary(lmres)

# Load package "car" (has to be installed):
install.packages("car")
library(car)
# Automatically calculate VIF :
vif(lmres)




#OLS Regression & Basic Machine learning-----

#Supervised Learning & Regression
#Extract the data and create the training and testing sample
#let's take the Boston dataset that is part of the MASS library in R Studio

#The problem statement is to predict 'medv' based on the set of input features.

#Step 1, call require library and import data
library(MASS)
library(ggplot2)
data("Boston")
View(Boston)
attach(Boston)
names(Boston)
write.csv(Boston,"Boston_ML.csv")


#Stpe2
#Split the sample data and make the model
#Split the input data into training and evaluation(testing) set and 
#make the model for the training dataset. 
#training dataset has 404 observations and testing dataset has 102 observations based on 80-20 split.

##Sample the dataset. The return for this is row nos.
set.seed(1734)
row.number <- sample(1:nrow(Boston), 0.8*nrow(Boston))
train <-  Boston[row.number,]
test <-  Boston[-row.number,]
dim(train)
dim(test)

#Explore the response variable
#Let's check for the distribution of response variable 'medv'

#the three distributions of 'medv' original, log transformation and square root transformation

##Explore the data.
ggplot(Boston, aes(medv)) + geom_density(fill="blue")
ggplot(train, aes(log(medv))) + geom_density(fill="blue")
ggplot(train, aes(sqrt(medv))) + geom_density(fill="blue")

#Model Building - Model 1
#Now as a first step we will fit the multiple regression models. 
#We will start by taking all input variables in the multiple regression

#Let's make default model.
model1 = lm(log(medv)~., data=train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

#Is there a relationship between predictor and response variables?
#Which of the predictor variables are significant?
#Is this model fit?
#Observation from the plot

#Fitted vs Residual graph
#Normal Q-Q Plot
#Scale-Location
#This shows how the residuals are spread and whether the residuals have an equal variance or not.

#Residuals vs Leverage
#The plot helps to find influential observations. 
#Here we need to check for points that are outside the dashed line. 
#A point outside the dashed line will be influential point and removal of that will affect the regression coefficients

#Model Building - Model 2


#we can remove the four lesser significant features ('zn', age' and 'indus' ) 
# remove the less significant feature
model2 = update(model1, ~.-zn-indus-age) 
summary(model2) 

par(mfrow=c(2,2))
plot(model2)

#Is there a relationship between predictor and response variable?
#F=131.2 is far greater than 1 and this value is more than the F value of the previous model.

#Which of the variable are significant?
#Now in this model, all the predictors are significant.
#Check for predictor vs Residual Plot
#In the next step, we will check the residual graph for all significant features from Model 2. 

##Plot the residual plot with all predictors.
attach(train)
require(gridExtra)
plot1 = ggplot(train, aes(crim, residuals(model2))) + geom_point() + geom_smooth()
plot2=ggplot(train, aes(chas, residuals(model2))) + geom_point() + geom_smooth()
plot3=ggplot(train, aes(nox, residuals(model2))) + geom_point() + geom_smooth()
plot4=ggplot(train, aes(rm, residuals(model2))) + geom_point() + geom_smooth()
plot5=ggplot(train, aes(dis, residuals(model2))) + geom_point() + geom_smooth()
plot6=ggplot(train, aes(rad, residuals(model2))) + geom_point() + geom_smooth()
plot7=ggplot(train, aes(tax, residuals(model2))) + geom_point() + geom_smooth()
plot8=ggplot(train, aes(ptratio, residuals(model2))) + geom_point() + geom_smooth()
plot9=ggplot(train, aes(black, residuals(model2))) + geom_point() + geom_smooth()
plot10=ggplot(train, aes(lstat, residuals(model2))) + geom_point() + geom_smooth()
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,ncol=5,nrow=2)

##Model Building - Model 3 & Model 4
###We can now enhance the model by adding a square term to check for non-linearity. 
###We can first try model3 by introducing square terms for all features ( from model 2).
###And in the next iteration, we can remove the insignificant feature from the model.

#Lets  make default model and add square term in the model.

model3 = lm(log(medv)~crim+chas+nox+rm+dis+rad+tax+ptratio+
              black+lstat+ I(crim^2)+ I(chas^2)+I(nox^2)+ I(rm^2)+ I(dis^2)+ 
              I(rad^2)+ I(tax^2)+ I(ptratio^2)+ I(black^2)+ I(lstat^2), data=train)
summary(model3)



##Removing the insignificant variables.
model4=update(model3, ~.-nox-rad-tax-I(crim^2)-I(chas^2)-I(rad^2)-
                I(tax^2)-I(ptratio^2)-I(black^2))
summary(model4)


par(mfrow=c(2,2))
plot(model4)

#Is there a relationship between predictor and response variables?
#F-Stat is 137.9 and it is far greater than 1. 
#So there is a relationship between predictor and response variable.

#Which of the predictor variable are significant?
#All predictor variables are significant.

#Is this model fit?
# R2 is 0.7946 and this is more ( and better ) than our first and second model.


##ML Prediction----
#the real goal of the model is to reduce the testing error
#we will use test dataset to evaluate the model
#We will make a prediction based on 'Model 4' and will evaluate the model. 
#we will predict the 'test' observation and will see 
#the comparison between predicted response and actual response value.
#RMSE explains on an average how much of the predicted value will be from the actual value

pred1 <- predict(model4, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$medv)^2)/length(test$medv))
c(RMSE = rmse, R2=summary(model4)$r.squared)
par(mfrow=c(1,1))
plot(test$medv, exp(pred1))

#Based on RMSE = 5.13, we can conclude that on an average predicted value will 
#be off by 5.13 from the actual value.





#Basic two sample bootstrapping -----
require(mosaic)
install.packages("mosaic")

#one sample bootstarp for mean
tv <- read.csv(file.choose())
basic <- subset(tv,cable=="Basic")

#calculate mean from the sample
xbar <- mean(~time,data=basic)


#what is resample, just an example

example_sample <- c(1,2,3,4,5)
resample(example_sample)
resample(example_sample)
resample(example_sample)

do(10)*resample(example_sample)

#Now do resample(with resample) using our basic data

resample(basic)
mean(~time,data=resample(basic))
do(5)* mean(~time,data=resample(basic))
trails <- do(100)* mean(~time,data=resample(basic))
hist(trails$mean, col=c("green", "red","blue"))
mean(trails$mean)


##Obtaining standard error manually

se <- sd(trails$mean)

#95% confidence interval

xbar+2*se
xbar-2*se


#for bootstrap percentile confidence interval

confint(trails, level = 0.95, method = "quantile")
confint(trails, level = 0.90, method = "quantile")




























