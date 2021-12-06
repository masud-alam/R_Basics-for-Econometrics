
#Discrete distribution

# We will use package foreign, please install the package first, if you didnt
install.packages("foreign")
require(foreign)

#download wooldridge data

affairs <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/affairs.dta")

#alternatively you can download data from the Blackboard
# or you can use package Wooldridge
# require(wooldridge)
# data(package="wooldridge")
# data("affairs")
# View(affairs)
View(affairs)

str(affairs)
View(affairs$kids)

class(affairs$kids)

#create factors for kids and for marraige and attach labels

haskids <- factor(affairs$kids,labels = c("no","yes"))

#for ratmarr collum, create five labels and convert the col values

mlab <- c("very unhappy","unhappy","average","happy","very happy")
marriage <- factor(affairs$ratemarr,labels = mlab)
marriage
table(haskids)#frequencies for kids
prop.table(table(marriage)) #marraige ratings and check the share/proportions


#Now make a contingency table and counts(display and store variables)

(countstab <- table(marriage,haskids))

#now we will see the share with in marraige,i.e with in a row (1)
prop.table(countstab,margin = 1)

#next check share within "haskids",i.e with in a column
prop.table(countstab,margin = 2)

#lets make some grpah to depcit above information
pie(table(marriage),col = c("blue","green","yellow","red","grey"),main = "Proportion of marriage couple")
table(marriage)

# x <- c(16,66,93,194,232)
# library(plotrix) # you need this package to draw 3D plot. it looks cool!!
# pie3D(x,labels=mlab,explode=0.1,
#       main="Distribution of marriage status ")

barplot(table(marriage),horiz = F,las=1,
        main = "Distribution of happiness",ylim = c(0,180), col = c("blue","green","yellow","red","purple"))

barplot(table(haskids, marriage),horiz = T,las=1,
        legend=T, args.legend = c(x="bottomright"),
        main = "Happiness by kids",col = c("green","purple"))



barplot(table(haskids, marriage),beside = T,las=2,
        legend=T, args.legend = c(x="topleft"),
        main = "Happiness by kids",col = c("green","purple"))

## Continuous distribution
#now dowload ceo salary data to see continuous distribution

ceodata <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/ceosal1.dta")

# extract ROE to single vector

roe <- ceodata$roe

#make a histogram for counts

hist(roe, col = c("blue","green","yellow","red","purple"))

# make histogram with explicit densities and explicit breaks

hist(roe, col = c("blue","green","yellow","red","purple"), breaks = c(0,5,10,20,30,60))
# now make a Kernel density plot

plot(density(roe), col="blue")

# we can overlay

hist(roe,freq = F,ylim = c(0,0.07), col = c("blue","green","yellow","red","purple"))
lines(density(roe),lwd=3, col="green")

# Empirical cumulative distribution function

plot(ecdf(roe),col="green",lwd=4,grid=T)
grid(3, lty = 1, lwd = 2)
boxplot(roe,horizontal = T, col=c("blue"))
#Draw box plots for roe, broken down by consprod
boxplot(roe~ceodata$consprod, col=c("green","red"))


#Please read the following example before starting running codes
#(Appendix B and C of Wooldridge book) B.6, C.2,C.3,C.5,C.6, C.7

##wooldridge book: example (from appendix C)  C.2
# How to make confidence interval----

n=3000;p=.85                    # set appropriate prob and number
y=rbinom(100,n,p)
ybar <- mean(y)
m <- length(y)
s <- sd(y)
se <- s/sqrt(m)
c <- qt(0.975,m-1)
ci <- c(ybar-c*se,ybar+c*se)
ci


# text book example
scrp87 <- c(10,1,6,0.45,1.25,1.3,1.06,3,8.18,1.67,0.98,1,0.45,5.03,8,9,18,0.28,7,3.96)
scrp88 <- c(3,1,5,0.5,1.54,1.5,0.8,2,0.67,1.17,0.51,0.5,0.61,6.7,4,7,19,0.2,5,3.83)
#need to see the chnage between two years of scrap

change <- scrp88-scrp87
change
avgchnge <- mean(change)
nn <- length(change)
sdch <- sd(change)
se <- sdch/sqrt(nn)
c <- qt(0.975,nn-1)

##finally confidence interval

ci <- c(avgchnge-c*se,avgchnge+c*se)
ci


##Once again lets see call our favourite package (woodridge)
#first install the package
# install.packages("wooldridge")


require(wooldridge)

#now see how many data sets are available in wooldridge package
data(package="wooldridge")

#now import ceo salary data
data("ceosal1")
View(ceosal1)


#exp C.3, race discrimination in hiring


data("audit")
View(audit)
str(audit)
avgy <- mean(audit$y)
n <- length(audit$y)
sdy <- sd(audit$y)
se <- sdy/sqrt(n)
c <- qt(0.975,n-1)
c <- qnorm(0.975)
c

## construct 95% confidence interval

ci1 <- c(avgy-c*se,avgy+c*se)
ci1

## construct 99% confidence interval

ci2 <- c(avgy-qnorm(0.995)*se,avgy+qnorm(0.995)*se)
ci2

##how to find critical values

df <- 19 #this is degrees of freedom as of n-1

#significance levels
alpha.one.tailtest<- c(0.1,0.05,0.025,0.01,0.005,0.001)
alpha.two.tailtest <- alpha.one.tailtest*2

# find crticical values


cv <- qt(alpha.one.tailtest,df)
cbind(alpha.one.tailtest,alpha.two.tailtest,cv)


##wooldridge book: example (from appendix C) C.5

t <- avgy/se
t
##critical values for t distribution with df 240=n-1
alpha.one.tailtest<- c(0.1,0.05,0.025,0.01,0.005,0.001)
cv <- qt(1-alpha.one.tailtest,n-1)
cbind(alpha.one.tailtest,cv)




#conditional statement----
#please see the other R-script file

#Monte Carlo Simulation----
#first, once again a simulation experiment

# Set the random seed
set.seed(123456)

# Draw a sample given the population parameters
sample <- rnorm(100,10,2)

# Estimate the population mean with the sample average
mean(sample)


# Draw a different sample and estimate again:
sample <- rnorm(100,10,2)
mean(sample)

# Draw a third sample and estimate again:
sample <- rnorm(100,10,2)
mean(sample)

#So, all sample means are around the ture mean=10

#Now draw 10000 samples of size n=100 and calculate the sample avaerage for all of them

# Set the random seed
set.seed(123456)

# initialize ybar to a vector of length r=10000 to later store results:
r <- 10000
ybar <- numeric(r)

head(ybar)
tail(ybar)
# repeat r times:
for(j in 1:r) {
        # Draw a sample and store the sample mean in pos. j=1,2,... of ybar: 
        sample <- rnorm(100,10,2)
        ybar[j] <- mean(sample)
}

#In above we ended up with a vector of 10000 estimates from different samples
#Now we will see the simulation result

# The first 20 of 10000 estimates:
ybar[1:20]

# Simulated mean:
mean(ybar)

# Simulated variance:
var(ybar)
sd(ybar)
# Simulated density:
plot(density(ybar), col="Green",lty=5)
curve( dnorm(x,10,sqrt(.04)), add=TRUE,col="red",lty=6)

# This simulated results confirm the theoreical results
#Finally, lets see the simulation of confidence intervals and t-test

# Set the random seed
set.seed(123456)

# initialize vectors to later store results:
r <- 10000
CIlower <- numeric(r); CIupper <- numeric(r)
pvalue1 <- numeric(r); pvalue2 <- numeric(r)

# repeat r times:
for(j in 1:r) {
        # Draw a sample
        sample <- rnorm(100,10,2)
        # test the (correct) null hypothesis mu=10:
        testres1 <- t.test(sample,mu=10)
        # store CI & p value:
        CIlower[j] <- testres1$conf.int[1]
        CIupper[j] <- testres1$conf.int[2]
        pvalue1[j] <- testres1$p.value
        # test the (incorrect) null hypothesis mu=9.5 & store the p value:
        pvalue2[j] <- t.test(sample,mu=9.5)$p.value
}

# Test results as logical value
reject1<-pvalue1<=0.05;  reject2<-pvalue2<=0.05
table(reject1)
table(reject2)
#now we will show our CI in grpahs
color <- rep(gray(0.5),100)
color[reject1[1:100]] <- "red"
#prepare empty plot with correct axis limits and levels
plot(0,xlim = c(9,11),ylim = c(1,100), ylab = "Number of samples",
     xlab = "Confidence bands",main = "coorect null hypothesis")
#set vertical line at 10
abline(v=10,lty=2)
#add the 100 first CIs (y is equal to j for each poiunts)
for (j in 1:100){
        lines(c(CIlower[j],CIupper[j]),c(j,j),col=color[j],lwd=2)
}

## and thats all for todays class. 


