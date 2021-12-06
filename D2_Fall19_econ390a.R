setwd("C:/Users/masud/Desktop/Econ390aFall2019/Day2")

getwd()

#Vectors & Types

x <-  c(4, 7, 9)
x
str(x)
## [1] 4 7 9
y <-  c('a', 'b', 'c')
y
str(y)
## [1] "a" "b" "c"


x1 <- c(10,12.5,6,7,12) #creates a vector named x1 of type Double
typeof(x1)
str(x1)

y1 <- c(6L,2L,5L,7L,65L) #creates a vector named y1 of type integer
typeof(y1)
str(y1)


x2 <- c(TRUE,F, T,FALSE, T, T) #creates a vector named x2 of type logical
typeof(x2)
str(x2)


x3 <- c("Bieber","Maroon5","Tyga","Drake","Baldwin")#creates a vector named x3 of type character
typeof(x3)
str(x3)

x4 <- c(micro=2.3,macro=3.4,math=3.5,soc=3.6)#creates a vector named x4 of type double with element names
typeof(x4)
str(x4)


#What happens when we try and combine things that aren't so obviously related?
  
x5 <- c(10, TRUE, "Baldwin")
x5
str(x5)
# This is known as Coercion;the order is logical < integer < double < character

#Factor - Factors are used to store predefined and categorical data.
x <- factor(c("single", "married", "married", "single"));

x

x1 <- factor(c("single", "married", "married", "single"), levels = c("single", "married", "divorced"));

x1

# Vector Arithmetic and Functions


y2 <- c(1, 2, 3)
y2 + 4
## [1] 5 6 7
y2/3
## [1] 0.3333333 0.6666667 1.0000000
-y2

y2^3

## [1]  1  8 27
y3 <-  c(3, 2, 1)
y2 - y3
## [1] -2  0  2
y2 * y3
## [1] 3 4 3
y2/y3
## [1] 0.3333333 1.0000000 3.0000000
y2 > 2
## [1] FALSE FALSE  TRUE
y2 >= 2
## [1] FALSE  TRUE  TRUE


#We can also apply any number of ubiquitous functions to our vector input
# Use built-in function in R

x = c(1, 2, 3)
#sum: add up the elements of a vector
sum(x)
## [1] 6
#Just like you can use the command sum to add up the
#  elements of a numeric vector, you can use
#  prod to take their product:
prod(x)
## [1] 6
sqrt(x)
## [1] 1.000000 1.414214 1.732051
y = c(-1, 2, 4)
#abs: absolute value
abs(y)
## [1] 1 2 4
#exp: exponential. exp(x) is e^x
exp(y)
## [1]  0.3678794  7.3890561 54.5981500
#log: _natural_ logarithm (base e)
log(x)
## [1] 0.0000000 0.6931472 1.0986123
#Note that these functions interpret their input 
#  as *radians* rather than degrees.
sin(x) + cos(y)
## [1]  1.3817733  0.4931506 -0.5125236
max(y)
## [1] 4
min(y)
## [1] -1
range(y)
## [1] -1  4
mean(x)
## [1] 2
median(x)


# Regularly-spaced sequences of numbers. These are created in R with : or seq

x <-  1:10
x
##  [1]  1  2  3  4  5  6  7  8  9 10
y <-  10:1
y
##  [1] 10  9  8  7  6  5  4  3  2  1
#some times the gap is not 1
z <-  seq(0, 1, by = .02)
z

#other times we care less about the gap and more
#  more about how many points we get out
w <- seq(0, 1, length.out = 20)
w


# Basic programming functions that you're likely to use all of the time

x <- 99:32
#length: how many elements (items) are there in x?
length(x)
## [1] 68
y <- c("hey despacito!", "must dance with you today") 
#what TYPE of variable does R think this is?
class(y)
## [1] "character"
#rep: repeat/reproduce
rep(y, 4)

#head/tail: display only the beginning/end
#  of an object -- very useful for very
#  large objects
x <-  1:100000
head(x)
## [1] 1 2 3 4 5 6
tail(x)


#Sub setting Vectors

x <- c(5, 4, 1)
x[1]
## [1] 5
x[3]
## [1] 1
x[1:2]
## [1] 5 4
x[2:3]

x <- 20:30
x
##  [1] 20 21 22 23 24 25 26 27 28 29 30
x[c(1, 3, 5)]
## [1] 20 22 24
x[c(5, 9)]
## [1] 24 28
x[seq(1, 10, by = 2)]


#Besides being an integer, something can be 
#a logical vector of the same length as the vector itself

x <- c(5, 6, 7)
x[c(TRUE, TRUE, FALSE)]
## [1] 5 6
x[c(FALSE, TRUE, FALSE)]
## [1] 6
x[c(FALSE, FALSE, TRUE)]
## [1] 7
X <- c(7,2,6,9,4,1,3)
Y <- X<3|X>=6 # Useful in data mining or extracting useful info from an excel data file

x <-  c(-1, 0, 1)
x > 0
## [1] FALSE FALSE  TRUE
x[x > 0]
## [1] 1
x[x <= 0]
## [1] -1  0


#replace parts of a vector by subsetting

x <-  c(-1, 5, 10)
x[3] <-  4
x
## [1] -1  5  4
x[x < 0] = 0
x

#Named Vectors

trump_ages <-  c(70, 46, 38, 34, 32, 22, 9)
trump_ages <-  c(Donald = 70, Melania = 46, Donald_Jr = 38, Ivanka = 34,
               Eric = 32, Tiffany = 22, Barron = 9)
trump_ages

str(trump_ages)
class(trump_ages)
attributes(trump_ages)
attr(trump_ages ,"names")

person_ages <-  c(70, 46, 38, 34, 32, 22, 9)
attr(person_ages ,"names") <- c("Donald", "Melania", "Donald_Jr", "Ivanka","Eric", "Tiffany", "Barron")
attributes(person_ages)             



#Or we can use names function
names(trump_ages) <-  c("Donald", "Melania", "Donald, Jr.", "Ivanka", "Eric", "Tiffany", "Barron")
trump_ages
trump_ages["Donald"]
## Donald 
##     70
trump_ages[c("Donald", "Barron")]


#List
#R has a different type of object besides a vector used 
#to store data of different types side-by-side: a list


x <- list(TRUE, 1, "Drake")
x

#Nested lists

x <- list(c(1, 2), c("Selina", "Bieber"), c(TRUE, FALSE), c(5L, 6L))
x
y <-  list(list(1, 2, 3), list(4:5), 6)
y
Mylist <- list(A=seq(8,36,4), campus="Huskie",idm=diag(3))
Mylist
names(Mylist)
Mylist$A # Use of dollar sign; we will talk more later about dollar sign

# Matrices
# Generating matrix A from one vector with all values


v <- c(2,-4,-1,5,7,0)
( A <- matrix(v,nrow=2) )

# Generating matrix A from two vectors corresponding to rows:
row1 <- c(2,-1,7); row2 <- c(-4,5,0)
( A <- rbind(row1, row2) )
# Generating matrix A from three vectors corresponding to columns:
col1 <- c(1,6); col2 <- c(2,3); col3 <- c(7,2)
( AT40 <- cbind(col1, col2, col3) )
# Giving names to rows and columns ( create matrix from AT40 of Ryan Seacrest):
colnames(AT40) <- c("Drake","Maroon5","Selina")
rownames(AT40) <- c("weekTop","Peak") 
AT40
# Indexing for extracting elements (still using AT40 from above):
AT40[2,1]
AT40[,2]
AT40 [,c(1,3)] 
AT40[2,c(1,2)]
AT40 [2,]
# Diaginal and identity matrices: 
diag( c(4,2,6) )
diag( 3 )

# Generating matrix A and B
A <- matrix( c(2,-4,-1,5,7,0), nrow=2)
B <- matrix( c(2,1,0,3,-1,5), nrow=2)
A
B
A*B
# Transpose:
(C <- t(B) )
# Matrix multiplication:
(D <- A %*% C ) 
# Inverse:
solve(D) 
# Giving names to rows and columns:
B = matrix(c(2, 4, 3, 1, 5, 7), nrow=3,ncol=2) 
C = matrix(c(7, 4, 2),nrow=3,ncol=1) 
# combine the columns of B and C with cbind
cbind(B, C)
# combine the rows of two matrices if they have the same number of columns 
D = matrix(c(6, 2),nrow=1,ncol=2)
rbind(B, D)


###Now Data Frame#########
#Working with Data Frames from scratch



##A data frame is used for storing data tables
topHit = c(1, 3, 5) 
s = c("Drake", "Swift", "Selina") 
at40 = c(TRUE, FALSE, TRUE) 
df = data.frame(topHit, s, at40) 
df
# Define one x vector for all:
year     <- c(2008,2009,2010,2011,2012,2013)
# Define a matrix of y values:
product1<-c(0,3,6,9,7,8); product2<-c(1,2,3,5,9,6); product3<-c(2,4,4,2,3,2)
sales_mat <- cbind(product1,product2,product3)
rownames(sales_mat) <- year
# The matrix looks like this:
sales_mat
# Create a data frame and display it:
sales <- as.data.frame(sales_mat)
sales
# Accessing a single variable:
sales$product2
# Generating a new  variable in the data frame:
sales$totalv1 <- sales$product1 + sales$product2 + sales$product3 
# The same but using "with":
sales$totalv2 <- with(sales, product1+product2+product3)

# The same but using "attach":
attach(sales)
sales$totalv3 <- product1+product2+product3
detach(sales)

# Result:
Sales

# Subset: all years in which sales of product 3 were >=3
subset(sales, product3>=3)

# Note: "sales" is defined in Data-frames.R, so it has to be run first!
# save data frame as RData file (in the current working directory)

save(sales, file = "oursalesdata.RData")
write.csv(sales,"mySales.csv")

# remove data frame "sales" from memory
rm(sales)

# Does variable "sales" exist?
exists("sales")

# Load data set  (in the current working directory):
load("oursalesdata.RData")
# Does variable "sales" exist?
exists("sales")
sales
# averages of the variables:
head(sales) 
str(sales) 
colMeans(sales)

#Build-in Data Frame in R
data()
#For example, here is a built-in data frame in R,called mtcars
mtcars
nrow(mtcars) 
ncol(mtcars)
mtcars["Mazda RX4", "cyl"]
head(mtcars)
tail(mtcars)      




