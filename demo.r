# OuR shot at James Banks' stata demonstration. 
# Have a look at the original code and file on the G100 moodle web-site.

# Welcome To R!
# (everything after a # character is a comment)




# assign values to objects with either "<-" or "=". Use "<-".

# different data structures: vectors, matrices, data.frames, lists.

# vectors
x <- 3.4
x
x <- "now x is a character string."
x
y <- letters
length(y)
dim(y)
y

# helpers for vector allocation: rep() and seq()
x <- seq(from=1,to=15,by=3)
x
y <- rep(1:3,c(2,3,4))
y
z <- rep(c("oh my word"),3)
z
length(x)
dim(x)
str(x)   # "structure?"
x <- "who goes there?"
str(x)

# some vector arithmetic

x <- 1:3
y <- 4:6
x + y

x <- 1:4
x + y  # recycling. all operations +-*/


# matrix
m1 <- matrix(data=1:9,nrow=3,ncol=3,byrow=TRUE)
m2 <- matrix(data=1:12,nrow=4,ncol=3,byrow=FALSE)
m1
m2
m1[2, ]   # row 2, all columns
m2[ ,1]   # all rows, column 1
diag(m2)

# basic matrix arithmetic
m1 + m2  # error
m1 * m2  # error. "+", "-" and "*" are element by element
m2 %*% m1
m1 %*% t(m2)

# data.frame
df <- data.frame(cat.1=rep(1:3,each=2),cat.2=1:2,values=rnorm(6))
df
# a matrix like object, but more user-friendly. R workhorse.
# can use the dollar sign to select a column. if column doesn't exist, a new one is created.
df$alphabet <- letters[1:6]
subset(df,alphabet=="c")
df[df$alphabet=="c",]
df$cat.1
df$new.col <- df$cat.1 + df$values	# make new column from 2 existing columns
df
df$new.col <- NULL	# remove a column by setting to NULL
df


# datasets are usually stored as a data.frames. there are several built-in datasets in base R. for example
data(LifeCycleSavings)
LS <- LifeCycleSavings
head(LS)	# show the first 6 rows of data.frame LifecycleSavings.

# ordering a data.frame
save.ranking <- order(LS$sr,decreasing=TRUE)	# the function order(x) returns the indices of x in ascending/descending order
LS[save.ranking,]	# if you plug in an index vector (i.e. save.ranking), the data.frame is reordered in accordance to those indices

# installing packages: "packages" are add on libraries, usually obtained from the CRAN. The great number of available packages is one of the strengths of R. install a package simply by typing, for example
install.packages("Ecdat")
# "Ecdat" is a package with many econometrics datasets from prominent textbooks or publications.
# load a package by calling the library function:
library(Ecdat)
help(package="Ecdat")	# get help for the package
data(Clothing)	# load a dataset from Ecdat
head(Clothing)


# functions
# R relies extensively on functions. you can look at the source code of a function by typing the name only, as in
matrix

# of course you can write your own functions, like this:
myfun <- function(x) {return(x)}
# this function only returns the argument. you can write any sequence of commands in the curled brackets, separated by a carriage return or a semicolon.

# lists
l <- list(words=z,mats=list(mat1=m1,mat2=m2),functions=myfun)
l

# access second element in list
l[[2]]
l["matrices"]  # access by name (if names were given)
l$matrices     # same
l$matrices$mat1[3, ]   # operate on a certain element

l$new.element <- rnorm(5)    # add a new element: 5 random draws from the standard normal
l$bool.value <- l$new.element>0   # add a new element: previously drawn numbers positive?
l

l$new.element <- NULL   # delete an element
l$bool.value <- NULL

# what is a character, numeric, vector, logical etc?

# your turn
# make a list "fruit" with two elements: "apples" and "pears". for both apples and pears make up 3 pairs of price/quantity values we might observe on a typical fruit market store them in a data.frame with colum names "price" and "quantity"
fruit <- list(apples=data.frame(price=c(2,1,1.5),quantity=c(3,2,4)),pears=data.frame(price=c(0.5,1,0.8),quantity=c(1,1,2)))
fruit


# workspace: what objects do we have?
ls()	# list function
rm(df,m1,m2)	# remove objects
rm(list=ls(all=TRUE))	# remove all objects
ls()


# A Sample Econometric Project


# First: set the working directory with the setwd() function.
setwd(dir="~/Dropbox/git/R-demo/")   # the setwd function accepts one argument: "dir", which must be a valid directory, supplied as a character.

# some functions do not require any arguments, either because it doesn't make any sense:
getwd()

# or because there are default arguments specified:
matrix()

# if you type "matrix(" or any other function name and an opening bracket in the R console or into RStudio, each will show you the required arguments and any default values. matrix() for example has
matrix(data=NA,nrow=1,ncol=1,byrow=FALSE,dimnames=NULL)
# you don't have to understand any of that, except that the function "matrix" accepts 5 arguments, and the default values are set to "NA", 1,1,FALSE and NULL.

# what is a character, numeric, vector, logical etc?



# R and data

# import data

# read.table()
# read.csv()
# read.dta() from library(foreign)

library(foreign)	# you don't have to install foreign, as it's shipped with R.
fesdat <- read.dta("data/fesdat2005.dta")	

# show first couple of observations
head(fesdat)	

summary(fesdat)
dim(fesdat)
str(fesdat)

# factors
str(fesdat$marstat)
levels(fesdat$marstat)
fesdat$sex <- factor(fesdat$sex,labels=c("male","female"))	# convert sex into a factor
head(fesdat)
summary(fesdat)

# summary statistics and tables
apply(X=fesdat[,c("numadern","age","foodin")],MARGIN=2,FUN="mean")

# contingency tables of factors
attach(fesdat)	# now all columns of fesdat are in the search path
table(sex)
table(marstat)

# cross tabulation
table(sex,marstat)

# formula
f <- ~ sex + marstat
class(f)  # formula with only RHS "explaining" variables
xtabs(f)	# same. this is a "formula"
xtabs(~ marstat + sex + numads)	# three way table
xtabs(~ sex + age)
prop.table(xtabs(~ sex + age),margin=1)	# proportion of age by sex
prop.table(xtabs(~ sex + age),margin=2) # proportion of sex within age


detach(fesdat)	# remove fesdat from search path











# merge datasets

# read second data set
incdat <- read.dta("fesinc2005.dta")
head(incdat)
incdat[incdat$hhref==1,]	# how many times does hhref number 1 show up? is this a panel?
incdat[incdat$hhref==134,]	# how many times does hhref number 134 show up? seems NOT to be a panel


# merge income data to other data for each household. there's an id "hhref" in both datasets.
alldata <- merge(fesdat,incdat,by=c("hhref"))	# we merge only by hhref. if households show up in several years, we do by=c("hhref","year")
head(alldata)

# we can drop the year columns. this is data from 2005 only.
# notice that the command x$y (x dollar y) refers to the subcomponent of x, called y. here, this is just the columns named "year.x" and "year.y"

names(alldata) # get the names of the components of an object by calling the names() function
alldata$year.x <- NULL	# remove a column from a data.frame by setting it to NULL
alldata$year.y <- NULL
head(alldata)

# alldata is a "data frame". Basically a matrix where each row is an observation and each column represents a different measurement (i.e. variable) on that observation
nrow(alldata)	# number of rows/observations
ncol(alldata)	# number of columns/variables
dim(alldata)	# same

# the entire memory of Stata is one big data.frame. The obvious advantage in R is that you can have many data.frames.
summary(alldata)  # summary stats for all variables
summary(alldata$durables)	# only for one
summary(alldata$marstat)	# note how the "summary" function behaves differently for different types of data. marstat is a categorical value, or in R-speak, a "factor". it's not an ordinal characteristic. so summary() just shows how many cases there are in each category. More general: everything in R is an OBJECT. 
str(alldata$marstat)	# shows structure of object alldata$marstat (in this case a variable). R has different methods (like summary() ) for different objects. 
str(alldata)	# let's see what alldata is



# task 2: get summary statistics by groups of variables
# --------------------------------------
# or: Split a large dataset, Apply a function to each piece, and combine the results
# this is a "Split-Apply-Combine" operation. enter the plyr package:

library(plyr)	

# amazing tutorial for plyr at http://plyr.had.co.nz/09-user/

# ddply: apply a function to a data.frame and return the resulting data.frame

ddply(.data=alldata,.variables=c("sex","numads"),.fun=summarise,alc=mean(alc),foodin=mean(foodin),foodout=mean(foodout)) # in words: ddply( use data "alldata", group results by variables sex and numads, use the funtion "summarise" to do a "summarise" operation, return in a suitably sized data.frame the new colums alc=mean(alc), output foodin, output foodout)
ddply(alldata,c("sex","marstat"),summarise, inc=mean(hhinc), alc=mean(alc))	# same by sex and marstat. notice that if I put function arguments in right order (or they are unambiguous in another sense), I don't have to say ddply(.data = alldata, ... )
ddply(alldata,"sex",summarise,inc=mean(hhinc))	# what is mean income by sex?
alldata$sex <- factor(alldata$sex,labels=c("Male","Female"))	# what does sex=1 mean? i guess 1 is male, 2 is female. let's put that in. this is a factor in R.
str(alldata$sex)	# str() shows the "structure" of any R object
help(factor)	# have a look at the help

withkids <- subset(alldata,numhhkid > 0)	# use the subset function to... subset!
head(withkids)
ddply(subset(alldata,numhhkid > 1),c("ncars"),function(x) data.frame(dur.inc=mean(x$dur/x$hhinc),nondur.inc=mean(x$nondur/x$hhinc)))	# the biggest strength of ddply and R is it's flexibility. suppose you want to have the mean ratios nondurable/income and durable/income by the number of cars

ddply(alldata,c("sex","marstat"),summarise, inc=mean(hhinc), alc=mean(alc))
# so: males consume most alcohol when the married spouse is not in the household.
# marrid women where spouse is not in household drink nothing at all. This is a peculiar group of data, would have to check.

# you can do many other summaries. see http://plyr.had.co.nz/09-user/


# task 3: graphically look at some relationships
# ----------------------------------------------

# budget share of food vs nondurable expenditure
plot(x=alldata$wfoodin, y=alldata$ndex)	# plot() is the R base package plotting function. it can be useful at times, because easy. but also a bit limited.

library(ggplot2)	# therefore Hadley Wickham gave us ggplot2.
p <- ggplot(data=alldata,aes(x=wfoodin,y=ndex))	# aes() is the "aestethic" mapping of data onto a graph. we map wfoodin as x and ndex as y
p + geom_point()	# now we choose a way to visualize this mapping. points.
p + geom_point() + scale_y_log10(name="log nondurable exp")	# place a log scale on y
p + geom_point(alpha=0.3) + scale_y_log10(name="log nondurable exp")	# 

# task 4: create bins of categorical variables
# ----------------------------------------------

# create some age bins
summary(alldata$age)
alldata$agecat <- cut(x=alldata$age, breaks=c(0,20,35,50,65,80))	# type help(cut)
table(alldata$agecat)	# table function produces a contigency table for factors

# create education bins for level of educ achieved.
summary(alldata$ageced)
alldata$educat <- cut(x=alldata$ageced,breaks = c(0,16,19,98),right=FALSE)
summary(alldata$educat)


# task 5: specify a model
# -----------------------

alldata <- alldata[!is.na(alldata$lndex),]	# get rid of NA entries in lndex
log.model <- lm(data = subset(alldata,wfoodin > 0),formula = log(wfoodin) ~ lndex)	# lm() is the 'linear model' function. type help(lm). subset to wfoodin>0 becaouse of log# lm() uses the formula interface, which is common in many R functions. a formula has a LHS and a RHS. we want to explain LHS by RHS variables. we say that
# "LHS explained by RHS", and that is in R "LHS ~ RHS". RHS can be any number of variables, or functions thereof>
# y ~ x + y + z
# y ~ x + y + z + y*z
# y ~ x + I(y^2) + z	the I() function takes an algebraic expression and takes it literally (i.e. makes sure the "^" operator gets passed as the "power" function into the formula, and not any other meaning it might have in R.
      
summary(log.model)	# look at the regression output
coef(log.model)		# prints the coefficients
# easy to visualize a 2 variable model:
plot.model <- ggplot(data=subset(alldata,wfoodin > 0), aes(x=lndex,y=log(wfoodin)))		# take subset of alldata, and map lndex as "x" and log(wfoodin) as "y"
plot.model	# empty plot: no layers!
plot.model <- plot.model + geom_point(alpha=0.3) 	# add a point for each (x,y) data
plot.model
plot.model <- plot.model + geom_abline(intercept = coef(log.model)[1], slope = coef(log.model)[2],color="red")	# add a straight line with intercept and slope from the model.
plot.model

# "add" stuff. that is 'update()' in R.
# e.g. "add an age category dummy".
log.model2 <- update(log.model, . ~ . + educat)	# that means "take model 'log.model', leave the LHS as is (that's the '.'), leave the RHS as is, but add variable 'agecat'".
summary(log.model2)	# prints a summary of the model.
# plot with different intercepts
p2 <- ggplot(data=subset(alldata,wfoodin > 0), aes(x=lndex,y=log(wfoodin),color=educat)) 	# the aes() function with color argument. means: group observations by category of "educat" and show the groups by assigning different colors. this is an additional layer we place on the plot. could also be "size=educat", or "shape=educat" and so on. check out the ggplot2 website for many examples.
p2 <- p2 + geom_point(alpha=0.6) + geom_abline(intercept = coef(log.model2)[1], slope = coef(log.model2)[2],color="red") 	# add points and the intercept of the base category
p2  <- p2 + geom_abline(intercept = sum(coef(log.model2)[c(1,3)]), slope = coef(log.model2)[2],color="green")	# add line with intercept for second educat. that's the sum of intercept plus the dummy on educat[16,19)
p2 <- p2 + geom_abline(intercept = sum(coef(log.model2)[c(1,4)]), slope = coef(log.model2)[2],color="blue")        
p2


log.model3 <- update(log.model2, . ~ . + agecat)	
summary(log.model2)	# let's see that
log.model3 <- update(log.model3, . ~ . - educat)	# remove educat 
summary(log.model3)

# add an interaction
log.model4 <- update(log.model, . ~ . + educat * agecat)	# adds all levels of educat, agecat, and agecat*educat
summary(log.model4)

# TODO iv reg
library(AER)	# install.packages('AER') if not yet done
iv.model <- ivreg( formula = wfoodin ~ factor(numads) + educat + numhhkid + lndex | factor(numads) + educat + numhhkid + hhinc + nrooms, data=alldata)
summary(iv.model)
# another ivreg function is explained in this video here. does first stage testing and has nicer syntax http://novicemetrics.blogspot.co.uk/2011/04/video-tutorial-on-iv-regression.html



# task 6: age profile of nondurable consumption
# =============================================

p1 <- ggplot(alldata, aes(y=ndex,x=age)) 	# base layer mapping age and nondurable expenditure
p1 + geom_point( alpha = 0.3 )	# points
ggplot(subset(alldata,ndex < 2000), aes(y=ndex,x=age)) + geom_point( alpha = 0.3)	# removing outliers, we see some kind of "hump-shape" over lifecycle. but a lot seems to be driven by outliers here. you could interpret this graph as saying that the variance in consumption expenditure increases with age.

qplot(data=alldata, x=ndex, geom = "density")	# short way of wrigin ggplot
ggplot(alldata,aes(x=ndex,color=educat)) + geom_density()	# by educat
ggplot(subset(alldata,ndex < 2000),aes(x=ndex,color=educat)) + geom_density()	# by educat and subset

# fit a quantile regression model on a 5-order polynomial of age
library(quantreg)
qreg <- rq( formula =  ndex ~ age , data=alldata, tau=0.5)
qreg50 <- update(qreg, .~. + I(age^2) + I(age^3) + I(age^4) + I(age^5))

# compare to mean and other quantiles
lm.model <- lm(formula = ndex ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5),data = alldata)
qreg10 <- update(qreg50, tau = 0.1)
qreg30 <- update(qreg50, tau = 0.3)
qreg70 <- update(qreg50, tau = 0.7)
qreg90 <- update(qreg50, tau = 0.9)

ages <- sort(unique(alldata$age))
# collect predicted values
preds <- data.frame(age = ages, ols= predict(object = lm.model, newdata = data.frame(age = seq(range(alldata$age)[1],range(alldata$age)[2],length=length(unique(alldata$age))))))
preds$qr50 <- predict(object = qreg50, newdata = data.frame(age = preds$age))
preds$qr10 <- predict(object = qreg10, newdata = data.frame(age = preds$age))
preds$qr30 <- predict(object = qreg30, newdata = data.frame(age = preds$age))
preds$qr70 <- predict(object = qreg70, newdata = data.frame(age = preds$age))
preds$qr90 <- predict(object = qreg90, newdata = data.frame(age = preds$age))


# ggplot2 works best with "molten" data, ie a data.frame that you "melt". melting means to heat up stuff until you can take it and cast it into another shape, right?
# well, that's what this does. type
head(preds)
# we have one id column "age", and 6 columns containing different measurements concerning id variable "age", i.e. each column has a different value for any age. ggplot
# wants a data.frame with at least 3 columns: the id variable (age), the category name (e.g. "ols"), and the correpsonding value. if you have more id variables (suppose "age" and "year" and "region"), you'll have 3 id columns. example.

library(reshape)
melt.preds <- melt(preds, id.vars="age")
head(melt.preds,80)

ggplot(data=melt.preds, aes(x=age,y=value,color=variable)) + geom_line()

# the rq() function has a nice standard plot method:
many.qregs     <- rq(formula =  ndex ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5) , data=alldata, tau=seq(0.05,0.95,by=0.05))
sum.many.qregs <- summary(many.qregs)
plot(sum.many.qregs)	# the red line is the corresponding OLS estimate. the plot is over quantiles of the outcome variable. so if the dash-dotted line (the estimate at the x-quantile is very different from the OLS one, this would be interesting. Here OLS seems to do a good job across all quantiles. the rq estimate is significantly different only at the very extreme quantiles, and there might be few observations:

