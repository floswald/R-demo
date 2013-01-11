

# R script for intro.Rmd

# Assigning Values to Objects

x <- 3.4
x
x <- "now x is a character string."
x
x <- c(3, 5, 2.1, 1001, 4.6)  # c() function combines single values into a vector.
x

typeof(x)
y <- letters[10:20]  # letters number 10 trough 20 of the english alphabet
typeof(y)
typeof(c(x, y))
str(c(x, y))  # compact display of structure of an object

# Subsetting a vector
x <- rnorm(n = 8)  # draw 8 random normal values
x
x[3:6]  # get elements 3,4,5,6
x[c(1, 5, 8)]  # elts 1,5 and 8
x[-c(1, 5, 8)]  # all elts except 1,5 and 8

# Helper functions seq, rep
x <- seq(from = 1, to = 15, by = 3)	# also "length" instead of "by"
x
str(x)
y <- rep(1:3, c(2, 3, 4))
y
z <- rep(c("oh my word"), 3)
z

# Your turn 1
# Your solution 1
x <- seq(from = 0, to = 18, by = 3)
x
typeof(x)
y <- c("are","my","favourite","numbers")
z <- c(x[c(6, 7)], y) # R "coerces" x and y to a common type. here: char
z
typeof(z)

# Some Vector Arithmetic
x <- 1:3
y <- 4:6
x + y
x <- 1:4
x + y  # recycling the shorter vector y. R gives a warning.

# matrix
m1 <- matrix(data = 1:9, nrow = 3, ncol = 3, byrow = TRUE)
m2 <- matrix(data = 1:12, nrow = 4, ncol = 3, byrow = FALSE)
m1
m2

# Combining matrices
rbind(m1, m2)  # glue together the last row of of m1 and first of m2
cbind(m1, t(m2))  # glue last col of m1 and first of t(m2)

# Subsetting a matrix
m1[2, ]  # row 2, all columns
m2[, 1]  # all rows, column 1
m1[c(1, 3), c(2, 3)]  # rows 1,3 and cols 2,3
colnames(m1) <- c("col1", "col2", "col3")
m1[, "col2"]
colnames(m1) <- NULL  # remove colnames

# basic matrix arithmetic
m1 + m2	# error

# Your turn 2
# Your solution 2
I <- diag(x = 2)
z <- matrix(data = rnorm(4), nrow = 2, ncol = 2)
A <- rbind(cbind(z, z, z), cbind(z, I, z), cbind(z, z, z))
A

# data.frame
df <- data.frame(cat.1 = rep(1:3, each = 2), cat.2 = 1:2, values = rnorm(6))
dim(df)
df

# Subsetting a data.frame

df$values
df$new.col <- df$cat.1 + df$values
df

# subset 1
subset(df, cat.1 == 1)  # select all obs were cat.1 equals 1

# subset 2
subset(df, select = c(cat.2, values))  # selects columns 'cat.2' and 'values'

# Removing columns
df$new.col <- NULL
df

# Appending Data: rbind
df2 <- df  # create df2 as an exact copy of df
df2$values <- 1:nrow(df)  # but change the entries for 'values'
rbind(df, df2)  # join them row-wise

# Looking at data.frames
data(LifeCycleSavings)  # data() shows available built-in datasets
LS <- LifeCycleSavings
head(LS)  # show the first 6 rows of data.frame LifecycleSavings.
summary(LS)  # function summary()

# ordering a data.frame
save.ranking <- order(LS$sr, decreasing = TRUE)
head(LS[save.ranking, ])

# packages
library(ggplot2) # now the content of ggplot2 (functions, data, etc) are "visible"
cut_interval     # for example, here's the code to function "cut_interval"

# Functions
matrix
span <- function(x) {
    stopifnot(is.numeric(x))  # stops if x is not numeric
    r <- range(x)  # range() gives the range
    rval <- abs(r[2] - r[1])  # computes and returns the interval spanned by x
    return(rval)  # returns result
}
myvec <- rnorm(50)  # draws 50 values from the standard normal pdf
span(myvec)

# Lists
l <- list(words = c("oh my word(s)"), mats = list(mat1 = m1, mat2 = m2), 
    funs = span)
l

# Working with Lists
str(l[[1]])
str(l[1])
l[[2]][[1]]
l$mats$mat1

# Adding and removing a new elements from a list
l$new.element <- rnorm(5)  # add a new element: 5 random draws from the standard normal
l$bool.value <- l$new.element > 0  # add a new element: previously drawn numbers positive?
l
l$new.element <- NULL  # delete an element
l$bool.value <- NULL
l$words <- NULL
l$funs <- NULL
l

# Your Turn 3
# Your Solution 3
apples <- data.frame(price = runif(min = 0, max = 4, n = 3), quantity = sample(1:4, size = 3))
pears <- data.frame(price = runif(min = 0, max = 4, n = 3), quantity = sample(1:4, size = 3))
fruit <- list()
fruit$apples <- apples
fruit$pears <- pears
fruit
# or all in one line:
fruit <- list(apples = data.frame(price = runif(min = 0, max = 4, 
    n = 3), quantity = sample(1:4, size = 3)), pears = data.frame(price = runif(min = 0, 
    max = 4, n = 3), quantity = sample(1:4, size = 3)))

# Factors
new.fac <- factor(x=c(1,1,2,5,5,5),labels=c("apples","pears","shoes"))
new.fac
table(new.fac)
plot(new.fac)

# Contingency tables of several factors
UCB <- as.data.frame(UCBAdmissions) 	# load built in data
head(UCB)
summary(UCB)	# notice how summary treats factors and numerics
table(UCB$Admit,UCB$Dept) 	# table admit vs Dept?

# Contingency tables 2
xtabs(Freq ~ Admit + Dept,data=UCB)
xtabs(Freq ~ Admit + Gender,data=UCB)

# Your Solution 4
xtabs(Freq ~ Admit + Gender + Dept,data=UCB)


# Workspace
ls()
rm(df, m1, m2, "%ni%", fruit, l)  # remove objects
ls()
rm(list = ls(all = TRUE))  # remove all objects
ls()

# A Sample Applied Econometric Project
# Reading Data Files
# Read FES
setwd(dir = "~/Dropbox/git/R-demo/")  # set working directory
fesdat.csv <- read.csv(file = "data/fesdat.csv")  # read the data in csv format. Note that 'file' could also be a URL
fesinc.csv <- read.csv(file = "data/fesinc.csv")  # read the income data in csv format
library(foreign)  # load foreign to read stata data
fesdat.dta <- read.dta(file = "data/fesdat2005.dta")
fesinc.dta <- read.dta(file = "data/fesinc2005.dta")
head(fesdat.csv)
head(fesdat.dta)

# Summary
dat <- fesdat.dta  # let's rename this to something shorter
inc <- fesinc.dta
summary(dat)
dat$year <- NULL  # we've only got one year, so useless info

# Factors
str(dat$marstat)  # marstat is already a factor
levels(dat$marstat)
dat$sex <- factor(dat$sex, labels = c("male", "female"))  # convert sex into a factor
summary(subset(dat, select = c(sex, marstat)))

# Contingency Tables of Factors
attach(dat)  # attach(x) makes all cols of x visible on the search path
table(sex)  # otherwise I'd have to type dat$sex here
table(marstat, sex)

# more complicated tables: xtabs()

f <- ~sex + kids0
class(f)  # formula with only RHS 'explaining' variables
xtabs(f)
xtabs(~kids0 + sex + nrooms)  # three way table
prop.table(xtabs(~sex + kids0), margin = 1)  # proportion of kids0 by sex
prop.table(xtabs(~sex + kids0), margin = 2)  # proportion of kids0 within age
detach(dat)  # remove dat from search path

# Merge Data
dat.inc <- merge(dat, inc, by = c("hhref"))  # if multiple keys: by=c('hhref','year')
head(dat.inc)  # note new column hhinc

# Cut intervals
dat.inc$age.cat <- cut_interval(dat.inc$age, length = 5)  # add col with age category
dat.inc$ced.cat <- cut_interval(dat.inc$ageced, length = 5)  # add col with age at educ completion category
str(dat.inc$age.cat)  # is a factor?
table(dat.inc$age.cat)
plot(dat.inc$age.cat)
table(dat.inc$ced.cat)

# Create Bins 2: decile of income?
inc.dec <- quantile(dat.inc$hhinc, prob = seq(0, 1, length = 11))  # income deciles
inc.dec
dat.inc$inc.cat <- cut(dat.inc$hhinc, inc.dec, labels = FALSE)
head(dat.inc[, c("hhinc", "inc.cat")])
table(dat.inc$inc.cat)



# summarise() and transform()
library(plyr)
summarise(dat.inc, mhhinc = mean(hhinc), vhhinc = sd(hhinc), mnumads = mean(numads))  # summarise over an entire data.frame
head(transform(dat.inc, inc.rank = rank(hhinc)), 10)  # add 'income rank'. head(...,10) to see first 10 lines only


# Summaries by groups of Variables
head(ddply(.data = dat.inc, .variables = "marstat", .fun = transform, 
    rank.marstat = rank(hhinc)), 10)
head(ddply(dat.inc, "marstat", transform, perc.of.max = hhinc/max(hhinc)), 
    10)
head(ddply(dat.inc, c("nrooms"), summarise, med.nondur.dur = median(nondur/durables, 
    na.rm = TRUE)), 10)
head(ddply(dat.inc, c("inc.cat"), summarise, med.nondur.dur = median(nondur/durables, 
    na.rm = T)), 10)
head(ddply(dat.inc, c("nrooms", "inc.cat"), summarise, med.nondur.dur = median(nondur/durables)), 
    10)

# more ddply{plyr}
ddply(.data = dat.inc, .variables = "sex", .fun = summarise, inc = mean(hhinc))  # mean income by sex?
ddply(dat.inc, c("sex", "numads"), summarise, alc = mean(alc), foodin = mean(foodin), 
    foodout = mean(foodout))

# ddply{plyr} 2
ddply(dat.inc, "sex", summarise, age.range = median(age), car.rooms = max(ncars/nrooms))
ddply(subset(dat.inc, numhhkid > 1), "ncars", function(x) data.frame(dur.inc = mean(x$durables/x$hhinc), 
    nondur.inc = mean(x$nondur/x$hhinc)))  # instead of summarise(), supply an 'anonymous' function

# Graphics and R
# Base R plots
plot(y = log(dat.inc$ndex), x = log(dat.inc$hhinc))  # many options. see ?plot and ?par
plot(factor(dat.inc$ncars))  # plot depends on data types

# Plots with ggplot
library(ggplot2)
p <- ggplot(data = dat.inc, aes(x = log(hhinc), y = log(ndex)))  # not a plot yet: misses a geom
p + geom_point()  # now that's a plot

# ggplot 2
dat <- subset(dat.inc, hhinc > 0 & ndex > 0)  # get rid of some negative values
# add another layer: color points by marstat, and decrease opacity to 0.4
p <- ggplot(data = dat, aes(x = log(hhinc), y = log(ndex)))  # not a plot yet: misses a geom
p + geom_point(aes(color = marstat), alpha = 0.4)
p + geom_point() + facet_wrap(~sex)  # split plot by factor sex

# More ggplots
ggplot(data = dat, aes(x = log(hhinc), y = log(ndex))) + geom_point(aes(color = age.cat), 
    alpha = 0.4)
ggplot(data = dat, aes(x = log(hhinc), y = log(ndex))) + geom_smooth(aes(color = age.cat), 
    method = "lm", se = FALSE)
sdat <- subset(dat, ced.cat %in% c("(10,15]", "(15,20]", "(20,25]") & 
    log(hhinc) > 3 & log(hhinc) < 9)  # select a subset of ced
ggplot(data = sdat, aes(x = log(hhinc), y = log(ndex))) + geom_point(aes(color = ced.cat), 
    alpha = 0.3) + geom_smooth(aes(color = ced.cat), se = FALSE, size = 1)

# Regression Models
mod.1 <- lm(formula = lndex ~ log(hhinc), data = dat)
summary(mod.1)

# ```update()``` a model
mod.2 <- update(mod.1, . ~ . - 1)  # leave formula as is, but substract the intercept
mod.3 <- update(mod.1, . ~ . + age + I(age^2))  # add age and age squared
coef(summary(mod.2))  # use coef() function to extract coefs only
coef(summary(mod.3))

# ```predict()``` data from a model
newdat <- expand.grid(hhinc = c(200, 300), age = c(20, 50, 80))
cbind(newdat, predict(mod.3, newdat))

# ```ggplot``` model coeficients
p <- ggplot(dat, aes(x = log(hhinc), y = lndex))
p + geom_point() + geom_abline(intercept = coef(mod.1)[1], slope = coef(mod.1)[2], 
    col = "red")

# ```update()``` a model 2: add a dummy variable
mod.4 <- update(mod.3, . ~ . + factor(nrooms))
summary(mod.4)

# Create an Age Profile for consumption
ggplot(subset(dat, ndex < 1000), aes(x = ndex)) + geom_density(aes(color = age.cat))

# Age Profile: Quantile Regression
library(quantreg)
qreg50 <- rq(formula = ndex ~ age, data = dat, tau = 0.5)  # median regression
qreg   <- rq(formula = ndex ~ age, data = dat, tau = seq(0.05, 0.95,
    le = 10))  # quantile reg at several quantiles
plot(summary(qreg))  # very nice plotting method

# Age Profile: Compare OLS and Quantile Reg
form    <- ndex ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5)
lin.mod <- lm(formula = form, data = dat)
taus    <- seq(0.1, 0.9, le = 9)  # desired quantiles
qr.mod  <- rq(formula = form, data = dat, tau = taus)
ages    <- sort(unique(dat$age))  # vector of ages in data
preds   <- data.frame(age = ages, OLS = predict(lin.mod, newdata = data.frame(age = ages)))
# add qreg predictions
preds <- cbind(preds, predict(qr.mod, data.frame(age = ages)))
head(preds)

# Plot OLS vs Quantile Reg Age Profiles
library(reshape)
mpreds <- melt(preds, id.vars = "age")  # now the data is long
head(mpreds)
ggplot(subset(mpreds, age > 22), aes(x = age, y = value, color = variable)) + 
    geom_line() + scale_y_continuous(name = "predicted nondurable expenditure")

# time series
data(UKDriverDeaths)
UK <- UKDriverDeaths
str(UK)
plot(UK)
plot(decompose(UK))
acf(UK)
pacf(UK)
