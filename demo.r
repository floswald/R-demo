# R shot at James Banks' stata demonstration. 
# you can get the original stata file at https://moodle.ucl.ac.uk/course/view.php?id=8932 (ECONG100 moodle)

setwd("~/Dropbox/teaching/UCL-R")	# set the working directory like this. 

# task 1: read two datasets and merge them
# --------------------------------------

# read stata data with function read.dta() from library "foreign"
library(foreign)	# load library "foreign". type help(package="foreign") to see what file formats can be read.
fesdat <- read.dta("fesdat2005.dta")	# you can use "<-" or "=" to assign some value to an object. use "<-" if you want to be called a proper useR by your friends. 

# show first couple of observations
head(fesdat)	# show first 6 lines (default)
head(fesdat,10) # show first 10

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
