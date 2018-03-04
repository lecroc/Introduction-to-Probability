# load data

load("brfss2013.Rdata")

# look at dimensions

dim(brfss2013)

# look for n/a

nas<-sum(is.na(brfss2013))

complete<-sum(!is.na(brfss2013))

total<-nas+complete

pctna<-nas/total

pctna

# select columns I want for my questions

# State column 1
# Veteran column 45
# Employment Status 49
# General Health 19
# Smoker 68
# Heart Attack 33

list<-c(1,19,33,45,49,68)

mydata<-brfss2013[,list]

