# load data

load("brfss2013.Rdata")

# load libraries

library(dplyr)
library(ggplot2)
library(tidyr)

# look at dimensions

dim(brfss2013)

# look for n/a

nas<-sum(is.na(brfss2013))

complete<-sum(!is.na(brfss2013))

total<-nas+complete

pctna<-nas/total

# Percentage of na values

pctna

# select columns I want for my questions

# State column 1
# Veteran column 45
# Time since checkup 26
# General Health 19
# Smoker 68
# Heart Attack 33

# list of columns I want

list<-c(1,19,26,33,45,68)

# create new dataframe with only the columns I want

mydata<-brfss2013[,list]

# dimensions of new data frame

dim(mydata)

# Use complete.cases to get rid of N/As

completes<-complete.cases(mydata)

mydata<-mydata[completes,]

# dimensions of data frame with N/A removed

dim(mydata)

# pull out states to classify as red or blue

states<-mydata %>%
  group_by(X_state) %>%
  summarize()

# write out states to append "Blue" or "Red" status
# write.csv(states, "matchstates.csv")

# Read in state data with state Type added:

statejoin<-read.csv("matchstates.csv")

# Convert X_state to character to facilitate join

mydata$X_state<-as.character(mydata$X_state)

statejoin$X_state<-as.character(statejoin$X_state)

# Use inner join to add "Type" column to mydata

mydata<-inner_join(mydata, statejoin)

# convert factors to numeric

mydata$genhlth<-as.numeric(mydata$genhlth)

mydata$checkup1<-as.numeric(mydata$checkup1)

# quick peek at table

head(mydata)

####################################################################

# Select data to work on question 1

sum1<- mydata %>%
  select(genhlth, veteran3, Type) %>%
  group_by(Type, veteran3) %>%
  summarise(Q1=quantile(genhlth, .25), Mean=mean(genhlth), Median=median(genhlth),
            Q3=quantile(genhlth, .75), IQR=IQR(genhlth), StDev=sd(genhlth)) %>%
  mutate(Skew=ifelse(Mean>Median, "Right", "Left"))

head(sum1)

# Non Veteran histogram data

p1dat<-mydata %>%
  select(genhlth, veteran3, Type) %>%
  filter(veteran3=="No")

# subset for t.test - blue states

p1adat<-p1dat %>%
  filter(Type=="Blue")

# subset for t.test - red states

p1bdat<-p1dat %>%
  filter(Type=="Red")

# Veteran histogram data

p2dat<-mydata %>%
  select(genhlth, veteran3, Type) %>%
  filter(veteran3=="Yes")

# subset for t.test - blue states

p2adat<-p2dat %>%
  filter(Type=="Blue")

# subset for t.test - red states

p2bdat<-p2dat %>%
  filter(Type=="Red")


# Histogram of non-Veterans


p1<-ggplot(p1dat, aes(genhlth, fill=Type))+geom_histogram(binwidth = .5)+
  facet_grid(.~Type)+scale_fill_manual(values=c("#00008b", "#ff0000"))+
  geom_vline(xintercept=mean(p1dat$genhlth), color="black")+
  ggtitle("General Health Value by State Type for non - Veterans")

p1

# Histogram of veterans

p2<-ggplot(p2dat, aes(genhlth, fill=Type))+geom_histogram(binwidth = .5)+
  facet_grid(.~Type)+scale_fill_manual(values=c("#00008b", "#ff0000"))+
  geom_vline(xintercept = mean(p2dat$genhlth), color="black")+
  ggtitle("General Health Value by State Type for Veterans")

p2


# t test for non-veterans

t.test(p1adat$genhlth, p1bdat$genhlth, var.equal = T)

# non-veterans in blue states report lower general health levels than in red states

# t test for veterans

t.test(p2adat$genhlth, p2bdat$genhlth, var.equal = T)

# veterans in blue states report lower general health levels than in red states

###################################################

sum2<- mydata %>%
  select(checkup1, veteran3, Type) %>%
  group_by(Type, veteran3) %>%
  summarise(Q1=quantile(checkup1, .25), Mean=mean(checkup1), Median=median(checkup1),
            Q3=quantile(checkup1, .75), IQR=IQR(checkup1), StDev=sd(checkup1)) %>%
  mutate(Skew=ifelse(Mean>Median, "Right", "Left"))

head(sum2)


# Question 2 data

p3dat<-mydata %>%
  select(checkup1, veteran3, Type) %>%
  filter(veteran3=="No")

# subset for t.test - blue states

p3adat<-p3dat %>%
  filter(Type=="Blue")

# subset for t.test - red states

p3bdat<-p3dat %>%
  filter(Type=="Red")



p4dat<-mydata %>%
  select(checkup1, veteran3, Type) %>%
  filter(veteran3=="Yes")

# subset for t.test - blue states

p4adat<-p4dat %>%
  filter(Type=="Blue")

# subset for t.test - red states

p4bdat<-p4dat %>%
  filter(Type=="Red")


# Histogram of non-Veterans

p3<-ggplot(p3dat, aes(checkup1, fill=Type))+geom_histogram(binwidth = .5)+
  facet_grid(.~Type)+scale_fill_manual(values=c("#00008b", "#ff0000"))+
  geom_vline(xintercept=mean(p3dat$checkup1), color="black")+
  ggtitle("Time Since last Checkup by State Type for non - Veterans")

p3

# Histogram of veterans

p4<-ggplot(p4dat, aes(checkup1, fill=Type))+geom_histogram(binwidth = .5)+
  facet_grid(.~Type)+scale_fill_manual(values=c("#00008b", "#ff0000"))+
  geom_vline(xintercept = mean(p4dat$checkup1), color="black")+
  ggtitle("Time Since last Checkup by State Type for Veterans")

p4

# t test for non-veterans

t.test(p3adat$checkup1, p3bdat$checkup1, var.equal = T)

# non-veterans in blue states report more frequent checkups than in red states

# t test for veterans

t.test(p4adat$checkup1, p4bdat$checkup1, var.equal = T)

# veterans in blue states report more frequent checkups than in red states

########################################################

# Question 3 data

sum3<- mydata %>%
  select(cvdinfr4, veteran3, smoke100) %>%
  group_by(veteran3, smoke100) %>%
  summarise(n=n()) %>%
  spread(veteran3, Sum) 

head(sum3, 16)

