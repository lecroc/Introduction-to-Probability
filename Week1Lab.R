# Week 1 lab

# load packages

library(dplyr)
library(ggplot2)
library(statsr)

# load data

data(arbuthnot)

# look at data

head(arbuthnot)

dim(arbuthnot)

names(arbuthnot)

# how many variables in data set?

ncol(arbuthnot)


# what years are included in the data set?

arbuthnot$year

# what command to get number of girls born?

arbuthnot$girls

# plot girls born over time

plot1<-ggplot(data=arbuthnot, aes(x=year, y=girls))+
  geom_line()

plot1

# load next data set

data("present")

dim(present)

head(present)

names(present)

# Plot proportion of boys by year

present$total<-present$boys+present$girls

present$prop_boys<-present$boys/present$total

plot2<-ggplot(data=present, aes(x=year, y=prop_boys))+
  geom_line()

plot2

# create moreboys variable

present$moreboys<-ifelse(present$boys>present$girls, "T", "F")

View(present)

# always more boys

# calculate boy:girl ratio

present$boy_girl_prop<-present$boys/present$girls

plot3<-ggplot(data=present, aes(x=year, y=boy_girl_prop))+
  geom_line()

plot3

# What year had the most total births

maxtot<-max(present$total)

answer<-present %>%
  filter(total==maxtot)

answer

