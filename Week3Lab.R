# Week 3 Lab

# Load libraries

library(statsr)
library(dplyr)
library(ggplot2)

# Load data

data(kobe_basket)

# look at data

View(kobe_basket)

# calculate streaks

kobe_streak <- calc_streak(kobe_basket$shot)

p1<-ggplot(kobe_streak, aes(x=length))+
  geom_histogram(binwidth = 1)

p1

# Simulate streak using Kobe's career shooting percentage - 45%

shot_outcomes <- c("H", "M")
sim_basket <- as.data.frame(sample(shot_outcomes, size = 133, replace = TRUE,
                     prob = c(0.45, 0.55)))

names(sim_basket)<-c("length")

View(sim_basket)

sim_streak<-calc_streak(sim_basket$length)

p2<-ggplot(sim_streak, aes(x=length))+
  geom_histogram(binwidth = 1)

p2