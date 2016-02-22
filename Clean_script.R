# load library
library(dplyr)
library(ggplot2)

# read in file
data <- read.csv(file = "Lab_data.csv", header = TRUE)

# approach
# scatterplots of variables against Y
# check assumptions
# add interactions?
# run AIC and stepwise regressions to determine model of best fit

# prep the dataset
colnames(data) <- c("obs", "mortpay", "income", "sqfoot", "loans", "age")

# scatterplots of variables against Y
ggplot(data, aes(x = income, y = mortpay)) +
  geom_point(size = 2) +
  geom_smooth() +
  xlab("Household Disposable Income ($1000)") +
  ylab("Monthly Mortgage Payment ($)") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_line(colour = "light gray"))
ggsave(file = "mortpay_vs_income.png", width = 5, height = 5)

ggplot(data, aes(x = sqfoot, y = mortpay)) +
  geom_point(size = 2) +
  geom_smooth() +
  xlab("Square Footage") +
  ylab("Monthly Mortgage Payment ($)") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_line(colour = "light gray"))
ggsave(file = "mortpay_vs_sqfoot.png", width = 5, height = 5)

ggplot(data, aes(x = age, y = mortpay)) +
  geom_point(size = 2) +
  geom_smooth() +
  xlab("Age of Housing Unit (Years)") +
  ylab("Monthly Mortgage Payment ($)") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_line(colour = "light gray"))
ggsave(file = "mortpay_vs_age.png", width = 5, height = 5)

# histograms of independent variables
ggplot(data, aes(x = income)) +
  geom_histogram(binwidth = 5) +
  xlab("Household Disposable Income ($1000)") +
  ylab("Frequency") 

ggplot(data, aes(x = sqfoot)) +
  geom_histogram(binwidth = 100) +
  xlab("Square Footage") +
  ylab("Frequency") 

ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  xlab("Age of Housing Unit (Years)") +
  ylab("Frequency") 
