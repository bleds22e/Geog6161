# load library
library(dplyr)
library(ggplot2)
library(forecast)

# read in file
data <- read.csv(file = "Lab_data.csv", header = TRUE)

# approach
# scatterplots of variables against Y
# check assumptions
# add interactions?
# run AIC and stepwise regressions to determine model of best fit

# prep the dataset
colnames(data) <- c("obs", "mortpay", "income", "sqfoot", "mort_type", "age")
as.factor(data$mort_type)

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

ggplot(data, aes(factor(mort_type), mortpay)) +
  geom_boxplot() +
  ylab("Monthly Mortgage Payment ($)") +
  xlab("Mortgage Type") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank())
ggsave(file = "mortpay_vs_type.png", width = 5, height = 5)

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

# optimizing Box-Cox
lambda <- BoxCox.lambda(x = data$mortpay)
data$boxcox_Y <- BoxCox(data$mortpay, lambda)

hist(data$boxcox_Y)
plot(ln_mortpay ~ sqfoot, data = data)
plot(log_mortpay ~ sqfoot, data = data)
plot(ln_mortpay ~ income, data = data)
plot(log_mortpay ~ income, data = data)
plot(ln_mortpay ~ age, data = data)
plot(boxcox_Y ~ income, data = data)

# trying ln and log10
data$ln_mortpay <- log(data$mortpay)
data$log_mortpay <- log10(data$mortpay)

# scatterplots with ln transformed Y
ggplot(data, aes(x = income, y = ln_mortpay)) +
  geom_point(size = 2) +
  geom_smooth() +
  xlab("Household Disposable Income ($1000)") +
  ylab("ln(Monthly Mortgage Payment ($))") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_line(colour = "light gray"))
ggsave(file = "ln_mortpay_vs_income.png", width = 5, height = 5)

ggplot(data, aes(x = sqfoot, y = ln_mortpay)) +
  geom_point(size = 2) +
  geom_smooth() +
  xlab("Square Footage") +
  ylab("ln(Monthly Mortgage Payment ($))") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_line(colour = "light gray"))
ggsave(file = "ln_mortpay_vs_sqfoot.png", width = 5, height = 5)

ggplot(data, aes(x = age, y = ln_mortpay)) +
  geom_point(size = 2) +
  geom_smooth() +
  xlab("Age of Housing Unit (Years)") +
  ylab("ln(Monthly Mortgage Payment ($))") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_line(colour = "light gray"))
ggsave(file = "ln_mortpay_vs_age.png", width = 5, height = 5)

ggplot(data, aes(factor(mort_type), ln_mortpay)) +
  geom_boxplot() +
  ylab("ln(Monthly Mortgage Payment ($))") +
  xlab("Mortgage Type") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank())
ggsave(file = "ln_mortpay_vs_type.png", width = 5, height = 5)

# create variables to include in regression analysis

data$ln_Y <- log(data$Y)
data$X1sq <- data$X1^2
data$X2sq <- data$X2^2
data$Asq <- data$A^2
data$Acube <- data$A^3

# create full regression model
 
mort_mod <- lm(ln_Y ~ X1 + X2 + X3 + A + X1sq + X2sq + Asq + Acube + X1:X3 + X2:X3 + )