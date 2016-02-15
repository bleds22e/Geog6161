# load library
library(dplyr)
library(ggplot2)
library(png)

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

# summary of transformations that seem to work
data <- select(data, obs:age) %>% 
  mutate(income_sq = income^2) %>% 
  mutate(income_cube = income^3) %>% 
  mutate(age_cube_root = age^(-1/3)) %>% 
  mutate(log10_mortpay = log10(mortpay))

# run regressions and plot pred. vs redis. on them

# income regression
plot(log10_mortpay ~ income_cube, data = data)
reg_income <- lm(log10_mortpay ~ income_cube, data = data)
abline(reg_income, col="red", lwd=3)

predicted_income<-predict.lm(reg_income)
residuals_income<-data$log10_mortpay-predicted_income
plot(predicted_income,residuals_income,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

ggplot(data, aes(y = residuals_income, x = predicted_income)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, colour = 'red', linetype = "dashed", size = 1) +
  ylab("Residuals") +
  xlab("Predicted Y") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14))
ggsave(file = "pred_res_income.png", width = 5, height = 5)


# sqfoot regression
plot(log10_mortpay ~ sqfoot, data = data)
reg_sqfoot <- lm(log10_mortpay ~ sqfoot, data = data)
abline(reg_sqfoot, col = "red", lwd = 3)

predicted_sqfoot <- predict.lm(reg_sqfoot)
residuals_sqfoot <- data$log10_mortpay - predicted_sqfoot
plot(predicted_sqfoot, residuals_sqfoot, cex=2, cex.lab=1.5, cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

# age regression
plot(log10_mortpay ~ age_cube_root, data = data)
reg_age <- lm(log10_mortpay ~ age_cube_root, data = data)
abline(reg_age, col = "red", lwd = 3)

predicted_age <- predict.lm(reg_age)
residuals_age <- data$log10_mortpay - predicted_age
plot(predicted_age, residuals_age, cex=2, cex.lab=1.5, cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")