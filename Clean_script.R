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

### run regressions and plot pred. vs redis. & qqplots for NON-transformed variables
## income regression
plot(mortpay ~ income, data = data)
reg_income <- lm(mortpay ~ income, data = data)
abline(reg_income, col="red", lwd=3)

predicted_income<-predict.lm(reg_income)
residuals_income<-data$mortpay - predicted_income
plot(predicted_income,residuals_income,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

ggplot(data, aes(y = residuals_income, x = predicted_income)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, colour = 'red', linetype = "dashed", size = 1) +
  ggtitle("Residuals vs. Predicted Values") +
  ylab("Residuals") +
  xlab("Predicted Y") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
ggsave(file = "pred_res_income.png", width = 5, height = 5)

# qqplot in ggplot2
ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample = .resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="red", size = 1) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("Normal Q-Q Plot") +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
  ggsave(filename = "income_qq.png", width = 5, height = 5)
  return(p)
}

ggQQ(reg_income)

## sqfoot regression
plot(mortpay ~ sqfoot, data = data)
reg_sqfoot <- lm(mortpay ~ sqfoot, data = data)
abline(reg_sqfoot, col="red", lwd=3)

predicted_sqfoot <- predict.lm(reg_sqfoot)
residuals_sqfoot <- data$mortpay-predicted_sqfoot
plot(predicted_sqfoot,residuals_sqfoot,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

ggplot(data, aes(y = residuals_sqfoot, x = predicted_sqfoot)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, colour = 'red', linetype = "dashed", size = 1) +
  ggtitle("Residuals vs. Predicted Values") +
  ylab("Residuals") +
  xlab("Predicted Y") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
ggsave(file = "pred_res_sqfoot.png", width = 5, height = 5)

# qqplot in ggplot2
ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample = .resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="red", size = 1) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("Normal Q-Q Plot") +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
  ggsave(filename = "sqfoot_qq.png", width = 5, height = 5)
  return(p)
}

ggQQ(reg_sqfoot)

# age regression
plot(mortpay ~ age, data = data)
reg_age <- lm(mortpay ~ age, data = data)
abline(reg_age, col="red", lwd=3)

predicted_age <- predict.lm(reg_age)
residuals_age <- data$mortpay-predicted_age
plot(predicted_age,residuals_age,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

ggplot(data, aes(y = residuals_age, x = predicted_age)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, colour = 'red', linetype = "dashed", size = 1) +
  ggtitle("Residuals vs. Predicted Values") +
  ylab("Residuals") +
  xlab("Predicted Y") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
ggsave(file = "pred_res_age.png", width = 5, height = 5)

# qqplot in ggplot2
ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample = .resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="red", size = 1) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("Normal Q-Q Plot") +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
  ggsave(filename = "age_qq.png", width = 5, height = 5)
  return(p)
}

ggQQ(reg_age)

### run regressions and plot pred. vs redis. & qqplots for transformed variables

## income regression
plot(log10_mortpay ~ income_cube, data = data)
reg_income_trans <- lm(log10_mortpay ~ income_cube, data = data)
abline(reg_income, col="red", lwd=3)

predicted_income_trans <- predict.lm(reg_income_trans)
residuals_income_trans <- data$log10_mortpay-predicted_income_trans
plot(predicted_income_trans,residuals_income_trans,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

ggplot(data, aes(y = residuals_income_trans, x = predicted_income_trans)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, colour = 'red', linetype = "dashed", size = 1) +
  ggtitle("Residuals vs. Predicted Values") +
  ylab("Residuals") +
  xlab("Predicted Y") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
ggsave(file = "pred_res_income_transformed.png", width = 5, height = 5)

# qqplot in ggplot2
ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample = .resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="red", size = 1) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("Normal Q-Q Plot") +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
  ggsave(filename = "income_qq_transformed.png", width = 5, height = 5)
  return(p)
}

ggQQ(reg_income_trans)

## sqfoot regression
plot(log10_mortpay ~ sqfoot, data = data)
reg_sqfoot_trans <- lm(log10_mortpay ~ sqfoot, data = data)
abline(reg_sqfoot_trans, col="red", lwd=3)

predicted_sqfoot_trans <- predict.lm(reg_sqfoot_trans)
residuals_sqfoot_trans <- data$log10_mortpay-predicted_sqfoot_trans
plot(predicted_sqfoot_trans,residuals_sqfoot_trans,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

ggplot(data, aes(y = residuals_sqfoot_trans, x = predicted_sqfoot_trans)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, colour = 'red', linetype = "dashed", size = 1) +
  ggtitle("Residuals vs. Predicted Values") +
  ylab("Residuals") +
  xlab("Predicted Y") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
ggsave(file = "pred_res_sqfoot_transformed.png", width = 5, height = 5)

# qqplot in ggplot2
ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample = .resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="red", size = 1) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("Normal Q-Q Plot") +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
  ggsave(filename = "sqfoot_qq_transformed.png", width = 5, height = 5)
  return(p)
}

ggQQ(reg_sqfoot_trans)

# age regression
plot(log10_mortpay ~ age_cube_root, data = data)
reg_age_trans <- lm(log10_mortpay ~ age_cube_root, data = data)
abline(reg_age_trans, col="red", lwd=3)

predicted_age_trans <- predict.lm(reg_age_trans)
residuals_age_trans <- data$log10_mortpay - predicted_age_trans
plot(predicted_age_trans,residuals_age_trans,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

ggplot(data, aes(y = residuals_age_trans, x = predicted_age_trans)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, colour = 'red', linetype = "dashed", size = 1) +
  ggtitle("Residuals vs. Predicted Values") +
  ylab("Residuals") +
  xlab("Predicted Y") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
ggsave(file = "pred_res_age_transformed.png", width = 5, height = 5)

# qqplot in ggplot2
ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample = .resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="red", size = 1) +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("Normal Q-Q Plot") +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size=14), plot.title = element_text(size = 18, face = "bold"))
  ggsave(filename = "age_qq_transformed.png", width = 5, height = 5)
  return(p)
}

ggQQ(reg_age_trans)