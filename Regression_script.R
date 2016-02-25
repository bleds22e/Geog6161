# Ellen Bledsoe
# GEO6161 Lab 1 script
# Initiated: Feb. 12, 2016

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

# log transform variables
data <- select(data, obs:age) %>% 
        mutate(log_income = log(income)) %>% 
        mutate(log_sqfoot = log(sqfoot)) %>% 
        mutate(log_mortpay = log(mortpay))

# sqrt transform
data <- select(data, obs:log_mortpay) %>% 
        mutate(sq_income = sqrt(income)) %>% 
        mutate(sq_sqfoot = sqrt(sqfoot)) %>% 
        mutate(recip_income = (1/income)) %>% 
        mutate(recip_sqfoot = (1/sqfoot)) %>% 
        mutate(cube_root_age = (age^(1/3)))
head(data)
data <- select(data, obs:cube_root_age) %>% 
        mutate(income2 = income^2) %>% 
        mutate(sqfoot2 = sqfoot^2) %>% 
        mutate(age3 = age^3) %>% 
        mutate(ageneg3 = age^-3) %>% 
        mutate(log10_income = log10(income)) %>% 
        mutate(log10_sqfoot = log10(sqfoot)) %>% 
        mutate(log10_mortpay = log10(mortpay)) %>% 
        mutate(recip_root_income = income^(-.5)) %>% 
        mutate(recip_root_sqfoot = sqfoot^(-.5)) %>% 
        mutate(recip_sq_income = income^(-2)) %>% 
        mutate(recip_sq_sqfoot = sqfoot^(-2)) %>% 
        mutate(income3 = income^3) %>% 
        mutate(recip_income = income^(-1)) %>% 
        mutate(recip_trio_income = income^(-1/3)) %>% 
        mutate(recip_quart_income = income^(-1/4)) %>%
        mutate(income2half = income^(2.5)) %>% 
        mutate(recip_age3 = 1/age3)
      
  
plot(data = data, log10_mortpay ~ recip_age3)
hist(data$income)

ggplot(data, aes(x = age, y = mortpay)) +
  geom_point(size = 2) +
  geom_smooth() +
  xlab("Household Disposable Income ($1000)") +
  ylab("Monthly Mortgage Payment ($)") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_line(colour = "light gray"))

# look at predicted vs residuals
plot(data$mortpay ~ data$income, xlab = "Household Disposable Income ($1000)", ylab = "Monthly Mortgage Payment ($)")
reg <- lm(data$mortpay ~ data$income)
abline(reg, col="red", lwd=3) 
predicted<-predict.lm(reg)
residuals<-data$mortpay-predicted
plot(predicted,residuals,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")
pred_res <- cbind(predicted, residuals)
as.data.frame(pred_res)
dev.copy(png, "residuals_vs_predicted_income.png")
dev.off()
stdRes = rstandard(reg)
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2)
dev.copy(png, "qqplot_income.png")
dev.off
hist(stdRes)

reg2 <- lm(data$mortpay ~ data$sqfoot)
abline(reg2, col="red", lwd=3) 
predicted2<-predict.lm(reg2)
residuals2<-data$mortpay-predicted2
plot(predicted2,residuals2,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")
dev.copy(png, "residuals_vs_predicted_sqfoot.png")
dev.off()
stdRes2 = rstandard(reg2)
qqnorm(stdRes2,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes2, col=2,lwd=2)
dev.copy(png, "qqplot_sqfoot.png")
dev.off
hist(stdRes)

reg3 <- lm(data$mortpay ~ data$age)
abline(reg3, col="red", lwd=3) 
predicted3<-predict.lm(reg3)
residuals3<-data$mortpay-predicted3
plot(predicted3,residuals3,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")
dev.copy(png, "residuals_vs_predicted_sqfoot.png")
dev.off()
stdRes2 = rstandard(reg2)
qqnorm(stdRes2,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes2, col=2,lwd=2)
dev.copy(png, "qqplot_sqfoot.png")
dev.off
hist(stdRes)

ggplot(pred_res, aes(x = predicted, y = residuals)) +
 gg_hline(yintercept = 0) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
        

# how to make a QQ plot in ggplot2
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
  ggsave(filename = "qq_mod2.png", width = 5, height = 5)
  return(p)
}

ggQQ(mod2)

