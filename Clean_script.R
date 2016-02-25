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

# trying ln and log10
data$ln_mortpay <- log(data$mortpay)
data$log_mortpay <- log10(data$mortpay)

hist(data$boxcox_Y)
plot(ln_mortpay ~ sqfoot, data = data)
plot(log_mortpay ~ sqfoot, data = data)
plot(ln_mortpay ~ income, data = data)
plot(log_mortpay ~ income, data = data)
plot(ln_mortpay ~ age, data = data)
plot(boxcox_Y ~ income, data = data)



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

# start making models!
# create variables to include in regression analysis

data$ln_Y <- log(data$Y)
data$X1sq <- data$X1^2
data$X2sq <- data$X2^2
data$Asq <- data$A^2
data$Acube <- data$A^3

extractAIC(mort_mod)
y <- data$ln_Y
x3 <- data[, -1]
x2 <- x3[, -1]
x <- x2[,-5]

# create full regression model
 
mort_mod <- lm(ln_Y ~ X1 + X2 + X3 + A + 
               X1sq + X2sq + Asq + Acube + 
               X1:X3 + X2:X3 + A:X3 + Asq:X3 + Acube:X3 +
               A:X1 + A:X2 + X1:X2 + X1sq:X3 + X2sq:X3, data = data)
summary(mort_mod)
null_mod <- lm(ln_Y ~ 1, data = data)

### use step-wise regression to find best-fit model

forward <- step(null_mod, scope = list(lower = null_mod, upper = mort_mod), direction = "forward")
backward <- step(mort_mod, scope = list(lower = null_mod, upper = mort_mod), direction = "backward")
both1 <- step(mort_mod, scope = list(upper = mort_mod, lower = null_mod), direction = "both")
both1 <- step(null_mod, scope = list(upper = mort_mod, lower = null_mod), direction = "both")

extractAIC(mort_mod)
y <- data$ln_Y
x3 <- data[, -1]
x2 <- x3[, -1]
x <- x2[,-5]

# new model
new_mod <- lm(ln_Y ~ X1 + X3 + A + 
                X1sq + X2sq + Acube + 
                X1:X3 + X2:X3 + A:X3, data = data)
summary(new_mod)

forward_new <- step(null_mod, scope = list(lower = null_mod, upper = new_mod), direction = "forward")
backward_new <- step(new_mod, scope = list(lower = null_mod, upper = new_mod), direction = "backward")
both_new <- step(null_mod, scope = list(upper = new_mod, lower = null_mod), direction = "both")
both_new1 <- step(new_mod, scope = list(upper = new_mod, lower = null_mod), direction = "both")

# suggested model
mod1 <- lm(ln_Y ~ X1 + X2+ X1sq + X2sq + X3 + Acube, data = data)
summary(mod1)

mod2 <- lm(ln_Y ~ X1 + X2 + X2sq + X3 + Acube, data = data)
summary(mod2)


#load librairies
library(car)
library(MVA)
library(MASS)
library(meifly)

##Call in Data

extractAIC(new_mod)

mods <- fitall(y, x, "lm")


## Extract AIC from each model
fitall.out.aic <- t(sapply(mods, extractAIC))
fitall.out.aic 

## Create an order list of increasing AIC
final.out.sort <- sort(fitall.out.aic[,2])
final.out.sort

deltaAIC<-final.out.sort-final.out.sort[1]
deltaAIC

## Show the result for the best model
final.out.order <-order(fitall.out.aic[,2])
models<-mods[final.out.order][1:5]
models

### checking for multicollinearity

model_var <- select(data, X1, X2, X2sq, X3, Acube)

#plotting all pairs
pairs(model_var)

#calculate R2 for all pairs:
round(cor(model_var)^2,2)

# very strong collinearity between X2 and X2sq (not surprising here)
  # remove X2
no_X2_mod <- lm(ln_Y ~ X1 + X2sq + X3 + Acube, data = data)
summary(no_X2_mod)
next_mod <- lm(ln_Y ~ X1sq + X2sq + X3 + Acube, data = data)
summary(next_mod)


predicted<-predict.lm(mod2)
residuals<-data$ln_Y-predicted

##  plot residuals vs. the predicted values:
plot(predicted,residuals,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")


###Assumption 2 Homogeneity of Variances
#We look at the same plot of residuals vs. the predicted values

#Assumption 3 Normality
#calculate standardized residuals
stdRes = rstandard(reg)

#QQPLOT
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2)

predicted<-predict.lm(mod2)
residuals<-data$ln_Y-predicted
plot(predicted,residuals,cex=2,cex.lab=1.5,cex.axis=1.15, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")
dev.copy(png, "pred_vs_resid_mod2.png")
dev.off()
