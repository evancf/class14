# EEB 338/538 Analysis and Visualization of Biological Data
# Class 14: Model selection practice

# Recall the example code for a linear model with interaction

y <- c(89, 88, 66, 59, 93, 73, 82, 77, 100, 67, 57, 68, 69, 59, 62, 59, 56, 66, 72)
x <- c(28, 27, 24, 23, 29, 25, 29, 25, 30, 23, 29, 32, 35, 31, 29, 26, 28, 33, 33)
m <- c(rep(0, 10), rep(1, 9))
dat <- data.frame(y = y, x = x, male = m)

plot(y ~ x, cex = 2, pch = ifelse(dat$male == 1, 16, 16),
     col = ifelse(dat$male == 1, "blue", "orange")) 

# Model that ignores sex:
m <- lm(y ~ x, data = dat)

plot(y ~ x, cex = 2, pch = ifelse(dat$male == 1, 16, 16),
     col = ifelse(dat$male == 1, "blue", "orange")) 
abline(m,col = "black", lwd = 2) 

summary(m)

# Model that includes sex as main effect:
m2 <- lm(y ~ x + male, data = dat)
plot(y ~ x, cex = 2, pch = ifelse(dat$male == 1, 16, 16),
     col = ifelse(dat$male == 1, "blue", "orange")) 
abline(a=coefficients(m2)[1], b=coefficients(m2)[2], col="orange", lwd = 2) # female line 
abline(a=coefficients(m2)[1] + coefficients(m2)[3], b=coefficients(m2)[2] ,col="blue", lwd = 2)

summary(m2)


# Model that includes interaction 
m3 <- lm(y ~ x*male, data = dat)
plot(y ~ x, cex = 2, pch = ifelse(dat$male == 1, 16, 16),
     col = ifelse(dat$male == 1, "blue", "orange")) 
abline(a = coefficients(m3)[1], b = coefficients(m3)[2], col= "orange", lwd = 2) # female line 
abline(a = coefficients(m3)[1] + coefficients(m3)[3], b = coefficients(m3)[2] + 
         coefficients(m3)[4], col = "blue", lwd = 2) # male line 
summary(m3)



# We can use AIC model selection to formally compare these three models using the ICtab function from the package bbmle
library(bbmle)

# the output table provides the change in AIC from the best model and the model weight. the "best" model has dAIC = 0
ICtab(m, m2, m3, type="AICc", weights=TRUE)

# We can create an averaged model based on the model weights using the model.avg() function from the package MuMIn
library(MuMIn)
model.avg(object=list(m, m2, m3))

# Examine the estimates of the "best" model compared to the averaged model. 
summary(m3)

# Why are the estimates so similar? Because the best model had a  weight of 0.9928 in the averaged model




# Next, a little simulation to see how model selection relates to more typical
# null hypothesis testing using p values

set.seed(99)

n <- 10000
counter <- 0

for(i in 1:n){
  
  yy <- rnorm(1000, 3, 2)
  
  xx <- runif(length(yy), 0, 10)
  
  mod1 <- lm(yy ~ 1)
  mod2 <- lm(yy ~ xx)
  
  if(AIC(mod1) - AIC(mod2) > 2){
    print(paste(i, "mod2 performs better by 2 AIC units"))
    counter <- counter + 1
  }
  
}

# What percent of times does mod2 perform better by 2 AIC units?
counter/n

# This is pretty close to the alpha = 0.05 cutoff that we typically use
# for deciding to reject a null hypothesis. The 0.05 means that we should expect
# a result this different from the null expectation (or more different)
# 5% of the time if the null hypothesis were true.


plot(yy ~ xx)

AIC(mod1)
AIC(mod2)

summary(mod2)

ICtab(mod1, mod2)