# Eric Zhang
# W4315
# HW 3


setwd('C:/Users/Jack/Desktop/Columbia Masters/Spring 2016 Courses/Linear Regression')

# call relevant libraries
library(ggplot2)
library(GGally)
library(leaps)

# read table of data
raw_data = read.delim('Homework 5 Problem 1 and 2.txt', header = FALSE, sep = '')

# separate x and y values
y = as.vector(raw_data[,1])
x = as.matrix(raw_data[,2:5])
x1 = x[,1]
x2 = x[,2]
x3 = x[,3]
x4 = x[,4]

# Problem 1 ------------------------------------------------------------------------------

# Part a
ggpairs(raw_data, columnLabels = c('y', 'x1', 'x2', 'x3', 'x4'))


# Part b
fit <- lm(y ~ x1 + x2 + x3 + x4)
summary(fit)


# Part c
model_sel <- leaps(x=x, y=y, int = TRUE, method = "adjr2", names = c('x1', 'x2', 'x3', 'x4'))
model_sel
inclusion_mat <- as.matrix(model_sel[[1]])
AdjR2_unsorted <- as.vector(model_sel[[4]])
sort_result <- sort(AdjR2_unsorted, decreasing = TRUE, index.return = TRUE)
indices <- sort_result[[2]][1:4]

inclusion_mat[indices,]

summary(lm(y ~ x1 + x3))
summary(lm(y ~ x1 + x3 + x4))
summary(lm(y ~ x1 + x2 + x3))
summary(lm(y ~ x1 + x2 + x3 + x4))


# Part d

# iteration 1
# pvalues: 0.00852 x1, 0.0115 x2, 1.26e-09 x3, 1.7e-08 x4
# therefore, add x3
fit <- lm(y ~ x3)
summary(fit)

# iteration 2
# pvalues: 1.58e-06 x1, 0.699 x2, 0.001570 x4
# therefore add x1 --> for this model, x3 pvalue = 6.31e-13, so keep x3
fit <- lm(y ~ x3 + x1)
summary(fit)

# iteration 3
# pvalues: 0.561 x2, 0.000735 x4
# therefore add x4 --> for this model x3 pvalue = 1.33e-08, x1 pvalue = 1.04e-06
# so keep x1 and x3
fit <- lm(y ~ x3 + x1 + x4)
summary(fit)

# iteration 4
# pvalues: 0.40383 x2
# therefore do not add x2 and stop
fit <- lm(y ~ x3 + x1 + x4 + x2)
summary(fit)


# Problem 2 ------------------------------------------------------------------------------

# Part a
fit <- lm(y ~ x1 + x3)

resid <- residuals(fit)
y_hat <- fitted(fit)
x1x3 <- x1 * x3

qplot(y_hat, resid, main = 'Plot of Residuals Against Y_hat')
qplot(resid, x1x3, main = 'Plot of Residuals Against X1 * X3')
qplot(resid, x1, main = 'Plot of Residuals Against X1')
qplot(resid, x2, main = 'Plot of Residuals Against X2')
qplot(resid, x3, main = 'Plot of Residuals Against X3')
qplot(resid, x4, main = 'Plot of Residuals Against X4')


# Part b
fit <- lm(y ~ x3)
resid_y_x3 <- residuals(fit)
fit <- lm(x1 ~ x3)
resid_x1_x3 <- residuals(fit)

fit <- lm(y ~ x1)
resid_y_x1 <- residuals(fit)
fit <- lm(x3 ~ x1)
resid_x3_x1 <- residuals(fit)

qplot(resid_x1_x3, resid_y_x3, main = 'Plot of e(X1|X3) vs. e(Y|X3)')
qplot(resid_x3_x1, resid_y_x1, main = 'Plot of e(X3|X1) vs. e(Y|X1)')


# Part c
fit <- lm(y ~ x1 + x3)
resid <- residuals(fit)

qqnorm(resid, main = "Normal Probability Plot of Residuals", plot.it = T, datax = F)
qqline(resid)

resid_sort <- sort(resid)
anova(fit)

n <- length(resid)

r <- sqrt(27.6)*qnorm((c(1:n)-0.375)/(n + 0.25))

cor(r, resid_sort)

#qplot(r, resid_sort)


# Part d
library(MASS)

X <- cbind(rep(1, n), x1, x3)
H <- (X %*% ginv(t(X) %*% X)) %*% t(X)
hii <- diag(H)

p <- 3
ti <- resid * ((n - p - 1)/(606.7*(1-hii)-resid^2))^(1/2)

crit_value <- qt(1 - 0.05/(2 * n), n - p - 1)

ind <- (abs(ti) > crit_value)
sum(ind)


# Part e
ind <- (hii > 2 * p/n)
cbind(seq(1, n, 1), ind)


# Part f
# DFFITS
DFFITS <- ti * (hii/(1 - hii))^(1/2)
DFFITS[c(7, 18, 16)]

# DFBETAS

coeffs_full <- coefficients(fit)
dummy <- ginv(t(X) %*% X)
dummy1 <- diag(dummy)


# obs 7
X_m7 <- X[-7,]
fit <- lm(y[-7] ~ X_m7[,2] + X_m7[,3])
coeffs_7 <- coefficients(fit)
MSE_7 <- anova(fit)[3,3]
DFBETAS_7 <- (coeffs_full - coeffs_7)/sqrt(MSE_7 * dummy1)

# obs 18
X_m18 <- X[-18,]
fit <- lm(y[-18] ~ X_m18[,2] + X_m18[,3])
coeffs_18 <- coefficients(fit)
MSE_18 <- anova(fit)[3,3]
DFBETAS_18 <- (coeffs_full - coeffs_18)/sqrt(MSE_18 * dummy1)

# obs 16
X_m16 <- X[-16,]
fit <- lm(y[-16] ~ X_m16[,2] + X_m16[,3])
coeffs_16 <- coefficients(fit)
MSE_16 <- anova(fit)[3,3]
DFBETAS_16 <- (coeffs_full - coeffs_16)/sqrt(MSE_16 * dummy1)


# Cook's distance
Di <- (resid^2) * (hii/(1 - hii)^2)/(p * 27.6)
Di[c(7, 18, 16)]


# Part g
model1 <- lm(x1 ~ x2 + x3 + x4)
dummy <- summary(model1)
VIF1 <- (1 - (as.numeric(dummy[8]))^2)^-1

model2 <- lm(x2 ~ x1 + x3 + x4)
dummy <- summary(model2)
VIF2 <- (1 - (as.numeric(dummy[8]))^2)^-1

model3 <- lm(x3 ~ x2 + x1 + x4)
dummy <- summary(model3)
VIF3 <- (1 - (as.numeric(dummy[8]))^2)^-1

model4 <- lm(x4 ~ x2 + x3 + x1)
dummy <- summary(model4)
VIF5 <- (1 - (as.numeric(dummy[8]))^2)^-1






