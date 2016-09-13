# Eric Zhang
# W4315
# HW 3


setwd('C:/Users/Jack/Desktop/Columbia Masters/Spring 2016 Courses/Linear Regression')

# call relevant libraries
library(ggplot2)
library(GGally)



# Problem 1 ------------------------------------------------------------------------------

# read table of data
raw_data = read.delim('Homework_4_data_Problem1.txt', header = FALSE, sep = '')

# separate x and y values
y = as.vector(raw_data[,1])
x = as.matrix(raw_data[,2:5])
x1 = x[,1]
x2 = x[,2]
x3 = x[,3]
x4 = x[,4]


# Part a
ggpairs(raw_data, columnLabels = c('y', 'x1', 'x2', 'x3', 'x4'))

# Part b
fit <- lm(y ~ x1 + x2 + x3 + x4)
summary(fit)

# Part c
resid = residuals(fit)

# Part d
y_hat = fitted(fit)

par(mfrow=c(3,4))
p1 = plot(x1, resid, main = 'Scatter of X1 vs. Residual', xlab = 'X1',
      ylab = 'Residual')
p2 = plot(x1*x2, resid, main = 'Scatter of X1 * X2 vs. Residual', xlab = 'X1 * X2',
      ylab = 'Residual')
p3 = plot(x1*x3, resid, main = 'Scatter of X1 * X3 vs. Residual', xlab = 'X1 * X3',
      ylab = 'Residual')
p4 = plot(x1*x4, resid, main = 'Scatter of X1 * X4 vs. Residual', xlab = 'X1 * X4',
      ylab = 'Residual')
p5 = plot(x2, resid, main = 'Scatter of X2 vs. Residual', xlab = 'X2',
      ylab = 'Residual')
p6 = plot(x2*x3, resid, main = 'Scatter of X2 * X3 vs. Residual', xlab = 'X2 * X3',
      ylab = 'Residual')
p7 = lot(x2*x4, resid, main = 'Scatter of X2 * X4 vs. Residual', xlab = 'X2 * X4',
      ylab = 'Residual')
p8 = plot(x3, resid, main = 'Scatter of X3 vs. Residual', xlab = 'X3',
      ylab = 'Residual')
p9 = plot(x3*x4, resid, main = 'Scatter of X3 * X4 vs. Residual', xlab = 'X3 * X4',
      ylab = 'Residual')
p10 = plot(x4, resid, main = 'Scatter of X4 vs. Residual', xlab = 'X4',
      ylab = 'Residual')
p11 = plot(y_hat, resid, main = 'Scatter of Y_hat vs. Residual', xlab = 'Y_hat',
      ylab = 'Residual')

x11()
qqnorm(resid, main = "Normal Probability Plot of Residuals", plot.it = T, datax = F)
qqline(resid)

# Part f
#x_new = c(1, 4.0, 10.0, 0.10, 80000)
#x_new = c(1, 6.0, 11.5, 0, 120000)
#x_new = c(1, 12.0, 12.5, 0.32, 340000)

coeff = coefficients(fit)
alpha = 0.05
MSE = sum(resid^2)/76
x_pad = cbind(mat.or.vec(81, 1) + 1, x)

y_hat_new = as.numeric(t(x_new) %*% coeff)
tau = as.numeric(sqrt(t(x_new) %*% solve(t(x_pad) %*% x_pad) %*% x_new + 1))
left_CI = y_hat_new - qt(1-alpha/2, 76) * sqrt(MSE) * tau
right_CI = y_hat_new + qt(1-alpha/2, 76) * sqrt(MSE) * tau

print(left_CI)
print(right_CI)
print((qt(1-alpha/2, 76) * sqrt(MSE) * tau)/y_hat_new)

# Part g
fit_marg1 <- lm(y ~ x4)
anova(fit_marg1)
SSR1 = sum((fitted(fit_marg1) - mean(y))^2)

fit_marg2 <- lm(y ~ x1 + x4)
anova(fit_marg2)
SSR2 = sum((fitted(fit_marg2) - mean(y))^2)

fit_marg3 <- lm(y ~ x1 + x2 + x4)
anova(fit_marg3)
SSR3 = sum((fitted(fit_marg3) - mean(y))^2)

fit_marg4 <- lm(y ~ x1 + x2 + x3 + x4)
anova(fit_marg4)
SSR4 = sum((fitted(fit_marg4) - mean(y))^2)


print(SSR1)

MARG_SSR1 = SSR2 - SSR1
print(MARG_SSR1)

MARG_SSR2 = SSR3 - SSR2
print(MARG_SSR2)

MARG_SSR3 = SSR4 - SSR3
print(MARG_SSR3)

print(SSR4) # Total SSR


# Part h

anova(fit_marg3)
summary(fit_marg3)
anova(fit_marg4)
summary(fit_marg4)

(sum(residuals(fit_marg3)^2)-sum(residuals(fit_marg4)^2))/(77-76)/(sum(residuals(fit_marg4)^2)/76)


# Problem 2 ------------------------------------------------------------------------------

# read table of data
raw_data2 = read.delim('Homework_4_data_Problem2.txt', header = FALSE, sep = '')

# separate x and y values
y_2 = raw_data2[,1]
x_2 = raw_data2[,2:3]
x_2_1 = x_2[,1]
x_2_2 = x_2[,2]
x_3 = x_2_1 * x_2_2

# Part b
fit2 <- lm(y_2 ~ x_2_1 + x_2_2)
summary(fit2)

# Part c
confint(fit2, level = 0.95)

# Part e
resid_2 = residuals(fit2)
qplot(x_3, resid_2, main = 'Scatter of X1 * X2 vs. Residual', 
      xlab = 'X1 * X2', ylab = 'Residual')

# Part f
fit3 <- lm(y_2 ~ x_2_1 + x_2_2 + x_3)
summary(fit3)




