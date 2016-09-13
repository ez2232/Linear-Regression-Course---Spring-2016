# Eric Zhang
# W4315
# HW 1
# Problem 3


setwd('C:/Users/Jack/Desktop/Columbia Masters/Spring 2016 Courses/Linear Regression')

# read table of data
raw_data = read.delim('Data_HW1_Problem3.txt', header = FALSE, sep = '')

# separate x and y values
y = raw_data[,1]
x = raw_data[,2]

# Part a
# compute b_0 and b_1
b_1 = as.numeric(((x - mean(x)) %*% (y - mean(y))) / sum((x - mean(x))^2))
b_0 = as.numerica(mean(y) - b_1 * mean(x))


# Part b
library(ggplot)

qplot(x,y,main = 'Scatter of Data Points vs. Regression Line', xlab = 'x: ACT test score',
      ylab = 'y: Freshman GPA') +
geom_abline(intercept = b_0, slope = b_1, color = 'red')

# Part c
30 * b_1 + b_0

# Part e
# Compute residuals
resid = y - (b_0 + b_1 * x)

# Sum of residuals
sum(resid)

# Part f
# Estimate sigma^2 (as SSE/(N-2)), sigma
sigma_sq_hat = sum(resid^2)/(length(x)-2)
sigma_hat = sqrt(sigma_sq_hat)


