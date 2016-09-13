# Eric Zhang
# W4315
# HW 3


setwd('C:/Users/Jack/Desktop/Columbia Masters/Spring 2016 Courses/Linear Regression')

# read table of data
raw_data = read.delim('Homework_2_data.txt', header = FALSE, sep = '')

# separate x and y values
y = raw_data[,1]
x = raw_data[,2]

# N
N = length(x)


# Problem 1 ------------------------------------------------------------------------------

# Part a
# compute b_0 and b_1
b_1 = as.numeric(((x - mean(x)) %*% (y - mean(y))) / sum((x - mean(x))^2))
b_0 = as.numeric(mean(y) - b_1 * mean(x))


# Part b
library(ggplot2)

qplot(x,y,main = 'Scatter of Data Points vs. Regression Line', xlab = 'x: Number of copiers serviced',
      ylab = 'y: Number of minutes spent by service person') +
geom_abline(intercept = b_0, slope = b_1, color = 'red')

# Compute residuals
resid = y - (b_0 + b_1 * x)

# Plotting scatter of residuals
qplot(x, resid, main = 'Scatter of x vs. residual values')

# Breusch-Pagan test for homoscedasticity
library(lmtest)
bptest(y~x)


# Problem 2 ------------------------------------------------------------------------------

# Part a
b_1 - qt(1-0.1/2,N-2,0) #lower limit of CI
b_1 + qt(1-0.1/2,N-2,0) #upper limit of CI

# Part b
sqrt((sum(resid^2)/(N-2))/sum((x-mean(x))^2)) #s^2{b1}

qt(1-0.1/2,N-2,0)

# Part d
qt(1-0.05,N-2,0)

