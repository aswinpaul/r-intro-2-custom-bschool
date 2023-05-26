#/////////////////////
# 1 Load packages ----

library(MASS)       # ginv -- coefficient estimation
library(splines)    # ns, bs -- spline curves
library(multcomp)   # glht -- linear hypotheses
library(emmeans)    # estimated marginal means
library(limma)      # lmFit, etc -- fitting many models
library(tidyverse)  # working with data frames, plotting

# Much of what we will be using is built into R without loading any
# packages.



#////////////////////////////
# 2 Vectors and matrices ----

# 2.1 Vector operations ----

a <- c(3,4)
b <- c(5,6)

length(a)

# R performs operations elementwise:

a + b
a * b

# We will be using the dot product a lot. This is:

sum(a*b)
t(a) %*% b

# The *geometric* length of a vector is (by Pythagoras, aka Euclidean
# distance):

sqrt(sum(a*a))

# 2.2 Matrix operations ----
#
# We can create a matrix with matrix, rbind (row bind), or cbind (column
# bind).

matrix(c(1,2,3,4), nrow=2, ncol=2)
rbind(c(1,3), c(2,4))
cbind(c(1,2), c(3,4))

X <- rbind(
  c(1,0),
  c(1,0),
  c(1,1),
  c(1,1))
X
class(X)

# The matrix transpose is obtained with t.

t(X)

# Matrix multiplication is performed with %*%. The dot product of each
# row of the left hand side matrix and each column of the right hand
# side matrix is calculated. %*% treats a vector as either a single
# column or single row matrix as will make sense for matrix
# multiplication. Actually all we need today is to multiply a matrix by
# a vector, in which case we get the dot product of each row of the
# matrix with the vector.

X %*% a
as.vector(X %*% a)

# 2.3 Challenge - use a dot product to calculate ----
#
# The following dot product is an elaborate way to retrieve x[2]:

x <- c(10,20,30,40)
weights <- c(0,1,0,0)       # <-- modify this line
sum(weights*x)

# Modify weights in the above to calculate different quantities:
#
# A. x[3]-x[2]
#
# B. The mean of all four values.
#


#//////////////////////////////////
# 3 Single numerical predictor ----
#
# The age (year) and height (cm) of 10 people has been measured. We want
# a model that can predict height based on age.

people <- read_csv(
  "age, height
      10,    131
      14,    147
      16,    161
       9,    136
      16,    170
      15,    160
      15,    153
      21,    187
       9,    145
      21,    195")

ggplot(people, aes(x=age, y=height)) + geom_point()

fit <- lm(height ~ age, data=people)

fit

# Coefficients are extracted with coef:

coef(fit)

# The residual standard deviation is extracted with sigma:

sigma(fit)

# Behind the scenes a matrix of predictors has been produced from the
# mysterious notation ~ age. We can examine it explicitly:

model.matrix(fit)

# model.matrix can be used without first calling lm.

model.matrix(~ age, data=people)

# n=10 observations minus p=2 columns in the model matrix leaves 8
# residual degrees of freedom:

df.residual(fit)

# 3.1 Prediction ----
#
# predict predicts. By default it produces predictions on the original
# dataset.

predict(fit)

predict(fit, interval="confidence")

# We can also calculate predictions manually.

# Prediction for a 15-year old
x <- c(1, 15)
beta <- coef(fit)
sum(x * beta)

# Prediction for all original data
X <- model.matrix(fit)
as.vector( X %*% beta )

# predict can be used with new data.

new_people <- tibble(age=5:25)

predict(fit, new_people)

new_predictions <- cbind(
  new_people,
  predict(fit, new_people, interval="confidence"))

ggplot() +
  geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), data=new_predictions, fill="grey") +
  geom_line(aes(x=age, y=fit), data=new_predictions, color="blue") +
  geom_point(aes(x=age, y=height), data=people) +
  labs(y="height (cm)", x="age (year)",
       subtitle="Ribbon shows 95% confidence interval of the model")

# If you have ever used geom_smooth, it should now be a little less
# mysterious.

ggplot(people, aes(x=age, y=height)) + geom_smooth(method="lm") + geom_point()

# 3.2 Residuals ----
#
# The residuals are the differences between predicted and actual values.

residuals(fit)

# There should be no remaining relationship between predictions and the
# residuals (or between any individual predictors and the residual).

plot(predict(fit), residuals(fit))

# A Q-Q (quantile-quantile) plot sorts the residuals and compares them
# to what would be expected from a normal distribution.

qqnorm(residuals(fit))
qqline(residuals(fit))

# Ideally points would lie close to the line, but deviations are not a
# disaster. Our coefficient estimates will tend toward normally
# distributed errors even if the data does not, due to the Central Limit
# Theorem. Wild outliers should be investigated, as they may have a
# large effect on the model. We will see further examples of things to
# look for in a Q-Q plot in section 6.
#
# plot(fit) produces a series of more sophisticated diagnostic plots.

plot(fit)