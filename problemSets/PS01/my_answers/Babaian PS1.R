#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

set.seed(123)
## generate 1,000 Cauchy random variables
data <- rcauchy(1000, location = 0, scale = 1)

# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))

D #0.1347281
# Smaller values indicate similarity of the empirical distribution
# and the queried theoretical distribution. 

# putting D in the p-value formula:
p_value <- sqrt(2*pi)/0.1347281*sum(exp(-((2*(1:1000)-1)^2*pi^2)/(8*0.1347281^2)))
p-value #5.65274e-29, reject H0 at 0.999 confidence level

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

## create a function that returns the residual sum of squares to be minimized.
# The function accepts beta as a vector with 2 values provided below:
rss <- function(beta, x, y) {
  y_hat <- beta[1] + beta[2]*x
  return(sum((y - y_hat)^2))
}

# Optimize using built-in BFGS minimizing the RSS, assuming both coefficients are 0
bfgs <- optim(fn = rss, par = c(0, 0), x = data$x, y = data$y,  method = "BFGS")

# Extract the best set of parameters found 
bfgs <- bfgs$par
bfgs # 0.1391778 ; 2.7267000

# Fit OLS via lm
m1 <- lm(y ~ x, data = data)
m1$coef #   0.1391874  ; 2.7266985 
