#####################
library(stargazer)
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

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

str(climateSupport)
  
m1<- glm(choice ~ .,family = "binomial"(link="logit"), 
         data = climateSupport)
stargazer(m1, out = 'm1.tex')

m0 <- glm(choice ~ 1,family = "binomial"(link="logit"), 
          data = climateSupport)
anova(m0, m1, test = "LRT") #can reject H0

### function code from  https://cimentadaj.github.io/blog/2016-08-22-producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/
stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}

stargazer2(m1, odd.ratio = T, out = 'm1.tex')


#####################
# Problem 2
#####################
predicted_data <- with(climateSupport, expand.grid(sanctions = unique(sanctions),
                                               countries = unique(countries)))

predicted_data <- cbind(predicted_data, predict(m1, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))

predicted_data <- within(predicted_data,
                         {PredictedProb <- plogis(fit)
                         LL <- plogis(fit - (1.96 * se.fit))
                         UL <- plogis(fit + (1.96 * se.fit))
                         })
xtable::xtable(predicted_data)

m2 <- glm(choice ~ countries*sanctions,family = "binomial"(link="logit"), 
         data = climateSupport)
anova(m1, m2, test = "LRT") #there's no evidence of a reliable interaction predictor