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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)
str(gdp_data)

# subsetting the DV values and transforming to a factor:
gdp_data$GDPWdiff <- as.factor(ifelse(gdp_data$GDPWdiff > 0, "positive", 
                       ifelse(gdp_data$GDPWdiff < 0, "negative", "no change")))

ftable(gdp_data$GDPWdiff)

# subsetting the regime values and transforming to a factor:
gdp_data$REG <- as.factor(ifelse(gdp_data$REG == 0, "Non-Democracy", "Democracy"))
ftable(gdp_data$REG)

# subsetting the fuel exports share values and transforming to a factor:
gdp_data$OIL <- as.factor(ifelse(gdp_data$OIL > 0.5, "1", "0"))
ftable(gdp_data$OIL)

### 1: an unordered multinomial logit 

# set a reference level for the DV
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no change")

# run model
mult.log <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data)
summary(mult.log)
exp(coef(mult.log))

stargazer::stargazer(mult.log,out = "1.1.tex")

### 2: an ordered multinomial logit 
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "negative")
ord.log <- polr(GDPWdiff ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(ord.log)

ci <- confint(ord.log)

exp(cbind(OR = coef(ord.log), ci))

stargazer::stargazer(ord.log,out = "1.2.tex")

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

mod.ps <- glm(PAN.visits.06 ~ competitive.district +
                marginality.06 + PAN.governor.06, data = mexico_elections, family = poisson)
stargazer::stargazer(mod.ps, out = "2.1.tex")

summary(mod.ps)
cfs <- exp(coef(mod.ps))
cfs


# competitive.district=1 + marginality.06 = 0 + PAN.governor.06=1:

exp(coef(mod.ps)[1] + coef(mod.ps)[2]*1 + coef(mod.ps)[3]*0 + coef(mod.ps)[4]*1)
# 0.01494818 

## verifying:
pred <- data.frame(competitive.district=1,
                   marginality.06 = 0,
                   PAN.governor.06=1)

# check with predict() function
predict(mod.ps, newdata = pred, type = "response")
#0.01494818 