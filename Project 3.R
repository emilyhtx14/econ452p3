setwd("~/Desktop/REE")
library(haven)
library(dplyr)
library(sjlabelled)
gss<- readRDS("gss.RDS")

# keeps only female respondents who are full- or part-time employed or ID as
# "keeping house" (no students, unemployed, or retired) 
# and have at least one child
wgss <- gss %>%
  filter(sex == 2 & wrkstat %in% c(1, 2, 3, 7) & childs != 0)

smalldata <- wgss[,c('year','wrkstat','fechld','fepresch','fepol','fund',
                     'polviews','childs','age','marital','educ','realinc',
                     'spwrksta')]
smalldata <- smalldata %>%
  filter(year > 1993)

smalldata$fechld <- smalldata$fechld - 1
# goes from 0 (strongly believe mother working doesn't hurt child) to 3
# (strongly believe mother working does hurt child)

smalldata$fepresch <- 4 - smalldata$fepresch
# goes from 0 (strongly believe mother working doesn't hurt child) to 3
# (strongly believe mother working does hurt child)

smalldata$nowork <- as.numeric(smalldata$wrkstat == 7)

# Instruments
smalldata$fepol <- 2 - smalldata$fepol
# 0 (men not better suited) and 1 (men better suited)
smalldata$isfund <- as.numeric(smalldata$fund == 1)
smalldata$iscons <- as.numeric(smalldata$polviews > 4)

# Controls
smalldata$mar <- as.numeric(smalldata$marital == 1)
smalldata$swork <- as.numeric(smalldata$spwrksta %in% c(1,2,3))
smalldata$faminc <- smalldata$realinc/1000 # family income in thousands

data1996 <- smalldata %>%
  filter(year == 1996)

library(stargazer)
library(ivreg)
model1 <- lm(nowork~fechld+childs+age+educ+faminc+mar+swork,data=data1996)
model2 <- ivreg(nowork~fechld+childs+age+educ+faminc+mar+swork
                |.-fechld+fepol+iscons+isfund,data=data1996)
stargazer(model1,model2,header=FALSE,title="",type="text")
summary(model2,diagnostics=TRUE)