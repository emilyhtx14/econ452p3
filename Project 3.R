library(haven)
library(dplyr)
library(sjlabelled)
library(ggplot2)
library(stargazer)
library(ivreg)

# setwd("~/Desktop/REE")
# setwd("/Users/emilyhuang/econ452/econ452p3")

# install.packages("AER")

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


model1 <- lm(nowork~fechld+childs+age+educ+faminc+mar+swork,data=data1996)
model2 <- ivreg(nowork~fechld+childs+age+educ+faminc+mar+swork
                |.-fechld+fepol+iscons+isfund,data=data1996)
stargazer(model1,model2,header=FALSE,title="",type="text")
summary(model2,diagnostics=TRUE)

# show that instruments correlate with fechld
# Scatterplot for iscons vs fechld
ggplot(data1996, aes(x = iscons, y = fechld)) +
  labs(x = "iscons", y = "fechld") +
  geom_smooth(data = subset(data1996, !is.na(iscons) & !is.na(fechld)), 
              aes(x = iscons, y = fechld))

# Scatterplot for isfund vs fechld
ggplot(data1996, aes(x = isfund, y = fechld)) +
  labs(x = "isfund", y = "fechld") +
  geom_smooth(data = subset(data1996, !is.na(isfund) & !is.na(fechld)), 
              aes(x = isfund, y = fechld))

# Scatterplot for fepol vs fechld
ggplot(data1996, aes(x = fepol, y = fechld)) +
  labs(x = "fepol", y = "fechld") +
  geom_smooth(data = subset(data1996, !is.na(fepol) & !is.na(fechld)), 
              aes(x = fepol, y = fechld), span = 0.8)

# show that instruments are not strongly correlated with employment
# Scatterplot for iscons vs nowork
ggplot(data1996, aes(x = iscons, y = nowork)) +
  labs(x = "iscons", y = "nowork") +
  geom_smooth(data = subset(data1996, !is.na(iscons) & !is.na(nowork)), 
              aes(x = iscons, y = nowork))

# Scatterplot for fepol vs nowork
ggplot(data1996, aes(x = fepol, y = nowork)) +
  labs(x = "fepol", y = "nowork") +
  geom_smooth(data = subset(data1996, !is.na(fepol) & !is.na(nowork)), 
              aes(x = fepol, y = nowork), span = 0.9)

# Scatterplot for isfund vs nowork
ggplot(data1996, aes(x = isfund, y = nowork)) +
  labs(x = "isfund", y = "nowork") +
  geom_smooth(data = subset(data1996, !is.na(isfund) & !is.na(nowork)), 
              aes(x = isfund, y = nowork))

# demonstrate why we chose fechld
# Scatterplot for fechld vs nowork
ggplot(data1996, aes(x = fechld, y = nowork)) +
  labs(x = "fechld", y = "nowork") +
  geom_smooth(data = subset(data1996, !is.na(fechld) & !is.na(nowork)), 
              aes(x = fechld, y = nowork))

# Scatterplot for fepresch vs nowork
ggplot(data1996, aes(x = fepresch, y = nowork)) +
  labs(x = "fepresch", y = "nowork") +
  geom_smooth(data = subset(data1996, !is.na(fepresch) & !is.na(nowork)), 
              aes(x = fepresch, y = nowork))

