library(haven)
library(ggplot2)
library(stargazer)
library(tidyverse)
library(sjlabelled)
library(labelled)

Q2 <- read.csv('War Split.csv')
Q2 <- Q2 %>% select('Split.T0','Beheading','Border','State', 'War.T0')
summary(Q2)
Q2 <- Q2 %>% mutate(Split = `Split.T0`)
Q2 <- Q2 %>% mutate(War = War.T0)
Q2 <- Q2 %>% mutate(Fragmentation = Split)
Q2 <- Q2 %>% mutate(Military_Operation = State)
Q2 <- Q2 %>% mutate(Border_Control = Border)
com_Q2 <- Q2[complete.cases(Q2), ]
summary(Q2$War)
dim(Q2)
summary(Q2)
# Get my Phi Correlation matrix
install.packages('psych')
library(psych)
corr = phi(Q2, digits=2)



# I will produce my tabulations to for descriptive statistics, including 
# the correlations of all the variables in my model.
table1 <- table(Q2$Beheading, Q2$Split)
prop.table(table1)
ftable(round(prop.table(table1), 3))
xtabs(Split ~ State + Beheading + Border + War.T0, Q2)
summary(xtabs(Split~ State + Beheading + Border + War.T0, Q2))
install.packages("crosstable")
library(crosstable)
ct1 = crosstable(Q2, c(Military_Operation, Beheading, Border_Control, War), by=Fragmentation, total="both", 
                 percent_digits=0) %>%
  as_flextable(compact=T, header_show_n=1:2)
ct1
summary(Q2)

Q2$Split <- as.factor(Q2$Split)
Q2$Beheading <- as.factor(Q2$Beheading)
Q2$Border <- as.factor(Q2$Border)
Q2$State <- as.factor(Q2$State)
Q2$War.T0 <- as.factor(Q2$War.T0)
dim(Q2)
summary(Q2)

#Now I develop my models. These will be nested models of a logistic regression.
library(stats)
logit_1 <- glm(Fragmentation ~ Beheading, data = Q2)
summary(logit_1)
logit_2 <- glm(Fragmentation ~ Beheading + Military_Operation, data=Q2)
summary(logit_2)
logit_3 <- glm(Fragmentation ~ Beheading + Military_Operation + Border_Control, data=Q2)
summary(logit_3)
logit_4 <- glm(Fragmentation ~ Beheading + Military_Operation + Border_Control + War, data=Q2)
# I use stargazer for nested models
stargazer(logit_1, logit_2, logit_3, logit_4,
          type = "latex", style='ajs',
          dep.var.labels = c("Fragmentation"),
          title = "Table 2: Logistic Regression Results",
          digits = 2,
          out = "models2.txt",
          covariate.labels = c("Beheading","Military operation", "Border Control"))



