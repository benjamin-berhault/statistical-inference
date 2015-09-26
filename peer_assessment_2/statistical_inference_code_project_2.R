## clear environment variable
rm(list=ls(all=TRUE))

# load necessary library
library(datasets)
library(ggplot2)
library(gridExtra)
library(GGally)
library(data.table)

# The dataset concern the Effect of Vitamin C on Tooth Growth in Guinea Pigs
data(ToothGrowth)
toothGrowth <- data.table(ToothGrowth) 
setnames(toothGrowth, c('len','supp','dose'),c('Length','Supplement','Dose'))
toothGrowth$Dose <- as.factor(toothGrowth$Dose) # convert to factor

# Basic Summary of the data
str(toothGrowth)
summary(toothGrowth)
head(toothGrowth)

# Contingency table
table(toothGrowth$Supplement, toothGrowth$Dose)

# Dose comparison
p1 <- ggplot(data=toothGrowth, aes(x=Dose,y=Length,fill=Dose)) +
  geom_boxplot() + 
  theme(legend.position="none") + 
  facet_grid(.~Supplement)

p2 <- ggplot(data=toothGrowth, aes(x=Supplement,y=Length,fill=Supplement)) +
  geom_boxplot() + 
  theme(legend.position="none") + 
  facet_grid(.~Dose) 

# Supplements comparison
p3 <- ggplot(data=toothGrowth, aes(x=Supplement,y=Length,fill=Supplement)) +
  geom_boxplot()

p4 <- ggplot(data=toothGrowth, aes(x=Dose,y=Length,fill=Dose)) +
  geom_boxplot()

grid.arrange(p1, p4, p2, p3, ncol = 2, nrow=2)

# Confidence intervals and/or hypothesis tests to compare tooth growth by Supplement and Dose.
dataVC <- toothGrowth[toothGrowth$Supplement=='VC',]
dataOJ <- toothGrowth[toothGrowth$Supplement=='OJ',]
lengthComparison <- (dataOJ$Length - dataVC$Length)
comp_data <- data.frame(lengthComparison, dataOJ$Dose)
names(comp_data) <- c("lengthComparison", "Dose")

# Dose : 0.5 mg
n <- subset(comp_data, Dose == 0.5)$lengthComparison
t.test(n)$conf

# Dose : 1 mg
n <- subset(comp_data, Dose == 1)$lengthComparison
t.test(n)$conf

# Dose : 2 mg
n <- subset(comp_data, Dose == 2)$lengthComparison
t.test(n)$conf

# Analysis of Variance (ANOVA)
anova.out <- aov(Length ~ Supplement * Dose, data=toothGrowth)
summary(anova.out)

# Table means (Supplement x Dose)
print(model.tables(anova.out,"means"),digits=3)
