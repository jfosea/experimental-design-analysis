library(ggplot2)
library(mosaic)
library(dplyr)
library(EnvStats)
library(lawstat)
library(DescTools)
setwd("C:/Users/surfacepro/Desktop/425_a2/datasets")

# problem 2
p2 <- read.csv("p2.csv")
type <- c(rep("Subcompact", 10), rep("Compact", 10), rep("Midsize", 10), rep("Full Size", 10))
period <- c(p2[,1], p2[,2], p2[,3], p2[,4])
p2 <- data.frame(type, period)
ggplot(data=p2, aes(x = type, y = period)) + geom_boxplot(col="black", fill="white") + xlab("Type of Car") + ylab("Number of Periods") + coord_flip()

a <- aov(period~type, data=p2)
p2$ei_terms <- residuals(a)
ggplot(data=p2, aes(sample=ei_terms)) + stat_qq(col="blue", size = 1) + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")
shapiro.test(p2$ei_terms)

b <- favstats(period~type, data=p2)
p2$fits <- c(rep(b$mean[1], 10), rep(b$mean[2], 10), rep(b$mean[3], 10), rep(b$mean[4], 10)) 
ggplot(data=p2, aes(x = fits, y = ei_terms)) + geom_point(size=2, col="blue") + xlab("Fitted Values") + ylab("Residuals/Error Terms") + ggtitle("Plot of Fits to Residuals") + geom_hline(yintercept=0, linetype="dashed", color="red")

a <- favstats(period~type, data=p2)$median
favstats(period~type, data=p2)
p2$median <- c(rep(a[4],10), rep(a[1],10), rep(a[3],10), rep(a[2],10))
p2$absdiff <- abs(p2$period - p2$median)
summary(aov(absdiff~type, data=p2))

# 1000 permutations
windows()
demopermtest1000.df = do(1000) * rsquared(lm(period ~ shuffle(type), data=p2))
obsrsquared = rsquared(lm(period~type, data=p2))
ggplot(data=demopermtest1000.df, aes(x = rsquared)) + geom_histogram(col="red", fill="blue", binwidth=0.05) + xlab("Values of r-squared") + ylab("Count") + ggtitle("Outcomes of 1000 Permutation Tests") + geom_vline(xintercept=obsrsquared, linetype="dashed", col="red")
howmany = sum(demopermtest1000.df$rsquared > obsrsquared)
emp_P_value = (howmany/1000)
emp_P_value
obsrsquared


# problem 3
windows()
p3 <- read.csv("p3.csv")
p3 <- p3[order(p3$SPECIES),]
ggplot(data=p3, aes(x = SPECIES, y = DDT)) + geom_boxplot(col="black", fill="white") + xlab("Species of Fish") + ylab("DDT Level") + coord_flip()

# check for homoscedasticity
a <- favstats(DDT~SPECIES, data=p3)$median
p3$median <- c(rep(a[1],96), rep(a[2],12), rep(a[3],36))
p3$absdiff <- abs(p3$DDT - p3$median)
summary(aov(absdiff~SPECIES, data=p3))

# check for normality
p3$residuals <- residuals(lm(DDT~SPECIES, data=p3))
ggplot(data=p3, aes(sample=residuals)) + stat_qq(col="blue", size = 1) + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")
shapiro.test(p3$residuals)

# does not pass the normality assumption
options(digits=5)
summary(aov(DDT~SPECIES, data=p3))
kruskal.test(DDT ~ SPECIES, data=p3)

demopermtest1000.df = do(1000) * rsquared(lm(DDT ~ shuffle(SPECIES), data=p3))
obsrsquared = rsquared(lm(DDT~SPECIES, data=p3))
ggplot(data=demopermtest1000.df, aes(x = rsquared)) + geom_histogram(col="red", fill="blue", binwidth=0.05) + xlab("Values of r-squared") + ylab("Count") + ggtitle("Outcomes of 1000 Permutation Tests") + geom_vline(xintercept=obsrsquared, linetype="dashed", col="red")
howmany = sum(demopermtest1000.df$rsquared > obsrsquared)
emp_P_value = (howmany/1000)
emp_P_value
obsrsquared


# CI for fishes
shapiro.test(p3$DDT[which(p3$SPECIES=="LARGEMOUTHBASS")])
ggplot(data=p3, aes(sample=residuals)) + stat_qq(col="blue", size = 1) + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")

options(decimal=3)
summary(aov(DDT~SPECIES, data=p3))
mse <- 9649  
mean(p3$DDT)+ c(-1,1)*abs(qt(0.05/2, 144-1)*(sqrt(mse/144)))
                      
a <- favstats(DDT~SPECIES, data=p3)

# CHANNELCATFISH
a$mean[1] + c(-1,1)*abs(qt(0.05/2, a$n[1]-1))*(sqrt(mse/a$n[1]))

# LARGEMOUTHBASS
a$mean[2] + c(-1,1)*abs(qt(0.05/2, a$n[2]-1))*(sqrt(mse/a$n[2]))

# SMALLMOUTHBUFF
a$mean[3] + c(-1,1)*abs(qt(0.05/2, a$n[3]-1))*(sqrt(mse/a$n[3]))

# d
PostHocTest(aov(DDT ~ SPECIES, data=p3), method="bonferroni", conf.level=0.95)


# problem 4
# a
p4 <- read.csv("p4.csv")
summary(aov(RECALL~GROUP, data=p4))
kruskal.test(RECALL ~ GROUP, data=p4)

# c
demopermtest1000.df = do(1000) * rsquared(lm(RECALL ~ shuffle(GROUP), data=p4))
obsrsquared = rsquared(lm(RECALL~GROUP, data=p4))
obsrsquared
ggplot(data=demopermtest1000.df, aes(x = rsquared)) + geom_histogram(col="red", fill="blue", binwidth=0.05) + xlab("Values of r-squared") + ylab("Count") + ggtitle("Outcomes of 1000 Permutation Tests") + geom_vline(xintercept=obsrsquared, linetype="dashed", col="red")
howmany = sum(demopermtest1000.df$rsquared > obsrsquared)
emp_P_value = (howmany/1000)
emp_P_value
favstats(RECALL~GROUP, data=p4)

# d
install.packages("mplot")
library(mplot)
HSD <-  TukeyHSD(RECALL ~ GROUP, ordered=T, conf.level=0.95, data=p4)
HSD <- PostHocTest(aov(RECALL ~ GROUP, data=p4), method="hsd", conf.level=0.95, ordered = TRUE)
mplot(HSD, order="pval")


# problem 5
windows()
p5 <- read.csv("p5.csv")
p5$residuals <- residuals(lm(SCORE~GROUP, data=p5))
b <- favstats(SCORE~GROUP, data=p5)$mean
p5$fits <- c(rep(b[1], 108), rep(b[2], 108), rep(b[3], 108))

# conditions check
# 1. normalityL use CLT
ggplot(data=p5, aes(sample=residuals)) + stat_qq(col="blue", size = 1) + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")
shapiro.test(residuals(lm(SCORE~GROUP, data=p5)))
ggplot(data=p5, aes(x = GROUP, y = SCORE)) + geom_boxplot(col="black", fill="white") + xlab("TV Show Rating") + ylab("Number of Commericals Remembered") + coord_flip()
ggplot(data=p5, aes(x = fits, y = residuals)) + geom_point(size=2, col="blue") + xlab("Fitted Values") + ylab("Residuals/Error Terms") + ggtitle("Plot of Fits to Residuals") + geom_hline(yintercept=0, linetype="dashed", color="red")

#  2. Homoscedasticity: levene test for equal variance
a <- favstats(SCORE~GROUP, data=p5)
b <- a$median
median <- c(rep(b[1], 108), rep(b[2], 108), rep(b[3], 108))
p5$median <- median
p5$absdiff <- abs(p5$SCORE - p5$median)
summary(aov(absdiff~GROUP, data=p5))

# (1) ANOVA test for equal means
summary(aov(SCORE~GROUP, data=p5))

# (2) Permutation test
set.seed(1)
demopermtest1000.df = do(1000) * rsquared(lm(SCORE ~ shuffle(GROUP), data=p5))
obsrsquared = rsquared(lm(SCORE~GROUP, data=p5))
obsrsquared
ggplot(data=demopermtest1000.df, aes(x = rsquared)) + geom_histogram(col="red", fill="blue", binwidth=0.05) + xlab("Values of r-squared") + ylab("Count") + ggtitle("Outcomes of 1000 Permutation Tests") + geom_vline(xintercept=obsrsquared, linetype="dashed", col="red")
howmany = sum(demopermtest1000.df$rsquared > obsrsquared)
emp_P_value = (howmany/1000)
emp_P_value


kruskal.test(SCORE~GROUP, data=p5)


# further
favstats(SCORE~GROUP, data=p5)
TukeyHSD(SCORE ~ GROUP, ordered=T, conf.level=0.95, data=p5)

# (1) condition checks
# central limit theorem for normality
# boxplots to check for homoscedasticity and perform levene test

# (2) Statistical methods
# use anova
# use permutation test

# (3) find most significant variable
# use multiple comparison test. use lsd, hsd, or bonferri.

# (4) conclude
        