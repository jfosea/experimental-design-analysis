library(ggplot2)
library(mosaic)
library(dplyr)
library(EnvStats)
library(lawstat)
library(DescTools)
library(ggpubr)
setwd("C:\\Users\\surfacepro\\Desktop\\a4")


# PROBLEM 1

# (B)
km <- c(0.74, 0.72, 0.57, 0.62, 
        0.61, 0.76, 0.74, 0.58,
        0.54, 0.60, 0.71, 0.70,
        0.71, 0.56, 0.60, 0.74)
driver <- rep(c("1", "2", "3", "4"), 4)
car <- c(rep(c("1"), 4), rep(c("2"), 4), rep(c("3"), 4), rep(c("4"), 4))
brand <- c("A", "B", "C", "D", 
           "D", "A", "B", "C", 
           "C", "D", "A", "B", 
           "B", "C", "D", "A")
p1 <- data.frame(km, driver, car, brand)
p1.aov = aov(km ~ brand + driver + car, data=p1)
summary(p1.aov)

# (C)
windows()
p1.Tukey = TukeyHSD(p1.aov, conf.level=0.95, ordered=T)
p1.Tukey$brand 

# (D)
e.terms = residuals(p1.aov)
fit.terms = fitted(p1.aov)
ggplot(data=p1.aov, aes(sample = e.terms)) + stat_qq(size=2, col="blue") + stat_qqline(col="red") + ggtitle("Normal Probability Plot of Residuals")
ggplot(data=p1.aov, aes(x = fit.terms, y = e.terms)) + geom_point(size=2, col="blue") + xlab("Fitted Values") + ylab("Residuals") + geom_hline(yintercept=0, linetype="dashed", col="red")


# PROBLEM 2

# (A)

# all
MSBlock1 <- 0.000092
MSBlock2 <- 0.000892
MSE <- 0.000058
p <- 4

num1 <- MSBlock1 + MSBlock2 - (p-1)*MSE
den1 <- (p+1)*MSE
RE1 <- num1/den1
RE1

# (B) (ii)
# driver
num2 <- MSBlock2 + (p-1)*MSE
den2 <- p*MSE
RE2 <- num2/den2

# (B) (i)
# car
num3 <- MSBlock1 + (p-1)*MSE
den3 <- p*MSE
RE3 < num3/den3



# PROBLEM 3

# (b)


# PROBLEM 4

# (a)
windows()
p4 <- read.csv("p4.csv")
PREP <- p4$PREP
RATING <- p4$RATING
STANDING <- p4$STANDING
interaction.plot(x.factor = PREP, 
                 trace.factor = STANDING, 
                 response=RATING, 
                 fun=mean, 
                 col=c("orange", "green", "blue"), 
                 xlab="Pesticide Type", 
                 ylab="Mean Bushels per Tree", 
                 main="Interaction Plot", lwd=2)


# (b) 
p4.aov <- aov(RATING~PREP*STANDING, data=p4)
summary(p4.aov)

# (c)
windows()
residuals <- residuals(p4.aov)
fitted <- fitted(p4.aov)
p4.diag <- data.frame(residuals, fitted, RATING, PREP, STANDING)
ggplot(data=p4.diag, aes(sample = residuals)) + stat_qq(size=2, col="blue") + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")
shapiro.test(p4.diag$residuals)

favstats(RATING~PREP, data=p4.diag)
favstats(RATING~STANDING, data=p4.diag)
standing.means <- c(rep(4.909091, 44), rep(4.909091, 44), rep(4.159091, 44))
prep.means <- c(rep(5.303030, 66), rep(4.015152, 66))
p4.diag <- data.frame(residuals, fitted, RATING, PREP, STANDING, standing.means, prep.means)
ggplot(data=p4.diag, aes(x = standing.means, y = residuals)) + geom_point(size=2, col="blue") + xlab("Means Response of Rating") + ylab("Residuals") + ggtitle("Plot of Residuals to Standing Level Means") + geom_hline(yintercept=0, linetype="dashed", col="red")
ggplot(data=p4.diag, aes(x = prep.means, y = residuals)) + geom_point(size=2, col="blue") + xlab("Means Response of Rating") + ylab("Residuals") + ggtitle("Plot of Residuals to Prep Level Means") + geom_hline(yintercept=0, linetype="dashed", col="red")


# (d)
p4.aov <- aov(RATING~PREP*STANDING, data=p4)
summary(p4.aov)
p4.Tukey <- TukeyHSD(p4.aov, conf.level=0.95, ordered=T)
p4.Tukey$PREP

# (e)
summary(p4.aov)


# PROBLEM 5

# (a)
p5 <- read.csv("p5.csv", header=TRUE)
p5.1 <- data.frame(stack(p5), rep(p5$ï..,3))
names(p5.1) <- c("rate", "hotel", "city")


# (b)
p5.aov <- aov(rate~hotel+city, p5.1)
summary(p5.aov)
p5.1$residuals <- residuals(p5.aov)
ggplot(data=p5.1, aes(sample = residuals)) + stat_qq(size=2, col="blue") + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")
shapiro.test(p5.1$residuals)

# (c)
p5.aov <- aov(rate~hotel+city, p5.1)
summary(p5.aov)

# (d)
p5.aov <- aov(rate~hotel+city, p5.1)
summary(p5.aov)


# PROBLEM 6

# (a)
p6 <- read.csv("p6.csv", header=TRUE)
p6.1 <- data.frame(stack(p6), rep(p6$C1.T, 3))
names(p6.1) <- c("cost", "city", "shop_type")

p6.1.aov <- aov(rate~city*shop_type, data=p6.1)
summary(p6.1.aov)
p6.1$residuals <- residuals(p6.1.aov)
ggplot(data=p6.1, aes(sample = residuals)) + stat_qq(size=2, col="blue") + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")

# (a)
msab <- 1.787

mse <- 11.213
msshop <- 0.720
mscity <- 15.563

mscity / msab 
msshop / msab

# (B)
summary(p6.1.aov)


# (d)
fobs <- msshop/msab
pval <- 1-pf(fobs, 1, 2)
c(fobs, pval)

# (e)
fobs <- mscity/msab
pval <- 1-pf(fobs, 2, 2)
c(fobs, pval)

# PROBLEM 7

p7 <- read.csv("p7.csv")
p7.aov <- aov(Price~Store+Item, data=p7)
summary(p7.aov)

p7.Tukey = TukeyHSD(p7.aov, conf.level=0.95, ordered=T)
p7.Tukey$Store

e.terms = residuals(p7.aov)
fit.terms = fitted(p7.aov)
ggplot(data=p7.aov, aes(sample = e.terms)) + stat_qq(size=2, col="blue") + stat_qqline(col="red") + ggtitle("Normal Probability Plot of Residuals")


favstats(Price~Item, data=p7)
favstats(Price~Store, data=p7)
standing.means <- c(rep(4.909091, 44), rep(4.909091, 44), rep(4.159091, 44))
prep.means <- c(rep(5.303030, 66), rep(4.015152, 66))
p4.diag <- data.frame(residuals, fitted, RATING, PREP, STANDING, standing.means, prep.means)
ggplot(data=p4.diag, aes(x = standing.means, y = residuals)) + geom_point(size=2, col="blue") + xlab("Means Response of Rating") + ylab("Residuals") + ggtitle("Plot of Residuals to Standing Level Means") + geom_hline(yintercept=0, linetype="dashed", col="red")
ggplot(data=p4.diag, aes(x = prep.means, y = residuals)) + geom_point(size=2, col="blue") + xlab("Means Response of Rating") + ylab("Residuals") + ggtitle("Plot of Residuals to Prep Level Means") + geom_hline(yintercept=0, linetype="dashed", col="red")
