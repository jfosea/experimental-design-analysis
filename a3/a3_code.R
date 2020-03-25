library(ggplot2)
library(mosaic)
library(dplyr)
library(EnvStats)
library(lawstat)
library(DescTools)
setwd("C:\\Users\\surfacepro\\Desktop\\a3")

# PROBLEM 1
p1 <- read.csv("p1.csv")
dosage <- c(rep("Honey", 37), rep("DM", 37), rep("Control", 37))
severity <- c(p1[,1], p1[,2], p1[,3])
p1 <- data.frame(dosage, severity)


# (a)
# treatment: DM
# placebo: Honey
# control: Control


# (b) boxplot
windows()
ggplot(data=p1, aes(x = dosage, y = severity)) + geom_boxplot(col="black", fill="white") + xlab("Type of Dosage") + ylab("Total Scale of Severity") + coord_flip()

# (c) means model
summary(coughaov <- aov(severity ~ dosage, data=p1))
DunnettTest(severity ~ dosage, data = p1, control="Control")

# PROBLEM 2
# use orthogonality
p2 <- read.csv("p2.5.csv")
p2
# L1 = mu_neutral - (mu_sex + mu_violent)/2
cneut <- 1
csex <- -1/2
cviol <- -1/2
summary(p2aov <- aov(SCORE~GROUP, data=p2))
xbarn <- favstats(SCORE~GROUP, data=p2)$mean
L1squared <- sum(xbarn*c(cneut, csex, cviol))^2
L1squared
denom <- sum(c(cneut, csex, cviol)^2/108)
denom
SSL1 <- L1squared/denom
SSL1
df1 <- 1
MSL1 <- SSL1/df1
MSE <- 3.01
1-pf(MSL1/MSE, 1,321)

# PROBLEM 3
p3 <- read.csv("p3.csv")

# (a) same variance?
summary(p3aov <- aov(DISTANCE ~ BRAND, data=p3))

# (b) 95% of sigma
n <- 40
MSW <- 350.5695
top.value  = (n - 4)*MSW #computes numerator
lb.sigma = top.value/(qchisq(0.975, n - 4))
ub.sigma = top.value/(qchisq(0.025, n - 4))
sqrt(c(lb.sigma, ub.sigma))

# (c) estimate total variation
MSB <- 1099.5523
(MSB-MSW)/4

# (d) find ICC
msbetween = MSB
mswithin = MSW
numerdf = 4 - 1
denomdf = 40 -4
ni = 4
#
L = (1/ni)*(((msbetween)/(mswithin)*(1/qf(0.975, numerdf, denomdf))) - 1)
U = (1/ni)*(((msbetween)/(mswithin)*(1/qf(0.025, numerdf, denomdf))) - 1)
icc.lb = L/(1 + L)
icc.ub = U/(1 + U)
c(icc.lb, icc.ub)

# PROBLEM 4
p4 <- read.csv("p4.csv")
strain <- c(rep("USDA",15), rep("Field", 15), rep("Resistant", 15))
eggs <- c(p4[,1], p4[,2], p4[,3])
p4 <- data.frame(strain, eggs)

# (a) check equal variances
windows()
ggplot(data=p4, aes(x = strain, y = eggs)) + geom_boxplot(col="black", fill="white") + xlab("Type of Strains") + ylab("Number of Eggs Laid") + coord_flip()
leveneTest(eggs~strain, data=p4)

# (b) find transformation
library(car)
par(mfrow=c(2,2))
shapiro.test(residuals(m1))

a <- favstats(eggs~strain, data=p4)
logsds = log(a$sd)
logmeans = log(a$mean)
logstats.df = data.frame(a$strain, logsds, logmeans)
logstats.df
ggplot(data=logstats.df, aes(x = logmeans, y = logsds)) + geom_point(col="blue", size=2) + xlab("Log of Sample Means") + ylab("Log of Sample Standard Deviations") + stat_smooth(method="lm", col="red") + ggtitle("Scatterplot of Log(Means) to Log(SDs)")
summary(lm(logsds ~ logmeans, data=logstats.df))

# (c) apply transform
p4$eggsT <- p4$eggs^0.5
leveneTest(eggsT~strain, data=p4)

# before transf
p4$residuals <- residuals(lm(eggs~strain, data=p4))
b <- favstats(eggs~strain, data=p4)
p4$fits <- c(rep(b$mean[1], b$n[1]), rep(b$mean[2], b$n[2]), rep(b$mean[3], b$n[3]))
plot1 <- ggplot(data=p4, aes(x = fits, y = residuals)) + geom_point(size=2, col="blue") + xlab("Fitted Values") + ylab("Residuals/Error Terms") + ggtitle("Plot of Fits to Residuals") + geom_hline(yintercept=0, linetype="dashed", color="red")
plot2 <- ggplot(data=p4, aes(sample=residuals)) + stat_qq(col="blue", size = 1) + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")

# after transf
p4$residualsT <- residuals(lm(eggsT~strain, data=p4))
b <- favstats(eggsT~strain, data=p4)
p4$fitsT <- c(rep(b$mean[1], b$n[1]), rep(b$mean[2], b$n[2]), rep(b$mean[3], b$n[3]))
plot3 <- ggplot(data=p4, aes(x = fitsT, y = residualsT)) + geom_point(size=2, col="blue") + xlab("Fitted Values") + ylab("Residuals/Error Terms") + ggtitle("Plot of Fits to Residuals") + geom_hline(yintercept=0, linetype="dashed", color="red")
plot4 <- ggplot(data=p4, aes(sample=residualsT)) + stat_qq(col="blue", size = 1) + stat_qqline(col="red") + ggtitle("Normal Probability Plot of the Residuals")

library(gridExtra)
grid.arrange(plot1, plot3, plot2, plot4, ncol=2, nrow=2)



# (d) equal means?
summary(aov1 <- aov(eggsT~strain, data=p4))
PostHocTest(aov(eggsT ~ strain, data=p4), method="hsd", conf.level=0.95, ordered = TRUE)
TukeyHSD(aov(eggsT ~ strain, data=p4), conf.level=0.95)


# (e)
p4$eggsPlus <-p4$eggs + 0.01
originalmodel = lm(eggsPlus ~ strain, data=p4) #assign the orginal model
bcoutput = boxcox(originalmodel, lambda=seq(-3, 3, 0.01))
best.lambda=bcoutput$x[which (bcoutput$y == max(bcoutput$y))]
best.lambda

# PROBLEM 5
windows()

# (b)
p5 <- read.csv("p5.csv")
p5$Month <- as.factor(p5$Month)
summary(aov(Energy~Condition+Month, data=p5))

# (c)
summary(block <- aov(Energy ~ Condition + Month, data=p5))

# (d)
windows()
favstats(Energy~Condition, data=p5)
p5tuk <- TukeyHSD(aov(Energy ~ Condition+Month, data=p5), conf.level=0.95, ordered = TRUE)
p5tuk
mplot(p5tuk)

# PROBLEM 6

# (a)
p6 <- read.csv("p6.csv")

timelapse <- c(p6[,2], p6[,3], p6[,4], p6[,5], p6[,6])
agent <- as.factor(c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20)))
p6 <- data.frame(agent, timelapse)

c1 <- 1/2
c2 <- 1/2
c3 <- -1/2
c4 <- -1/2
summary(p6aov <- aov(timelapse~agent, data=p6))
xbarn <- favstats(timelapse~agent, data=p6)$mean[1]
L1squared <- sum(xbarn*c(c1, c2, c3, c4))^2
L1squared
denom <- sum(c(c1, c2, c3, c4)^2/20)
denom
SSL1 <- L1squared/denom
SSL1
df1 <- 1
MSL1 <- SSL1/df1
MSL1
MSE <- 7.5226
fObs <- MSL1/MSE
fObs
1-pf(fObs, 1,98)

# (b)
alpha <- 1-0.99^{1/5}
alpha
contrastError = function(alpha, MSW, c1, c2, c3, c4, k, ni) #create a function that computes the MOE of a contrast
{
  svalue = sqrt((k - 1)*qf(1 - alpha, k - 1, (k*ni - k)))
  contrast.sd = sqrt(MSW*((c1^2/ni) + (c2^{2}/ni) + (c3^2/ni) + (c4^{2}/ni)))
  contrast.moe = svalue*contrast.sd
  contrast.moe
}

moecontrast1 = contrastError(alpha, MSE, 1, -1, 0, 0, 5, 20)
moecontrast2 = contrastError(alpha, MSE, 1, -1, 0, 0, 5, 20)
moecontrast3 = contrastError(alpha, MSE, 1/2, 1/2, -1, 0, 5, 20)
moecontrast4 = contrastError(alpha, MSE, 1/2, 1/2, -1, 0, 5, 20)
moecontrast5 = contrastError(alpha, MSE, 1/2, 1/2, -1/2, -1/2, 5, 20)

lEstimate <- function(c1, c2, c3, c4, xbar1, xbar2, xbar3, xbar4)
{
  a <- c(c1, c2, c3, c4)
  b <- c(xbar1, xbar2, xbar3, xbar4)
  sum(a*b)
}


x <- favstats(timelapse~agent, data=p6)
a1 <- x$mean[1]  
a2 <- x$mean[2]  
a3 <- x$mean[3]  
a4 <- x$mean[4]  
a5 <- x$mean[5]

L1 <- lEstimate(1, -1, 0, 0, a1, a2, 0, 0)
L2 <- lEstimate(1, -1, 0, 0, a3, a4, 0, 0) 
L3 <- lEstimate(1/2, 1/2, -1, 0, a1, a2, a5, 0)
L4 <- lEstimate(1/2, 1/2, -1, 0, a3, a4, a5, 0)
L5 <- lEstimate(1/2, 1/2, -1/2, -1/2, a1, a2, a3, a4)

a <- L1 + c(-1,1)*moecontrast1 #CI for L1
b <- L2 + c(-1,1)*moecontrast2 #CI for L2
c <- L3 + c(-1,1)*moecontrast3 #CI for L3
d <- L4 + c(-1,1)*moecontrast4 #CI for L4
e <- L5 + c(-1,1)*moecontrast5 #CI for L5


lower <- as.numeric(rbind(a, b, c, d, e)[,1])
upper <- as.numeric(rbind(a, b, c, d, e)[,2])
combo <- c("L1", "L2", "L3", "L4", "L5")
options(digits = 5)
data.frame(combo, lower, upper)

# (c)
p6 <- read.csv("p6.csv")
A1 <- p6$A1*0.25
A2 <- p6$A2*0.2
A3 <- p6$A3*0.2
A4 <- p6$A4*0.2
A5 <- p6$A5*0.15
p6T <- stack(data.frame(A1, A2, A3, A4, A5))
ggplot(data=p6T, aes(x = ind, y = values)) + geom_boxplot(col="black", fill="white") + xlab("Agent Number") + ylab("Time Lapse in Days") + coord_flip()


a <- t.test(A2,A1, alternative = "two.sided", var.equal=T)$conf.int[1:2]
b <- t.test(A3,A1, alternative = "two.sided", var.equal=T)$conf.int[1:2]
c <- t.test(A4,A1, alternative = "two.sided", var.equal=T)$conf.int[1:2]
d <- t.test(A5,A1, alternative = "two.sided", var.equal=T)$conf.int[1:2]
e <- t.test(A3,A2, alternative = "two.sided", var.equal=T)$conf.int[1:2]
f <- t.test(A4,A2, alternative = "two.sided", var.equal=T)$conf.int[1:2]
g <- t.test(A5,A2, alternative = "two.sided", var.equal=T)$conf.int[1:2]
h <- t.test(A4,A3, alternative = "two.sided", var.equal=T)$conf.int[1:2]
i <- t.test(A5,A3, alternative = "two.sided", var.equal=T)$conf.int[1:2]
j <- t.test(A5,A4, alternative = "two.sided", var.equal=T)$conf.int[1:2]

lower <- as.numeric(rbind(a,b,c,d,e,f,g,h,i,j)[,1])
upper <- as.numeric(rbind(a,b,c,d,e,f,g,h,i,j)[,2])
combo <- c("A2-A1","A3-A1", "A4-A1", "A5-A1", "A3-A2",
           "A4-A2", "A5-A2", "A4-A3", "A5-A3", "A5-A4")

data.frame(combo, lower, upper)