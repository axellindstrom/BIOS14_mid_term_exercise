#!/usr/bin/env Rscript


# Effects on adult weight and growth rate.
# 1. is there a differnce between different larval host plants?
# 2. is there a difference between sexes?
# 3. difference between maternal host plants?

# Remove global variables in the envionment
rm(list = ls())

# Imoprt Butterfly data
butterflies <- read.csv('./Data/butterflies.csv',
                        header = T,
                        as.is = 1)

str(butterflies)

# Plot data
plot(butterflies$GrowthRate, butterflies$AdultWeight, 
     col = as.numeric(butterflies$LarvalHost), 
     pch = as.numeric(butterflies$MaternalHost))


plot(butterflies$DevelopmentTime, butterflies$AdultWeight, 
     col = as.numeric(butterflies$LarvalHost))



##### Adult weigth ####
# Plot adult weight against sex.
plot(butterflies$Sex, butterflies$AdultWeight, ylab = 'Adult weight', las = 1)

# Perform an anova
m <- lm(AdultWeight~LarvalHost*MaternalHost*GrowthRate, data = butterflies)
anova(m)

# no group mean is different from the others.
# The difference between the sexes couldt not be determined to be statisticaly significant
# Same with maternal host.
# Therfore the sex and maternal host will not be regarded to have an effect ont he adult weight.
summary(m)





# Illustrating that the maternal host plant do not significantlly affect the adult weight
dat <- butterflies
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")
means = tapply(dat$AdultWeight, list(dat$MaternalHost, dat$LarvalHost), mean)
ses = tapply(dat$AdultWeight,
             list(dat$MaternalHost, dat$LarvalHost),
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means

plot(c(0.97, 1.03), means[,1], ylim=c(45, 75), xlim=c(0.8, 2.2),
     xlab="Larval host",
     ylab="AdultWeight",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03),
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03),
       means[,2]+ses[,2], length=0.05, angle=90, code=3)
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])
points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Maternal host", "Barbarea", "Berteroa"),
       bty="n", pch=c(NA,21,16))


# Subset data, only maternal host = Berteroa
LBerteroa <- subset(butterflies, butterflies$LarvalHost == 'Berteroa')
# Perform a anova to evaluate the effect on adult weight
m <- lm(AdultWeight~MaternalHost, data = butterflies)
anova(m)


# Subset data, only larval host = Barbarea
LBarbarea <- subset(butterflies, butterflies$LarvalHost == 'Barbarea')
# Perform a anova to evaluate the effect of maternal host on adult weight
m <- lm(AdultWeight~MaternalHost, data = butterflies)
anova(m)




#### GROWTH RATE ####


# Perform an anova
m <- lm(AdultWeight~LarvalHost*MaternalHost, data = butterflies)
anova(m)

# no group mean is different from the others.
# The difference between the sexes couldt not be determined to be statisticaly significant
# Same with maternal host.
# Therfore the sex and maternal host will not be regarded to have an effect ont he adult weight.
summary(m)





# Illustrating that the maternal host plant do not significantlly affect the adult weight
dat <- butterflies
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")
means = tapply(dat$GrowthRate, list(dat$MaternalHost, dat$LarvalHost), mean)
ses = tapply(dat$GrowthRate,
             list(dat$MaternalHost, dat$LarvalHost),
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means

plot(c(0.97, 1.03), means[,1], ylim=c(0, 0.1), xlim=c(0.8, 2.2),
     xlab="Larval host",
     ylab="Growth Rate",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03),
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03),
       means[,2]+ses[,2], length=0.05, angle=90, code=3)
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])
points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Maternal host", "Barbarea", "Berteroa"),
       bty="n", pch=c(NA,21,16))


# Subset data, only maternal host = Berteroa
LBerteroa <- subset(butterflies, butterflies$LarvalHost == 'Berteroa')
# Perform a anova to evaluate the effect on adult weight
m <- lm(GrowthRate~MaternalHost, data = butterflies)
anova(m)


# Subset data, only larval host = Barbarea
LBarbarea <- subset(butterflies, butterflies$LarvalHost == 'Barbarea')
# Perform a anova to evaluate the effect of maternal host on adult weight
m <- lm(GrowthRate~MaternalHost, data = butterflies)
anova(m)


# No significant differnce between different maternal host plants.
# Test for devel time / adult weight
# maternal host effect on devel time



##### Random mixed model. Random variable = Larval Host ####
plot(butterflies$GrowthRate, butterflies$AdultWeight,
     col = as.numeric(butterflies$LarvalHost))

m <- lm(AdultWeight~GrowthRate, data = butterflies)
xx <- seq(min(butterflies$GrowthRate), max(butterflies$GrowthRate), length.out = 1000)
y_hat <- predict(m, newdata = list(GrowthRate=xx), se.fit = T, type = 'response')

lines(xx, y_hat$fit)
summary(m)



library(glmmTMB)
m <- glmmTMB(AdultWeight~GrowthRate + (1|LarvalHost), data = butterflies)
summary(m)

VarCorr(m)
VarAmongGroups = attr(VarCorr(m)$cond$LarvalHost, "stddev")^2
VarWithinGroups = attr(VarCorr(m)$cond, "sc")^2
VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100
df = data.frame(Mean = mean(butterflies$AdultWeight), SD = sd(butterflies$AdultWeight),
                Among = VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100,
                Within = VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100)
df = apply(df, 2, round, 2)
df
# weight % difference between host plants
(colMeans(means)[1]/colMeans(means)[2]-1)*100



