rm(list = ls())

butterflies <- read.csv('./Data/butterflies.csv')


summary(butterflies)
butterflies$LarvalHost <- as.factor(butterflies$LarvalHost)
butterflies$Sex <- as.factor(butterflies$Sex)
butterflies$MaternalHost <- as.factor(butterflies$MaternalHost)
summary(butterflies)
butterflies$MotherID <- as.factor(butterflies$MotherID)


plot(butterflies$LarvalHost, butterflies$DevelopmentTime)
plot(butterflies$LarvalHost, butterflies$AdultWeight)
plot(butterflies$LarvalHost, butterflies$GrowthRate)

plot(butterflies$GrowthRate, butterflies$AdultWeight, 
     col = as.numeric(butterflies$LarvalHost), 
     pch = as.numeric(butterflies$MaternalHost))

plot(as.numeric(butterflies$LarvalHost), butterflies$DevelopmentTime, 
     las = 1, 
     xlab = 'Larval host', 
     ylab = 'Developing time (w)',
     type = 'p', 
     col = 'grey',
     xaxt = 'n',
     yaxt = 'n',
     xlim = c(0.5,2.75))

axis(1, 1:2, labels = levels(butterflies$LarvalHost))
means <- tapply(butterflies$DevelopmentTime, butterflies$LarvalHost, mean)
points(1:2, means, pch = 16, col = 'black')

par(new = T)
plot(butterflies$LarvalHost, butterflies$DevelopmentTime, at = c(1.39, 2.2), boxwex = 0.1,
     xaxt = 'n', yaxt = 'n',
     las = 1,
     xlab = '',
     ylab = '')

m <- lm(butterflies$DevelopmentTime~butterflies$LarvalHost)
summary(m)

anova(m)

ss_t <- sum(anova(m)$Sum) / (287-1)
var(butterflies$DevelopmentTime)

r2 <- anova(m)$Sum[1]/ss_t

effect_size <- summary(m)$coef[2]/summary(m)$coef[1]*100

m2 <- lm(butterflies$DevelopmentTime~butterflies$LarvalHost-1)
summary(m2)
confint(m2)


library(glmmTMB)
m <- glmmTMB(LarvalHost ~ 1 + (1|MotherID), data = butterflies)
summary(m)

VarAmongGroups = attr(VarCorr(m)$cond$MotherID, "stddev")^2
VarWithinGroups = attr(VarCorr(m)$cond, "sc")^2

# Explain variance by groups
VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100

CV2_Among = VarAmongGroups/mean(butterflies$DevelopmentTime)^2
CV2_Within = VarWithinGroups/mean(butterflies$DevelopmentTime)^2
CV2_Total = CV2_Among + CV2_Within


plot(butterflies$GrowthRate, butterflies$AdultWeight, 
     col = as.numeric(butterflies$LarvalHost), 
     pch = as.numeric(butterflies$MaternalHost))

