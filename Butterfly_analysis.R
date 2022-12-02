#!/usr/bin/env Rscript

library(glmmTMB)

# Remove global variables in the envionment
rm(list = ls())

# Imoprt Butterfly data
butterflies <- read.csv('./Data/butterflies.csv',
                        header = T,
                        as.is = 1)

# Plot data
plot(butterflies$GrowthRate, butterflies$AdultWeight, 
     col = as.numeric(butterflies$LarvalHost), 
     pch = as.numeric(butterflies$MaternalHost))

plot(butterflies$DevelopmentTime, butterflies$AdultWeight, 
     col = as.numeric(butterflies$LarvalHost))



##### Adult weigth ####
# Perform an anova
# Linear model to be used in the anova
m <- lm(AdultWeight~LarvalHost*MaternalHost, data = butterflies)

# Perform the anova
anova(m)
summary(m)


# Illustrating that the maternal host plant do not significantlly affect the adult weight
# And illustrating that the larval host plant greatly affects the adult weight
dat <- butterflies

# Separate the names based on larval or mother host plant
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")

# Get means for adult weight for differnt combinations of host plants
means = tapply(dat$AdultWeight, list(dat$MaternalHost, dat$LarvalHost), mean)

# Get sum of squared errors
ses = tapply(dat$AdultWeight,
             list(dat$MaternalHost, dat$LarvalHost),
             function(x) sd(x)/sqrt(sum(!is.na(x))))

# Plot adult weight over larval host plant
# Empty plot
plot(c(0.97, 1.03), means[,1], ylim=c(45, 75), xlim=c(0.8, 2.2),
     xlab="Larval host",
     ylab="AdultWeight",
     xaxt="n", las=1, pch=c(21,16), col="white")

# Add x-axis lables
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))

# Add error bars for Barbarea as larval host
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03),
       means[,1]+ses[,1], length=0.05, angle=90, code=3)

# Add error bars for Berteroa as larval host
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03),
       means[,2]+ses[,2], length=0.05, angle=90, code=3)

# Add line between goups with same maternal host plant
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])

# Add points representing the groups means
points(c(0.97, 1.03), means[,1], pch=c(1,1), col = c('black', 'red'))
points(c(1.97, 2.03), means[,2], pch=c(1, 1), col = c('black', 'red'))

# Add description of colors in the plot
legend("topleft", c("Maternal host", "Barbarea", "Berteroa"),
       bty="n", pch=c(NA,1,1), col = c('red', 'black'))


##### Random mixed model. Random variable = Larval Host ####
# Plot adult weight against growth rate
plot(butterflies$GrowthRate, butterflies$AdultWeight,
     col = as.numeric(butterflies$LarvalHost),
     ylab = 'Adult Weight (mg)',
     xlab = 'Growth Rate')

# Add description of colors (larval host)
legend("topleft", c("Larval host", "Barbarea", "Berteroa"),
       bty="n", pch=c(NA,1,1), col = c('red', 'black'))

# Fit model of growth rate as a predictor of adult weight
m2 <- lm(AdultWeight~GrowthRate, data = butterflies)

# Define new x values in the span of the data
xx <- seq(min(butterflies$GrowthRate), max(butterflies$GrowthRate), length.out = 1000)

# Predict new values based on the new x values
y_hat <- predict(m2, newdata = list(GrowthRate=xx), se.fit = T, type = 'response')

# Fit line to plotted data
lines(xx, y_hat$fit)


##### Linear mixed model ####
m <- glmmTMB(AdultWeight~1 + (1|MotherID), data = butterflies)
summary(m)

VarAmongGroups = attr(VarCorr(m)$cond$MotherID, "stddev")^2
VarWithinGroups = attr(VarCorr(m)$cond, "sc")^2
df = data.frame(vari = 'Adult Weight (mg)',
                Mean = round(mean(butterflies$AdultWeight),2), 
                SD = round(sd(butterflies$AdultWeight), 2),
                Among = round(VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100, 2),
                Within = round(VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100, 2))
df = apply(df, 2, round, 2)
df
names(df) <- c('variable', 'Mean','SD', 'Among (%)', 'Within (%)')
rownames(df) <- NULL




