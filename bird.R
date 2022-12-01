rm(list = ls())

bird <- read.csv("./Data/bird_allometry.csv")
blossom <- read.csv('./Data/blossoms.csv')
butterfly <- read.csv('./Data/butterflies.csv')
dormancy <- read.csv('./Data/dormancy.csv')
eulaema <- read.csv('./Data/Eulaema.csv')


## Bird
summary(bird)
bird$Genus_Species <- as.factor(bird$Genus_Species)
bird$Sex <- as.factor(bird$Sex)
summary(bird)

plot(log(bird$brain_mass), 
     log(bird$body_mass), 
     col = as.numeric(bird$Sex))



m <- lm(log(bird$body_mass)~log(bird$brain_mass))
x_hat <- seq(log(min(bird$brain_mass)), log(max(bird$body_mass)), length.out = 2500)
y_hat <- summary(m)$coef[1,1] + summary(m)$coef[2,1]*x_hat

lines(x_hat, y_hat)










