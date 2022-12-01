alpine <- read.csv('./Data/alpineplants.csv')

colnames(alpine)
sub <- subset(alpine, subset = min_T_winter != 'NA')
sub <- subset(sub, subset = Carex.bigelowii != 0.5)



y <- sub$Carex.bigelowii
x <- sub$min_T_winter
m <- glm(y~x, family = 'poisson')

xx <- seq(min(x), max(x), length.out = 2000)

y_hat <- predict(m, 
                 newdata = list(x = xx), 
                 type = 'response' ,se.fit = T)


plot(x, 
     y, 
     xlab = 'Minimum temperature (C) winter', 
     ylab = 'Number of plants')

lines(xx, y_hat$fit)

polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE)
