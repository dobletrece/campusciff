housetrain <- read.csv('house_train.csv')
str(housetrain)
head(housetrain)
summary(housetrain)

# Analizar el efecto de la superficie de la vivienda en el precio de la misma

#Efecto marginal
plot(housetrain$sqft_living, housetrain$price)
mod_level <- lm(price~sqft_living, data=housetrain)
summary(mod_level)
dev.off()
png("análisis residuos mod_level.png")
par(mfrow=c(2,2))
plot(mod_level$residuals)
smoothScatter(mod_level$residuals)
hist(mod_level$residuals)
qqnorm(mod_level$residuals); qqline(mod_level$residuals, col=2)
dev.off()
confint(mod_level, level = 0.95)

#Semi-elasticidad
plot(housetrain$sqft_living, log(housetrain$price))
mod_log_lev <- lm(log(price)~sqft_living, data=housetrain)
summary(mod_log_lev)
dev.off()
png("análisis residuos mod_log_lev.png")
par(mfrow=c(2,2))
plot(mod_log_lev$residuals)
smoothScatter(mod_log_lev$residuals)
hist(mod_log_lev$residuals)
qqnorm(mod_log_lev$residuals); qqline(mod_log_lev$residuals, col=2)
dev.off()
confint(mod_log_lev, level = 0.95)

#Elasticidad
plot(log(housetrain$sqft_living), log(housetrain$price))
mod_log <- lm(log(price)~log(sqft_living), data=housetrain)
summary(mod_log)
dev.off()
png("análisis residuos mod_log.png")
par(mfrow=c(2,2))
plot(mod_log$residuals)
smoothScatter(mod_log$residuals)
hist(mod_log$residuals)
qqnorm(mod_log$residuals); qqline(mod_log$residuals, col=2)
dev.off()
confint(mod_log, level = 0.95)

#Comparación de modelos
AIC(mod_level)
AIC(mod_log_lev)
AIC(mod_log)


BIC(mod_level)
BIC(mod_log_lev)
BIC(mod_log)

