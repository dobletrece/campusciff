#Construcción del modelo predictivo
housetrain <- read.csv('house_train.csv')

#Existencia de valores nulos
sapply(housetrain, function(x) sum(is.na(x))) / nrow(housetrain)
#No hay

#Visión global de las variables
str(housetrain)
summary(housetrain)

#Eliminamos variables de tipo ID y pseudoidentificadores
housetrain$id <- NULL
housetrain$date <- NULL

#Veamos posibles correlaciones entre las variables numéricas
library(corrplot)
#cor(housetrain[,sapply(housetrain, is.numeric)])
corrmatrix <- cor(housetrain)
print(round(corrmatrix, digits = 2))
par(mfrow=c(1,1))
corrplot(corrmatrix, type="full", main="Correlation")

#Visualizamos de forma gráfica el precio y el resto de variables del dataset
par(mfrow=c(3,3))
for(i in 2:10){
  plot(housetrain[,i], housetrain$price, main=names(housetrain[i]), ylab=names(housetrain$price), xlab="", col=636)
}
par(mfrow=c(3,3))
for(i in 11:19){
  plot(housetrain[,i], housetrain$price, main=names(housetrain[i]), ylab=names(housetrain$price), xlab="", col=636)
}

#Transformación de variables
housetrain$zipcode <- as.factor(housetrain$zipcode)
#housetrain$grade <- as.factor(housetrain$grade)
housetrain$waterfront <- as.factor(housetrain$waterfront)
#housetrain$floors <- as.factor(housetrain$floors)
#housetrain$bedrooms <- as.factor(housetrain$bedrooms)
housetrain$yr_renovated <- ifelse(housetrain$yr_renovated > 0, 1, 0)
housetrain$condition <- as.factor(housetrain$condition)
housetrain$view <- as.factor(housetrain$view)

#Vamos a hacer un split en housetrain para entrenamiento y validación, 
#usando housetest sólo como versión final de testing
set.seed(1234)
sample <- sample.int(n=nrow(housetrain), size = floor(0.80*nrow(housetrain)), replace = F)
h_train <- housetrain[sample, ]
h_valid  <- housetrain[-sample, ]

#Antes queremos averiguar si nos podemos enfrentar a un problema de multicolinealidad entre variables
#Creamos un modelo con todas las variables para calcular el VIF entre ellas
previo <- lm(price ~ ., data = h_train)
summary(previo)
#Nos advierte que hay 1 coeficiente no definido debido a singularidades
library(car)
vif(previo) 
# Error: aliased coefficients in the model, parece indicar que una variable tiene colinealidad total
#Vemos de qué dimensión se trata
attributes(alias(previo)$Complete)$dimnames[[1]]
#[1] "sqft_basement" (aparece como NA en el summary del modelo)

#Procedemos a eliminarla y calculamos nuevamente el VIF
previo <- lm(price ~ .-sqft_basement, data = h_train)
summary(previo)
vif(previo)

#Variables con VIF > 10 : zipcode, lat y long
#Deberíamos en este caso eliminar la mayor (zipcode) pero creo que como factor
#indicador de un "distrito" nos puede ayudar más que lat y long (cuya combinación es 
#un valor único) en la predicción. Probamos a eliminar lat y long y calculamos el VIF
previo <- lm(price ~ .-sqft_basement -zipcode, data = h_train)
summary(previo) #RSE 206300 Adj R-sq 0.6974
vif(previo)
AIC(previo) #[1] 379849.1


previo <- lm(price ~ .-sqft_basement -lat -long, data = h_train)
summary(previo) #RSE 164900 Adj R-sq 0.8065
vif(previo)
AIC(previo) #[1] 373694.9

#Elegimos este segundo modelo como punto de partida, con menor RSE, mejor R-cuadrado ajustado y un menor AIC para empezar a modelar

#Queda eliminada la multicolinealidad gracias a las variables descartadas

#Consideraciones:

#La idea es crear una serie de modelos donde podamos observar una disminución del error y del AIC, un aumento en 
#el R-cuadrado ajustado sin llegar a realizar un modelo demasiado flexible/complejo incapaz de generalizar

#sqft-above y sqft-basement sumados son igual al sqft_living por lo que nos quedaremos con éste último
#más correlacionado con la variable objetivo para modelar

#Durante la parte de análisis vimos que el modelo con menor error y AIC que explicaba la relación era el log-log

#Grade, viendo la distribución en función del precio, parece un buen predictor aún desconociendo su significado.

#Añadimos también el zipcode como factor para incluir un distribución de precios según la localización

mod <- lm(log(price) ~ log(sqft_living) + grade + zipcode, data = h_train)
summary(mod) #RSE 0.2081 Adj R-sq 0.8451
vif(mod)
AIC(mod) #[1] -4114.464

#R2_h_train
#[1] 0.7902934
#R2_h_valid
#[1] 0.7951704

#Probamos a añadir view (previamente convertido en factor) que podría ayudar a clasificar los tipos de vivienda
mod <- lm(log(price) ~ log(sqft_living) + grade + zipcode + view, data = h_train)
summary(mod) #RSE 0.1944 Adj R-sq 0.8648
vif(mod)
AIC(mod) #[1] -6004.81

#R2_h_train
#[1] 0.8399973
#R2_h_valid
#[1] 0.8308553


#Creamos el modelo robusto
if (!require("MASS")){
  install.packages("MASS") 
  library(MASS)
}

mod <- rlm(log(price) ~ log(sqft_living) + grade + zipcode + view, data = h_train)
summary(mod) #RSE 0.1561 
vif(mod)
AIC(mod) #[1] -5971.674

#Train
h_train$prediction=exp(predict(mod,type = "response"))
R2_h_train=1-sum((h_train$price-h_train$prediction)^2)/sum((h_train$price-mean(h_train$price))^2)
R2_h_train #[1] 0.8373898

#Validation
h_valid$prediction=exp(predict(mod, newdata=h_valid, type = "response"))
R2_h_valid=1-sum((h_valid$price-h_valid$prediction)^2)/sum((h_valid$price-mean(h_valid$price))^2)
R2_h_valid #[1] 0.8292151

#Residuals: la inmensa mayoría entre -1 y 1, siguen una distribución aparentemente normal con media en torno a 0
plot(mod$residuals)
hist(mod$residuals)

#Test
housetest <- read.csv('house_test.csv')
str(housetest)

housetest$zipcode <- as.factor(housetest$zipcode)
housetest$view <- as.factor(housetest$view)

housetest$precioestimado=exp(predict(mod, newdata=housetest, type = "response"))
head(housetest)

write.csv(x = housetest, file = 'house_test_modificado.csv')
