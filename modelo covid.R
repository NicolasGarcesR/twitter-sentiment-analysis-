library(tidyverse)
library(readxl)
library(gmodels)
library(glmnet)
library(MASS)

###############################################
######### MODELO COVID TWITTER ################
###############################################



# APROXIMACION A LOS DATOS ------------------------------------------------

base<-COVID_final
base<- base[,-c(1,2,13,16)] # se elimina la columna 13 porque no cambia para ninguna observacion y la 16 por exogeneidad.
base$twitter_sentiment <- as.numeric(base$twitter_sentiment)
view(base)
hist(base$twitter_sentiment)
summary(base$twitter_sentiment)
var(base$twitter_sentiment)#0.0344552
sd(base$twitter_sentiment)#0.1856211
# El analisis de sentimiento tiene una distrubucion normal con media -0.44.
# los datos varian entre -1 y 0, con 2 datos en la parte positiva
# Los datos tienen muy poca varianza, lo cual es problematico.

#tests,confirmed,recovered y deaths tienen mucha varianza, por ello normalizamos tests
# y aplicamos logaritmo a los demas. Se manipula de forma diferenciada el tests porque es
# la unica de stas variables con datos iguales a 0
normalize <- function(x)
{
  return((x-min(x))/ (max(x)-min(x)))
}

base$tests<-normalize(base$tests)
base$confirmed<- log(base$confirmed)
base$recovered<- log(base$recovered)
base$deaths<- log(base$deaths)

pairs(base[,c(2,3,4,13)])
# La variable objetivo no parece tener correlacion con las variables explicativas principales.
pairs(base[,c(5:13)])
# tampoco parece tener correlacion significativa con las variables de stringency-index 

# CREACION DE GRUPOS TEST Y TRAIN -----------------------------------------

set.seed(2020)
train<- sample(193,155)
base_train<- base[train,]
base_test <- base[-train,]
covid_train<- base_train#[,c(2,3,4,8,9,10,13,14)]# EX ANTE SE DETERMINAN ESTAS VARIABLES
covid_test<- base_test#[,c(2,3,4,8,9,10,13,14)]
covid_lineal<-base#[,c(2,3,4,8,9,10,13,14)]
view(covid_train)


# MODELO LINEAL -----------------------------------------------------------

m_lineal<- lm(twitter_sentiment ~ . , data= covid_lineal)
summary(m_lineal)

# La variables "recovered" y "international_movement_restrictions", con un intervalo de confianza del 5%, 
# son la unica variable con poder explicativo significativo sobre los tweets. A pesar de ello, "Recovered"
# se relaciona de forma negativa con el sentimiento de los tweets, lo que es algo conraintuitivo.Esto implica
#que entre mas recuperados, los tweets seran mas negativos. La variable "international_movement_restrictions" se 
# relaciona de forma positiva (entre menos restricciones internacionales de mobilidad los tweets son mas positivos).
# El modelo explica un 25% de la varianza de los sentimientos de los tweets. tiene un R ajustado del 20%.

# Para evaluar el modelo se crea la funcion de Error Absoluto Medio
MAE <- function(actual, predicted){ 
  mean(abs(actual-predicted))
}
####

lin_pred<- predict(m_lineal,data=covid_test)
MAE(lin_pred,covid_lineal$twitter_sentiment)

# El Error absoluto medio es de 0.125. Si se tiene en cuenta que la varianza es de 0,03,
# el modelo no tiene un gran poder para pronosticar los datos. De todas formas, la distancia
# promedio entre lo pronosticado y el dato real esta a menos de una desviacion estandar, lo que significa
# que lo que limita el poder explicativo del modelo son los datos.



# REGRESION TREE ----------------------------------------------------------
library(rpart)
library(rpart.plot)


arbol<- rpart(twitter_sentiment ~ ., data=covid_train)
summary(arbol)
# Las variables mas importantes del arbol son: tests, deaths, confirmed, recoverd y testing policy.
# de todas formas, ninguna de estas tiene una importancia significativa (ninguna pasa del 20%)
rpart.plot(arbol)
# El arbol hace splits en tests, deaths, testing policy, confirmed y recovered
# El arbol tiene 8 nodos finales con distribuciones muy homogeneas , lo que indica que 
# el MAE puede ser grande dada la similitud entre las observaciones.
arbol_pred <- predict(arbol,covid_test)

cor(arbol_pred,covid_test$twitter_sentiment)
#La correlacion del pronostico es grande, pero no necesariamente es algo bueno.
# La poca varianza de los datos hace que esta medida no sea muy diciente de la capacidad
# predictiva del modelo. Si los datos estan muy agrupados, la estimacion sera muy cercana 
# a esos valores, pero no porque logre explicar cada variable, sino porque estadisticamente 
# las predicciones tendran mas oportunidad de acertar si se encuentran en ese rango. 



MAE(arbol_pred,covid_test$twitter_sentiment)
# El error absoluto medio es 0.12909
# presenta sus mismos problemas y limitaciones del MAE anterior.


# CROSS VALIDATION & REGRESION TREE ---------------------------------------
library(caret)
library(pls)
library(Cubist)
set.seed(2020)
ctrl <- trainControl(method = "cv", number=10)

## REGRESION LINEAL CON CROSS-VALIDATION ##

## MODELO LINEAL CON CROSS-VALIDATION##

lm_cv <- train(twitter_sentiment ~ ., data = covid_train,metric = "RMSE", method = 'lm', trControl = ctrl)
summary(lm_cv)
# Las variables significativas del modelo lineal normal siguen siendo las mismas con cross validation
# De todas formas "international_movement_restrictions" pasa de ser significativa del 5% al 10%
lm_cv_pred <- predict(lm_cv,covid_test)
MAE(lm_cv_pred,covid_test$twitter_sentiment)
#Hacer cross-validation para la regresion lineal no presenta ninguna mejora en el modelo. El MAE
# pasa a ser de 0.133, peor que en el modelo lineal normal.




## ARBOL DE REGRESION CON CROSS-VALIDATION##

Rt_cv <- train(twitter_sentiment ~ ., data = covid_train,metric = "RMSE", method = 'rpart', trControl = ctrl)
summary(Rt_cv)
# la importancia de las variables aumenta un poco y el orden de importancia se mantiene constante
Rt_cv_pred <- predict(Rt_cv,covid_test)
cor(Rt_cv_pred,covid_test$twitter_sentiment)
# la correlacion cae un poco, pero ya se sabe que esta medida no es muy diciente.
MAE(Rt_cv_pred,covid_test$twitter_sentiment)
# Hacer cross-validation para el arbol de regresion tampoco presenta ninguna mejora. El MAE
# empeora y pasa a ser 0.1367.





## ARBOL DE REGRESION ALTERNATIVO CON CROSS-VALIDATION ##

# se utiliza cubist para hacer el cubist model tree, la diferencia metodologica con el arbol de regresion es que 
#las regresiones realizadas en cada nodo están suavizadas teniendo en cuenta las predicciones de nodos anteriores. 
#Además, realiza regresiones entre nodos.Se incluyo este último modelo para tratar de mejorar la estimación 
#del árbol de regresiones dada la poca varianza de la variable explicativa.
arbol_cv <- train(twitter_sentiment ~ ., data = covid_train,metric = "RMSE", method = 'cubist', trControl = ctrl)
summary(arbol_cv)
# el error promedio de los 10 arboles fue de 0.15 y el error relativo es 1.07
# testing_policy es la variable explicativa con la que los splits crea grupos mas homogeneos.

arbol_cv_pred <- predict(arbol_cv,covid_test)
cor(arbol_cv_pred,covid_test$twitter_sentiment)
# correlacion del 0.5437
MAE(arbol_cv_pred,covid_test$twitter_sentiment)
# El MAE de este modelo es 0.1223426, por lo cual, el cubist model tree es modelo
# con el mejor pronostico de todas. a pesar de ello, no dista mucho de los otros modelos
# y sigue teniendo los mismos problemas ya mencionados.


