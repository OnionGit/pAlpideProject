# Limpiar entorno
rm(list=ls())
library("lattice")
#Lectura de los csv's
train <- read.csv("C:/Users/Rafael/Archivos/Universidad/Ciencias/Fisica/TecnicasBazo/Proyecto/datasample2y3.dat")
train <- read.csv("C:/Users/Rafael/Archivos/Universidad/Ciencias/Fisica/TecnicasBazo/Proyecto/datasample1y3.dat")
summary(train)
# Sacar una muestra de datos para entrenamiento y para la prueba
strain <- train[sample(nrow(train), 5000, replace=FALSE),]
stest <- train[sample(nrow(train), 3000, replace=FALSE),]

strain <- strain[,-1][,-1]
#summary(stest[,-(3:5)][,-(3:4)])

stestlab <-stest[,-(3:5)][,-(3:4)]
stest<-stest[,-1][,-1]

summary(strain)

# requiere el paquete tree
# install.packages('tree')
library(tree)
set.seed(42)
# se crea el modelo en base a la data de entrenamiento
fit1 <- tree(as.factor(reg) ~ ., data=strain)
plot(fit1)
title(main="tree")
text(fit1)
# Se prueba el modelo
fit1.pred <- predict(fit1, stest, type="class")
# Tabla para ver la concurrencia con los datos reales
table(fit1.pred,stest$reg)
# El error de la prediccion sera
fit1$error <- 1-(sum(fit1.pred==stest$reg)/length(stest$reg))
fit1$error
# Grafico para comparar la distribucion de pixeles
xyplot(stestlab$fil~stestlab$col,group=stest$reg,main="Dispersion real de los pixeles" ,xlab = "Columnas",ylab = "Filas",pch=19)
xyplot(stestlab$fil~stestlab$col,group=fit1.pred,main="Prediccion de los pixeles por DT",xlab = "Columnas",ylab = "Filas",pch=19)



# requiere el paquete random forest
# install.packages('randomForest')
library(randomForest)
set.seed(42)
# se encuentra el parametro ideal, en este caso 4
mtry <- tuneRF(strain[,1:4], as.factor(strain[,5]), mtryStart=1, ntreeTry=50, stepFactor=2, improve=0.05,trace=TRUE, plot=TRUE, doBest=FALSE)
# se crea el modelo
ptm4 <- proc.time()
fit4 <- randomForest(as.factor(reg) ~ ., data=strain, importance=TRUE, ntree=100, mtry=4)
fit4.time <- proc.time() - ptm4
# se analiza cual es el peso de las distintas variables
varImpPlot(fit4)
# Ahora probamos el modelo
fit4.pred <- predict(fit4, stest, type="response")
# La concurrencia de los datos sera
table(fit4.pred,stest$reg)
# El error es:
fit4$error <- 1-(sum(fit4.pred==stest$reg)/length(stest$reg))
fit4$error
# Grafico para comparar la distribucion de pixeles
xyplot(stestlab$fil~stestlab$col,group=stest$reg,main="Dispersion real de los pixeles" ,xlab = "Columnas",ylab = "Filas",pch=19)
xyplot(stestlab$fil~stestlab$col,group=fit4.pred,main="Prediccion de los pixeles por Bosques Aleatorios",xlab = "Columnas",ylab = "Filas",pch=19)


# necesitamos el paquete gbm
#install.packages('gbm')
library(gbm)
set.seed(42)
# se crea el modelo
ptm5 <- proc.time()
fit5 <- gbm(reg ~ ., data=strain, distribution="multinomial", n.trees=2000, cv.folds=2)
fit5.time <- proc.time() - ptm5
trees <- gbm.perf(fit5)
# se prueba el modelo
fit5.stest <- predict(fit5, stest, n.trees=trees, type="response")
fit5.stest <- as.data.frame(fit5.stest)
names(fit5.stest) <- c(1,3)
fit5.stest.pred <- rep(NA,3000)
for (i in 1:nrow(stest)) {
  fit5.stest.pred[i] <- colnames(fit5.stest)[(which.max(fit5.stest[i,]))]}
fit5.pred <- as.factor(fit5.stest.pred)
# La concurrencia de los datos sera
table(fit5.pred,stest$reg)
fit5.pred <- as.character(fit5.pred)
# El error es:
fit5$error <- 1-(sum(fit5.pred==stest$reg)/length(stest$reg))
fit5$error
# Grafico para comparar la distribucion de pixeles
xyplot(stestlab$fil~stestlab$col,group=stest$reg,main="Dispersion real de los pixeles" ,xlab = "Columnas",ylab = "Filas",pch=19)
xyplot(stestlab$fil~stestlab$col,group=fit5.pred,main="Prediccion de los pixeles por BDT",xlab = "Columnas",ylab = "Filas",pch=19)
