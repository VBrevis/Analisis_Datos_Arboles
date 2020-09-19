library("C50")
library("caret")
library("lattice")
library("ggplot2")
library("dplyr")


# Se cargan los datos
datos <- read.csv('C:/Users/Will/Desktop/U/Analisis/Lab 4/Analisis_Datos_Arboles/bank-additional.csv', sep = ";")
#datos <- read.csv('C:/bank-additional.csv', sep = ";")


# Se borran registros con datos desconocidos del conjunto de datos
datos.dis <- datos
datos.dis <- datos.dis[apply(datos.dis!="unknown", 1, all),] %>% droplevels()

# Se borran variables irrelevantes
datos.dis <- dplyr::select(datos.dis, -job, -age, -marital, -education, -default,  -housing,
                           -loan, -day_of_week, -pdays, -previous, -euribor3m, -nr.employed)


# Discretización de las variables cuantitativas
#Duracion de la llamada#
datos.dis[,"duration"] <- cut(datos.dis$duration, breaks = c(-1, 60, 180, 480, 600, 4000), 
                              labels = c("very.short", "short", "average", "long", "very.long"))

#Número de contactos realizados durante la campaña actual#
datos.dis[,"campaign"] <- cut(datos.dis$campaign, breaks = c(-1, 8, 16, 24, 32, 40), 
                              labels = c("less.than.eight", "nine.to.sixteen", "seventeen.to.twentyfour",
                                         "twentyfive.to.thrirtytwo", "more.than.thrirtythree"))

#Tasa de variación del empleo#
datos.dis[,"emp.var.rate"] <- cut(datos.dis$emp.var.rate, breaks = c(-4.00, -3.00, 0.00, 0.18, 1.30, 2.30), 
                                  labels = c("very.negative", "negative", "average", "positive", "very.positive"))

#IPC
datos.dis[,"cons.price.idx"] <- cut(datos.dis$cons.price.idx, breaks = c(0.00, 92.00, 94.00, 120.00), 
                                    labels = c("decrease", "remained", "increase"))

#ICC#
datos.dis[,"cons.conf.idx"] <- cut(datos.dis$cons.conf.idx, breaks = c(-51.00, -35.00, -30.00, -20.00, -15.00, 0.00), 
                                   labels = c("very.high", "high", "normal", "low", "very.low"))


# Se transforman los datos cualitativos a factor
datos.dis$contact <- as.factor(datos.dis$contact)
datos.dis$month <- as.factor(datos.dis$month)
datos.dis$poutcome <- as.factor(datos.dis$poutcome)
datos.dis$y <- as.factor(datos.dis$y)


# Semilla
#set.seed(012)
set.seed(2021)
training.index <- createDataPartition(datos.dis$y, p=0.7)$Resample1
training.set <- datos.dis[training.index, ]
test.set <- datos.dis[-training.index, ]
tree <- C5.0(y ~ ., training.set)

# Álbol
plot(tree)
summary(tree)

# Reglas
tree.rules <- C5.0(x = training.set[, -9], y = training.set$y, rules = T)
summary(tree.rules)

# Predicción
tree.pred.class <- predict(tree, test.set[, -9], type = "class")
tree.pred.prob <- predict(tree, test.set[, -9], type = "prob")
head(tree.pred.prob)

# Matriz de confusión
conf.matrix.tree <- confusionMatrix(table(test.set$y, tree.pred.class))
print(conf.matrix.tree)
