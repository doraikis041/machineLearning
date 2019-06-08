#Librerias Principales
library(class)
library(e1071)
#Tree
library(rpart)
library(rpart.plot)
library(dplyr)
#RandomForest
library(randomForest)

#Definiendo el valor de see para los datos aleatorios generados
set.seed(117)

#Variables Generales
nTrain <- 37445
k <- 5

### Grilla para visualizacion
dfGridMetada <- function(min = 0, max = 5, by = 0.1){grid_df <- data.frame(x = seq(min, max, by))}

# Carga de los datos principales con las ETL generales para todos los algoritmos
loadData <- function()
{
  T0 <- read.csv(file = "Dataset/dataset.csv")
  T0$CustomerID <- NULL
  T0$ServiceArea <- NULL
  df <- na.omit(T0)
}

#formulas
h.formula <- c('as.factor(Churn) ~ ThreewayCalls',
               'as.factor(Churn) ~ CreditRating',
               'as.factor(Churn) ~ Occupation + MaritalStatus')

print("Cargado todas las librerias y transformaciones inciales")