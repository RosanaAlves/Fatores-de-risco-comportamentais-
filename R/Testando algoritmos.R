#install.packages("caret")
library(caret)

indice_treino = createDataPartition(y=dados$Diabetes, p=0.7, list=FALSE)
treino = dados[indice_treino, ]
teste = dados[-indice_treino, ]
