
# carregando pacotes ------------------------------------------------------
library(usethis)
library(tidyverse)
library(httr)
library(tibble)

# -------------------------------------------------------------------------


# carregando o arquivo ----------------------------------------------------

dados <- read.csv("data/health_data.csv")
dados <- tibble(dados)
dados$Diabetes <- as.factor(dados$Diabetes) 
dados$Sex <- as.factor(dados$Sex)
dados$HighChol <- as.factor(dados$HighChol)
dados$CholCheck <- as.factor(dados$CholCheck)
dados$Smoker <- as.factor(dados$Smoker)
dados$HeartDiseaseorAttack <- as.factor(dados$HeartDiseaseorAttack)
dados$PhysActivity <- as.factor(dados$PhysActivity)
dados$Fruits <-  as.factor(dados$Fruits)
dados$Veggies <- as.factor(dados$Veggies)
dados$HvyAlcoholConsump <- as.factor(dados$HvyAlcoholConsump)
dados$GenHlth <- as.factor(dados$GenHlth)
dados$DiffWalk <- as.factor(dados$DiffWalk)
dados$Hypertension   <- as.factor(dados$Hypertension)
dados$Stroke <- as.factor(dados$Stroke)

# dados2 <- read_delim("data/health_data.csv", ",", escape_double = FALSE, col_types = cols(Diabetes = col_factor(levels = c())))
dados2 <-dados

# Posso extrair direto do site
# Pacote readr
#teste <- read_csv2("https://www.kaggle.com/datasets/prosperchuks/health-dataset?select=health_data.csv")

# -------------------------------------------------------------------------

str(dados)
glimpse(dados)
table(dados$Diabetes)


# selecinonado mulheres ---------------------------------------------------


fem <- dados%>% filter(Sex == 0)
table(fem$Diabetes)

## Dentre as mulheres com diabetes quantas possuem colesterol tbm?

diabete_colest <- fem%>%filter(Diabetes == 1, HighChol == 1)
plot(diabete_colest$Diabetes,diabete_colest$HighChol)
12521/38386


## Ajustando modelo de regressão logistica


# estudo baseado em https://smolski.github.io/livroavancado/reglog.html

# Regressão Logística -----------------------------------------------------

# A variavel resposta é a presença ou não de diabetes, é uma variavel dependente binaria


## install.packages(c("readr","mfx","caret","pRoc",                   
##                   "ResourceSelection","modEvA","foreign","stargazer"))

summary(dados2)
require(ggplot2)

ggplot(dados2, aes(x=Age, y=Diabetes)) + 
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#glm(Y~modelo, family=binomial(link="logit"))

m1=glm(Diabetes~Age, family = binomial(link="logit"), data = dados2)
summary(m1)
m1

dados2$PRED=predict(m1, type="response")

ggplot(dados2, aes(x=Age, y=PRED)) + 
  geom_point()

## Estimando a razão das chances

require(mfx)
logitor(m1,data = dados2)

# resultado da razão de chances da variável Age foi de 1.2349582, 
# o que pode assim ser interpretado: para cada variação unitária na idade (Age),
# as chances de ocorrência de Diabetes aumentam 1.2349582 vezes

## Determinando o intervalo de confiança

exp(cbind(OR=coef(m1), confint(m1)))

# razão das chances (OR - Odds Ratio em inglês) para as variáveis independentes.

## predição de probabilidades

media = data.frame(Age=mean(dados2$Age))
media

media$pred.prob = predict(m1, newdata=media, type="response")
media

## Matriz de confusão

# Definir ponto de corte para classificação usualmente é 0.5

# Precisão: representa a proporção das predições corretas do modelo sobre o total(ACC)
# Sensibilidade: representa a proporção de verdadeiros positivos (SENS)
# Especificidade: a proporção apresentada dos verdadeiros negativos (SENS)
# Verdadeiro Preditivo Positivo: se caracteriza como proporção de verdadeiros positivos com relação ao total de predições positivas (VPP)
# Verdadeiro Preditivo Negativo: se caracteriza pela proporção de verdadeiros negativos comparando-se com o total de predições negativas. (VPN)

require(caret)


dados2$pdata <- as.factor(
  ifelse(
    predict(m1, 
            newdata = dados2, 
            type = "response")
    >0.5,"1","0"))

confusionMatrix(dados2$pdata, dados2$Diabetes, positive="1")

# A matriz de confusão retoma a acurácia total do modelo em 62%,
# sendo que o modelo consegue acertos de 60,3% na predição de valores positivos ou dos “eventos” e
# 63,38 % na predição de valores negativos ou os “não eventos”.

## A Curva ROC (Receiver Operating Characteristic Curve) associada ao modelo logístico mensura a capacidade de predição do modelo proposto, através das predições da sensibilidade e da especificidade.
require(pROC)
roc1=plot.roc(dados2$Diabetes,fitted(m1))
plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)

## O teste Hosmer e Lemeshow

# sugerem um tamanho de amostra de pelo menos 50 casos para a realização deste teste.
#A hipótese nula H\({_0}\) do qui-quadrado (p=0,05) deste teste é a de que as proporções observadas e esperadas são as mesmas ao longo da amostra. Abaixo segue a estrutura do teste, sendo que o modelo apresenta dificuldade de ajuste em função de que rejeita a hipótese nula a p=0,05.


require(ResourceSelection)

hl <- hoslem.test(dados2$Diabetes,fitted(m1),g=10)

hl

## Pseudo R²

# install.packages("modEvA")

# require(modEvA)
 # RsqGLM(m1)


## Regressão Logística Múltipla

require(haven)
logit=glm(Diabetes~Age+Sex+HighChol+CholCheck+BMI+Smoker+HeartDiseaseorAttack+PhysActivity+Fruits+Veggies+HvyAlcoholConsump+GenHlth+MentHlth+PhysHlth+DiffWalk+Hypertension+Stroke, data = dados2, family = binomial(link="logit"))
summary(logit)


#Observa-se que os valores estimados mostram os coeficientes em formato logarítmo de chances.
# Assim, quando Cholcheck eleva-se em 1 (uma) unidade, o log das chances esperado para Cholcheck altera-se em 1.31.

require(stargazer)
stargazer(logit, title="Resultados",type = "text")
# 
# A razão de chances (OR - odds ratio em inglês) estimada
# no modelo terá de ser transformada por estar apresentada na forma 
# logarítma conforme o modelo de regressão logística o estima. Assim,
# utiliza-se o pacote mfx para efetuar esta transformação para todo o 
# modelo de forma automatizada

require(mfx)
logitor(Diabetes~Age+Sex+HighChol+CholCheck+BMI+Smoker+HeartDiseaseorAttack+PhysActivity+Fruits+Veggies+HvyAlcoholConsump+GenHlth+MentHlth+PhysHlth+DiffWalk+Hypertension+Stroke, data = dados)
# a chance de Diabetes=1 é 0,753 vezes maior quando Fruits aumenta em uma unidade (sendo que aqui mantêm-se as demais variáveis independentes constantes).

#Para facilitar a interpretação do modelo, se torna mais fácil depois 
# de transformado a sua exponenciação dos coeficientes logísticos utilizando 
# o comando exp(coef(logit)). Desta forma, para cada incremento unitário em cholchek 
# e mantendo as demais variáveis constantes, conclui-se que é 5,26 vezes provável que y seja igual a 1 em oposição a não ser (igual a zero), ou seja, as chances aumentam em 426%.
exp(coef(logit))

# O intervalo de confiança do modelo pode ser exposto utilizando o comando confint para os coeficientes estimados, como segue abaixo:
exp(cbind(OR=coef(logit), confint(logit)))

























## Prediçoes

allmean = data.frame(CholCheck=mean(dados2$CholCheck),
                     MentHlth=mean(dados2$MentHlth),
                     Sex=mean(dados2$Sex),
                     Age=mean(dados2$Age),
                     Fruits=mean(dados$Fruits))
allmean
allmean$pred.prob = predict(logit, newdata=allmean, type="response")
allmean

# Como resultado, o modelo informa que constando os valores médios das variáveis independentes, obtêm-se a probabilidade de 49% em y se constituir igual a 1.


## Método Stepwise

# O método Stepwise auxilia o pesquisador em selecionar as variáveis importantes ao modelo, sendo que podem ser utilizadas nas direções “both”, “backward”, “forward”. Este método, por sua vez, utiliza o Critério de Informação de Akaike (AIC - Akaike Information Criterion) na combinação das variáveis dos diversos modelos simulados para selecionar o modelo mais ajustado. Quanto menor o AIC, melhor o ajuste do modelo. 
step(logit, direction = 'both')

## VIF - Variance Inflation Factor

# índice o qual não deve ficar abaixo de 10 para representar baixo problema de multicolinearidade segundo Rawlings, Pantula, e Dickey (1998).

require(faraway)
vif(logit)



