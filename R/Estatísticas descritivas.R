
# carregando pacotes ------------------------------------------------------

library(tidyverse)
# install.packages("googlesheets4")

# -------------------------------------------------------------------------


# carregando o arquivo ----------------------------------------------------

dados <- read.csv("data/health_data.csv")

# Posso extrair direto do site
# Pacote readr

#teste <- read_csv2("https://www.kaggle.com/datasets/prosperchuks/health-dataset?select=health_data.csv")
# -------------------------------------------------------------------------

str(dados)
glimpse(dados)
summary(dados)


