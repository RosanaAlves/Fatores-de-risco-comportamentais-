
# carregando pacotes ------------------------------------------------------
library(usethis)
library(tidyverse)
library(httr)

# -------------------------------------------------------------------------


# carregando o arquivo ----------------------------------------------------

dados <- read.csv("data/health_data.csv")

# Posso extrair direto do site
# Pacote readr
#teste <- read_csv2("https://www.kaggle.com/datasets/prosperchuks/health-dataset?select=health_data.csv")

# -------------------------------------------------------------------------

str(dados)
glimpse(dados)
table(dados$Diabetes)
