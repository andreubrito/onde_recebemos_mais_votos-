
# Introdução --------------------------------------------------------------


#  Autor: André Brito
#  Título: Onde recebemos mais votos?
#  Curso-r



# Carregando os pacotes ---------------------------------------------------

library(tidyverse)
library(electionsBR)
library(fs)



# Importando os dados -----------------------------------------------------

dados_tse_2020 <- vote_mun_zone_local(year = 2020)

fs::dir_create("data") # Pasta de dados de trabalho
fs::dir_create("data-raw") # Pasta de dados brutos

readr::write_rds(dados_tse_2020, "data-raw/dados_tse_2020.rds") # Salvando a base


# Tratando a base ----------------------------------------------------------------


resultado_2020 <- readr::read_rds("data-raw/dados_tse_2020.rds")

resultado_2020 <-  dplyr::filter(resultado_2020, DESCRICAO_CARGO == "Prefeito" &
                           NUM_TURNO == 1) # Filtrando apenas prefeitos e primeiro turno


# Agregando os votos totais para prefeito             


resultado_2020_1 <- dplyr::select(resultado_2020, SQ_CANDIDATO, TOTAL_VOTOS)


resultado_2020_2 <- aggregate(TOTAL_VOTOS ~ SQ_CANDIDATO, 
                              data = resultado_2020_1, FUN = sum, drop = FALSE)

colnames(resultado_2020_2) <- c("SQ_CANDIDATO", "VOTOS")


# Juntando as bases


prefeitos_2020 <- read_rds("data-raw/prefeitos_2020.rds") #Carregando a base prefeitos_2020

df_2020 <- merge(
  x = prefeitos_2020, y = resultado_2020_2, by = "SQ_CANDIDATO"
  ) # Juntando


# Salvando a base ---------------------------------------------------------

readr::write_rds(df_2020, "data-raw/df_2020.rds")
rm(list = ls()) 
