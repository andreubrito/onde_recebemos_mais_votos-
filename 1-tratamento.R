
# Carregando a base -------------------------------------------------------

library(magrittr)
library(tidyverse)
base_2020 <- readr::read_rds("data-raw/df_2020.rds")

# Excluindo linhas de segundo turno e filtrando as variáveis de interesse

base_2020 <- base_2020 %>% 
  dplyr::filter(
    CD_SIT_TOT_TURNO == 1 | CD_SIT_TOT_TURNO == 4) %>%
  dplyr::select(
    1, 4, 7, 12:14, 18:20, 26:33, 35:36, 38:40, 42:56, 59, 62:64)



# Adicionando variáveis de interesse --------------------------------------

# Criando os objetos das regiões 

sudeste <- c("SP", "MG", "RJ", "ES")
sul <- c("RS", "PR", "SC")
centro_oeste <- c("GO", "DF", "MT", "MS")
nordeste <- c("BA", "PE", "CE", "MA", "PB", "RN", "AL", "PI", "SE")
norte <- c("PA", "AM", "RO", "AP", "RR", "TO", "AC")

# Criando a variável REGIAO

base_2020 <- base_2020 %>% dplyr::mutate(
  REGIAO = dplyr::case_when(
    SG_UF %in% sudeste ~ "SUDESTE",
    SG_UF %in% nordeste ~ "NORDESTE",
    SG_UF %in% sul ~ "SUL",
    SG_UF %in% centro_oeste ~ "CENTRO-OESTE",
    SG_UF %in% norte ~ "NORTE"
  )
)

# Criando e adicionando as variáveis IND_IDEOL e IDEOLOGIA

ideologia <- c("PSTU 0.51", 
               "PCO 0.61", 
               "PCB 0.91",
               "PSOL 1.28",
               "PCdoB 1.92",
               "PT 2.97",
               "PDT 3.92",
               "PSB 4.05",
               "Rede 4.77",
               "PPS 4.92",
               "PV 5.29",
               "PTB 6.10",
               "Avante 6.32",
               "SDD 6.50",    # Importando de forma não estruturada a tabela do índice  
               "PMN 6.88",       
               "PMB 6.96",
               "PHS 6.96",
               "MDB 7.01",
               "PSD 7.09",
               "PSDB 7.11",
               "Podemos 7.24",
               "PPL 7.27",
               "PRTB 7.45",
               "Pros 7.47",
               "PRP 7.59",
               "PRB 7.78",
               "PR 7.78",
               "PTC 7.86",
               "DC 8.11",
               "PSL 8.11",
               "Novo 8.13",
               "Progressistas 8.20",
               "PSC 8.33",
               "Patriota 8.55",
               "DEM 8.57",
               "UP 1.00") # Designação própria

ideologia <- as.data.frame(ideologia)

# Separando o data.frame em duas colunas com a função separate()

ideologia <- ideologia %>%  tidyr::separate(
  ideologia, into = c("SG_PARTIDO", "IND_IDEO"), sep = -5, remove = TRUE)

ideologia <- ideologia %>% tidyr::separate(
  IND_IDEO, into = c("x", "IND_IDEO"), sep = -4, remove = TRUE) # Retirando um espaço vazio

ideologia$x <- NULL

ideologia$IND_IDEO <- as.numeric(ideologia$IND_IDEO)


# Adequando os nomes

ideologia$SG_PARTIDO[13] <- "AVANTE"
ideologia$SG_PARTIDO[5]  <- "PC do B"
ideologia$SG_PARTIDO[9]  <- "REDE"
ideologia$SG_PARTIDO[10] <- "CIDADANIA"
ideologia$SG_PARTIDO[14] <- "SOLIDARIEDADE"
ideologia$SG_PARTIDO[21] <- "PODE"
ideologia$SG_PARTIDO[24] <- "PROS"
ideologia$SG_PARTIDO[26] <- "REPUBLICANOS"
ideologia$SG_PARTIDO[27] <- "PL"
ideologia$SG_PARTIDO[31] <- "NOVO"
ideologia$SG_PARTIDO[32] <- "PP"
ideologia$SG_PARTIDO[34] <- "PATRIOTA"


# Adicionando a variável IND_IDEO

base_2020 <- base_2020 %>% dplyr::left_join(ideologia, by = "SG_PARTIDO")


# Criando e adicionando a variável IDEOLOGIA

base_2020 <- base_2020 %>% dplyr::mutate(
  IDEOLOGIA = dplyr::case_when(
  IND_IDEO < 2 ~ "ESQUERDA",
  IND_IDEO >= 2 & IND_IDEO < 4 ~ "CENTRO-ESQUERDA",
  IND_IDEO >= 4 & IND_IDEO < 6 ~ "CENTRO",
  IND_IDEO >= 6 & IND_IDEO < 8 ~ "CENTRO-DIREITA",
  IND_IDEO >= 8 & IND_IDEO <= 10 ~ "DIREITA"
))



# Variáveis de voto total por município, estado, região e ideologia

VOTOS_UE <- base_2020 %>% 
  dplyr::group_by(SG_UE) %>% 
  dplyr::summarize(
    VOTOS_UE = sum(VOTOS)) 

VOTOS_UF <- base_2020 %>% 
  dplyr::group_by(SG_UF) %>% 
  dplyr::summarize(
    VOTOS_UF = sum(VOTOS))

VOTOS_REG <- base_2020 %>% 
  dplyr::group_by(REGIAO) %>% 
  dplyr::summarise(
    VOTOS_REG = sum(VOTOS))

VOTOS_IDEO <- base_2020 %>% 
  dplyr::group_by(IDEOLOGIA) %>% 
  dplyr::summarise(
    VOTOS_IDEO = sum(VOTOS))

# Adicionando as variáveis à base

base_2020 <- base_2020 %>% dplyr::left_join(VOTOS_UF, by = "SG_UF") # Votos totais por Estado

base_2020 <- base_2020 %>% dplyr::left_join(VOTOS_UE, by = "SG_UE") # Votos totais por Município

base_2020 <- base_2020 %>% dplyr::left_join(VOTOS_REG, by = "REGIAO") # Votos totais por Região

base_2020 <- base_2020 %>% dplyr::left_join(VOTOS_IDEO, by = "IDEOLOGIA") # Votos totais por ideologia


# Adicionando a variável de % de voto do candidato em cada eleição

source("0-funcoes.R")

base_2020$PORCENT_UE <- porcent(base_2020$VOTOS, base_2020$VOTOS_UE)


# Criando a base com apenas candidaturas femininas

base_fem <- base_2020 %>% filter(CD_GENERO == 4) %>%  
  arrange(desc(PORCENT_UE))


# Salvando a base de trabalho na pasta data -------------------------------


readr::write_rds(base_2020, "data/base_2020.rds")
readr::write_rds(base_fem, "data/base_fem.rds")

rm(VOTOS_UE, VOTOS_UF, VOTOS_REG, VOTOS_IDEO, 
   centro_oeste, nordeste, norte, sul, sudeste, 
   ideologia, func_tab_fem, func_tabela, porcent,
   porcent_cand, porcent_votos)








