# Funções 

# Função de tabela

func_tabela <- function(mydf, myvar){
  mydf %>% group_by({{myvar}}) %>% 
    summarise(
      Candidaturas = n(),
      Porcentagem = porcent_cand(Candidaturas),
      Votos = sum(VOTOS),
      Porcent = porcent_votos(Votos)) %>%
    arrange(desc(Votos))
  }

# Função tabela em base_fem

func_tab_fem <- function(mydf, myvar, mytitle){
  library(tidyverse)
  mydf %>% group_by({{myvar}}) %>% 
    summarise(
      Candidaturas = n(),
      Votos = sum(VOTOS),
      Porcentagem = mean(PORCENT_UE), 
      Porcentagem = round(Porcentagem, 1)) %>%
    arrange(desc(Porcentagem))
}

# Funções de porcentagem por n de candidaturas e n de votos

porcent_cand <- function(x){
  k <- x / 18090
  i <- k*100
  round(i, 1)
} 

porcent_votos <- function(x){
  k <- x / 99855917
  i <- k*100
  round(i, 1)
}

# Porcentagem por n de candidaturas e votos por município

porcent <- function(myvar, myref){
  k <- myvar/myref
  i <- k*100
  round(i, 1)
}
