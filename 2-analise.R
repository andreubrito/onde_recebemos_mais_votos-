
usethis::use_github()
3
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)

library(kableExtra)
library(viridis)

# Carregando as funções

source("1-tratamento.R")

source("0-funcoes.R")


# Tabela da base_2020

sum(base_2020$VOTOS)

func_tabela(base_2020, DS_COR_RACA)

tab_1 <- func_tabela(base_2020, DS_GENERO)

func_tabela(base_2020, DS_GRAU_INSTRUCAO)

k <- func_tabela(base_2020, SG_PARTIDO)

func_tabela(base_2020, IND_IDEO)

k <- colnames()


readr::write_rds(tab_1, "data/tabela1.rds")

# Teste
# base_fem %>% filter(NM_UE == "RECIFE") %>%  view()
#   summarize(Media = mean(PORCENT_UE)) %>%
#   view()

  
# Tabela para a base_fem

func_tab_fem(base_fem, DS_COR_RACA)

func_tab_fem(base_fem, DS_GRAU_INSTRUCAO)

func_tab_fem(base_fem, IDEOLOGIA)

func_tab_fem(base_fem, REGIAO)

func_tab_fem(base_fem, SG_UF)

i <- func_tab_fem(base_fem, SG_PARTIDO)

mean(base_fem$PORCENT_UE)

round(mean(base_mas$PORCENT_UE), 2)


# Tabela para a base_mas

base_mas <- filter(base_2020, DS_GENERO == "MASCULINO") # Criando a base de candidaturas masculinas


func_tab_fem(base_fem, DS_COR_RACA)

func_tab_fem(base_fem, DS_GRAU_INSTRUCAO)

func_tab_fem(base_fem, IDEOLOGIA)

func_tab_fem(base_mas, REGIAO)

func_tab_fem(base_mas, SG_UF)


# pivot_wide
?pivot_wider

base <- base_2020 %>% pivot_wider(
  names_from = "DS_GENERO",
  values_from = "DS_GENERO"
) 

base <- as.numeric(base$FEMININO)

base %>% group_by(MASCULINO, FEMININO) %>% 
  summarise(
    Candidaturas = n(),
    Votos = sum(VOTOS),
    Desempenho = mean(PORCENT_UE)
    ) %>% view()



total <- base_2020 %>% nrow()
t_fem <- base_fem %>% nrow()
t_mas <- base_mas %>% nrow()

count

base_2020 %>% group_by(DS_GENERO) %>% 
  summarize(
    Total = n(),
    Porcentagem = porcent(n(), Total),
    Desempenho = mean(porcent(VOTOS, VOTOS_UE))
  ) %>% view()





# Total e porcentagem de eleitos.

base_2020 %>% filter(DS_SIT_TOT_TURNO == "NÃO ELEITO") %>% 
  group_by(DS_GENERO) %>% 
  summarize(
    Total = n(), 
    Porcentagem = porcent(1798, Total)
  ) %>% view()

table(base_2020$DS_SIT_TOT_TURNO)

# Código com casas decimais que funcionam

base_2020 %>% group_by(DS_GENERO) %>% 
  summarise(
    Candidaturas = n(),
    Votos = sum(VOTOS),
    Desempenho = mean(PORCENT_UE)
  ) %>% dplyr::mutate_if(is.numeric, 
                 format, 
                 digits = 3,
                 big.mark = ".") %>%  view()

# Gráfico que funciona

k <-  base_2020 %>% group_by(SG_UF,DS_GENERO) %>% 
  summarize(
    Votos = sum(VOTOS),
    Total = unique(VOTOS_UF),
    Porcentagem = porcent(Votos, Total)
    )

k <- as.data.frame(k)

# Transformando em fator
k$SG_UF <- factor(k$SG_UF, levels = c("AM", "ES", "SC", "MS", "MT", "SP", "PR", 
                                      "GO", "MG", "RO", "AP", "PA", "RS", "RR", 
                                      "CE", "PI", "RJ", "AL", "BA", "MA", "PB", 
                                      "AC", "RN", "TO", "SE", "PE"))

# mm

# k$SG_UF <- factor(k$SG_UF, levels = c("PE", "SE", "TO", "RN", "AC", "PB", "MA", 
#                                       "BA", "AL", "RJ", "PI", "CE", "RR", "RS", 
#                                       "PA", "AP", "RO", "MG", "GO", "PR", "SP",
#                                       "MT", "MS", "SC", "ES", "AM"))

m <-  ggplot(k, aes(fill=DS_GENERO, y=SG_UF, x=Votos)) + 
  geom_bar(position="fill", stat="identity") + theme_classic() +
  labs(title = "Proporção de votos totais por Estado da federação", 
       caption = "Fonte: TSE", 
       x = "Votos (%)", 
       y = ""
       ) +
  scale_fill_manual(values = c("#9ecae1", "#3182bd"), name = "Gênero", labels = c("Feminino", "Masculino"))
m





