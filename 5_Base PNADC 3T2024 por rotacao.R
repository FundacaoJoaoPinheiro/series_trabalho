################################################################################
##                      BASE DE DADOS POR ROTAÇÃO (k)                         ##
################################################################################

## Objetivo: Criar uma base de dados .RDS por rotação com os grupos da PNAD

# Pacotes necessários:

library(PNADcIBGE)
library(survey)
library(tictoc)
library(srvyr)
library(dplyr)
library(tidyr)
library(purrr)

################################################################################
### PRIMEIRO PASSO: AVALIANDO O ARQUIVO "GABARITO"
  ## O arquivo "base mg_k" contém a base feita anteriormente pelo Caio
    ## Recomenda-se o estudo de sua estrutura para saber como montar a base atualizada

baseMG_k <- readRDS("D:/FJP2425/Programacao/data/baseMG_k.RDS")

View(baseMG_k[["01-Belo Horizonte"]])


################################################################################
### TESTANDO FUNÇÃO BASE DE ROTAÇÃO ATUALIZADA ATÉ O TERCEIRO TRIMESTRE DE 2024

## Estabelecimento das variáveis:
    
  # V1008: Número de seleção do domicílio;
  # V1014: Painel;
  # V1016: Número da entrevista do domicílio;
  # V4001 e V4002: Referentes a trabalho na semana de referência

# vars = c("Ano","Trimestre","V1008","V1014","V1016","VD4002","VD4001","UF")
  # Pode ser incluído na função ou separadamente


### Função para os grupos de rotação:
# Essa função segue os mesmos moldes da função que criou a base da PNADC no arquivo "2_PNADC 2T2024 RDS"

calcula_ocup_desocup_k<-function(mesano){
  pnadc <- pnadc_design(read_pnadc(
    paste0("data/txt/PNADC_0", mesano, ".txt"),
    "data/documentacao/input_PNADC_trimestral.txt",
    vars = c("V1016", "VD4002", "VD4001", "UF")
  )) %>%
    update(
      ocupada = 1 * (VD4002 == 1),
      desocupada = 1 * (VD4002 == 2),
      regioes = case_when(
        Estrato %in% c("3110213", "3110113", "3110112", "3110212", "3110111", "3110211") ~ "01-Belo Horizonte",
        Estrato %in% c("3120011", "3120013", "3120020", "3120012") ~ "02-Entorno metropolitano de BH",
        Estrato %in% c("3130011", "3130012", "3130020") ~ "03-Colar metropolitano de BH",
        Estrato %in% c("3140010", "3140020") ~ "04-RIDE de Brasília em Minas",
        Estrato %in% c("3151011", "3151012", "3151013", "3151021", "3151022", "3151023") ~ "05-Sul de Minas",
        Estrato %in% c("3152011", "3152012", "3152013", "3152021", "3152022") ~ "06-Triângulo Mineiro",
        Estrato %in% c("3153011", "3153012", "3153013", "3153021", "3153022", "3153023") ~ "07-Mata de Minas Gerais",
        Estrato %in% c("3154011", "3154012", "3154013", "3154021", "3154022", "3154023") ~ "08-Norte de Minas",
        Estrato %in% c("3155011", "3155012", "3155013", "3155021", "3155022", "3155023") ~ "09-Vale do Rio Doce",
        Estrato %in% c("3156011", "3156012", "3156013", "3156021", "3156022") ~ "10-Central",
        TRUE ~ "11 - Minas Gerais"
      )
    )
  
  # Fim da semelhança com a função referida anterioremente. Adaptando o código para capturar as estimativas
  # por região e rotação
  
  estimativas <- svyby(
    ~ocupada + desocupada,
    by = ~regioes + V1016,
    subset(pnadc, UF == "31"),
    svytotal,
    na.rm = TRUE
  )
  
  colnames(estimativas)[3:6] <- c("ocupada", "desocupada", "se_ocupada", "se_desocupada")
  
  # Total para Minas Gerais:
  
  total_ocupada <- svytotal(~ocupada, subset(pnadc, UF == "31"), na.rm = TRUE)
  total_desocupada <- svytotal(~desocupada, subset(pnadc, UF == "31"), na.rm = TRUE)
  
  total_mg <- tibble(
    regioes = "11 - Minas Gerais",
    V1016 = NA,
    ocupada = coef(total_ocupada),
    desocupada = coef(total_desocupada),
    se_ocupada = SE(total_ocupada),
    se_desocupada = SE(total_desocupada),
    periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  )
  
  
  # Juntando as partes:
  
  estimativas <- estimativas %>%
    mutate(periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))) %>%
    bind_rows(total_mg)
  
  # Save
  
  saveRDS(estimativas, paste0("D:/FJP2425/Programacao/data/rotacao/resultados_0", mesano, ".RDS"))
  
  rm(pnadc)
  gc()
  
  paste("Concluído:", mesano)
  
}

### Teste para apenas um único trimestre

lista2<-c(012012)

sapply(lista2, function(i) calcula_ocup_desocup_k(i))

teste1 <- readRDS("D:/FJP2425/Programacao/data/rotacao/resultados_012012.RDS")

View(teste1)








