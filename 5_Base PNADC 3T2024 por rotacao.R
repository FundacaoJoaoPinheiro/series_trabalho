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
library(tidyverse)

################################################################################
### PRIMEIRO PASSO: AVALIANDO O ARQUIVO "GABARITO"
  ## O arquivo "base mg_k" contém a base feita anteriormente pelo Caio
    ## Recomenda-se o estudo de sua estrutura para saber como montar a base atualizada

baseMG_k <- readRDS("D:/FJP2425/Programacao/data/baseMG_k.RDS")

View(baseMG_k[["01-Belo Horizonte"]])


################################################################################
### FUNÇÃO BASE DE ROTAÇÃO ATUALIZADA ATÉ O TERCEIRO TRIMESTRE DE 2024

## Estabelecimento das variáveis:
    
  # V1008: Número de seleção do domicílio;
  # V1014: Painel;
  # V1016: Número da entrevista do domicílio;
  # V4001 e V4002: Referentes a trabalho na semana de referência

# vars = c("Ano","Trimestre","V1008","V1014","V1016","VD4002","VD4001","UF")
  # Pode ser incluído na função ou separadamente


### Função para criar os arquivos .RDS por grupo de rotação:
# Essa função segue os mesmos moldes da função que criou a base da PNADC no arquivo "2_PNADC 2T2024 RDS"
# Primeiro, ela irá criar os .RDS de cada trimestre

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
  
  estimativas <- estimativas %>%
    arrange(regioes, V1016)
  
  # Save
  
  saveRDS(estimativas, paste0("D:/FJP2425/Programacao/data/rotacao/resultados_0", mesano, ".RDS"))
  
  rm(pnadc)
  gc()
  
  paste("Concluído:", mesano)
  
}        


#### Funções que reorganizam os dfs:"
  ## Funorg -> organiza os dataframes conforme o formato do arquivo "baseMG_k"

funorg <- function(data_list) {
  regioes <- c(
    "01-Belo Horizonte",
    "02-Entorno metropolitano de BH",
    "03-Colar metropolitano de BH",
    "04-RIDE de Brasília em Minas",
    "05-Sul de Minas",
    "06-Triângulo Mineiro",
    "07-Mata de Minas Gerais",
    "08-Norte de Minas",
    "09-Vale do Rio Doce",
    "10-Central",
    "11 - Minas Gerais"
  )
  
  # Definição das colunas esperadas
  colunas_esperadas <- c(
    "periodo",
    paste0(rep(c("ocupada_", "se_ocupada_"), each = 5), 1:5),
    paste0(rep(c("desocupada_", "se_desocupada_"), each = 5), 1:5)
  )
  
  resultados <- list()
  
  for (regiao in regioes[1:10]) {
    dados_regiao <- lapply(data_list, function(df) df[df$regioes == regiao, ])
    
    # Por V1016
    df_regiao <- do.call(rbind, lapply(dados_regiao, function(df) {
      
      ## Organização para se assemelhar ao formato do baseMG_k
      wide_data <- reshape(
        df,
        idvar = "periodo",
        timevar = "V1016",
        direction = "wide",
        sep = "_"
      )
      
          # Mudando os nomes
      colnames(wide_data) <- gsub("\\.ocupada", "ocupada", colnames(wide_data))
      colnames(wide_data) <- gsub("\\.se\\.ocupada", "se_ocupada", colnames(wide_data))
      colnames(wide_data) <- gsub("\\.desocupada", "desocupada", colnames(wide_data))
      colnames(wide_data) <- gsub("\\.se\\.desocupada", "se_desocupada", colnames(wide_data))
      
          # Lidando com NAs (caso necessário)
      colunas_faltantes <- setdiff(colunas_esperadas, colnames(wide_data))
      wide_data[colunas_faltantes] <- NA
      
          # Reordenação
      wide_data <- wide_data[, colunas_esperadas, drop = FALSE]
      
      return(wide_data)
    }))
    
    # Ordenar as linhas pelos trimestres
    df_regiao <- df_regiao[order(df_regiao$periodo), ]
    
    resultados[[regiao]] <- df_regiao
  }
  
  # Sublista para o total MG e organização final
  
  dados_totais <- lapply(data_list, function(df) df[df$regioes == "11 - Minas Gerais", ])
  df_total <- do.call(rbind, dados_totais)
  df_total <- df_total[, !colnames(df_total) %in% "V1016"]
  df_total <- df_total[, c("periodo", "ocupada", "se_ocupada", "desocupada", "se_desocupada")]
  df_total <- df_total[order(df_total$periodo), ]
  resultados[["11 - Minas Gerais"]] <- df_total
  
  return(resultados)
}


################################################################################
#### Montagem da base:

#lista<-lista <- c(012012,012013,012014,012015,012016,012017,012018,012019,012020,012021,012022,012023,012024,
#                  022012,022013,022014,022015,022016,022017,022018,022019,022020,022021,022022,022023,022024,
#                  032012,032013,032014,032015,032016,032017,032018,032019,032020,032021,032022,032023,032024,
#                  042012,042013,042014,042015,042016,042017,042018,042019,042020,042021,042022,042023)

# sapply(lista, function(i) calcula_ocup_desocup_k(i))

dados<-list.files("data/rotacao", pattern = "\\.RDS$", full.names = TRUE)

lista_dados<-lapply(dados, readRDS)

baserot0324<-funorg(lista_dados)

saveRDS(baserot0324, file = "D:/FJP2425/Programacao/data/baserot0324.rds")

leitura<-readRDS("D:/FJP2425/Programacao/data/baserot0324.RDS")


################################################################################
#### Teste para apenas um único trimestre

# lista2<-c(012012)

# sapply(lista2, function(i) calcula_ocup_desocup_k(i))

teste1 <- readRDS("D:/FJP2425/Programacao/data/rotacao/resultados_012012.RDS")

View(teste1)


################################################################################
#### TESTE PARA TRÊS TRIMESTRES

lista3<-c(012012,022012,032012)

#sapply(lista3, function(i) calcula_ocup_desocup_k(i))

teste3<-list.files("data/rotacao", pattern = "\\.RDS$", full.names = TRUE)

data_list <- lapply(teste3, readRDS)

teste3a<-funorg(data_list)


################################################################################
#### TESTE PARA DEZESSEIS TRIMESTRES

lista4<-c(012012,012013,012014,012015,
          022012,022013,022014,022015,
          032012,032013,032014,032015,
          042012,042013,042014,042015)

# sapply(lista4, function(i) calcula_ocup_desocup_k(i))

teste4<-list.files("data/rotacao", pattern = "\\.RDS$", full.names = TRUE)

data_list16 <- lapply(teste4, readRDS)

teste4a<-funorg(data_list16)




