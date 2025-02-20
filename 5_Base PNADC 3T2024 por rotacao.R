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
  # Alguns comandos estão ocultados por conta da fase de testes

lista<-lista <- c(012012,012013,012014,012015,012016,012017,012018,012019,012020,012021,012022,012023,012024,
                  022012,022013,022014,022015,022016,022017,022018,022019,022020,022021,022022,022023,022024,
                  032012,032013,032014,032015,032016,032017,032018,032019,032020,032021,032022,032023,032024,
                  042012,042013,042014,042015,042016,042017,042018,042019,042020,042021,042022,042023,042024)

 sapply(lista, function(i) calcula_ocup_desocup_k(i))

 dados<-list.files("data/rotacao", pattern = "\\.RDS$", full.names = TRUE)

 lista_dados<-lapply(dados, readRDS)

 baserot0424<-funorg(lista_dados)

 saveRDS(baserot0424, file = "D:/FJP2425/Programacao/data/baserot0424.rds")

datarot<-readRDS("D:/FJP2425/Programacao/data/baserot0424.RDS")

### AJUSTE DE DESALINHAMENTO DA BASE ###########################################

## O primeiro passo é dividir a base nos seguintes subgrupos:
  # Ocupada; se_ocupada; desocupada; se_desocupada
    # Importante para aplicar a função org posteriormente

ocupada <- lapply(datarot[-11], function(sublista) {
  sublista[, grepl("^ocupada_", names(sublista))]
})

se_ocupada <- lapply(datarot[-11], function(sublista) {
  sublista[, grep("^se_ocupada_", colnames(sublista))]
})

desocupada<- lapply(datarot[-11], function(sublista) {
  sublista[, grep("^desocupada_", colnames(sublista))]
})

se_desocupada<- lapply(datarot[-11], function(sublista) {
  sublista[, grep("^se_desocupada_", colnames(sublista))]
})

  # Fazendo o mesmo para baseMG_k para verificação posterior:

ocMG_k <- lapply(baseMG_k, function(sublista) {
  sublista[, grepl("^ocupada.", names(sublista))]
})

desMG_k <- lapply(baseMG_k, function(sublista) {
  sublista[, grepl("^desocupada.", names(sublista))]
})


## Transformando essas novas bases em numéricas:

ocupada <- lapply(ocupada, as.matrix)

se_ocupada <- lapply(se_ocupada, as.matrix)

desocupada <- lapply(desocupada, as.matrix)

se_desocupada <- lapply(se_desocupada, as.matrix)

## a<-as.matrix(baserot0324[["01-Belo Horizonte"]][,2:6])%*%diag(5)

## a <- as.matrix(as.data.frame(lapply(baserot0324[["01-Belo Horizonte"]][, 2:6], as.numeric)))


## Criando a função para alinhar o banco de dados:

# Criando a matriz identidade de tamanho 5
identity_matrix <- diag(5)

# Número de vezes que queremos repetir a diagonal
repetitions <- 20

t <- nrow(datarot$`01-Belo Horizonte`)

# Repetindo as diagonais uma embaixo da outra

organiza_base<-function(matriz){
  org <- do.call(rbind, replicate(repetitions, identity_matrix, simplify = FALSE))
  org1<- org[1:t,] # Aqui é uma generalização
  org2<- org[2:(t+1),]
  org3<- org[3:(t+2),]
  org4<- org[4:(t+3),]
  org5<- org[5:(t+4),]
  
  col1<-rowSums(matriz*org1)
  col2<-rowSums(matriz*org2)
  col3<-rowSums(matriz*org3)
  col4<-rowSums(matriz*org4)
  col5<-rowSums(matriz*org5)
  
  
  matrizfinal<-cbind(col1,col2,col3,col4,col5)
  
  return(matrizfinal)
  
}
  
ocuporg <- lapply(ocupada, function(sublista) {
  resultado <- organiza_base(sublista)
  colnames(resultado) <- c("ocupada_1", "ocupada_2", "ocupada_3", "ocupada_4", "ocupada_5")
  return(resultado)
})

se_ocuporg <- lapply(se_ocupada, function(sublista) {
  resultado <- organiza_base(sublista)
  colnames(resultado) <- c("se_ocupada_1", "se_ocupada_2", "se_ocupada_3", "se_ocupada_4", "se_ocupada_5")
  return(resultado)
})

desocuporg <- lapply(desocupada, function(sublista) {
  resultado <- organiza_base(sublista)
  colnames(resultado) <- c("desocupada_1", "desocupada_2", "desocupada_3", "desocupada_4", "desocupada_5")
  return(resultado)
})

se_desocuporg <- lapply(se_desocupada, function(sublista) {
  resultado <- organiza_base(sublista)
  colnames(resultado) <- c("se_desocupada_1", "se_desocupada_2", "se_desocupada_3", "se_desocupada_4", "se_desocupada_5")
  return(resultado)
})


# Juntando as 4 bases por região e adicionando o total MG:

basetotal <- Map(cbind, ocuporg, se_ocuporg, desocuporg, se_desocuporg)

a <- as.matrix(as.data.frame(lapply(datarot[["11 - Minas Gerais"]][, 2:5], as.numeric)))

basetotal <- append(basetotal, list(a))

names(basetotal)[11] <- "11 - Minas Gerais"

basetotal <- lapply(basetotal, as.data.frame)


# Adicionando novamente a coluna "periodo" e salvando o objeto

periodo <- paste0(rep(2012:2024, each = 4), "Q", 1:4)
periodo <- periodo[1:52]  # 52 Trimestres -> importante porque ainda não temos os dados do 1T2025

# Adicionar a coluna "periodo" a cada sub-data.frame e posicioná-la na primeira posição
basetotal <- lapply(basetotal, function(df) {
  df <- cbind(periodo = periodo, df)
  return(df)
})

saveRDS(basetotal, file = "D:/FJP2425/Programacao/data/basealinhada0424.rds")

dados<-readRDS("D:/FJP2425/Programacao/data/basealinhada0424.rds")


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




