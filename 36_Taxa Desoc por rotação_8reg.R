################################################################################
##             BASE DE DADOS POR ROTAÇÃO - TAXA DE DESOCUPAÇÃO                ##
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

baseMG_k <- readRDS("C:/FJP2425/Programacao/data/baseMG_k.RDS")

View(baseMG_k[["01-Belo Horizonte"]])

################################################################################
### FUNÇÃO BASE DE ROTAÇÃO ATUALIZADA ATÉ O QUARTO TRIMESTRE DE 2024

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

calcula_tx_k <- function(mesano) {
  pnadc <- pnadc_design(read_pnadc(
    paste0("data/txt/PNADC_0", mesano, ".txt"),
    "data/documentacao/input_PNADC_trimestral.txt",
    vars = c("V1016", "VD4002", "VD4001", "UF")
  )) %>%
    update(
      ocupada = 1 * (VD4002 == 1),
      desocupada = 1 * (VD4002 == 2),
      regioes = case_when(
        Estrato %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~ "01-Belo Horizonte",
        Estrato %in% c("3120011","3120013","3120020","3120012","3130011","3130012","3130020") ~ "02-Colar e Entorno Metropolitano de BH",
        Estrato %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~ "03-Sul de Minas",
        Estrato %in% c("3152011","3152012","3152013","3152021","3152022") ~ "04-Triângulo Mineiro",
        Estrato %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~ "05-Mata de Minas Gerais",
        Estrato %in% c("3154011","3154012","3154013","3154021","3154022","3154023","3140010","3140020") ~ "06-Norte de Minas",
        Estrato %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~ "07-Vale do Rio Doce",
        Estrato %in% c("3156011","3156012","3156013","3156021","3156022") ~ "08-Central",
        TRUE ~ "09 - Minas Gerais"
      )
    )
  
  # Totais da ocupação e desocupação por região + rotação
  estimativas <- svyby(
    ~ocupada + desocupada,
    by = ~regioes + V1016,
    subset(pnadc, UF == "31"),
    svytotal,
    na.rm = TRUE
  )
  colnames(estimativas)[3:6] <- c("ocupada", "desocupada", "se_ocupada", "se_desocupada")
  
  # Cálculo da taxa de desocupação usando svyratio, por região + rotação
  taxa <- svyby(
    ~I(desocupada/(ocupada + desocupada)),  # Fórmula da taxa
    by = ~regioes + V1016,
    design = subset(pnadc, UF == "31"),
    FUN = svymean,  # svymean para proporções
    na.rm = TRUE,
    keep.var = TRUE
  )
  colnames(taxa)[3:4] <- c("tx_desocup", "se_tx_desocup")
  
  # Total de Minas Gerais por rotação (todos os estratos agregados)
  total_mg <- svyby(
    ~ocupada + desocupada,
    by = ~V1016,
    subset(pnadc, UF == "31"),
    svytotal,
    na.rm = TRUE
  )
  colnames(total_mg)[2:5] <- c("ocupada", "desocupada", "se_ocupada", "se_desocupada")
  total_mg$regioes <- "09 - Minas Gerais"
  
  # Taxa para Minas Gerais total
  taxa_mg <- svyby(
    ~I(desocupada/(ocupada + desocupada)),
    by = ~V1016,
    design = subset(pnadc, UF == "31"),
    FUN = svymean,
    na.rm = TRUE,
    keep.var = TRUE
  )
  colnames(taxa_mg)[2:3] <- c("tx_desocup", "se_tx_desocup")
  taxa_mg$regioes <- "09 - Minas Gerais"
  
  # Construção da variável de período
  periodo <- paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  estimativas$periodo <- periodo
  taxa$periodo <- periodo
  total_mg$periodo <- periodo
  taxa_mg$periodo <- periodo
  
  # Combina os resultados
  estimativas_tot <- rbind(estimativas, total_mg)
  taxa_tot <- rbind(taxa, taxa_mg)
  
  # Une estimativas com taxas por regioes + V1016 + periodo
  final <- merge(
    estimativas_tot,
    taxa_tot,
    by = c("regioes", "V1016", "periodo"),
    all = TRUE
  )
  
  # Salva o resultado
  saveRDS(final, paste0("C:/FJP2425/Programacao/data/rotacao_taxa_8reg/resultados_0", mesano, ".RDS"))
  
  # Limpeza de memória
  rm(pnadc)
  gc()
  
  paste("Concluído:", mesano)
}


#### Funções que reorganizam os dfs:"
## Funorg -> organiza os dataframes conforme o formato do arquivo "baseMG_k"

funorg <- function(data_list) {
  regioes <- c(
    "01-Belo Horizonte",
    "02-Colar e Entorno Metropolitano de BH",
    "03-Sul de Minas",
    "04-Triângulo Mineiro",
    "05-Mata de Minas Gerais",
    "06-Norte de Minas",
    "07-Vale do Rio Doce",
    "08-Central",
    "09 - Minas Gerais"
  )
  
  # Definição das colunas esperadas
  colunas_esperadas <- c(
    "periodo",
    paste0(rep(c("ocupada_", "se_ocupada_"), each = 5), 1:5),
    paste0(rep(c("desocupada_", "se_desocupada_"), each = 5), 1:5),
    paste0(rep(c("tx_desocup_", "se_tx_desocup_"), each = 5), 1:5)
  )
  
  resultados <- list()
  
  for (regiao in regioes) {
    dados_regiao <- lapply(data_list, function(df) df[df$regioes == regiao, ])
    
    # Por V1016
    df_regiao <- do.call(rbind, lapply(dados_regiao, function(df) {
      
      ## Organização para se assemelhar ao formato esperado
      wide_data <- reshape(
        df,
        idvar = "periodo",
        timevar = "V1016",
        direction = "wide",
        sep = "_"
      )
      
      # Renomear colunas para o formato padronizado
      colnames(wide_data) <- gsub("\\.ocupada", "ocupada", colnames(wide_data))
      colnames(wide_data) <- gsub("\\.se\\.ocupada", "se_ocupada", colnames(wide_data))
      colnames(wide_data) <- gsub("\\.desocupada", "desocupada", colnames(wide_data))
      colnames(wide_data) <- gsub("\\.se\\.desocupada", "se_desocupada", colnames(wide_data))
      colnames(wide_data) <- gsub("\\.tx_desocup", "tx_desocup", colnames(wide_data))
      colnames(wide_data) <- gsub("\\.se\\.tx_desocup", "se_tx_desocup", colnames(wide_data))
      
      # Garantir que todas as colunas existam (mesmo que NA)
      colunas_faltantes <- setdiff(colunas_esperadas, colnames(wide_data))
      wide_data[colunas_faltantes] <- NA
      
      # Reordenar as colunas
      wide_data <- wide_data[, colunas_esperadas, drop = FALSE]
      
      return(wide_data)
    }))
    
    # Ordenar os períodos
    df_regiao <- df_regiao[order(df_regiao$periodo), ]
    
    resultados[[regiao]] <- df_regiao
  }
  
  return(resultados)
}

################################################################################
#### Montagem da base:
# Alguns comandos estão ocultados por conta da fase de testes

lista<-lista <- c(012012,012013,012014,012015,012016,012017,012018,012019,012020,012021,012022,012023,012024,
                  022012,022013,022014,022015,022016,022017,022018,022019,022020,022021,022022,022023,022024,
                  032012,032013,032014,032015,032016,032017,032018,032019,032020,032021,032022,032023,032024,
                  042012,042013,042014,042015,042016,042017,042018,042019,042020,042021,042022,042023,042024)

sapply(lista, function(i) calcula_tx_k(i))

dados<-list.files("data/rotacao_taxa_8reg", pattern = "\\.RDS$", full.names = TRUE)

lista_dados<-lapply(dados, readRDS)

tx_rot8reg<-funorg(lista_dados)

saveRDS(tx_rot8reg, file = "C:/FJP2425/Programacao/data/tx_rot8reg.rds")

datarot<-readRDS("C:/FJP2425/Programacao/data/tx_rot8reg.RDS")

################################################################################
#### Teste para apenas um único trimestre

lista2<-c(012012)

sapply(lista2, function(i) calcula_tx_k(i))

teste1 <- readRDS("C:/FJP2425/Programacao/data/rotacao_taxa_8reg/resultados_012012.RDS")

View(teste1)


################################################################################
#### TESTE PARA TRÊS TRIMESTRES

lista3<-c(012012,022012,032012)

#sapply(lista3, function(i) calcula_tx_k(i))

teste3<-list.files("data/rotacao_taxa_8reg", pattern = "\\.RDS$", full.names = TRUE)

data_list <- lapply(teste3, readRDS)

teste3a<-funorg(data_list)


################################################################################
#### TESTE PARA DEZESSEIS TRIMESTRES

lista4<-c(012012,012013,012014,012015,
          022012,022013,022014,022015,
          032012,032013,032014,032015,
          042012,042013,042014,042015)

sapply(lista4, function(i) calcula_tx_k(i))

teste4<-list.files("data/rotacao_taxa_8reg", pattern = "\\.RDS$", full.names = TRUE)

data_list16 <- lapply(teste4, readRDS)

teste4a<-funorg(data_list16)


### AJUSTE DE DESALINHAMENTO DA BASE ###########################################

## O primeiro passo é dividir a base nos seguintes subgrupos:
# Ocupada; se_ocupada; desocupada; se_desocupada;tx_desocup; se_tx_desocup
# Importante para aplicar a função org posteriormente

datarot<-readRDS("C:/FJP2425/Programacao/data/tx_rot8reg.RDS")

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

txdesoc <- lapply(datarot[-11], function(sublista) {
  sublista[, grepl("^tx_desocup_", names(sublista))]
})

se_txdesoc <- lapply(datarot[-11], function(sublista) {
  sublista[, grepl("^se_tx_desocup_", names(sublista))]
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

txdesoc <- lapply(txdesoc, as.matrix)

se_txdesoc <- lapply(se_txdesoc, as.matrix)

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

txdesocorg <- lapply(txdesoc, function(sublista) {
  resultado <- organiza_base(sublista)
  colnames(resultado) <- c("txdesoc_1", "txdesoc_2", "txdesoc_3", "txdesoc_4", "txdesoc_5")
  return(resultado)
})

se_txdesocorg <- lapply(se_txdesoc, function(sublista) {
  resultado <- organiza_base(sublista)
  colnames(resultado) <- c("se_txdesoc_1", "se_txdesoc_2", "se_txdesoc_3", "se_txdesoc_4", "se_txdesoc_5")
  return(resultado)
})


# Juntando as 4 bases por região e adicionando o total MG:

basetotal <- Map(cbind, ocuporg, se_ocuporg, desocuporg, se_desocuporg, txdesocorg, se_txdesocorg)

basetotal <- lapply(basetotal, as.data.frame)


# Adicionando novamente a coluna "periodo" e salvando o objeto

periodo <- paste0(rep(2012:2024, each = 4), "Q", 1:4)
periodo <- periodo[1:52]  # 52 Trimestres -> importante porque ainda não temos os dados do 1T2025

# Adicionar a coluna "periodo" a cada sub-data.frame e posicioná-la na primeira posição
basetotal <- lapply(basetotal, function(df) {
  df <- cbind(periodo = periodo, df)
  return(df)
})

saveRDS(basetotal, file = "C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.rds")

dados<-readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.rds")



