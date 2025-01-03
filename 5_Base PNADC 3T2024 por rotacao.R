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
  
  estimativas <- estimativas %>%
    arrange(regioes, V1016)
  
  # Save
  
  saveRDS(estimativas, paste0("D:/FJP2425/Programacao/data/rotacao/resultados_0", mesano, ".RDS"))
  
  rm(pnadc)
  gc()
  
  paste("Concluído:", mesano)
  
}        

################################################################################
#### Teste para apenas um único trimestre

# lista2<-c(012012)

# sapply(lista2, function(i) calcula_ocup_desocup_k(i))

teste1 <- readRDS("D:/FJP2425/Programacao/data/rotacao/resultados_012012.RDS")

View(teste1)

### Reorganizando o DF:

funorg <- function(df) {
  
  ## Total MG (sem rotação):
  
  total_mg <- df %>%
    filter(is.na(V1016)) %>%
    select(periodo, ocupada, se_ocupada, desocupada, se_desocupada) %>%
    mutate(regioes = "11 - Minas Gerais") %>% 
    select(-6)
  
  ## Regiões:
  
  regioes_df <- df %>%
    filter(!is.na(V1016)) %>%
    pivot_wider(
      names_from = V1016,
      values_from = c(ocupada, se_ocupada, desocupada, se_desocupada),
      names_glue = "{.value}_{V1016}"
    ) %>%
    select(-regioes) %>%
    relocate(
      order(colnames(.) %in% c("ocupada_", "se_ocupada_", "desocupada_", "se_desocupada_")),
      .after = periodo
    ) %>%
    arrange(periodo)
  
  ## Separando os dados por região:
  regioes_split <- split(regioes_df, df$regioes[!is.na(df$regioes)])
  
  ## Incluindo o total de Minas Gerais no resultado final:
  regioes_split[["11 - Minas Gerais"]] <- total_mg
  
  return(regioes_split)
}

baserot012012 <-funorg(teste1)



################################################################################
#### TESTE PARA TRÊS TRIMESTRES

lista3<-c(012012,022012,032012)

sapply(lista3, function(i) calcula_ocup_desocup_k(i))


## Correção do desalinhamento -> se necessário

corrigir_rotacao <- function(df) {
  df <- df %>%
    mutate(
      V1016 = case_when(
        V1016 == "1" ~ "2",   # Grupo 1 vai para o grupo 2
        V1016 == "2" ~ "3",   # Grupo 2 vai para o grupo 3
        V1016 == "3" ~ "4",   # Grupo 3 vai para o grupo 4
        V1016 == "4" ~ "5",   # Grupo 4 vai para o grupo 5
        V1016 == "5" ~ "1",   # Grupo 5 vai para o grupo 1
        TRUE ~ V1016          # Manter os demais inalterados
      )
    )
  
  return(df)
}

# Leitura

teste3<-list.files("data/rotacao", pattern = "\\.RDS$", full.names = TRUE)

lapply(teste3, function(file_path) {
  estimativas <- readRDS(file_path)  
  estimativas_corrigida <- corrigir_rotacao(estimativas)  
  saveRDS(estimativas_corrigida, file_path)  
  paste("Desalinhamento corrigido em:", file_path)
})

data_list <- lapply(teste3, readRDS)

base8trim <- lapply(data_list, funorg)


# Função para organização dos arquivos lidos

comb <- function(list_of_dataframes) {
  combined <- vector("list", length = 11)
  names(combined) <- c(
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
  
  # Itera sobre cada lista de dataframes (um para cada trimestre)
  for (df_list in list_of_dataframes) {
    for (region_name in names(df_list)) {
      if (is.null(combined[[region_name]])) {
        combined[[region_name]] <- df_list[[region_name]]
      } else {
      combined[[region_name]] <- bind_rows(combined[[region_name]], df_list[[region_name]])
      }
    }
  }
  
  # Ordena os dataframes de cada região pela coluna `periodo`
  combined <- lapply(combined, function(df) {
    df %>% arrange(periodo)
  })
  
  return(combined)
}

baseteste3<-comb(base8trim)

sapply(baseteste3, dim)


