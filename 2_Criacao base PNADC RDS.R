################################################################################
##                      TENTATIVA PNADC RDS 2024                              ##
################################################################################

## Arquivos .RDS são bases da própria PNAD

library(PNADcIBGE)
library(survey)
library(tidyverse)

## Para que a função funcione, será necessário criar o caminho data/documentacao dentro do diretório principal
  ## Fazer isso uma única vez.
  ## Feitos os caminhos, mova o input para "caminhoprincipal/data/documentacao" e os dados para "caminhoprincipal/data/txt"
    # Código para criar os caminhos:

      # dir.create("data/documentacao", recursive = TRUE)
      # input <- "data/documentacao/input_PNADC_trimestral.txt"
      # print(file.exists(arquivo_dicionario)) 
          # Se retornar TRUE, o código deu certo. O arquivo input está no lugar certo

      # dir.create("D:/FJP2425/Programacao/data/txt", recursive = TRUE)
      # dados <- "data/txt/PNADC_012012.txt"
      # print(file.exists(dados))
          # Se retornar TRUE, o código deu certo. O arquivo input está no lugar certo

    # Código para criar o caminho que armazena as bases em .RDS:

      # dir.create("D:/FJP2425/Programacao/data/estimativas", recursive = TRUE)


## Testando com as bases no diretório
  ## Foram feitos os downloads de todos os arquivos .txt da PNADC no diretório

## Função que automatiza a chamada dos dados
  ## Tudo uma única função

## Alterando a chamada dos estratos -> utilizando %in%

calcula_ocup_desocup <- function(mesano){
  pnadc <- pnadc_design(read_pnadc(paste0("data/txt/PNADC_0",mesano, ".txt"), 
                                   "data/documentacao/input_PNADC_trimestral.txt",vars=c("VD4002"))) %>% 
    update(ocupada = 1 * (VD4002 == 1),
           desocupada = 1 * (VD4002 == 2),
           regioes = case_when(
             Estrato %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~"01-Belo Horizonte",
             Estrato %in% c("3120011","3120013","3120020","3120012") ~"02-Entorno metropolitono de BH",
             Estrato %in% c("3130011","3130012","3130020") ~"03-Colar metropolitano de BH",
             Estrato %in% c("3140010","3140020") ~"04-RIDE de Brasília em Minas",
             Estrato %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~"05-Sul de Minas",
             Estrato %in% c("3152011","3152012","3152013","3152021","3152022") ~"06-Triângulo Mineiro",
             Estrato %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~"07-Mata de Minas Gerais",
             Estrato %in% c("3154011","3154012","3154013","3154021","3154022","3154023") ~"08-Norte de Minas",
             Estrato %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~"09-Vale do Rio Doce",
             Estrato %in% c("3156011","3156012","3156013","3156021","3156022") ~"10-Central",
             TRUE ~ "11 - Minas Gerais")
           # regioes
    )
  # Criando estimativas regionais (reescrevendo parte do código)
  
    ## Total de ocupados
  
  estimativas <- svyby(~ocupada, by = ~regioes, subset(pnadc, UF =="31"),na.rm = TRUE, svytotal)
  colnames(estimativas)[3]<-"se_o"
  
    ## Total de desocupados
  estimativas_d <- svyby(~desocupada, by = ~regioes, subset(pnadc, UF =="31"),na.rm = TRUE, svytotal)
  colnames(estimativas_d)[3]<-"se_d"

    ## Taxa de desocupação
  estimativas_td <- svyby(~desocupada, by = ~regioes, subset(pnadc, UF == "31"), na.rm = TRUE, svymean)
  colnames(estimativas_td)[2]<-"tx_desocupada"
  colnames(estimativas_td)[3]<-"se_td"
  
  # criando estimativas para o total MG
    
    ## Primeiro: variáveis que estavam sendo trabalhadas para cada um desses estratos
      ## Código remete ao "TRUE ~ "11 - Minas Gerais"" utilizado anteriormente
  
  total_ocupada<- svytotal(~ocupada,subset(pnadc, UF=="31"),na.rm = TRUE)
  total_desocupada<- svytotal(~desocupada,subset(pnadc,UF=="31"), na.rm = TRUE)
  total_tx_desocupacao <- svymean(~desocupada, subset(pnadc, UF == "31"), na.rm = TRUE)
  
    ## Segundo: resolvendo a questão discutida na última reunião (19/11/24) de inclusão desses dados no RDS
      ## Tentativa: criando uma tabela específica <função tibble> e juntando às regionais posteriormente
  
  total_mg<-tibble(
    regioes = "11 - Minas Gerais",
    ocupada = coef(total_ocupada),
    se_o = SE(total_ocupada),
    desocupada = coef(total_desocupada),
    se_d = SE(total_desocupada),
    tx_desocupada = coef(total_tx_desocupacao),
    se_td = SE(total_tx_desocupacao),
    periodo = paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  )
  
  
  # Juntando as bases -> regional e MG
  estimativas <- estimativas %>% 
    left_join(estimativas_d) %>% 
    left_join(estimativas_td) %>% 
    mutate(periodo = paste0(substr(mesano,2,5),"_0",substr(mesano,1,1))) %>%
    bind_rows(total_mg) # Adicionando o tibble criado
  
  
  rm(estimativas_d,estimativas_td,pnadc)
  gc()
  
  # salva base
  saveRDS(estimativas,paste0("D:/FJP2425/Programacao/data/estimativas/resultados_0",mesano,".RDS"))
  paste("Concluído:",mesano)
  
}
        ## Fim da função

## Atualizando lista até o 3T2024:
  ## Continuando a formatação por colunas

lista <- c(012012,012013,012014,012015,012016,012017,012018,012019,012020,012021,012022,012023,012024,
           022012,022013,022014,022015,022016,022017,022018,022019,022020,022021,022022,022023,022024,
           032012,032013,032014,032015,032016,032017,032018,032019,032020,032021,032022,032023,032024,
           042012,042013,042014,042015,042016,042017,042018,042019,042020,042021,042022,042023,042024)


sapply(lista, function(i) calcula_ocup_desocup(i))


## Testando leitura de um arquivo

pnad022024<-readRDS("data/estimativas/resultados_022024.RDS")

rm(pnad022024)


## Teste antes de prosseguir para o próx script: comparando com dados do 1T2023
  ## Tentando criar apenas um RDS

lista2<-c(012023)

sapply(lista2, function(i) calcula_ocup_desocup(i))


## Verificando se está correto o RDS criado a partir da função modificada para incluir MG
  # Leituras feitas para o 012023


arquivos <- list.files("data/estimativas", pattern = "\\.RDS$", full.names = TRUE)

pnadcrds <- lapply(arquivos, readRDS)

dimensions <- sapply(pnadcrds, dim)

dimensions_df <- data.frame(t(dimensions))
colnames(dimensions_df) <- c("Linhas", "Colunas")
print(dimensions_df)

column_names <- lapply(pnadcrds, colnames)
print(column_names[[1]])

View(pnadcrds[[52]])

## Diagnóstico:
  ## Os dados feitos dessa forma conferem com o SIDRA. O DF também ficou de acordo com o trabalhado no próximo script. 













