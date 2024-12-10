################################################################################
##         CALCULANDO O TAMANHO DA AMOSTRA EFETIVA PARA OS 10 ESTRATOS        ##
################################################################################

### CASO JÁ ESTEJA COM OS DADOS: COMEÇAR A PARTIR DA LEITURA DO ARQV EXCEL

# Diretório principal

setwd("D:/FJP2425/Programacao")

library(PNADcIBGE)
library(survey)
library(tidyverse)
library(dplyr)
library(tidyr)
library(writexl)
library(readxl)
library(srvyr)

dev.new()
dev.new()

################################################################################
###  TESTANDO PRIMEIRO SOMENTE PARA O ANO DE 2022

pnad012022<- pnadc_design(read_pnadc(paste0("data/txt/PNADC_0","12022", ".txt"), 
                                     "data/documentacao/input_PNADC_trimestral.txt",vars=c("VD4002","V2005")))

##### Códigos separados

## Para o total de pessoas:

tabble <- pnad012022$variables %>%
  filter(UF == "31") %>%
  mutate(one = 1,
         ocupada = 1 * (VD4002 == 1),
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
  ) %>%
  group_by(regioes) %>%
  summarise(t.pes = n())

View(tabble)


## Para domicílios:

tabble2 <- pnad012022$variables %>%
  filter(UF == "31"&V2005 == "01") %>%
  mutate(one = 1,
         ocupada = 1 * (VD4002 == 1),
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
  ) %>%
  group_by(regioes) %>%
  summarise(t.dom = n())

View(tabble2)


### Código que une os dois resultados:
  ## Ajustado para o total de Minas Gerais

result <- pnad012022$variables %>%
  filter(UF == "31") %>%
  mutate(
    one = 1,
    ocupada = 1 * (VD4002 == 1),
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
      TRUE ~ "11 - Minas Gerais"
    )
  ) %>%
  group_by(regioes) %>%
  summarise(
    t.pes = n(),  # Total de pessoas por região
    t.dom = sum(V2005 == "01", na.rm = TRUE)  # Total de domicílios principais por região
  ) %>%
  ungroup() %>%
  reframe(
    regioes = c(regioes, "Total MG"),
    t.pes = c(t.pes, sum(t.pes, na.rm = TRUE)),  
    t.dom = c(t.dom, sum(t.dom, na.rm = TRUE))  
  )

View(result)



################################################################################
### Generalizando a função para todas as PNADS

calculo_amostra <- function(mesano){
  pnadc <- pnadc_design(read_pnadc(paste0("data/txt/PNADC_0", mesano, ".txt"), 
                                   "data/documentacao/input_PNADC_trimestral.txt", vars=c("VD4002", "V2005", "Estrato", "UF"))) 
  ## Tentando mudar o formato do objeto para conseguir generalizar a função:
    ## O problema estava ocorrendo no filtro e depois no formato do objeto
  
  pnadc_regular <- as_survey(pnadc)
  
  pnadc_regular <- pnadc_regular %>%
    filter(UF == "31") %>%
    mutate(
      one = 1,
      ocupada = 1 * (VD4002 == 1),
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
        TRUE ~ "11 - Minas Gerais"
      )
    ) %>%
    group_by(regioes) %>%
    summarise(
      t.pes = n(),
      t.dom = sum(V2005 == "01", na.rm = TRUE)
    ) %>%
    ungroup() %>%
    reframe(
      regioes = c(regioes, "Total MG"),
      t.pes = c(t.pes, sum(t.pes, na.rm = TRUE)),  
      t.dom = c(t.dom, sum(t.dom, na.rm = TRUE))  
    )
  
  ## Transformando os resultados em data frames separados:
  
  df_amostra_pes<-pnadc_regular %>% select(regioes,t.pes)
  df_amostra_dom<-pnadc_regular %>% select(regioes,t.dom)
  
  df_amostra_pes<-as.data.frame(df_amostra_pes)
  df_amostra_dom<-as.data.frame(df_amostra_dom)
  
  df_amostra_pes$Periodo<-mesano
  df_amostra_dom$Periodo<-mesano
  
  gc()
  
  return(list(df_amostra_pes = df_amostra_pes, df_amostra_dom = df_amostra_dom))
}


  ## Lista conforme anterioremente:

lista <- c(012012,012013,012014,012015,012016,012017,012018,012019,012020,012021,012022,012023,012024,
           022012,022013,022014,022015,022016,022017,022018,022019,022020,022021,022022,022023,022024,
          032012,032013,032014,032015,032016,032017,032018,032019,032020,032021,032022,032023,032024,
          042012,042013,042014,042015,042016,042017,042018,042019,042020,042021,042022,042023)

#lista2<-c(012022,022022)


## Juntando os resultados:

# tam_amos<- bind_rows(lapply(lista,calculo_amostra)) #rode apenas para gerar os dfs e armazenar as informações
tam_amos_pes <- bind_rows(tam_amos$df_amostra_pes)
tam_amos_dom <- bind_rows(tam_amos$df_amostra_dom)


## "Girando" o df para que cada coluna seja um estrato -> para facilitar a chamada do gráfico
  ## Também somando a coluna final como MG

pessoas<-tam_amos_pes %>%
  pivot_wider(names_from = regioes,values_from = t.pes) %>%
  rowwise() %>%
  ungroup()

View(pessoas)


domicilios<-tam_amos_dom %>%
  pivot_wider(names_from = regioes,values_from = t.dom) %>%
  rowwise() %>%
  ungroup()

View(domicilios)

gc()


## Reorganizando os DFs gerados:

ordemlinhas<- c(1,14,27,40,
                2,15,28,41,
                3,16,29,42,
                4,17,30,43,
                5,18,31,44,
                6,19,32,45,
                7,20,33,46,
                8,21,34,47,
                9,22,35,48,     # Lista coletada do script de "leituras"
                10,23,36,49,
                11,24,37,50,
                12,25,38,51,
                13,26,39)


am.pessoas<-pessoas[ordemlinhas,]

am.domicilios<-domicilios[ordemlinhas,]

View(am.pessoas)


#write_xlsx(am.pessoas, "D:/FJP2425/Programacao/data/tampessoas3t2024.xlsx")
#write_xlsx(am.domicilios, "D:/FJP2425/Programacao/data/tamdom3t2024.xlsx")




################################################################################
### Transformando as colunas do DF em ts e fazendo os gráficos:

## Total de pessoas:
  ## Lembrar de fazer o upload da base

am.pessoas <- read_xlsx("D:/FJP2425/Programacao/data/tampessoas3t2024.xlsx")

{pes_mg<-ts(am.pessoas$`Total MG`[1:51],start=c(2012,1),frequency = 4)
  pes_bh<-ts(am.pessoas$`01-Belo Horizonte`[1:51],start=c(2012,1),frequency = 4)
  pes_entorno<-ts(am.pessoas$`02-Entorno metropolitono de BH`[1:51],start=c(2012,1),frequency = 4)
  pes_colar<-ts(am.pessoas$`03-Colar metropolitano de BH`[1:51],start=c(2012,1),frequency = 4)
  pes_RIDE<-ts(am.pessoas$`04-RIDE de Brasília em Minas`[1:51],start=c(2012,1),frequency = 4)
  pes_sul<-ts(am.pessoas$`05-Sul de Minas`[1:51],start=c(2012,1),frequency = 4)
  pes_trng<-ts(am.pessoas$`06-Triângulo Mineiro`[1:51],start=c(2012,1),frequency = 4)
  pes_mata<-ts(am.pessoas$`07-Mata de Minas Gerais`[1:51],start=c(2012,1),frequency = 4)
  pes_norte<-ts(am.pessoas$`08-Norte de Minas`[1:51],start=c(2012,1),frequency = 4)
  pes_vale<-ts(am.pessoas$`09-Vale do Rio Doce`[1:51],start=c(2012,1),frequency = 4)
  pes_central<-ts(am.pessoas$`10-Central`[1:51],start=c(2012,1),frequency = 4)
}


plot.ts(pes_mg, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="Total Minas Gerais",lwd=2)

plot.ts(pes_bh, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="1 - Belo Horizonte",lwd=2)

plot.ts(pes_entorno,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="2 - Entorno metropolitano de bh", lwd=2)

plot.ts(pes_colar,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="3 - Colar metropolitano de bh", lwd=2)

plot.ts(pes_RIDE,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="4 - RIDE de Brasília em Minas", lwd=2)

plot.ts(pes_sul,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="5 - Sul de Minas", lwd=2)

plot.ts(pes_trng,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="6 - Triângulo Mineiro", lwd=2)

plot.ts(pes_mata,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="7 - Zona da Mata", lwd=2)

plot.ts(pes_norte,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="8 - Norte de Minas Gerais", lwd=2)

plot.ts(pes_vale,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="9 - Vale do Rio Doce", lwd=2)

plot.ts(pes_central,col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="10 - central", lwd=2)


{seriespes<-cbind(pes_bh,pes_entorno,pes_colar,pes_RIDE,pes_sul,pes_trng,pes_mata,pes_norte,pes_vale,pes_central)
  colnames(seriespes)<-c("BH", "Entorno BH", "Colar BH", "RIDE", "Sul", "Triângulo", "Mata", "Norte MG", "Rio doce", "Central" )
}

{cores <- c("black", "blue", "red", "green", "purple", "orange", "brown", "pink", "cyan", "yellow")
  par(xpd = TRUE, mar = c(5, 4, 4, 8))
  plot.ts(seriespes,plot.type = "single",main="Evolução do tamanho da amostra de pessoas por estrato",
          col=cores,
          xlab="Ano",ylab="Número de pessoas",lwd=2)
    legend("bottomright",inset = c(-0.14, -0),legend=colnames(seriespes),col=cores,lwd = 1,cex=0.8)
}



### Total de domicílios

am.domicilios <- read_xlsx("D:/FJP2425/Programacao/data/tamdom3t2024.xlsx")

{dom_mg<-ts(am.domicilios$`Total MG`[1:51],start=c(2012,1),frequency = 4)
dom_bh<-ts(am.domicilios$`01-Belo Horizonte`[1:51],start=c(2012,1),frequency = 4)
dom_entorno<-ts(am.domicilios$`02-Entorno metropolitono de BH`[1:51],start=c(2012,1),frequency = 4)
dom_colar<-ts(am.domicilios$`03-Colar metropolitano de BH`[1:51],start=c(2012,1),frequency = 4)
dom_RIDE<-ts(am.domicilios$`04-RIDE de Brasília em Minas`[1:51],start=c(2012,1),frequency = 4)
dom_sul<-ts(am.domicilios$`05-Sul de Minas`[1:51],start=c(2012,1),frequency = 4)
dom_trng<-ts(am.domicilios$`06-Triângulo Mineiro`[1:51],start=c(2012,1),frequency = 4)
dom_mata<-ts(am.domicilios$`07-Mata de Minas Gerais`[1:51],start=c(2012,1),frequency = 4)
dom_norte<-ts(am.domicilios$`08-Norte de Minas`[1:51],start=c(2012,1),frequency = 4)
dom_vale<-ts(am.domicilios$`09-Vale do Rio Doce`[1:51],start=c(2012,1),frequency = 4)
dom_central<-ts(am.domicilios$`10-Central`[1:51],start=c(2012,1),frequency = 4)
}


plot.ts(dom_mg, col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="Total Minas Gerais",lwd=2)

plot.ts(dom_bh, col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="1 - Belo Horizonte",lwd=2)

plot.ts(dom_entorno,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="2 - Entorno metropolitano de bh", lwd=2)

plot.ts(dom_colar,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="3 - Colar metropolitano de bh", lwd=2)

plot.ts(dom_RIDE,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="4 - RIDE de Brasília em Minas", lwd=2)

plot.ts(dom_sul,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="5 - Sul de Minas", lwd=2)

plot.ts(dom_trng,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="6 - Triângulo Mineiro", lwd=2)

plot.ts(dom_mata,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="7 - Zona da Mata", lwd=2)

plot.ts(dom_norte,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="8 - Norte de Minas Gerais", lwd=2)

plot.ts(dom_vale,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="9 - Vale do Rio Doce", lwd=2)

plot.ts(dom_central,col = "black", xlab="Ano", ylab="Quantidade de domicílios",main="10 - central", lwd=2)


{seriesdom<-cbind(dom_bh,dom_entorno,dom_colar,dom_RIDE,dom_sul,dom_trng,dom_mata,dom_norte,dom_vale,dom_central)
  colnames(seriesdom)<-c("BH", "Entorno BH", "Colar BH", "RIDE", "Sul", "Triângulo", "Mata", "Norte MG", "Rio doce", "Central" )
}

{cores <- c("black", "blue", "red", "green", "purple", "orange", "brown", "pink", "cyan", "yellow")
  par(xpd = TRUE, mar = c(5, 4, 4, 8))
  plot.ts(seriesdom,plot.type = "single",main="Evolução do tamanho da amostra de domicílios por estrato",
          col=cores,
          xlab="Ano",ylab="Número de domicílios",lwd=2)
  legend("bottomright",inset = c(-0.14, -0),legend=colnames(seriesdom),col=cores,lwd = 1,cex=0.8)
}


##############
### Gráficos conjuntos:

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_mg, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_mg,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("Total de Minas Gerais", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_bh, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_bh,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("1 - Belo Horizonte", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_entorno, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_entorno,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("2 - Entorno metropolitano de BH", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_colar, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_colar,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("3 - Colar metropolitano de BH", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_RIDE, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_RIDE,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("4 - RIDE de Brasília em Minas", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_sul, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_sul,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("5 - Sul de Minas", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_trng, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_trng,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("6 - Triângulo Mineiro", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_mata, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_mata,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("7 - Zona da Mata", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_norte, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_norte,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("8 - Norte de Minas Gerais", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_vale, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_vale,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("9 - Vale do Rio Doce", outer=TRUE, line =0,cex=1.5,font=2)}

{par(mfrow=c(2,1),oma = c(2, 2, 3, 2))
  par(mar = c(2, 4, 1, 2))
  plot.ts(pes_central, col = "black", xlab="Ano", ylab="Quantidade de pessoas",main="",lwd=2)
  par(mar = c(4, 4, 1, 2))
  plot.ts(dom_central,col = "blue", xlab="Ano", ylab="Quantidade de domicílios",main="", lwd=2)
  mtext("10 - Central", outer=TRUE, line =0,cex=1.5,font=2)}
