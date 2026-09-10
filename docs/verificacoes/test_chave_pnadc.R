# Testa a chave de ligação da PNADc: segue a coorte de 1ª visita de 2012Q1 pelos
# 5 trimestres consecutivos (2012Q1..2013Q1) e avalia a estabilidade das chaves.
suppressMessages({ library(PNADcIBGE); library(dplyr) })
zips <- c("E:/Dados/PNADC/2012/PNADC_012012_20250815.zip",
          "E:/Dados/PNADC/2012/PNADC_022012_20250815.zip",
          "E:/Dados/PNADC/2012/PNADC_032012_20250815.zip",
          "E:/Dados/PNADC/2012/PNADC_042012_20250815.zip",
          "E:/Dados/PNADC/2013/PNADC_012013_20250815.zip")
rot <- c("2012Q1","2012Q2","2012Q3","2012Q4","2013Q1")
input <- "E:/Dados/PNADC/documentacao/input_PNADC_trimestral.txt"
vars <- c("V1008","V1014","V1016","V2003","V2005","V2007","V2008","V20081","V20082","UF")

le <- function(zip, tri) {
  td <- file.path(tempdir(), paste0("k_", tri)); unzip(zip, exdir = td)
  txt <- list.files(td, "[.]txt$", recursive = TRUE, full.names = TRUE); txt <- txt[which.max(file.size(txt))]
  d <- read_pnadc(txt, input, vars = vars)
  unlink(td, recursive = TRUE)
  d <- d[d$UF == "31", ]
  data.frame(
    tri  = tri,
    hh   = paste(d$UPA, d$V1008, d$V1014, sep = "_"),   # chave do DOMICÍLIO
    V1016= as.integer(as.character(d$V1016)),
    ord  = as.character(d$V2003),                        # nº de ordem da pessoa
    sexo = as.character(d$V2007),
    nasc = paste(d$V2008, d$V20081, d$V20082, sep = "-") # data de nascimento
  )
}
df <- bind_rows(Map(le, zips, rot))
cat("linhas MG por trimestre:\n"); print(table(df$tri))

# coorte: domicílios em 1ª visita no 2012Q1
coorte_hh <- unique(df$hh[df$tri == "2012Q1" & df$V1016 == 1])
cat("\nDomicílios em 1a visita (V1016==1) no 2012Q1:", length(coorte_hh), "\n")

cat("\n=== (A) CHAVE DO DOMICÍLIO: retenção da coorte por trimestre ===\n")
for (t in rot) {
  hh_t <- unique(df$hh[df$tri == t])
  achados <- sum(coorte_hh %in% hh_t)
  # V1016 esperado = posição na sequência
  v1016_moda <- names(sort(table(df$V1016[df$tri == t & df$hh %in% coorte_hh]), decreasing = TRUE))[1]
  cat(sprintf("%s: %d/%d domicílios da coorte presentes (%.1f%%) | V1016 dominante = %s\n",
              t, achados, length(coorte_hh), 100*achados/length(coorte_hh), v1016_moda))
}

cat("\n=== (B) CHAVE DE PESSOA: o nº de ordem (V2003) se mantém p/ a mesma pessoa? ===\n")
# nos domicílios da coorte, parear pessoas por (sexo, nascimento) e ver se a 'ord' é estável
d2 <- df %>% filter(hh %in% coorte_hh)
# pessoa identificada por hh + sexo + nascimento (assumindo sem gêmeos de mesmo sexo/data)
pes <- d2 %>% group_by(hh, sexo, nasc) %>%
  summarise(n_visitas = n(), ordens_distintas = n_distinct(ord), .groups = "drop") %>%
  filter(n_visitas >= 2)   # só quem aparece em 2+ visitas
cat("Pessoas (hh×sexo×nasc) presentes em 2+ visitas:", nrow(pes), "\n")
cat("Com nº de ordem ESTÁVEL (mesma 'ord' em todas as visitas):",
    round(mean(pes$ordens_distintas == 1)*100, 1), "%\n")
cat("Com nº de ordem QUE MUDOU entre visitas:",
    round(mean(pes$ordens_distintas > 1)*100, 1), "%\n")
# visão inversa: um mesmo (hh, ord) aponta p/ pessoas diferentes ao longo das visitas?
slot <- d2 %>% group_by(hh, ord) %>%
  summarise(n_visitas = n(), pessoas_distintas = n_distinct(paste(sexo, nasc)), .groups = "drop") %>%
  filter(n_visitas >= 2)
cat("\nSlots (hh×ordem) em 2+ visitas:", nrow(slot), "\n")
cat("Slots em que a MESMA ordem aponta p/ pessoas DIFERENTES:",
    round(mean(slot$pessoas_distintas > 1)*100, 1), "%\n")
