################################################################################
## 44_confere_numeros.R
##
## Confere os numeros AFIRMADOS NA PROSA do manuscrito contra os resultados da
## rodada C, recalculados aqui a partir dos .rds e dos CSVs -- nao lendo as
## tabelas .tex. A distincao importa: a conferencia de 08/11/2026 comparava
## texto contra tabela, e um erro na tabela passaria pelos dois lados. Aqui a
## referencia e independente.
##
## Cada afirmacao do texto entra como uma linha de conf(): rotulo, valor que o
## manuscrito imprime, valor recalculado e tolerancia (a folga do arredondamento
## que o texto usa). Se um numero deixar de bater -- porque alguem reestimou,
## trocou a janela ou mexeu no pipeline --, o script falha e diz qual.
##
## USO
##   Rscript rotinas/44_confere_numeros.R
##   REPO_RAIZ=... Rscript rotinas/44_confere_numeros.R
##   ARTIGO_TEX=/caminho/econbr-sample.tex Rscript rotinas/44_confere_numeros.R
##
## Com ARTIGO_TEX, roda tambem uma varredura: extrai todo numero da prosa do
## .tex e lista os que nao aparecem entre os valores conferidos. E uma REDE, nao
## um teste -- anos, contagens e numeros de secao caem nessa lista sem serem
## erro. Serve para flagrar numero novo que entrou no texto sem conferencia.
## O manuscrito vive no Overleaf; baixe o fonte se quiser essa parte.
##
## NAO confere: texto, citacoes, referencias cruzadas, nem os numeros que estao
## so dentro das tabelas (esses saem do 25_ e do 41_/42_/43_, que os geram).
##
## Insumos: outputs/univariado_final/, outputs/multivariado_final/,
##          outputs/taxa_final/, outputs/validacao_escala/,
##          outputs/robustez_2026-11-10/, outputs/teste_especificacoes.csv,
##          data/baseestr8reg.rds
## Saida  : relatorio no console; status 1 se alguma afirmacao falhar.
################################################################################
suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
BURN <- 8; P <- 8
B_NIVEL <- 1:8; B_SAZ1 <- 17:24; B_SAZ3 <- 33:40
IDX <- (BURN + 1):52                      # 2014T1 a 2024T4, a janela do artigo

## ---------------------------------------------------------------- utilitario
.res <- new.env(parent = emptyenv()); .res$linhas <- list()
conf <- function(rotulo, texto, calculado, tol = 0.05) {
  ok <- is.finite(calculado) && abs(texto - calculado) <= tol
  .res$linhas[[length(.res$linhas) + 1]] <-
    list(rotulo = rotulo, texto = texto, calc = calculado, ok = ok)
  cat(sprintf("  [%s] %-58s texto %-9s calculado %s\n",
              if (ok) "ok" else "FALHA", rotulo,
              format(texto, nsmall = 0), format(round(calculado, 4), nsmall = 0)))
  invisible(ok)
}
secao <- function(x) cat("\n===", x, "===\n")

## --------------------------------------------------------- carga dos modelos
carrega <- function(ind) {
  uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final",
                           paste0("modelos_", ind, ".rds")))
  cod <- names(uni)
  Y  <- sapply(cod, function(k) uni[[k]]$serie$y)
  SE <- sapply(cod, function(k) uni[[k]]$serie$se)
  m  <- readRDS(file.path(RAIZ, "outputs", "multivariado_final",
                          paste0("multivariado_", ind, ".rds")))
  flt <- dlmFilter(Y, m$mod); mse <- dlmSvd2var(flt$U.C, flt$D.C)
  est <- dropFirst(flt$m)
  list(uni = uni, cod = cod, Y = Y, SE = SE, m = m,
       tr = est[, B_NIVEL],
       se_tr = sapply(B_NIVEL, function(i)
         dropFirst(sapply(mse, function(x) sqrt(x[i, i])))),
       ## mse alinhado com dropFirst: necessario para a covariancia ENTRE
       ## estratos, que entra nas comparacoes entre pares (Holm) e nos
       ## residuos padronizados usados no teste de normalidade sem 2020.
       mse = mse[-1], flt = flt,
       sin = est[, B_NIVEL] + est[, B_SAZ1] + est[, B_SAZ3])
}
D <- carrega("desocupados"); O <- carrega("ocupados"); T. <- carrega("taxa")

cobertura <- function(d) {
  lo <- d$Y[IDX, ] - 1.96 * d$SE[IDX, ]; hi <- d$Y[IDX, ] + 1.96 * d$SE[IDX, ]
  sum(d$sin[IDX, ] >= lo & d$sin[IDX, ] <= hi)
}
cv_dir <- function(d) colMeans(100 * d$SE[IDX, ] / abs(d$Y[IDX, ]))
cv_mv  <- function(d) colMeans(100 * d$se_tr[IDX, ] / abs(d$tr[IDX, ]))
cv_uni <- function(d) sapply(d$cod, function(k) {
  u <- d$uni[[k]]$corrigido; mean(100 * u$se_trend[IDX] / abs(u$trend[IDX]))
})
dr_mv  <- function(d) colMeans((d$SE[IDX, ] - d$se_tr[IDX, ]) / d$SE[IDX, ]) * 100
dr_uni <- function(d) sapply(d$cod, function(k) d$uni[[k]]$rrse)
vr_mv  <- function(d) colSums(d$sin[IDX, ] - d$Y[IDX, ]) / colSums(d$Y[IDX, ]) * 100
vr_uni <- function(d) sapply(d$cod, function(k) d$uni[[k]]$vicio)

## DR medio como o PIPELINE gravou (e como as tabelas imprimem), para os casos
## em que a terceira casa decide o arredondamento do texto
SR <- read.csv(file.path(RAIZ, "outputs/robustez_2026-11-10/sintese_rodadas.csv"),
               stringsAsFactors = FALSE)
dr_pipe <- function(ind, mod = "multivariado", va = "C") {
  x <- SR$DR[SR$variante == va & SR$indicador == ind & SR$modelo == mod]
  if (length(x)) x[1] else NA_real_
}

## =============================================================== SECAO 2
secao("Secao 2 -- os estratos")
be <- readRDS(file.path(RAIZ, "data", "baseestr8reg.rds"))
media_taxa <- function(nm) {
  x <- be[[nm]]; p <- as.character(x[[1]]); ok <- p >= "2012_01" & p <= "2024_04"
  tx <- x[[8]][ok]; if (max(tx, na.rm = TRUE) < 1) tx <- tx * 100
  mean(tx, na.rm = TRUE)
}
conf("taxa media do periodo, Triangulo", 6.69, media_taxa(names(be)[4]), 0.005)
conf("taxa media do periodo, Colar",    11.39, media_taxa(names(be)[2]), 0.005)
conf("taxa media do periodo, Norte",    10.99, media_taxa(names(be)[6]), 0.005)

mg <- be[["09 - Minas Gerais"]]; pm <- as.character(mg[[1]])
tmg <- mg[[8]][pm == "2024_04"]; if (tmg < 1) tmg <- tmg * 100
conf("taxa de MG no 4o tri de 2024 (Introducao)", 4.3, tmg, 0.005)

conf("CV medio da direta, ocupados, Belo Horizonte", 1.8, cv_dir(O)[1], 0.05)
conf("CV medio da direta, ocupados, Triangulo",      5.9, cv_dir(O)[4], 0.05)

## =============================================================== SECAO 3
secao("Secao 3 -- especificacao")
ml <- read.csv(file.path(RAIZ, "outputs/robustez_2026-11-10/escala/validacao_escala_multistart.csv"),
               stringsAsFactors = FALSE)
conf("mediana de lambda (multistart)", 1.03, median(ml$lambda), 0.005)
conf("rejeicoes de lambda = 1 a 5%", 2, sum(ml$p < 0.05), 0)

esp <- read.csv(file.path(RAIZ, "outputs/teste_especificacoes.csv"), stringsAsFactors = FALSE)
sp <- split(esp, paste(esp$ind, esp$estrato))
suave <- sapply(sp, function(x) x[which.min(x$bic), ]$sL < 1e-6)
conf("BIC prefere tendencia suave (de 24)", 20, sum(suave), 0)

## restricao sazonal: quantas combinacoes a imposicao sS = 0 nao muda
delta_S <- sapply(sp, function(x) {
  liv <- x[x$sL < 1e-6 & x$sS > 1e-6, ]; res <- x[x$sL < 1e-6 & x$sS < 1e-6, ]
  if (!nrow(liv) || !nrow(res)) return(NA_real_)
  max(liv$ll) - max(res$ll)
})
conf("sazonal: combinacoes sem perda relevante (de 24)", 23,
     sum(delta_S <= 1.92, na.rm = TRUE) + sum(is.na(delta_S)), 0)
nrt <- esp[esp$ind == "taxa" & grepl("Norte", esp$estrato), ]
conf("perda de logLik ao impor sS=0 na taxa do Norte", 3.99,
     max(nrt$ll[nrt$sL < 1e-6 & nrt$sS > 1e-6]) -
       max(nrt$ll[nrt$sL < 1e-6 & nrt$sS < 1e-6]), 0.005)
conf("Ljung-Box da taxa do Norte com sS livre", 0.13,
     nrt$lb[nrt$sL < 1e-6 & nrt$sS > 1e-6][1], 0.005)

## perfil do irregular: o texto diz "menos de 0,3"
plano <- unlist(lapply(c("desocupados", "ocupados", "taxa"), function(ind) {
  uu <- readRDS(file.path(RAIZ, "outputs/univariado_final", paste0("modelos_", ind, ".rds")))
  sapply(uu, function(u) abs(u$corrigido$plano_I))
}))
cat(sprintf("  [%s] %-58s maximo %.4f\n",
            if (max(plano) < 0.3) "ok" else "FALHA",
            "perfil do irregular abaixo de 0,3", max(plano)))
.res$linhas[[length(.res$linhas) + 1]] <-
  list(rotulo = "perfil do irregular < 0,3", texto = 0.3, calc = max(plano),
       ok = max(plano) < 0.3)

## =============================================================== SECAO 4.1
secao("Secao 4.1 -- total de desocupados")
conf("IC95: pontos dentro (de 352)", 345, cobertura(D), 0)
conf("DR medio univariado",  12.1, mean(dr_uni(D)))
conf("DR medio multivariado", 34.3, mean(dr_mv(D)))
conf("DR multivariado, Triangulo", 51.1, dr_mv(D)[4])
conf("DR multivariado, Central",   40.5, dr_mv(D)[8])
conf("DR multivariado, Sul",       38.6, dr_mv(D)[3])
conf("DR multivariado, Zona da Mata", 19.4, dr_mv(D)[5])
conf("multivariado supera univariado (de 8)", 8, sum(dr_mv(D) > dr_uni(D)), 0)
conf("vicio relativo maximo, em modulo", 3.73,
     max(abs(c(vr_mv(D), vr_uni(D)))), 0.01)
conf("univariado com vicio menor (de 8)", 7, sum(abs(vr_uni(D)) < abs(vr_mv(D))), 0)
conf("CV multivariado, maximo pontual", 14.3,
     max(100 * D$se_tr[IDX, ] / abs(D$tr[IDX, ])), 0.05)
conf("CV medio da direta, Triangulo", 13.1, cv_dir(D)[4])
conf("CV medio multivariado, Triangulo", 6.5, cv_mv(D)[4])
conf("CV medio da direta, Belo Horizonte", 7.7, cv_dir(D)[1])
conf("CV medio multivariado, Belo Horizonte", 5.4, cv_mv(D)[1])
cd <- D$m$Corr_R
conf("correlacao mediana", 0.97, median(cd[lower.tri(cd)]), 0.005)
conf("correlacao minima fora da Zona da Mata", 0.86,
     min(cd[lower.tri(cd)][cd[lower.tri(cd)] > 0.7]), 0.01)
conf("Zona da Mata, correlacao minima", 0.17, min(cd[5, -5]), 0.005)
conf("Zona da Mata, correlacao maxima", 0.64, max(cd[5, -5]), 0.005)
conf("1o autovalor", 206.23, D$m$autovalores[1], 0.005)
conf("2o autovalor",  11.64, D$m$autovalores[2], 0.005)
conf("3o autovalor",   0.50, D$m$autovalores[3], 0.005)
conf("4o autovalor",   0.05, D$m$autovalores[4], 0.005)
conf("autovalores abaixo de 1e-3", 4, sum(D$m$autovalores < 1e-3), 0)
conf("cobertura do IC95 em %%", 98.0, 100 * cobertura(D) / 352, 0.05)
## coeficiente dos processos AR(1); o texto da a faixa
ar1 <- unlist(lapply(D$cod, function(k)
  if (identical(D$uni[[k]]$processo, "AR(1)")) D$uni[[k]]$phi else NULL))
conf("coeficiente AR(1) minimo", 0.220, min(ar1), 0.0005)
conf("coeficiente AR(1) maximo", 0.560, max(ar1), 0.0005)

## =============================================================== SECAO 4.2
secao("Secao 4.2 -- total de ocupados")
conf("IC95: pontos dentro (de 352)", 347, cobertura(O), 0)
conf("DR medio univariado",  19.7, mean(dr_uni(O)))
conf("DR medio multivariado", 25.5, mean(dr_mv(O)))
conf("DR multivariado, Belo Horizonte", 5.0, dr_mv(O)[1])
conf("DR multivariado, Colar",          9.1, dr_mv(O)[2])
conf("DR multivariado, Triangulo",     37.7, dr_mv(O)[4])
conf("DR multivariado, Central",       35.6, dr_mv(O)[8])
conf("DR univariado, Central",         22.0, dr_uni(O)[8])
conf("DR multivariado, Norte",         29.0, dr_mv(O)[6])
conf("DR univariado, Norte",           18.9, dr_uni(O)[6])
conf("multivariado supera univariado (de 8)", 6,
     sum(dr_mv(O) - dr_uni(O) > 0.01), 0)
conf("multivariado iguala univariado (de 8)", 2,
     sum(abs(dr_mv(O) - dr_uni(O)) <= 0.01), 0)
conf("CV medio da direta, Belo Horizonte", 1.8, cv_dir(O)[1])
conf("CV medio multivariado, Belo Horizonte", 1.7, cv_mv(O)[1])
conf("CV medio da direta, Colar", 2.2, cv_dir(O)[2])
conf("CV medio multivariado, Colar", 2.0, cv_mv(O)[2])
co <- O$m$Corr_R
conf("correlacao minima", 0.24, min(co[lower.tri(co)]), 0.005)
conf("correlacao maxima", 0.98, max(co[lower.tri(co)]), 0.005)
conf("correlacao mediana", 0.87, median(co[lower.tri(co)]), 0.005)
conf("Zona da Mata, correlacao maxima", 0.69, max(co[5, -5]), 0.005)
conf("1o autovalor", 654.99, O$m$autovalores[1], 0.005)
ev <- eigen(co, only.values = TRUE)$values
conf("1o autovalor da matriz de correlacao, %% da soma", 85,
     100 * ev[1] / sum(ev), 0.5)
## normalidade rejeitada em 5 dos 8 estratos (Shapiro-Wilk do univariado)
sw <- sapply(O$cod, function(k) O$uni[[k]]$corrigido$shapiro)
conf("normalidade rejeitada a 5% (de 8)", 5, sum(sw < 0.05), 0)
conf("cobertura do IC95 em %%", 98.6, 100 * cobertura(O) / 352, 0.05)
## variancia da inovacao do erro amostral: 1 - phi^2, pela condicao V(e~)=1
inov <- sapply(O$cod, function(k) 1 - O$uni[[k]]$phi[1]^2)
conf("variancia da inovacao: minimo", 0.39, min(inov), 0.005)
conf("variancia da inovacao: maximo", 0.58, max(inov), 0.005)
conf("processos AR(1) entre os oito estratos", 8,
     sum(sapply(O$cod, function(k) identical(O$uni[[k]]$processo, "AR(1)"))), 0)

## =============================================================== SECAO 4.3
secao("Secao 4.3 -- taxa de desocupacao")
conf("IC95: pontos dentro (de 352)", 347, cobertura(T.), 0)
conf("DR medio univariado",  11.5, mean(dr_uni(T.)))
## O DR multivariado da taxa e o unico caso em que a terceira casa decide o
## arredondamento: o pipeline grava 35,75, que o texto imprime como 35,8; o
## recalculo daqui da 35,7485, que arredondaria para 35,7. A diferenca de
## 0,0015 vem do caminho de calculo (serie gravada x refiltrada), nao do
## resultado. Por isso a afirmacao do texto e conferida contra o valor do
## pipeline -- que e o que a Tabela imprime -- e o recalculo entra como um
## segundo teste, de que os dois caminhos concordam.
conf("DR medio multivariado (valor do pipeline)", 35.8, dr_pipe("taxa"), 0.051)
conf("recalculo do DR multivariado bate com o pipeline",
     dr_pipe("taxa"), mean(dr_mv(T.)), 0.01)

TL <- D$tr[IDX, ] + O$tr[IDX, ]
r  <- D$tr[IDX, ] / TL
v  <- (O$tr[IDX, ]^2 * D$se_tr[IDX, ]^2 + D$tr[IDX, ]^2 * O$se_tr[IDX, ]^2) / TL^4
cvi <- colMeans(100 * sqrt(v) / r)
cvd <- cv_dir(T.); cvp <- cv_mv(T.)
conf("CV medio da estimativa direta", 10.0, mean(cvd))
conf("CV medio do calculo indireto",   7.0, mean(cvi))
conf("CV medio da propria taxa",       6.4, mean(cvp))
conf("propria taxa melhor que a indireta (de 8)", 7, sum(cvp < cvi), 0)
conf("Zona da Mata: direta",   10.8, cvd[5])
conf("Zona da Mata: indireta",  9.6, cvi[5])
conf("Zona da Mata: propria",   8.3, cvp[5])
conf("Vale do Rio Doce: direta",   10.6, cvd[7])
conf("Vale do Rio Doce: indireta",  8.6, cvi[7])
conf("Vale do Rio Doce: propria",   7.3, cvp[7])

## leitura regional (Tabela 12), do 33_tabela_regional_taxa.R
tr <- read.csv(file.path(RAIZ, "outputs/taxa_final/tabela_regional_taxa.csv"),
               stringsAsFactors = FALSE, encoding = "UTF-8")
alta <- c(2, 6, 7)                                  # Colar, Norte, Vale
bh   <- 1
conf("pico da crise em 2017T3 (BH e Colar)", 2,
     sum(tr$tri_pico_crise[c(bh, 2)] == "2017T3"), 0)
conf("pico da crise em 2017T1 (demais)", 6, sum(tr$tri_pico_crise == "2017T1"), 0)
conf("pico da pandemia em 2020T3", 7, sum(tr$tri_pico_covid == "2020T3"), 0)
conf("elevacao minima no grupo alto + BH", 5.8, min(tr$var_crise_pp[c(bh, alta)]), 0.05)
conf("elevacao maxima no grupo alto + BH", 8.7, max(tr$var_crise_pp[c(bh, alta)]), 0.05)
conf("elevacao minima nos demais", 3.6, min(tr$var_crise_pp[-c(bh, alta)]), 0.05)
conf("elevacao maxima nos demais", 5.3, max(tr$var_crise_pp[-c(bh, alta)]), 0.05)
conf("queda minima no grupo alto + BH", 9.8, min(abs(tr$var_covid_fim_pp[c(bh, alta)])), 0.05)
conf("queda maxima no grupo alto + BH", 12.5, max(abs(tr$var_covid_fim_pp[c(bh, alta)])), 0.05)
conf("queda minima nos demais", 6.0, min(abs(tr$var_covid_fim_pp[-c(bh, alta)])), 0.05)
conf("queda maxima nos demais", 8.6, max(abs(tr$var_covid_fim_pp[-c(bh, alta)])), 0.05)
conf("estratos abaixo do nivel de 2014 em 2024T4", 8,
     sum(tr$fim_2024T4 < tr$pre_2014T4), 0)
conf("Vale do Rio Doce em 2024T4", 4.8, tr$fim_2024T4[7], 0.05)
conf("Sul de Minas em 2024T4", 3.1, tr$fim_2024T4[3], 0.05)
conf("patamar alto: minimo", 5.8, min(tr$fim_2024T4[c(1, 2, 6)]), 0.05)
conf("patamar alto: maximo", 6.3, max(tr$fim_2024T4[c(1, 2, 6)]), 0.05)

pares_modelo <- length(unique(unlist(lapply(seq_len(nrow(tr)), function(i) {
  o <- trimws(strsplit(tr$difere_de[i], ",")[[1]]); o <- o[nzchar(o)]
  vapply(o, function(x) paste(sort(c(sprintf("%02d", i), x)), collapse = "-"), character(1))
}))))
pares_direta <- 0
for (i in 1:7) for (j in (i + 1):8) {
  if (abs(tr$direta_2024T4[i] - tr$direta_2024T4[j]) >
      1.96 * sqrt(tr$se_direta_2024T4[i]^2 + tr$se_direta_2024T4[j]^2)) {
    pares_direta <- pares_direta + 1
  }
}
conf("pares distinguiveis pelo modelo (de 28)", 18, pares_modelo, 0)
conf("pares distinguiveis pelas diretas (de 28)", 5, pares_direta, 0)

## =============================================================== CONCLUSAO
secao("Conclusao -- robustez")
conf("DR univariado da rodada C, desocupados", 12.1,
     dr_pipe("desocupados", "univariado", "C"))

## SAIRAM DO MANUSCRITO EM 20/11/2026, por decisao do Caio, e por isso deixaram
## de ser conferidas aqui: a DR com a tendencia escolhida caso a caso (12,4), a
## DR com c_t multiplicado por 0,8 e por 1,2 (7,1 e 17,0) e todo o bloco do
## vies de grupo de rotacao (MDE de 8% a 15%, vies de 3,5% a 5,2%). Os numeros
## continuam nos outputs; as afirmacoes nao existem mais na prosa.

## =============================================================== 20/11/2026
secao("Apendices e acrescimos de 20/11/2026")

## -- custo das restricoes: Secao 3 e Tabela A.2 do Apendice A ---------------
es <- read.csv(file.path(RAIZ, "outputs", "teste_especificacoes.csv"),
               stringsAsFactors = FALSE)
ew <- reshape(es[, c("ind", "estrato", "espec", "ll")],
              idvar = c("ind", "estrato"), timevar = "espec", direction = "wide")
## leituras CONDICIONAIS, que sao as que a tabela reporta:
##   C -> D  custo de impor sigma2_S = 0 dada a tendencia suave
##   B -> D  custo de impor sigma2_L = 0 dada a sazonalidade deterministica
conf("sazonalidade deterministica: casos acima de 1,92", 1,
     sum(ew$ll.C - ew$ll.D > 1.92), 0)
conf("custo maximo da sazonalidade deterministica", 3.99,
     max(ew$ll.C - ew$ll.D), 0.01)
conf("tendencia suave: casos acima de 1,92", 4,
     sum(ew$ll.B - ew$ll.D > 1.92), 0)

## -- multiplicidade: Secao 4.4, conclusao e nota da Tabela 9 ----------------
V_fim <- T.$mse[[52]][B_NIVEL, B_NIVEL]
pm <- pd <- numeric(0)
for (i in 1:7) for (j in (i + 1):8) {
  dm <- T.$tr[52, i] - T.$tr[52, j]
  pm <- c(pm, 2 * pnorm(-abs(dm) / sqrt(V_fim[i, i] + V_fim[j, j] - 2 * V_fim[i, j])))
  dd <- T.$Y[52, i] - T.$Y[52, j]
  pd <- c(pd, 2 * pnorm(-abs(dd) / sqrt(T.$SE[52, i]^2 + T.$SE[52, j]^2)))
}
conf("pares pelo modelo sob Holm (de 28)", 11, sum(p.adjust(pm, "holm") < 0.05), 0)
conf("pares pelas diretas sob Holm (de 28)", 1, sum(p.adjust(pd, "holm") < 0.05), 0)

## -- benchmark contra Minas Gerais: Apendice D ------------------------------
## Le o CSV do 46_, que e quem estima o modelo estadual. Aqui nao se reestima:
## o 46_ leva minutos e a conferencia tem de ser rapida.
bm_arq <- file.path(RAIZ, "outputs", "benchmark_mg.csv")
if (file.exists(bm_arq)) {
  bm <- read.csv(bm_arq, stringsAsFactors = FALSE)
  g <- function(i, col) bm[[col]][bm$ind == i]
  conf("benchmark: diferenca relativa, desocupados", -1.45, g("desocupados", "dif_rel"), 0.01)
  conf("benchmark: diferenca relativa, ocupados",    -1.56, g("ocupados", "dif_rel"), 0.01)
  conf("benchmark: diferenca relativa, taxa",        -0.20, g("taxa", "dif_rel"), 0.01)
  conf("benchmark: trimestres compativeis, desocupados", 44, g("desocupados", "dentro"), 0)
  conf("benchmark: trimestres compativeis, ocupados",    28, g("ocupados", "dentro"), 0)
  conf("benchmark: trimestres compativeis, taxa",        44, g("taxa", "dentro"), 0)
} else {
  cat("  [pulado] outputs/benchmark_mg.csv ausente -- rode o 46_benchmark_mg.R\n")
}

## -- normalidade sem 2020: Secao 4.2 ----------------------------------------
## 2012T1 = 1, logo 2020T2 = 34 e 2020T3 = 35.
ro <- as.matrix(residuals(O$flt, type = "standardized", sd = FALSE))
i1 <- IDX; i2 <- setdiff(IDX, c(34, 35))
sw <- function(ix) sum(vapply(1:8, function(i) shapiro.test(ro[ix, i])$p.value, 0) < 0.05)
conf("ocupados: rejeicoes de normalidade com 2020", 5, sw(i1), 0)
conf("ocupados: rejeicoes de normalidade sem 2020", 1, sw(i2), 0)
conf("residuo padronizado mais extremo em 2020T2", -5.1, min(ro[34, ]), 0.05)
conf("residuo padronizado menos extremo em 2020T2", -1.9, max(ro[34, ]), 0.05)
## O texto diz "p < 0,01" e nao um valor: 0,0025 cai no fio da navalha do
## arredondamento a tres casas (0,002 ou 0,003 conforme a convencao). A primeira
## versao da frase trazia 0,004, que e o p-valor COM 2020 -- erro pego por esta
## propria afirmacao.
conf("Belo Horizonte: Shapiro-Wilk sem 2020 fica abaixo de 0,01", 1,
     as.numeric(shapiro.test(ro[i2, 1])$p.value < 0.01), 0)

## =============================================================== varredura
tex <- Sys.getenv("ARTIGO_TEX", unset = "")
if (nzchar(tex) && file.exists(tex)) {
  secao("Varredura do .tex (rede, nao teste)")
  L <- readLines(tex, encoding = "UTF-8", warn = FALSE)
  L <- vapply(L, function(l) {
    pos <- gregexpr("(?<!\\\\)%", l, perl = TRUE)[[1]]
    if (pos[1] == -1) l else substr(l, 1, pos[1] - 1)
  }, character(1), USE.NAMES = FALSE)
  dentro <- FALSE; manter <- logical(length(L))
  for (i in seq_along(L)) {
    if (grepl("\\\\begin\\{(equation|align|bmatrix)", L[i])) dentro <- TRUE
    manter[i] <- !dentro
    if (grepl("\\\\end\\{(equation|align|bmatrix)", L[i])) dentro <- FALSE
  }
  txt <- paste(L[manter], collapse = " ")
  txt <- gsub("\\\\(cite[tp]?|ref)\\{[^}]*\\}", " ", txt)
  txt <- gsub("\\\\[a-zA-Z]+", " ", txt)
  nums <- unique(regmatches(txt, gregexpr("[0-9]+,[0-9]+", txt))[[1]])
  ## compara por VALOR, nao por grafia: "5,0" e 5, "0,50" e 0,5 sao o mesmo
  ## numero, e compara-los como texto encheria a lista de falso positivo.
  val <- as.numeric(sub(",", ".", nums, fixed = TRUE))
  conferidos <- vapply(.res$linhas, function(x) as.numeric(x$texto), numeric(1))
  bate <- vapply(val, function(v)
    any(abs(conferidos - v) < 1e-9), logical(1))
  falta <- nums[!bate]
  cat("  numeros decimais na prosa:", length(nums),
      "| nao conferidos aqui:", length(falta), "\n")
  if (length(falta)) cat("   ", paste(falta, collapse = "  "), "\n")
  cat("  (limiares, multiplicadores e constantes do metodo caem aqui sem serem erro)\n")
} else {
  cat("\n(ARTIGO_TEX nao definido: varredura do .tex pulada)\n")
}

## =============================================================== resumo
n  <- length(.res$linhas)
nf <- sum(!vapply(.res$linhas, function(x) isTRUE(x$ok), logical(1)))
cat(sprintf("\n================ %d afirmacoes conferidas, %d falha(s) ================\n",
            n, nf))
if (nf) {
  for (x in .res$linhas) if (!isTRUE(x$ok)) {
    cat(sprintf("  FALHA  %-58s texto %s, calculado %s\n",
                x$rotulo, x$texto, round(x$calc, 4)))
  }
  quit(status = 1)
}
