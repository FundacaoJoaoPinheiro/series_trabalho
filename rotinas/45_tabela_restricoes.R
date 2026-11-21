################################################################################
## 45_tabela_restricoes.R
##
## Tabela do Apendice A com o CUSTO EM LOG-VEROSSIMILHANCA das duas restricoes
## que definem a especificacao adotada: sigma2_L = 0 (tendencia suave) e
## sigma2_S = 0 (sazonalidade deterministica).
##
## Por que existe (20/11/2026): na leitura como parecerista, o vazio mais
## visivel do artigo era impor as duas restricoes sem reportar evidencia
## nenhuma. O material existia -- o 15_comparacao_formulacoes.R ja havia
## estimado as quatro especificacoes para as 24 combinacoes --, mas so
## aparecia como duas frases na conclusao, que sairam nos cortes do mesmo dia.
## Agora vira tabela, e o corpo do texto passa a apontar para ela.
##
## Fonte: outputs/teste_especificacoes.csv, com as quatro especificacoes
##   A  sigma2_L livre, sigma2_S livre   (irrestrita)
##   B  sigma2_L livre, sigma2_S = 0
##   C  sigma2_L = 0,   sigma2_S livre
##   D  sigma2_L = 0,   sigma2_S = 0     (a adotada no artigo)
##
## As colunas da tabela sao as leituras CONDICIONAIS, que sao as que interessam:
##   dL = ll(B) - ll(D)  custo de impor a tendencia suave, dada a sazonalidade
##                       deterministica
##   dS = ll(C) - ll(D)  custo de impor a sazonalidade deterministica, dada a
##                       tendencia suave
## Limiar de referencia: 1,92 (metade de 3,84, o qui-quadrado com 1 g.l. a 5%).
## E conservador aqui, porque a hipotese nula poe a variancia na fronteira do
## espaco parametrico (Self e Liang, 1987) -- o artigo ja cita isso na Secao 3.
##
## Saida: artigo/resultados/tabrestricoes_apendice.tex
################################################################################

RAIZ   <- Sys.getenv("REPO_RAIZ", unset = getwd())
ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))
RES    <- file.path(ARTIGO, "resultados")

LIMIAR <- 1.92

d <- read.csv(file.path(RAIZ, "outputs", "teste_especificacoes.csv"),
              stringsAsFactors = FALSE, encoding = "UTF-8")
stopifnot(nrow(d) == 96, setequal(unique(d$espec), c("A", "B", "C", "D")))

w <- reshape(d[, c("ind", "estrato", "espec", "ll", "bic")],
             idvar = c("ind", "estrato"), timevar = "espec", direction = "wide")
w$dL <- w$ll.B - w$ll.D
w$dS <- w$ll.C - w$ll.D
w$melhor_bic <- c("A", "B", "C", "D")[apply(w[, c("bic.A","bic.B","bic.C","bic.D")], 1, which.min)]

## guarda-corpo: se uma reestimacao futura mudar o quadro, o script para.
stopifnot(sum(w$dS > LIMIAR) == 1, sum(w$dL > LIMIAR) == 4)

IND <- list(c("desocupados", "A. Total de desocupados"),
            c("ocupados",    "B. Total de ocupados"),
            c("taxa",        "C. Taxa de desocupa\u00e7\u00e3o"))

## os rotulos do CSV vem como "01-Belo Horizonte"; o padrao das demais tabelas
## do artigo e "01 - Belo Horizonte", e a Mata aparece com outro nome.
rotulo <- function(x) {
  x <- sub("^(\\d{2})-", "\\1 - ", x)
  x <- sub("05 - Mata de Minas Gerais", "05 - Zona da Mata", x, fixed = TRUE)
  sub("Entorno metropolitano", "Entorno Metropolitano", x, fixed = TRUE)
}
## arredonda antes de formatar: a restricao nao pode AUMENTAR a verossimilhanca,
## entao os negativos minusculos sao ruido do otimizador e sairiam como "-0,00".
fmt <- function(v) {
  v <- round(v, 2)
  v[abs(v) < 0.005] <- 0
  formatC(v, format = "f", digits = 2, decimal.mark = ",")
}
marca <- function(v) if (v > LIMIAR) paste0("\\textbf{", fmt(v), "}") else fmt(v)

corpo <- unlist(lapply(seq_along(IND), function(k) {
  i <- IND[[k]]
  s <- w[w$ind == i[1], ]
  s <- s[order(s$estrato), ]
  stopifnot(nrow(s) == 8)
  lin <- vapply(seq_len(nrow(s)), function(j) sprintf(
    "%s & %s & %s & %s \\\\", rotulo(s$estrato[j]), marca(s$dL[j]), marca(s$dS[j]),
    if (s$melhor_bic[j] == "D") "D" else s$melhor_bic[j]), "")
  c(if (k > 1) "\\midrule",
    sprintf("\\multicolumn{4}{l}{\\textit{%s}} \\\\", i[2]), lin)
}))

tex <- c(
  "\\begin{table}[H]", "\\centering", "\\footnotesize",
  "\\captionsetup{justification=centering}",
  paste0("\\caption{Custo em log-verossimilhan\u00e7a das restri\u00e7\u00f5es da especifica\u00e7\u00e3o ",
         "adotada}"),
  "\\label{tab:apx_restricoes}",
  "\\adjustbox{max width=\\linewidth}{%", "\\renewcommand{\\arraystretch}{0.85}",
  "\\begin{tabular}{lccc}", "\\toprule",
  paste0("\\textbf{Estrato geogr\u00e1fico} & ",
         "\\textbf{\\makecell{Tend\u00eancia suave \\\\ \\(\\sigma_L^2=0\\)}} & ",
         "\\textbf{\\makecell{Sazonalidade determin\u00edstica \\\\ \\(\\sigma_S^2=0\\)}} & ",
         "\\textbf{\\makecell{Prefer\u00eancia \\\\ do BIC}} \\\\"),
  "\\midrule", corpo, "\\bottomrule", "\\end{tabular}}",
  paste0("\\fonte{Elabora\u00e7\u00e3o pr\u00f3pria, com base nos dados da PNAD Cont\u00ednua. ",
         "Nota: queda da log-verossimilhan\u00e7a ao impor cada restri\u00e7\u00e3o, mantida a outra. ",
         "Em negrito, os casos acima de 1,92, metade do qui-quadrado com um grau de ",
         "liberdade a 5\\%; o limiar \u00e9 conservador porque a hip\u00f3tese nula p\u00f5e a ",
         "vari\u00e2ncia na fronteira do espa\u00e7o param\u00e9trico. A \u00faltima coluna indica a ",
         "especifica\u00e7\u00e3o preferida pelo BIC entre as quatro combina\u00e7\u00f5es das duas ",
         "restri\u00e7\u00f5es: D \u00e9 a adotada no artigo e B \u00e9 a que mant\u00e9m o n\u00edvel estoc\u00e1stico. ",
         "Uma restri\u00e7\u00e3o n\u00e3o pode aumentar a verossimilhan\u00e7a: os dois valores ",
         "negativos indicam que o otimizador n\u00e3o alcan\u00e7ou o \u00f3timo da especifica\u00e7\u00e3o ",
         "irrestrita naquele estrato.}"),
  "\\end{table}")

con <- file(file.path(RES, "tabrestricoes_apendice.tex"), open = "w", encoding = "UTF-8")
writeLines(tex, con); close(con)
cat("gravado: tabrestricoes_apendice.tex\n")

cat("\nsigma2_S = 0 acima do limiar:", sum(w$dS > LIMIAR), "de 24 (max",
    round(max(w$dS), 2), ")\n")
cat("sigma2_L = 0 acima do limiar:", sum(w$dL > LIMIAR), "de 24 (max",
    round(max(w$dL), 2), ")\n")
cat("BIC prefere outra especificacao em:", sum(w$melhor_bic != "D"), "casos\n")
