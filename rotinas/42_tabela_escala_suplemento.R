################################################################################
## 42_tabela_escala_suplemento.R
##
## Tabela do material suplementar (decisão do Caio em 20/11/2026): a validação
## da condição V(ẽ_t) = 1, que até agora só existia como uma frase na Seção 3
## ("a estimativa, obtida com múltiplos valores iniciais, tem mediana 1,03, e a
## hipótese de que ela é igual a um só é rejeitada, a 5%, em dois dos 24 pares").
## A condição é o que torna interpretável todo o ganho de precisão do artigo:
## sem ela, o modelo pode reduzir o erro-padrão da tendência apenas atribuindo
## ao erro amostral variância menor que a do desenho.
##
## FONTE DOS NÚMEROS: outputs/robustez_2026-11-10/escala/
## validacao_escala_multistart.csv, do 36_escala_trg_multistart.R -- NÃO o
## validacao_escala.csv do 27_, que usa uma partida só. A diferença não é
## cosmética: no 27_, Triângulo Mineiro x ocupados sai com lambda = 1 e RV = 0
## EXATOS, que é o otimizador parado no chute, e a mediana das 24 combinações
## vai a 1,01. Com as partidas múltiplas o caso se resolve (lambda = 1,054,
## p = 0,453) e a mediana sobe para 1,03, que é o número que o manuscrito
## passou a trazer desde 10/11/2026. Usar o CSV do 27_ aqui poria a tabela em
## contradição com o texto.
##
## Os rótulos dos estratos são lidos de uma tabela já gerada pelo
## 25_saidas_artigo.R, para não divergirem dos do artigo.
##
## Não reestima nada. Rodar depois do 25_ e do 36_.
##
## Saída: artigo/resultados/escala_suplemento.tex
################################################################################

RAIZ   <- Sys.getenv("REPO_RAIZ", unset = getwd())
ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))
RES    <- file.path(ARTIGO, "resultados")

IND <- list(
  list(csv = "desocupados", pasta = "Total de desocupados", suf = "desoc",
       rot = "A. Total de desocupados"),
  list(csv = "ocupados",    pasta = "Total de ocupados",    suf = "ocup",
       rot = "B. Total de ocupados"),
  list(csv = "taxa",        pasta = "Taxa de desocupa\u00e7\u00e3o", suf = "taxa",
       rot = "C. Taxa de desocupa\u00e7\u00e3o"))

mult <- read.csv(file.path(RAIZ, "outputs", "robustez_2026-11-10", "escala",
                           "validacao_escala_multistart.csv"),
                 stringsAsFactors = FALSE, encoding = "UTF-8")
proc <- read.csv(file.path(RAIZ, "outputs", "validacao_escala", "validacao_escala.csv"),
                 stringsAsFactors = FALSE, encoding = "UTF-8")
stopifnot(nrow(mult) == 24, nrow(proc) == 24)

## confere que a frase da Secao 3 continua valendo para estes numeros
stopifnot(round(median(mult$lambda), 2) == 1.03, sum(mult$p < 0.05) == 2)

num <- function(x, d) sub(".", ",", formatC(x, format = "f", digits = d), fixed = TRUE)

## rotulos dos estratos como o artigo os grafa
rotulos <- function(pasta, suf) {
  l <- readLines(file.path(RES, pasta, paste0("diag", suf, ".tex")),
                 encoding = "UTF-8", warn = FALSE)
  ## a tabela de diagnosticos traz DOIS blocos de oito estratos (univariado e
  ## multivariado, empilhados), entao ficam 16 linhas; os rotulos sao os mesmos
  ## nos dois e basta o primeiro bloco.
  l <- l[grepl("^0[1-8] - ", l)]
  stopifnot(length(l) == 16)
  l <- utils::head(l, 8)
  trimws(sapply(strsplit(l, " & ", fixed = TRUE), `[`, 1))
}

corpo <- unlist(lapply(seq_along(IND), function(k) {
  i <- IND[[k]]
  m <- mult[mult$indicador == i$csv, ]
  p <- proc[proc$indicador == i$csv, ]
  m <- m[order(m$estrato), ]; p <- p[order(p$estrato), ]
  stopifnot(nrow(m) == 8, identical(m$estrato, p$estrato))
  linhas <- sprintf("%s & %s & %s & %s & %s \\\\",
                    rotulos(i$pasta, i$suf), p$processo,
                    num(m$lambda, 3), num(m$RV, 2), num(m$p, 3))
  c(if (k > 1) "\\midrule",
    sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", i$rot),
    linhas)
}))

tex <- c(
  "\\begin{table}[H]", "\\centering", "\\footnotesize",
  "\\captionsetup{justification=centering}",
  paste0("\\caption{Vari\u00e2ncia estacion\u00e1ria do erro amostral padronizado ",
         "estimada livremente}"),
  "\\label{tab:sup_escala}",
  "\\adjustbox{max width=\\linewidth}{%", "\\renewcommand{\\arraystretch}{0.85}",
  "\\begin{tabular}{llccc}", "\\toprule",
  paste0("\\textbf{Estrato geogr\u00e1fico} & \\textbf{Processo} & ",
         "\\(\\hat{\\lambda}\\) & \\textbf{RV} & \\textbf{p} \\\\"),
  "\\midrule", corpo, "\\bottomrule", "\\end{tabular}}",
  paste0("\\fonte{Elabora\u00e7\u00e3o pr\u00f3pria, com base nos dados da PNAD Cont\u00ednua. ",
         "Nota: \\(\\hat{\\lambda}\\) \u00e9 a vari\u00e2ncia estacion\u00e1ria de ",
         "\\(\\tilde{e}_t\\) quando estimada livremente, em vez de imposta igual a um, ",
         "obtida com m\u00faltiplos valores iniciais. RV \u00e9 a estat\u00edstica de raz\u00e3o de ",
         "verossimilhan\u00e7a da hip\u00f3tese \\(\\lambda = 1\\), com distribui\u00e7\u00e3o ",
         "\\(\\chi^2_1\\); o valor cr\u00edtico a 5\\% \u00e9 3,84. Valores pr\u00f3ximos de um ",
         "indicam que o erro-padr\u00e3o do desenho descreve adequadamente a vari\u00e2ncia ",
         "amostral da s\u00e9rie.}"),
  "\\end{table}")

con <- file(file.path(RES, "escala_suplemento.tex"), open = "w", encoding = "UTF-8")
writeLines(tex, con); close(con)
cat("gravado: escala_suplemento.tex |", length(corpo), "linhas de corpo |",
    "mediana lambda =", round(median(mult$lambda), 3),
    "| rejeicoes a 5% =", sum(mult$p < 0.05), "\n")
