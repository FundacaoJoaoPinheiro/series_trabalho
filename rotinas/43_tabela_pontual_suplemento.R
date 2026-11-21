################################################################################
## 43_tabela_pontual_suplemento.R
##
## Tabela do material suplementar (decisão do Caio em 20/11/2026): as três
## tabelas de estimativas pontuais do último trimestre, que saíram do corpo nos
## cortes de 10/11, voltam como UMA tabela com um painel por indicador
## (A desocupados, B ocupados, C taxa), no mesmo padrão do Apêndice A.
##
## Juntá-las não é só economia de espaço: as três tinham a mesma estrutura de
## sete colunas e a mesma nota de 233 caracteres, repetida palavra por palavra.
## Aqui a nota aparece uma vez e a unidade de cada indicador vai no rótulo do
## painel, que era a única coisa que a nota dizia de diferente entre elas.
##
## Não recalcula nada: lê as linhas que o 25_saidas_artigo.R grava em
## artigo/resultados/<indicador>/est_pontual_<suf>.tex e as recompõe, como o
## 41_tabelas_apendice.R faz com os hiperparâmetros e os diagnósticos.
## Rodar depois do 25_.
##
## Saída: artigo/resultados/pontual_suplemento.tex
################################################################################

RAIZ   <- Sys.getenv("REPO_RAIZ", unset = getwd())
ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))
RES    <- file.path(ARTIGO, "resultados")

IND <- list(
  list(pasta = "Total de desocupados", suf = "desoc",
       rot = "A. Total de desocupados (mil pessoas)"),
  list(pasta = "Total de ocupados", suf = "ocup",
       rot = "B. Total de ocupados (mil pessoas)"),
  list(pasta = "Taxa de desocupa\u00e7\u00e3o", suf = "taxa",
       rot = "C. Taxa de desocupa\u00e7\u00e3o (pontos percentuais)"))

## O rotulo do Colar vem embrulhado num \raisebox{\parbox{...}} de duas linhas,
## entao nao da para pescar as linhas por "^0[1-8] - " como faz o 41_. Pega-se
## o bloco entre o primeiro \midrule e o \bottomrule.
corpo_tabela <- function(arq) {
  l <- readLines(arq, encoding = "UTF-8", warn = FALSE)
  i <- grep("^\\s*\\\\midrule\\s*$", l)[1]
  j <- grep("^\\s*\\\\bottomrule\\s*$", l)[1]
  stopifnot(!is.na(i), !is.na(j), j > i)
  l <- trimws(l[(i + 1):(j - 1)])
  l <- l[nzchar(l)]
  stopifnot(length(l) == 8)
  l
}

corpo <- unlist(lapply(seq_along(IND), function(k) {
  i <- IND[[k]]
  linhas <- corpo_tabela(file.path(RES, i$pasta, paste0("est_pontual_", i$suf, ".tex")))
  c(if (k > 1) "\\midrule",
    sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", i$rot),
    linhas)
}))

tex <- c(
  "\\begin{table}[H]", "\\centering", "\\footnotesize",
  "\\captionsetup{justification=centering}",
  paste0("\\caption{Resultados pontuais do modelo multivariado - ",
         "4\\textordmasculine{} trimestre de 2024}"),
  "\\label{tab:sup_pontual}",
  "\\adjustbox{max width=\\linewidth}{%", "\\renewcommand{\\arraystretch}{0.8}",
  "\\begin{tabular}{@{}p{2.8cm}cccccc@{}}", "\\toprule",
  paste0("\\textbf{Estrato Geogr\u00e1fico} & $\\hat{y}$ & $CV(\\hat{y})$ & IC 95\\% & ",
         "$\\hat{\\theta}$ & $CV(\\hat{\\theta})$ & IC 95\\% \\\\"),
  "\\midrule", corpo, "\\bottomrule", "\\end{tabular}%", "}",
  paste0("\\fonte{Elabora\u00e7\u00e3o pr\u00f3pria, com base nos dados da PNAD Cont\u00ednua. ",
         "Nota: \\(\\hat{y}\\) \u00e9 a estimativa direta e \\(\\hat{\\theta}\\) o sinal ",
         "estimado pelo modelo multivariado (tend\u00eancia mais sazonalidade). A unidade ",
         "de cada indicador est\u00e1 no r\u00f3tulo do painel.}"),
  "\\end{table}")

con <- file(file.path(RES, "pontual_suplemento.tex"), open = "w", encoding = "UTF-8")
writeLines(tex, con); close(con)
cat("gravado: pontual_suplemento.tex |", length(corpo), "linhas de corpo\n")
