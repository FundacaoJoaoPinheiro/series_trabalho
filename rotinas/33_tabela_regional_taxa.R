################################################################################
## 33_tabela_regional_taxa.R
##
## Tabela da leitura regional da taxa de desocupação (Seção 4.3): para cada
## estrato, a tendência estimada pelo modelo multivariado da própria taxa em
## quatro momentos (antes da crise de 2015-2016, pico da crise, pico da Covid e
## último trimestre), e quais estratos diferem entre si no último trimestre.
##
## Responde ao que a Seção 2 promete ("comparar os estratos com segurança") e
## substitui as tabelas pontuais, que comparam modelo e direta no MESMO estrato.
##
## Não reestima nada: lê os .rds da rodada C, como o 25_saidas_artigo.R.
## A tendência é o bloco de nível (B_NIVEL) das estimativas FILTRADAS, a mesma
## série das figuras. A comparação entre estratos usa a covariância filtrada
## entre os níveis, que o modelo multivariado fornece; na estimativa direta os
## estratos são amostras independentes e a covariância é tomada como zero.
##
## Saídas: artigo/resultados/Taxa de desocupação/tabregional_taxa.tex
##         outputs/taxa_final/tabela_regional_taxa.csv
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))

P <- 8; B_NIVEL <- 1:8
INICIO <- c(2012, 1)
ROT <- c("01 - Belo Horizonte", "02 - Colar e Entorno Metropolitano de BH",
         "03 - Sul de Minas", "04 - Triângulo Mineiro", "05 - Zona da Mata",
         "06 - Norte de Minas", "07 - Vale do Rio Doce", "08 - Central")

fmt <- function(x, d = 1) gsub("\\.", ",", formatC(x, format = "f", digits = d))
## índice da observação t (1 = 2012T1) <-> rótulo "2016T4"
idx <- function(ano, tri) 4 * (ano - INICIO[1]) + tri
rot_t <- function(t) sprintf("%dT%d", INICIO[1] + (t - 1) %/% 4, (t - 1) %% 4 + 1)

## ---------------------------------------------------------------------------
## modelo multivariado da taxa, filtrado
## ---------------------------------------------------------------------------
uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final", "modelos_taxa.rds"))
cod <- names(uni)
Y   <- sapply(cod, function(k) uni[[k]]$serie$y)
SE  <- sapply(cod, function(k) uni[[k]]$serie$se)
m   <- readRDS(file.path(RAIZ, "outputs", "multivariado_final", "multivariado_taxa.rds"))
flt <- dlmFilter(Y, m$mod)
mse <- dlmSvd2var(flt$U.C, flt$D.C)[-1]      # alinha com dropFirst
est <- dropFirst(flt$m)
TR  <- est[, B_NIVEL]
n   <- nrow(TR)
stopifnot(n == 52, ncol(TR) == P)
SE_TR <- sapply(1:P, function(i) sapply(mse, function(x) sqrt(x[i, i])))

## ---------------------------------------------------------------------------
## momentos da série
## ---------------------------------------------------------------------------
t_pre  <- idx(2014, 4)
jan_cr <- idx(2015, 1):idx(2018, 4)          # crise de 2015-2016 e sua cauda
jan_cv <- idx(2020, 1):idx(2021, 4)          # Covid-19
t_fim  <- n

pico <- function(jan) t(sapply(1:P, function(i) {
  j <- jan[which.max(TR[jan, i])]; c(t = j, v = TR[j, i])
}))
pc <- pico(jan_cr); pv <- pico(jan_cv)

## ---------------------------------------------------------------------------
## comparações entre pares no último trimestre (bicaudal, 5%, sem ajuste)
## ---------------------------------------------------------------------------
V_fim <- mse[[t_fim]][B_NIVEL, B_NIVEL]
difere_mod <- difere_dir <- matrix(FALSE, P, P)
for (i in 1:P) for (j in 1:P) if (i != j) {
  dm <- TR[t_fim, i] - TR[t_fim, j]
  vm <- V_fim[i, i] + V_fim[j, j] - 2 * V_fim[i, j]
  difere_mod[i, j] <- abs(dm) / sqrt(vm) > qnorm(0.975)
  dd <- Y[t_fim, i] - Y[t_fim, j]
  vd <- SE[t_fim, i]^2 + SE[t_fim, j]^2
  difere_dir[i, j] <- abs(dd) / sqrt(vd) > qnorm(0.975)
}
n_mod <- sum(difere_mod[upper.tri(difere_mod)])
n_dir <- sum(difere_dir[upper.tri(difere_dir)])

lista <- function(i) {
  k <- which(difere_mod[i, ])
  if (!length(k)) "--" else paste(sprintf("%02d", k), collapse = ", ")
}

## ---------------------------------------------------------------------------
## CSV completo
## ---------------------------------------------------------------------------
tab <- data.frame(
  estrato   = ROT,
  pre_2014T4 = TR[t_pre, ],
  pico_crise = pc[, "v"], tri_pico_crise = rot_t(pc[, "t"]),
  pico_covid = pv[, "v"], tri_pico_covid = rot_t(pv[, "t"]),
  fim_2024T4 = TR[t_fim, ], ic_inf = TR[t_fim, ] - 1.96 * SE_TR[t_fim, ],
  ic_sup = TR[t_fim, ] + 1.96 * SE_TR[t_fim, ],
  var_crise_pp = pc[, "v"] - TR[t_pre, ],
  var_covid_fim_pp = TR[t_fim, ] - pv[, "v"],
  difere_de = sapply(1:P, lista),
  direta_2024T4 = Y[t_fim, ], se_direta_2024T4 = SE[t_fim, ],
  row.names = NULL)
write.csv(tab, file.path(RAIZ, "outputs", "taxa_final", "tabela_regional_taxa.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

## ---------------------------------------------------------------------------
## .tex
## ---------------------------------------------------------------------------
linhas <- sapply(1:P, function(i) sprintf(
  "%s & %s & %s (%s) & %s (%s) & %s [%s ; %s] & %s & %s \\\\",
  ROT[i], fmt(TR[t_pre, i]),
  fmt(pc[i, "v"]), rot_t(pc[i, "t"]), fmt(pv[i, "v"]), rot_t(pv[i, "t"]),
  fmt(TR[t_fim, i]), fmt(tab$ic_inf[i]), fmt(tab$ic_sup[i]),
  sub("^-", "\\\\(-\\\\)", fmt(tab$var_covid_fim_pp[i])), lista(i)))

tex <- c(
  "\\begin{table}[H]", "\\centering", "\\footnotesize",
  "\\captionsetup{justification=centering}",
  paste0("\\caption{Tendência da taxa de desocupação estimada pelo modelo multivariado ",
         "em momentos selecionados -- oito estratos geográficos}"),
  "\\label{tab:regional_taxa}", "",
  "\\renewcommand{\\arraystretch}{0.8}",
  "\\adjustbox{max width=\\linewidth}{%",
  "\\begin{tabular}{@{}lcccccc@{}}", "\\toprule",
  paste0("\\textbf{Estrato Geográfico} & \\textbf{2014T4} & ",
         "\\textbf{\\makecell{Pico\\\\2015--2018}} & \\textbf{\\makecell{Pico\\\\2020--2021}} & ",
         "\\textbf{\\makecell{2024T4\\\\{[}IC 95\\%{]}}} & ",
         "\\textbf{\\makecell{Variação desde o\\\\pico da Covid (p.p.)}} & ",
         "\\textbf{\\makecell{Difere em 2024T4\\\\de (5\\%)}} \\\\"),
  "\\midrule", linhas, "\\bottomrule", "\\end{tabular}%", "}",
  paste0("\\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
         "Nota: tendência filtrada (\\%) do modelo multivariado da própria taxa; ",
         "entre parênteses, o trimestre do pico. A última coluna lista os estratos ",
         "dos quais a tendência difere a 5\\% em 2024T4, considerando a covariância ",
         "entre estratos, sem ajuste para comparações múltiplas.}"),
  "\\end{table}")

d <- file.path(ARTIGO, "resultados", "Taxa de desocupação")
con <- file(file.path(d, "tabregional_taxa.tex"), open = "w", encoding = "UTF-8")
writeLines(tex, con); close(con)

cat("Pares que diferem em 2024T4: modelo", n_mod, "| direta", n_dir, "de 28\n")
print(format(tab[, c("estrato", "pre_2014T4", "pico_crise", "tri_pico_crise",
                     "pico_covid", "tri_pico_covid", "fim_2024T4", "ic_inf", "ic_sup",
                     "var_covid_fim_pp", "difere_de")], digits = 3), row.names = FALSE)
