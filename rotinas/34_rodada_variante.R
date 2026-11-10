################################################################################
## 34_rodada_variante.R   — robustez da rodada C (10/11/2026)
##
## Roda o 22_ (univariado) ou o 23_ (multivariado) da rodada C SEM COPIAR o
## código: lê o texto do script, troca apenas os caminhos de entrada/saída e,
## opcionalmente, multiplica c_t por uma constante, e avalia o resultado. Assim
## a variante usa exatamente as mesmas funções, partidas e multi-start do
## Cholesky (commit 41ca2f8) que a rodada publicada. As saídas publicadas em
## outputs/univariado_final e outputs/multivariado_final NÃO são tocadas.
##
## Variáveis de ambiente:
##   ETAPA       uni | mv
##   SAIDA_UNI   diretório do univariado da variante (lido pelo mv)
##   SAIDA_MV    diretório do multivariado da variante (só ETAPA=mv)
##   ESCALA_CT   fator que multiplica c_t (padrão 1); só faz efeito em ETAPA=uni,
##               o mv herda o se escalado de modelos_<ind>.rds
##   TENDENCIA   repassada ao 22_ (por_estrato = escolha caso a caso pelo BIC)
##   INDICADOR   repassada ao 23_
##   MAX_REINICIO limite de reinícios do teste de perturbação no 23_ (padrão 5,
##               como na rodada C)
##
## Uso (Git Bash, na raiz do repositório):
##   ETAPA=uni SAIDA_UNI=outputs/robustez_2026-11-10/ct_0.8/univariado ESCALA_CT=0.8 \
##     Rscript --vanilla rotinas/34_rodada_variante.R
################################################################################

RAIZ  <- Sys.getenv("REPO_RAIZ", unset = getwd())
ETAPA <- Sys.getenv("ETAPA", "uni")
SUNI  <- normalizePath(Sys.getenv("SAIDA_UNI"), mustWork = FALSE)
ESC   <- as.numeric(Sys.getenv("ESCALA_CT", "1"))
stopifnot(nzchar(Sys.getenv("SAIDA_UNI")), is.finite(ESC), ESC > 0)

troca <- function(txt, de, para) {
  i <- which(txt == de)
  if (length(i) != 1) stop("linha não encontrada (ou repetida) no script-fonte:\n  ", de)
  txt[i] <- para
  txt
}
insere_antes <- function(txt, ancora, novas) {
  i <- which(txt == ancora)
  if (length(i) != 1) stop("âncora não encontrada no script-fonte:\n  ", ancora)
  append(txt, novas, after = i - 1)
}

if (ETAPA == "uni") {
  txt <- readLines(file.path(RAIZ, "rotinas", "22_univariado_final.R"), encoding = "UTF-8")
  txt <- troca(txt, 'SAIDA <- file.path(RAIZ, "outputs", "univariado_final")',
               sprintf('SAIDA <- "%s"', gsub("\\\\", "/", SUNI)))
  ## c_t escalado: o se entra no modelo (Z_t do erro amostral) E é a referência do
  ## DR. Os dois usos ficam coerentes com a hipótese "o desenho erra c_t por ESC".
  txt <- insere_antes(txt, "teste_H <- function(r) {",
    c(sprintf("ESCALA_CT <- %s", format(ESC, digits = 15)),
      "serie_c <- serie",
      "serie <- function(ind, i) { s <- serie_c(ind, i); s$se <- s$se * ESCALA_CT; s }",
      ""))
  cat("ETAPA uni | TENDENCIA =", Sys.getenv("TENDENCIA", "(rodada C: suave)"),
      "| ESCALA_CT =", ESC, "| saída:", SUNI, "\n")
} else if (ETAPA == "mv") {
  SMV <- normalizePath(Sys.getenv("SAIDA_MV"), mustWork = FALSE)
  stopifnot(nzchar(Sys.getenv("SAIDA_MV")))
  txt <- readLines(file.path(RAIZ, "rotinas", "23_multivariado_final.R"), encoding = "UTF-8")
  txt <- troca(txt, 'SAIDA <- file.path(RAIZ, "outputs", "multivariado_final")',
               sprintf('SAIDA <- "%s"', gsub("\\\\", "/", SMV)))
  txt <- troca(txt, 'uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final",',
               sprintf('uni <- readRDS(file.path("%s",', gsub("\\\\", "/", SUNI)))
  ## VERSÃO REDUZIDA (QUENTE_DE=<rds do multivariado da rodada C>): uma única
  ## partida, a partir do ótimo publicado -- irregulares e Cholesky da rodada C,
  ## nível dos estratos com nível livre no valor do univariado. Serve para a taxa,
  ## cujo multivariado com as três partidas frias levou horas na rodada C.
  qd <- Sys.getenv("QUENTE_DE", "")
  if (nzchar(qd)) txt <- insere_antes(txt, 'cat("otimizando...\\n"); flush.console()', c(
    sprintf('antC <- readRDS("%s")', gsub("\\\\", "/", normalizePath(qd))),
    "stopifnot(length(antC$fit$par) == P + N_CH)   # rodada C: sem nível livre",
    "p0[IDX$irreg] <- antC$fit$par[seq_len(P)]",
    "pcC <- antC$fit$par[P + seq_len(N_CH)]; Lc <- matrix(0, P, P); k <- 1",
    "for (j in 1:P) for (i in j:P) { Lc[i, j] <- if (i == j) exp(pcC[k]) else pcC[k]; k <- k + 1 }",
    "PARTIDAS <- list(quente_rodadaC = Lc)",
    'cat("VERSÃO REDUZIDA: partida única a partir do ótimo da rodada C (logLik C =", round(-antC$fit$value, 3), ")\\n")'))
  mr <- Sys.getenv("MAX_REINICIO", "")
  if (nzchar(mr)) txt <- troca(txt, "MAX_REINICIO <- 5",
                               sprintf("MAX_REINICIO <- %d", as.integer(mr)))
  cat("ETAPA mv | INDICADOR =", Sys.getenv("INDICADOR", "desocupados"),
      "| entrada:", SUNI, "| saída:", SMV, "\n")
} else stop("ETAPA deve ser uni ou mv")

t0 <- Sys.time()
eval(parse(text = txt, encoding = "UTF-8"), envir = globalenv())
cat("\n[34_] tempo total:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
