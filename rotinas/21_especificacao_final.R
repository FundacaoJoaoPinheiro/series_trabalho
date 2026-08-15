################################################################################
## 21_especificacao_final.R   — fecha o PASSO 2 do pipeline
##
## Produz a ESPECIFICACAO DEFINITIVA do processo do erro amostral, individual
## por estrato e indicador, com os coeficientes prontos para os passos 3 a 6.
##
## DECISOES DOS AUTORES QUE ESTE SCRIPT IMPLEMENTA
##  1. Var(e~) = 1 imposta (issue #17). O modelo respeita a variancia do
##     desenho amostral; a variancia da inovacao e derivada, nao estimada.
##  2. Ruido branco EXCLUIDO do conjunto de candidatos. Ha autocorrelacao
##     detectavel nos pseudo-erros e a decisao foi modela-la.
##  3. sigma2_I (irregular) permanece estimado internamente; o irregular e
##     obtido por diferenca. ATENCAO: o perfil de verossimilhanca e quase plano
##     em sigma2_I (variar de 0,1% a 10% da variancia do desenho custa < 0,3 de
##     log-verossimilhanca, contra o limiar de 1,92), portanto sigma2_I e
##     FRACAMENTE IDENTIFICADO e sua estimativa depende da convencao de
##     otimizacao. A convencao esta fixada em _id_funcoes.R e deve ser mantida.
##  4. Especificacao INDIVIDUAL por estrato e indicador -- nao uniforme.
##
## CRITERIO
##  Ljung-Box nos residuos padronizados (defasagem 8): entre os candidatos que
##  nao rejeitam brancura a 5%, escolhe-se o mais parcimonioso; empate pelo BIC.
##  Se nenhum passar, registra-se o de maior p-valor com a marca "nao passa".
##
##  O Ljung-Box e o diagnostico da literatura (Harvey); a identificacao
##  Box-Jenkins pela FAC/FACP (Pfeffermann, Feder e Signorelli 1998; Silva e
##  Cruz 2002) e reportada em paralelo, para o artigo poder mostrar as duas.
##
## SAIDA
##  outputs/identificacao/especificacao_final.csv -- uma linha por estrato x
##  indicador, com processo, coeficientes phi/theta, metricas e diagnostico.
################################################################################

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
source(file.path(RAIZ, "rotinas", "_id_funcoes.R"))

SAIDA <- file.path(RAIZ, "outputs", "identificacao")
tab <- read.csv(file.path(SAIDA, "identificacao.csv"),
                stringsAsFactors = FALSE, fileEncoding = "UTF-8")
facs <- readRDS(file.path(RAIZ, "outputs", "fac_pseudo_erros", "fac_padrao.rds"))
REGF <- names(readRDS(file.path(RAIZ, "dadosalin_txdesoc_8reg.rds")))[1:8]

## ---- escolha por Ljung-Box, sem ruido branco -------------------------------
t2 <- tab[tab$formulacao != "Ruído branco", ]
esc <- do.call(rbind, lapply(split(t2, list(t2$indicador, t2$estrato), drop = TRUE),
  function(x) {
    ok <- x[!is.na(x$ljung) & x$ljung > 0.05, ]
    if (nrow(ok)) { y <- ok[order(ok$npar, ok$bic), ][1, ]; y$diagnostico <- "passa" }
    else          { y <- x[which.max(x$ljung), ];           y$diagnostico <- "nao passa" }
    y
  }))

## ---- recupera os coeficientes de cada processo escolhido -------------------
## Reconstroi os candidatos a partir da FAC (mesma rotina do 19_/20_) e extrai
## phi e theta do processo vencedor, para os passos seguintes consumirem.
set.seed(20260815)   # casa_momentos usa reinicios aleatorios; fixa a reproducao
linhas <- list()
for (i in seq_len(nrow(esc))) {
  e <- esc[i, ]
  idx <- match(e$estrato, sub("metropolitano", "Metropolitano", REGF))
  if (is.na(idx)) idx <- which(sapply(REGF, function(z)
    identical(tolower(z), tolower(e$estrato))))[1]
  stopifnot(!is.na(idx))
  rho <- facs[[paste(e$indicador, REGF[idx], sep = "|")]][-1]
  cds <- candidatos(rho)
  cd  <- cds[[e$formulacao]]
  stopifnot(!is.null(cd))
  linhas[[i]] <- data.frame(
    indicador = e$indicador, estrato = e$estrato, ordem = idx,
    processo  = e$formulacao,
    phi   = paste(round(cd$phi, 6),   collapse = ";"),
    theta = paste(round(cd$theta, 6), collapse = ";"),
    p = length(cd$phi), q = length(cd$theta),
    npar = e$npar, loglik = e$loglik, aicc = e$aicc, bic = e$bic,
    ljung = e$ljung, eqm1 = e$eqm1, rrse = e$rrse,
    diagnostico = e$diagnostico, stringsAsFactors = FALSE)
}
fin <- do.call(rbind, linhas)
fin <- fin[order(fin$indicador, fin$ordem), ]

write.csv(fin, file.path(SAIDA, "especificacao_final.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

## ---- relatorio --------------------------------------------------------------
cat("############ ESPECIFICACAO FINAL -- individual por estrato ############\n\n")
cat(sprintf("%-12s %-40s %-14s %-24s %7s %8s %s\n",
            "indicador","estrato","processo","coeficientes","LB","ganho%","diag"))
for (i in seq_len(nrow(fin))) with(fin[i,],
  cat(sprintf("%-12s %-40s %-14s %-24s %7.3f %8.2f  %s\n",
              indicador, substr(estrato,1,38), processo,
              paste(c(if(nchar(phi)) paste0("phi=",phi),
                      if(nchar(theta)) paste0("th=",theta)), collapse=" "),
              ljung, rrse, diagnostico)))

cat("\n### contagem de processos ###\n"); print(table(fin$processo))
cat("\n### por indicador ###\n");        print(table(fin$indicador, fin$processo))
cat("\n### ganho medio por indicador ###\n")
print(round(tapply(fin$rrse, fin$indicador, mean), 2))
cat("\nganho medio geral:", round(mean(fin$rrse), 2), "%\n")
cat("estratos em que o Ljung-Box NAO passa:", sum(fin$diagnostico == "nao passa"),
    "de", nrow(fin), "\n")
if (any(fin$diagnostico == "nao passa"))
  print(fin[fin$diagnostico == "nao passa", c("indicador","estrato","processo","ljung")],
        row.names = FALSE)

cat("\nGravado: outputs/identificacao/especificacao_final.csv\n")
