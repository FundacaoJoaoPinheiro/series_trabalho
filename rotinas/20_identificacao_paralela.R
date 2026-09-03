################################################################################
## 20_identificacao_paralela.R  — PASSO 2, versão paralela
##
## Mesma identificação do 19_, distribuída entre núcleos. As 24 combinações
## estrato × indicador são independentes, então a paralelização é direta.
##
## Cada tarefa grava seu próprio CSV (um arquivo por combinação), o que evita
## disputa de escrita entre processos e dá progresso verificável — o stdout dos
## workers não chega ao log do processo pai.
##
## As funções vêm de rotinas/_id_funcoes.R, compartilhadas com a versão
## sequencial para as duas não divergirem.
################################################################################

suppressMessages({ library(dlm); library(parallel) })

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

SAIDA <- file.path(RAIZ, "outputs", "identificacao")
PARC  <- file.path(SAIDA, "parciais")
dir.create(PARC, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(PARC, full.names = TRUE))

BURN <- 8
NUC  <- as.integer(Sys.getenv("NUCLEOS", "9"))   # 10 fisicos, deixa 1 livre

facs <- readRDS(file.path(RAIZ, "outputs", "fac_pseudo_erros", "fac_padrao.rds"))
base <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
REG  <- names(base)[1:8]

## ATENCAO: os nomes dos estratos DIFEREM entre as duas bases --
## "02-Colar e Entorno metropolitano de BH" em baseestr8reg.rds contra
## "...Metropolitano..." em dadosalin_txdesoc_8reg.rds (m maiusculo). A busca da
## FAC por nome falhava para esse estrato e as 3 combinacoes eram descartadas em
## silencio. A chave passa a vir da base ALINHADA (que gerou as FACs), e a
## ausencia de qualquer chave aborta a execucao em vez de omitir resultado.
REG_FAC <- names(readRDS(file.path(RAIZ, "dadosalin_txdesoc_8reg.rds")))[1:8]
stopifnot(length(REG_FAC) == 8)

## séries por indicador/estrato, extraídas aqui para os workers não relerem a base
tarefas <- list()
for (ind in c("desocupados", "ocupados", "taxa")) {
  for (i in seq_along(REG)) {
    rho <- facs[[paste(ind, REG_FAC[i], sep = "|")]]
    if (is.null(rho))
      stop(sprintf("FAC ausente para %s / %s -- chave nao encontrada", ind, REG_FAC[i]))
    d <- base[[ REG[i] ]]
    s <- switch(ind,
      desocupados = list(y = d$Total.de.desocupados/1000, se = d$sd_d/1000),
      ocupados    = list(y = d$Total.de.ocupados/1000,    se = d$sd_o/1000),
      taxa        = list(y = d[["Taxa.de.desocupação"]]*100, se = d[["sd_txd"]]*100))
    tarefas[[length(tarefas)+1]] <- list(ind = ind, i = i, reg = REG[i],
                                         rho = rho[-1], y = s$y, se = s$se)
  }
}
cat("tarefas:", length(tarefas), "| nucleos:", NUC, "\n"); flush.console()

################################################################################
executa <- function(tf) {
  t0 <- Sys.time()
  i0 <- log(pmax(c(var(diff(tf$y)), var(diff(diff(tf$y)))/4, 1e-6, 1e-6), 1e-8))
  cds <- candidatos(tf$rho)
  linhas <- list()
  for (nm in names(cds)) {
    cd <- cds[[nm]]
    r  <- ajusta(tf$y, tf$se, cd$phi, cd$theta, i0, BURN)
    if (is.null(r)) next
    linhas[[length(linhas)+1]] <- data.frame(
      indicador = tf$ind, estrato = tf$reg, formulacao = nm, npar = r$npar,
      loglik = round(r$loglik,2), aicc = round(r$aicc,1), bic = round(r$bic,1),
      ljung = round(r$ljung,4), eqm1 = round(r$eqm1,3), rrse = round(r$rrse,2),
      stringsAsFactors = FALSE)
  }
  if (!length(linhas)) return(NULL)
  out <- do.call(rbind, linhas)
  arq <- file.path(PARC, sprintf("%s_%02d.csv", tf$ind, tf$i))
  write.csv(out, arq, row.names = FALSE)
  ## marcador de conclusão, legível de fora enquanto roda
  cat(sprintf("%s|%s|%d ajustes|%.1f min\n", tf$ind, tf$reg, nrow(out),
              as.numeric(difftime(Sys.time(), t0, units = "mins"))),
      file = file.path(PARC, "_progresso.txt"), append = TRUE)
  out
}

################################################################################
cl <- makeCluster(NUC)
on.exit(stopCluster(cl), add = TRUE)
clusterEvalQ(cl, suppressMessages(library(dlm)))
clusterExport(cl, c("RAIZ", "PARC", "BURN"), envir = environment())
clusterEvalQ(cl, source(file.path(RAIZ, "rotinas", "_id_funcoes.R")))

t0 <- Sys.time()
## balanceamento de carga com chunk.size = 1: por padrão o parLapplyLB divide em
## blocos de ceiling(n/nucleos) tarefas e balanceia por BLOCO, o que deixou
## workers ociosos com tarefas pendentes na execução anterior. Como os custos são
## desiguais (ocupados é mais caro que desocupados), o balanceamento precisa ser
## tarefa a tarefa.
res <- parLapplyLB(cl, tarefas, executa, chunk.size = 1)
stopCluster(cl)

cat("tempo total:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    "min\n")

tab <- do.call(rbind, Filter(Negate(is.null), res))
write.csv(tab, file.path(SAIDA, "identificacao.csv"), row.names = FALSE)
cat("ajustes:", nrow(tab), "| combinacoes:",
    length(unique(paste(tab$indicador, tab$estrato))), "\n")

################################################################################
cat("\n\n############ ESCOLHA POR CRITÉRIO ############\n")
for (ind in unique(tab$indicador)) {
  cat("\n===", ind, "===\n")
  s <- tab[tab$indicador == ind, ]
  for (e in unique(s$estrato)) {
    x <- s[s$estrato == e, ]
    ok <- x[!is.na(x$ljung) & x$ljung > 0.05, ]
    esc_lb <- if (nrow(ok)) ok$formulacao[which.min(ok$npar)] else "nenhum passa"
    cat(sprintf("%-40s LB: %-14s | AICc: %-14s | BIC: %-14s | EQM: %-14s\n",
                substr(e,1,38), esc_lb,
                x$formulacao[which.min(x$aicc)], x$formulacao[which.min(x$bic)],
                x$formulacao[which.min(x$eqm1)]))
  }
}

esc <- do.call(rbind, lapply(split(tab, list(tab$indicador, tab$estrato), drop = TRUE),
  function(x) {
    ok <- x[!is.na(x$ljung) & x$ljung > 0.05, ]
    data.frame(lb = if (nrow(ok)) ok$formulacao[which.min(ok$npar)] else NA,
               aicc = x$formulacao[which.min(x$aicc)],
               bic  = x$formulacao[which.min(x$bic)],
               eqm  = x$formulacao[which.min(x$eqm1)], stringsAsFactors = FALSE)
  }))
cat("\n\n############ CONTAGEM DE ESCOLHAS ############\n")
cat("\nLjung-Box (mais parcimonioso que passa):\n"); print(table(esc$lb, useNA = "ifany"))
cat("\nAICc:\n"); print(table(esc$aicc))
cat("\nBIC:\n");  print(table(esc$bic))
cat("\nEQM 1 passo:\n"); print(table(esc$eqm))

cat("\n\n############ O CANDIDATO DO DESENHO ############\n")
d <- tab[tab$formulacao == "MA(4) desenho", ]
if (nrow(d)) {
  cat("presente em", nrow(d), "das 24 combinacoes\n")
  cat("passa no Ljung-Box em", sum(d$ljung > 0.05, na.rm = TRUE), "delas\n")
  cat("RRSE medio:", round(mean(d$rrse, na.rm = TRUE), 2), "%\n")
  pos <- sapply(split(tab, list(tab$indicador, tab$estrato), drop = TRUE),
                function(x) which(x$formulacao[order(x$bic)] == "MA(4) desenho")[1])
  cat("posicao media no ranking de BIC:", round(mean(pos, na.rm = TRUE), 1),
      "de", round(mean(table(paste(tab$indicador, tab$estrato))), 1), "\n")
}

cat("\n\n############ RUÍDO BRANCO ############\n")
w <- tab[tab$formulacao == "Ruído branco", ]
if (nrow(w)) {
  cat("passa no Ljung-Box em", sum(w$ljung > 0.05, na.rm = TRUE), "de", nrow(w), "\n")
  cat("vence por BIC em", sum(esc$bic == "Ruído branco", na.rm = TRUE), "de", nrow(esc), "\n")
}
cat("\nGravado em", SAIDA, "\n")
