################################################################################
## 19_identificacao.R   — PASSO 2 do pipeline de revisão
##
## Identifica o processo do erro amostral, por estrato e indicador, com os
## MESMOS candidatos para todos (corrige a incomparabilidade apontada na V1,
## em que o Vale do Rio Doce competia com um único candidato).
##
## CANDIDATOS (18)
##   ruído branco
##   AR(1) a AR(6)      — Yule-Walker sobre a FAC dos pseudo-erros
##   MA(1) a MA(6)      — inversão da FAC, sempre na forma invertível
##   ARMA(1,1) (2,1) (1,2) (2,2) — casamento de momentos com a FAC
##   MA(4) soma móvel   — imposto pelo desenho, SEM parâmetro livre:
##                        e_t = xi_t + ... + xi_{t-4}, cuja FAC é (5-j)/5,
##                        que é exatamente a sobreposição do esquema 1-2(5)
##
## Todos sob Var(e~) = 1 (o modelo respeita a variância do desenho).
##
## MÉTRICAS
##   Ljung-Box  — PRINCIPAL. A pergunta é se a autocorrelação do erro amostral
##                foi contemplada; se foi, os resíduos de um passo são brancos.
##   AICc       — AIC corrigido para amostra pequena (T = 52)
##   BIC        — com k = 4 + p + q: os coeficientes ARMA vêm dos dados (das
##                séries por grupo de rotação), então contam
##   EQM 1 passo— erro quadrático médio de previsão um passo à frente
##
## As montagens usadas aqui são as validadas em rotinas/18_valida_processos.R
## (56 verificações, 0 falhas).
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
SAIDA <- file.path(RAIZ, "outputs", "identificacao")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

BURN <- 8
facs <- readRDS(file.path(RAIZ, "outputs", "fac_pseudo_erros", "fac_padrao.rds"))
base <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
REG  <- names(base)[1:8]

serie <- function(ind, i) {
  d <- base[[ REG[i] ]]
  switch(ind,
    desocupados = list(y = d$Total.de.desocupados/1000, se = d$sd_d/1000),
    ocupados    = list(y = d$Total.de.ocupados/1000,    se = d$sd_o/1000),
    taxa        = list(y = d[["Taxa.de.desocupação"]]*100, se = d[["sd_txd"]]*100))
}

################################################################################
## Montagem validada
################################################################################
blocos <- function(phi, theta) {
  p <- length(phi); q <- length(theta); r <- max(p, q + 1, 1)
  v  <- c(1, theta, rep(0, max(0, r - 1 - q)))[1:r]
  ph <- c(phi,  rep(0, max(0, r - p)))[1:r]
  G <- matrix(0, r, r); G[, 1] <- ph
  if (r > 1) G[1:(r-1), 2:r] <- diag(r - 1)
  list(G = G, v = v, r = r)
}
P_est <- function(G, v, s2) {
  r <- nrow(G)
  matrix(solve(diag(r*r) - kronecker(G, G), as.vector(s2 * (v %*% t(v)))), r, r)
}

monta <- function(se, phi, theta) {
  b <- blocos(phi, theta)
  p11 <- try(P_est(b$G, b$v, 1)[1,1], silent = TRUE)
  if (inherits(p11, "try-error") || !is.finite(p11) || p11 <= 0) return(NULL)
  s2 <- 1 / p11                      # Var(e~) = 1
  ns <- 5 + b$r
  function(pp) {
    m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se, addInt = FALSE)
    if (b$r > 1) {
      m$FF <- cbind(m$FF, matrix(0, 1, b$r - 1))
      m$GG <- rbind(m$GG, matrix(0, b$r - 1, 6))
      m$GG <- cbind(m$GG, matrix(0, ns, b$r - 1))
    }
    m$GG[6:ns, 6:ns] <- b$G
    W <- matrix(0, ns, ns)
    W[6:ns, 6:ns] <- s2 * (b$v %*% t(b$v))
    W[1,1] <- exp(pp[1]); W[2,2] <- exp(pp[2]); W[3,3] <- exp(pp[3])
    d <- diag(W); diag(W)[d == 0] <- 1e-10
    m$W  <- W
    m$V  <- exp(pp[4])
    m$m0 <- rep(0, ns)
    m$C0 <- diag(x = 1e6, ns)
    m
  }
}

################################################################################
## Derivação dos candidatos a partir da FAC
################################################################################
yule_walker <- function(rho, p) {
  R <- if (p == 1) matrix(1,1,1) else toeplitz(c(1, rho[1:(p-1)]))
  ph <- try(as.numeric(solve(R, rho[1:p])), silent = TRUE)
  if (inherits(ph, "try-error") || any(!is.finite(ph))) return(NULL)
  ph
}
estacionario <- function(phi, tol = 1.0001)
  !length(phi) || all(Mod(polyroot(c(1, -phi))) > tol)

torna_invertivel <- function(th) {
  r <- polyroot(c(1, th)); dentro <- Mod(r) < 1
  if (!any(dentro)) return(th)
  r[dentro] <- 1/r[dentro]
  cf <- 1; for (ri in r) cf <- c(cf, 0) - c(0, cf)/ri
  Re(cf[-1])
}

## casamento de momentos generico: acha (phi, theta) cuja FAC teorica mais se
## aproxima da observada nos primeiros nlags
## `tent` reduzido de 25 para 8: o casamento de momentos converge quase sempre
## na primeira tentativa, e os reinicios dominavam o custo (a derivacao dos
## candidatos roda 10x por estrato e nao aparecia no cronometro do ajuste).
casa_momentos <- function(rho, p, q, nlags = NULL, tent = 8) {
  if (is.null(nlags)) nlags <- max(p + q + 2, 4)
  alvo <- rho[1:nlags]
  perda <- function(z) {
    ph <- if (p) z[1:p] else numeric(0)
    th <- if (q) z[(p+1):(p+q)] else numeric(0)
    if (length(ph) && !estacionario(ph, 1.001)) return(1e6)
    r <- tryCatch(ARMAacf(ar = ph, ma = th, lag.max = nlags)[-1],
                  error = function(e) rep(NA, nlags))
    if (any(!is.finite(r))) return(1e6)
    sum((as.numeric(r) - alvo)^2)
  }
  melhor <- NULL
  for (t in 1:tent) {
    ini <- if (t == 1) c(rep(0.3, p), rep(0.2, q)) else runif(p + q, -0.6, 0.8)
    r <- try(optim(ini, perda, method = "BFGS",
                   control = list(maxit = 800, reltol = 1e-12)), silent = TRUE)
    if (inherits(r, "try-error") || !is.finite(r$value)) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  if (is.null(melhor) || melhor$value > 0.05) return(NULL)
  ph <- if (p) melhor$par[1:p] else numeric(0)
  th <- if (q) torna_invertivel(melhor$par[(p+1):(p+q)]) else numeric(0)
  if (length(ph) && !estacionario(ph)) return(NULL)
  list(phi = ph, theta = th)
}

candidatos <- function(rho) {
  out <- list("Ruído branco" = list(phi = numeric(0), theta = numeric(0)))
  for (p in 1:6) {
    ph <- yule_walker(rho, p)
    if (!is.null(ph) && estacionario(ph))
      out[[sprintf("AR(%d)", p)]] <- list(phi = ph, theta = numeric(0))
  }
  for (q in 1:6) {
    cm <- casa_momentos(rho, 0, q, nlags = q)
    if (!is.null(cm)) out[[sprintf("MA(%d)", q)]] <- cm
  }
  for (pq in list(c(1,1), c(2,1), c(1,2), c(2,2))) {
    cm <- casa_momentos(rho, pq[1], pq[2])
    if (!is.null(cm)) out[[sprintf("ARMA(%d,%d)", pq[1], pq[2])]] <- cm
  }
  ## candidato do desenho: soma movel de 5 choques, sem parametro livre
  out[["MA(4) desenho"]] <- list(phi = numeric(0), theta = rep(1, 4), desenho = TRUE)
  out
}

################################################################################
## Ajuste e métricas
################################################################################
ajusta <- function(y, se, phi, theta, i0) {
  fn <- monta(se, phi, theta)
  if (is.null(fn)) return(NULL)
  obj <- function(pp) {
    ll <- try(dlmLL(y, fn(pp)), silent = TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
    ll
  }
  ## Sem limites na caixa de busca: testado no caso que parecia travar
  ## (BH-ocupados, AR(1)), o irrestrito converge em 0,7 s com conv = 0, enquanto
  ## a versao limitada em [-15,15] devolve conv = 52 e verossimilhanca pior.
  ## O que parecia travamento era buffer de saida do Rscript, nao falta de
  ## progresso -- por isso o cronometro por candidato mais abaixo.
  melhor <- NULL
  for (p0 in list(i0, i0 + 1, i0 - 1, rep(0, 4))) {
    r <- try(optim(p0, obj, method = "L-BFGS-B", control = list(maxit = 300)),
             silent = TRUE)
    if (inherits(r, "try-error") || !is.finite(r$value) || r$value >= 1e9) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  if (is.null(melhor)) return(NULL)
  mod <- fn(melhor$par)
  flt <- try(dlmFilter(y, mod), silent = TRUE)
  if (inherits(flt, "try-error")) return(NULL)

  ## residuos padronizados de um passo (decomposicao do erro de predicao)
  rp <- try(residuals(flt, type = "standardized", sd = FALSE), silent = TRUE)
  rb <- try(residuals(flt, type = "raw", sd = FALSE), silent = TRUE)
  if (inherits(rp, "try-error")) return(NULL)
  rp <- as.numeric(rp)[-(1:BURN)]
  rb <- if (inherits(rb, "try-error")) rep(NA, length(rp)) else as.numeric(rb)[-(1:BURN)]

  npar <- 4 + length(phi) + length(theta)
  n    <- length(y)
  ll   <- -melhor$value
  lb   <- try(Box.test(rp, lag = 8, type = "Ljung-Box")$p.value, silent = TRUE)
  if (inherits(lb, "try-error")) lb <- NA_real_

  mse <- dlmSvd2var(flt$U.C, flt$D.C)
  se_tr <- dropFirst(sapply(mse, function(x) sqrt(x[1,1])))
  est   <- dropFirst(flt$m)
  ix <- (BURN+1):n

  list(loglik = ll, npar = npar,
       aicc = 2*npar - 2*ll + (2*npar*(npar+1))/max(1, n - npar - 1),
       bic  = npar*log(n) - 2*ll,
       ljung = lb,
       eqm1 = mean(rb^2, na.rm = TRUE),
       rrse = mean((se[ix] - se_tr[ix]) / se[ix]) * 100)
}

################################################################################
## Execução
################################################################################
## PERSISTENCIA INCREMENTAL.
## O stdout do Rscript redirecionado para arquivo e bufferizado, e
## flush.console() nao tem efeito fora de sessao interativa -- por isso o log
## parecia congelado enquanto o processo trabalhava. A partir daqui cada estrato
## grava suas linhas no CSV assim que termina (conexao aberta e fechada a cada
## escrita, o que forca a descarga), e um arquivo de progresso registra o avanco.
ARQ_CSV  <- file.path(SAIDA, "identificacao.csv")
ARQ_PROG <- file.path(SAIDA, "progresso.txt")
if (file.exists(ARQ_CSV))  file.remove(ARQ_CSV)
if (file.exists(ARQ_PROG)) file.remove(ARQ_PROG)

anota <- function(txt) {
  con <- file(ARQ_PROG, open = "a"); writeLines(txt, con); close(con)
}
grava <- function(df) {
  novo <- !file.exists(ARQ_CSV)
  con <- file(ARQ_CSV, open = "a")
  write.table(df, con, sep = ",", row.names = FALSE, col.names = novo, qmethod = "double")
  close(con)
}

linhas <- list()
for (ind in c("desocupados", "ocupados", "taxa")) {
  cat("\n\n####################", toupper(ind), "####################\n")
  for (i in seq_along(REG)) {
    rho <- facs[[paste(ind, REG[i], sep = "|")]]
    if (is.null(rho)) next
    rho <- rho[-1]
    s <- serie(ind, i)
    i0 <- log(pmax(c(var(diff(s$y)), var(diff(diff(s$y)))/4, 1e-6, 1e-6), 1e-8))
    cat("\n--", REG[i], "--\n"); flush.console()
    t_est <- Sys.time()
    n0 <- length(linhas)
    cds <- candidatos(rho)
    anota(sprintf("%-12s %-40s candidatos derivados: %2d (%.1fs)", ind,
                  substr(REG[i], 1, 38), length(cds),
                  as.numeric(difftime(Sys.time(), t_est, units = "secs"))))
    for (nm in names(cds)) {
      cd <- cds[[nm]]
      t0 <- Sys.time()
      r  <- ajusta(s$y, s$se, cd$phi, cd$theta, i0)
      dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
      if (is.null(r)) { cat(sprintf("  %-14s falhou/pulado (%.1fs)\n", nm, dt))
                        flush.console(); next }
      linhas[[length(linhas)+1]] <- data.frame(
        indicador = ind, estrato = REG[i], formulacao = nm, npar = r$npar,
        loglik = round(r$loglik,2), aicc = round(r$aicc,1), bic = round(r$bic,1),
        ljung = round(r$ljung,4), eqm1 = round(r$eqm1,3), rrse = round(r$rrse,2),
        stringsAsFactors = FALSE)
      cat(sprintf("  %-14s k=%2d logLik=%9.2f AICc=%7.1f BIC=%7.1f LB=%.3f EQM=%8.2f RRSE=%6.2f%% (%.1fs)\n",
                  nm, r$npar, r$loglik, r$aicc, r$bic, r$ljung, r$eqm1, r$rrse, dt))
      flush.console()   # sem efeito fora de sessao interativa; ver anota()/grava()
    }
    ## grava o estrato assim que fecha -- progresso verificavel e resultado
    ## parcial preservado se a execucao for interrompida
    if (length(linhas) > n0) grava(do.call(rbind, linhas[(n0+1):length(linhas)]))
    anota(sprintf("%-12s %-40s CONCLUIDO: %2d ajustes em %.1f min", ind,
                  substr(REG[i], 1, 38), length(linhas) - n0,
                  as.numeric(difftime(Sys.time(), t_est, units = "mins"))))
  }
}

tab <- do.call(rbind, linhas)
anota("FIM")

################################################################################
cat("\n\n############ ESCOLHA POR CRITÉRIO ############\n")
for (ind in unique(tab$indicador)) {
  cat("\n===", ind, "===\n")
  s <- tab[tab$indicador == ind, ]
  for (e in unique(s$estrato)) {
    x <- s[s$estrato == e, ]
    ## Ljung-Box: entre os que NAO rejeitam brancura (p > 0.05), o mais parcimonioso
    ok <- x[!is.na(x$ljung) & x$ljung > 0.05, ]
    esc_lb <- if (nrow(ok)) ok$formulacao[which.min(ok$npar)] else "nenhum passa"
    cat(sprintf("%-40s LB: %-14s | AICc: %-14s | BIC: %-14s\n", substr(e,1,38),
                esc_lb, x$formulacao[which.min(x$aicc)], x$formulacao[which.min(x$bic)]))
  }
}

cat("\n\n############ CONTAGEM DE ESCOLHAS ############\n")
esc <- do.call(rbind, lapply(split(tab, list(tab$indicador, tab$estrato), drop = TRUE),
  function(x) {
    ok <- x[!is.na(x$ljung) & x$ljung > 0.05, ]
    data.frame(lb = if (nrow(ok)) ok$formulacao[which.min(ok$npar)] else NA,
               aicc = x$formulacao[which.min(x$aicc)],
               bic = x$formulacao[which.min(x$bic)], stringsAsFactors = FALSE)
  }))
cat("\npor Ljung-Box (mais parcimonioso que passa):\n"); print(table(esc$lb, useNA = "ifany"))
cat("\npor AICc:\n"); print(table(esc$aicc))
cat("\npor BIC:\n");  print(table(esc$bic))

cat("\n\n############ O CANDIDATO DO DESENHO ############\n")
d <- tab[tab$formulacao == "MA(4) desenho", ]
if (nrow(d)) {
  cat("passa no Ljung-Box em", sum(d$ljung > 0.05, na.rm=TRUE), "de", nrow(d), "casos\n")
  cat("posicao media no ranking de BIC:",
      round(mean(sapply(split(tab, list(tab$indicador, tab$estrato), drop=TRUE),
        function(x) which(x$formulacao[order(x$bic)] == "MA(4) desenho")[1])), 1),
      "de", round(mean(table(tab$indicador, tab$estrato)), 0), "\n")
}
cat("\nGravado em", SAIDA, "\n")
