################################################################################
## 15_comparacao_formulacoes.R
##
## Compara TODAS as formulacoes do erro amostral, nos tres indicadores e nos
## oito estratos, ate a medida final de precisao:
##   AR(1), AR(2), ARMA(1,1)  -> coeficientes gravados nos pseudo-erros
##   MA(1) a MA(4)            -> invertidos da FAC dos pseudo-erros
##
## Para cada ajuste registra logLik, AIC, BIC, Var(e~) implicita, ganho de
## erro-padrao (RRSE) e vicio relativo.
##
## AVISO SOBRE O CRITERIO. Nao selecionar pela maior RRSE. O ganho de precisao
## e quase uma funcao monotona de quanto o modelo desconta a variancia do
## desenho: como e_t = c_t * e~_t com c_t o erro-padrao do desenho, Var(e~) < 1
## significa que o modelo atribui ao erro amostral menos variancia do que o
## desenho da PNADc diz existir, e o excedente vai para o sinal. Selecionar pela
## RRSE seria selecionar a formulacao que mais discorda do IBGE. O criterio
## defensavel e ajuste (BIC) com fidelidade ao desenho (Var(e~) ~ 1).
## O script reporta a correlacao entre RRSE e Var(e~) para tornar isso explicito.
##
## REPRESENTACAO EM ESPACO DE ESTADOS (forma geral de Harvey/Akaike)
##   r = max(p, q+1);  alpha_t = (e~_t, alpha_2t, ..., alpha_rt)
##   G = [[phi_1, 1, 0, ...], [phi_2, 0, 1, ...], ..., [phi_r, 0, ..., 0]]
##   w_t = v * xi_t,  v = (1, theta_1, ..., theta_{r-1})'  =>  W = sigma2 * v v'
## Subsume AR(1), AR(2), MA(q) e ARMA(1,1) numa unica montagem.
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

BURN  <- 8
SAIDA <- file.path(RAIZ, "outputs", "comparacao_formulacoes")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

COD <- c("bh","ent","sul","trg","mat","nrt","val","cen")
ROT <- c("01 - Belo Horizonte", "02 - Colar e Entorno", "03 - Sul de Minas",
         "04 - Triângulo Mineiro", "05 - Zona da Mata", "06 - Norte de Minas",
         "07 - Vale do Rio Doce", "08 - Central")
names(ROT) <- COD
SUF <- c(bh="bh", ent="ent", sul="sul", trg="trg", mat="mat", nrt="nrt",
         val="rio", cen="cen")

base <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
nomes_reg <- names(base)[1:8]

pega <- function(ind, i) {
  d <- base[[ nomes_reg[i] ]]
  switch(ind,
    desocupados = list(y = d$Total.de.desocupados/1000, se = d$sd_d/1000),
    ocupados    = list(y = d$Total.de.ocupados/1000,    se = d$sd_o/1000),
    taxa        = list(y = d[["Taxa.de.desocupação"]]*100, se = d[["sd_txd"]]*100))
}

arquivo_pe <- function(ind, k) {
  i <- match(k, COD)
  if (ind == "taxa")
    file.path(RAIZ, "pseudoerros_taxa_8reg",
              sprintf("%02d_params_taxa_%s.rds", i, k))
  else
    file.path(RAIZ, "pseudoerros_8reg",
              sprintf("%02d_params_%s.rds", i, SUF[[k]]))
}

## ---- candidatos por estrato -------------------------------------------------
candidatos <- function(ind, k) {
  pe  <- readRDS(arquivo_pe(ind, k))
  taxa <- ind == "taxa"
  pre  <- if (taxa) "taxamod_" else "mod_"
  s    <- SUF[[k]]; sfx <- if (ind == "desocupados") "d" else "o"
  gv <- function(m, nome, pos) {
    b <- pe[[paste0(pre, m)]]
    if (is.null(b)) return(NULL)
    if (taxa) as.numeric(b[[pos]]) else as.numeric(b[[paste0(nome, sfx, s)]])
  }
  cal <- pe[[grep("^calculos", names(pe), value = TRUE)[
    if (taxa) 1 else if (ind == "desocupados") 1 else 2]]]
  fac <- cal$fac[-1]

  out <- list()
  a1 <- gv("ar1", "phi1_ar1_", 1)
  if (length(a1)) out[["AR(1)"]] <- list(phi = a1, theta = numeric(0))
  b2 <- pe[[paste0(pre, "ar2")]]
  if (!is.null(b2)) {
    v <- if (taxa) c(as.numeric(b2[[1]]), as.numeric(b2[[2]]))
         else c(as.numeric(b2[[paste0("phi1_ar2_", sfx, s)]]),
                as.numeric(b2[[paste0("phi2_ar2_", sfx, s)]]))
    if (all(is.finite(v))) out[["AR(2)"]] <- list(phi = v, theta = numeric(0))
  }
  b11 <- pe[[paste0(pre, "arma11")]]
  if (!is.null(b11)) {
    v <- if (taxa) c(as.numeric(b11[[1]]), as.numeric(b11[[2]]))
         else c(as.numeric(b11[[paste0("phi1_arma11_", sfx, s)]]),
                as.numeric(b11[[paste0("theta1_arma11_", sfx, s)]]))
    if (all(is.finite(v))) out[["ARMA(1,1)"]] <- list(phi = v[1], theta = v[2])
  }
  for (q in 1:4) {
    inv <- inverte_ma(fac, q)
    if (!is.null(inv)) out[[paste0("MA(", q, ")")]] <-
      list(phi = numeric(0), theta = inv$theta, invertivel = inv$invertivel)
  }
  out
}

inverte_ma <- function(rho, q) {
  alvo <- rho[1:q]
  perda <- function(z) {
    r <- tryCatch(ARMAacf(ma = as.numeric(z), lag.max = q)[-1],
                  error = function(e) rep(NA, q))
    if (any(!is.finite(r))) return(1e6)
    sum((r - alvo)^2)
  }
  melhor <- NULL
  for (t in 1:15) {
    ini <- if (t == 1) rep(0.2, q) else runif(q, -0.7, 0.9)
    r <- try(optim(ini, perda, method = "BFGS",
                   control = list(maxit = 3000, reltol = 1e-12)), silent = TRUE)
    if (inherits(r, "try-error")) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  if (is.null(melhor) || melhor$value > 1e-4) return(NULL)
  th <- melhor$par
  list(theta = th, invertivel = all(Mod(polyroot(c(1, th))) > 1.0001))
}

## ---- montagem geral ARMA(p,q) ----------------------------------------------
monta <- function(se, phi, theta) {
  p <- length(phi); q <- length(theta)
  r <- max(p, q + 1)
  v <- c(1, theta, rep(0, max(0, r - 1 - q)))[1:r]
  ph <- c(phi, rep(0, max(0, r - p)))[1:r]
  ns <- 5 + r
  function(params) {
    m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se, addInt = FALSE)
    if (r > 1) {
      m$FF <- cbind(m$FF, matrix(0, 1, r - 1))
      m$GG <- rbind(m$GG, matrix(0, r - 1, 6))
      m$GG <- cbind(m$GG, matrix(0, ns, r - 1))
    }
    ## bloco ARMA nas posicoes 6..(5+r)
    G <- matrix(0, r, r)
    G[, 1] <- ph
    if (r > 1) G[1:(r-1), 2:r] <- diag(r - 1)
    m$GG[6:ns, 6:ns] <- G
    W <- matrix(0, ns, ns)
    W[6:ns, 6:ns] <- exp(params[5]) * (v %*% t(v))
    W[1,1] <- exp(params[1]); W[2,2] <- exp(params[2]); W[3,3] <- exp(params[3])
    d <- diag(W); diag(W)[d == 0] <- 1e-10
    m$W <- W
    m$V <- exp(params[4])
    m$m0 <- rep(0, ns)
    m$C0 <- diag(x = 1e6, ns)
    m
  }
}

## Var(e~) estacionaria: resolve P = G P G' + W no bloco ARMA (equacao de
## Lyapunov discreta, via vec). Exata e valida para qualquer ARMA(p,q).
var_implicita <- function(phi, theta, s2) {
  p <- length(phi); q <- length(theta); r <- max(p, q + 1)
  v  <- c(1, theta, rep(0, max(0, r - 1 - q)))[1:r]
  ph <- c(phi, rep(0, max(0, r - p)))[1:r]
  G <- matrix(0, r, r); G[, 1] <- ph
  if (r > 1) G[1:(r-1), 2:r] <- diag(r - 1)
  W <- s2 * (v %*% t(v))
  A <- diag(r * r) - kronecker(G, G)
  P <- try(matrix(solve(A, as.vector(W)), r, r), silent = TRUE)
  if (inherits(P, "try-error")) return(NA_real_)
  P[1, 1]
}

ajusta <- function(y, se, phi, theta, i0) {
  fn  <- monta(se, phi, theta)
  obj <- function(pp) {
    ll <- try(dlmLL(y, fn(pp)), silent = TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
    ll
  }
  melhor <- NULL
  for (p0 in list(i0, i0 + 1, i0 - 1, rep(0, 5))) {
    r <- try(optim(p0, obj, method = "L-BFGS-B", control = list(maxit = 5000)),
             silent = TRUE)
    if (inherits(r, "try-error") || r$value >= 1e9) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  if (is.null(melhor)) return(NULL)
  mod <- fn(melhor$par)
  flt <- try(dlmFilter(y, mod), silent = TRUE)
  if (inherits(flt, "try-error")) return(NULL)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)
  se_tr <- dropFirst(sapply(mse, function(x) sqrt(x[1, 1])))
  est   <- dropFirst(flt$m)
  sinal <- est[, 1] + est[, 3] + est[, 5]
  k <- 5
  list(loglik = -melhor$value, bic = 2*melhor$value + k*log(length(y)),
       aic = 2*melhor$value + 2*k, se_tr = se_tr, sinal = sinal,
       s2 = exp(melhor$par[5]))
}

################################################################################

linhas <- list()
for (ind in c("desocupados", "ocupados", "taxa")) {
  cat("\n\n#################### ", toupper(ind), " ####################\n")
  for (i in seq_along(COD)) {
    k <- COD[i]
    s <- pega(ind, i)
    ix <- (BURN + 1):length(s$y)
    i0 <- log(pmax(c(var(diff(s$y)), var(diff(diff(s$y)))/4, 1e-6, 1e-6, 0.5), 1e-8))
    cands <- candidatos(ind, k)
    cat("\n--", ROT[[k]], "--\n")
    for (nm in names(cands)) {
      cd <- cands[[nm]]
      r  <- ajusta(s$y, s$se, cd$phi, cd$theta, i0)
      if (is.null(r)) { cat(sprintf("  %-10s falhou\n", nm)); next }
      ve   <- var_implicita(cd$phi, cd$theta, r$s2)
      rrse <- mean((s$se[ix] - r$se_tr[ix]) / s$se[ix]) * 100
      vic  <- sum(r$sinal[ix] - s$y[ix]) / sum(s$y[ix]) * 100
      inv  <- if (is.null(cd$invertivel)) NA else cd$invertivel
      linhas[[length(linhas)+1]] <- data.frame(
        indicador = ind, estrato = ROT[[k]], formulacao = nm,
        invertivel = inv, loglik = round(r$loglik, 2), bic = round(r$bic, 1),
        var_e = round(ve, 3), rrse = round(rrse, 2), vicio = round(vic, 2),
        stringsAsFactors = FALSE)
      cat(sprintf("  %-10s logLik=%9.2f  BIC=%8.1f  Var(e~)=%6.3f  RRSE=%7.2f%%  %s\n",
                  nm, r$loglik, r$bic, ve, rrse,
                  if (isTRUE(!inv)) "[nao invertivel]" else ""))
    }
  }
}

tab <- do.call(rbind, linhas)
write.csv(tab, file.path(SAIDA, "comparacao_formulacoes.csv"), row.names = FALSE)

################################################################################
## Sinteses
################################################################################

cat("\n\n############ MELHOR FORMULACAO POR CRITERIO ############\n")
for (ind in unique(tab$indicador)) {
  s <- tab[tab$indicador == ind, ]
  cat("\n===", ind, "===\n")
  for (e in unique(s$estrato)) {
    x <- s[s$estrato == e, ]
    cat(sprintf("%-26s  BIC: %-10s (%.1f)   |  RRSE: %-10s (%.1f%%, Var(e~)=%.2f)\n",
        e, x$formulacao[which.min(x$bic)], min(x$bic),
        x$formulacao[which.max(x$rrse)], max(x$rrse),
        x$var_e[which.max(x$rrse)]))
  }
}

cat("\n\n############ O GANHO MEDE DISCORDANCIA COM O DESENHO? ############\n")
ok <- is.finite(tab$rrse) & is.finite(tab$var_e) & tab$var_e > 0 & tab$var_e < 5
cat("n =", sum(ok), " ajustes\n")
cat("correlacao de Pearson  entre RRSE e Var(e~):",
    round(cor(tab$rrse[ok], tab$var_e[ok]), 3), "\n")
cat("correlacao de Spearman entre RRSE e Var(e~):",
    round(cor(tab$rrse[ok], tab$var_e[ok], method = "spearman"), 3), "\n")
q <- cut(tab$var_e[ok], breaks = c(0, 0.25, 0.5, 0.75, 1, 5),
         labels = c("<0,25", "0,25-0,50", "0,50-0,75", "0,75-1", ">1"))
cat("\nRRSE media por faixa de Var(e~):\n")
print(round(tapply(tab$rrse[ok], q, mean), 1))
cat("\nnumero de ajustes por faixa:\n")
print(table(q))

cat("\nGravado em", SAIDA, "\n")
