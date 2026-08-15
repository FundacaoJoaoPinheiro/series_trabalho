################################################################################
## _id_funcoes.R — funções compartilhadas da identificação do erro amostral
##
## Extraídas de rotinas/19_identificacao.R para serem carregadas tanto pela
## versão sequencial quanto pela paralela (rotinas/20_), evitando divergência
## entre as duas. As montagens aqui são as validadas em
## rotinas/18_valida_processos.R — 56 verificações, 0 falhas.
################################################################################

## ---- bloco ARMA em espaço de estados (forma de Harvey/Akaike) ---------------
blocos <- function(phi, theta) {
  p <- length(phi); q <- length(theta); r <- max(p, q + 1, 1)
  v  <- c(1, theta, rep(0, max(0, r - 1 - q)))[1:r]
  ph <- c(phi,  rep(0, max(0, r - p)))[1:r]
  G <- matrix(0, r, r); G[, 1] <- ph
  if (r > 1) G[1:(r-1), 2:r] <- diag(r - 1)
  list(G = G, v = v, r = r)
}

## variância estacionária: resolve P = G P G' + s2 v v' (Lyapunov, via vec)
P_est <- function(G, v, s2) {
  r <- nrow(G)
  matrix(solve(diag(r*r) - kronecker(G, G), as.vector(s2 * (v %*% t(v)))), r, r)
}

## modelo estrutural com erro amostral e Var(e~) = 1
monta <- function(se, phi, theta) {
  b <- blocos(phi, theta)
  p11 <- try(P_est(b$G, b$v, 1)[1,1], silent = TRUE)
  if (inherits(p11, "try-error") || !is.finite(p11) || p11 <= 0) return(NULL)
  s2 <- 1 / p11
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
    m$W <- W; m$V <- exp(pp[4])
    m$m0 <- rep(0, ns); m$C0 <- diag(x = 1e6, ns)
    m
  }
}

## ---- derivação dos candidatos a partir da FAC ------------------------------
yule_walker <- function(rho, p) {
  R <- if (p == 1) matrix(1,1,1) else toeplitz(c(1, rho[1:(p-1)]))
  ph <- try(as.numeric(solve(R, rho[1:p])), silent = TRUE)
  if (inherits(ph, "try-error") || any(!is.finite(ph))) return(NULL)
  ph
}
estacionario <- function(phi, tol = 1.0001)
  !length(phi) || all(Mod(polyroot(c(1, -phi))) > tol)

## converte um MA para a representação invertível equivalente (preserva a FAC)
torna_invertivel <- function(th) {
  r <- polyroot(c(1, th)); dentro <- Mod(r) < 1
  if (!any(dentro)) return(th)
  r[dentro] <- 1/r[dentro]
  cf <- 1; for (ri in r) cf <- c(cf, 0) - c(0, cf)/ri
  Re(cf[-1])
}

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
    if (melhor$value < 1e-12) break
  }
  ## ACEITACAO ESTRITA. A tolerancia antiga (soma de quadrados < 0,05) admitia
  ## ajustes INFACTIVEIS: para ocupados, com rho1 = 0,66, o MA(1) tem teto
  ## teorico de 0,5, o melhor resultado possivel deixa residuo de 0,026 -- e
  ## passava. O otimizador entao empurrava theta para a fronteira (theta = 1,
  ## raiz sobre o circulo unitario), e o filtro de Kalman travava nesse processo
  ## degenerado. Foi a causa dos ajustes de ocupados nunca terminarem.
  ## Agora exige-se casamento efetivo da FAC E raizes afastadas da fronteira.
  if (is.null(melhor)) return(NULL)
  ph <- if (p) melhor$par[1:p] else numeric(0)
  th <- if (q) torna_invertivel(melhor$par[(p+1):(p+q)]) else numeric(0)
  r <- tryCatch(ARMAacf(ar = ph, ma = th, lag.max = nlags)[-1],
                error = function(e) NULL)
  if (is.null(r) || max(abs(as.numeric(r) - alvo)) > 0.01) return(NULL)  # infactivel
  if (length(ph) && !estacionario(ph, 1.02)) return(NULL)
  if (length(th) && any(Mod(polyroot(c(1, th))) < 1.02)) return(NULL)    # fronteira
  list(phi = ph, theta = th)
}

candidatos <- function(rho) {
  out <- list("Ruído branco" = list(phi = numeric(0), theta = numeric(0)))
  for (p in 1:6) {
    ph <- yule_walker(rho, p)
    ## tol 1,02: raiz sobre o circulo unitario deixa o filtro mal condicionado
    if (!is.null(ph) && estacionario(ph, 1.02))
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
  ## candidato do desenho: soma móvel de 5 choques, sem parâmetro livre.
  ## FAC = (5-j)/5, exatamente a sobreposição do esquema 1-2(5) da PNADc.
  out[["MA(4) desenho"]] <- list(phi = numeric(0), theta = rep(1, 4))
  out
}

## ---- ajuste e métricas ------------------------------------------------------
ajusta <- function(y, se, phi, theta, i0, burn = 8) {
  fn <- monta(se, phi, theta)
  if (is.null(fn)) return(NULL)
  ## TETO DE AVALIACOES.
  ## Cada dlmLL custa milissegundos; o que consome horas e o otimizador fazer
  ## dezenas de milhares delas quando cai numa regiao mal condicionada. Um teto
  ## no NUMERO de avaliacoes limita cada partida a poucos segundos.
  ## Por que nao setTimeLimit: ele so interrompe no laco de avaliacao do R, e o
  ## tempo aqui e gasto dentro do C do dlm. O contador vive em codigo R, entao o
  ## erro e lancado ENTRE chamadas ao C -- funciona onde o cronometro falha.
  MAX_AVAL <- 3000
  n_aval <- 0
  obj <- function(pp) {
    n_aval <<- n_aval + 1
    if (n_aval > MAX_AVAL) stop("teto de avaliacoes")
    ll <- try(dlmLL(y, fn(pp)), silent = TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
    ll
  }
  ## Partidas TODAS escaladas pelos dados. A antiga `rep(0,4)` fixava todas as
  ## variancias em 1 -- razoavel para desocupados (serie na casa das centenas),
  ## absurdo para ocupados (casa dos milhares), onde jogava o filtro numa regiao
  ## degenerada e o otimizador nao saia mais de la. Era a causa dos ajustes de
  ## ocupados nunca terminarem, junto com os candidatos na fronteira.
  ## CAIXA DE BUSCA LIMITADA -- e o unico recurso que de fato impede o
  ## travamento. O teto de avaliacoes acima nao basta porque o tempo se perde
  ## DENTRO de UMA chamada ao dlmLL: para certos valores de parametro o SVD do
  ## filtro do dlm nao converge, e isso ocorre em codigo C, ininterrompivel.
  ## Restringir o dominio impede o otimizador de alcancar essa regiao.
  ## exp(+-20) cobre de 2e-9 a 4,8e8, folga larga para series em milhares.
  LIM <- 20
  melhor <- NULL
  for (p0 in list(i0, i0 + 1, i0 - 1, i0 - 3)) {
    n_aval <- 0                      # o teto vale por partida
    p0 <- pmin(pmax(p0, -LIM + 1), LIM - 1)
    r <- try(optim(p0, obj, method = "L-BFGS-B",
                   lower = rep(-LIM, 4), upper = rep(LIM, 4),
                   control = list(maxit = 300)), silent = TRUE)
    if (inherits(r, "try-error") || !is.finite(r$value) || r$value >= 1e9) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  if (is.null(melhor)) return(NULL)
  mod <- fn(melhor$par)
  flt <- try(dlmFilter(y, mod), silent = TRUE)
  if (inherits(flt, "try-error")) return(NULL)

  rp <- try(residuals(flt, type = "standardized", sd = FALSE), silent = TRUE)
  rb <- try(residuals(flt, type = "raw", sd = FALSE), silent = TRUE)
  if (inherits(rp, "try-error")) return(NULL)
  rp <- as.numeric(rp)[-(1:burn)]
  rb <- if (inherits(rb, "try-error")) rep(NA, length(rp)) else as.numeric(rb)[-(1:burn)]

  npar <- 4 + length(phi) + length(theta)
  n    <- length(y); ll <- -melhor$value
  lb   <- try(Box.test(rp, lag = 8, type = "Ljung-Box")$p.value, silent = TRUE)
  if (inherits(lb, "try-error")) lb <- NA_real_

  mse   <- dlmSvd2var(flt$U.C, flt$D.C)
  se_tr <- dropFirst(sapply(mse, function(x) sqrt(x[1,1])))
  ix <- (burn+1):n

  list(loglik = ll, npar = npar,
       aicc = 2*npar - 2*ll + (2*npar*(npar+1))/max(1, n - npar - 1),
       bic  = npar*log(n) - 2*ll,
       ljung = lb, eqm1 = mean(rb^2, na.rm = TRUE),
       rrse = mean((se[ix] - se_tr[ix]) / se[ix]) * 100)
}
