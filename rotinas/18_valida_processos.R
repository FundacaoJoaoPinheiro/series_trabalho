################################################################################
## 18_valida_processos.R
##
## Valida que a montagem em espaço de estados do erro amostral REPRODUZ de fato
## o processo pretendido, para cada família de candidatos.
##
## Motivo: a issue #20 (termo MA inerte) sobreviveu por anos porque ninguém
## conferiu se a matriz montada gerava a autocorrelação pretendida. Este script
## é a rede de proteção que faltava.
##
## MÉTODO (analítico, não simulação)
## Para o bloco de estados do erro amostral com transição G e inovação w = v*xi:
##   variância estacionária:  P = G P G' + sigma2 v v'   (equação de Lyapunov)
##   autocovariância no lag h: gamma(h) = [G^h P]_{11}
##   autocorrelação:           rho(h)   = gamma(h) / P[1,1]
## Compara-se rho(h) com o valor teórico de ARMAacf(ar = phi, ma = theta).
## Testa-se também que, sob a restrição, P[1,1] = 1 exatamente.
################################################################################

TOL <- 1e-8
falhas <- 0
ok_n   <- 0

reporta <- function(nome, ok, detalhe = "") {
  if (ok) { ok_n <<- ok_n + 1; cat(sprintf("  [ok]    %-34s %s\n", nome, detalhe)) }
  else    { falhas <<- falhas + 1; cat(sprintf("  [FALHA] %-34s %s\n", nome, detalhe)) }
}

################################################################################
## Montagem do bloco (a MESMA usada em rotinas/15_ e 16_)
################################################################################
blocos <- function(phi, theta) {
  p <- length(phi); q <- length(theta); r <- max(p, q + 1, 1)
  v  <- c(1, theta, rep(0, max(0, r - 1 - q)))[1:r]
  ph <- c(phi,  rep(0, max(0, r - p)))[1:r]
  G <- matrix(0, r, r); G[, 1] <- ph
  if (r > 1) G[1:(r-1), 2:r] <- diag(r - 1)
  list(G = G, v = v, r = r)
}

## P estacionário via Lyapunov
P_estacionario <- function(G, v, s2) {
  r <- nrow(G)
  W <- s2 * (v %*% t(v))
  matrix(solve(diag(r*r) - kronecker(G, G), as.vector(W)), r, r)
}

## autocorrelação implicada pela montagem
acf_implicada <- function(phi, theta, s2 = 1, lag.max = 10) {
  b <- blocos(phi, theta)
  P <- P_estacionario(b$G, b$v, s2)
  Gh <- diag(b$r)
  out <- numeric(lag.max)
  for (h in 1:lag.max) { Gh <- Gh %*% b$G; out[h] <- (Gh %*% P)[1, 1] / P[1, 1] }
  list(rho = out, var = P[1, 1])
}

################################################################################
cat("################ 1. FAMÍLIAS DE PROCESSOS ################\n\n")

casos <- list(
  list(n = "Ruído branco",  phi = numeric(0), theta = numeric(0)),
  list(n = "AR(1) phi=.30", phi = 0.30,       theta = numeric(0)),
  list(n = "AR(1) phi=.71", phi = 0.71,       theta = numeric(0)),
  list(n = "AR(2)",         phi = c(0.56, 0.17),  theta = numeric(0)),
  list(n = "AR(3)",         phi = c(0.45, 0.20, 0.10), theta = numeric(0)),
  list(n = "AR(4)",         phi = c(0.40, 0.18, 0.10, 0.07), theta = numeric(0)),
  list(n = "AR(5)",         phi = c(0.35, 0.15, 0.10, 0.07, 0.05), theta = numeric(0)),
  list(n = "AR(6)",         phi = c(0.30, 0.15, 0.10, 0.07, 0.05, 0.04), theta = numeric(0)),
  list(n = "MA(1) th=.353", phi = numeric(0), theta = 0.353),
  list(n = "MA(2)",         phi = numeric(0), theta = c(0.28, 0.33)),
  list(n = "MA(3)",         phi = numeric(0), theta = c(0.25, 0.28, 0.19)),
  list(n = "MA(4)",         phi = numeric(0), theta = c(0.22, 0.19, 0.13, 0.07)),
  list(n = "MA(5)",         phi = numeric(0), theta = c(0.20, 0.17, 0.12, 0.08, 0.05)),
  list(n = "MA(6)",         phi = numeric(0), theta = c(0.18, 0.15, 0.11, 0.08, 0.05, 0.03)),
  list(n = "ARMA(1,1)",     phi = 0.382,      theta = -0.021),
  list(n = "ARMA(1,1) forte", phi = 0.892,    theta = 0.691),
  list(n = "ARMA(2,1)",     phi = c(0.50, 0.20), theta = 0.30),
  list(n = "ARMA(1,2)",     phi = 0.60,       theta = c(0.25, 0.15))
)

for (cs in casos) {
  im <- acf_implicada(cs$phi, cs$theta, s2 = 1, lag.max = 10)
  te <- if (!length(cs$phi) && !length(cs$theta)) rep(0, 10)
        else ARMAacf(ar = cs$phi, ma = cs$theta, lag.max = 10)[-1]
  d <- max(abs(im$rho - as.numeric(te)))
  reporta(cs$n, d < TOL, sprintf("max |rho_montado - rho_teorico| = %.2e", d))
}

################################################################################
cat("\n################ 2. RESTRIÇÃO Var(e~) = 1 ################\n\n")

for (cs in casos) {
  b   <- blocos(cs$phi, cs$theta)
  p11 <- P_estacionario(b$G, b$v, 1)[1, 1]
  s2  <- 1 / p11
  v1  <- P_estacionario(b$G, b$v, s2)[1, 1]
  reporta(paste("Var(e~)=1:", cs$n), abs(v1 - 1) < TOL,
          sprintf("sigma2 = %.5f  ->  Var(e~) = %.10f", s2, v1))
}

################################################################################
cat("\n################ 3. MA(4) COM PESOS DO DESENHO (Bartlett) ################\n\n")
## O esquema 1-2(5) da PNADc da sobreposicao (5-j)/5 entre t e t-j.
## Se o erro amostral fosse a media de K grupos com contribuicao proporcional a
## sobreposicao, a autocorrelacao seria rho_j = (5-j)/5. Buscamos o MA(4) cuja
## FAC reproduz esse padrao.
sobrep <- (5 - (1:4)) / 5
cat("sobreposicao implicada pelo desenho:", paste(sobrep, collapse = "  "), "\n")

perda <- function(th) {
  r <- tryCatch(ARMAacf(ma = th, lag.max = 4)[-1], error = function(e) rep(NA, 4))
  if (any(!is.finite(r))) return(1e6)
  sum((as.numeric(r) - sobrep)^2)
}
melhor <- NULL
for (t in 1:40) {
  ini <- if (t == 1) c(.8,.6,.4,.2) else runif(4, -0.5, 1.5)
  r <- try(optim(ini, perda, method = "BFGS", control = list(maxit = 5000, reltol = 1e-14)),
           silent = TRUE)
  if (inherits(r, "try-error")) next
  if (is.null(melhor) || r$value < melhor$value) melhor <- r
}
th_b <- melhor$par
rho_b <- as.numeric(ARMAacf(ma = th_b, lag.max = 4)[-1])
cat("theta obtido :", paste(round(th_b, 4), collapse = "  "), "\n")
cat("FAC implicada:", paste(round(rho_b, 4), collapse = "  "), "\n")
cat("residuo      :", format(sqrt(melhor$value), scientific = TRUE), "\n")
reporta("MA(4)-Bartlett reproduz a sobreposicao", sqrt(melhor$value) < 1e-4,
        sprintf("desvio = %.2e", sqrt(melhor$value)))
inv <- all(Mod(polyroot(c(1, th_b))) > 1)
reporta("MA(4)-Bartlett invertivel", inv,
        sprintf("|raizes| = %s", paste(round(Mod(polyroot(c(1, th_b))), 3), collapse = " ")))
im <- acf_implicada(numeric(0), th_b, 1, 6)
reporta("MA(4)-Bartlett zera a partir do lag 5", max(abs(im$rho[5:6])) < TOL,
        sprintf("rho5 = %.2e, rho6 = %.2e", im$rho[5], im$rho[6]))

################################################################################
cat("\n################ 4. YULE-WALKER: AR(p) a partir da FAC ################\n\n")
yule_walker <- function(rho, p) {
  R <- if (p == 1) matrix(1,1,1) else toeplitz(c(1, rho[1:(p-1)]))
  as.numeric(solve(R, rho[1:p]))
}
## ida e volta: gerar FAC de um AR conhecido, recuperar os phi
for (cs in casos[grepl("^AR\\(", sapply(casos, `[[`, "n"))]) {
  p   <- length(cs$phi)
  rho <- as.numeric(ARMAacf(ar = cs$phi, lag.max = p + 2)[-1])
  ph  <- yule_walker(rho, p)
  d   <- max(abs(ph - cs$phi))
  reporta(paste("YW recupera", cs$n), d < 1e-8,
          sprintf("max |phi_recuperado - phi| = %.2e", d))
}

################################################################################
cat("\n################ 5. INVERSÃO MA: FAC -> theta ################\n\n")
torna_invertivel <- function(th) {
  r <- polyroot(c(1, th)); dentro <- Mod(r) < 1
  if (!any(dentro)) return(th)
  r[dentro] <- 1/r[dentro]
  cf <- 1; for (ri in r) cf <- c(cf, 0) - c(0, cf)/ri
  Re(cf[-1])
}
for (cs in casos[grepl("^MA\\(", sapply(casos, `[[`, "n"))]) {
  q   <- length(cs$theta)
  rho <- as.numeric(ARMAacf(ma = cs$theta, lag.max = q)[-1])
  perda2 <- function(z) {
    r <- tryCatch(ARMAacf(ma = as.numeric(z), lag.max = q)[-1], error = function(e) rep(NA,q))
    if (any(!is.finite(r))) return(1e6)
    sum((as.numeric(r) - rho)^2)
  }
  m <- NULL
  for (t in 1:20) {
    ini <- if (t == 1) rep(0.2, q) else runif(q, -0.7, 0.9)
    r <- try(optim(ini, perda2, method="BFGS", control=list(maxit=3000, reltol=1e-14)), silent=TRUE)
    if (inherits(r, "try-error")) next
    if (is.null(m) || r$value < m$value) m <- r
  }
  th <- torna_invertivel(m$par)
  rr <- as.numeric(ARMAacf(ma = th, lag.max = q)[-1])
  reporta(paste("inversao recupera", cs$n), max(abs(rr - rho)) < 1e-6,
          sprintf("FAC reproduzida com desvio %.2e; invertivel = %s",
                  max(abs(rr - rho)), all(Mod(polyroot(c(1, th))) > 1 - 1e-9)))
}

################################################################################
cat("\n################ 6. O ERRO AMOSTRAL ENTRA ESCALADO PELO se ################\n\n")
suppressMessages(library(dlm))
set.seed(7)
se_t <- seq(2, 5, length.out = 52)          # erro-padrao do desenho variando
b <- blocos(0.353 * 0, 0.353)               # MA(1)
s2 <- 1 / P_estacionario(b$G, b$v, 1)[1,1]
ns <- 5 + b$r
m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se_t, addInt = FALSE)
m$FF <- cbind(m$FF, matrix(0, 1, b$r - 1))
m$GG <- rbind(m$GG, matrix(0, b$r - 1, 6)); m$GG <- cbind(m$GG, matrix(0, ns, b$r - 1))
m$GG[6:ns, 6:ns] <- b$G
W <- matrix(0, ns, ns); W[6:ns, 6:ns] <- s2 * (b$v %*% t(b$v))
m$W <- W
cat("FF (linha de medida):", paste(round(as.numeric(m$FF), 3), collapse = " "), "\n")
reporta("FF liga o estado 6 (erro amostral)", m$FF[1, 6] == 1,
        "o regressor dlmModReg ocupa a posicao 6")
reporta("FF ignora os estados auxiliares", all(m$FF[1, 7:ns] == 0),
        "auxiliares nao entram na medida, so carregam xi defasado")
reporta("JX escala pelo se do desenho", !is.null(m$X) && all(abs(m$X - se_t) < TOL),
        "X = se_t, logo e_t = e~_t * se_t")
reporta("Var(e~) = 1 no bloco montado",
        abs(P_estacionario(b$G, b$v, s2)[1,1] - 1) < TOL, "")

################################################################################
cat("\n################################ RESUMO ################################\n")
cat("verificacoes ok :", ok_n, "\n")
cat("FALHAS          :", falhas, "\n")
if (falhas == 0) cat("\nTodas as montagens reproduzem o processo pretendido.\n")
