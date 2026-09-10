################################################################################
## 12_erro_amostral_maq.R
##
## Testa MA(1), MA(2), MA(3) e MA(4) para o erro amostral dos desocupados,
## contra o AR(1) usado na tese.
##
## MOTIVACAO. O esquema de rotacao 1-2(5) da PNADc implica sobreposicao de
## (5-j)/5 entre os trimestres t e t-j: 0,80 / 0,60 / 0,40 / 0,20 e ZERO a
## partir de j = 5. Sem sobreposicao nao ha covariancia, logo a autocovariancia
## do erro amostral e ESTRUTURALMENTE NULA a partir do lag 5 — o que caracteriza
## um MA(4), nao um AR(1) (decaimento geometrico que nunca zera) nem um MA(1)
## (que zera cedo demais, no lag 2).
## A propria tese (p. 179) lista 80/60/40/20 e conclui AR(1) na frase seguinte.
##
## REPRESENTACAO EM ESPACO DE ESTADOS (forma de Harvey generalizada)
##   estado 6      = e~_t
##   estado 7      = xi_t
##   estados 8..6+q= xi_{t-1} ... xi_{t-q+1}
##   linha 6 de GG : [0, theta_1, ..., theta_q]   (le xi_{t-1}..xi_{t-q})
##   linha 7 de GG : nula                          (xi_t e ruido puro)
##   linha 7+j     : desloca xi
##   W: bloco 2x2 (6,7) inteiro = s2  -> MESMA inovacao nos dois estados
##      (issue #20: preencher so a diagonal deixa a acf de lag 1 em zero)
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
stopifnot(dir.exists(file.path(RAIZ, "pseudoerros_8reg")))

BURN  <- 8
SAIDA <- file.path(RAIZ, "outputs", "erro_amostral_maq")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

ARQ <- c(bh="01_params_bh.rds",  ent="02_params_ent.rds", sul="03_params_sul.rds",
         trg="04_params_trg.rds", mat="05_params_mat.rds", nrt="06_params_nrt.rds",
         rio="07_params_rio.rds", cen="08_params_cen.rds")
ROT <- c(bh="01 - Belo Horizonte", ent="02 - Colar e Entorno Metrop. de BH",
         sul="03 - Sul de Minas", trg="04 - Triângulo Mineiro",
         mat="05 - Zona da Mata", nrt="06 - Norte de Minas",
         rio="07 - Vale do Rio Doce", cen="08 - Central")

base <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
nomes_reg <- names(base)[1:8]

################################################################################
## 1. Inversao: da FAC observada para os coeficientes MA(q)
################################################################################

## Resolve theta_1..theta_q que reproduzem rho_1..rho_q de um MA(q).
## Parametrizacao por raizes reciprocas via tanh -> garante invertibilidade.
inverte_ma <- function(rho, q) {
  alvo <- rho[1:q]
  perda <- function(z) {
    th <- as.numeric(z)
    r  <- tryCatch(ARMAacf(ma = th, lag.max = q)[-1], error = function(e) rep(NA, q))
    if (any(!is.finite(r))) return(1e6)
    sum((r - alvo)^2)
  }
  melhor <- NULL
  for (tentativa in 1:12) {
    ini <- if (tentativa == 1) rep(0.2, q) else runif(q, -0.6, 0.9)
    r <- try(optim(ini, perda, method = "BFGS",
                   control = list(maxit = 2000, reltol = 1e-12)), silent = TRUE)
    if (inherits(r, "try-error")) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  if (is.null(melhor)) return(NULL)
  th <- melhor$par
  ## checa invertibilidade: raizes de 1 + th1 z + ... + thq z^q fora do circulo
  raizes <- polyroot(c(1, th))
  list(theta = th, erro = sqrt(melhor$value), invertivel = all(Mod(raizes) > 1.0001))
}

################################################################################
## 2. Modelo estrutural com erro amostral MA(q) ou AR(1)
################################################################################

monta <- function(se, tipo, phi = 0, theta = numeric(0)) {
  q  <- length(theta)
  ns <- 6 + q
  function(params) {
    m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se, addInt = FALSE)
    if (q > 0) {
      m$FF <- cbind(m$FF, matrix(0, 1, q))
      m$GG <- rbind(m$GG, matrix(0, q, 6))
      m$GG <- cbind(m$GG, matrix(0, ns, q))
      m$GG[6, 6] <- phi
      for (j in 1:q) m$GG[6, 6 + j] <- theta[j]      # le xi_{t-j}
      if (q > 1) for (j in 1:(q - 1)) m$GG[7 + j, 6 + j] <- 1   # desloca xi
      W <- matrix(0, ns, ns)
      s2 <- exp(params[5])
      W[6, 6] <- s2; W[7, 7] <- s2; W[6, 7] <- s2; W[7, 6] <- s2
    } else {
      m$GG[6, 6] <- phi
      W <- matrix(0, 6, 6)
      W[6, 6] <- exp(params[5])
    }
    W[1, 1] <- exp(params[1])
    W[2, 2] <- exp(params[2])
    W[3, 3] <- exp(params[3])
    d <- diag(W); diag(W)[d == 0] <- 1e-10
    m$W  <- W
    m$V  <- exp(params[4])
    m$m0 <- rep(0, ns)
    m$C0 <- diag(x = 1e6, ns)
    m
  }
}

## Var(e~) implicita: deve ser ~1 para o modelo respeitar a variancia do desenho
var_implicita <- function(tipo, phi, theta, s2) {
  if (tipo == "ar1") s2 / (1 - phi^2) else s2 * (1 + sum(theta^2))
}

ajusta <- function(y, se, tipo, phi, theta, i0) {
  fn  <- monta(se, tipo, phi, theta)
  obj <- function(p) {
    ll <- try(dlmLL(y, fn(p)), silent = TRUE)
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
  flt <- dlmFilter(y, mod)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)
  se_tr <- dropFirst(sapply(mse, function(x) sqrt(x[1, 1])))
  hp <- exp(melhor$par)
  k  <- 5                       # hiperparametros estimados
  list(loglik = -melhor$value, aic = 2 * melhor$value + 2 * k,
       bic = 2 * melhor$value + k * log(length(y)),
       hp = hp, se_tr = se_tr,
       var_e = var_implicita(tipo, phi, theta, hp[5]))
}

################################################################################
## 3. Execucao
################################################################################

INI <- rbind(
  bh  = c(21.9403,  20.3976, 1e-6, 1e-6, 0.5430),
  ent = c( 1e-6,   120.4332, 2.8673, 0.0044, 0.4923),
  sul = c(142.8495,  0.9414, 1e-6, 1e-6, 0.3764),
  trg = c(798.9974,  0.6172, 1e-6, 1e-6, 0.3357),
  mat = c(100.9284,  0.1223, 1e-6, 0.0028, 0.3478),
  nrt = c(194.5015,  1.1215, 1e-6, 0.0037, 0.3780),
  rio = c(198.9328,  0.7609, 1e-6, 0.0002, 0.1260),
  cen = c(159.6191,  0.0021, 1e-6, 1e-6, 0.2429))

linhas <- list()
for (k in names(ARQ)) {
  d  <- base[[ nomes_reg[ match(k, names(ARQ)) ] ]]
  y  <- d$Total.de.desocupados / 1000
  se <- d$sd_d / 1000
  pe  <- readRDS(file.path(RAIZ, "pseudoerros_8reg", ARQ[[k]]))
  fac <- pe[[paste0("calculos_desocupada_", k)]]$fac[-1]     # rho_1, rho_2, ...
  i0  <- log(INI[k, ])
  ix  <- (BURN + 1):length(y)

  cat("\n########", ROT[[k]], "########\n")
  cat("FAC observada (lags 1-5):", paste(sprintf("%.3f", fac[1:5]), collapse = "  "), "\n")

  ## AR(1) — referencia da tese
  phi <- pe$mod_ar1[[paste0("phi1_ar1_d", k)]]
  r   <- ajusta(y, se, "ar1", phi, numeric(0), i0)
  linhas[[length(linhas)+1]] <- data.frame(
    estrato = ROT[[k]], modelo = "AR(1)", coefs = sprintf("phi=%.3f", phi),
    invertivel = NA, loglik = round(r$loglik,2), aic = round(r$aic,1),
    bic = round(r$bic,1), var_e = round(r$var_e,3),
    rrse = round(mean((se[ix]-r$se_tr[ix])/se[ix])*100, 2), stringsAsFactors = FALSE)
  cat(sprintf("  AR(1)  phi=%.3f   logLik=%8.2f  BIC=%7.1f  Var(e~)=%.3f  RRSE=%6.2f%%\n",
              phi, r$loglik, r$bic, r$var_e,
              mean((se[ix]-r$se_tr[ix])/se[ix])*100))

  for (q in 1:4) {
    inv <- inverte_ma(fac, q)
    if (is.null(inv)) { cat("  MA(", q, ") inversao falhou\n"); next }
    r <- ajusta(y, se, "maq", 0, inv$theta, i0)
    if (is.null(r)) { cat("  MA(", q, ") estimacao falhou\n"); next }
    linhas[[length(linhas)+1]] <- data.frame(
      estrato = ROT[[k]], modelo = paste0("MA(", q, ")"),
      coefs = paste(sprintf("%.3f", inv$theta), collapse = " "),
      invertivel = inv$invertivel, loglik = round(r$loglik,2), aic = round(r$aic,1),
      bic = round(r$bic,1), var_e = round(r$var_e,3),
      rrse = round(mean((se[ix]-r$se_tr[ix])/se[ix])*100, 2), stringsAsFactors = FALSE)
    cat(sprintf("  MA(%d)  theta=%-28s logLik=%8.2f  BIC=%7.1f  Var(e~)=%.3f  RRSE=%6.2f%%  %s\n",
                q, paste(sprintf("%.3f", inv$theta), collapse=" "), r$loglik, r$bic,
                r$var_e, mean((se[ix]-r$se_tr[ix])/se[ix])*100,
                if (inv$invertivel) "" else "[NAO INVERTIVEL]"))
  }
}

tab <- do.call(rbind, linhas)
cat("\n\n################ RESUMO ################\n")
print(tab, row.names = FALSE)

cat("\n\n### Melhor modelo por estrato (menor BIC) ###\n")
for (e in unique(tab$estrato)) {
  s <- tab[tab$estrato == e, ]
  cat(sprintf("%-36s %s (BIC %.1f)\n", e, s$modelo[which.min(s$bic)], min(s$bic)))
}

write.csv(tab, file.path(SAIDA, "comparacao_maq_desocupados.csv"), row.names = FALSE)
cat("\nGravado em", SAIDA, "\n")
