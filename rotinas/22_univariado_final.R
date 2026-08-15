################################################################################
## 22_univariado_final.R   — PASSO 3 do pipeline de revisão
##
## Estima os modelos estruturais univariados com a ESPECIFICAÇÃO FINAL do erro
## amostral (uma por estrato × indicador), sob Var(ẽ) = 1.
##
## Insumo : outputs/identificacao/especificacao_final.csv  (passo 2)
## Saída  : outputs/univariado_final/modelos_<indicador>.rds
##          — mesma estrutura que os passos 4 a 6 já consomem:
##            $serie$y, $serie$se, $phi, $theta, $rotulo,
##            $corrigido$hp (L, R, S, I, s2e), $trend, $se_trend, $sinal
##
## A convenção de otimização vem de rotinas/_id_funcoes.R e NÃO deve mudar:
## multi-start determinístico com caixa de busca limitada. Ver a ressalva sobre
## σ²_I em docs/ESTADO_REVISAO.md — o componente é fracamente identificado e a
## estimativa depende dessa convenção.
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
source(file.path(RAIZ, "rotinas", "_id_funcoes.R"))

BURN  <- 8
SAIDA <- file.path(RAIZ, "outputs", "univariado_final")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

espec <- read.csv(file.path(RAIZ, "outputs", "identificacao", "especificacao_final.csv"),
                  stringsAsFactors = FALSE, fileEncoding = "UTF-8")
base  <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
REG   <- names(base)[1:8]
COD   <- c("bh","ent","sul","trg","mat","nrt","val","cen")

le_coef <- function(s) if (is.na(s) || !nchar(s)) numeric(0) else as.numeric(strsplit(s, ";")[[1]])

serie <- function(ind, i) {
  d <- base[[ REG[i] ]]
  switch(ind,
    desocupados = list(y = d$Total.de.desocupados/1000, se = d$sd_d/1000),
    ocupados    = list(y = d$Total.de.ocupados/1000,    se = d$sd_o/1000),
    taxa        = list(y = d[["Taxa.de.desocupação"]]*100, se = d[["sd_txd"]]*100))
}

## teste H de heterocedasticidade (Durbin e Koopman): razão entre as somas de
## quadrados dos resíduos do último e do primeiro terço da série
teste_H <- function(r) {
  n <- length(r); h <- floor(n/3)
  H <- sum(r[(n-h+1):n]^2) / sum(r[1:h]^2)
  p <- 2 * min(pf(H, h, h), 1 - pf(H, h, h))
  c(H = H, p = p)
}

ajusta_final <- function(y, se, phi, theta) {
  fn <- monta(se, phi, theta)          # impõe Var(ẽ) = 1
  if (is.null(fn)) stop("montagem inválida")
  obj <- function(pp) {
    ll <- try(dlmLL(y, fn(pp)), silent = TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
    ll
  }
  i0 <- log(pmax(c(var(diff(y)), var(diff(diff(y)))/4, 1e-6, 1e-6), 1e-8))
  LIM <- 20
  melhor <- NULL
  for (p0 in list(i0, i0 + 1, i0 - 1, i0 - 3)) {
    p0 <- pmin(pmax(p0, -LIM + 1), LIM - 1)
    r <- try(optim(p0, obj, method = "L-BFGS-B", lower = rep(-LIM, 4),
                   upper = rep(LIM, 4), control = list(maxit = 300)), silent = TRUE)
    if (inherits(r, "try-error") || !is.finite(r$value) || r$value >= 1e9) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  stopifnot(!is.null(melhor))

  mod <- fn(melhor$par)
  flt <- dlmFilter(y, mod)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)
  est <- dropFirst(flt$m)
  ns  <- nrow(mod$GG)

  c_sin <- matrix(0, 1, ns); c_sin[1, c(1,3,5)] <- 1
  se_tr  <- dropFirst(sapply(mse, function(x) sqrt(x[1,1])))
  se_sin <- dropFirst(sapply(mse, function(x) sqrt(c_sin %*% x %*% t(c_sin))))
  trend  <- est[,1]
  sinal  <- est[,1] + est[,3] + est[,5]
  ## irregular POR DIFERENÇA, como decidido pelos autores
  ea     <- est[,6] * se
  irreg  <- y - sinal - ea

  rp <- as.numeric(residuals(flt, type = "standardized", sd = FALSE))[-(1:BURN)]
  b  <- blocos(phi, theta)
  s2e <- 1 / P_est(b$G, b$v, 1)[1,1]
  hp <- c(exp(melhor$par), s2e)        # L, R, S, I, s2e

  list(convergencia = melhor$convergence, loglik = -melhor$value, hp = hp,
       trend = trend, sinal = sinal, se_trend = se_tr, se_sinal = se_sin,
       irregular = irreg, erro_amostral = ea,
       shapiro = shapiro.test(rp)$p.value,
       ljung   = Box.test(rp, lag = 8, type = "Ljung-Box")$p.value,
       H       = teste_H(rp))
}

################################################################################
resumo <- list()
for (ind in c("desocupados", "ocupados", "taxa")) {
  cat("\n####################", toupper(ind), "####################\n")
  out <- list()
  for (i in 1:8) {
    e <- espec[espec$indicador == ind & espec$ordem == i, ]
    stopifnot(nrow(e) == 1)
    s   <- serie(ind, i)
    phi <- le_coef(e$phi); theta <- le_coef(e$theta)
    r   <- ajusta_final(s$y, s$se, phi, theta)

    ix   <- (BURN+1):length(s$y)
    rrse <- mean((s$se[ix] - r$se_trend[ix]) / s$se[ix]) * 100
    vic  <- sum(r$sinal[ix] - s$y[ix]) / sum(s$y[ix]) * 100

    out[[ COD[i] ]] <- list(rotulo = REG[i], processo = e$processo,
                            phi = phi, theta = theta, serie = s,
                            corrigido = r, rrse = rrse, vicio = vic)
    cat(sprintf("%-40s %-14s conv=%d logLik=%9.2f RRSE=%6.2f%% vicio=%6.2f%% SW=%.3f LB=%.3f H=%.3f\n",
                substr(REG[i],1,38), e$processo, r$convergencia, r$loglik,
                rrse, vic, r$shapiro, r$ljung, r$H["p"]))
    resumo[[length(resumo)+1]] <- data.frame(
      indicador = ind, estrato = REG[i], processo = e$processo,
      sigma2_L = r$hp[1], sigma2_R = r$hp[2], sigma2_S = r$hp[3],
      sigma2_I = r$hp[4], sigma2_e = r$hp[5],
      loglik = r$loglik, rrse = rrse, vicio = vic,
      shapiro = r$shapiro, ljung = r$ljung, H_p = unname(r$H["p"]),
      dp_irregular = sd(r$irregular[ix]), stringsAsFactors = FALSE)
  }
  saveRDS(out, file.path(SAIDA, paste0("modelos_", ind, ".rds")))
}

tab <- do.call(rbind, resumo)
write.csv(tab, file.path(SAIDA, "resumo_univariado_final.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n\n############ SÍNTESE ############\n")
cat("ganho médio por indicador:\n"); print(round(tapply(tab$rrse, tab$indicador, mean), 2))
cat("\nganho médio geral:", round(mean(tab$rrse), 2), "%\n")
cat("vício absoluto médio:", round(mean(abs(tab$vicio)), 2), "%\n")
cat("\ndiagnósticos que rejeitam a 5%:\n")
cat("  Shapiro-Wilk (normalidade):", sum(tab$shapiro <= 0.05), "de", nrow(tab), "\n")
cat("  Ljung-Box (autocorrelação):", sum(tab$ljung   <= 0.05), "de", nrow(tab), "\n")
cat("  teste H (heterocedasticidade):", sum(tab$H_p  <= 0.05), "de", nrow(tab), "\n")
cat("\nsigma2_I: casos > 0,01:", sum(tab$sigma2_I > 0.01), "de", nrow(tab), "\n")
cat("desvio-padrão do irregular por diferença: média", round(mean(tab$dp_irregular), 4), "\n")
cat("\nGravado em", SAIDA, "\n")
