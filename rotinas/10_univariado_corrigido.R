################################################################################
## 10_univariado_corrigido.R
##
## Reestima os modelos estruturais univariados com erro amostral para os 8
## estratos geográficos, comparando a especificação LEGADO (a que gerou os
## resultados do artigo) com a CORRIGIDA.
##
## Correções aplicadas na versão CORRIGIDA:
##   - issue #20: o termo MA do erro amostral estava inerte (W[7,7] = 0 com a
##     linha 7 de GG nula zera o estado auxiliar). Passa a usar a forma de
##     Harvey, com a mesma inovação nos dois estados:
##         e_t = xi_t + theta*xi_{t-1}   (MA(1))
##         e_t = phi*e_{t-1} + xi_t + theta*xi_{t-1}   (ARMA(1,1))
##   - issue #1: BH-desocupados usava phi1_ar1 no lugar de theta1_ma1. O efeito
##     estava mascarado pela #20 (o coeficiente não era usado); com a #20
##     corrigida, passa a importar.
##   - issue #2: a seleção passa a exigir convergência limpa, com multi-start.
##
## Dados: baseestr8reg.rds e pseudoerros_8reg/ DA RAIZ do repo (vintage do
## artigo). A cópia em data/ é o vintage revisado pós-Censo 2022 (~0,8% menor)
## e NÃO reproduz o artigo — ver data/COMO_FORAM_GERADOS.md.
################################################################################

suppressMessages(library(dlm))

## raiz do repo: variável de ambiente, ou o diretório de trabalho, ou o pai dele
RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) {
  RAIZ <- dirname(RAIZ)
}
stopifnot(dir.exists(file.path(RAIZ, "pseudoerros_8reg")))

INDICADOR <- Sys.getenv("INDICADOR", "desocupados")   # "desocupados" | "ocupados"
BURN      <- 8                                        # descarte de inicialização
SAIDA     <- file.path(RAIZ, "outputs", "univariado_corrigido")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

################################################################################
## Configuração por estrato
################################################################################

# processo ARMA do erro amostral por estrato (Tabela do artigo)
PROC <- list(
  desocupados = c(bh="ma1", ent="ma1", sul="arma11", trg="ma1",
                  mat="ma1", nrt="ma1", val="ar1", cen="ma1"),
  ocupados    = c(bh="ar1", ent="ar1", sul="ar1", trg="ar1",
                  mat="ar1", nrt="ar1", val="ar1", cen="ar1")
)[[INDICADOR]]

ARQ_PE <- c(bh="01_params_bh.rds",  ent="02_params_ent.rds", sul="03_params_sul.rds",
            trg="04_params_trg.rds", mat="05_params_mat.rds", nrt="06_params_nrt.rds",
            val="07_params_rio.rds", cen="08_params_cen.rds")

# sufixo usado nos nomes de coluna dos pseudo-erros
SUF <- c(bh="bh", ent="ent", sul="sul", trg="trg",
         mat="mat", nrt="nrt", val="rio", cen="cen")

ROTULO <- c(bh="01 - Belo Horizonte", ent="02 - Colar e Entorno Metropolitano de BH",
            sul="03 - Sul de Minas", trg="04 - Triângulo Mineiro",
            mat="05 - Zona da Mata", nrt="06 - Norte de Minas",
            val="07 - Vale do Rio Doce", cen="08 - Central")

# valores iniciais = estimativas univariadas publicadas (Tabelas hiperdesoc/hiperocup)
INI <- list(
  desocupados = rbind(
    bh  = c(21.9403,  20.3976, 0.0000, 0.0000, 0.5430),
    ent = c( 0.0000, 120.4332, 2.8673, 0.0044, 0.4923),
    sul = c(142.8495,  0.9414, 0.0000, 0.0000, 0.3764),
    trg = c(798.9974,  0.6172, 0.0000, 0.0000, 0.3357),
    mat = c(100.9284,  0.1223, 0.0000, 0.0028, 0.3478),
    nrt = c(194.5015,  1.1215, 0.0000, 0.0037, 0.3780),
    val = c(198.9328,  0.7609, 0.0000, 0.0002, 0.1260),
    cen = c(159.6191,  0.0021, 0.0000, 0.0000, 0.2429)),
  ocupados = rbind(
    bh  = c( 132.4109, 2.3101, 3.7110,  0.0005, 0.9957),
    ent = c( 433.3725, 8.8030, 0.0000,  0.0015, 0.8156),
    sul = c( 191.0349, 5.0668, 0.0000, 82.4760, 0.1243),
    trg = c( 757.2480, 0.0001, 0.0000,  0.0000, 0.2231),
    mat = c( 624.4167, 0.0000, 0.0001,  0.0000, 0.1917),
    nrt = c( 891.6365, 2.1295, 0.0000,  0.0000, 0.4001),
    val = c( 163.8916, 0.0000, 0.0000, 72.4594, 0.1619),
    cen = c(1961.4271, 0.0000, 0.0000,  0.0000, 0.0312))
)[[INDICADOR]]

################################################################################
## Leitura dos dados
################################################################################

base <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
nomes_reg <- names(base)[1:8]   # 8 estratos, na ordem

pega_serie <- function(k) {
  d <- base[[ nomes_reg[k] ]]
  if (INDICADOR == "desocupados")
    list(y = d$Total.de.desocupados/1000, se = d$sd_d/1000)
  else
    list(y = d$Total.de.ocupados/1000,    se = d$sd_o/1000)
}

pega_arma <- function(cod) {
  pe  <- readRDS(file.path(RAIZ, "pseudoerros_8reg", ARQ_PE[[cod]]))
  s   <- SUF[[cod]]
  ind <- if (INDICADOR == "desocupados") "d" else "o"
  switch(PROC[[cod]],
    ar1    = list(phi = pe$mod_ar1[[paste0("phi1_ar1_", ind, s)]],       theta = 0),
    ma1    = list(phi = 0,
                  theta = pe$mod_ma1[[paste0("theta1_ma1_", ind, s)]]),
    arma11 = list(phi = pe$mod_arma11[[paste0("phi1_arma11_", ind, s)]],
                  theta = pe$mod_arma11[[paste0("theta1_arma11_", ind, s)]])
  )
}

################################################################################
## Construtor do modelo
################################################################################

monta <- function(se, proc, phi, theta, corrigido) {
  function(params) {
    m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se, addInt = FALSE)
    s2e <- exp(params[5])

    if (proc == "ar1") {
      m$GG[6, 6] <- phi
      W <- matrix(0, 6, 6)
      W[6, 6] <- s2e
    } else {
      m$FF <- cbind(m$FF, rep(0, 1))
      m$GG <- rbind(m$GG, rep(0, 6))
      m$GG <- cbind(m$GG, rep(0, 7))
      m$GG[6, 6] <- phi
      m$GG[6, 7] <- theta
      m$GG[7, 6] <- 0
      m$GG[7, 7] <- 0
      W <- matrix(0, 7, 7)
      if (corrigido) {
        # forma de Harvey: w6 = w7 = xi_t  ->  o termo MA passa a existir
        W[6, 6] <- s2e; W[7, 7] <- s2e; W[6, 7] <- s2e; W[7, 6] <- s2e
      } else {
        W[6, 6] <- s2e; W[7, 7] <- 0      # LEGADO: termo MA inerte
      }
      m$m0 <- rep(0, 7)
      m$C0 <- diag(x = 10^7, 7)
    }

    W[1, 1] <- exp(params[1])
    W[2, 2] <- exp(params[2])
    W[3, 3] <- exp(params[3])
    m$W <- W
    m$V <- exp(params[4])
    m
  }
}

## variância implícita do erro amostral padronizado (issue #17).
## Se o processo estiver bem normalizado, Var(e~) deve ser ~1: o modelo estaria
## respeitando a variância do desenho. Valores << 1 indicam que o modelo está
## deflacionando a variância amostral publicada.
var_implicita <- function(proc, phi, theta, s2e) {
  switch(proc,
    ar1    = s2e / (1 - phi^2),
    ma1    = s2e * (1 + theta^2),
    arma11 = s2e * (1 + 2*phi*theta + theta^2) / (1 - phi^2)
  )
}

################################################################################
## Ajuste com multi-start
################################################################################

flr <- function(v) log(pmax(v, 1e-6))

ajusta <- function(y, se, proc, phi, theta, corrigido, i0) {
  fn <- monta(se, proc, phi, theta, corrigido)

  partidas <- list(flr(i0))
  for (f in c(0.1, 10)) partidas <- c(partidas, list(flr(i0 * f)))
  partidas <- c(partidas, list(rep(0, 5)), list(flr(c(1, 1, 1e-6, 1e-6, 0.5))))

  melhor <- NULL
  for (p0 in partidas) {
    r <- try(dlmMLE(y, p0, fn, control = list(maxit = 1e5)), silent = TRUE)
    if (inherits(r, "try-error") || !is.finite(r$value)) next
    ok <- isTRUE(r$convergence == 0)
    cand <- list(fit = r, ok = ok, ll = -r$value)
    if (is.null(melhor)) { melhor <- cand; next }
    # prioriza convergência limpa; entre iguais, maior verossimilhança
    if ((cand$ok && !melhor$ok) || (cand$ok == melhor$ok && cand$ll > melhor$ll))
      melhor <- cand
  }
  if (is.null(melhor)) return(NULL)

  fit <- melhor$fit
  mod <- fn(fit$par)
  flt <- dlmFilter(y, mod)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)

  ns <- nrow(mod$GG)
  c_sinal <- matrix(0, 1, ns); c_sinal[1, c(1, 3, 5)] <- 1

  se_trend  <- dropFirst(sapply(mse, function(x) sqrt(x[1, 1])))
  se_sinal  <- dropFirst(sapply(mse, function(x) sqrt(c_sinal %*% x %*% t(c_sinal))))
  est       <- dropFirst(flt$m)
  trend     <- est[, 1]
  sinal     <- est[, 1] + est[, 3] + est[, 5]

  hp <- exp(fit$par)
  list(convergencia = fit$convergence, conv_ok = melhor$ok, loglik = melhor$ll,
       hp = hp, var_e = var_implicita(proc, phi, theta, hp[5]),
       trend = trend, sinal = sinal, se_trend = se_trend, se_sinal = se_sinal)
}

################################################################################
## Execução
################################################################################

codigos <- names(PROC)
res <- list()

for (cod in codigos) {
  s  <- pega_serie(match(cod, codigos))
  ar <- pega_arma(cod)
  i0 <- INI[cod, ]

  cat("\n==== ", ROTULO[[cod]], " | ", PROC[[cod]],
      " | phi=", round(ar$phi, 4), " theta=", round(ar$theta, 4), " ====\n", sep = "")

  leg <- ajusta(s$y, s$se, PROC[[cod]], ar$phi, ar$theta, corrigido = FALSE, i0)
  cor <- ajusta(s$y, s$se, PROC[[cod]], ar$phi, ar$theta, corrigido = TRUE,  i0)

  ix <- (BURN + 1):length(s$y)
  rrse <- function(r) if (is.null(r)) NA else mean((s$se[ix] - r$se_trend[ix]) / s$se[ix]) * 100
  vic  <- function(r) if (is.null(r)) NA else sum(r$sinal[ix] - s$y[ix]) / sum(s$y[ix]) * 100

  cat("  LEGADO   : conv=", leg$convergencia, " logLik=", round(leg$loglik, 2),
      " RRSE=", round(rrse(leg), 2), "%  Var(e~)=", round(leg$var_e, 3), "\n", sep = "")
  cat("  CORRIGIDO: conv=", cor$convergencia, " logLik=", round(cor$loglik, 2),
      " RRSE=", round(rrse(cor), 2), "%  Var(e~)=", round(cor$var_e, 3), "\n", sep = "")

  res[[cod]] <- list(rotulo = ROTULO[[cod]], proc = PROC[[cod]],
                     phi = ar$phi, theta = ar$theta,
                     serie = s, legado = leg, corrigido = cor,
                     rrse_legado = rrse(leg), rrse_corrigido = rrse(cor),
                     vicio_legado = vic(leg), vicio_corrigido = vic(cor))
}

## ---- tabela-resumo ----------------------------------------------------------
tab <- do.call(rbind, lapply(codigos, function(cod) {
  r <- res[[cod]]
  data.frame(
    estrato        = r$rotulo,
    processo       = r$proc,
    conv_legado    = r$legado$convergencia,
    conv_corrigido = r$corrigido$convergencia,
    loglik_legado    = round(r$legado$loglik, 2),
    loglik_corrigido = round(r$corrigido$loglik, 2),
    rrse_legado    = round(r$rrse_legado, 2),
    rrse_corrigido = round(r$rrse_corrigido, 2),
    vicio_legado    = round(r$vicio_legado, 2),
    vicio_corrigido = round(r$vicio_corrigido, 2),
    var_e_legado    = round(r$legado$var_e, 3),
    var_e_corrigido = round(r$corrigido$var_e, 3),
    stringsAsFactors = FALSE)
}))

cat("\n\n################ RESUMO —", INDICADOR, "################\n")
print(tab, row.names = FALSE)

write.csv(tab, file.path(SAIDA, paste0("resumo_", INDICADOR, ".csv")), row.names = FALSE)
saveRDS(res, file.path(SAIDA, paste0("modelos_", INDICADOR, ".rds")))
cat("\nGravado em", SAIDA, "\n")
