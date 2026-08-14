################################################################################
## 14_figuras.R
##
## Regera as figuras do artigo a partir dos modelos corrigidos:
##   - painel esquerdo: estimativa direta (preta), IC 95% (tracejado),
##     tendencia univariada (azul) e multivariada (vermelha)
##   - painel direito: coeficientes de variacao das tres series
##   - 4 regioes por figura, 2 figuras por indicador
##
## As series de tendencia do multivariado nao foram gravadas pelo 11_; sao
## recalculadas aqui com um unico passe do filtro sobre `mod` (sem reotimizar).
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

SAIDA <- file.path(RAIZ, "outputs", "figuras")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

## primeiro trimestre exibido, por indicador (descarte de inicializacao)
INICIO <- list(desocupados = c(2014, 1), ocupados = c(2013, 4), taxa = c(2014, 1))
ROTULO_Y <- list(desocupados = "Desocupados (mil pessoas)",
                 ocupados    = "Ocupados (mil pessoas)",
                 taxa        = "Taxa de desocupação (%)")

B_NIVEL <- 1:8

################################################################################

carrega <- function(ind) {
  uni <- readRDS(file.path(RAIZ, "outputs", "univariado_corrigido",
                           paste0("modelos_", ind, ".rds")))
  cod <- names(uni)
  Y  <- sapply(cod, function(k) uni[[k]]$serie$y)
  SE <- sapply(cod, function(k) uni[[k]]$serie$se)

  tr_uni <- sapply(cod, function(k) uni[[k]]$corrigido$trend)
  se_uni <- sapply(cod, function(k) uni[[k]]$corrigido$se_trend)
  rot    <- sapply(cod, function(k) uni[[k]]$rotulo)

  arq_m <- file.path(RAIZ, "outputs", "multivariado_cholesky",
                     paste0("multivariado_", ind, ".rds"))
  if (file.exists(arq_m)) {
    m   <- readRDS(arq_m)
    flt <- dlmFilter(Y, m$mod)
    mse <- dlmSvd2var(flt$U.C, flt$D.C)
    est <- dropFirst(flt$m)
    tr_mult <- est[, B_NIVEL]
    se_mult <- sapply(B_NIVEL, function(i)
      dropFirst(sapply(mse, function(x) sqrt(x[i, i]))))
  } else {
    tr_mult <- se_mult <- NULL
  }
  ## Na taxa, a serie de comparacao no lugar do univariado e o CALCULO INDIRETO
  ## a partir das tendencias de desocupados e ocupados (ver rotinas/13_).
  leg <- c("Tendência - Mod. univariado", "Tendência - Mod. multivariado")
  if (ind == "taxa") {
    arq_i <- file.path(RAIZ, "outputs", "taxa_indireta", "taxa_indireta.rds")
    if (file.exists(arq_i)) {
      ti <- readRDS(arq_i)
      tr_uni <- ti$taxa * 100     # a base guarda a taxa em proporcao
      se_uni <- ti$se   * 100
      leg    <- c("Taxa calculada indiretamente", "Tendência - Mod. multivariado")
    }
  }

  list(Y = Y, SE = SE, tr_uni = tr_uni, se_uni = se_uni,
       tr_mult = tr_mult, se_mult = se_mult, rot = rot, leg = leg)
}

## desenha um par de paineis (nivel + CV) para uma regiao
painel <- function(d, i, ini, rot_y, titulo = NULL) {
  n   <- nrow(d$Y)
  ts_ <- function(v) window(ts(v, start = c(2012, 1), frequency = 4), start = ini)

  y   <- ts_(d$Y[, i]);  se <- ts_(d$SE[, i])
  tu  <- ts_(d$tr_uni[, i]); su <- ts_(d$se_uni[, i])
  tem_mult <- !is.null(d$tr_mult)
  if (tem_mult) { tm <- ts_(d$tr_mult[, i]); sm <- ts_(d$se_mult[, i]) }

  li <- y - 1.96 * se; ls <- y + 1.96 * se

  ## --- painel de nivel ---
  ylim <- range(c(li, ls, tu, if (tem_mult) tm), na.rm = TRUE)
  plot(y, type = "l", lwd = 2, col = "black", ylim = ylim,
       xlab = "Ano", ylab = rot_y, col.lab = "blue4", cex.lab = 0.9)
  if (!is.null(titulo)) mtext(titulo, side = 3, line = 0.9, cex = 0.82,
                              font = 2, adj = 1.18)
  lines(li, lty = 2); lines(ls, lty = 2)
  lines(tu, col = "blue", lwd = 2)
  if (tem_mult) lines(tm, col = "red", lwd = 2)
  rot_leg <- c("Estimativa direta", d$leg[1],
               if (tem_mult) d$leg[2], "IC 95% - estimativa direta")
  legend("topleft", bty = "n", cex = 0.62,
         lwd = c(2, 2, if (tem_mult) 2, 1),
         col = c("black", "blue", if (tem_mult) "red", "black"),
         lty = c(1, 1, if (tem_mult) 1, 2), legend = rot_leg)

  ## --- painel de CV ---
  cv  <- 100 * se / y
  cvu <- 100 * su / tu
  if (tem_mult) cvm <- 100 * sm / tm
  ylim2 <- range(c(cv, cvu, if (tem_mult) cvm), na.rm = TRUE)
  plot(cv, type = "l", lwd = 2, col = "black", ylim = ylim2,
       xlab = "Ano", ylab = "CV (%)", col.lab = "blue4", cex.lab = 0.9)
  lines(cvu, col = "blue", lwd = 2)
  if (tem_mult) lines(cvm, col = "red", lwd = 2)
  legend("topleft", bty = "n", cex = 0.62, lwd = 2,
         col = c("black", "blue", if (tem_mult) "red"),
         legend = c("CV Estimativa direta", paste("CV", d$leg[1]),
                    if (tem_mult) paste("CV", d$leg[2])))
}

figura <- function(d, regs, arquivo, ini, rot_y) {
  png(arquivo, width = 1000, height = 1480, res = 100)
  op <- par(mfrow = c(4, 2), mar = c(4.2, 4.4, 2.6, 1.2), oma = c(0, 0, 0, 0))
  for (i in regs) {
    ## o titulo e escrito a partir do painel esquerdo, deslocado para a direita
    ## de modo a cair aproximadamente no centro do par (adj > 1)
    painel(d, i, ini, rot_y, titulo = d$rot[i])
  }
  par(op); dev.off()
  cat("gravado:", basename(arquivo), "\n")
}

################################################################################

indicadores <- strsplit(Sys.getenv("INDICADORES", "desocupados,ocupados,taxa"), ",")[[1]]

for (ind in indicadores) {
  arq_u <- file.path(RAIZ, "outputs", "univariado_corrigido",
                     paste0("modelos_", ind, ".rds"))
  if (!file.exists(arq_u)) { cat("pulando", ind, "- univariado ausente\n"); next }
  d <- carrega(ind)
  if (is.null(d$tr_mult)) cat("AVISO:", ind, "- multivariado ausente; figura so com univariado\n")
  nome <- c(desocupados = "Desocupacao", ocupados = "Ocupacao", taxa = "TaxaDesoc")[ind]
  figura(d, 1:4, file.path(SAIDA, paste0("Figura_", nome, "_1.png")),
         INICIO[[ind]], ROTULO_Y[[ind]])
  figura(d, 5:8, file.path(SAIDA, paste0("Figura_", nome, "_2.png")),
         INICIO[[ind]], ROTULO_Y[[ind]])
}

cat("\nFiguras em", SAIDA, "\n")
