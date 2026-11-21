################################################################################
## 27_validacao_escala.R   — PASSO 7 do pipeline de revisão
##
## A condição V(ẽ_t) = 1 é imposta: a variância do erro amostral vem do desenho,
## pelo c_t, e o estado padronizado não pode reescalá-la. Este passo verifica se
## o DADO concorda com a imposição — a mesma checagem que van den Brakel e Krieg
## (2015) fazem e reportam como "as expected, around 1".
##
## Para cada estrato, reestima-se a especificação final com a escala livre,
## V(ẽ_t) = lambda, e compara-se com lambda = 1 por razão de verossimilhança.
## lambda = 1 é ponto interior do espaço paramétrico, então vale o qui-quadrado
## com um grau de liberdade (crítico 3,84 a 5%) -- ao contrário de sigma2_S = 0,
## que fica na fronteira.
##
## Insumo : outputs/univariado_final/modelos_<indicador>.rds
## Saída  : outputs/validacao_escala/ e escala_<suf>.tex no diretório do artigo
################################################################################
suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
source(file.path(RAIZ, "rotinas", "_id_funcoes.R"))

ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))
SAIDA  <- file.path(RAIZ, "outputs", "validacao_escala")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)
LIM <- 20

PASTA <- c(desocupados = "Total de desocupados", ocupados = "Total de ocupados",
           taxa = "Taxa de desocupação")
SUFIXO  <- c(desocupados = "desoc", ocupados = "ocup", taxa = "taxa")
ROT_IND <- c(desocupados = "total de desocupados", ocupados = "total de ocupados",
             taxa = "taxa de desocupação")
ART <- c(desocupados = "o", ocupados = "o", taxa = "a")

fmt <- function(x, d = 3) gsub(".", ",", formatC(x, format = "f", digits = d), fixed = TRUE)

## especificação final com a escala do erro amostral livre: o último parâmetro
## multiplica o bloco ARMA em W e na condição inicial, de modo que a variância
## estacionária de ẽ passa a ser lambda em vez de 1
com_escala <- function(se, phi, theta, smooth) {
  fn0 <- monta(se, phi, theta, smooth = smooth, sazdet = TRUE)
  if (is.null(fn0)) return(NULL)
  np0 <- length(attr(fn0, "nomes"))
  ns  <- nrow(fn0(rep(0, np0))$GG)
  fn <- function(pp) {
    m <- fn0(pp[seq_len(np0)]); lam <- exp(pp[np0 + 1]); ix <- 6:ns
    m$W[ix, ix]  <- lam * m$W[ix, ix]
    m$C0[ix, ix] <- lam * m$C0[ix, ix]
    m
  }
  attr(fn, "np0") <- np0
  fn
}

otimiza <- function(y, fn, np, i0) {
  obj <- function(pp) {
    ll <- try(dlmLL(y, fn(pp)), silent = TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
    ll
  }
  melhor <- NULL
  for (d in list(0, 1, -1, -3, 3)) {
    p0 <- pmin(pmax(i0 + d, -LIM + 1), LIM - 1)
    r <- try(optim(p0, obj, method = "L-BFGS-B", lower = rep(-LIM, np),
                   upper = rep(LIM, np), control = list(maxit = 300)), silent = TRUE)
    if (inherits(r, "try-error") || !is.finite(r$value) || r$value >= 1e9) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  melhor
}

res <- list()
for (ind in names(PASTA)) {
  uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final",
                           paste0("modelos_", ind, ".rds")))
  cat("\n####", toupper(ind), "####\n")
  linhas <- character(0)
  for (k in names(uni)) {
    u  <- uni[[k]]; y <- u$serie$y; se <- u$serie$se; sm <- isTRUE(u$smooth)
    fn <- com_escala(se, u$phi, u$theta, sm); np0 <- attr(fn, "np0")
    i0 <- c(log(pmax(u$corrigido$hp[c(if (!sm) 1, 2, 4)], 1e-8)), 0)
    r  <- otimiza(y, fn, np0 + 1, i0)
    stopifnot(!is.null(r))
    lam <- exp(r$par[np0 + 1])
    LR  <- 2 * ((-r$value) - u$corrigido$loglik)
    pv  <- pchisq(max(LR, 0), df = 1, lower.tail = FALSE)
    cat(sprintf("  %-4s %-8s %-12s lambda = %6.3f   RV = %6.2f   p = %.3f %s\n",
                k, u$processo, if (sm) "suave" else "local linear", lam, LR, pv,
                if (pv < 0.05) "  <-- rejeita" else ""))
    res[[length(res)+1]] <- data.frame(indicador = ind, estrato = u$rotulo,
      processo = u$processo, tendencia = if (sm) "suave" else "local linear",
      lambda = lam, loglik_restrito = u$corrigido$loglik, loglik_livre = -r$value,
      RV = LR, p = pv, stringsAsFactors = FALSE)
    linhas <- c(linhas, sprintf(r"(%s & %s & %s & %s & %s \\)",
                                u$rotulo, u$processo, fmt(lam), fmt(LR, 2), fmt(pv, 3)))
  }
  tex <- c(r"(\begin{table}[H])", r"(\centering)",
    r"(\captionsetup{justification=centering})",
    sprintf(paste0(r"(\caption{Verificação da condição \(V(\tilde{e}_t)=1\) para %s %s )",
                   r"(- escala do erro amostral estimada livremente})"), ART[ind], ROT_IND[ind]),
    sprintf(r"(\label{tab:escala%s})", SUFIXO[ind]),
    "{%", r"(\renewcommand{\arraystretch}{0.85})", r"(\scalebox{0.95}{%)",
    r"(\begin{tabular}{llccc})", r"(\toprule)",
    paste0(r"(\textbf{Estrato geográfico} & \textbf{Processo} & )",
           r"(\(\hat{\lambda}\) & \textbf{RV} & \textbf{valor-p} \\)"),
    r"(\midrule)", linhas, r"(\bottomrule)", r"(\end{tabular}%)", "}", "}",
    paste0(r"(\fonte{Elaboração própria, com base nos dados da PNAD Contínua. )",
           r"(Nota: \(\hat{\lambda}\) é a variância estacionária do erro amostral )",
           r"(padronizado quando estimada livremente, em vez de imposta igual a um. )",
           r"(RV é a estatística de razão de verossimilhança da hipótese \(\lambda = 1\), )",
           r"(com distribuição \(\chi^2_1\) -- o valor crítico a 5\% é 3,84. Valores )",
           r"(próximos de um indicam que o erro-padrão do desenho descreve )",
           r"(adequadamente a variância amostral da série.})"),
    r"(\end{table})")
  d <- file.path(ARTIGO, "resultados", PASTA[ind])
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  con <- file(file.path(d, paste0("escala_", SUFIXO[ind], ".tex")), open = "w", encoding = "UTF-8")
  writeLines(tex, con); close(con)
  cat("  gravado:", file.path(PASTA[ind], paste0("escala_", SUFIXO[ind], ".tex")), "\n")
}
t <- do.call(rbind, res)
write.csv(t, file.path(SAIDA, "validacao_escala.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\n############ SÍNTESE ############\n")
print(aggregate(cbind(lambda, RV) ~ indicador, data = t, FUN = mean))
cat("\nrejeita lambda = 1 (5%) em", sum(t$p < 0.05), "de", nrow(t), "estratos\n")
cat("lambda: mediana", round(median(t$lambda), 3),
    "| faixa", round(min(t$lambda), 3), "a", round(max(t$lambda), 3), "\n")
