################################################################################
## 46_benchmark_mg.R
##
## VALIDACAO EXTERNA: a soma das oito tendencias estimadas reproduz a tendencia
## do estado? (20/11/2026)
##
## Por que existe: na leitura como parecerista, a objecao mais forte ao artigo
## era que o ganho de precisao e medido contra um alvo que o proprio modelo
## define. A ausencia de benchmarking estava listada como limitacao.
##
## O TESTE E TENDENCIA CONTRA TENDENCIA, e nao tendencia contra estimativa
## direta. Comparar a soma das tendencias com a direta estadual seria injusto
## nos dois sentidos: a direta carrega sazonalidade e ruido amostral que a
## tendencia por definicao nao tem, e o intervalo da direta estadual e estreito
## demais (CV de 0,8% nos ocupados) para servir de regua. Entao estima-se um
## modelo PROPRIO para a serie de Minas Gerais, com a mesma receita dos
## estratos -- pseudo-erros, identificacao do ARMA, tendencia suave,
## sazonalidade deterministica, V(e~)=1 -- e comparam-se as duas tendencias.
##
## O teste faz sentido porque os oito estratos sao exaustivos: conferido aqui,
## a soma das estimativas diretas dos oito reproduz a direta de Minas Gerais.
## Logo as duas tendencias estimam o mesmo objeto por caminhos independentes.
##
## Atencao as unidades: series por estrato em MILHARES; a de MG, em pessoas.
##
## Saidas: artigo/resultados/tabbenchmark_mg.tex
##         outputs/benchmark_mg.csv
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))
source(file.path(RAIZ, "rotinas", "_id_funcoes.R"))

P <- 8; B_NIVEL <- 1:8
BURN <- 8
K <- 5; LAGS <- 12
MG <- "09 - Minas Gerais"
IND <- c(desocupados = "desocupada", ocupados = "ocupada", taxa = "txdesoc")

## ---------------------------------------------------------------------------
## 1. serie de Minas Gerais e FAC dos seus pseudo-erros
##    mesmo estimador do 17_fac_pseudo_erros.R (media unica, divisor T)
## ---------------------------------------------------------------------------
base  <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
alin  <- readRDS(file.path(RAIZ, "dadosalin_txdesoc_8reg.rds"))
stopifnot(MG %in% names(base), MG %in% names(alin))
mg <- base[[MG]]

serie_mg <- function(ind) switch(ind,
  desocupados = list(y = mg$Total.de.desocupados / 1000, se = mg$sd_d / 1000),
  ocupados    = list(y = mg$Total.de.ocupados / 1000,    se = mg$sd_o / 1000),
  taxa        = list(y = mg[["Taxa.de.desocupação"]] * 100, se = mg[["sd_txd"]] * 100))

autocov <- function(v, lag) {
  T <- length(v); m <- mean(v)
  sapply(0:lag, function(h) sum((v[1:(T-h)] - m) * (v[(1+h):T] - m)) / T)
}

fac_mg <- function(ind) {
  d <- alin[[MG]]
  g <- paste0(IND[[ind]], "_", 1:K)
  stopifnot(all(g %in% names(d)))
  G <- as.matrix(d[, g]); G[G == 0] <- NA
  Pe <- sweep(G, 1, rowMeans(G, na.rm = TRUE), "-")
  soma <- rowSums(sapply(1:K, function(k) autocov(Pe[, k], LAGS)))
  soma / soma[1]
}

## ---------------------------------------------------------------------------
## 2. identificacao do erro amostral de MG: mesma regra do artigo --
##    o mais parcimonioso entre os que passam no Ljung-Box a 5%, BIC no desempate
## ---------------------------------------------------------------------------
identifica <- function(ind) {
  s <- serie_mg(ind)
  rho <- fac_mg(ind)[-1]
  i0 <- log(pmax(c(var(diff(s$y)), var(diff(diff(s$y)))/4, 1e-6, 1e-6), 1e-8))
  cds <- candidatos(rho)
  res <- list()
  for (nm in names(cds)) {
    c0 <- cds[[nm]]
    r <- try(ajusta(s$y, s$se, c0$phi, c0$theta, i0, burn = BURN), silent = TRUE)
    if (inherits(r, "try-error") || is.null(r)) next
    res[[nm]] <- data.frame(form = nm, npar = r$npar, bic = r$bic,
                            ljung = r$ljung, stringsAsFactors = FALSE)
  }
  tab <- do.call(rbind, res)
  ok <- tab[!is.na(tab$ljung) & tab$ljung > 0.05, ]
  if (!nrow(ok)) ok <- tab
  ok <- ok[order(ok$npar, ok$bic), ]
  c(cds[[ ok$form[1] ]], list(form = ok$form[1], ljung = ok$ljung[1]))
}

## ---------------------------------------------------------------------------
## 3. modelo final de MG: tendencia suave + sazonalidade deterministica
##    (mesma convencao de otimizacao do 22_univariado_final.R)
## ---------------------------------------------------------------------------
estima_mg <- function(ind, phi, theta) {
  s <- serie_mg(ind)
  fn <- monta(s$se, phi, theta, smooth = TRUE, sazdet = TRUE)
  stopifnot(!is.null(fn))
  np <- length(attr(fn, "nomes"))
  obj <- function(pp) {
    ll <- try(dlmLL(s$y, fn(pp)), silent = TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
    ll
  }
  i0 <- log(pmax(c(var(diff(diff(s$y)))/4, 1e-6), 1e-8))
  inv1 <- i0; inv1[1] <- i0[1] + 3
  inv2 <- i0; inv2[1] <- i0[1] - 6
  LIM <- 20; melhor <- NULL
  for (p0 in list(i0, i0 + 1, i0 - 1, i0 - 3, inv1, inv2)) {
    p0 <- pmin(pmax(p0, -LIM + 1), LIM - 1)
    r <- try(optim(p0, obj, method = "L-BFGS-B", lower = rep(-LIM, np),
                   upper = rep(LIM, np), control = list(maxit = 300)), silent = TRUE)
    if (inherits(r, "try-error") || !is.finite(r$value) || r$value >= 1e9) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  stopifnot(!is.null(melhor))
  flt <- dlmFilter(s$y, fn(melhor$par))
  mse <- dlmSvd2var(flt$U.C, flt$D.C)[-1]
  list(tend = dropFirst(flt$m)[, 1],
       se   = sqrt(sapply(mse, function(x) x[1, 1])),
       y = s$y, se_dir = s$se)
}

## ---------------------------------------------------------------------------
## 4. soma das oito tendencias do modelo multivariado
## ---------------------------------------------------------------------------
agrega <- function(ind) {
  uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final",
                           paste0("modelos_", ind, ".rds")))
  Y <- sapply(names(uni), function(k) uni[[k]]$serie$y)
  m <- readRDS(file.path(RAIZ, "outputs", "multivariado_final",
                         paste0("multivariado_", ind, ".rds")))
  flt <- dlmFilter(Y, m$mod)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)[-1]
  TR  <- dropFirst(flt$m)[, B_NIVEL]
  cc <- rep(0, ncol(dropFirst(flt$m))); cc[B_NIVEL] <- 1
  list(soma_direta = rowSums(Y), tend = rowSums(TR),
       se = sqrt(sapply(mse, function(x) as.numeric(t(cc) %*% x %*% cc))))
}

cat("== exaustividade dos oito estratos ==\n")
for (ind in c("desocupados", "ocupados")) {
  col <- if (ind == "desocupados") "Total.de.desocupados" else "Total.de.ocupados"
  raz <- agrega(ind)$soma_direta / (mg[[col]] / 1000)
  cat(sprintf("  %-12s soma dos 8 / MG direto: min %.5f max %.5f\n",
              ind, min(raz), max(raz)))
  stopifnot(all(abs(raz - 1) < 1e-3))
}

cat("\n== FAC dos pseudo-erros de MG (lags 1 a 4) ==\n")
for (ind in names(IND))
  cat(sprintf("  %-12s %s\n", ind,
              paste(formatC(fac_mg(ind)[2:5], format = "f", digits = 3), collapse = "  ")))

ix <- (BURN + 1):52
linhas <- list()
agD <- agrega("desocupados"); agO <- agrega("ocupados")
for (ind in c("desocupados", "ocupados", "taxa")) {
  cat("\n== ", ind, " ==\n", sep = "")
  id <- identifica(ind)
  cat("  erro amostral de MG identificado: ", id$form,
      "  (Ljung-Box p = ", round(id$ljung, 3), ")\n", sep = "")
  e <- estima_mg(ind, id$phi, id$theta)
  ## A TAXA NAO SE SOMA. A agregacao dos estratos para a taxa e a razao das
  ## somas dos dois totais, com variancia pela mesma linearizacao de Taylor da
  ## Equacao 15 do artigo (Cov entre os dois totais tomada como nula).
  a <- if (ind != "taxa") agrega(ind) else {
    TT <- agD$tend + agO$tend
    list(tend = agD$tend / TT * 100,
         se = sqrt((agO$tend^2 * agD$se^2 + agD$tend^2 * agO$se^2) / TT^4) * 100)
  }

  ## as duas tendencias estimam o mesmo objeto; a diferenca e padronizada pelo
  ## erro-padrao da diferenca, tratando as duas como independentes (conservador:
  ## se houver correlacao positiva, a variancia da diferenca e menor)
  dif <- a$tend - e$tend
  sed <- sqrt(a$se^2 + e$se^2)
  linhas[[ind]] <- data.frame(
    ind = ind, form_mg = id$form,
    dif_rel = sum(dif[ix]) / sum(e$tend[ix]) * 100,
    max_z   = max(abs(dif[ix] / sed[ix])),
    dentro  = sum(abs(dif[ix]) <= qnorm(0.975) * sed[ix]), n = length(ix),
    cv_agreg = mean(a$se[ix] / a$tend[ix]) * 100,
    cv_mg    = mean(e$se[ix] / e$tend[ix]) * 100,
    stringsAsFactors = FALSE)
  print(linhas[[ind]], row.names = FALSE, digits = 4)
}

res <- do.call(rbind, linhas)
write.csv(res, file.path(RAIZ, "outputs", "benchmark_mg.csv"), row.names = FALSE)

## ---------------------------------------------------------------------------
## 5. tabela
## ---------------------------------------------------------------------------
ROT <- c(desocupados = "Total de desocupados", ocupados = "Total de ocupados",
         taxa = "Taxa de desocupa\u00e7\u00e3o")
f <- function(v, d = 2) formatC(v, format = "f", digits = d, decimal.mark = ",")

lin <- vapply(seq_len(nrow(res)), function(i) sprintf(
  "%s & %s & %s & %d de %d & %s & %s \\\\", ROT[[res$ind[i]]], res$form_mg[i],
  f(res$dif_rel[i]), res$dentro[i], res$n[i],
  f(res$cv_agreg[i]), f(res$cv_mg[i])), "")

tex <- c(
  "\\begin{table}[H]", "\\centering", "\\footnotesize",
  "\\captionsetup{justification=centering}",
  paste0("\\caption{Agrega\u00e7\u00e3o das oito tend\u00eancias comparada \u00e0 tend\u00eancia ",
         "estimada para Minas Gerais}"),
  "\\label{tab:apx_benchmark}",
  "\\adjustbox{max width=\\linewidth}{%", "\\renewcommand{\\arraystretch}{0.85}",
  "\\begin{tabular}{llcccc}", "\\toprule",
  paste0("\\textbf{Indicador} & \\textbf{\\makecell{Erro amostral \\\\ de MG}} & ",
         "\\textbf{\\makecell{Diferen\u00e7a \\\\ relativa (\\%)}} & ",
         "\\textbf{\\makecell{Compat\u00edveis \\\\ a 5\\%}} & ",
         "\\textbf{\\makecell{CV m\u00e9dio \\\\ agregado (\\%)}} & ",
         "\\textbf{\\makecell{CV m\u00e9dio \\\\ de MG (\\%)}} \\\\"),
  "\\midrule", lin, "\\bottomrule", "\\end{tabular}}",
  paste0("\\fonte{Elabora\u00e7\u00e3o pr\u00f3pria, com base nos dados da PNAD Cont\u00ednua. ",
         "Nota: compara\u00e7\u00e3o entre a soma das oito tend\u00eancias do modelo ",
         "multivariado e a tend\u00eancia de um modelo estimado diretamente para ",
         "Minas Gerais, com a mesma especifica\u00e7\u00e3o. A soma das oito estimativas ",
         "diretas reproduz a estimativa direta do estado, de modo que as duas ",
         "tend\u00eancias estimam o mesmo objeto. A quarta coluna conta os trimestres ",
         "em que a diferen\u00e7a entre as duas n\u00e3o \u00e9 significativa a 5\\%, tratando-as ",
         "como independentes. Descartadas as oito primeiras observa\u00e7\u00f5es.}"),
  "\\end{table}")

con <- file(file.path(ARTIGO, "resultados", "tabbenchmark_mg.tex"),
            open = "w", encoding = "UTF-8")
writeLines(tex, con); close(con)
cat("\ngravado: tabbenchmark_mg.tex\n")
