################################################################################
## 16_erro_amostral_restrito.R
##
## Reestima todas as formulacoes IMPONDO Var(e~) = 1 (issue #17).
##
## MOTIVACAO. O modelo adota a especificacao de Binder e Dick (1989), em que o
## erro amostral e proporcional ao erro-padrao do desenho:
##       e_t = c_t * e~_t ,   c_t = sqrt(var(y_t))
## Se e~_t e o erro amostral PADRONIZADO, entao Var(e~) = 1 por construcao e
## Var(e_t) = c_t^2, isto e, o modelo respeita a variancia do desenho amostral.
##
## Na especificacao livre, sigma2_xi e estimado sem essa restricao, e a Var(e~)
## implicita cai para 0,08-0,86. A comparacao do 15_ mostrou que o "ganho de
## precisao" e quase inteiramente explicado por esse desvio: correlacao -0,961
## e R2 = 0,924 entre RRSE e Var(e~).
##
## Aqui a variancia da inovacao deixa de ser livre e passa a ser DERIVADA:
##   P = G P G' + W  com W = vv'  ->  sigma2_xi = 1 / P[1,1]
## garantindo Var(e~) = 1 exatamente. O modelo perde um parametro (4 em vez de
## 5) e o que sobrar de reducao de erro-padrao e filtragem, nao reatribuicao de
## variancia amostral para o sinal.
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

BURN  <- 8
SAIDA <- file.path(RAIZ, "outputs", "erro_amostral_restrito")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

COD <- c("bh","ent","sul","trg","mat","nrt","val","cen")
ROT <- c("01 - Belo Horizonte","02 - Colar e Entorno","03 - Sul de Minas",
         "04 - Triângulo Mineiro","05 - Zona da Mata","06 - Norte de Minas",
         "07 - Vale do Rio Doce","08 - Central"); names(ROT) <- COD
SUF <- c(bh="bh",ent="ent",sul="sul",trg="trg",mat="mat",nrt="nrt",val="rio",cen="cen")

base <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
nomes_reg <- names(base)[1:8]

pega <- function(ind, i) {
  d <- base[[ nomes_reg[i] ]]
  switch(ind,
    desocupados = list(y = d$Total.de.desocupados/1000, se = d$sd_d/1000),
    ocupados    = list(y = d$Total.de.ocupados/1000,    se = d$sd_o/1000),
    taxa        = list(y = d[["Taxa.de.desocupação"]]*100, se = d[["sd_txd"]]*100))
}

## ---- matrizes do bloco ARMA -------------------------------------------------
blocos <- function(phi, theta) {
  p <- length(phi); q <- length(theta); r <- max(p, q + 1)
  v  <- c(1, theta, rep(0, max(0, r - 1 - q)))[1:r]
  ph <- c(phi,  rep(0, max(0, r - p)))[1:r]
  G <- matrix(0, r, r); G[, 1] <- ph
  if (r > 1) G[1:(r-1), 2:r] <- diag(r - 1)
  list(G = G, v = v, r = r)
}

## variancia estacionaria de e~ com inovacao unitaria (equacao de Lyapunov)
p11_unit <- function(phi, theta) {
  b <- blocos(phi, theta)
  W <- b$v %*% t(b$v)
  A <- diag(b$r * b$r) - kronecker(b$G, b$G)
  P <- try(matrix(solve(A, as.vector(W)), b$r, b$r), silent = TRUE)
  if (inherits(P, "try-error")) return(NA_real_)
  P[1, 1]
}

## ---- modelo com Var(e~) = 1 -------------------------------------------------
monta_restrito <- function(se, phi, theta) {
  b   <- blocos(phi, theta)
  p11 <- p11_unit(phi, theta)
  if (!is.finite(p11) || p11 <= 0) return(NULL)
  s2  <- 1 / p11                       # <- deriva a inovacao; nao e estimada
  ns  <- 5 + b$r
  function(params) {
    m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se, addInt = FALSE)
    if (b$r > 1) {
      m$FF <- cbind(m$FF, matrix(0, 1, b$r - 1))
      m$GG <- rbind(m$GG, matrix(0, b$r - 1, 6))
      m$GG <- cbind(m$GG, matrix(0, ns, b$r - 1))
    }
    m$GG[6:ns, 6:ns] <- b$G
    W <- matrix(0, ns, ns)
    W[6:ns, 6:ns] <- s2 * (b$v %*% t(b$v))
    W[1,1] <- exp(params[1]); W[2,2] <- exp(params[2]); W[3,3] <- exp(params[3])
    d <- diag(W); diag(W)[d == 0] <- 1e-10
    m$W  <- W
    m$V  <- exp(params[4])            # so 4 parametros livres
    m$m0 <- rep(0, ns)
    m$C0 <- diag(x = 1e6, ns)
    m
  }
}

ajusta <- function(y, se, phi, theta, i0) {
  fn <- monta_restrito(se, phi, theta)
  if (is.null(fn)) return(NULL)
  obj <- function(pp) {
    ll <- try(dlmLL(y, fn(pp)), silent = TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
    ll
  }
  melhor <- NULL
  for (p0 in list(i0[1:4], i0[1:4] + 1, i0[1:4] - 1, rep(0, 4))) {
    ## teto de tempo por partida: combinacoes com raiz proxima do circulo
    ## unitario travavam o otimizador por horas. As guardas de raiz pegam a
    ## maioria dos casos; este e o backstop que garante progresso.
    setTimeLimit(elapsed = 45, transient = TRUE)
    r <- try(optim(p0, obj, method = "L-BFGS-B", control = list(maxit = 400)),
             silent = TRUE)
    setTimeLimit()
    if (inherits(r, "try-error") || r$value >= 1e9) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
  }
  if (is.null(melhor)) return(NULL)
  mod <- fn(melhor$par)
  flt <- try(dlmFilter(y, mod), silent = TRUE)
  if (inherits(flt, "try-error")) return(NULL)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)
  est <- dropFirst(flt$m)
  list(loglik = -melhor$value,
       bic = 2*melhor$value + 4*log(length(y)),
       se_tr = dropFirst(sapply(mse, function(x) sqrt(x[1,1]))),
       sinal = est[,1] + est[,3] + est[,5])
}

## ---- candidatos (mesma logica do 15_) ---------------------------------------
## Converte um MA em sua representacao INVERTIVEL equivalente. Toda raiz dentro
## do circulo unitario e substituida pela sua reciproca: a funcao de
## autocorrelacao e preservada e a representacao passa a ser invertivel. Sem
## isso, o otimizador cai ora numa raiz ora na outra (ambas com perda zero) e
## candidatos MA validos eram descartados.
torna_invertivel <- function(th) {
  r <- polyroot(c(1, th))
  dentro <- Mod(r) < 1
  if (!any(dentro)) return(th)
  r[dentro] <- 1 / r[dentro]
  cf <- 1
  for (ri in r) cf <- c(cf, 0) - c(0, cf) / ri
  Re(cf[-1])
}

inverte_ma <- function(rho, q) {
  perda <- function(z) {
    r <- tryCatch(ARMAacf(ma = as.numeric(z), lag.max = q)[-1],
                  error = function(e) rep(NA, q))
    if (any(!is.finite(r))) return(1e6)
    sum((r - rho[1:q])^2)
  }
  melhor <- NULL
  for (t in 1:6) {
    ini <- if (t == 1) rep(0.2, q) else runif(q, -0.7, 0.9)
    r <- try(optim(ini, perda, method="BFGS", control=list(maxit=500, reltol=1e-10)),
             silent = TRUE)
    if (inherits(r, "try-error")) next
    if (is.null(melhor) || r$value < melhor$value) melhor <- r
    if (melhor$value < 1e-10) break        # ja casou a FAC; nao precisa mais
  }
  if (is.null(melhor) || melhor$value > 1e-4) return(NULL)
  th <- torna_invertivel(melhor$par)
  ## confere que a conversao preservou a FAC alvo
  ck <- tryCatch(ARMAacf(ma = th, lag.max = q)[-1], error = function(e) NULL)
  if (is.null(ck) || sum((ck - rho[1:q])^2) > 1e-4) return(NULL)
  if (!all(Mod(polyroot(c(1, th))) > 1 - 1e-8)) return(NULL)
  list(theta = th)
}

estacionario <- function(phi) !length(phi) || all(Mod(polyroot(c(1, -phi))) > 1.0001)

## AR(p) por Yule-Walker a partir da FAC dos pseudo-erros. E o procedimento que
## a propria tese descreve (p. 180: "os parametros do modelo ... sao obtidos
## pela resolucao das equacoes de Yule-Walker"), mas so AR(1) e AR(2) estavam
## gravados. Aqui a familia e completada ate AR(4).
yule_walker <- function(rho, p) {
  if (length(rho) < p) return(NULL)
  R <- if (p == 1) matrix(1, 1, 1) else toeplitz(c(1, rho[1:(p - 1)]))
  ph <- try(as.numeric(solve(R, rho[1:p])), silent = TRUE)
  if (inherits(ph, "try-error") || any(!is.finite(ph))) return(NULL)
  ph
}

candidatos <- function(ind, k) {
  i <- match(k, COD); taxa <- ind == "taxa"
  f <- if (taxa) file.path(RAIZ,"pseudoerros_taxa_8reg", sprintf("%02d_params_taxa_%s.rds", i, k))
       else      file.path(RAIZ,"pseudoerros_8reg",      sprintf("%02d_params_%s.rds", i, SUF[[k]]))
  pe <- readRDS(f); pre <- if (taxa) "taxamod_" else "mod_"
  sfx <- if (ind == "desocupados") "d" else "o"; s <- SUF[[k]]
  gp <- function(m, nomes, pos) {
    b <- pe[[paste0(pre, m)]]; if (is.null(b)) return(NULL)
    v <- if (taxa) sapply(pos, function(j) as.numeric(b[[j]]))
         else sapply(nomes, function(n) as.numeric(b[[paste0(n, sfx, s)]]))
    if (any(!is.finite(v))) return(NULL)
    unname(v)
  }
  cal <- pe[[grep("^calculos", names(pe), value=TRUE)[
    if (taxa) 1 else if (ind=="desocupados") 1 else 2]]]
  fac <- cal$fac[-1]

  out <- list()
  ## familia AR(1) a AR(4), toda por Yule-Walker sobre a mesma FAC
  for (p in 1:4) {
    ph <- yule_walker(fac, p)
    if (!is.null(ph) && estacionario(ph))
      out[[paste0("AR(", p, ")")]] <- list(phi = ph, theta = numeric(0))
  }
  ## ARMA(1,1) continua vindo dos parametros gravados
  a11 <- gp("arma11", c("phi1_arma11_","theta1_arma11_"), 1:2)
  if (!is.null(a11) && estacionario(a11[1]))
    out[["ARMA(1,1)"]] <- list(phi=a11[1], theta=a11[2])
  ## familia MA(1) a MA(4), invertida da FAC
  for (q in 1:4) {
    inv <- inverte_ma(fac, q)
    if (!is.null(inv)) out[[paste0("MA(",q,")")]] <- list(phi=numeric(0), theta=inv$theta)
  }
  out
}

################################################################################

linhas <- list()
for (ind in c("desocupados","ocupados","taxa")) {
  cat("\n\n####################", toupper(ind), "####################\n")
  for (i in seq_along(COD)) {
    k <- COD[i]; s <- pega(ind, i); ix <- (BURN+1):length(s$y)
    i0 <- log(pmax(c(var(diff(s$y)), var(diff(diff(s$y)))/4, 1e-6, 1e-6, 0.5), 1e-8))
    cat("\n--", ROT[[k]], "--\n"); flush.console()
    cands <- candidatos(ind, k)     # calcular UMA vez (era recalculado por candidato)
    for (nm in names(cands)) {
      cd <- cands[[nm]]
      ## Raiz quase no circulo unitario, de AR OU de MA, sob Var(e~)=1: o erro
      ## amostral fica indistinguivel da tendencia (AR) ou o processo fica na
      ## fronteira da invertibilidade (MA), e o otimizador nao progride.
      ## Estratos com FAC de ordem alta quase nula -- caso do Central em
      ## desocupados -- produzem exatamente isso na inversao MA.
      raiz_ar <- if (length(cd$phi)) max(Mod(polyroot(c(1, -cd$phi)))^-1) else 0
      raiz_ma <- if (length(cd$theta) && any(cd$theta != 0))
                   max(Mod(polyroot(c(1, cd$theta)))^-1) else 0
      if (max(raiz_ar, raiz_ma) > 0.97) {
        cat(sprintf("  %-10s pulado (raiz %s proxima de 1: %.3f)\n", nm,
                    if (raiz_ar >= raiz_ma) "de AR" else "de MA",
                    max(raiz_ar, raiz_ma))); next
      }
      r  <- ajusta(s$y, s$se, cd$phi, cd$theta, i0)
      if (is.null(r)) { cat(sprintf("  %-10s falhou\n", nm)); next }
      rrse <- mean((s$se[ix] - r$se_tr[ix]) / s$se[ix]) * 100
      vic  <- sum(r$sinal[ix] - s$y[ix]) / sum(s$y[ix]) * 100
      linhas[[length(linhas)+1]] <- data.frame(
        indicador=ind, estrato=ROT[[k]], formulacao=nm,
        loglik=round(r$loglik,2), bic=round(r$bic,1),
        rrse=round(rrse,2), vicio=round(vic,2), stringsAsFactors=FALSE)
      cat(sprintf("  %-10s logLik=%9.2f  BIC=%8.1f  RRSE=%7.2f%%\n",
                  nm, r$loglik, r$bic, rrse))
    }
  }
}

tab <- do.call(rbind, linhas)
write.csv(tab, file.path(SAIDA,"comparacao_restrita.csv"), row.names = FALSE)

cat("\n\n############ RESUMO — Var(e~) FIXA EM 1 ############\n")
cat("ajustes:", nrow(tab), "\n\n")
ag <- aggregate(rrse ~ formulacao, tab, mean)
n  <- aggregate(rrse ~ formulacao, tab, length)
ag <- merge(ag, n, by="formulacao", suffixes=c("","_n"))
print(data.frame(formulacao=ag$formulacao, n=ag$rrse_n,
                 rrse_media=round(ag$rrse,1)), row.names=FALSE)

cat("\nganho medio por indicador:\n")
print(round(tapply(tab$rrse, tab$indicador, mean), 2))
cat("\nganhos positivos:", sum(tab$rrse>0), "de", nrow(tab),
    sprintf(" (%.0f%%)\n", 100*mean(tab$rrse>0)))
cat("faixa de RRSE:", round(min(tab$rrse),1), "a", round(max(tab$rrse),1), "%\n")

cat("\n### melhor formulacao por BIC ###\n")
print(table(sapply(split(tab, list(tab$indicador, tab$estrato), drop=TRUE),
                   function(x) x$formulacao[which.min(x$bic)])))

## comparacao direta com a versao livre
liv <- file.path(RAIZ,"outputs","comparacao_formulacoes","comparacao_formulacoes.csv")
if (file.exists(liv)) {
  L <- read.csv(liv, stringsAsFactors=FALSE)
  m <- merge(tab, L[, c("indicador","estrato","formulacao","rrse","var_e")],
             by=c("indicador","estrato","formulacao"), suffixes=c("_fixo","_livre"))
  m <- m[is.finite(m$var_e) & m$var_e > 0, ]
  cat("\n\n############ LIVRE vs FIXO (mesmos ajustes, n =", nrow(m), ") ############\n")
  cat("RRSE media  livre:", round(mean(m$rrse_livre),2), "%\n")
  cat("RRSE media  fixo :", round(mean(m$rrse_fixo),2), "%\n")
  cat("queda media      :", round(mean(m$rrse_livre - m$rrse_fixo),2), "p.p.\n")
  cat("Var(e~) media na versao livre:", round(mean(m$var_e),3), "\n")
  write.csv(m, file.path(SAIDA,"livre_vs_fixo.csv"), row.names=FALSE)
}
cat("\nGravado em", SAIDA, "\n")
