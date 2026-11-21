################################################################################
## 36_escala_trg_multistart.R — robustez de 10/11/2026, tarefa 2
##
## No 27_validacao_escala.R, Triângulo Mineiro × total de ocupados sai com
## lambda = 1 e RV = 0 EXATOS — suspeita de otimizador parado no chute inicial.
## Aqui se reestima esse caso (e, por precaução, os 24) com partidas em que o
## lambda inicial é 0,5; 0,8; 1,2; 1,5 e 2, mantendo as demais variâncias no
## valor da rodada C, além das cinco partidas do 27_. Fica o melhor ponto.
##
## Insumo: outputs/univariado_final/ (somente leitura), validacao_escala.csv
## Saída : outputs/robustez_2026-11-10/escala/
################################################################################
suppressMessages(library(dlm))
RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
source(file.path(RAIZ, "rotinas", "_id_funcoes.R"))
SAIDA <- file.path(RAIZ, "outputs", "robustez_2026-11-10", "escala")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)
LIM <- 20

## idêntica à do 27_
com_escala <- function(se, phi, theta, smooth) {
  fn0 <- monta(se, phi, theta, smooth = smooth, sazdet = TRUE)
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

ajusta <- function(u) {
  y <- u$serie$y; se <- u$serie$se; sm <- isTRUE(u$smooth)
  fn <- com_escala(se, u$phi, u$theta, sm); np0 <- attr(fn, "np0"); np <- np0 + 1
  obj <- function(pp) {
    ll <- try(dlmLL(y, fn(pp)), silent = TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
    ll
  }
  i0 <- c(log(pmax(u$corrigido$hp[c(if (!sm) 1, 2, 4)], 1e-8)), 0)
  partidas <- c(
    setNames(lapply(list(0, 1, -1, -3, 3), function(d) i0 + d),
             paste0("27_desloc", c(0, 1, -1, -3, 3))),
    setNames(lapply(c(0.5, 0.8, 1.2, 1.5, 2), function(l) { p <- i0; p[np] <- log(l); p }),
             paste0("lambda0=", c(0.5, 0.8, 1.2, 1.5, 2))))
  res <- do.call(rbind, lapply(names(partidas), function(nm) {
    p0 <- pmin(pmax(partidas[[nm]], -LIM + 1), LIM - 1)
    r <- try(optim(p0, obj, method = "L-BFGS-B", lower = rep(-LIM, np),
                   upper = rep(LIM, np), control = list(maxit = 300)), silent = TRUE)
    if (inherits(r, "try-error"))
      return(data.frame(partida = nm, lambda0 = exp(p0[np]), lambda = NA, loglik = NA,
                        conv = NA, iter = NA, msg = "erro", sR = NA, sI = NA))
    data.frame(partida = nm, lambda0 = exp(p0[np]), lambda = exp(r$par[np]),
               loglik = -r$value, conv = r$convergence, iter = r$counts[1],
               msg = if (is.null(r$message)) "" else r$message,
               sR = exp(r$par[np0 - 1]), sI = exp(r$par[np0]))
  }))
  ## diagnóstico do ponto de partida do 27_: o objetivo e o gradiente numérico
  g <- sapply(seq_len(np), function(j) { h <- 1e-4; e <- numeric(np); e[j] <- h
    (obj(i0 + e) - obj(i0 - e)) / (2 * h) })
  list(tab = res, grad_i0 = g, obj_i0 = obj(i0))
}

ve <- read.csv(file.path(RAIZ, "outputs", "validacao_escala", "validacao_escala.csv"),
               fileEncoding = "UTF-8", stringsAsFactors = FALSE)
todos <- list()
for (ind in c("desocupados", "ocupados", "taxa")) {
  uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final", paste0("modelos_", ind, ".rds")))
  for (k in names(uni)) {
    u <- uni[[k]]; a <- ajusta(u)
    b <- a$tab[which.max(a$tab$loglik), ]
    LR <- 2 * (b$loglik - u$corrigido$loglik)
    todos[[length(todos) + 1]] <- data.frame(indicador = ind, estrato = u$rotulo,
      lambda_27 = ve$lambda[ve$indicador == ind & ve$estrato == u$rotulo],
      RV_27 = ve$RV[ve$indicador == ind & ve$estrato == u$rotulo],
      lambda = b$lambda, loglik_restrito = u$corrigido$loglik, loglik_livre = b$loglik,
      RV = LR, p = pchisq(max(LR, 0), 1, lower.tail = FALSE), melhor_partida = b$partida,
      stringsAsFactors = FALSE)
    if (ind == "ocupados" && k == "trg") {
      cat("\n#### Triângulo Mineiro × ocupados: todas as partidas ####\n")
      print(a$tab, row.names = FALSE, digits = 6)
      cat("objetivo no chute do 27_:", format(a$obj_i0, digits = 10),
          "| -logLik restrito da rodada C:", format(-u$corrigido$loglik, digits = 10), "\n")
      cat("gradiente numérico no chute do 27_ (log sR, log sI, log lambda):",
          format(a$grad_i0, digits = 4), "\n")
      write.csv(a$tab, file.path(SAIDA, "trg_ocupados_partidas.csv"), row.names = FALSE,
                fileEncoding = "UTF-8")
    }
    cat(sprintf("%-11s %-4s lambda 27_ = %.3f -> %.3f | RV %.2f -> %.2f\n", ind, k,
                tail(todos, 1)[[1]]$lambda_27, b$lambda, tail(todos, 1)[[1]]$RV_27, LR))
  }
}
t <- do.call(rbind, todos)
write.csv(t, file.path(SAIDA, "validacao_escala_multistart.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
cat("\n############ SÍNTESE ############\n")
cat("27_ original : mediana lambda =", round(median(t$lambda_27), 3),
    "| rejeitam a 5% =", sum(pchisq(pmax(t$RV_27, 0), 1, lower.tail = FALSE) < 0.05), "de", nrow(t), "\n")
cat("multi-start  : mediana lambda =", round(median(t$lambda), 3),
    "| rejeitam a 5% =", sum(t$p < 0.05), "de", nrow(t),
    "| faixa", round(min(t$lambda), 3), "a", round(max(t$lambda), 3), "\n")
cat("casos em que o multi-start melhora a logLik livre em > 0,01:",
    sum(t$loglik_livre - (t$loglik_restrito + t$RV_27 / 2) > 0.01), "\n")
