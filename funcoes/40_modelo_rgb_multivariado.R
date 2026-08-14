################################################################################
## MODELO ESTRUTURAL MULTIVARIADO POR ONDA DE ROTAÇÃO, COM EFEITO DE PAINEL
## (rotation group bias) — van den Brakel & Krieg (2015); Pfeffermann (1991)
##
## MOTIVAÇÃO (ver docs/verificacoes/V2_rotation_group_bias.md)
## O modelo atual observa UMA série trimestral agregada e precisa dos pseudo-erros
## só para fixar um escalar: a autocorrelação do erro amostral (`m$GG[6,6]`).
## Esse escalar é estimado a partir de um pseudo-erro que, nos ocupados, está
## contaminado por rotation group bias — daí |rho1| > 0,5 e a cauda longa da FAC.
##
## Aqui as 5 ondas de rotação entram como observação MULTIVARIADA. Com isso:
##   - o efeito de painel é ESTIMADO (lambda_j,t), em vez de contaminar o erro;
##   - a variância amostral V vem dos erros-padrão design-based POR ONDA — é
##     CONHECIDA, não estimada, e não precisa de pseudo-erros;
##   - há 5 observações por trimestre em vez de 1.
##
## ESPECIFICAÇÃO
##   y_{j,t} = mu_t + gamma_t + lambda_{j,t} + e_{j,t},   j = 1..5 (V1016)
##
##   mu_t     tendência linear local (nível + inclinação)
##   gamma_t  sazonalidade trigonométrica (s = 4)
##   lambda   efeito da j-ésima entrevista. Restrição soma_j lambda_j = 0, então
##            só 4 são livres e lambda_5 = -(lambda_1+...+lambda_4). Passeio
##            aleatório, porque o diagnóstico mostrou que o viés DERIVA no tempo
##            (rgb = "rw"); use rgb = "fixo" para efeito constante e rgb = "nao"
##            para desligar — é assim que se testa se o efeito é necessário.
##   e_{j,t}  erro amostral, com Var = se_{j,t}^2 (design-based, conhecido)
##
## ESTADOS: 1 nivel | 2 inclinacao | 3-5 sazonal trig | 6-9 lambda_1..lambda_4
##
## LIMITAÇÃO CONHECIDA: e_{j,t} é tratado como independente entre ondas e no
## tempo. Ondas distintas no mesmo trimestre SÃO amostras disjuntas (então a
## independência vale dentro de t), mas a mesma coorte reaparece na onda j+1 do
## trimestre t+1 — essa correlação não está modelada aqui e tende a subestimar um
## pouco a variância do sinal. Tratá-la exige estados que acompanhem a coorte
## (van den Brakel & Krieg §3.2), etapa seguinte da fase 2.
################################################################################
suppressMessages(library(dlm))

## y  : matriz T×5 de estimativas por onda (coluna j = V1016 = j)
## se : matriz T×5 de erros-padrão design-based, mesma orientação
## rgb: "rw" (padrão) | "fixo" | "nao"
loglik_pos_difusao <- NULL  # definida abaixo; declarada aqui para o escopo
f.modelo_rgb <- function(y, se, rgb = c("rw", "fixo", "nao"), init = NULL) {
  rgb <- match.arg(rgb)
  y <- as.matrix(y); se <- as.matrix(se)
  stopifnot(ncol(y) == 5, all(dim(y) == dim(se)))
  Tn <- nrow(y)
  n_lambda <- if (rgb == "nao") 0 else 4
  p <- 5 + n_lambda                       # nº de estados

  ## ---- matrizes fixas ------------------------------------------------------
  poly <- dlmModPoly(2)                   # estados 1-2
  trig <- dlmModTrig(s = 4)               # estados 3-5

  GG <- matrix(0, p, p)
  GG[1:2, 1:2] <- poly$GG
  GG[3:5, 3:5] <- trig$GG
  if (n_lambda) GG[6:p, 6:p] <- diag(n_lambda)   # lambda: passeio aleatório

  ## FF 5×p — a onda 5 entra com -1 nos quatro lambdas (restrição de soma zero)
  FF <- matrix(0, 5, p)
  FF[, 1] <- 1                            # nível
  FF[, 3] <- 1; FF[, 5] <- 1              # sazonal trigonométrico
  if (n_lambda) {
    FF[1:4, 6:p] <- diag(4)
    FF[5, 6:p]   <- -1
  }

  ## V variável no tempo: diag(se_{j,t}^2), conhecida. dlm lê de X via JV.
  X  <- se^2
  JV <- matrix(0, 5, 5); diag(JV) <- 1:5

  ## ---- construtor ----------------------------------------------------------
  # params: 1 nivel, 2 inclinacao, 3 sazonal, 4 lambda (se rgb == "rw")
  build <- function(params) {
    W <- matrix(0, p, p)
    W[1, 1] <- exp(params[1])
    W[2, 2] <- exp(params[2])
    W[3, 3] <- exp(params[3]); W[5, 5] <- exp(params[3])
    # atenção: `diag(W)[6:p] <- x` NÃO funciona — modifica uma cópia da diagonal
    # e a descarta, deixando W zerado em silêncio. Indexar a matriz diretamente.
    if (rgb == "rw") W[cbind(6:p, 6:p)] <- exp(params[4])
    # rgb == "fixo": W dos lambdas fica 0 -> efeito constante no tempo
    dlm(FF = FF, GG = GG, V = diag(5), W = W,
        m0 = rep(0, p), C0 = diag(1e7, p), X = X, JV = JV)
  }

  npar <- if (rgb == "rw") 4 else 3
  if (is.null(init)) init <- rep(0, npar)

  modelo <- list(rgb = rgb, npar = npar, fn = build)
  modelo$fit <- dlmMLE(y, init, build, control = list(maxit = 1e5))
  modelo$mod <- build(modelo$fit$par)
  modelo$filtered <- dlmFilter(y, modelo$mod)
  modelo$smoothed <- dlmSmooth(modelo$filtered)
  modelo$sm <- dropFirst(modelo$smoothed$s)

  ## ---- componentes ---------------------------------------------------------
  modelo$ts.trend    <- modelo$sm[, 1]
  modelo$ts.slope    <- modelo$sm[, 2]
  modelo$ts.seasonal <- modelo$sm[, 3] + modelo$sm[, 5]
  modelo$ts.signal   <- modelo$ts.trend + modelo$ts.seasonal

  ## lambda das 5 ondas (a 5ª por diferença), em nível
  if (n_lambda) {
    L <- modelo$sm[, 6:p, drop = FALSE]
    modelo$lambda <- cbind(L, -rowSums(L))
    colnames(modelo$lambda) <- paste0("ent", 1:5)
  } else {
    modelo$lambda <- NULL
  }

  ## erro-padrão do sinal (nível + sazonais), a partir da variância suavizada
  vs <- dlmSvd2var(modelo$smoothed$U.S, modelo$smoothed$D.S)
  cs <- matrix(0, 1, p); cs[1, c(1, 3, 5)] <- 1
  modelo$se.signal <- dropFirst(sapply(vs, function(P) sqrt(cs %*% P %*% t(cs))))
  modelo$cv.signal <- 100 * modelo$se.signal / modelo$ts.signal

  ## ---- ajuste --------------------------------------------------------------
  ## ATENÇÃO à comparação entre variantes: a verossimilhança cheia NÃO é
  ## comparável entre modelos com números diferentes de estados difusos. As 4
  ## componentes lambda entram com C0 = 1e7, e o preço da difusão inicial recai
  ## sobre o modelo COM efeito de painel — o que inverte artificialmente a
  ## comparação (na PoC, o modelo sem lambda "vencia" por 20 pontos de AIC; com
  ## a verossimilhança pós-difusão, PERDE por 13). Use sempre `loglik_pos`.
  modelo$loglik <- -modelo$fit$value
  modelo$loglik_pos <- loglik_pos_difusao(modelo, y)
  modelo$aic    <- 2 * modelo$npar - 2 * modelo$loglik_pos
  modelo$bic    <- log(Tn * 5) * modelo$npar - 2 * modelo$loglik_pos
  modelo$aic_full <- 2 * modelo$npar - 2 * modelo$loglik   # só para referência
  modelo$conv   <- modelo$fit$convergence
  modelo
}

## Verossimilhança descartando o período de difusão inicial (padrão: 8 trimestres),
## para que variantes com números diferentes de estados difusos sejam comparáveis.
loglik_pos_difusao <- function(modelo, y, burn = 8) {
  y <- as.matrix(y)
  f  <- dlmFilter(y, modelo$mod)
  vR <- dlmSvd2var(f$U.R, f$D.R)
  FFm <- modelo$mod$FF; X <- modelo$mod$X
  s <- 0
  for (t in (burn + 1):nrow(y)) {
    Q <- FFm %*% vR[[t]] %*% t(FFm) + diag(X[t, ], 5)
    e <- as.numeric(y[t, ] - FFm %*% f$a[t, ])
    s <- s - 0.5 * (log(det(Q)) + t(e) %*% solve(Q, e) + 5 * log(2 * pi))
  }
  as.numeric(s)
}
