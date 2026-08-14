################################################################################
## MODELO POR ONDA COM EFEITO DE PAINEL **E** ERRO AMOSTRAL CORRELACIONADO
## AO LONGO DA COORTE  — fase 2 do caminho C
## van den Brakel & Krieg (2015) §3.2; Pfeffermann (1991)
##
## O QUE MUDA EM RELAÇÃO AO `40_modelo_rgb_multivariado.R`
## Lá o erro amostral e(j,t) era independente entre ondas e no tempo. Dentro de um
## trimestre isso vale — as 5 ondas são amostras DISJUNTAS. Mas a mesma coorte de
## domicílios reaparece na onda j+1 do trimestre t+1, e essa correlação existe.
## Ignorá-la subestima a variância do sinal (o CV da fase 1 é otimista).
##
## ESPECIFICAÇÃO
##   y(j,t) = mu(t) + gamma(t) + lambda(j) + se(j,t) * a(j,t)
##
##   a(1,t) = eta(1,t)                      <- coorte NOVA, independente
##   a(j,t) = rho * a(j-1,t-1) + eta(j,t)   <- MESMA coorte, entrevista seguinte
##
## `a` é o erro amostral PADRONIZADO (Var = 1), e a heterocedasticidade entra por
## se(j,t) na matriz de observação — daí Var(eta(1)) = 1 e Var(eta(j)) = 1-rho^2.
## Assim `rho` é diretamente a correlação do erro amostral entre duas entrevistas
## consecutivas do mesmo domicílio: exatamente o que os pseudo-erros estimavam por
## fora, aqui obtido por máxima verossimilhança junto com o resto do modelo.
##
## ESTADOS: 1 nivel | 2 inclinacao | 3-5 sazonal | 6-9 lambda | 10-14 a(1..5)
##
## Hiperparâmetros: var(nivel), var(inclinacao), var(sazonal), rho  -> 4
## (o modelo atual do artigo tem 5, e ainda depende dos pseudo-erros)
################################################################################
suppressMessages(library(dlm))

## y  : matriz T×5 de estimativas por onda (coluna j = V1016 = j)
## se : matriz T×5 de erros-padrão design-based
## rgb: "fixo" (padrão — a PoC mostrou efeito constante) | "nao"
## rho_fixo: se dado, não estima rho (útil para perfilar a verossimilhança)
## cor_ondas: correlação CONTEMPORÂNEA entre as 5 ondas no mesmo trimestre.
##   Elas compartilham UPAs, então são NEGATIVAMENTE correlacionadas — medido em
##   162 casos (V8): -0,215 nos ocupados e -0,110 nos desocupados, estável ao
##   longo de 13 anos. O default 0 reproduz o comportamento anterior (errado,
##   mantido só para comparação); passe o valor medido para a especificação certa.
##   Limite teórico: -1/(K-1) = -0,25 para K = 5.
f.modelo_coorte <- function(y, se, rgb = c("fixo", "nao"), rho_fixo = NULL,
                            init = NULL, cor_ondas = 0) {
  stopifnot(cor_ondas > -1/4, cor_ondas < 1)   # PD da equicorrelação com K=5
  rgb <- match.arg(rgb)
  y <- as.matrix(y); se <- as.matrix(se)
  stopifnot(ncol(y) == 5, all(dim(y) == dim(se)))
  Tn <- nrow(y)

  ## ---- ESCALONAMENTO INTERNO --------------------------------------------------
  ## As séries vão de ~20 mil (desocupados de região pequena) a ~9.000 mil
  ## (ocupados de MG). Um mesmo conjunto de valores iniciais não pode servir a
  ## essa faixa: foi o que quebrou a 1ª tentativa da fase 3. Aqui tudo é levado a
  ## nível ~1, estimado, e as saídas em nível são reescaladas de volta. Razões
  ## (CV) e `rho` são invariantes à escala.
  escala <- mean(y)
  y  <- y  / escala
  se <- se / escala
  n_lam <- if (rgb == "nao") 0 else 4
  i_a   <- (5 + n_lam) + 1:5              # posições dos estados de erro amostral
  p     <- 5 + n_lam + 5

  poly <- dlmModPoly(2); trig <- dlmModTrig(s = 4)

  ## ---- FF: se(j,t) multiplica o estado a(j) -> varia no tempo (JFF/X) -------
  FF <- matrix(0, 5, p)
  FF[, 1] <- 1; FF[, 3] <- 1; FF[, 5] <- 1
  if (n_lam) { FF[1:4, 6:9] <- diag(4); FF[5, 6:9] <- -1 }
  JFF <- matrix(0, 5, p)
  for (j in 1:5) JFF[j, i_a[j]] <- j       # coluna j de X guarda se(j,t)
  X <- se

  ## V é nominal: todo o erro amostral está nos estados a(j). Um valor pequeno,
  ## proporcional à escala, evita matriz de previsão singular no filtro.
  V_nominal <- diag(max(1e-8, 1e-10 * mean(y^2)), 5)

  build <- function(params) {
    rho <- if (is.null(rho_fixo)) tanh(params[length(params)]) else rho_fixo

    GG <- matrix(0, p, p)
    GG[1:2, 1:2] <- poly$GG
    GG[3:5, 3:5] <- trig$GG
    if (n_lam) GG[6:9, 6:9] <- diag(4)          # lambda constante (W = 0)
    ## a(j,t) = rho * a(j-1,t-1): subdiagonal dentro do bloco de erro amostral.
    ## a(1,t) não tem antecessor — a coorte é nova, então a linha fica zerada.
    for (j in 2:5) GG[i_a[j], i_a[j - 1]] <- rho

    W <- matrix(0, p, p)
    W[1, 1] <- exp(params[1])
    W[2, 2] <- exp(params[2])
    W[3, 3] <- exp(params[3]); W[5, 5] <- exp(params[3])
    ## Bloco do erro amostral. As variâncias das inovações são 1 para a onda 1
    ## (coorte nova) e 1-rho^2 para as demais, de modo que Var(a_j) = 1 e o `se`
    ## carregue toda a escala. A correlação contemporânea entre ondas entra como
    ## equicorrelação nas INOVAÇÕES: com Var(a_j)=1 e a recursão
    ## a_j,t = rho*a_{j-1,t-1} + eta_j,t, impor Corr(eta_i,eta_j) = cor_ondas
    ## induz Corr(a_i,t, a_j,t) ~ cor_ondas no mesmo trimestre.
    s_eta <- sqrt(c(1, rep(1 - rho^2, 4)))
    R_w <- matrix(cor_ondas, 5, 5); diag(R_w) <- 1
    W[i_a, i_a] <- outer(s_eta, s_eta) * R_w

    ## C0: difuso no estrutural e no lambda; estacionário no erro amostral —
    ## a distribuição estacionária de a tem Var=1 e a mesma equicorrelação
    C0 <- diag(1e7, p); C0[i_a, i_a] <- R_w

    dlm(FF = FF, GG = GG, V = V_nominal, W = W,
        m0 = rep(0, p), C0 = C0, JFF = JFF, X = X)
  }

  npar <- 3 + as.integer(is.null(rho_fixo))

  ## ---- valores iniciais: MULTI-START -----------------------------------------
  ## Com a série escalonada (nível ~ 1, ver `escala` no wrapper), as log-variâncias
  ## ficam em faixa conhecida, o que permite partir de pontos sensatos em vez de
  ## `rep(0,3)` — que significa variância 1 num nível 1, absurdamente grande e a
  ## causa das não-convergências da 1ª tentativa da fase 3.
  if (is.null(init)) {
    v_nivel <- log(max(var(diff(rowMeans(y))), 1e-10))
    grade <- list(c(v_nivel, v_nivel - 4, v_nivel - 4),
                  c(v_nivel - 2, v_nivel - 6, v_nivel - 6),
                  c(v_nivel + 2, v_nivel - 2, v_nivel - 2),
                  c(-8, -12, -12))
    if (is.null(rho_fixo)) grade <- unlist(
      lapply(grade, function(g) list(c(g, atanh(0.9)), c(g, atanh(0.6)))),
      recursive = FALSE)
    inits <- grade
  } else {
    inits <- list(init)
  }

  ## roda todos os pontos de partida e fica com o de maior verossimilhança
  fits <- lapply(inits, function(i0)
    ## maxit MODERADO, de propósito. Com `maxit = 1e5` (o valor herdado dos
    ## scripts originais) um ponto de partida ruim itera essencialmente para
    ## sempre dentro do otimizador — e como o filtro de Kalman roda em código
    ## COMPILADO, `setTimeLimit` do R não consegue interromper: o teto de tempo
    ## nunca é checado. Foi assim que a fase 3 travou duas vezes (8,3 h e 1,6 h
    ## de CPU sem sair do lugar). Com o teto baixo, um start ruim simplesmente
    ## não converge e é descartado — que é o papel do multi-start.
    tryCatch(dlmMLE(y, i0, build, control = list(maxit = 200)),
             error = function(e) NULL))
  ok <- Filter(function(f) !is.null(f) && f$convergence == 0 && is.finite(f$value), fits)
  if (!length(ok)) stop("nenhum ponto de partida convergiu")

  m <- list(rgb = rgb, npar = npar, fn = build, i_a = i_a)
  m$fit <- ok[[which.min(sapply(ok, `[[`, "value"))]]
  m$n_starts_ok <- length(ok)
  m$n_starts <- length(inits)
  ## dispersão entre os ótimos encontrados: se for grande, a superfície tem
  ## múltiplos máximos e o resultado depende do ponto de partida — sinal de alerta
  m$spread_loglik <- diff(range(sapply(ok, `[[`, "value")))
  m$mod <- build(m$fit$par)
  m$rho <- if (is.null(rho_fixo)) tanh(m$fit$par[npar]) else rho_fixo
  m$conv <- m$fit$convergence

  m$smoothed <- dlmSmooth(dlmFilter(y, m$mod))
  m$sm <- dropFirst(m$smoothed$s)

  ## saídas em NÍVEL voltam à escala original; CV e rho são invariantes
  m$escala      <- escala
  m$ts.trend    <- m$sm[, 1] * escala
  m$ts.seasonal <- (m$sm[, 3] + m$sm[, 5]) * escala
  m$ts.signal   <- m$ts.trend + m$ts.seasonal
  if (n_lam) {
    L <- m$sm[, 6:9, drop = FALSE]
    m$lambda <- cbind(L, -rowSums(L)) * escala
    colnames(m$lambda) <- paste0("ent", 1:5)
  }

  vs <- dlmSvd2var(m$smoothed$U.S, m$smoothed$D.S)
  cs <- matrix(0, 1, p); cs[1, c(1, 3, 5)] <- 1
  m$se.signal <- dropFirst(sapply(vs, function(P) sqrt(cs %*% P %*% t(cs)))) * escala
  m$cv.signal <- 100 * m$se.signal / m$ts.signal

  m$loglik <- -m$fit$value
  m$loglik_pos <- loglik_pos_coorte(m, y)
  m$aic <- 2 * m$npar - 2 * m$loglik_pos
  m
}

## Verossimilhança pós-difusão (ver nota em funcoes/40_): modelos com números
## diferentes de estados difusos não são comparáveis pela verossimilhança cheia.
loglik_pos_coorte <- function(m, y, burn = 8) {
  y <- as.matrix(y)
  f  <- dlmFilter(y, m$mod)
  vR <- dlmSvd2var(f$U.R, f$D.R)
  s <- 0
  for (t in (burn + 1):nrow(y)) {
    FFt <- m$mod$FF
    FFt[cbind(1:5, m$i_a)] <- m$mod$X[t, ]     # FF depende de t (JFF)
    Q <- FFt %*% vR[[t]] %*% t(FFt) + m$mod$V
    e <- as.numeric(y[t, ] - FFt %*% f$a[t, ])
    s <- s - 0.5 * (log(det(Q)) + t(e) %*% solve(Q, e) + 5 * log(2 * pi))
  }
  as.numeric(s)
}
