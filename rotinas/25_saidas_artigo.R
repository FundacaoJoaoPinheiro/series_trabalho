################################################################################
## 25_saidas_artigo.R   — PASSO 6 do pipeline de revisão
##
## Regera TODAS as saídas que o artigo consome, a partir dos resultados finais:
##   - 6 figuras (2 por indicador): nível com IC 95% + coeficientes de variação
##   - 5 tabelas .tex por indicador:
##       tabhiper*   hiperparâmetros (univariado e multivariado) + processo ARMA
##       diag*       diagnósticos dos resíduos (Shapiro-Wilk, Ljung-Box, H)
##       matrizcorr* correlação dos distúrbios das inclinações + autovalores
##       diffvicio*  diferença relativa do erro-padrão e vício relativo
##       est_pontual* estimativas pontuais do último trimestre
##   - tabcomptaxa.tex — NOVA: taxa indireta vs direta (resultado central da seção)
##
## Insumos: outputs/univariado_final/, outputs/multivariado_final/,
##          outputs/taxa_final/
## Saída  : diretório do artigo (ver ARTIGO abaixo) e outputs/figuras_final/
##
## Os diagnósticos do MULTIVARIADO são calculados aqui — o passo 4 não os grava.
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

## O manuscrito passou a ser versionado dentro do repositorio, em artigo/.
## Essa e a copia CANONICA: e nela que este script grava as tabelas e figuras.
## ARTIGO_DIR ainda permite apontar para outro lugar, se necessario.
ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))
FIGS   <- file.path(RAIZ, "outputs", "figuras_final")
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)

BURN <- 8
P    <- 8
B_NIVEL <- 1:8; B_SAZ1 <- 17:24; B_SAZ3 <- 33:40

PASTA <- c(desocupados = "Total de desocupados",
           ocupados    = "Total de ocupados",
           taxa        = "Taxa de desocupação")
SUFIXO <- c(desocupados = "desoc", ocupados = "ocup", taxa = "taxa")
NOMEFIG <- c(desocupados = "Desocupacao", ocupados = "Ocupacao", taxa = "TaxaDesoc")
ROT_IND <- c(desocupados = "total de desocupados",
             ocupados    = "total de ocupados",
             taxa        = "taxa de desocupação")
## artigo definido, para as legendas concordarem ("para o total" / "para a taxa")
ART <- c(desocupados = "o", ocupados = "o", taxa = "a")

## rótulos como aparecem no artigo (uma linha) e na versão em parbox
ROT <- c("01 - Belo Horizonte",
         "02 - Entorno e Colar Metropolitano de BH",
         "03 - Sul de Minas", "04 - Triângulo Mineiro", "05 - Zona da Mata",
         "06 - Norte de Minas", "07 - Vale do Rio Doce", "08 - Central")
ROT_BOX <- ROT
ROT_BOX[2] <- paste0("\\raisebox{0em}{\\parbox[t]{2.8cm}{02 - Entorno e Colar ",
                     "\\\\[-2pt] Metropolitano de BH}}")

################################################################################
## utilitários de formatação
################################################################################
fmt <- function(x, d = 4) {
  s <- formatC(x, format = "f", digits = d, big.mark = "")
  gsub("\\.", ",", s)
}
## p-valor com estrelas: * 10%, ** 5%, *** 1%
pstar <- function(p) {
  e <- ifelse(p <= 0.01, "***", ifelse(p <= 0.05, "**", ifelse(p <= 0.10, "*", "")))
  paste0(fmt(p, 4), e)
}
grava <- function(txt, ind, arq) {
  d <- file.path(ARTIGO, "resultados", PASTA[ind])
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  con <- file(file.path(d, arq), open = "w", encoding = "UTF-8")
  writeLines(txt, con); close(con)
  cat("  gravado:", file.path(PASTA[ind], arq), "\n")
}
## teste H de heterocedasticidade (Durbin e Koopman)
teste_H <- function(r) {
  n <- length(r); h <- floor(n/3)
  H <- sum(r[(n-h+1):n]^2) / sum(r[1:h]^2)
  2 * min(pf(H, h, h), 1 - pf(H, h, h))
}

################################################################################
## carrega tudo de um indicador
################################################################################
carrega <- function(ind) {
  uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final",
                           paste0("modelos_", ind, ".rds")))
  cod <- names(uni)
  Y   <- sapply(cod, function(k) uni[[k]]$serie$y)
  SE  <- sapply(cod, function(k) uni[[k]]$serie$se)
  m   <- readRDS(file.path(RAIZ, "outputs", "multivariado_final",
                           paste0("multivariado_", ind, ".rds")))
  flt <- dlmFilter(Y, m$mod)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)
  est <- dropFirst(flt$m)

  tr_mv <- est[, B_NIVEL]
  se_mv <- sapply(B_NIVEL, function(i)
    dropFirst(sapply(mse, function(x) sqrt(x[i, i]))))
  sin_mv <- est[, B_NIVEL] + est[, B_SAZ1] + est[, B_SAZ3]
  se_sin_mv <- sapply(1:P, function(i) {
    cc <- rep(0, ncol(est)); cc[c(B_NIVEL[i], B_SAZ1[i], B_SAZ3[i])] <- 1
    dropFirst(sapply(mse, function(x) sqrt(drop(cc %*% x %*% cc))))
  })
  ## hiperparâmetros do multivariado, na mesma ordem do univariado
  W <- m$mod$W
  hp_mv <- rbind(L = diag(W)[B_NIVEL], R = diag(W)[9:16], S = diag(W)[B_SAZ1],
                 I = diag(m$mod$V))
  ## diagnósticos do multivariado
  rr <- residuals(flt, type = "standardized", sd = FALSE)
  rr <- as.matrix(rr)[-(1:BURN), , drop = FALSE]
  diag_mv <- sapply(1:P, function(i) c(
    sw = shapiro.test(rr[, i])$p.value,
    lb = Box.test(rr[, i], lag = 8, type = "Ljung-Box")$p.value,
    h  = teste_H(rr[, i])))

  list(uni = uni, cod = cod, Y = Y, SE = SE, m = m,
       tr_mv = tr_mv, se_mv = se_mv, sin_mv = sin_mv, se_sin_mv = se_sin_mv,
       hp_mv = hp_mv, diag_mv = diag_mv,
       tr_uni = sapply(cod, function(k) uni[[k]]$corrigido$trend),
       se_uni = sapply(cod, function(k) uni[[k]]$corrigido$se_trend),
       sin_uni = sapply(cod, function(k) uni[[k]]$corrigido$sinal),
       hp_uni = sapply(cod, function(k) uni[[k]]$corrigido$hp),
       proc = sapply(cod, function(k) uni[[k]]$processo),
       diag_uni = sapply(cod, function(k) c(
         sw = uni[[k]]$corrigido$shapiro,
         lb = uni[[k]]$corrigido$ljung,
         h  = unname(uni[[k]]$corrigido$H["p"]))),
       rrse_uni = sapply(cod, function(k) uni[[k]]$rrse),
       vicio_uni = sapply(cod, function(k) uni[[k]]$vicio))
}

################################################################################
## TABELA 1 — hiperparâmetros
################################################################################
tab_hiper <- function(d, ind) {
  lin <- function(rot, v, proc)
    sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", rot, proc,
            fmt(v[1]), fmt(v[2]), fmt(v[3]), fmt(v[4]), fmt(v[5]))
  cab <- paste0("\\textbf{Estrato geográfico} & \\textbf{Processo} & ",
                "\\(\\hat{\\sigma}_L^2\\) & \\(\\hat{\\sigma}_R^2\\) & ",
                "\\(\\hat{\\sigma}_S^2\\) & \\(\\hat{\\sigma}_I^2\\) & ",
                "\\(\\hat{\\sigma}_{\\tilde{e}}^2\\) \\\\")
  c("\\begin{table}[H]", "\\centering",
    "\\captionsetup{justification=centering}",
    sprintf("\\caption{Hiperparâmetros estimados para %s %s - modelos univariado e multivariado}",
            ART[ind], ROT_IND[ind]),
    sprintf("\\label{tab:hiper%s}", SUFIXO[ind]),
    "\\scalebox{0.92}{", "\\renewcommand{\\arraystretch}{0.85}",
    "\\begin{tabular}{llccccc}", "\\toprule",
    "& & \\multicolumn{5}{c}{\\textbf{Modelo Univariado}} \\\\",
    "\\cmidrule(lr){3-7}", cab, "\\midrule",
    sapply(1:P, function(i) lin(ROT[i], d$hp_uni[, i], d$proc[i])),
    "\\midrule",
    "& & \\multicolumn{5}{c}{\\textbf{Modelo Multivariado}} \\\\",
    "\\cmidrule(lr){3-7}", cab, "\\midrule",
    sapply(1:P, function(i)
      lin(ROT[i], c(d$hp_mv[, i], d$hp_uni[5, i]), d$proc[i])),
    "\\bottomrule", "\\end{tabular}}",
    paste0("\\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
           "Nota: o processo do erro amostral é identificado individualmente por ",
           "estrato (Seção \\ref{sec:metodologia}), e ",
           "\\(\\hat{\\sigma}_{\\tilde{e}}^2\\) é derivada dos coeficientes do ",
           "processo, por isso coincidindo nos dois modelos. Sobre ",
           "\\(\\sigma_I^2\\), ver a ressalva de identificação no texto.}"),
    "\\end{table}")
}

################################################################################
## TABELA 2 — diagnósticos
################################################################################
tab_diag <- function(d, ind) {
  lin <- function(rot, v)
    sprintf("%s & %s & %s & %s \\\\", rot, pstar(v[1]), pstar(v[2]), pstar(v[3]))
  cab <- "\\textbf{Estrato geográfico} & Shapiro-Wilk & Ljung-Box & H \\\\"
  c("\\begin{table}[H]", "\\centering",
    "\\captionsetup{justification=centering}",
    sprintf("\\caption{Teste de diagnóstico do resíduo para %s %s - modelos univariado e multivariado}",
            ART[ind], ROT_IND[ind]),
    sprintf("\\label{tab:diag%s}", SUFIXO[ind]),
    "\\scalebox{1}{", "\\renewcommand{\\arraystretch}{0.7}",
    "\\begin{tabular}{lccc}", "\\toprule",
    "& \\multicolumn{3}{c}{\\textbf{Modelo Univariado}} \\\\",
    "\\cmidrule(lr){2-4}", cab, "\\midrule",
    sapply(1:P, function(i) lin(ROT[i], d$diag_uni[, i])),
    "\\midrule",
    "& \\multicolumn{3}{c}{\\textbf{Modelo Multivariado}} \\\\",
    "\\cmidrule(lr){2-4}", cab, "\\midrule",
    sapply(1:P, function(i) lin(ROT[i], d$diag_mv[, i])),
    "\\bottomrule", "\\end{tabular}}",
    paste0("\\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
           "Nota: * representa rejeição de \\(H_0\\) a 10\\%, ** a 5\\% e *** a 1\\%. ",
           "As oito primeiras observações foram descartadas, em razão do período de ",
           "estabilização do filtro de Kalman.}"),
    "\\end{table}")
}

################################################################################
## TABELA 3 — matriz de correlação
################################################################################
tab_corr <- function(d, ind) {
  C  <- d$m$Corr_R; s2 <- diag(d$m$Sigma_R); ev <- d$m$autovalores
  tol <- 1e-8 * max(abs(ev)); posto <- sum(ev > tol)
  ## posto EFETIVO: autovalores que carregam variância relevante (>= 1% do maior).
  ## É essa a leitura substantiva -- o posto numérico apenas atesta que a matriz
  ## não é singular até a precisão da máquina.
  efet <- sum(ev >= 0.01 * max(ev))
  linhas <- sapply(1:P, function(i) {
    cel <- sapply(1:P, function(j) if (j < i) fmt(C[i, j], 4) else if (j == i) "1" else "")
    sprintf("%s & %s & %s \\\\", ROT_BOX[i], fmt(s2[i], 4), paste(cel, collapse = " & "))
  })
  ev_txt <- paste(fmt(ev[ev > 1e-3], 3), collapse = "; ")
  n_peq  <- sum(ev <= 1e-3)
  c("\\begin{table}[H]", "\\centering",
    "\\captionsetup{justification=centering}",
    sprintf("\\caption{Correlação estimada entre os distúrbios das inclinações - modelo multivariado - %s}",
            ROT_IND[ind]),
    sprintf("\\label{tab:matriz_correlacao_%s}", SUFIXO[ind]), "",
    "\\renewcommand{\\arraystretch}{0.8}", "",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{@{}p{2.8cm}c*{8}{c}@{}}", "\\toprule",
    paste0("\\textbf{Estrato Geográfico} & \\(\\hat{\\sigma}_R^2\\) & ",
           paste(sprintf("\\textbf{%d}", 1:P), collapse = " & "), " \\\\"),
    "\\midrule", linhas, "\\bottomrule", "\\end{tabular}%", "}", "",
    paste0("\\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
           "Nota: ",
           sprintf("autovalores estimados de \\(\\Sigma_R\\): %s%s. ", ev_txt,
                   if (n_peq == 0) "" else sprintf(" e %s inferior%s a \\(10^{-3}\\)",
                     if (n_peq == 1) "1 valor" else paste(n_peq, "valores"),
                     if (n_peq == 1) "" else "es")),
           sprintf(paste0("Posto numérico de %d em %d; posto \\textbf{efetivo} de %d, ",
                          "contando os autovalores que retêm ao menos 1\\%% do maior. "),
                   posto, P, efet),
           "As correlações devem, portanto, ser lidas em conjunto, e não ",
           "isoladamente.}"),
    "\\end{table}")
}

################################################################################
## TABELA 4 — desempenho
################################################################################
tab_desemp <- function(d, ind) {
  mv <- d$m$desempenho
  linhas <- sapply(1:P, function(i)
    sprintf("%s & %s & %s & %s & %s \\\\", ROT[i],
            fmt(d$rrse_uni[i], 2), fmt(mv$rrse[i], 2),
            fmt(d$vicio_uni[i], 2), fmt(mv$vicio[i], 2)))
  c("\\begin{table}[H]", "\\centering",
    "\\captionsetup{justification=centering}",
    sprintf("\\caption{Medidas de desempenho dos modelos univariado e multivariado - %s}",
            ROT_IND[ind]),
    sprintf("\\label{tab:diffvicio%s}", SUFIXO[ind]),
    "{%", "\\renewcommand{\\arraystretch}{0.8}", "\\scalebox{0.95}{%",
    "\\begin{tabular}{lcccc}", "\\toprule",
    paste0("& \\multicolumn{2}{c}{\\textbf{\\makecell{Diferença relativa média \\\\ ",
           "do erro padrão (\\%)}}} & \\multicolumn{2}{c}{\\textbf{Vício relativo (\\%)}} \\\\"),
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    paste0("\\multicolumn{1}{l}{\\textbf{Estrato Geográfico}} & \\textbf{Univariado} & ",
           "\\textbf{Multivariado} & \\textbf{Univariado} & \\textbf{Multivariado} \\\\"),
    "\\midrule", linhas, "\\midrule",
    sprintf("\\textbf{Média} & \\textbf{%s} & \\textbf{%s} & & \\\\",
            fmt(mean(d$rrse_uni), 2), fmt(mean(mv$rrse), 2)),
    "\\bottomrule", "\\end{tabular}%", "}", "}",
    paste0("\\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
           "Nota: as oito primeiras observações de cada série foram descartadas do ",
           "cálculo, em razão do período de estabilização do filtro de Kalman.}"),
    "\\end{table}")
}

################################################################################
## TABELA 5 — estimativas pontuais do último trimestre
################################################################################
tab_pontual <- function(d, ind) {
  n <- nrow(d$Y); dg <- if (ind == "taxa") 2 else 2
  linhas <- sapply(1:P, function(i) {
    y <- d$Y[n, i]; se <- d$SE[n, i]
    th <- d$sin_mv[n, i]; ste <- d$se_sin_mv[n, i]
    sprintf("%s & %s & %s\\%% & [%s ; %s] & %s & %s\\%% & [%s ; %s] \\\\",
            ROT_BOX[i], fmt(y, dg), fmt(100*se/y, 2),
            fmt(y - 1.96*se, dg), fmt(y + 1.96*se, dg),
            fmt(th, dg), fmt(100*ste/th, 2),
            fmt(th - 1.96*ste, dg), fmt(th + 1.96*ste, dg))
  })
  c("\\begin{table}[H]", "\\centering",
    "\\captionsetup{justification=centering}",
    sprintf(paste0("\\caption{Resultados pontuais do modelo multivariado para %s %s ",
                   "- 4\\textordmasculine{} trimestre de 2024}"), ART[ind], ROT_IND[ind]),
    sprintf("\\label{tab:pontual_%s}", SUFIXO[ind]), "",
    "\\renewcommand{\\arraystretch}{0.8}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{@{}p{2.8cm}cccccc@{}}", "\\toprule",
    paste0("\\textbf{Estrato Geográfico} & $\\hat{y}$ & $CV(\\hat{y})$ & IC 95\\% & ",
           "$\\hat{\\theta}$ & $CV(\\hat{\\theta})$ & IC 95\\% \\\\"),
    "\\midrule", linhas, "\\bottomrule", "\\end{tabular}%", "}",
    paste0("\\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
           "Nota: \\(\\hat{y}\\) é a estimativa direta e \\(\\hat{\\theta}\\) o sinal ",
           "estimado pelo modelo multivariado (tendência mais sazonalidade).",
           if (ind == "taxa") " Valores em pontos percentuais." else
             " Valores em milhares de pessoas.", "}"),
    "\\end{table}")
}

################################################################################
## TABELA 6 — taxa: indireta vs direta (nova)
################################################################################
tab_comp_taxa <- function() {
  tt <- readRDS(file.path(RAIZ, "outputs", "taxa_final", "taxa_final.rds"))
  t2 <- tt$desempenho
  linhas <- sapply(1:P, function(i)
    sprintf("%s & %s & %s & %s & %s & %s \\\\", ROT[i],
            fmt(t2$cv_direta[i], 2), fmt(t2$cv_indireta[i], 2),
            fmt(t2$cv_direta_mod[i], 2),
            fmt(t2$ganho_indireta[i], 2), fmt(t2$ganho_direta[i], 2)))
  c("\\begin{table}[H]", "\\centering",
    "\\captionsetup{justification=centering}",
    paste0("\\caption{Comparação entre as duas estratégias para a taxa de desocupação ",
           "- cálculo indireto e estimação direta pelo modelo multivariado}"),
    "\\label{tab:comptaxa}",
    "{%", "\\renewcommand{\\arraystretch}{0.8}", "\\scalebox{0.9}{%",
    "\\begin{tabular}{lccccc}", "\\toprule",
    paste0("& \\multicolumn{3}{c}{\\textbf{Coeficiente de variação médio (\\%)}} & ",
           "\\multicolumn{2}{c}{\\textbf{\\makecell{Diferença relativa média \\\\ ",
           "do erro padrão (\\%)}}} \\\\"),
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6}",
    paste0("\\multicolumn{1}{l}{\\textbf{Estrato Geográfico}} & ",
           "\\textbf{\\makecell{Estimativa \\\\ direta}} & ",
           "\\textbf{\\makecell{Cálculo \\\\ indireto}} & ",
           "\\textbf{\\makecell{Modelo \\\\ direto}} & ",
           "\\textbf{\\makecell{Cálculo \\\\ indireto}} & ",
           "\\textbf{\\makecell{Modelo \\\\ direto}} \\\\"),
    "\\midrule", linhas, "\\midrule",
    sprintf("\\textbf{Média} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
            fmt(mean(t2$cv_direta), 2), fmt(mean(t2$cv_indireta), 2),
            fmt(mean(t2$cv_direta_mod), 2),
            fmt(mean(t2$ganho_indireta), 2), fmt(mean(t2$ganho_direta), 2)),
    "\\bottomrule", "\\end{tabular}%", "}", "}",
    paste0("\\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
           "Nota: o cálculo indireto obtém a taxa a partir das tendências estimadas ",
           "do total de desocupados e do total de ocupados, com variância por ",
           "linearização de Taylor sob a hipótese \\(\\mathrm{Cov}(\\hat{D}_L,T_L)=0\\), ",
           "discutida no texto. As oito primeiras observações ",
           "foram descartadas do cálculo.}"),
    "\\end{table}")
}

################################################################################
## TABELA 7 — processos ARMA selecionados (seção de metodologia)
################################################################################
tab_arma <- function() {
  esp <- read.csv(file.path(RAIZ, "outputs", "identificacao", "especificacao_final.csv"),
                  stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  pega <- function(ind, i) {
    e <- esp[esp$indicador == ind & esp$ordem == i, ]
    stopifnot(nrow(e) == 1)
    e$processo
  }
  linhas <- sapply(1:P, function(i)
    sprintf("        %s & %s & %s & %s \\\\", sub(" - ", " -- ", ROT[i]),
            pega("desocupados", i), pega("ocupados", i), pega("taxa", i)))
  c("\\begin{table}[!htb]", "    \\centering",
    "    \\captionsetup{justification=centering}",
    paste0("    \\caption{Processo do erro amostral identificado por estrato ",
           "geográfico e indicador}"),
    "    \\resizebox{\\textwidth}{!}{",
    "    \\begin{tabular}{@{}lccc@{}}", "        \\toprule",
    paste0("        \\textbf{Estrato geográfico} & \\textbf{Total de desocupados} & ",
           "\\textbf{Total de ocupados} & \\textbf{Taxa de desocupação} \\\\"),
    "        \\midrule", linhas, "        \\bottomrule",
    "    \\end{tabular}", "    }",
    paste0("    \\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
           "Nota: a identificação é individual por estrato e indicador. Entre os ",
           "candidatos cujos resíduos não apresentam autocorrelação significativa pelo ",
           "teste de Ljung-Box a 5\\%, seleciona-se o mais parcimonioso, com ",
           "desempate pelo BIC.}"),
    "    \\label{tab:modelos_arma}", "\\end{table}")
}

################################################################################
## FIGURAS
################################################################################
INICIO <- list(desocupados = c(2014, 1), ocupados = c(2014, 1), taxa = c(2014, 1))  # BURN = 8 para os tres
ROTULO_Y <- list(desocupados = "Desocupados (mil pessoas)",
                 ocupados    = "Ocupados (mil pessoas)",
                 taxa        = "Taxa de desocupação (%)")

suppressMessages(library(patchwork))
source(file.path(RAIZ, "rotinas", "00_tema_graficos.R"))

## Cores semanticas, na paleta do artigo:
##   estimativa direta = cinza | univariado (ou indireta) = azul | multivariado = vermelho
COR_SERIE <- c("grey30", PAL_ARTIGO[1], PAL_ARTIGO[3])
ROT_IC    <- "IC 95% da estimativa direta"

painel <- function(f, i, ini, rot_y) {
  ts_ <- function(v) window(ts(v, start = c(2012, 1), frequency = 4), start = ini)
  y  <- ts_(f$Y[, i]);    se <- ts_(f$SE[, i])
  tu <- ts_(f$tr_a[, i]); su <- ts_(f$se_a[, i])
  tm <- ts_(f$tr_b[, i]); sm <- ts_(f$se_b[, i])
  dt <- periodo_para_data(sprintf("%d_0%d", floor(as.numeric(time(y))), cycle(y)))

  lv <- c("Estimativa direta", f$leg[1], f$leg[2])
  linha_df <- function(a, b, c) {
    d <- rbind(data.frame(data = dt, valor = as.numeric(a), serie = lv[1]),
               data.frame(data = dt, valor = as.numeric(b), serie = lv[2]),
               data.frame(data = dt, valor = as.numeric(c), serie = lv[3]))
    d$serie <- factor(d$serie, levels = lv); d
  }
  d_niv <- linha_df(y, tu, tm)
  d_cv  <- linha_df(100 * se / y, 100 * su / tu, 100 * sm / tm)
  ic    <- data.frame(data = dt,
                      li = as.numeric(y - 1.96 * se),
                      ls = as.numeric(y + 1.96 * se))

  esqueleto <- function(d, ylab) {
    ggplot(d, aes(data, valor, color = serie)) +
      scale_color_manual(values = setNames(COR_SERIE, lv)) +
      scale_x_date(breaks = seq(as.Date("2014-01-01"), as.Date("2026-01-01"), by = "2 years"),
                   date_labels = "%Y", date_minor_breaks = "1 year",
                   expand = expansion(mult = c(.01, .02))) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = NULL, y = ylab) +
      tema_artigo(11) +
      theme(plot.title   = element_text(face = "bold", size = 11.5),
            plot.margin  = margin(6, 10, 4, 8),
            legend.margin = margin(0, 0, 0, 0))
  }
  p_niv <- esqueleto(d_niv, rot_y) +
    geom_ribbon(data = ic, inherit.aes = FALSE,
                aes(data, ymin = li, ymax = ls, fill = ROT_IC), alpha = .16) +
    scale_fill_manual(values = setNames("grey45", ROT_IC)) +
    geom_line(linewidth = .65)
  p_cv <- esqueleto(d_cv, "Coeficiente de variação (%)") + geom_line(linewidth = .65)
  list(niv = p_niv, cv = p_cv)
}

figura <- function(f, regs, arq, ini, rot_y, rot) {
  ps <- lapply(regs, function(i) painel(f, i, ini, rot_y))
  linhas <- lapply(seq_along(regs), function(k)
    (ps[[k]]$niv + labs(title = rot[regs[k]])) | (ps[[k]]$cv + labs(title = " ")))
  g <- Reduce(`/`, linhas) + plot_layout(guides = "collect") &
       theme(legend.position = "bottom")
  ggsave(arq, g, width = 10, height = 12.6, dpi = 120, bg = "white")
}

################################################################################
## execução
################################################################################
cat("Artigo:", ARTIGO, "\n\n")
stopifnot(dir.exists(ARTIGO))
sintese <- list()

for (ind in c("desocupados", "ocupados", "taxa")) {
  cat("####", toupper(ind), "####\n")
  d <- carrega(ind)

  grava(tab_hiper(d, ind),   ind, paste0("tabhiper",  SUFIXO[ind], ".tex"))
  grava(tab_diag(d, ind),    ind, paste0("diag",      SUFIXO[ind], ".tex"))
  grava(tab_corr(d, ind),    ind, paste0("matrizcorr_", SUFIXO[ind], ".tex"))
  grava(tab_desemp(d, ind),  ind, paste0("diffvicio", SUFIXO[ind], ".tex"))
  grava(tab_pontual(d, ind), ind, paste0("est_pontual_", SUFIXO[ind], ".tex"))

  ## figuras: azul = univariado (ou cálculo indireto, na taxa), vermelho = multivariado
  f <- list(Y = d$Y, SE = d$SE, tr_b = d$tr_mv, se_b = d$se_mv,
            tr_a = d$tr_uni, se_a = d$se_uni,
            leg = c("Tendência - Mod. univariado", "Tendência - Mod. multivariado"))
  if (ind == "taxa") {
    tt <- readRDS(file.path(RAIZ, "outputs", "taxa_final", "taxa_final.rds"))
    f$tr_a <- tt$taxa_indireta * 100
    f$se_a <- tt$se_indireta   * 100
    f$leg  <- c("Taxa calculada indiretamente", "Tendência - Mod. multivariado")
  }
  for (k in 1:2) {
    regs <- if (k == 1) 1:4 else 5:8
    arq  <- paste0("Figura_", NOMEFIG[ind], "_", k, ".png")
    figura(f, regs, file.path(FIGS, arq), INICIO[[ind]], ROTULO_Y[[ind]], ROT)
    file.copy(file.path(FIGS, arq),
              file.path(ARTIGO, "resultados", PASTA[ind], arq), overwrite = TRUE)
    cat("  gravado:", file.path(PASTA[ind], arq), "\n")
  }

  sintese[[ind]] <- c(uni = mean(d$rrse_uni), mv = mean(d$m$desempenho$rrse),
                      lb_uni = sum(d$diag_uni["lb", ] <= 0.05),
                      lb_mv  = sum(d$diag_mv["lb", ]  <= 0.05),
                      sw_mv  = sum(d$diag_mv["sw", ]  <= 0.05),
                      h_mv   = sum(d$diag_mv["h",  ]  <= 0.05),
                      posto  = sum(d$m$autovalores > 1e-8*max(d$m$autovalores)),
                      efetivo = sum(d$m$autovalores >= 0.01*max(d$m$autovalores)))
  cat("\n")
}

grava(tab_comp_taxa(), "taxa", "tabcomptaxa.tex")

## tabela da seção de metodologia — fora das pastas por indicador
con <- file(file.path(ARTIGO, "resultados", "modelos_arma.tex"), open = "w", encoding = "UTF-8")
writeLines(tab_arma(), con); close(con)
cat("  gravado: modelos_arma.tex\n")

cat("\n############ SÍNTESE ############\n")
print(round(do.call(rbind, sintese), 2))
cat("\nFiguras também em", FIGS, "\n")
