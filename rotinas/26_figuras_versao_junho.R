################################################################################
## 26_figuras_versao_junho.R
##
## Redesenha as seis figuras de resultados da VERSÃO DE JUNHO/2026 do artigo
## (a submetida e apresentada em Diamantina) no padrão gráfico NOVO, para a
## comparação "antes x depois" ficar na mesma formatação.
##
## Os números antigos vêm dos .Rdata que os scripts 28_, 32_ e 35_ liam da
## máquina do Paulo (C:/FJP2425/Programacao/data/Rdatas/). Esses objetos nunca
## foram versionados; este script espera encontrá-los em RDATA_DIR, com a MESMA
## estrutura de subpastas de lá:
##
##   RDATA_DIR/
##     6_estruturaldesocup_8reg/01_mod_bh.Rdata ... 08_mod_cen.Rdata
##     8_estruturalocup_8reg/01_mod_bh.Rdata ... 08_mod_cen.Rdata
##     12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata
##     14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata
##     15_estruturaltaxadesocup_8reg/01_mod_txbh.Rdata ... 08_mod_txcen.Rdata
##     16_multivariado_comcorr - taxadesoc_8reg/estimados/01_taxamod_comcorr.Rdata
##
## (a taxa "indireta" da figura antiga era calculada de 12_ e 14_; o univariado
## da taxa, pasta 15_, entra como terceira linha de modelo, igual à figura nova).
## Os objetos e campos usados
## são exatamente os dos scripts antigos: ma1_bh$ts.trend, ar1_val$cv.trend,
## modelo_mult$ts.trend_3, modelo_mult$se.trend_3 etc.
##
## Estimativas diretas e EPs do desenho: baseestr8reg.rds da RAIZ (vintage do
## artigo), como nos scripts antigos.
##
## Uso:  REPO_RAIZ=<repo> RDATA_DIR=<pasta Rdatas> Rscript rotinas/26_figuras_versao_junho.R
##       INDICADORES="taxa" (opcional) restringe a execução, p.ex. enquanto os
##       .Rdata univariados de desocupados/ocupados ainda não chegaram.
## Saída: outputs/figuras_versao_junho/Figura_<Ind>_<k>.png (6 arquivos, CV na
##        escala comum CV_MAX) e Figura_<Ind>_<k>_semescala.png (CV com eixo livre),
##        pela mesma função figura() de 25_ -> mesma paleta, janela e layout.
################################################################################

suppressMessages({ library(dlm); library(ggplot2); library(scales); library(patchwork) })

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
stopifnot(dir.exists(file.path(RAIZ, "pseudoerros_8reg")))

RDATA <- Sys.getenv("RDATA_DIR", unset = file.path(RAIZ, "data", "_versao_junho_2026", "Rdatas"))
FIGS  <- file.path(RAIZ, "outputs", "figuras_versao_junho")
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)

## --- as funções de figura de 25_, sem executar o resto daquele script --------
## (bloco entre "## FIGURAS" e "## execução"; define INICIO, ROTULO_Y, COR_SERIE,
##  painel() e figura(); carrega 00_tema_graficos.R)
src25 <- readLines(file.path(RAIZ, "rotinas", "25_saidas_artigo.R"), encoding = "UTF-8")
ini <- grep("^## FIGURAS", src25); fim <- grep("^## execução", src25)
stopifnot(length(ini) == 1, length(fim) == 1, fim > ini)
eval(parse(text = src25[(ini + 1):(fim - 2)], encoding = "UTF-8"))
ROT <- c("01 - Belo Horizonte", "02 - Entorno e Colar Metropolitano de BH",
         "03 - Sul de Minas", "04 - Triângulo Mineiro", "05 - Zona da Mata",
         "06 - Norte de Minas", "07 - Vale do Rio Doce", "08 - Central")
NOMEFIG <- c(desocupados = "Desocupacao", ocupados = "Ocupacao", taxa = "TaxaDesoc")

## --- estimativas diretas (vintage do artigo) ---------------------------------
base <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))[1:8]
diretas <- function(ind) {
  col <- switch(ind, desocupados = c("Total.de.desocupados", "sd_d"),
                     ocupados    = c("Total.de.ocupados",    "sd_o"),
                     taxa        = c("Taxa.de.desocupação",  "sd_txd"))
  esc <- if (ind == "taxa") 100 else 1/1000       # totais em milhares, taxa em %
  list(Y  = sapply(base, function(b) b[[col[1]]] * esc),
       SE = sapply(base, function(b) b[[col[2]]] * esc))
}

## --- leitura dos .Rdata antigos ----------------------------------------------
carrega_env <- function(rel) {
  p <- file.path(RDATA, rel)
  if (!file.exists(p)) stop("não encontrado: ", p, "\n  (RDATA_DIR = ", RDATA, ")")
  e <- new.env(); load(p, envir = e); e
}
COD <- c("bh", "ent", "sul", "trg", "mat", "nrt", "val", "cen")
## objeto univariado escolhido no artigo antigo, por estrato (scripts 28_ e 32_)
OBJ_UNI <- list(
  desocupados = c(bh = "ma1_bh", ent = "ma1_ent", sul = "arma11_sul", trg = "ma1_trg",
                  mat = "ma1_mat", nrt = "ma1_nrt", val = "ar1_val", cen = "ma1_cen"),
  ocupados    = setNames(paste0("ar1_", COD), COD),
  taxa        = c(bh = "ma1_bh", ent = "ma1_ent", sul = "arma11_sul", trg = "ma1_trg",
                  mat = "ma1_mat", nrt = "ma1_nrt", val = "arma11_val", cen = "ma1_cen"))
PASTA_UNI <- c(desocupados = "6_estruturaldesocup_8reg", ocupados = "8_estruturalocup_8reg",
               taxa = "15_estruturaltaxadesocup_8reg")
PREFIXO_UNI <- c(desocupados = "", ocupados = "", taxa = "tx")   # 01_mod_bh vs 01_mod_txbh
ARQ_MULT  <- c(desocupados = "12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",
               ocupados    = "14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",
               taxa        = "16_multivariado_comcorr - taxadesoc_8reg/estimados/01_taxamod_comcorr.Rdata")

## EP a partir do que o objeto tiver: se.trend, ou cv.trend * ts.trend
## (o cv antigo ora é fração, ora %; decide-se pela mediana fora do burn-in)
ep_de <- function(obj, tr, sufixo = "") {
  se <- obj[[paste0("se.trend", sufixo)]]
  if (!is.null(se)) return(as.numeric(se))
  cv <- as.numeric(obj[[paste0("cv.trend", sufixo)]])
  if (median(abs(cv[-(1:8)]), na.rm = TRUE) > 1) cv <- cv / 100
  cv * tr
}

univariado <- function(ind) {
  tr <- se <- matrix(NA_real_, 52, 8)
  for (i in 1:8) {
    e   <- carrega_env(file.path(PASTA_UNI[ind],
                                 sprintf("%02d_mod_%s%s.Rdata", i, PREFIXO_UNI[ind], COD[i])))
    nm  <- OBJ_UNI[[ind]][COD[i]]
    if (!exists(nm, envir = e)) {           # tolera outro candidato escolhido
      cand <- ls(e, pattern = paste0("_", COD[i], "$"))
      stop("objeto ", nm, " não está em ", PASTA_UNI[ind], "/", i, "; existem: ",
           paste(cand, collapse = ", "))
    }
    obj <- get(nm, envir = e)
    tr[, i] <- as.numeric(obj$ts.trend); se[, i] <- ep_de(obj, tr[, i])
  }
  list(tr = tr, se = se)
}
multivariado <- function(ind) {
  e <- carrega_env(ARQ_MULT[ind]); m <- e$modelo_mult
  stopifnot(!is.null(m))
  tr <- sapply(1:8, function(i) as.numeric(m[[paste0("ts.trend_", i)]]))
  se <- sapply(1:8, function(i) ep_de(m, tr[, i], paste0("_", i)))
  list(tr = tr, se = se)
}

## --- monta o objeto f de figura() para cada indicador ------------------------
monta_f <- function(ind) {
  d <- diretas(ind)
  U <- univariado(ind); M <- multivariado(ind)
  f <- list(Y = d$Y, SE = d$SE,
            mod = list(list(tr = U$tr, se = U$se), list(tr = M$tr, se = M$se)),
            leg = c("Tendência - Mod. univariado", "Tendência - Mod. multivariado"))
  if (ind == "taxa") {
    ## cálculo indireto da versão antiga: tendências multivariadas de 12_ e 14_,
    ## variância pela fórmula do script 35_ (Cov(D,O) = 0)
    D <- multivariado("desocupados"); O <- multivariado("ocupados")
    TL <- D$tr + O$tr
    r  <- D$tr / TL
    vr <- (1 / TL^2) * D$se^2 + (D$tr^2 / TL^4) * (D$se^2 + O$se^2)
    f$mod[[3]] <- list(tr = 100 * r, se = 100 * sqrt(vr))
    f$leg      <- c(f$leg, "Taxa calculada indiretamente")
  }
  f
}

## --- execução ------------------------------------------------------------------
cat("Rdata antigos em:", RDATA, "\n")
INDS <- strsplit(Sys.getenv("INDICADORES", "desocupados,ocupados,taxa"), ",")[[1]]
ganhos <- list()
for (ind in INDS) {
  cat("####", toupper(ind), "####\n")
  f <- monta_f(ind)
  ## a taxa antiga foi modelada em %, os totais em milhares — mesma escala que a figura nova
  ok <- all(sapply(f$mod, function(m) all(dim(m$tr) == c(52, 8), is.finite(m$tr[9:52, ]))))
  if (!ok) stop("séries incompletas em ", ind)
  ## ganho de precisão (RRSE) por estrato, mesma fórmula e janela do pipeline novo
  ix <- 9:52
  rrse <- function(se_m) colMeans((f$SE[ix, ] - se_m[ix, ]) / f$SE[ix, ]) * 100
  g <- data.frame(indicador = ind, estrato = ROT)
  for (k in seq_along(f$mod)) g[[paste0("rrse_", k)]] <- round(rrse(f$mod[[k]]$se), 2)
  names(g)[-(1:2)] <- c("rrse_uni", "rrse_multi", "rrse_indireta")[seq_along(f$mod)]
  ganhos[[ind]] <- g
  for (k in 1:2) {
    regs <- if (k == 1) 1:4 else 5:8
    arq  <- file.path(FIGS, paste0("Figura_", NOMEFIG[ind], "_", k, ".png"))
    figura(f, regs, arq, INICIO[[ind]], ROTULO_Y[[ind]], ROT, CV_MAX[ind])
    cat("  gravado:", basename(arq), "\n")
    ## versão com o eixo de CV livre, como no 25_
    figura(f, regs, sub("\\.png$", "_semescala.png", arq),
           INICIO[[ind]], ROTULO_Y[[ind]], ROT, cv_max = NA)
  }
  figuras_por_estrato(f, FIGS, ind, CV_MAX[ind])
  saveRDS(f, file.path(FIGS, paste0("series_figuras_", ind, ".rds")))
  ## ANTES x DEPOIS por estrato, se a rodada final (25_) já salvou as suas séries
  fd_arq <- file.path(RAIZ, "outputs", "figuras_final", paste0("series_figuras_", ind, ".rds"))
  if (file.exists(fd_arq)) {
    fd <- readRDS(fd_arq); dc <- file.path(FIGS, "comparacao"); dir.create(dc, showWarnings = FALSE)
    for (i in 1:8) {
      arq_c <- file.path(dc, sprintf("Figura_%s_estrato_%02d_comparacao.png", NOMEFIG[ind], i))
      figura_comparacao(f, fd, i, arq_c, INICIO[[ind]], ROTULO_Y[[ind]], ROT, CV_MAX[ind])
      figura_comparacao(f, fd, i, sub("\\.png$", "_semescala.png", arq_c), INICIO[[ind]], ROTULO_Y[[ind]], ROT, NA)
    }
    cat("  gravadas: 16 comparações antes x depois em", dc, "\n")
  }
}
## uma tabela por indicador (a taxa tem uma coluna a mais)
for (ind in names(ganhos))
  write.csv(ganhos[[ind]], file.path(FIGS, paste0("ganhos_versao_junho_", ind, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
cat("\nPronto. Figuras da versão de junho, no padrão novo, em", FIGS, "\n")
