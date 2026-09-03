################################################################################
## 06 - PSEUDO-ERROS: autocovariâncias do erro amostral e parâmetros dos modelos
## Versão revisada/limpa. Consolida "15_Erro Amostral 8reg.R" (2.539 linhas, um
## bloco copiado-colado por região) num único passe sobre as 9 séries.
##
## Insumo : base ALINHADA por grupo de rotação (saída da rotina 05 / script 14).
## Saída  : pseudoerros_8reg/NN_params_<sigla>.rds — um por região, com a mesma
##          estrutura do original (db, calculos_desocupada, calculos_ocupada,
##          mod_ar1, mod_ar2, mod_ma1, mod_ma2, mod_arma11, mod_ar5).
##
## Método: pseudo-erros de Silva & Cruz (2002). Para cada trimestre, o desvio de
## cada grupo de rotação em relação à média dos K grupos estima o erro amostral;
## as autocovariâncias desses pseudo-erros, somadas sobre os grupos e divididas
## por (K^2 - K), estimam as autocovariâncias do erro amostral da estimativa
## trimestral, que alimentam a componente de erro amostral dos modelos.
##
## Correções em relação ao 15 original (cada uma sob a flag FIXES, para permitir
## rodar "com/sem" e medir o efeito — ver docs/verificacoes/):
##   [#10a] MA(1): theta1 = (1 - sqrt(1 - 4*rho1^2))/(2*rho1) só é real quando
##          |rho1| <= 0.5. O original devolve NaN em silêncio; aqui a violação é
##          registrada e o parâmetro vira NA explícito.
##   [#10b] MA(2)/ARMA(1,1): nleqslv/uniroot podiam não convergir (ou uniroot
##          falhar por falta de troca de sinal) sem que isso aparecesse; aqui a
##          convergência é checada e registrada.
##   [#18]  Pcov2 usa divisor (T-i) e subtrai a média de cada sublag. Isso NÃO é
##          a autocovariância amostral usual (divisor T, média global) e pode
##          gerar sequência não positiva-definida. A variante corrigida usa Pcov.
##
## Uso:
##   Rscript rotinas/06_pseudo_erros.R                # reprodução fiel + confere
##   Rscript rotinas/06_pseudo_erros.R --fixes        # com as correções
##   Rscript rotinas/06_pseudo_erros.R --base=<rds> --out=<dir>
################################################################################
suppressMessages({ library(dplyr); library(nleqslv) })
options(scipen = 999)

## ============================ CONFIGURAÇÃO ====================================
args <- commandArgs(trailingOnly = TRUE)
opt <- function(nome, padrao) {
  hit <- grep(paste0("^--", nome, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", nome, "="), "", hit[1]) else padrao
}
## Os fixes são independentes para permitir atribuir o efeito a cada um:
##   --fix10  guardas de viabilidade/convergência (MA(1), MA(2), ARMA(1,1), zeros)
##   --fix18  autocovariância amostral usual (divisor T, média única)
##   --fixes  os dois
FIX10 <- any(c("--fixes", "--fix10") %in% args)
FIX18 <- any(c("--fixes", "--fix18") %in% args)
FIXES <- FIX10 || FIX18
CONFERIR <- !("--sem-conferencia" %in% args)

# Por padrão lê a base COMMITADA pelo Paulo (vintage do artigo), não a
# reproduzida em data/ (vintage 2025, ~1% mais baixa) — assim o efeito de cada
# fix fica isolado do efeito da revisão dos pesos pós-Censo 2022.
arq_base  <- opt("base", "basealinhada_8reg.rds")
dir_saida <- opt("out",  if (FIXES) "outputs/pseudoerros_8reg_fixes" else "outputs/pseudoerros_8reg_repro")
dir_gab   <- opt("gabarito", "pseudoerros_8reg")   # params commitados = gabarito

LAGS <- 24        # autocovariâncias 0..24
K    <- 5         # grupos de rotação da PNADc

# ordem e siglas conforme os arquivos commitados (01..09)
REGIOES <- c(
  "01-Belo Horizonte"                      = "bh",
  "02-Colar e Entorno Metropolitano de BH" = "ent",
  "03-Sul de Minas"                        = "sul",
  "04-Triângulo Mineiro"                   = "trg",
  "05-Mata de Minas Gerais"                = "mat",
  "06-Norte de Minas"                      = "nrt",
  "07-Vale do Rio Doce"                    = "rio",
  "08-Central"                             = "cen",
  "09 - Minas Gerais"                      = "mg"
)

## ============================== FUNÇÕES =======================================

## Autocovariâncias dos pseudo-erros.
## versao = "original": divisor (T-i) e média recalculada em cada sublag — é o
##   que o funcoes/01_funcoes_pseudo_erro.R chama de Pcov2 e o script 15 usa.
## versao = "amostral": divisor T e média única da série — estimador usual, que
##   garante sequência positiva-semidefinida (é o Pcov do mesmo arquivo). [#18]
autocov_pseudo <- function(v, lag, versao = c("original", "amostral")) {
  versao <- match.arg(versao)
  T <- length(v)
  out <- rep(0, lag)
  if (versao == "original") {
    for (i in 0:(lag - 1)) {
      out[i + 1] <- sum((v[1:(T - i)] - mean(v[1:(T - i)])) *
                        (v[(1 + i):T] - mean(v[(1 + i):T]))) / (T - i)
    }
  } else {
    m <- mean(v)
    for (i in 0:(lag - 1)) {
      out[i + 1] <- sum((v[1:(T - i)] - m) * (v[(1 + i):T] - m)) / T
    }
  }
  out
}

## FACP a partir da FAC, via razão de determinantes (idêntica ao facp_acf do
## funcoes/01, reescrita sem o `print` inalcançável e sem crescer a matriz no laço)
facp_acf <- function(fac, lag) {
  facp <- numeric(lag)
  m <- diag(2)
  for (i in seq_len(lag)) {
    if (i == 1) {
      facp[i] <- fac[i + 1]
    } else {
      if (i == 2) {
        m[2, 1] <- fac[2]; m[1, 2] <- fac[2]
      } else {
        m <- cbind(m, rev(fac[2:i]))
        m <- rbind(m, rev(fac[1:i]))
      }
      den <- m
      num <- den
      num[, i] <- fac[2:(i + 1)]
      facp[i] <- det(num) / det(den)
    }
  }
  facp
}

teste_facp <- function(facp, n) 1 - pchisq(n * facp^2, 1)

## Monta a tabela de cálculos (autocov por grupo, soma, FAC, FACP, testes)
## para um indicador ("ocupada" | "desocupada") de uma região.
tabela_calculos <- function(db, indicador, versao) {
  pseudos <- paste0("pseudo", 1:K, "_", indicador)
  tab <- data.frame(lag = 0:LAGS)
  for (k in 1:K) {
    tab[[paste0("Ch", k)]] <- autocov_pseudo(db[[pseudos[k]]], LAGS + 1, versao)
  }
  tab$SomaChk <- rowSums(tab[, paste0("Ch", 1:K)])
  tab$autocov <- tab$SomaChk / (K^2 - K)
  tab$fac     <- tab$SomaChk / tab$SomaChk[1]
  tab$facp    <- c(0, facp_acf(tab$fac, LAGS))
  tab$esttest <- nrow(db) * tab$facp^2
  tab$pvalor  <- teste_facp(tab$facp, nrow(db))
  tab
}

## Diagnóstico de positividade-definida [#18]. Uma sequência de autocovariâncias
## legítima tem de gerar matriz de covariância (Toeplitz) positiva-semidefinida —
## caso contrário implica variância negativa para alguma combinação linear, e a
## componente de erro amostral do modelo em espaço de estados fica mal definida.
diag_pd <- function(gama) {
  M <- outer(seq_along(gama), seq_along(gama), function(i, j) gama[abs(i - j) + 1])
  min(eigen(M, symmetric = TRUE, only.values = TRUE)$values) / gama[1]
}

pd <- list()
registrar_pd <- function(regiao, indicador, tab) {
  pd[[length(pd) + 1]] <<- data.frame(
    regiao = regiao, indicador = indicador,
    autoval_min_rel = diag_pd(tab$autocov))
}

## Acumulador de avisos (o que os fixes detectam)
avisos <- list()
avisar <- function(regiao, indicador, codigo, msg) {
  avisos[[length(avisos) + 1]] <<-
    data.frame(regiao = regiao, indicador = indicador, codigo = codigo, aviso = msg)
}

## --- parâmetros por especificação -------------------------------------------

## AR(1): phi1 = rho1
par_ar1 <- function(rho_d, rho_o) {
  data.frame(phi1_ar1_d = rho_d[1], phi1_ar1_o = rho_o[1])
}

## AR(2): Yule-Walker
par_ar2 <- function(rho_d, rho_o) {
  yw2 <- function(r) c((r[1] - r[1] * r[2]) / (1 - r[1]^2),
                       (r[2] - r[1]^2)     / (1 - r[1]^2))
  d <- yw2(rho_d); o <- yw2(rho_o)
  data.frame(phi1_ar2_d = d[1], phi2_ar2_d = d[2],
             phi1_ar2_o = o[1], phi2_ar2_o = o[2])
}

## MA(1): rho1 = theta1/(1+theta1^2) invertido. Real só se |rho1| <= 0.5. [#10a]
ma1_theta <- function(rho1, regiao, indicador) {
  if (FIX10 && abs(rho1) > 0.5) {
    avisar(regiao, indicador, "10a",
           sprintf("MA(1) inviavel: |rho1| = %.4f > 0.5 -> theta1 = NA (original devolvia NaN)", rho1))
    return(NA_real_)
  }
  (1 - sqrt(1 - 4 * rho1^2)) / (2 * rho1)
}

par_ma1 <- function(rho_d, rho_o, regiao) {
  data.frame(theta1_ma1_d = ma1_theta(rho_d[1], regiao, "desocupada"),
             theta1_ma1_o = ma1_theta(rho_o[1], regiao, "ocupada"))
}

## MA(2) — apenas desocupados. Sistema não-linear em (theta1, theta2). [#10b]
par_ma2 <- function(rho_d, regiao) {
  sistema <- function(th) {
    c((-th[1] * (1 - th[2])) / (1 + th[1]^2 + th[2]^2) - rho_d[1],
      (-th[2])               / (1 + th[1]^2 + th[2]^2) - rho_d[2])
  }
  sol <- nleqslv(c(0, 0), sistema)
  if (FIX10 && sol$termcd != 1) {
    avisar(regiao, "desocupada", "10b",
           sprintf("MA(2) nleqslv nao convergiu (termcd = %d, ||F|| = %.3g) -> parametros NA",
                   sol$termcd, max(abs(sol$fvec))))
    return(data.frame(theta1_ma2_d = NA_real_, theta2_ma2_d = NA_real_))
  }
  data.frame(theta1_ma2_d = sol$x[1], theta2_ma2_d = sol$x[2])
}

## ARMA(1,1) — apenas desocupados. phi1 = rho2/rho1; theta1 por uniroot. [#10b]
par_arma11 <- function(rho_d, regiao) {
  phi1 <- rho_d[2] / rho_d[1]
  eq <- function(theta1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho_d[1]
  }
  theta1 <- tryCatch(uniroot(eq, interval = c(-1, 1))$root, error = function(e) {
    if (FIX10) avisar(regiao, "desocupada", "10b",
                      paste("ARMA(1,1) uniroot falhou em [-1,1]:", conditionMessage(e)))
    NA_real_
  })
  data.frame(phi1_arma11_d = phi1, theta1_arma11_d = theta1)
}

## AR(5): Yule-Walker, R phi = rho, com R Toeplitz das autocorrelações
par_ar5 <- function(rho_d, rho_o) {
  yw5 <- function(r) {
    R <- matrix(1, 5, 5)
    for (i in 1:5) for (j in 1:5) if (i != j) R[i, j] <- r[abs(i - j)]
    as.numeric(solve(R, r))
  }
  data.frame(phi_d = yw5(rho_d), phi_o = yw5(rho_o))
}

## ======================== PROCESSAMENTO POR REGIÃO ============================
processa_regiao <- function(db, nome, sigla) {
  # médias dos painéis (zeros tratados como ausência de informação, como no original)
  db$media_ocupada <- db %>% select(starts_with("ocupada")) %>%
    replace(. == 0, NA) %>% rowMeans(na.rm = TRUE)
  db$media_desocupada <- db %>% select(starts_with("desocupada")) %>%
    replace(. == 0, NA) %>% rowMeans(na.rm = TRUE)

  # [#10] zeros incoerentes: um grupo de rotação com estimativa zero num
  # trimestre é implausível para ocupados/desocupados de uma região inteira e
  # contamina a média e, portanto, todos os pseudo-erros daquela linha.
  for (ind in c("ocupada", "desocupada")) {
    cols <- paste0(ind, "_", 1:K)
    nz <- sum(db[, cols] == 0, na.rm = TRUE)
    if (nz > 0) avisar(nome, ind, "10",
                       sprintf("%d celula(s) com estimativa zero em %d linhas (grupos de rotacao)",
                               nz, sum(rowSums(db[, cols] == 0, na.rm = TRUE) > 0)))
  }

  for (ind in c("ocupada", "desocupada")) {
    for (k in 1:K) {
      db[[paste0("pseudo", k, "_", ind)]] <- db[[paste0(ind, "_", k)]] - db[[paste0("media_", ind)]]
    }
  }

  versao <- if (FIX18) "amostral" else "original"   # [#18]
  clc_d <- tabela_calculos(db, "desocupada", versao)
  clc_o <- tabela_calculos(db, "ocupada",    versao)
  registrar_pd(nome, "desocupada", clc_d)
  registrar_pd(nome, "ocupada",    clc_o)

  rho_d <- clc_d$fac[2:6]   # rho1..rho5
  rho_o <- clc_o$fac[2:6]

  out <- list(db, clc_d, clc_o,
              par_ar1(rho_d, rho_o), par_ar2(rho_d, rho_o),
              par_ma1(rho_d, rho_o, nome), par_ma2(rho_d, nome),
              par_arma11(rho_d, nome), par_ar5(rho_d, rho_o))
  # nomes idênticos aos do script 15 (que sufixa com a sigla da região)
  names(out) <- c(paste0("db", sigla),
                  paste0("calculos_desocupada_", sigla),
                  paste0("calculos_ocupada_", sigla),
                  "mod_ar1", "mod_ar2", "mod_ma1", "mod_ma2", "mod_arma11", "mod_ar5")
  out
}

## ================================ EXECUÇÃO ====================================
stopifnot(file.exists(arq_base))
base <- readRDS(arq_base)
dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)

cat("Base    :", arq_base, "(", length(base), "series )\n")
cat("Saida   :", dir_saida, "\n")
cat("Modo    :", if (FIXES) "COM fixes (#10, #18)" else "reproducao fiel do script 15", "\n\n")

faltantes <- setdiff(names(REGIOES), names(base))
if (length(faltantes)) stop("regioes ausentes na base: ", paste(faltantes, collapse = ", "))

resultados <- list()
for (i in seq_along(REGIOES)) {
  nome  <- names(REGIOES)[i]
  sigla <- REGIOES[[i]]
  res <- processa_regiao(base[[nome]], nome, sigla)
  arq <- file.path(dir_saida, sprintf("%02d_params_%s.rds", i, sigla))
  saveRDS(res, arq)
  resultados[[sigla]] <- res
  cat(sprintf("  [%d/9] %-40s -> %s\n", i, nome, basename(arq)))
}

## --------------------------- avisos detectados -------------------------------
if (length(avisos)) {
  cat("\n=== AVISOS (", length(avisos), ") ===\n", sep = "")
  print(do.call(rbind, avisos), row.names = FALSE)
} else {
  cat("\n(sem avisos)\n")
}

## ------------- positividade-definida da sequência de autocov [#18] -----------
tab_pd <- do.call(rbind, pd)
cat("\n=== POSITIVIDADE-DEFINIDA das autocovariancias (autovalor min / gama0) ===\n")
cat("   negativo = matriz NAO positiva-semidefinida (variancia negativa implicita)\n\n")
tab_pd$situacao <- ifelse(tab_pd$autoval_min_rel < -1e-10, "NAO-PSD", "ok")
print(tab_pd, row.names = FALSE, digits = 4)
cat("\n  series NAO-PSD:", sum(tab_pd$situacao == "NAO-PSD"), "de", nrow(tab_pd), "\n")

## --------------------- conferência contra o gabarito -------------------------
## O gabarito são os params_*.rds que o Paulo commitou. Na reprodução fiel a
## diferença tem de ser ~0; com fixes, a tabela mostra o tamanho do efeito.
if (CONFERIR && dir.exists(dir_gab)) {
  cat("\n=== CONFERENCIA vs", dir_gab, "===\n")
  linhas <- list()
  for (i in seq_along(REGIOES)) {
    sigla <- REGIOES[[i]]
    arq_g <- file.path(dir_gab, sprintf("%02d_params_%s.rds", i, sigla))
    if (!file.exists(arq_g)) next
    gab <- readRDS(arq_g)
    novo <- resultados[[sigla]]
    for (bloco in c("mod_ar1", "mod_ar2", "mod_ma1", "mod_ma2", "mod_arma11", "mod_ar5")) {
      g <- as.matrix(gab[[bloco]]); n <- as.matrix(novo[[bloco]])
      if (!all(dim(g) == dim(n))) {
        linhas[[length(linhas) + 1]] <- data.frame(
          regiao = sigla, bloco = bloco, dif_abs_max = NA_real_, obs = "dimensoes diferem")
        next
      }
      # NA em ambos conta como igual; NA só num lado é divergência
      amb_na <- is.na(g) & is.na(n)
      um_na  <- xor(is.na(g), is.na(n))
      d <- abs(n - g); d[amb_na] <- 0
      linhas[[length(linhas) + 1]] <- data.frame(
        regiao = sigla, bloco = bloco,
        dif_abs_max = suppressWarnings(max(d, na.rm = TRUE)),
        obs = if (any(um_na)) sprintf("%d parametro(s) viraram/deixaram NA", sum(um_na)) else "")
    }
  }
  conf <- do.call(rbind, linhas)
  conf$dif_abs_max[!is.finite(conf$dif_abs_max)] <- NA
  print(conf, row.names = FALSE, digits = 4)
  pior <- suppressWarnings(max(conf$dif_abs_max, na.rm = TRUE))
  cat("\nMaior diferenca absoluta em qualquer parametro:", format(pior, digits = 6), "\n")
  if (!FIXES) {
    cat(if (is.finite(pior) && pior < 1e-8)
          "OK - reproducao fiel (diferenca numericamente nula).\n"
        else
          "ATENCAO - a reproducao NAO bateu; investigar antes de testar os fixes.\n")
  }
}

cat("\nConcluido.\n")
