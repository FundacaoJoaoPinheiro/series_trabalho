################################################################################
## V8 — A correlação entre ondas de rotação é estável ao longo do tempo?
##
## O V7 mediu, em UM trimestre (2024Q4) e só para MG, correlação NEGATIVA entre
## as 5 ondas (ρ̄ = −0,236 ocupados, −0,085 desocupados), explicada pelo
## compartilhamento de UPAs. Antes de reescrever o modelo com `V` cheia é preciso
## saber se isso é estável — no tempo e POR REGIÃO, que é o nível em que o modelo
## é ajustado.
##
## Para cada trimestre, extrai de `svyby(..., covmat=TRUE)` os blocos 5×5 de cada
## região e resume a correlação média fora da diagonal.
##
## Uso: Rscript docs/verificacoes/V8_estabilidade_correlacao.R <zip1> <zip2> ...
## Saída: outputs/correlacao_ondas/<periodo>.rds + consolidado ao final.
################################################################################
options(scipen = 999, width = 140)
suppressMessages({ library(PNADcIBGE); library(survey); library(dplyr) })

input <- "data/documentacao/input_PNADC_trimestral.txt"
dir_out <- "outputs/correlacao_ondas"
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

reg8 <- function(e) dplyr::case_when(
  e %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~ "01-Belo Horizonte",
  e %in% c("3120011","3120013","3120020","3120012","3130011","3130012","3130020") ~ "02-Colar e Entorno Metropolitano de BH",
  e %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~ "03-Sul de Minas",
  e %in% c("3152011","3152012","3152013","3152021","3152022") ~ "04-Triângulo Mineiro",
  e %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~ "05-Mata de Minas Gerais",
  e %in% c("3154011","3154012","3154013","3154021","3154022","3154023","3140010","3140020") ~ "06-Norte de Minas",
  e %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~ "07-Vale do Rio Doce",
  e %in% c("3156011","3156012","3156013","3156021","3156022") ~ "08-Central",
  TRUE ~ "99-NAO MAPEADO")

processa <- function(zip) {
  base_n <- sub("_\\d{8}$", "", sub("\\.zip$", "", basename(zip)))
  mesano <- sub("^PNADC_0", "", base_n)
  per <- paste0(substr(mesano, 2, 5), "Q", substr(mesano, 1, 1))
  arq <- file.path(dir_out, paste0(per, ".rds"))
  if (file.exists(arq)) { message("skip ", per); return(invisible(NULL)) }

  td <- file.path(tempdir(), paste0("v8_", mesano)); dir.create(td, showWarnings = FALSE)
  unzip(zip, exdir = td)
  txt <- list.files(td, "[.]txt$", recursive = TRUE, full.names = TRUE)
  txt <- txt[which.max(file.size(txt))]
  des <- pnadc_design(read_pnadc(txt, input, vars = c("V1016", "VD4002"))) %>%
    update(ocupada = 1 * (VD4002 == 1), desocupada = 1 * (VD4002 == 2))
  unlink(td, recursive = TRUE)

  mg <- subset(des, UF == "31")
  mg <- update(mg, regioes = reg8(as.character(Estrato)))

  linhas <- list()
  for (v in c("ocupada", "desocupada")) {
    f <- as.formula(paste0("~", v))

    ## --- MG agregado ---
    by <- svyby(f, ~V1016, mg, svytotal, na.rm = TRUE, covmat = TRUE)
    V <- vcov(by); R <- cov2cor(V)
    tot <- svytotal(f, mg, na.rm = TRUE)
    linhas[[length(linhas)+1]] <- data.frame(
      periodo = per, indicador = v, regiao = "09 - Minas Gerais",
      cor_media = mean(R[upper.tri(R)]),
      razao_indep = sqrt(sum(diag(V))) / as.numeric(SE(tot)),
      razao_full  = sqrt(sum(V)) / as.numeric(SE(tot)))

    ## --- por região: blocos 5x5 do svyby conjunto ---
    byr <- svyby(f, ~regioes + V1016, mg, svytotal, na.rm = TRUE, covmat = TRUE)
    Vr <- vcov(byr)
    lab <- paste(byr$regioes, byr$V1016, sep = "|")
    rownames(Vr) <- colnames(Vr) <- lab
    for (rg in setdiff(unique(byr$regioes), "99-NAO MAPEADO")) {
      idx <- which(byr$regioes == rg)
      if (length(idx) < 2) next
      Vb <- Vr[idx, idx, drop = FALSE]
      Rb <- suppressWarnings(cov2cor(Vb))
      tt <- svytotal(f, subset(mg, regioes == rg), na.rm = TRUE)
      linhas[[length(linhas)+1]] <- data.frame(
        periodo = per, indicador = v, regiao = rg,
        cor_media = mean(Rb[upper.tri(Rb)], na.rm = TRUE),
        razao_indep = sqrt(sum(diag(Vb))) / as.numeric(SE(tt)),
        razao_full  = sqrt(sum(Vb)) / as.numeric(SE(tt)))
    }
  }
  res <- do.call(rbind, linhas)
  saveRDS(res, arq)
  cat(sprintf("OK %s | cor media MG: ocup %+.3f deso %+.3f\n", per,
              res$cor_media[res$regiao == "09 - Minas Gerais" & res$indicador == "ocupada"],
              res$cor_media[res$regiao == "09 - Minas Gerais" & res$indicador == "desocupada"]))
  flush.console()
  invisible(res)
}

args <- commandArgs(trailingOnly = TRUE)
for (z in args) try(processa(z), silent = FALSE)

## ------------------------------- consolidação --------------------------------
f <- list.files(dir_out, "\\.rds$", full.names = TRUE)
if (length(f)) {
  todos <- do.call(rbind, lapply(f, readRDS))
  cat("\n=== ", length(f), " trimestres ===\n", sep = "")

  cat("\n--- MG agregado, por trimestre ---\n")
  m <- todos[todos$regiao == "09 - Minas Gerais", ]
  print(m[order(m$indicador, m$periodo),
          c("periodo","indicador","cor_media","razao_indep","razao_full")],
        row.names = FALSE, digits = 3)

  cat("\n--- resumo por indicador (todas as regioes e trimestres) ---\n")
  for (v in unique(todos$indicador)) {
    s <- todos[todos$indicador == v, ]
    cat(sprintf("  %-11s: cor media %+.3f (dp %.3f, min %+.3f, max %+.3f) | %d obs | negativa em %.0f%%\n",
        v, mean(s$cor_media), sd(s$cor_media), min(s$cor_media), max(s$cor_media),
        nrow(s), 100 * mean(s$cor_media < 0)))
  }

  cat("\n--- por regiao (media entre trimestres) ---\n")
  ag <- aggregate(cor_media ~ regiao + indicador, todos, mean)
  ag <- ag[order(ag$indicador, ag$regiao), ]
  ag$regiao <- substr(ag$regiao, 1, 24)
  print(ag, row.names = FALSE, digits = 3)

  cat("\n--- coerencia: se(T) implicado / se(T) do survey ---\n")
  cat(sprintf("  somando so variancias : media %.2f\n", mean(todos$razao_indep)))
  cat(sprintf("  com covariancia cheia : media %.4f  <- deve ser ~1\n", mean(todos$razao_full)))
}
