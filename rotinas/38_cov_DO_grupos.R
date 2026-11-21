################################################################################
## 38_cov_DO_grupos.R — robustez de 10/11/2026, tarefa 6
##
## O artigo assume Cov(D_L, O_L) = 0 na taxa indireta. A medida direta seria
## design-based nos microdados (svyby(~desocupada+ocupada, ..., covmat = TRUE) +
## svycontrast), mas os microdados da PNAD Contínua NÃO estão nesta máquina
## (E:/Dados/PNADC não montado; C:/Dados/PNADC, usado no 29_, não existe) e não
## há nenhum objeto svydesign salvo no repositório.
##
## APROXIMAÇÃO POR GRUPOS ALEATÓRIOS. Os cinco grupos de visita (V1016) de um
## trimestre são subamostras de UPAs disjuntas do mesmo desenho, e o estimador
## publicado é a soma dos cinco. Se os grupos forem replicações independentes,
## a correlação entre D̂_k e Ô_k dentro do trimestre estima a correlação
## amostral entre D̂ e Ô (somar réplicas i.i.d. não muda a correlação). Usa-se:
##   d_kt = D_kt / média_k(D_kt) − 1 − efeito médio da visita k   (idem para O)
## isto é, desvios relativos ao trimestre, descontado o viés de grupo de rotação
## (que nos ocupados é real e sistemático e criaria correlação espúria). A
## correlação de Pearson de (d, o) empilhada nos 52 trimestres tem 52·4 − 4 g.l.
##
## Limites: (i) os grupos não são estritamente independentes -- compartilham os
## estratos e a calibração dos pesos é feita na amostra inteira; (ii) mede-se a
## correlação dos ESTIMADORES DIRETOS, não das tendências filtradas; (iii) MAIS
## GRAVE: os totais por grupo não são calibrados separadamente, então um grupo
## que cobre mais população tem mais D E mais O -- um fator de tamanho que puxa
## a correlação para cima e que a calibração remove no estimador da amostra
## inteira. Resultado de 10/11/2026: r positivo em 7 de 8 estratos, contra a
## troca de sinal medida nos microdados de 2024. NÃO usar para o sinal.
##
## Saída: outputs/robustez_2026-11-10/cov_DO/
################################################################################
RAIZ  <- Sys.getenv("REPO_RAIZ", unset = getwd())
SAIDA <- file.path(RAIZ, "outputs", "robustez_2026-11-10", "cov_DO")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)
br  <- readRDS(file.path(RAIZ, "baserot8reg.rds"))
REG <- names(br)

desvio <- function(X) {                      # X: T x 5
  R <- X / rowMeans(X) - 1                   # desvio relativo ao trimestre
  sweep(R, 2, colMeans(R))                   # tira o efeito médio de cada visita
}
cor_ic <- function(x, y, gl) {
  r <- cor(x, y); z <- atanh(r); s <- 1 / sqrt(gl - 1)
  c(r = r, li = tanh(z - 1.96 * s), ls = tanh(z + 1.96 * s),
    p = 2 * pnorm(-abs(z / s)))
}
res <- do.call(rbind, lapply(REG, function(r) {
  b <- br[[r]]
  D <- as.matrix(b[, paste0("desocupada_", 1:5)])
  O <- as.matrix(b[, paste0("ocupada_", 1:5)])
  dd <- desvio(D); oo <- desvio(O)
  tot <- cor_ic(as.vector(dd), as.vector(oo), gl = nrow(D) * 4 - 4)
  ## só 2024 (4 trimestres), para comparar com a medida design-based de 2024
  ## registrada no cabeçalho do 24_taxa_final.R (BH -0,25 ... Vale +0,27)
  i24 <- which(substr(b$periodo, 1, 4) == "2024")
  r24 <- cor(as.vector(desvio(D[i24, ])), as.vector(desvio(O[i24, ])))
  ## sem descontar o efeito de visita (para mostrar o quanto o RGB pesa)
  r_bruto <- cor(as.vector(D / rowMeans(D) - 1), as.vector(O / rowMeans(O) - 1))
  data.frame(estrato = r, t(round(tot, 3)), r_sem_desconto_rgb = round(r_bruto, 3),
             r_2024 = round(r24, 3), check.names = FALSE)
}))
write.csv(res, file.path(SAIDA, "correlacao_D_O_grupos.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
options(width = 150)
cat("Correlação amostral entre D̂ e Ô por grupos aleatórios (5 visitas × 52 trimestres)\n\n")
print(res, row.names = FALSE)
s <- res[1:8, ]
cat("\n8 estratos: r mediano", round(median(s$r), 3), "| faixa", min(s$r), "a", max(s$r),
    "| negativos:", sum(s$r < 0), "| significativos a 5%:", sum(s$p < 0.05), "\n")
