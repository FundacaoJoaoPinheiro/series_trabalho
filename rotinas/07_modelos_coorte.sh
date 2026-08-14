#!/usr/bin/env bash
# 07 - Fase 3: ajusta as 18 séries (9 regiões x 2 indicadores), uma por processo.
#
# Cada série roda em processo próprio sob `timeout`, porque o otimizador pode se
# perder dentro do filtro de Kalman (código compilado) em séries muito ruidosas,
# e nesse caso NENHUM teto imposto de dentro do R funciona. Ver 07a_uma_serie.R.
#
# Uso:  bash rotinas/07_modelos_coorte.sh [segundos_por_serie]
set -u
R="/c/Program Files/R/R-4.3.2/bin/Rscript.exe"
TETO="${1:-150}"
mkdir -p outputs/modelos_coorte/series
: > outputs/modelos_coorte/falhas.txt

for ind in ocupada desocupada; do
  for i in $(seq 1 9); do
    # retomada: série já ajustada não roda de novo (use --refazer para forçar)
    csv=$(printf "outputs/modelos_coorte/series/%02d_%s.csv" "$i" "$ind")
    if [ -s "$csv" ] && [ "${REFAZER:-0}" != "1" ]; then
      echo "PULA     $ind regiao $i (ja ajustada)"
      continue
    fi

    # ATENÇÃO: num pipe, `$?` é o status do ÚLTIMO comando (o grep), não do
    # `timeout`. Sem capturar o status do timeout separadamente, todo estouro de
    # tempo se disfarça de "grep não encontrou nada" (rc=1) e o log mente sobre
    # a causa. Grava a saída num arquivo e lê o status do timeout direto.
    tmp=$(mktemp)
    timeout "$TETO" "$R" rotinas/07a_uma_serie.R "$i" "$ind" > "$tmp" 2>&1
    rc=$?
    linha=$(grep -E "^OK|^SEM_AJUSTE" "$tmp" | head -1)
    if [ "$rc" -eq 0 ] && [ -n "$linha" ]; then
      echo "$linha"
    elif [ "$rc" -eq 124 ]; then
      echo "TIMEOUT  $ind regiao $i (>${TETO}s)" | tee -a outputs/modelos_coorte/falhas.txt
    else
      echo "FALHOU   $ind regiao $i (rc=$rc) $(tail -2 "$tmp" | tr '\n' ' ')" \
        | tee -a outputs/modelos_coorte/falhas.txt
    fi
    rm -f "$tmp"
  done
done

echo "--- consolidando ---"
"$R" -e '
f <- list.files("outputs/modelos_coorte/series", "\\.csv$", full.names=TRUE)
if (!length(f)) { cat("nenhuma serie ajustada\n"); quit() }
res <- do.call(rbind, lapply(f, read.csv))
write.csv(res, "outputs/modelos_coorte/resumo.csv", row.names=FALSE)
cat("\n=== RESUMO (", nrow(res), "de 18 series ) ===\n")
print(res[order(res$indicador, res$regiao), ], row.names=FALSE)
cat("\n--- efeito de painel selecionado ---\n")
for (i in unique(res$indicador)) {
  s <- res[res$indicador==i & res$comparavel, ]
  cat(sprintf("  %-11s: %d de %d comparaveis (deltaAIC medio %+.1f) | rho medio %.3f\n",
      i, sum(s$escolhido=="fixo"), nrow(s), mean(s$delta_aic_lambda, na.rm=TRUE),
      mean(res$rho[res$indicador==i])))
}
cat(sprintf("\nganho medio de CV: %.1f%%\n", mean(res$ganho_cv_pct)))
'
