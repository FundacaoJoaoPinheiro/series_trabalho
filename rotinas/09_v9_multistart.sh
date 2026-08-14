#!/usr/bin/env bash
# V9 definitivo — ajusta as 18 séries COM a correlação entre ondas, multi-start,
# um processo por série sob `timeout` do SO (o único teto que funciona quando o
# otimizador se perde dentro do filtro de Kalman, que roda em código compilado).
#
# Retomável: série com CSV já gravado é pulada (REFAZER=1 força).
# Uso:  bash rotinas/09_v9_multistart.sh [segundos_por_serie]
set -u
R="/c/Program Files/R/R-4.3.2/bin/Rscript.exe"
TETO="${1:-400}"
mkdir -p outputs/modelos_cor
: > outputs/modelos_cor/falhas.txt

for ind in ocupada desocupada; do
  for i in $(seq 1 9); do
    csv=$(printf "outputs/modelos_cor/%02d_%s.csv" "$i" "$ind")
    if [ -s "$csv" ] && [ "${REFAZER:-0}" != "1" ]; then
      echo "PULA     $ind regiao $i"; continue
    fi
    tmp=$(mktemp)
    timeout "$TETO" "$R" rotinas/09a_uma_serie_cor.R "$i" "$ind" > "$tmp" 2>&1
    rc=$?
    linha=$(grep -E "^OK" "$tmp" | head -1)
    if [ "$rc" -eq 0 ] && [ -n "$linha" ]; then
      echo "$linha"
    elif [ "$rc" -eq 124 ]; then
      echo "TIMEOUT  $ind regiao $i (>${TETO}s)" | tee -a outputs/modelos_cor/falhas.txt
    else
      echo "FALHOU   $ind regiao $i (rc=$rc) $(tail -2 "$tmp" | tr '\n' ' ')" \
        | tee -a outputs/modelos_cor/falhas.txt
    fi
    rm -f "$tmp"
  done
done
echo "--- fim; consolidar com rotinas/09b_consolida.R ---"
