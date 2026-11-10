#!/usr/bin/env bash
# 35_robustez_rodadas.sh — rodadas de robustez de 10/11/2026 (tarefas 4 e 5)
#
#   por_estrato/  tendência escolhida caso a caso pelo BIC (TENDENCIA=por_estrato),
#                 univariado + multivariado, com o multi-start do Cholesky que já
#                 está no 23_ (commit 41ca2f8)
#   ct_0.8/ ct_1.2/  rodada C com c_t multiplicado por 0,8 e 1,2: univariado dos
#                 três indicadores e multivariado de desocupados
#   ct_1.0/       controle: a mesma máquina com fator 1 deve reproduzir a rodada C
#
# RAM limitada: no máximo 4 processos R simultâneos no total da máquina, por isso
# os multivariados são lançados um a um (cada chamada é um processo), sob teto
# de TETO_MV segundos (timeout do SO).
# Uso (da raiz do repositório):
#   bash rotinas/35_robustez_rodadas.sh uni                    # os 4 univariados, em paralelo
#   bash rotinas/35_robustez_rodadas.sh mv por_estrato desocupados
set -u
R="/c/Program Files/R/R-4.3.2/bin/Rscript.exe"
B=outputs/robustez_2026-11-10
TETO_MV="${TETO_MV:-9000}"
mkdir -p "$B/logs"

uni() {  # $1 = subdir  $2 = escala  $3 = tendencia
  mkdir -p "$B/$1/univariado"
  ETAPA=uni SAIDA_UNI="$B/$1/univariado" ESCALA_CT="$2" TENDENCIA="$3" \
    "$R" --vanilla rotinas/34_rodada_variante.R > "$B/logs/uni_$1.txt" 2>&1
  echo "[$(date +%H:%M)] uni $1 rc=$?"
}
mv() {   # $1 = subdir  $2 = indicador
  mkdir -p "$B/$1/multivariado"
  ETAPA=mv SAIDA_UNI="$B/$1/univariado" SAIDA_MV="$B/$1/multivariado" INDICADOR="$2" \
    timeout "$TETO_MV" "$R" --vanilla rotinas/34_rodada_variante.R > "$B/logs/mv_$1_$2.txt" 2>&1
  echo "[$(date +%H:%M)] mv $1 $2 rc=$? (124 = timeout)"
}

case "${1:-}" in
  uni)
    uni por_estrato 1 por_estrato &
    uni ct_0.8 0.8 "" &
    uni ct_1.2 1.2 "" &
    uni ct_1.0 1 "" &
    wait ;;
  mv) mv "$2" "$3" ;;
  *) echo "uso: $0 uni | mv <subdir> <indicador>"; exit 1 ;;
esac
