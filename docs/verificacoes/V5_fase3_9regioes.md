# V5 — Fase 3: modelo com efeito de painel nas 9 regiões

**Data:** 2026-08-01 · **Motor:** `funcoes/41_modelo_rgb_coorte.R` ·
**Execução:** `rotinas/07_modelos_coorte.sh` + `rotinas/07a_uma_serie.R` ·
**Resultados:** `outputs/modelos_coorte/` (17 séries + `resumo.csv`)

Base: `baserot8reg.rds` (vintage do artigo). **8 regiões + MG = 9 séries** × 2 indicadores.
Os 10 estratos oficiais viram 8 por duas fusões documentadas no manuscrito (RIDE→Norte de
Minas; Colar+Entorno) — código e texto conferidos, batem.

## Resultado

| indicador | n | efeito de painel selecionado | ρ médio | ruído médio | **ganho de CV** |
|---|---|---|---|---|---|
| **ocupados** | 9/9 | **9 de 9** | 0,987 | 20% | **58,6%** |
| **desocupados** | 8/9 | **2 de 8** | 0,800 | 28% | 20,2% |

**Zero séries não comparáveis** — em todas, as duas variantes convergiram, então cada
seleção é uma comparação de fato (a regra do V1 §1.1 foi respeitada).

### Ocupados — confirmação independente do V2

O efeito de painel vence em **todas** as 9 séries, com ΔAIC de +2,9 a +119,4, e ρ
notavelmente estável (0,980–0,992). O CV cai de 8,3–11,3% para 3,6–4,5%.

Isto reproduz, por uma via inteiramente distinta (máxima verossimilhança dentro do modelo
em espaço de estados), o que o V2 mediu com índice de Bailar e teste F: **rotation group
bias existe nos ocupados**.

### Desocupados — e por que a divergência com o V2 é aparente

O V2 não achou RGB em nenhuma série de desocupados (0/9 no teste F). Aqui o AIC seleciona
o efeito em 2 de 8 — Belo Horizonte e Colar/Entorno. **Não é contradição: é poder.**
Essas duas são exatamente as séries de MENOR ruído entre os desocupados (25,4% e 25,5%,
contra 29–34% nas demais).

### O ruído governa tudo

| | n | ruído médio | ganho de CV médio |
|---|---|---|---|
| selecionou efeito de painel | 11 | **21,1%** | **53,5%** |
| não selecionou | 6 | **29,0%** | 16,6% |

Correlação entre ruído e ganho de CV: **−0,575**.

Ou seja: quanto maior o erro amostral relativo, menos o modelo consegue extrair — tanto
em detectar o efeito de painel quanto em ganhar precisão. O método entrega muito onde o
dado permite (ocupados, ganho de ~59%) e pouco onde não permite (desocupados de regiões
pequenas, ganho de 5–20%).

### A série que não ajusta

**Triângulo Mineiro / desocupados não converge** em 400 s por variante (travou também as
duas tentativas anteriores, consumindo 8,3 h e 1,6 h de CPU). É a única das 18 sem
resultado. Não é bug: nessas séries o erro-padrão chega a **60% do próprio valor**, e a
verossimilhança fica mal-condicionada. É um limite do dado, e deve ser reportado como tal
— na mesma linha do que o manuscrito já faz ao fundir RIDE e Colar por imprecisão.

## Três armadilhas de execução (custaram ~10 h de CPU)

Registradas porque qualquer um que rode isto de novo vai esbarrar nelas:

1. **`setTimeLimit` não interrompe código compilado.** O filtro de Kalman roda em C e o R
   só checa o limite ao voltar ao interpretador — que não acontece durante o ajuste. Teto
   de tempo imposto de dentro do R é inútil aqui. **Solução:** um processo por série, com
   `timeout` do sistema operacional (é o que `07_modelos_coorte.sh` faz).
2. **`timeout cmd | grep` devolve o status do `grep`.** Todo estouro de tempo se
   disfarçava de "rc=1", e o log mentia sobre a causa — quase me levou a concluir que o
   modelo não ajustava em Belo Horizonte, quando ajusta bem (ρ = 0,982). **Solução:**
   redirecionar para arquivo e ler o status do `timeout` direto.
3. **Matar o job não mata os filhos.** Duas rodadas ficaram vivas em paralelo disputando
   CPU. Conferir os processos antes de relançar.

Reduzir `maxit` de 1e5 para 200 **não** resolveu — o problema não era o número de
iterações, e sim a superfície mal-condicionada.

## Pendências

1. **Triângulo Mineiro / desocupados** — tentar reparametrização ou aceitar como limite.
2. **Taxa de desocupação** — `tx_rot8reg.rds` já traz `txdesoc_1..5` e `se_txdesoc_1..5`,
   então o mesmo motor se aplica direto (não é preciso escolher entre aproximação linear e
   modelar numerador/denominador).
3. **Multimodalidade** — `spread_ll` chega a 152 entre pontos de partida. O multi-start
   não é refinamento, é requisito; e vale reportar no artigo que a estimação exige isso.
4. **Comparar com os resultados atuais do artigo** — o ganho de CV aqui (58,6% nos
   ocupados) precisa ser confrontado com o que os modelos com pseudo-erros entregam.

## Como reproduzir

```
bash rotinas/07_modelos_coorte.sh 400      # retomável; pula séries já ajustadas
REFAZER=1 bash rotinas/07_modelos_coorte.sh 400   # força tudo de novo
```
