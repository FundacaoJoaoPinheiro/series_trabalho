# V3 — Prova de conceito do modelo com efeito de painel (caminho C)

**Data:** 2026-07-31 · **Motor:** `funcoes/40_modelo_rgb_multivariado.R` ·
**Teste:** `docs/verificacoes/V3_poc_modelo_rgb.R` · **Série:** Minas Gerais / ocupados

## O que muda em relação ao modelo atual

O modelo atual observa **uma** série trimestral agregada e usa todo o aparato dos
pseudo-erros para produzir **um escalar**: a autocorrelação do erro amostral, que entra
fixa em `m$GG[6,6]`. Só a variância `W[6,6]` é estimada.

O modelo novo observa as **5 ondas de rotação** como vetor:

> y(j,t) = μ(t) + γ(t) + λ(j,t) + e(j,t),  j = 1..5 (V1016), Σⱼ λ = 0

| | modelo atual | modelo com efeito de painel |
|---|---|---|
| observações | 52 | 52 × 5 = **260** |
| variância amostral `V` | estimada (1 parâmetro) | **conhecida** — `se` design-based por onda |
| autocorrelação do erro | dos **pseudo-erros**, fixada | dispensada |
| efeito de painel | ignorado (vai para o erro) | **estimado** (λ) |
| hiperparâmetros | 5 | **3** |

Ou seja, o caminho C **elimina os pseudo-erros do pipeline** — e com eles o M1 (a
justificativa frágil para usá-los) e o M2 (o viés que eles não capturam).

## Validação

Critérios definidos **antes** de rodar, para não virar racionalização:

| | critério | resultado |
|---|---|---|
| V-1 | converge | **OK** |
| V-2 | λ reproduz o índice de Bailar medido em V2 | **OK — r = 0,9997** |
| V-3 | o efeito de painel é necessário | **OK — ΔAIC = 13,4** contra o modelo sem λ |
| V-4 | o efeito deriva no tempo | **NÃO — é constante** (refuta V2 §3) |
| V-5 | sinal próximo do agregado design-based | **OK** — dif. média −0,6%, máx. 5,3% |

**V-2 é o resultado forte.** O λ estimado dentro do modelo reproduz o índice de Bailar
medido de forma totalmente independente em V2:

| | ent1 | ent2 | ent3 | ent4 | ent5 |
|---|---|---|---|---|---|
| índice implicado pelo modelo | 103,64 | 101,03 | 99,58 | 98,54 | 97,20 |
| índice medido nos dados (V2) | 103,70 | 101,13 | 99,63 | 98,44 | 97,09 |

Correlação 0,9997. O modelo está capturando o efeito real, não um artefato numérico.

**Ganho de precisão:** o CV médio do sinal cai de **3,45%** (design-based agregado) para
**1,77%** — que é exatamente o objetivo de estimação em pequenos domínios do artigo.

## Dois erros cometidos no caminho (registrados para não se repetirem)

1. **`diag(W)[6:p] <- x` não funciona em R.** Modifica uma cópia da diagonal e a
   descarta, deixando `W` zerado em silêncio. Sintoma: `rw` e `fixo` com
   log-verossimilhança *idêntica*. Corrigido com indexação matricial. *(É o mesmo tipo de
   falha silenciosa que motivou os issues #10/#18 no código do Paulo.)*
2. **Verossimilhança cheia não compara modelos com números diferentes de estados
   difusos.** Os 4 estados λ entram com `C0 = 1e7`, e o custo da difusão inicial recai
   sobre o modelo *com* efeito de painel. Com a verossimilhança cheia, o modelo sem λ
   "vencia" por 20 pontos de AIC; com a verossimilhança pós-difusão (descartando 8
   trimestres), **perde por 13,4**. A conclusão se inverte por inteiro.
   `loglik_pos_difusao()` passou a ser o padrão em `funcoes/40_`.

## FASE 2 — CONCLUÍDA: erro amostral correlacionado na coorte

Motor: `funcoes/41_modelo_rgb_coorte.R` · Teste: `docs/verificacoes/V4_fase2_coorte.R`

A pendência principal da fase 1 era o erro amostral independente. O mesmo domicílio
entrevistado na onda *j* do trimestre *t* reaparece na onda *j+1* do trimestre *t+1*, o
que vira um AR(1) sobre a diagonal da coorte:

> a(1,t) = η(1,t)  *(coorte nova)* · a(j,t) = **ρ**·a(j−1,t−1) + η(j,t)  *(mesma coorte)*

com `a` padronizado e a heterocedasticidade entrando por se(j,t) na matriz de observação.
Assim **ρ é diretamente a correlação do erro amostral entre entrevistas consecutivas do
mesmo domicílio** — o que os pseudo-erros estimavam por fora, aqui por máxima
verossimilhança junto com o resto.

| | fase 1 (erro independente) | **fase 2 (erro na coorte)** |
|---|---|---|
| hiperparâmetros | 3 | 4 |
| loglik pós-difusão | −1374,67 | **−1190,45** |
| AIC | 2755,4 | **2388,9** (ΔAIC = **−366**) |
| CV do sinal | 1,77% | **1,66%** (design-based: 3,45%) |
| ρ | — | **0,979** |

**ρ = 0,979** é altíssimo e é o resultado teoricamente esperado: enquanto a amostra não
muda, o erro amostral do painel é quase perfeitamente persistente — é o mesmo conjunto de
domicílios. Verificado que é **máximo interior**, não deriva para a fronteira (a
verossimilhança cai para −1305 em ρ=0,995 e −1207 em ρ=0,95), então a especificação está
identificada e o erro amostral não está competindo com o nível pela baixa frequência.

**O λ é robusto:** entre as duas especificações do erro amostral, r = 0,9995 e diferença
máxima de 2,4 mil pessoas. O achado de rotation group bias não depende de como o erro
amostral é modelado.

## Pendências

1. **A tendência no pseudo-erro (V2 §3.2) segue sem explicação.** 31 de 45 séries dos
   ocupados, R² ≈ 0,21. Não é o RGB, que é constante. Fica em aberto — e deixa de ser
   crítico se os pseudo-erros saírem do pipeline.
3. **Taxa de desocupação.** É razão, não total; a especificação precisa ser pensada
   (aproximação linear ou modelar numerador e denominador conjuntamente).

## Fases seguintes

- **Fase 2** — tratar a pendência 1; estender às 9 regiões × ocupados/desocupados.
- **Fase 3** — taxa de desocupação.
- **Fase 4** — reanálise e reescrita das seções de método/resultados.

## Como reproduzir

```
Rscript docs/verificacoes/V3_poc_modelo_rgb.R
```
