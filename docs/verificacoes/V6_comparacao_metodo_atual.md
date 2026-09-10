# V6 — Comparação com o método atual: o ganho de precisão NÃO se sustenta

> ## ⚠️ RETIFICAÇÃO (V7, 2026-08-01)
> **O §2 abaixo está ERRADO.** Os `se` por onda NÃO são inconsistentes: as ondas são
> **negativamente correlacionadas** (ρ̄ = −0,236 nos ocupados), porque compartilham UPAs.
> Com a matriz de covariância completa, o total implicado pelas ondas reproduz o do
> `survey` **exatamente** (razão 1,00). Ver `V7_auditoria_se_rotacao.md`.
>
> **A conclusão do documento permanece** — o ganho de precisão não se sustenta —, mas a
> causa é outra: a linha de base do V5 assumia independência entre ondas e por isso
> superestimava. E o modelo `41_` tem o mesmo defeito (`V = diag(se²)`), o que o torna
> **mal especificado**. Corrigível: ver V7.

**Data:** 2026-08-01 · **Script:** `docs/verificacoes/V6_comparacao_metodo_atual.R`

## Resumo

A comparação direta parecia espetacular — e é justamente por isso que foi verificada.
**O ganho de precisão do modelo novo é artefato de uma linha de base inflada.** Os
erros-padrão por onda de rotação são inconsistentes com o erro-padrão do total, e são
eles que alimentam o `V` do modelo e a referência contra a qual eu comparava.

> **Não usar os números de ganho de CV do V5 (58,6% / 20,2%) nem os de RRSE deste
> documento (80% / 86%).** Ambos dependem da mesma referência viciada.

## O que apareceu primeiro

RRSE (redução relativa média do erro-padrão vs estimativa direta, a métrica que o artigo
publica em `diffvicio*.tex`):

| indicador | atual univ. | atual mult. | "novo" | vence |
|---|---|---|---|---|
| ocupados | 31,6% | 23,8% | **80,3%** | 7/7 |
| desocupados | 40,5% | 40,4% | **86,1%** | 3/3 |

E o caso mais chamativo: Belo Horizonte/ocupados, onde o método atual **piora** a precisão
(−17,5% univariado, −21,0% multivariado — anomalia que o manuscrito registra na seção de
resultados), aparecia com **+58,7%** no método novo.

Bom demais. Foi verificado, e caiu.

## Por que caiu

### 1. Cada onda estima 1/5 da população, não o total

A razão entre a média das 5 ondas e o total do `baseestr8reg` é **exatamente 0,200** em
todas as 18 séries. O `svyby(~ocupada, ~regioes + V1016, svytotal)` soma os pesos dos
indivíduos daquele grupo de rotação sem recalibrar para o subdomínio — então `ocupada_j`
é o total capturado pela onda *j*, ≈ T/5.

Isso, isolado, não seria problema (CV é invariante à escala). O problema é o seguinte.

### 2. Os erros-padrão por onda são inconsistentes com o do total

Se as 5 ondas são amostras disjuntas dentro do trimestre, somá-las dá o total e
Var(T) = Σⱼ Var(ondaⱼ), ou seja se(T) = √(Σ se²). Comparando esse valor implicado com o
`sd` que o `survey` calcula da amostra completa:

| indicador | se(T) implicado ÷ sd(T) do survey |
|---|---|
| **ocupados** | **2,2 a 4,8** |
| desocupados | 1,25 a 1,49 |

Os erros-padrão por onda implicam um total **2 a 5 vezes menos preciso** do que o survey
calcula. Alguma das duas quantidades está errada — provavelmente a variância do subdomínio
(poucas UPAs por grupo de rotação, e a calibração/pós-estratificação que beneficia a
amostra completa não se propaga proporcionalmente aos subdomínios).

### 3. Consequência: a linha de base estava inflada

O `cv_design` usado como referência no V5 foi construído **a partir desses mesmos `se` por
onda** (`sqrt(rowSums(se²))/5 ÷ média das ondas`). Para Belo Horizonte/ocupados isso dá
**8,33%**, enquanto o CV design-based real do total é **1,8%**.

Ou seja, o modelo foi comparado contra uma referência ~4,6× pior que a verdadeira. Contra
o CV real:

| | CV |
|---|---|
| design-based real (total, survey) | **1,8%** |
| modelo novo | 3,84% |

**O modelo é pior, não melhor.** A conclusão inverte.

## O que permanece válido

O achado de **rotation group bias não é afetado**. Ele vem das estimativas **pontuais**
por onda (índice de Bailar, V2) e do λ estimado (V3/V5) — nenhum dos dois depende dos
erros-padrão por onda. Continuam de pé:

- RGB monotônico e significativo nos ocupados (8/9 regiões, V2);
- λ estimado reproduzindo o índice de Bailar com r = 0,9997 (V3);
- efeito de painel selecionado em 9/9 ocupados por AIC (V5);
- ρ ≈ 0,98 nos ocupados, ≈ 0,80 nos desocupados (V5).

O que cai é exclusivamente a alegação de **ganho de precisão**.

## O que fazer antes de qualquer número de precisão

1. **Investigar a estimação dos `se` por grupo de rotação** (rotina 05 / script 5 e 14).
   Verificar como o `pnadc_design` trata subdomínios e se a variância por V1016 está sendo
   calculada corretamente — inclusive contra o que o IBGE publica.
2. **Decidir o que alimenta o `V` do modelo.** Se os `se` por onda não são confiáveis, o
   modelo com `V` conhecida perde sua principal vantagem sobre os pseudo-erros. Uma
   alternativa é calibrar os `se` por onda para que reproduzam o `sd` do total.
3. **Refazer a comparação** só depois de (1) e (2), e sempre contra o CV design-based do
   **total** (`baseestr8reg`), nunca contra uma referência derivada dos `se` por onda.

## Nota de método

Este erro sobreviveria a qualquer revisão que olhasse só o código: os cálculos estão
corretos, as métricas são as do artigo, e o resultado é internamente consistente. O que o
pegou foi desconfiar de um resultado **bom demais** e testar a coerência entre duas
quantidades que deveriam bater (`se` por onda e `sd` do total). Vale como regra para o
resto do trabalho.
