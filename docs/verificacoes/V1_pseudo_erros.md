# V1 — Verificação dos pseudo-erros (script 15) e dos issues #10 e #18

**Data:** 2026-07-31 · **Rotina:** `rotinas/06_pseudo_erros.R` · **Testes:** `docs/verificacoes/test_pd_autocov.R`

Insumo: `basealinhada_8reg.rds` **commitada pelo Paulo** (vintage do artigo), não a
reproduzida em `data/` (vintage 2025, ~1% mais baixa). Assim o efeito de cada fix fica
isolado do efeito da recalibração pós-Censo 2022.

---

## 0. Reprodução fiel — OK

`rotinas/06_pseudo_erros.R` consolida as 2.539 linhas do `15_Erro Amostral 8reg.R`
(um bloco copiado-colado por região) num único passe sobre as 9 séries, e reproduz os
9 arquivos `pseudoerros_8reg/NN_params_*.rds` commitados:

> **maior diferença absoluta em qualquer um dos 54 blocos de parâmetros: 2,8 × 10⁻¹⁵**

Isto é ruído de ponto flutuante. A reprodução é fiel, então tudo o que segue mede o
efeito dos fixes, não diferença de implementação.

---

## 1. Issue #10a — MA(1) inviável (|rho1| > 0.5)

`theta1 = (1 - sqrt(1 - 4·rho1²)) / (2·rho1)` só tem raiz real quando **|rho1| ≤ 0,5**
(o limite teórico da autocorrelação de lag 1 de um MA(1)). Acima disso o R devolve
`NaN` **em silêncio**, sem erro nem aviso.

**Ocorre em 10 dos 18 casos** (9 regiões × 2 indicadores):

| série | rho1 | usado no modelo? |
|---|---|---|
| ocupados — Belo Horizonte | 0,6603 | **não** |
| ocupados — Colar e Entorno | 0,6683 | **não** |
| ocupados — Sul de Minas | 0,7313 | **não** |
| ocupados — Triângulo Mineiro | 0,7762 | **não** |
| ocupados — Mata de Minas | 0,6940 | **não** |
| ocupados — Norte de Minas | 0,6874 | **não** |
| ocupados — Vale do Rio Doce | 0,7831 | **não** |
| ocupados — Central | 0,6495 | **não** |
| ocupados — Minas Gerais | 0,6044 | **não** |
| **desocupados — Vale do Rio Doce** | **0,5601** | **ver §1.1** |

### Impacto real — menor do que parece, mas há uma omissão metodológica

- **Ocupados (9 casos): inócuo.** O `20_Modelo Estrutural univariado 8 reg - Ocupados.R`
  **não ajusta MA(1)** (zero ocorrências de `theta1_ma1`). O parâmetro é calculado, salvo
  e nunca lido. Nenhum número do artigo é afetado.
- **Desocupados — Vale do Rio Doce (1 caso): ver abaixo.**

### 1.1 O achado que importa: o Vale do Rio Doce ficou com uma só especificação

No `17_Estrutural univariado 8 reg - Desocupados.R`, o **Vale do Rio Doce (`dbvl`) é a
única das 9 regiões sem `theta1_ma1`** — e é exatamente a única região cujo MA(1) de
desocupados é inviável. Somando as demais especificações:

| região | AR(1) | MA(1) | ARMA(1,1) | nº de candidatos |
|---|:--:|:--:|:--:|:--:|
| Colar e Entorno, Sul de Minas, Mata, Norte de Minas, Minas Gerais | sim | sim | sim | 3 |
| Belo Horizonte, Triângulo Mineiro, Central | sim | sim | — | 2 |
| **Vale do Rio Doce** | sim | — | — | **1** |

Ou seja: o `NaN` não produziu número errado — ele **eliminou silenciosamente as
alternativas** para uma região. O Vale do Rio Doce não teve seleção de modelo: ficou com
o AR(1) por ausência de concorrentes, enquanto as outras regiões compararam 2 ou 3.

**Isto é um ponto de manuscrito, não só de código.** O artigo precisa ou (a) declarar
que para o Vale do Rio Doce a especificação MA(1) do erro amostral é inviável porque a
autocorrelação estimada de lag 1 (0,56) excede o limite teórico do MA(1), e que por isso
a região só admitiu AR(1); ou (b) uniformizar o conjunto de candidatos entre regiões.
Como está, a seleção de modelos não é comparável entre estratos e nada disso é dito.

**Fix recomendado:** guarda explícita (`|rho1| > 0.5` → `NA` + aviso), como implementado
sob `--fix10`. Custo zero, e transforma uma falha silenciosa em decisão documentada.

---

## 2. Issue #10b — MA(2) sem convergência

No **Vale do Rio Doce**, o `nleqslv` do sistema MA(2) **não converge**
(`termcd = 3`, ||F|| = 0,0296 — parou sem reduzir o resíduo). O script original usa
`solucao$x` sem checar `termcd`, gravando parâmetros espúrios.

**Impacto: nenhum.** `mod_ma2` **não é lido por nenhum script a jusante** (zero
ocorrências no `17_...Desocupados.R`). A especificação MA(2) foi calculada mas nunca
ajustada.

**Recomendação:** manter a guarda de convergência (barata, evita bug latente), mas
**não** apresentar isto ao Paulo como correção de resultado — não é. Vale mais perguntar
se o `mod_ma2` deve continuar sendo calculado.

---

## 3. Issue #18 — Pcov2 (divisor T−i, média por sublag) e positividade-definida

A `Pcov2` usa divisor `(T−i)` e **recalcula a média em cada sublag**. Não é o estimador
amostral usual de autocovariância (divisor `T`, média única), que é o que garante
sequência positiva-semidefinida.

### A alegação de não-PSD só se confirma na dimensão certa

| matriz testada | Pcov2 (original) | estimador usual |
|---|:--:|:--:|
| **25 × 25** (só os lags 0–24 estimados) | 0/18 não-PSD | 0/18 não-PSD |
| **52 × 52** (T trimestres, lags > 24 truncados em zero) | **11/18 não-PSD** | **2/18 não-PSD** |

Nos lags efetivamente estimados a matriz é positiva-definida nas **duas** versões — a
alegação, como formulada no issue, não se sustenta aí. O problema aparece na matriz de
dimensão T, e o estimador usual reduz as violações de 11 para 2 (restam ocupados da Mata
de Minas e do Vale do Rio Doce, resíduo do truncamento, que o fix não resolve).

### Impacto prático: indireto

A matriz empírica de covariância **não entra no modelo**. O
`funcoes/03_modelo_bsm_error.R` recebe apenas **escalares** (`m$GG[6,6] <- par_ar_erro`)
e os erros-padrão como regressor (`dlmModReg(se_db)`). A sequência de autocovariâncias é
usada só para derivar `phi`/`theta` dos lags 1–5. Logo a não-PSD **não corrompe** as
estimativas publicadas.

O que o fix muda são os parâmetros derivados: **até 0,047 em valor absoluto**
(≈ 1–4% relativo), propagando-se a todas as especificações porque altera a FAC.

**Recomendação:** trocar por `Pcov` (estimador usual) é **defensável e barato** — é o
estimador padrão, e reduz as violações de PSD. Mas o issue deve ser **reescrito**: a
justificativa correta não é "corrige a matriz do modelo" (a matriz não entra), e sim
"usa o estimador consistente e padrão da literatura, em vez de uma variante ad hoc".
E o efeito nos resultados precisa ser medido rodando os modelos, não apenas os
parâmetros — **pendente** (§5).

---

## 4. Zeros incoerentes (parte do #10)

**Não ocorrem** na `basealinhada_8reg.rds`: nenhuma célula com estimativa zero nos
grupos de rotação, em nenhuma das 9 regiões. O `replace(. == 0, NA)` do script original
é, nesta base, um no-op. **Falta verificar** na base de 10 estratos
(`basealinhada0424.rds`), onde os domínios são menores.

---

## 5. Pendente

1. **Medir o efeito do #18 nos resultados publicados** — rodar os modelos (scripts 17/20)
   com os parâmetros das duas versões e comparar as séries estimadas, não só os `phi`/`theta`.
   É o único jeito de dizer se a mudança altera alguma conclusão do artigo.
2. **Zeros na base de 10 estratos** (§4).
3. **Reescrever os issues #10b e #18** no GitHub com o impacto real apurado aqui, para não
   levar ao Paulo correções cuja justificativa não se confirma.
4. **Levar o §1.1 ao manuscrito** — é o achado com consequência editorial.

---

## Como reproduzir

```
Rscript rotinas/06_pseudo_erros.R                  # reprodução fiel + conferência
Rscript rotinas/06_pseudo_erros.R --fix10          # só as guardas de viabilidade
Rscript rotinas/06_pseudo_erros.R --fix18          # só o estimador usual
Rscript rotinas/06_pseudo_erros.R --fixes          # os dois
Rscript docs/verificacoes/test_pd_autocov.R        # PSD: 25x25 vs 52x52
```

Requer `nleqslv` (não vinha instalado; binário para R 4.3 em
`https://cran.r-project.org/bin/windows/contrib/4.3/nleqslv_3.3.5.zip`).
