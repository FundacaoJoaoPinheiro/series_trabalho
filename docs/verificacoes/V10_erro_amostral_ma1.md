# V10 — O termo MA do erro amostral estava inerte; efeito nos ganhos de precisão

**Data:** 2026-08-14 · **Scripts:** `rotinas/10_univariado_corrigido.R`, `rotinas/11_multivariado_cholesky.R`
**Issues:** #20 (nova), #1, #2, #11, #17
**Dados:** `baseestr8reg.rds` e `pseudoerros_8reg/` **da raiz** (vintage do artigo; a cópia em
`data/` é o vintage revisado pós-Censo 2022 e não reproduz o artigo — ver `data/COMO_FORAM_GERADOS.md`)

---

## 1. O bug

Em `funcoes/14_estrutural_MA1.R` e `15_estrutural_ARMA11.R` (e nos multivariados 25/27/30/31/39),
o erro amostral com termo MA usa um 7º estado auxiliar com `W[7,7] <- 0` e linha 7 de `GG` nula.
O estado auxiliar fica identicamente zero, e `alpha_6,t = theta*alpha_7,t-1 + w_6,t = w_6,t`:
**o termo MA nunca entra**. O erro amostral colapsa para ruído branco.

Verificação por simulação da recursão de estados (theta = 0,3531, BH-desocupados):

| estrutura | var(e_t) | acf(1) |
|---|---|---|
| atual (`W[7,7]=0`) | 1,003 | **−0,003** |
| corrigida (`w6 = w7 = xi_t`) | 1,127 | **0,318** |
| teórico MA(1) | 1,125 | **0,314** |

A autocorrelação identificada pelos pseudo-erros simplesmente não chegava ao filtro. Ou seja: a
etapa de identificação do processo ARMA — que é o núcleo metodológico do artigo — não estava
sendo usada na estimação para os estratos com termo MA.

**Correção (forma de Harvey):** a mesma inovação alimenta os dois estados.
```r
s2 <- exp(params[5])
W[6,6] <- s2; W[7,7] <- s2; W[6,7] <- s2; W[7,6] <- s2
```
Serve para MA(1) (`phi = 0`) e para ARMA(1,1) sem alteração.

### Alcance

- **Desocupados:** 6 dos 8 estratos são MA(1) e o Sul é ARMA(1,1) → 7 de 8 afetados.
  Só o Vale do Rio Doce (AR(1)) estava correto.
- **Ocupados:** todos AR(1) → **nenhum** afetado.
- O `12_estrutural_AR1.R` não usa estado auxiliar e sempre esteve correto.

### Controle da verificação

A reestimação confirma o alcance previsto exatamente: em **ocupados**, legado e corrigido dão
resultados idênticos nos 8 estratos; em **desocupados**, o Vale do Rio Doce (único AR(1)) é o
único idêntico. Nenhum outro resultado poderia produzir esse padrão por acaso.

### Interação com a issue #1

A #1 (BH-desocupados recebia `phi1_ar1` no lugar de `theta1_ma1`) estava **mascarada** pela #20:
o coeficiente errado não fazia diferença porque o coeficiente não era usado. Com a #20 corrigida,
a #1 passa a importar. As duas se corrigem juntas.

---

## 2. Efeito nos ganhos de precisão — desocupados

Diferença relativa média do erro-padrão da tendência vs estimativa direta (burn-in de 8
trimestres descartado), modelos univariados:

| estrato | processo | legado | corrigido | Δ |
|---|---|---:|---:|---:|
| 01 Belo Horizonte | MA(1) | 36,27 % | 23,01 % | −13,3 |
| 02 Colar e Entorno de BH | MA(1) | 33,36 % | 16,10 % | −17,3 |
| 03 Sul de Minas | ARMA(1,1) | 28,87 % | 31,22 % | +2,4 |
| 04 Triângulo Mineiro | MA(1) | 47,83 % | 40,07 % | −7,8 |
| 05 Zona da Mata | MA(1) | 42,02 % | 29,21 % | −12,8 |
| 06 Norte de Minas | MA(1) | 38,82 % | 33,12 % | −5,7 |
| 07 Vale do Rio Doce | AR(1) | 48,35 % | 48,35 % | **0,0** |
| 08 Central | MA(1) | 45,82 % | 29,65 % | −16,2 |
| **média** | | **40,2 %** | **31,3 %** | **−8,8** |

O Sul de Minas praticamente não muda porque seu theta estimado é −0,021, quase nulo — o termo MA
não tinha o que contribuir de qualquer modo.

**Leitura:** tratar erro amostral positivamente autocorrelacionado como ruído branco deixa o
filtro mais agressivo na suavização, o que superestima o ganho. A conclusão central do artigo
**sobrevive** — as estimativas baseadas em modelo continuam mais precisas que as diretas, com
ganho médio de ~31% —, mas a magnitude publicada está inflada em cerca de um quinto.

### Convergência (issue #2)

Com valor inicial único, o L-BFGS-B retornava código 52 (terminação anormal) — inclusive nas
estimativas que geraram o artigo. Com multi-start (5 partidas, exigindo `convergence == 0`),
**todos os 16 ajustes convergem limpo**. Os valores publicados vieram de otimizações que não
convergiram.

---

## 3. A escala do erro amostral (issue #17) — o segundo problema

O modelo estima livremente a variância da inovação do erro amostral, em vez de amarrá-la à
normalização implícita pelo processo ARMA escolhido. Como `e_t = e~_t * se_t` (com `se_t` o
erro-padrão do desenho), o modelo só respeita a variância do desenho se `Var(e~) = 1`.

Var(e~) implícita nas estimativas corrigidas:

| estrato | desocupados | ocupados | RRSE ocupados |
|---|---:|---:|---:|
| 01 Belo Horizonte | 0,468 | 1,821 | −17,6 % |
| 02 Colar e Entorno | 0,668 | 1,527 | −11,7 % |
| 03 Sul de Minas | 0,464 | 0,280 | 49,5 % |
| 04 Triângulo Mineiro | 0,445 | 0,595 | 33,5 % |
| 05 Zona da Mata | 0,586 | 0,781 | 21,7 % |
| 06 Norte de Minas | 0,076 | 1,656 | 6,4 % |
| 07 Vale do Rio Doce | 0,187 | 0,447 | 40,1 % |
| 08 Central | 0,543 | 0,054 | 69,4 % |

**Nenhum estrato de desocupados chega perto de 1.** O modelo está atribuindo ao erro amostral
entre 8% e 67% da variância que o desenho da PNADc diz existir; o restante é absorvido pelo nível
e pelo irregular.

O padrão em ocupados é o mais eloquente: o ganho de precisão é praticamente uma função monótona
de quanto o modelo desconta a variância do desenho. Os três estratos com `Var(e~) > 1`
(BH, Colar, Norte) são exatamente os de ganho nulo ou negativo; o Central, com `Var(e~) = 0,054`
— o modelo descarta 95% da variância do desenho —, é o de maior ganho, 69,4%.

**Implicação:** parte do "ganho de precisão" não vem de filtragem do ruído, e sim de o modelo
discordar do erro-padrão publicado pelo IBGE. Isso precisa ser declarado, e a variante com a
escala fixada em `Var(e~) = 1` precisa entrar como robustez.

---

## 4. Multivariado (issue #11)

As três matrizes de correlação publicadas não são positivas semidefinidas:

| matriz | menor autovalor | posto efetivo |
|---|---:|---:|
| desocupados | −0,593 | 3 de 8 |
| ocupados | −1,087 | 3 de 8 |
| taxa | −2,000 | 2 de 8 |

Inconsistências verificáveis sem autovalor: em desocupados, rho(BH,Colar) = 1,0000 obrigaria
rho(BH,Central) = rho(Colar,Central), mas os valores são −0,2095 e −0,5845. Em ocupados,
rho(Colar,Sul) = 0,9999 com rho(BH,Sul) = 0,9473 obrigaria rho(BH,Colar) ≥ 0,94, mas o valor é
−0,5130.

Causa: as 28 correlações entram como `tanh(params[k])` livres, o que restringe cada correlação
par a par mas não o conjunto. Correção em `rotinas/11_multivariado_cholesky.R`: o bloco 8×8 é
parametrizado por fator de Cholesky (`Sigma_R = L L'`), positivo-definido por construção, com 36
parâmetros livres — o mesmo número de graus de liberdade de antes (8 variâncias + 28 correlações).

Também corrigido nesse script: `m0` tinha dimensão errada (`rep(0,7) %x% diag(8)` produz uma
matriz 56×8, não um vetor de 56).

### Resultado da reestimação

Ambos os indicadores convergiram (segundo multi-start, `conv = 0`), com verossimilhanças
praticamente iguais entre os dois pontos de partida — o ótimo está bem determinado.

| | menor autovalor antes | menor autovalor depois | posto efetivo |
|---|---:|---:|---:|
| desocupados | −0,593 | **+1,05e−06** | 2 de 8 |
| ocupados | −1,087 | **−1,14e−15** (zero de máquina) | 5 de 8 |

**As células "não identificadas" eram artefato da parametrização.** Sob `tanh` livre, σ̂²_R ia a
zero em alguns estratos (Central em desocupados; BH e Triângulo em ocupados), o que impedia
identificar suas correlações. Com Cholesky nenhum σ̂²_R vai a zero: desocupados de 10,87 a 120,53;
ocupados de 0,25 a 21,53.

**Os achados regionais mudam de figura.** O que o artigo lia como "BH e Triângulo têm dinâmica
distinta" era indeterminação numérica. O que aparece de fato:

- **desocupados:** correlações acima de 0,96, exceto a **Zona da Mata** (0,37 a 0,70);
- **ocupados:** bloco integrado **Norte–Vale–Central** (0,92–0,99), **Triângulo** intermediário
  (0,58–0,71), e **BH, Colar, Sul e Zona da Mata** quase independentes entre si (< 0,14).

**O multivariado compensa em um indicador e não no outro:**

| | univariado | multivariado | posto efetivo |
|---|---:|---:|---:|
| desocupados | 31,3 % | **37,9 %** | 2 |
| ocupados | 23,9 % | 23,8 % | 5 |

O empréstimo de força rende onde há tendência comum a explorar, e não rende onde não há. Isso
substitui a justificativa do artigo ("por consistência") por uma empírica.

Ainda assim, com posto efetivo 2 (desocupados) as 28 correlações individuais permanecem mal
determinadas mesmo sendo agora admissíveis. A especificação natural é **posto reduzido / fatores
comuns** (Σ_R = ΛΛ' + D, com k = 1 ou 2), que troca 36 parâmetros por 16 ou 24 e reporta cargas
fatoriais no lugar de uma matriz 8×8 quase toda perto de 1. Pendente.

---

## 5. Situação por issue

| issue | situação |
|---|---|
| #20 erro amostral MA inerte | **corrigida** em `rotinas/10_` e `11_` |
| #1 BH usa phi no lugar de theta | **corrigida** (estava mascarada pela #20) |
| #2 convergência ignorada | **corrigida** via multi-start; todos convergem limpo |
| #11 correlação sem garantia de PD | **corrigida** via Cholesky em `rotinas/11_` |
| #17 escala do erro amostral livre | **medida e documentada**; falta a variante restrita |
| #8 sazonalidade em 1 de 3 estados trig | **não é inconsistência**: o univariado faz igual. Documentar |
| #14 burn-in inconsistente | `rotinas/10_/11_` padronizam em 8 trimestres |

## 6. Pendências

- Variante com `Var(e~) = 1` fixo, como robustez da issue #17.
- Taxa de desocupação: refazer sobre os componentes corrigidos (cálculo indireto como referência).
- Regerar tabelas e figuras do manuscrito a partir dos novos resultados.
