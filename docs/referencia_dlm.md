# Referência do pacote `dlm` (Giovanni Petris) — para o artigo dos estratos de MG

Documento de referência do pacote **`dlm`** (v1.1-6.1), o motor dos modelos estruturais
de séries temporais do artigo (scripts 7–40). Baseado na vignette oficial de Petris
(*dlm: MLE and Bayesian analysis of Dynamic Linear Models*) + inspeção das funções.
Substitui, com rigor, o caderno de estudo `4_Estudo dlm.R` (que era só notas/scratch).

---

## 1. O modelo (DLM = modelo linear em espaço de estados)

$$
\begin{aligned}
y_t &= F_t\,\theta_t + v_t, & v_t &\sim \mathcal N(0, V_t) \quad\text{(equação de observação)}\\
\theta_t &= G_t\,\theta_{t-1} + w_t, & w_t &\sim \mathcal N(0, W_t) \quad\text{(equação de estado/transição)}
\end{aligned}
\qquad \theta_0 \sim \mathcal N(m_0, C_0)
$$

- `y_t` (m×1): observação. `θ_t` (p×1): **vetor de estado** não observado.
- `F_t` (m×p): **matriz de observação** — como o estado carrega na observação (`FF`).
- `G_t` (p×p): **matriz de transição** — dinâmica do estado (`GG`).
- `V_t` (m×m): variância do **erro de observação** (`V`). **Deve ser não-singular.**
- `W_t` (p×p): variância do **erro de evolução** (`W`). Pode ser singular (o filtro SVD tolera).
- `m0`, `C0`: média e variância do estado inicial (fazem parte da definição do modelo).

Um objeto `dlm` é uma lista com componentes `FF, V, GG, W, m0, C0` (e, se tempo-variante,
`JFF, JV, JGG, JW, X`). Extratores/substituidores: `FF(m)`, `GG(m)`, `V(m)`, `W(m)`,
`m0(m)`, `C0(m)` (e `V(m) <- ...` etc.).

> O `dlm` usa filtro/suavizador baseados na **decomposição em valores singulares (SVD)**
> das matrizes de variância → estável numericamente (evita a instabilidade do Kalman ingênuo).

---

## 2. Funções construtoras (blocos do modelo)

| Função | Modelo | Estados |
|---|---|---|
| `dlmModPoly(order, dV, dW)` | polinomial de ordem `order` | `order` (nível, inclinação, …) |
| `dlmModTrig(s, q, dV, dW)` | sazonal **trigonométrico**, período `s` | `s-1` |
| `dlmModSeas(frequency, dV, dW)` | sazonal por **fatores** (dummies) | `frequency-1` |
| `dlmModReg(X, addInt, dV, dW)` | regressão linear (tempo-variante) | `NCOL(X)+addInt` |
| `dlmModARMA(ar, ma, sigma2)` | processo ARMA | depende de p,q |

`dV` = diagonal de `V`; `dW` = diagonal de `W`. **Defaults:**
- `dlmModPoly(order=2)` → `dW = c(0,1)`: só a **inclinação** tem perturbação (tendência
  suave / passeio integrado). `dlmModTrig`/`dlmModSeas` → `dW=0` por padrão (**sazonal determinístico**).

### 2.1 `dlmModPoly(2)` — tendência linear local (LLT)  → 2 estados
```
FF = [1  0]        GG = [1 1]      # nível_t = nível_{t-1} + inclin_{t-1}
                        [0 1]      # inclin_t = inclin_{t-1} + w
```
Estado 1 = **nível** (μ, carrega na obs); estado 2 = **inclinação** (β, não carrega).

### 2.2 `dlmModTrig(s=4)` — sazonalidade trigonométrica trimestral → 3 estados
```
FF = [1  0  1]     GG = [ 0 1  0]   # harmônico 1 (freq π/2): par cos/sin (estados 3,4)
                        [-1 0  0]   #   rotação [[0,1],[-1,0]]
                        [ 0 0 -1]   # harmônico 2 = Nyquist (freq π): 1 estado (estado 5), GG=-1
```
- **Estado 3 = cos do harmônico 1** (carrega na obs, `FF=1`).
- **Estado 4 = sin do harmônico 1** (auxiliar, gira com o 3, **não carrega**, `FF=0`).
- **Estado 5 = Nyquist** (carrega na obs, `FF=1`).
- Contribuição sazonal na observação = **estado 3 + estado 5**.

### 2.3 `dlmModReg(x, addInt=FALSE)` — regressão (o **erro amostral** entra aqui)
```
FF = [1]  (tempo-variante: JFF=1, X = x)   GG = [1]   # coef segue passeio (default)
```
`y_t` recebe `coef_t · x_t`. No artigo, `x = se_db` (desvio-padrão amostral conhecido) e o
coeficiente é forçado a **AR(1)** por `GG[k,k] <- phi1` → o erro amostral é `e_t = a_t · se_db_t`
com `a_t` AR(1) (abordagem Pfeffermann-Tiller).

### 2.4 Combinando modelos: `+` e `%+%`
- **`m1 + m2`**: soma DLMs para a **mesma** observação (empilha estados; `FF` concatena,
  `GG/W/C0` bloco-diagonais; `V` soma). Ex.: tendência + sazonal + ruído.
- **`m1 %+% m2`**: "soma externa" para observações **multivariadas** independentes.

---

## 3. O MODELO DO ARTIGO (BSM univariado com erro amostral)

```r
m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se_db, addInt = FALSE)   # 6 estados
```

| Estado | Componente | GG (dinâmica) | `FF` |
|---|---|---|---|
| 1 | **nível** μ | `μ_t = μ_{t-1} + β_{t-1}` | 1 |
| 2 | **inclinação** β | `β_t = β_{t-1} + w` | 0 |
| 3 | sazonal H1 **cos** | rotação π/2 com o estado 4 | 1 |
| 4 | sazonal H1 **sin** (auxiliar) | rotação π/2 com o estado 3 | **0** |
| 5 | sazonal H2 **Nyquist** | `= −estado5_{t-1}` | 1 |
| 6 | **coef. do erro amostral** (×`se_db_t`) | AR(1): `GG[6,6]=φ` | 1 |

`FF = [1, 0, 1, 0, 1, 1]`. **Sinal** (parâmetro populacional θ) = nível + sazonal = estados **1,3,5**.
O `buildFun` típico monta `W` (diagonais dos hiperparâmetros) e `V` (irregular) e devolve `m`.

> ⚠️ **Ponto de método (issue #8):** para um sazonal trigonométrico **estocástico** a variância
> de perturbação deve ir simetricamente nos estados **3 e 4** (par do H1, mesma variância) e no
> **5** (Nyquist). Os scripts do artigo põem `W` só no estado 3 → 4 e 5 ficam determinísticos.
> Ver `docs`/issues.

---

## 4. Estimação por máxima verossimilhança — `dlmMLE`

```r
dlmMLE(y, parm, build, method = "L-BFGS-B", ..., debug = FALSE)
```
- É um **wrapper do `optim`**. `build(parm)` deve devolver um `dlm` a partir do vetor `parm`.
- **Parametrizar variâncias em log** (`exp(x)`) para garantir positividade.
- Retorna a saída do `optim`: `$par` (MLE), `$value` (**−log-verossimilhança** no ótimo),
  `$convergence` (**0 = convergiu**), `$hessian` (se `hessian=TRUE`).
- `...` é repassado ao `optim` (ex.: `control=list(maxit=...)`, `hessian=TRUE`, `lower/upper`).

```r
build <- function(x) { m <- dlmModPoly(1, dV = exp(x[1]), dW = exp(x[2])); m }
fit <- dlmMLE(Nile, parm = c(0,0), build = build)
fit$convergence            # deve ser 0
mod <- build(fit$par)      # modelo no ótimo
```
`dlmLL(y, mod)` avalia a −log-verossimilhança diretamente (útil para razão de verossimilhança).

> **Boas práticas (viram issues na revisão):** checar `fit$convergence==0` **e** a Hessiana
> positiva-definida antes de aceitar o ótimo; usar múltiplos pontos iniciais (multistart);
> AIC/BIC = `2*value + {2k | k·log T}` com `k` = nº real de hiperparâmetros.

---

## 5. Filtragem — `dlmFilter`

```r
f <- dlmFilter(y, mod)
```
Retorna (n = nº de observações):
- **`m`** — médias filtradas dos estados, `E[θ_t | y_{1:t}]`, **(n+1) × p** (linha 1 = prior θ_0).
- **`U.C`, `D.C`** — SVD das variâncias filtradas `C_t = Var[θ_t | y_{1:t}]` (U = lista de
  matrizes; D = matriz cujas linhas são as diagonais).
- **`a`, `U.R`, `D.R`** — previsão 1-passo do estado `E[θ_t | y_{1:t-1}]` e sua variância (SVD).
- **`f`** — previsão 1-passo da observação `E[y_t | y_{1:t-1}]` (resíduos = `y - f`).

### Reconstruir variâncias e extrair erros-padrão — `dlmSvd2var`
```r
vC <- dlmSvd2var(f$U.C, f$D.C)      # lista de (n+1) matrizes p×p
sqrt(vC[[t]][i,i])                  # EP do i-ésimo estado no tempo t
```
**EP de uma combinação linear de estados** (é assim que os scripts pegam o EP do *sinal*):
$$\text{EP}(c'\theta_t) = \sqrt{c'\,C_t\,c}$$
```r
c_sinal <- matrix(c(1,0,1,0,1,0), 1, 6)     # nível + sazonal (estados 1,3,5)
se_sinal_t <- sqrt(c_sinal %*% vC[[t]] %*% t(c_sinal))
```

---

## 6. Suavização — `dlmSmooth`

```r
s <- dlmSmooth(f)          # aceita um objeto filtrado, OU dlmSmooth(y, mod = mod)
```
Retorna **`s`** (estados suavizados `E[θ_t | y_{1:n}]`, (n+1)×p) e **`U.S`, `D.S`** (SVD das
variâncias suavizadas). EP idem via `dlmSvd2var(s$U.S, s$D.S)`.

- **`dropFirst(x)`** — remove a 1ª linha (o estado inicial θ_0) das saídas `m`/`s` para alinhar
  com a série observada. Usado em quase todo plot/extração.
- Cautela com as **primeiras observações**: o filtro ainda está "aquecendo" (inicialização
  difusa via `C0` grande) → o artigo descarta um *burn-in* e concentra a análise a partir de ~2013/14.

---

## 7. Previsão — `dlmForecast`

```r
fore <- dlmForecast(f, nAhead = 20)   # f é um dlmFiltered
```
Retorna `a` (média dos estados futuros), `R` (var-cov dos estados, lista), `f` (média das obs
futuras), `Q` (var-cov das obs futuras). Não é o foco do artigo (que estima tendência, não prevê).

---

## 8. Análise bayesiana (não usada no artigo, mas disponível)

- **`dlmBSample(dlmFiltered)`** — *Forward Filtering Backward Sampling* (FFBS): amostra
  `θ_{0:n}` da condicional completa dos estados (parte de um Gibbs).
- **`dlmGibbsDIG(y, mod, a.y, b.y, a.theta, b.theta, n.sample, ind, …)`** — Gibbs pronto para o
  modelo *d-inverse-gamma* (variâncias com priori gama inversa). Devolve cadeias `dV`, `dW`.
- **`arms(...)`** — *Adaptive Rejection Metropolis Sampling* (uni e multivariado).
- Utilidades MCMC: `mcmcMean`, `ergMean`, `mcmcSD`.

---

## 9. Como o pipeline do artigo usa tudo isso (resumo do fluxo)

1. Constrói o BSM 6-estados (`dlmModPoly(2)+dlmModTrig(4)+dlmModReg(se_db)`), com AR/MA/ARMA no
   estado do erro amostral (`GG[6,6]`), via as funções em `funcoes/` (`12_estrutural_AR1.R`, etc.).
2. `dlmMLE` estima os hiperparâmetros (variâncias de `W`, `V`) — com busca em grid de valores
   iniciais e seleção por verossimilhança.
3. `dlmFilter` + `dlmSmooth` → estados filtrados/suavizados.
4. **Sinal** = nível + sazonal (`ts.signal = m[,1] + (m[,3]+m[,5])`); **EP do sinal** via
   `dlmSvd2var` + `c_sinal %*% Var %*% t(c_sinal)`; **CV** = EP/sinal.
5. Multivariado: empilha os 8 estratos (`%x% diag(8)`), com covariâncias cruzadas nas inclinações
   (com/sem correlação); diagnósticos (Shapiro, teste H, Ljung-Box), RRSE, viés, razão de verossimilhança.

---

## 10. Armadilhas e pontos de atenção

- `V` **precisa** ser não-singular; `W` pode ser singular (OK no filtro SVD).
- As variâncias filtradas/suavizadas vêm **em SVD** (`U.*`/`D.*`) — sempre reconstruir com `dlmSvd2var`.
- `m`/`s` têm **n+1** linhas — usar `dropFirst()`.
- `dlmMLE$value` é **−logLik** (menor = melhor); atenção ao sinal em AIC/BIC e na razão de verossimilhança.
- `dlmModTrig` vs `dlmModSeas`: **trigonométrico** (usado no artigo) vs **fatores/dummies** (variante testada).
- Inicialização difusa (`C0` grande) infla o EP nas primeiras observações → *burn-in*.

---

*Fontes: vignette `dlm` (Petris, 2009-01-14, `system.file("doc","dlm.pdf",package="dlm")`);
inspeção de `FF/GG/V/W` das construtoras; Harvey (1989); West & Harrison (1997); Durbin & Koopman (2012).*
