# Erro amostral em séries de pesquisas repetidas: pseudo-erros e alternativas

Memo de referência sobre a estimação da **estrutura de autocovariância do erro amostral**
em pesquisas amostrais periódicas com painéis rotativos (como a PNADc) — o componente que
entra no modelo estrutural em espaço de estados. Cobre o método usado no artigo (pseudo-erros)
e as **formas alternativas** de cálculo, com referências.

---

## 1. O problema

Numa pesquisa repetida com sobreposição de amostra (painel rotativo), a estimativa direta
`ŷ_t = θ_t + e_t` tem erro amostral `e_t` **autocorrelacionado no tempo** (porque parte das
unidades é reentrevistada). No modelo estrutural (Scott–Smith / Pfeffermann / Harvey), é preciso
**modelar `e_t`** — tipicamente como um AR/MA de baixa ordem — senão sua autocorrelação é
absorvida pela tendência/sazonalidade, viesando a inferência (ponto central de Pfeffermann, 1991).
A questão prática: **como estimar a estrutura de autocorrelação (e variância) de `e_t`?**

---

## 2. O método do artigo: pseudo-erros (Silva & Cruz, 2002)

- **Referência:** Silva, D.B.N. & Cruz, M.M. (2002). *Séries temporais de pesquisas amostrais
  periódicas*. São Paulo: ABE. (Minicurso/monografia da Associação Brasileira de Estatística.)
- **Ideia:** estima-se o indicador **separadamente por grupo de rotação** (na PNADc, os 5 grupos
  distinguidos por `V1016` = nº da entrevista). Os "pseudo-erros" = desvios das estimativas de
  cada grupo em relação à média dos grupos. A **FAC/FACP** dos pseudo-erros, após alinhamento
  pela coorte de entrada, **aproxima** a autocorrelação de `e_t` induzida pela rotação → identifica
  o processo AR/MA. É a base da tese de Gonçalves (2023, cap. 9) e do artigo.
- **Quando é a escolha certa:** quando **não se tem acesso fácil ao painel ligado ao longo do
  tempo** (microdados com identificador de unidade entre trimestres) para estimar as covariâncias
  cruzadas diretamente. É uma **aproximação prática**.
- **Limitação:** aproxima a estrutura a partir dos grupos de um mesmo trimestre + alinhamento, não
  das covariâncias diretas entre estimativas de trimestres diferentes.

---

## 3. Formas ALTERNATIVAS de calcular (com referências)

### (A) Estimação direta (design-based) das covariâncias do erro amostral
A abordagem "padrão-ouro" quando se tem o **painel ligado**: estimar diretamente
`Cov(e_t, e_{t-k})` usando as **unidades que estão na amostra em t e em t−k** (a sobreposição),
via linearização/replicação. van den Brakel & Krieg: *"elementary estimates based on data from
units that join and leave the survey at the same time can be used to estimate the covariance
structure of the sampling errors."*
- **Refs:** Pfeffermann (1991), JBES 9(2):163–175 (usa estimativas por painel **ou** agregadas,
  conforme disponibilidade dos dados); van den Brakel & Krieg (2009, 2015) — LFS holandês.

### (B) Modelo estrutural MULTIVARIADO sobre as ondas do painel
Em vez de "colapsar" numa série só, modela-se **cada onda (wave) do painel como uma série** num
STM multivariado, estimando conjuntamente a autocorrelação do erro amostral **e** o **viés de
grupo de rotação** (rotation group bias — diferenças sistemáticas entre ondas). van den Brakel &
Krieg (2015) mostram que a autocorrelação das ondas 2–5 se modela bem por um **AR(1)**.
- **Refs:** Pfeffermann (1991); van den Brakel & Krieg (2009, CBS Discussion Paper; 2015, JRSS-A);
  Boonstra & van den Brakel; aplicações COVID-19 (van den Brakel et al., 2022, JRSS-A).
- **Vantagem:** trata o rotation group bias explicitamente (o método dos pseudo-erros não).

### (C) Estrutura de autocorrelação ASSUMIDA/paramétrica do desenho de rotação
Deriva-se a forma do AR/MA de `e_t` da **teoria do esquema de rotação** + uma correlação `ρ`
entre ondas (às vezes **fixada**, não estimada), com a variância vinda de estimativas prévias.
Abordagem clássica do **US Census Bureau / CPS**.
- **Refs:** Scott & Smith (1974, JASA); Scott, Smith & Jones (1977); Bell & Hillmer (1990,
  *Survey Methodology* — "the time series approach to estimation for repeated surveys");
  Tiller (1992, *J. Official Statistics* — CPS); Binder & Dick (1989, 1990 — ARIMA de erros
  amostrais); Harvey & Chung (2000, JRSS-A — LFS do Reino Unido).

### (D) GVF — Generalized Variance Functions (para a MAGNITUDE, não a autocorrelação)
Modela `Var(ŷ_t)` como função suave do estimador (ex.: CV vs nível), suavizando os EPs diretos
(que são ruidosos em domínios pequenos). **Complementar** ao (A)–(C): dá o **nível** da variância,
não a autocorrelação. (O artigo já suaviza/usa o CV nessa linha.)
- **Refs:** Wolter (2007), *Introduction to Variance Estimation*; Valliant, Dever & Kreuter.

### (E) Replicação/bootstrap para covariâncias entre tempos
Pesos replicados (jackknife, BRR, *successive-difference replication*) que respeitam a
sobreposição do painel → estima `Cov(e_t, e_{t-k})` empiricamente. Também Fuller (1990) para
bootstrap de séries de estimativas amostrais.

### (F) Ajuste sazonal ciente da correlação do erro amostral
Linha aplicada recente que incorpora (A)/(B) no ajuste sazonal em espaço de estados.
- **Ref:** Mayer (2018), *Statistical Journal of the IAOS* — "Improving seasonal adjustment by
  accounting for sample error correlation using state space models".

---

## 4. Posicionamento para o artigo

- O método do artigo (**pseudo-erros, Silva & Cruz**) é **legítimo e adequado** ao contexto de
  **acesso limitado ao painel ligado** — usa só as estimativas por grupo de rotação de cada
  trimestre. Identifica a ordem/coeficientes AR/MA (que é o que entra no estado do erro amostral).
- A alternativa mais **rigorosa** seria **(A)/(B)** — estimar as covariâncias do erro amostral
  diretamente do painel ligado, ou modelar as 5 ondas num STM multivariado (van den Brakel &
  Krieg), o que **também captura o rotation group bias** (que os pseudo-erros ignoram). Custo:
  precisa dos microdados com identificador longitudinal e é bem mais pesado.
- **A PNADc TEM as chaves** (`UPA`+`V1008`+`V1014` p/ domicílio; `V2003`/sexo/data de nascimento p/
  pessoa) — os microdados são públicos. **Porém a ligação é imperfeita**, sobretudo nos períodos
  iniciais: o **número de ordem (`V2003`) não se mantém entre visitas** (observação de Luna Hidalgo,
  IBGE), exigindo um **procedimento de verificação/re-pareamento** com erro. Logo, a justificativa
  correta para os pseudo-erros é a **imperfeição/custo da ligação**, não "restrição de acesso". Ver
  `docs/pontos_revisao_manuscrito.md` (ponto M1).
- Uma **simplificação** comum e defensável: assumir **AR(1)** para o erro amostral (van den Brakel
  & Krieg 2015 mostram que ondas 2–5 são bem descritas por AR(1)) — mais parcimonioso que
  identificar MA/ARMA por estrato.
- **Recomendação de revisão:** manter os pseudo-erros como método principal (coerente com a tese),
  mas (i) **validar** a estrutura AR/MA identificada contra a expectativa teórica do desenho de
  rotação da PNADc (1-2(5)-2) e, se possível, (ii) **citar** as alternativas (Pfeffermann 1991;
  van den Brakel & Krieg 2015) e discutir o **rotation group bias** como limitação não tratada.

---

## Referências-chave

- Silva, D.B.N.; Cruz, M.M. (2002). *Séries temporais de pesquisas amostrais periódicas*. ABE. *(pseudo-erros — método do artigo)*
- Gonçalves, C.C.S. (2023). *Tese de doutorado*, ENCE/IBGE, cap. 9. *(aplicação à PNADc — fonte primária)*
- Scott, A.J.; Smith, T.M.F. (1974). *JASA*. — Scott, Smith & Jones (1977). *(extração de sinal)*
- Pfeffermann, D. (1991). Estimation and seasonal adjustment of population means using data from repeated surveys. *JBES* 9(2):163–175. *(fundacional)*
- Bell, W.R.; Hillmer, S.C. (1990). The time series approach to estimation for repeated surveys. *Survey Methodology*.
- Tiller, R. (1992). *J. Official Statistics*. *(CPS)*; Binder & Dick (1989/1990); Harvey & Chung (2000, *JRSS-A*).
- van den Brakel, J.A.; Krieg, S. (2009; 2015). STM multivariado do LFS holandês; AR(1) para ondas 2–5; rotation group bias.
- Mayer, A. (2018). *Statistical Journal of the IAOS*. *(ajuste sazonal + correlação do erro amostral)*
- Rao, J.N.K.; Molina, I. (2015). *Small Area Estimation*, 2ª ed., Wiley.
- Wolter, K. (2007). *Introduction to Variance Estimation*, Springer. *(GVF/replicação)*

*Buscas: Elicit (sem acesso à API neste plano) e busca web (jul/2026).*
