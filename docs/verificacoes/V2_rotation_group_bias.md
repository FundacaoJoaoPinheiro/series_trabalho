# V2 — Rotation group bias nos ocupados: diagnóstico

**Data:** 2026-07-31 · **Scripts:** `docs/verificacoes/test_rotation_group_bias.R`,
`docs/verificacoes/test_rgb_estabilidade.R` · **Base:** `baserot8reg.rds` (vintage do artigo)

## Ponto de partida

A FAC do pseudo-erro dos **ocupados** não morre no lag 5, embora a sobreposição de amostra
da PNADc (5 entrevistas em trimestres consecutivos, sobreposição (5−k)/5) seja **zero** a
partir dali. A dos **desocupados** morre, como esperado:

| | lag1 | lag2 | lag3 | lag4 | **lag5** | lag6 | lag8 | lag10 |
|---|---|---|---|---|---|---|---|---|
| teórico (5−k)/5 | 0,80 | 0,60 | 0,40 | 0,20 | **0** | 0 | 0 | 0 |
| desocupados | 0,36 | 0,20 | 0,13 | 0,09 | **0,04** | −0,04 | 0,03 | 0,00 |
| **ocupados** | 0,71 | 0,55 | 0,45 | 0,39 | **0,39** | 0,24 | 0,11 | 0,17 |

---

## 1. O rotation group bias existe — e só nos ocupados

Índice de Bailar (média do grupo j ÷ média do trimestre, ×100; 100 = sem efeito), com
teste F de dois fatores (trimestre absorve o nível, testa-se o efeito de entrevista):

**Ocupados** — padrão **monotônico decrescente**, muito consistente entre regiões:

| região | ent1 | ent2 | ent3 | ent4 | ent5 | amplitude | p |
|---|---|---|---|---|---|---|---|
| Belo Horizonte | 102,9 | 100,8 | 99,4 | 99,0 | 98,0 | 5,0 | 0,013 |
| Colar e Entorno | 104,1 | 100,8 | 99,2 | 98,2 | 97,6 | 6,5 | 0,002 |
| Sul de Minas | 103,9 | 101,1 | 99,7 | 98,3 | 97,0 | 7,0 | 0,004 |
| Triângulo Mineiro | 103,5 | 101,4 | 99,4 | 98,8 | 97,0 | 6,5 | 0,136 |
| Mata de Minas | 103,1 | 101,2 | 100,0 | 98,5 | 97,3 | 5,8 | 0,030 |
| Norte de Minas | 103,6 | 101,3 | 99,7 | 98,8 | 96,6 | 7,0 | 0,015 |
| Vale do Rio Doce | 104,5 | 101,7 | 100,2 | 97,8 | 95,8 | 8,8 | 0,002 |
| Central | 104,0 | 101,1 | 99,8 | 98,2 | 97,0 | 7,0 | 0,0006 |
| Minas Gerais | 103,7 | 101,1 | 99,6 | 98,4 | 97,1 | 6,6 | 2e-10 |

**Significativo em 8 de 9 regiões.** A 1ª entrevista superestima a ocupação em ~3–4% e a
5ª subestima em ~3%, com decaimento monotônico. É rotation group bias de manual
(Bailar 1975).

**Desocupados**: **0 de 9** significativos, sinais errantes, sem padrão monotônico. Não há
RGB detectável.

> Isto já explica a assimetria entre os dois indicadores — e explica por que o MA(1) é
> inviável justamente nos ocupados (V1 §1.1): |rho1| = 0,60–0,78 excede o teto teórico de
> 0,5 do MA(1) porque a autocorrelação está inflada por algo que não é erro amostral.

---

## 2. Hipótese testada e REFUTADA: RGB constante não explica a cauda

O alinhamento diagonal (`organiza_base`) permuta ciclicamente os grupos — verificado:
a coluna k no trimestre i corresponde a V1016 = ((k+i−2) mod 5)+1. Logo um efeito **fixo**
de entrevista deveria aparecer como componente **periódica de período 5**, com repique na
FAC nos lags 5, 10, 15. Há indício disso (lag10 > lag9 em 8 de 9 regiões nos ocupados,
contra 4 de 9 nos desocupados), mas o teste direto refutou a explicação:

Removido o efeito de entrevista como fator multiplicativo constante
(X\*ⱼ = Xⱼ / bⱼ) e recalculada a FAC:

| | cauda média (lags 5–12), antes | depois |
|---|---|---|
| ocupados | 0,149 | 0,156 (**−5%**, ou seja, não caiu) |

**A correção não reduziu a cauda.** RGB constante não é a causa.

*(Também foi descartado, por inconclusivo, um teste de detrend por loess: o filtro é
agressivo o bastante para comer a autocorrelação legítima — aplicado aos desocupados,
que são bem-comportados, derrubava rho1 de 0,360 para 0,123 e tornava a FAC negativa.
Mede-se o filtro, não o dado.)*

---

> ## ⚠️ RETIFICAÇÃO (2026-07-31, após a PoC do V3)
> **A conclusão do §3 abaixo — de que o RGB deriva no tempo — NÃO se sustentou.**
> Quando o efeito é estimado dentro do modelo em espaço de estados
> (`docs/verificacoes/V3_poc_modelo_rgb.R`), a máxima verossimilhança leva a
> variância do passeio aleatório de λ a ~0 (2,6e-14) e a variante de **efeito
> constante vence** a de efeito variável por AIC. O RGB **existe e é necessário**
> (ΔAIC = 13,4 contra o modelo sem efeito de painel), mas é **estável no tempo**.
>
> O que o §3 mediu foi ruído amostral nas médias por subperíodo: as amplitudes por
> triênio **oscilam** (3,4 → 8,6 → 4,4 → 9,2) em vez de crescer, e as correlações
> com o tempo (~0,2) nunca foram testadas formalmente. Foi leitura minha de um
> padrão que os dados não sustentam.
>
> **O que permanece válido:** §1 (o RGB existe, é monotônico e só nos ocupados) e
> a constatação de tendência no pseudo-erro do §3.2 — que segue sem explicação
> pelo RGB e fica como questão aberta (ver V3 §Pendências).

## 3. A causa: o RGB **deriva no tempo** ⚠️ (ver retificação acima)

O efeito de entrevista não é estável — ele **se amplia** ao longo da série:

Índice de Bailar dos ocupados, Minas Gerais, por triênio:

| período | ent1 | ent2 | ent3 | ent4 | ent5 | **amplitude** |
|---|---|---|---|---|---|---|
| 2012–2014 | 102,0 | 100,8 | 99,4 | 99,4 | 98,5 | **3,4** |
| 2015–2017 | 104,8 | 101,5 | 99,8 | 97,8 | 96,2 | **8,6** |
| 2018–2020 | 102,4 | 100,6 | 99,7 | 99,3 | 98,0 | **4,4** |
| 2021–2024 | 105,2 | 101,6 | 99,7 | 97,6 | 96,0 | **9,2** |

A correlação do índice com o tempo tem sinal sistemático — **ent1 positivo, ent5 negativo**
em 6 das 9 regiões (Colar +0,20/−0,19; Central +0,18/−0,23; Minas Gerais +0,19/−0,17;
Norte, Sul e Mata idem). O viés da 1ª entrevista cresce e o da 5ª cai: **o RGB está se
abrindo ao longo dos 13 anos.**

Isso explica por que a correção constante falhou: não há um bⱼ a remover, há um bⱼ(t).

### Confirmação: há tendência dentro do pseudo-erro

Regressão de cada pseudo-erro no tempo (sem filtro):

| | grupos com tendência significativa (5%) | R² médio |
|---|---|---|
| **ocupados** | **31 de 45** | **0,21** |
| desocupados | 11 de 45 | 0,06 |

Nos ocupados, **~20% da variação do que o modelo trata como "erro amostral" é tendência
determinística**. Um erro amostral legítimo tem média zero e nenhuma tendência.

Magnitude para dimensionar: o pseudo-erro dos ocupados é 4,9–11,7% do nível da série
(desocupados: 7,7–26,2%). O erro amostral dos ocupados é relativamente pequeno, então uma
contaminação de ~20% dele é proporcionalmente relevante.

---

## 4. Consequência para o artigo

O artigo estima **tendências** do mercado de trabalho. A componente de erro amostral do
modelo em espaço de estados é calibrada com parâmetros derivados de um pseudo-erro que,
nos ocupados, **contém tendência**. Há portanto risco de má-alocação entre sinal e erro
amostral exatamente na dimensão que o artigo quer medir.

Não é motivo para descartar os resultados — o efeito é de segunda ordem e os desocupados
(indicador central do artigo, junto com a taxa) estão limpos. Mas precisa ser tratado.

### Encaminhamentos, do mais barato ao mais correto

1. **Declarar a limitação** (mínimo). O `docs/pontos_revisao_manuscrito.md` já registra, no
   ponto M1, que os pseudo-erros ignoram o rotation group bias. Este diagnóstico dá a
   evidência quantitativa: RGB significativo em 8/9 regiões nos ocupados, crescente,
   ausente nos desocupados.
2. **Modelar o RGB explicitamente** (correto, e é o que a literatura faz). Incluir efeitos
   de grupo de rotação no modelo em espaço de estados — Pfeffermann (1991, JBES 9(2)),
   van den Brakel & Krieg (2015). Trata o viés em vez de absorvê-lo no erro amostral.
3. **Não** remover a tendência do pseudo-erro ad hoc antes de estimar os parâmetros: seria
   corrigir o sintoma sem base teórica, e o resultado passa a depender do filtro escolhido
   (ver §2).

### Impacto na questão das especificações (V1 §1.1)

Isto reenquadra a pergunta "estimar todas as especificações para todas as regiões".
Ampliar o leque de ARMA(p,q) e escolher por AIC **otimiza o ajuste a um objeto que, nos
ocupados, não é erro amostral**. Vale fazer pela comparabilidade entre regiões — que hoje
não existe — mas não resolve isto, e a decisão de (2) deve vir antes.

---

## Como reproduzir

```
Rscript docs/verificacoes/test_rotation_group_bias.R   # Bailar + F; teste da hipótese periódica
Rscript docs/verificacoes/test_rgb_estabilidade.R      # deriva temporal + tendência no pseudo-erro
```
