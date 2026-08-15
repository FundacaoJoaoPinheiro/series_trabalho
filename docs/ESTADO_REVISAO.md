# Estado da revisão — retomada em contexto novo

**Atualizado:** 2026-08-15 · **Branch:** `correcao-erro-amostral` · **PR:** #21
**Vintage:** `baseestr8reg.rds` e `pseudoerros_8reg/` **da raiz** (reproduz o artigo; a
cópia em `data/` é o vintage revisado pós-Censo 2022 e NÃO reproduz).

Documento de handoff. Quem retomar isto em sessão nova deve ler este arquivo primeiro,
depois `docs/verificacoes/V10_erro_amostral_ma1.md`.

---

## Decisões dos autores (fechadas, não reabrir sem motivo)

| # | decisão | consequência |
|---|---|---|
| 1 | **Var(ẽ) = 1 imposta** | o modelo respeita a variância do desenho; a inovação é derivada, não estimada |
| 2 | **Ruído branco excluído** dos candidatos | há autocorrelação nos pseudo-erros e a decisão foi modelá-la |
| 3 | **σ²_I estimado internamente**, irregular por diferença | ver ressalva abaixo |
| 4 | **Especificação individual** por estrato × indicador | não uniformizar |
| 5 | Critério: **Ljung-Box**, mais parcimonioso que passa; empate por BIC | |
| 6 | Estimador da autocovariância dos pseudo-erros: **padrão** (média única, divisor T) | |

### Ressalva registrada sobre a decisão 3

O perfil de verossimilhança é **quase plano** em σ²_I: variar de 0,1% a 10% da variância
do desenho custa menos de 0,3 de log-verossimilhança, contra o limiar de 1,92. O
componente é **fracamente identificado** — a estimativa depende da convenção de
otimização, e com busca livre chega a valores muito diferentes (33,7 em BH contra ~0).

Consequência prática medida: com σ̂²_I ≈ 0, o filtro reproduz a observação exatamente e
**o irregular por diferença sai zero**, o que faz a série dessazonalizada coincidir com a
tendência. A convenção de otimização está fixada em `rotinas/_id_funcoes.R` e **deve ser
mantida** para o resultado ser reproduzível. O artigo precisa declarar o componente como
fracamente identificado e, se possível, mostrar o perfil.

---

## Pipeline — o que está feito

| passo | estado | artefato |
|---|---|---|
| 0 — estimador da autocovariância | ✅ | `rotinas/17_fac_pseudo_erros.R` |
| 1 — FAC/FACP | ✅ | `outputs/fac_pseudo_erros/` |
| validação das montagens | ✅ 56 verificações, 0 falhas | `rotinas/18_valida_processos.R` |
| 2 — identificação (301 ajustes) | ✅ | `rotinas/20_identificacao_paralela.R`, `outputs/identificacao/` |
| 2b — especificação final | ✅ | `rotinas/21_especificacao_final.R` → `especificacao_final.csv` |
| **3 — univariados** | ⬜ pendente | consumir `especificacao_final.csv` |
| **4 — multivariado Cholesky** | ⬜ pendente | `rotinas/11_multivariado_cholesky.R` com `RESTRITO=1` |
| **5 — taxa direta e indireta** | ⬜ pendente | `rotinas/13_taxa_indireta.R` |
| **6 — tabelas, figuras, `.tex`** | ⬜ pendente | `rotinas/14_figuras.R` |

## A especificação final

19 AR(1), 2 AR(3), 1 AR(2), 1 MA(1), 1 MA(4) do desenho. Ganho médio 12,4 %
(desocupados 12,0 · ocupados 15,4 · taxa 9,8). Coeficientes em
`outputs/identificacao/especificacao_final.csv`.

**Único estrato em que o Ljung-Box não passa:** Colar e Entorno na taxa (p = 0,014).
Declarar como limitação.

Convergência das três abordagens, que sustenta a escolha no texto: Box-Jenkins pela
FAC/FACP indica AR(1) compatível em 18 de 24 (inequívoco em 10); o Ljung-Box aceita AR(1)
em 19 de 24; e o AR(1) tem o maior ganho médio entre as formulações disponíveis em todas
as combinações (12,90 %).

---

## Correções de código já aplicadas (issues do repo)

| issue | o quê |
|---|---|
| **#20** (nova) | termo MA do erro amostral **inerte**: `W[7,7]=0` com linha nula de `GG` zerava o estado auxiliar. Ganho de desocupados caía de 40,2 % para 31,3 % ao corrigir |
| #1 | BH-desocupados usava `phi1_ar1` no lugar de `theta1_ma1` — estava mascarada pela #20 |
| #11 | covariância das inclinações por Cholesky; as matrizes publicadas têm autovalores negativos (−0,59 / −1,09 / −2,00) |
| #2 | multi-start com exigência de convergência limpa |
| #17 | escala do erro amostral — resolvida pela decisão 1 |
| — | `m0` com dimensão errada nos 5 multivariados |

## Armadilhas técnicas descobertas (não repetir)

- **`flush.console()` não funciona** em `Rscript` com saída redirecionada. Use gravação
  incremental em arquivo (conexão aberta e fechada a cada escrita).
- **Travamento dentro do `dlmLL`**: para certos parâmetros o SVD do `dlm` não converge, em
  código C. Nem `setTimeLimit` nem teto de avaliações alcançam. **Só limitar a caixa de
  busca resolve** (`lower`/`upper` em `optim`).
- **`casa_momentos` aceitando ajustes infactíveis**: com ρ₁ = 0,66 e teto teórico de 0,5 no
  MA(1), o melhor ajuste possível passava numa tolerância frouxa e o otimizador levava θ à
  fronteira. Corrigido com exigência de casamento efetivo da FAC e raízes afastadas.
- **Nomes dos estratos DIFEREM entre bases**: `baseestr8reg.rds` tem
  "02-Colar e Entorno **m**etropolitano de BH" e `dadosalin_txdesoc_8reg.rds` tem
  "**M**etropolitano". A busca por nome falhava em silêncio.
- **`parLapplyLB` balanceia por blocos** de `ceiling(n/núcleos)` por padrão. Use
  `chunk.size = 1`.
- Ocupados é ~7× mais caro de estimar que desocupados (autocorrelação alta).

## Estado do manuscrito

As tabelas e figuras instaladas em `Artigo Estratos Geográficos/Versão atual/` são de uma
rodada **anterior** a estas decisões e serão substituídas no passo 6. Originais das
figuras preservados em `Versão atual/resultados/_figuras_originais_20260814/`.
Ver `Artigo Estratos Geográficos/revisao_pre_apresentacao.md`.
