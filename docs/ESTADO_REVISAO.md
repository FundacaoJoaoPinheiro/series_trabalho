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
| 2b | **MA(4) do desenho excluído** dos candidatos (16/08) | mesmo critério do ruído branco; ver justificativa abaixo |
| 3 | **σ²_I estimado internamente**, irregular por diferença | ver ressalva abaixo |
| 4 | **Especificação individual** por estrato × indicador | não uniformizar |
| 5 | Critério: **Ljung-Box**, mais parcimonioso que passa; empate por BIC | |
| 6 | Estimador da autocovariância dos pseudo-erros: **padrão** (média única, divisor T) | |

### Justificativa da decisão 2b

O MA(4) implicado pelo esquema de rotação 1-2(5) é uma **identidade do desenho**, não uma
aproximação: a sobreposição (5−j)/5 dá θ = 1 em todas as defasagens, logo FAC teórica
0,8 / 0,6 / 0,4 / 0,2. Essa estrutura branqueia bem os resíduos (passa no Ljung-Box em 20
de 24) mas tem o **pior ajuste** do conjunto — posição média 8,5 de 12,5 no BIC e ganho
médio de 2,5 %. Foi escolhida em **um único estrato** (taxa, Norte de Minas) e ali rendeu
1,37 % no multivariado, contra 17,24 % do cálculo indireto, destoando de todos os demais.
A sobreposição teórica é forte demais frente à observada — diagnóstico já registrado no
passo 1. Excluída pelo mesmo critério aplicado ao ruído branco.

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
| 2b — especificação final | ✅ reaberto 16/08 (exclusão do MA(4)) | `rotinas/21_especificacao_final.R` → `especificacao_final.csv` |
| 3 — univariados | ✅ | `rotinas/22_univariado_final.R` → `outputs/univariado_final/` |
| 4 — multivariado Cholesky | ✅ três indicadores | `rotinas/23_multivariado_final.R` |
| 5 — taxa direta e indireta | ✅ | `rotinas/24_taxa_final.R` → `outputs/taxa_final/` |
| 6 — tabelas, figuras, `.tex` | ✅ | `rotinas/25_saidas_artigo.R` |

## Resultados finais

| indicador | ganho univariado | ganho multivariado | posto efetivo de Σ_R |
|---|---:|---:|---:|
| desocupados | 12,00 % | **32,69 %** | 2 |
| ocupados | 14,93 % | 14,63 % | 7 |
| taxa | 10,75 % | **35,60 %** | 2 |

Números após a correção do issue #8 (distúrbio sazonal nos três estados
trigonométricos). Todos os ajustes verificados por teste de perturbação
coordenada: **0 de 384** no univariado e **0 de 240** em cada multivariado.

**Achado metodológico central:** o ganho da especificação multivariada não é automático —
depende de a matriz de covariância dos distúrbios das inclinações ter posto reduzido.
Onde há tendência comum forte (desocupados e taxa, posto 2), o multivariado praticamente
triplica o ganho; onde não há (ocupados, posto 6), os dois modelos empatam.

**A recomendação da taxa se inverteu.** Estimação direta 35,60 % contra 19,51 % do cálculo
indireto, superando em **todos os 8 estratos**. A versão publicada recomendava a indireta,
com o argumento de que a vantagem da direta vinha de uma matriz de covariância inadmissível
— argumento que caiu com a parametrização de Cholesky.

## Limitações declaradas no texto

1. σ²_I fracamente identificado ⟹ irregular por diferença é nulo ⟹ série dessazonalizada
   coincide com a tendência. O artigo **não** apresenta série dessazonalizada como produto.
   O mesmo vale para σ²_S em 23 das 24 combinações; a exceção é taxa × Norte de Minas,
   onde σ²_S é fortemente identificada (planura 26,9).
2. Normalidade rejeitada em 5 de 8 estratos no total de ocupados (choque da Covid em 2020).
3. Autocorrelação remanescente em 2 casos no multivariado: desocupados × Norte (0,001) e
   ocupados × Colar (0,042). **Na taxa, nenhum estrato rejeita.**
4. Viés de grupo de rotação não tratado — é o tema do segundo artigo.

## Armadilhas registradas (não repetir)

- **`optimHess` não serve para verificar o ótimo aqui.** Nas direções planas (σ²_S, σ²_I)
  as diferenças finitas devolvem curvatura negativa espúria: 20 de 24 estratos apareciam
  como "Hessiana não definida positiva". Verificado que caminhar ao longo do autovetor
  negativo **aumenta** o objetivo nos dois sentidos. Usar teste de perturbação coordenada.
- **Tolerância do teste de perturbação: 0,01, não 1e-8.** Com 1e-8 o teste nunca converge
  (persegue ruído do `dlmLL`): numa rodada, um reinício custou 56 min para ganhar 0,113 de
  log-verossimilhança e o contador de falhas ainda subiu, de 100 para 103.
- **Reinício deve partir do ponto perturbado**, nunca do mesmo ponto: reiniciar o L-BFGS-B
  de onde ele parou reproduz a mesma trajetória. Observado: quatro rodadas com logLik e
  contagem idênticos.
- **`ErrorActionPreference = "Stop"` + `2>&1 |` mata o driver PowerShell** silenciosamente,
  porque avisos benignos do R em stderr viram `NativeCommandError`. Usar `Start-Process`
  com redirecionamento em arquivo.
- **Literais acentuados em `.ps1`**: o PowerShell 5.1 lê arquivos sem BOM como ANSI, e
  "Geográficos"/"Versão atual" chegam corrompidos. Localizar diretórios por padrão.

## A especificação final

19 AR(1), 3 AR(3), 1 AR(2), 1 MA(1). Ganho médio univariado **12,79 %**
(desocupados 12,0 · ocupados 15,4 · taxa 10,6). Coeficientes em
`outputs/identificacao/especificacao_final.csv`.

**Estratos em que o Ljung-Box não passa (2 de 24):** Colar e Entorno na taxa (p = 0,014) e
Norte de Minas na taxa (p = 0,047, marginal). Declarar como limitação. O Norte passou a
figurar aqui após a exclusão do MA(4): aquela formulação branqueava melhor os resíduos,
mas ao custo de ganho negativo (−1,28 %), contra 4,66 % do AR(3) que a substituiu.

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
