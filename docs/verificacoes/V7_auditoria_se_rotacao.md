# V7 — Auditoria dos erros-padrão por grupo de rotação

**Data:** 2026-08-01 · **Script:** `docs/verificacoes/V7_auditoria_se_rotacao.R`
**Dado:** microdados PNADc 2024Q4 (`D:/Dados/PNADC/2024/PNADC_042024_20250815.zip`), MG

## Veredito

**Os `se` por grupo de rotação estão CORRETOS.** O erro era meu: somei variâncias de
estimativas que são **negativamente correlacionadas**.

| | ocupados | desocupados |
|---|---|---|
| (a) se do total, amostra completa | 83.663 | 23.867 |
| (c) se implicado somando só as **variâncias** | 375.588 (**4,49×**) | 29.169 (1,22×) |
| (d) se implicado com a **matriz de covariância** | **83.663 (1,00×)** | **23.867 (1,00×)** |

Com a matriz de covariância completa, o total implicado pelas 5 ondas reproduz o do
`survey` **exatamente**. A soma das estimativas também bate (razão 1,0000).

### Por que a correlação é negativa

| | correlação média entre ondas | faixa |
|---|---|---|
| ocupados | **−0,236** | −0,317 a −0,111 |
| desocupados | −0,085 | −0,163 a −0,021 |

As ondas **compartilham UPAs**: dentro de um mesmo setor censitário os domicílios se
dividem entre os grupos de rotação. Se um grupo capta mais ocupados naquela UPA, os outros
captam menos — o total da UPA é aproximadamente fixo. Daí a correlação negativa, que é
exatamente o que faz Var(total) < Σ Var(ondas).

> Isto **retifica o V6 §2**, que afirmava haver inconsistência na estimação por subdomínio.
> Não há. O `svyby` por `V1016` está certo.

## O que isso muda — três consequências

### 1. A conclusão do V6 permanece, por outro motivo

O `cv_design` que usei como linha de base no V5 (`sqrt(rowSums(se²))/5 ÷ média das ondas`)
assume independência entre ondas — e portanto **superestima**. Para Belo Horizonte /
ocupados dava 8,33%, quando o CV design-based correto é **1,8%** (o do total, que agora se
confirma coerente com as ondas).

Contra a referência certa: design-based **1,8%** vs modelo **3,84%**. **O modelo continua
pior.** O ganho de precisão segue não demonstrado.

### 2. O modelo está MAL ESPECIFICADO — e este é o achado acionável

`funcoes/41_modelo_rgb_coorte.R` usa `V = diag(se_j²)`, isto é, assume erro amostral
**independente entre ondas dentro do trimestre**. O comentário no cabeçalho do arquivo diz
que "ondas distintas no mesmo trimestre SÃO amostras disjuntas, então a independência vale
dentro de t". **Isso está errado** — amostras disjuntas de UPAs compartilhadas são
negativamente correlacionadas.

Efeito: sob independência, Var(média das 5) = σ²/5. Com ρ̄ = −0,236,
Var(média) = (σ²/5)·(1 + 4ρ̄) ≈ 0,06·(σ²/5) — mais de dez vezes menor. **O modelo trata as
observações como muito mais ruidosas do que são**, e por isso não consegue superar a
estimativa direta.

Ou seja: o caminho C não fracassou por concepção. Fracassou porque a matriz de covariância
das observações está errada.

### 3. A correção é viável

`svyby(..., covmat = TRUE)` devolve a matriz de covariância completa entre os subdomínios.
Basta reprocessar os microdados guardando `vcov(by)` por trimestre × região, em vez de só
os cinco `se`, e alimentar o modelo com `V` cheia (o `dlm` aceita `V` não-diagonal
variável no tempo via `JV`/`X`). Os microdados estão disponíveis em `D:/Dados/PNADC`.

## Próximos passos

1. **Estender a rotina 05** para gravar a matriz de covariância 5×5 por trimestre × região
   (ocupados, desocupados e taxa), não apenas os `se`.
2. **Alterar `funcoes/41_`** para aceitar `V` cheia.
3. **Refazer a fase 3** e só então comparar precisão — sempre contra o CV design-based do
   **total**.
4. Rodar o V7 em mais trimestres para confirmar que a correlação é estável (aqui: 1
   trimestre).

## O que continua válido, independentemente disto

Todo o achado de rotation group bias — vem das estimativas **pontuais** e do λ, não dos
erros-padrão: RGB em 8/9 ocupados (V2), λ ≈ índice de Bailar com r = 0,9997 (V3), efeito
selecionado em 9/9 por AIC (V5), ρ ≈ 0,98 / 0,80.
