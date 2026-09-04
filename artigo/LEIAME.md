# Insumos e saídas do manuscrito

Esta pasta reúne o que o pipeline gera para o artigo e os arquivos que o
manuscrito consome. **O texto em si não fica aqui.**

## Onde está o texto

O manuscrito é escrito e revisado no **Overleaf**, projeto
`697bbbf567ec83d3bfcd67f0`, com controle de alterações ativo. É lá que está a
versão corrente, e é de lá que sai o PDF.

O `econbr-sample.tex` deixou de ser versionado aqui em 04/09/2026. A razão é
simples: uma cópia dentro do repositório envelhece em relação ao Overleaf sem
avisar, e um repositório com tabelas novas e texto velho engana mais do que um
repositório sem o texto. O arquivo continua ignorado pelo `.gitignore`, então
uma cópia local de trabalho não polui o `git status`.

## O que é gerado pelo pipeline

**Não editar à mão** — sobrescrito a cada execução de `rotinas/25_saidas_artigo.R`:

- `resultados/<indicador>/tabhiper*.tex`, `diag*.tex`, `matrizcorr_*.tex`,
  `diffvicio*.tex`, `est_pontual_*.tex`
- `resultados/Taxa de desocupação/tabcomptaxa.tex`
- `resultados/modelos_arma.tex`
- as 6 figuras `resultados/<indicador>/Figura_*.png`

Ao subir uma tabela ou figura para o Overleaf, subir sempre a versão desta
pasta — nunca editar o `.tex` de tabela lá dentro, porque a próxima execução do
pipeline reescreve o arquivo daqui e as duas versões divergem em silêncio.

## O que é escrito à mão

- `bibliography.bib` — atenção: também é editado no Overleaf, e portanto está
  sujeito ao mesmo risco de divergência que motivou a saída do `.tex`.
- `Figs estratos/`, `Figs Metodologia/` — figuras que não vêm do pipeline.
  A `Amostra efetiva de domicílios - editado.png` é a Figura 3, ainda no
  formato anterior à padronização de setembro; refazê-la exige os microdados
  da PNAD Contínua, que ficam num disco externo (`E:/Dados/PNADC`), pelo script
  `rotinas/01_amostra_efetiva.R`.

`econbr.cls` e `econbr.bst` são os arquivos de estilo da revista; não mexer.

O `econbr-sample.pdf` desta pasta é de junho de 2026 e **não reflete** as
revisões de agosto e setembro. Vale a mesma ressalva do `.tex`: o PDF corrente
é o do Overleaf.

## Como reproduzir os números

Da raiz do repositório:

```
powershell -File rotinas\_rodar_tudo.ps1
```

Isso reexecuta a identificação do erro amostral, os univariados, os
multivariados, a comparação da taxa e a regeração de todas as tabelas e
figuras desta pasta. Leva cerca de 2 horas. Para retomar de um passo
específico, use `-De N` (por exemplo, `-De 6` só regera as saídas).

O estado da revisão, com as decisões metodológicas fechadas e as armadilhas já
mapeadas, está em `docs/ESTADO_REVISAO.md`.
