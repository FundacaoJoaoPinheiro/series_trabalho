# Manuscrito — versão canônica

Esta pasta é a **fonte única de verdade** do manuscrito submetido à Revista
Brasileira de Economia. Foi trazida para dentro do repositório em 16/08/2026,
para que texto, tabelas, figuras e código fiquem versionados juntos.

> **Atenção:** existe uma cópia antiga em
> `PROJETO_Estatísticas de mercade de trabalho\Artigo Estratos Geográficos\Versão atual`.
> Ela está **obsoleta** a partir desta data. Editar lá não tem efeito nenhum:
> o `rotinas/25_saidas_artigo.R` grava aqui, e é daqui que sai o PDF.

## O que é gerado e o que é escrito à mão

**Não editar à mão** — sobrescrito a cada execução de `rotinas/25_saidas_artigo.R`:

- `resultados/<indicador>/tabhiper*.tex`, `diag*.tex`, `matrizcorr_*.tex`,
  `diffvicio*.tex`, `est_pontual_*.tex`
- `resultados/Taxa de desocupação/tabcomptaxa.tex`
- `resultados/modelos_arma.tex`
- as 6 figuras `resultados/<indicador>/Figura_*.png`

**Escrito à mão:**

- `econbr-sample.tex` — o texto do artigo
- `bibliography.bib`
- `Figs estratos/`, `Figs Metodologia/` — figuras que não vêm do pipeline

`econbr.cls` e `econbr.bst` são os arquivos de estilo da revista; não mexer.

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

## Compilação

Não há LaTeX instalado na máquina em que a revisão foi feita, portanto **o PDF
desta versão ainda não foi compilado**. Antes de submeter ou apresentar, rodar:

```
pdflatex econbr-sample && bibtex econbr-sample && pdflatex econbr-sample && pdflatex econbr-sample
```

O `econbr-sample.pdf` presente na pasta é de junho de 2026 e **não reflete** as
revisões atuais.
