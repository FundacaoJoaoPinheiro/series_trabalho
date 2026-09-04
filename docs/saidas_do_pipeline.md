# O que o pipeline produz, e para onde vai

Este repositório versiona **código**. O manuscrito — texto, bibliografia,
arquivos de estilo da revista e PDFs — não fica aqui: é escrito e revisado no
Overleaf, projeto `697bbbf567ec83d3bfcd67f0`, com controle de alterações.

A razão é prática. Uma cópia do texto dentro do repositório envelhece em
relação ao Overleaf sem avisar, e um repositório com tabelas novas e texto
velho engana mais do que um repositório sem o texto.

## Onde as saídas são gravadas

`rotinas/25_saidas_artigo.R` grava em dois lugares:

| Saída | Destino | Versionado |
|---|---|---|
| 21 tabelas `.tex` | `artigo/resultados/` | não |
| 6 figuras `Figura_*.png` | `artigo/resultados/` e `outputs/figuras_final/` | só em `outputs/` |

A pasta `artigo/` é ignorada pelo git: ela existe na máquina de quem roda o
pipeline, como área de montagem para subir ao Overleaf, e não no repositório.
O destino pode ser mudado pela variável de ambiente `ARTIGO_DIR`.

`rotinas/03_organizacao_cv.R` grava as figuras e tabelas de coeficiente de
variação em `outputs/cv/`. O trimestre final é configurável por `TRI_FINAL`
(o artigo usa `2024_04`; o padrão do script é `2025_02`).

## Regra ao levar uma tabela para o Overleaf

Subir sempre o arquivo gerado pelo pipeline. Nunca editar o `.tex` de uma
tabela dentro do Overleaf: a próxima execução reescreve o arquivo de origem e
as duas versões divergem em silêncio. Correção em tabela se faz no script que
a gera.

## Figuras que não vêm do pipeline

Mapa dos estratos, FAC/FACP do Sul de Minas e a figura da amostra efetiva de
domicílios são insumos do manuscrito, e ficam no Overleaf junto com o texto.
A da amostra efetiva (Figura 3) é a única ainda no formato anterior à
padronização de setembro de 2026; refazê-la exige os microdados da PNAD
Contínua, que ficam num disco externo, pelo script `rotinas/01_amostra_efetiva.R`.

## Como reproduzir os números

Da raiz do repositório:

```
powershell -File rotinas\_rodar_tudo.ps1
```

Reexecuta a identificação do erro amostral, os univariados, os multivariados,
a comparação da taxa e a regeração de todas as tabelas e figuras. Leva cerca
de 2 horas. Para retomar de um passo específico, use `-De N` (por exemplo,
`-De 6` só regera as saídas).

O estado da revisão, com as decisões metodológicas fechadas e as armadilhas já
mapeadas, está em `docs/ESTADO_REVISAO.md`.
