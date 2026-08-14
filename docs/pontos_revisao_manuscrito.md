# Pontos de revisão do MANUSCRITO (texto/metodologia)

Pontos que dizem respeito ao **texto/metodologia do artigo** (não ao código — os bugs de código
estão nas issues #1–#19 do repositório). Para decisão dos autores (Paulo 1º autor, Caio coord.).

---

## M1 — Justificativa do uso dos pseudo-erros para o erro amostral

**Onde:** Seção 3.1 (Metodologia), parágrafo dos pseudo-erros.

**Texto atual (aprox.):** *"Dada a restrição de acesso aos microdados completos da pesquisa,
utilizou-se o método dos pseudo-erros, conforme Silva (2002)."*

**Problema (2 partes):**
1. **Imprecisão / inconsistência interna.** Os microdados da PNADc são **públicos** e trazem as
   chaves de identificação/ligação: **`UPA` + `V1008` + `V1014`** (domicílio ao longo das 5
   entrevistas), `V1016` (nº da entrevista), e para pessoas `V2003` (ordem), `V2007` (sexo),
   `V2008/V20081/V20082` (data de nascimento). Ou seja, **não há "restrição de acesso"**. Pior: a
   **própria introdução** do artigo cita **Guinesi et al. (2026)**, que usam a estrutura
   longitudinal da PNADc para transições individuais — reconhecendo que a ligação é usável.
   Justificar os pseudo-erros por "acesso" contradiz essa citação.
2. **A razão REAL (observação da Luna Hidalgo / IBGE):** a **ligação longitudinal não é confiável**,
   sobretudo nos **períodos iniciais** da PNADc — a cada nova visita, o **número de ordem** das
   pessoas/domicílios (`V2003`) **não se mantém**, de modo que a chave direta não pareia sozinha; é
   preciso um **procedimento de verificação/re-pareamento** (por sexo, data de nascimento,
   relação no domicílio). Esse pareamento tem erro e custo. **É essa a justificativa correta** para
   preferir os pseudo-erros — não "acesso".

**Recomendação:**
- **Mínimo (reescrever a justificativa):** ex.: *"A ligação longitudinal do painel da PNADc é
  imperfeita — o número de ordem das unidades não se mantém entre visitas, especialmente nos
  períodos iniciais, exigindo um procedimento de pareamento sujeito a erro. Por isso, optou-se pelo
  método dos pseudo-erros (Silva & Cruz, 2002), que dispensa a ligação das unidades no tempo."*
  E remover a inconsistência com a citação de Guinesi (2026).
- **Ambicioso (upgrade, agenda futura):** implementar o pareamento com verificação e estimar as
  covariâncias do erro amostral **diretamente** das unidades sobrepostas, ou modelar as **5 ondas**
  num STM multivariado (Pfeffermann 1991; van den Brakel & Krieg 2015) — o que também trataria o
  **rotation group bias**, ignorado pelos pseudo-erros. Ver `docs/pseudo_erros_referencias.md`.

**Evidência empírica (teste próprio, coorte de 1ª visita 2012Q1 → 2013Q1, MG, 2.657 domicílios):**
- **Chave do domicílio** (`UPA+V1008+V1014`): **confiável** — retenção ~94% nas 5 visitas (2012Q2–2013Q1: 95,0/93,7/93,9/94,3%), com `V1016` progredindo 1→2→3→4→5 (confirma a rotação de 5 trimestres).
- **Chave de pessoa** (nº de ordem `V2003`): **NÃO confiável sozinha** — pareando por sexo+data de nascimento, **8,5% das pessoas mudam de ordem** entre visitas e **23,6% dos slots (domicílio×ordem) apontam para pessoas diferentes**. Ou seja, ligar pessoas só pela ordem erra ~1/4 dos casos já em 2012 → **confirma a observação de Luna Hidalgo (IBGE)**. A ligação de pessoas é possível, mas **exige pareamento verificado** (sexo/nascimento/relação), com erro residual. *(Script: `scratchpad/test_chave.R`.)*

**Refs:** Silva & Cruz (2002); Pfeffermann (1991, JBES 9(2)); van den Brakel & Krieg (2015); Guinesi et al. (2026).

---

## M2 — Rotation group bias nos ocupados (evidência quantitativa)

**Onde:** Seção 3.1 (Metodologia) e Limitações. Complementa o M1.

**O ponto:** o M1 já registra que os pseudo-erros ignoram o *rotation group bias*. O
diagnóstico em `docs/verificacoes/V2_rotation_group_bias.md` mostra que, nestes dados,
isso não é uma ressalva formal — é um efeito medido e grande:

- **Ocupados:** viés monotônico por número de entrevista (1ª ~+4%, 5ª ~−3% em relação à
  média do trimestre), **significativo em 8 das 9 regiões**;
- **Desocupados:** **nenhum** efeito detectável (0 de 9);
- O viés **cresce ao longo da série** (amplitude em MG: 3,4 p.p. em 2012-14 → 9,2 p.p. em
  2021-24), o que deposita **tendência dentro do pseudo-erro** dos ocupados (31 de 45
  séries com tendência significativa, R² ≈ 0,21, contra 11/45 e R² ≈ 0,06 nos desocupados).

**Por que importa:** o artigo estima tendências, e nos ocupados a componente calibrada como
"erro amostral" contém tendência. Há risco de má-alocação entre sinal e erro amostral na
própria dimensão de interesse. É também a explicação da inviabilidade do MA(1) para os
ocupados (V1 §1.1): |rho1| = 0,60–0,78 excede o teto de 0,5 do MA(1) porque a
autocorrelação está inflada por algo que não é erro amostral.

**Recomendação:**
- **Mínimo:** declarar nas limitações, com os números acima, que o RGB afeta os ocupados e
  não os desocupados, e que os pseudo-erros não o contemplam.
- **Ambicioso:** modelar os efeitos de grupo de rotação no próprio modelo em espaço de
  estados (Pfeffermann 1991, JBES 9(2); van den Brakel & Krieg 2015), tratando o viés em
  vez de absorvê-lo no erro amostral. É a solução da literatura e resolveria M1 e M2 juntos.

**Refs:** Bailar (1975, JASA 70); Pfeffermann (1991); van den Brakel & Krieg (2015).

---

*(próximos pontos de revisão do manuscrito entram aqui — ex.: fórmula da variância da taxa (issue #3),
especificação da sazonalidade estocástica (issue #8), benchmarking declarado como limitação, etc.)*
