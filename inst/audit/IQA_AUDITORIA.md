# Auditoria técnico-científica do IQA e do IET — tikatuwq 0.11.0

- **Data:** 2026-10-15 (2ª rodada)
- **Branch:** `fix/iqa-scientific-stabilization`; base `4e88fa9` (v0.10.0, CRAN)
- **Documentos complementares nesta pasta:**
  - `VALIDACAO_DBO_NITROGENIO.md`: fronteiras e decisões de DBO e NT;
  - `MATRIZ_VALIDACAO_QI.csv`: um trecho por linha, com fonte, status e limites laterais;
  - `REGRESSAO_0_10_0_0_11_0.csv`: 131 amostras, com entradas, qi, IQA bruto/exibido e classes;
  - `fronteiras_qi.csv`: qi em b − ε, b e b + ε (ε = 1e-6), atual e v0.10.0;
  - `sensibilidade_variantes.csv`: efeito das escolhas em disputa;
  - `curvas_qi_pacote.png`: curvas do pacote nos eixos da Figura 1;
  - `RELATORIO_TESTES_CHECK.md`: comandos, contagens e saída dos checks.
- **Scripts:** `audit/` (fora do pacote). A verificação independente gera
  `audit/resultados/verificacao_independente.csv`.

> Testes internos passarem não é validação oficial. Nenhum item abaixo tem
> endosso institucional da CETESB, ANA ou IGAM.

## 1. Cadeia de evidências

Categorias usadas em todo o material:
- **(a) oficial literal:** texto ou nota da fonte oficial;
- **(b) secundária rastreável:** equação encontrada em fonte secundária identificada;
- **(c) derivação própria:** escolha do pacote sem transcrição direta;
- **(d) hipótese não verificada / sem fonte.**

| Fonte | Conteúdo | Uso |
|---|---|---|
| CETESB (2023), *Apêndice D — Índices de Qualidade das Águas e Critérios de Avaliação da Qualidade dos Sedimentos*, seção 2 (pp. 3–5), Figura 1, Tabela 1; seção 4 (pp. 9–11) | Fórmula do IQA, pesos, curvas **gráficas** dos qi, valores fixos, regras de E. coli, PO₄, dados ausentes, temperatura, classes; equações literais do IET | (a) |
| CPRH, PNMA II — *Índices e indicadores de qualidade da água: seleção preliminar e material de apoio* (Pernambuco), seção 4.1 "IQA-CETESB", fórmulas da planilha Excel da CETESB. http://www.cprh.pe.gov.br/downloads/indice-agua-volume2.pdf (acesso 2026-10-15) | Equações por trecho dos 9 qi; Cs(T, h); qi de temperatura = 94 | (b) |
| von Sperling, M. (2007). *Estudos e modelagem da qualidade da água de rios*, p. 254–255, **via** IBEAS (2015), Oliveira & Xavier, VI Congresso Brasileiro de Gestão Ambiental, trabalho VIII-041, Tabela 2. https://www.ibeas.org.br/congresso/Trabalhos2015/VIII-041.pdf | Mesmas equações, com LOG10 em DBO/NT e outra reta em pH 9–10; exemplo numérico (IQA = 81) e qi calculados (Tabelas 4–12) | (b) e gabarito externo |
| ANA, Portal de Qualidade das Águas — IQA (acesso 2026-10-15) | Faixas 19/36/51/79 | (a) para `classification = "ana"` |
| IGAM, Índice de Qualidade das Águas — IQA (acesso 2026-10-15) | Faixas e regras próprias; tabela de classes em imagem | Rótulos IGAM: (d), pendentes |

**Achado central.** A CETESB publica os qi **só como curvas**. As equações do
pacote são o ajuste da planilha CETESB transcrito por terceiros. As duas
transcrições concordam em quase tudo e divergem em quatro pontos:

| Ponto | CPRH | von Sperling via IBEAS | CETESB oficial | Pacote 0.11.0 |
|---|---|---|---|---|
| DBO 5–15 | `log C` (base ambígua) | `LOG10` | curva compatível só com `ln` | `ln` |
| NT 10–60 | `log C` | `LOG10` | curva compatível só com `ln` | `ln` |
| pH 9–10 | `228 − 27 pH` (dá qi < 0: erro tipográfico) | `50 − 32 (pH − 9)` | curva decrescente, sem salto em 9 | `288 − 27 pH` |
| OD > 140 % | `47 + 3` = 50 | `3 + 47` = 50 | **47** (nota da Figura 1) | 47 |

## 2. Situação por componente

Resumo. O detalhe por trecho está em `MATRIZ_VALIDACAO_QI.csv`: 13 trechos (a), 27 (b), 3 (b) em conflito e 1 (d).

| Componente | Situação | Status |
|---|---|---|
| Agregação ∏ qi^wi e pesos | Confere com a p. 3 e a Figura 1 | (a) |
| Classes CETESB (Tabela 1) | Confere; intervalos fechados à direita; **aplicadas ao IQA não arredondado** (mudança da 0.11.0) | (a) |
| OD | Trechos (b), confirmados por gabarito IBEAS (±0,15). Cs corrigido: 0,00005898 → 0,00005896 (CPRH e IBEAS; efeito < 0,02%). Cs não consta do Apêndice D. OD > 140% = 47 (oficial; secundárias dizem 50) | (a)+(b) |
| Coliformes / E. coli | Trechos (b), gabarito IBEAS ok; > 10⁵ → 3 e E. coli × 1,25 (a); fator aplicado uma única vez | (a)+(b) |
| pH | 8,5–9: `exp` (b), confirmado pela curva. 9–10: `288 − 27 pH`, **(b) em conflito; decisão pendente** (efeito máximo 0,9 ponto de IQA) | (b) |
| DBO | 5–15: `ln`, (b) em conflito, resolvido pela curva. Salto +0,39 em 15 é do ajuste. > 30 → 2 (a) | ver `VALIDACAO_DBO_NITROGENIO.md` |
| NT | 10–60: `ln`, mesmo caso. Saltos +0,03 em 60 e +0,056 em 100 documentados | idem |
| PT | Trechos (b), gabarito IBEAS ok; P × 3,066 → PO₄ (a). A curva é de fosfato; o pacote não confunde com ortofosfato medido | (a)+(b) |
| Temperatura padrão | qi = 94: CPRH e IBEAS ("constante de 94 adotada pela CETESB"); compatível com q^w = 1,58 da p. 5 (94^0,1 = 1,5751). Não é valor oficial exato demonstrado | (b) |
| Temperatura `delta`/`reference` | **Curva sem fonte** e divergente da q6 (pico ~93 em 0; ~30 em 10; o código dá 100 e 68). ΔT < −5 → `NA` (a). Modos marcados como **experimentais**: aviso e `iqa_nonstandard = "experimental_temperature_curve"` | (d) |
| Turbidez | Trechos (b); > 100 → 5 (a); o salto em 100 é da regra oficial | (a)+(b) |
| Sólidos totais | Trechos (b); > 500 → 32 (a); TDS continua não aceito como ST no modo estrito | (a)+(b) |

## 3. Verificações independentes

| Verificação | Resultado |
|---|---|
| Reimplementação do zero (`audit/iqa_independent.R`, sem funções do pacote) em 3000 entradas determinísticas cobrindo todos os trechos, com altitude de 0 a 2000 m | max \|dif\| = 1,4e-14; 0 classes diferentes. Detecta erros de implementação, **não** valida coeficientes |
| Gabarito externo IBEAS / von Sperling: 21 valores de qi fora dos trechos em disputa | Todos dentro de ±0,06 (±0,15 em OD) |
| Exemplo de von Sperling (2007, p. 254) | IQA = 81, reproduzido |
| IBEAS, DBO 12 mg/L → 70,60 | Diverge de propósito (log10 × ln); fixado em teste |
| Comparação visual com a Figura 1 | Rejeita a v0.10.0 (erros de 40–80 qi); tolerância de 8 pontos nos testes |
| Sensibilidade às variantes em disputa | log10 em DBO/NT: até +14 pontos; pH 9–10: ≤ 0,9; OD > 140 = 50: ≤ 0,75; só a variante log10 muda classe (1 caso) |

## 4. Regressão 0.10.0 → 0.11.0 (`REGRESSAO_0_10_0_0_11_0.csv`)

| Conjunto | n | IQA alterado (\|Δ\| > 0,005) | Maior \|Δ\| | Classe alterada |
|---|---:|---:|---:|---:|
| Grade de fronteiras | 86 | 18 | 60,71 | 6 |
| Gradiente de classes | 25 | 17 | 4,14 | 2 (Boa → Regular) |
| `wq_demo` | 20 | 3 | 66,97 | 1 |

A amostra do `wq_demo` FBS-BRH-300 de 24/05/2023 foi de 15,58 (Péssima) para
82,55 (Ótima). Auditoria: pH = 8,53, logo acima da fronteira de 8,5. Na
v0.10.0, qi_pH = 0,0002, e na média geométrica esse único termo derrubava o
IQA. Agora qi_pH = 77,3, contínuo com 79,5 do lado esquerdo. **Atenção:** o
`wq_demo` não tem sólidos totais (só TDS), então **todos os 20 IQAs são
estimativas parciais com 8 parâmetros**, não IQA CETESB. Também não existe IQA
publicado para essas amostras, então o `wq_demo` não serve de benchmark.

## 5. Outros módulos

| Módulo | Achado | Ação |
|---|---|---|
| `iet_lamparelli()` | **P0.** Usava `10 + 10·log10(x)` para PT e CL, igual em rio e reservatório, sem fonte. Com PT = 137 µg/L em rio dava 1,4 (correto: 59,5). Os modos vetorial e data frame usavam unidades diferentes | **Corrigido** com as equações literais da p. 10 (a). Reproduz os cortes das Tabelas 5 e 6 (±0,6). Secchi vira componente auxiliar fora do agregado (a CETESB não usa transparência) |
| `iet_carlson()` | **P0.** `log10` em vez de `ln`. Carlson (1977) define TSI = 10 (6 − log₂ SD), e os coeficientes 14,41/14,42/9,81 só valem com ln. TP = 48 µg/L dava 28,4 (definição: 60) | **Corrigido**; testes nos valores de definição |
| `classify_tsi_carlson()` | Cortes 30/40/50/70: convenção da literatura (Carlson & Simpson 1996), não do artigo de 1977 | Mantido; (b) |
| `generate_analysis()` | Classe dominante por escala própria 25/50/70/90; IQA parcial sem aviso | Usa `classify_iqa()` (CETESB) e informa amostras parciais |
| `conama_limits()` | Classes 1–4: OD, DBO, turbidez, pH, coliformes, clorofila-a e P por ambiente conferidos com a Resolução 357/2005 | Sem mudança. Não implica conformidade jurídica completa |
| `plot_map_quality()` | Detecção automática procurava `IET_Lamparelli`, mas `iet_lamparelli()` devolve `IET_Lamp`; o teste correspondente era pulado em silêncio | Corrigido; teste agora obrigatório |
| `inst/CITATION` | Versão fixa no texto | Usa `meta$Version`. O DOI do Zenodo é de release anterior; **depositar a 0.11.0 é decisão do mantenedor** |

## 6. Pendências que pedem decisão do mantenedor

| # | Prioridade | Item | Alternativas e consequências |
|---|---|---|---|
| 1 | P1 | pH 9–10 (EQ-03) | (i) manter `288 − 27 pH` (CPRH com o dígito corrigido; contínua; coerente com a curva); (ii) voltar a `50 − 32 (pH − 9)` (von Sperling/IBEAS; salto +5 em pH 9, contrário à curva). Efeito ≤ 0,9 ponto, sem mudança de classe nos testes |
| 2 | P1 | Curva de ΔT (EQ-07/08) | (i) manter como experimental, com aviso e marcação (atual); (ii) desativar `delta`/`reference` até haver fonte; (iii) ajustar à curva q6 por digitalização documentada, com erro estimado e aprovação. A opção (iii) é proibida sem aprovação explícita |
| 3 | P2 | Rótulos IGAM | Conferir a tabela-imagem do portal IGAM |
| 4 | P2 | von Sperling (2007), p. 255 | Consultar o livro para saber se o `LOG10` é do autor ou do IBEAS |
| 5 | P2 | Zenodo | Depositar a 0.11.0 e atualizar o DOI do README |
| 6 | backlog | COT → DBO (p. 5: DBO = 0,510658 · COT^1,27831) | Opção futura configurável e marcada como estimativa |

## 7. Migração para usuários

- `classify_iqa(x, scheme = …)` → `classification = …`; `iqa(df, na_rm = TRUE)` → `allow_partial = TRUE`.
- `pesos` com nome desconhecido (ex.: `tds`) agora dá erro.
- `temperature_method = "delta"/"reference"`: aviso, marcação experimental, ΔT < −5 → `NA`.
- **Recalcule** séries com pH entre 8,5 e 10, DBO entre 5 e 15 mg/L ou NT entre 10 e 60 mg/L.
- **Recalcule todo IET**, Lamparelli e Carlson: os valores anteriores estavam errados.
- A classe do IQA agora vem do valor não arredondado (diferença só em valores
  a menos de 0,005 de um corte).
