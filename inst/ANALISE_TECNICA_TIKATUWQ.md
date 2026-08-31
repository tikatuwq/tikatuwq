# Análise Técnica do pacote tikatuwq — Avaliação Dual
**Especialista em R · Doutor em Ciências Ambientais / Qualidade da Água**  
Versão avaliada: 0.8.2 | Data: junho/2026

---

## 1. Contexto e propósito

O **tikatuwq** é um pacote R para análise de qualidade de água doce no contexto brasileiro. Suas responsabilidades centrais são: importação e limpeza de dados de monitoramento, cálculo dos índices IQA/WQI, IET Carlson e Lamparelli e NSF WQI, verificação de conformidade com a Resolução CONAMA 357/2005, visualizações estáticas e interativas, e geração de relatórios reprodutíveis. O pacote está publicado no CRAN (v0.8.2) com 0 erros/avisos/notas e usa dados reais do Rio Buranhem (INEMA/BA, 2021–2024) como conjunto de demonstração.

---

## 2. Avaliação — Perspectiva R

### 2.1 Pontos fortes

- Estrutura modular limpa (cada arquivo `.R` com responsabilidade única)
- Cobertura de testes adequada: 17 arquivos `testthat`, abrangendo todos os módulos principais
- Conformidade CRAN rigorosa — escrita apenas em `tempdir()`, nenhuma dependência de arquivo externo em exemplos
- Suporte bilíngue (PT/EN) nas labels via argumento `locale`
- Tratamento robusto de entradas: dados censurados `<LD/<LOQ`, separador decimal vírgula/ponto, alias de nomes de coluna (`ph`/`pH`, `temp`/`temperatura`)
- Uso do pipe nativo `|>` com dependência mínima de R ≥ 4.1
- CI/CD configurado: `R-CMD-check` e `pkgdown` via GitHub Actions
- Dataset real publicado com DOI Zenodo

### 2.2 Problemas de código identificados

| Arquivo | Problema | Severidade |
|---|---|---|
| `R/iqa.R` | Usa média **aritmética** ponderada; CETESB/NSF usam média **geométrica** | 🔴 CRÍTICO |
| `R/iqa_equations.R` | Implementação correta (geométrica) existe como `iqa_official()` mas está `@noRd` — invisível ao usuário | 🔴 CRÍTICO |
| `R/testando.R` | Arquivo vazio — não deve existir no pacote CRAN | 🟡 MÉDIO |
| `R/wq_buranhem.R` | Documenta `wq_demo` como `"wq_demo"` duplicando `data_wq_demo.R` — gera confusão | 🟡 MÉDIO |
| `R/nsfwqi.R` | Autodescrição como "prototype" num pacote publicado prejudica credibilidade | 🟡 MÉDIO |
| `inst/extdata/conama_limits.csv` | Apenas 38 linhas/7 parâmetros — CONAMA 357 tem dezenas de parâmetros regulados | 🟠 ALTO |
| `conama_check()` | Verificação por linha apenas — não implementa regra de 80%/≥6 amostras da norma | 🔴 CRÍTICO |

---

## 3. Avaliação — Perspectiva Ciências Ambientais

### 3.1 Bug crítico: método de agregação do IQA

O IQA CETESB (e o NSF WQI original de Brown et al., 1970) usa **média geométrica ponderada**:

```
IQA = Π(Qi^Wi)   com   ΣWi = 1
```

A função pública `iqa()` calcula:

```
IQA = Σ(Qi × Wi) / Σ(Wi_presentes)   ← ARITMÉTICA — ERRADO
```

A implementação correta já existe internamente como `iqa_official()` (produto geométrico, com OD calculado via % saturação, dependente de temperatura e altitude), mas está marcada `@noRd` e inacessível. Resultado: qualquer valor de IQA gerado pelo pacote atual está metodologicamente incorreto. Este seria o primeiro ponto levantado numa defesa de tese ou revisão por pares.

**Correção necessária:** expor `iqa_official()` como `method = "CETESB"` dentro de `iqa()`, tornando-o o default, e renomear o método atual como `method = "NSF_approx"` (média aritmética com curvas interpoladas).

### 3.2 Bug crítico: conformidade CONAMA por frequência

A Resolução CONAMA 357/2005 especifica (Art. 15, §4° e tabelas): parâmetros como coliformes termotolerantes e OD devem ser atendidos em **≥80% das amostras** quando há **≥6 coletas por estação/ano**. O `conama_check()` verifica cada linha independentemente, ignorando esta regra estatística. Um monitoramento com 10 amostras onde 3 violam o limite seria classificado como "conforme" linha a linha, mas está em **não-conformidade legal**. Isso é uma falha técnica grave para uso regulatório.

### 3.3 Tabela de limites CONAMA incompleta

O arquivo `conama_limits.csv` cobre apenas 7–8 parâmetros das classes 1–4. A Resolução CONAMA 357/2005 regula dezenas de parâmetros adicionais para águas doces, incluindo:

- Metais: As, Cd, Pb, Cr total, Cr VI, Cu, Hg, Ni, Zn, Fe, Mn, Ba, Al
- Nutrientes: NO₃⁻, NO₂⁻, NH₃, P total (distinção lêntico/lótico/intermediário)
- Orgânicos: fenóis, surfactantes, óleos e graxas
- Cianobactérias e clorofila-a (presentes, mas incompletos)
- Parâmetros físicos: cor verdadeira (presente apenas nas classes 2–3)

Sem estes parâmetros, o pacote não pode ser usado como ferramenta completa de auditoria CONAMA, limitando seu uso prático por órgãos ambientais.

### 3.4 Ausência de análise sazonal

Em bacias tropicais brasileiras, a sazonalidade (período chuvoso vs. seco) é o principal fator de variação na qualidade da água — frequentemente mais relevante que tendências de longo prazo. O pacote tem análise de tendência temporal robusta (Theil-Sen, OLS, LOESS) mas **não tem nenhuma função para comparação entre períodos hidrológicos**, para teste de Mann-Kendall sazonal (Hirsch et al., 1982), nem para atribuição de amostras ao período seco/chuvoso baseada em calendário hidrológico regional.

### 3.5 IET Lamparelli: distinção lêntico/lótico

A função `iet_lamparelli()` aceita o argumento `ambiente = "rio"/"reservatorio"` com limiares diferentes, o que é correto. Porém não há função de visualização dedicada para o IET (apenas `plot_iqa()` para o IQA), nem integração do IET no template de relatório. O IET é frequentemente o índice mais relevante para reservatórios e lagos — mais de 50% dos corpos d'água regulados pelo INEMA/BA são lênticos.

---

## 4. Tabela de novas funcionalidades — Ordem de prioridade

| # | Funcionalidade | Módulo / Função proposta | Perspectiva R | Perspectiva Ambiental | Impacto na tese | Esforço estimado |
|---|---|---|---|---|---|---|
| **P0** | **Corrigir `iqa()` para usar média geométrica (CETESB oficial)** | `iqa()` com `method = "CETESB"` como default; expor `iqa_official()` | Refatoração de 1 função | CRÍTICO — método errado invalida resultados | ⭐⭐⭐⭐⭐ | Baixo (código já existe) |
| **P1** | **Conformidade CONAMA por frequência (regra dos 80%/≥6 amostras)** | `conama_freq_check(df, classe, min_n=6, threshold=0.8, by=c("ponto","ano"))` | Nova função com `dplyr::group_by` por ponto+ano | CRÍTICO — é exatamente o que a norma exige | ⭐⭐⭐⭐⭐ | Médio |
| **P2** | **Expandir tabela CONAMA 357 com todos os parâmetros regulados** | Ampliar `inst/extdata/conama_limits.csv` | Dados + testes | Essencial para uso regulatório real | ⭐⭐⭐⭐⭐ | Médio (pesquisa + digitação) |
| **P3** | **Análise por período hidrológico (seco/chuvoso)** | `assign_season(df, region="nordeste"/"sudeste"/"sul"/"centro_oeste"/"norte")` + `compare_seasons(df, param, test="wilcoxon")` | Novo módulo; depende de `stats` | Fundamental para bacias tropicais | ⭐⭐⭐⭐⭐ | Médio |
| **P4** | **Visualização e relatório do IET** | `plot_iet(df)` + integração no `render_report()` | Novo plot ggplot2 | IET é principal índice para ambientes lênticos | ⭐⭐⭐⭐ | Baixo |
| **P5** | **Completar e validar NSF WQI** | `nsfwqi()` com curvas Brown et al. (1970) completas; remover rótulo "prototype" | Substituir lógica simplificada | NSF WQI é padrão internacional comparável | ⭐⭐⭐⭐ | Médio |
| **P6** | **Teste de Mann-Kendall sazonal** | `mk_seasonal(df, param, season_col="estacao", alpha=0.05)` | Depende de `trend` ou implementação interna | Hirsch et al. (1982) — padrão em hidrologia | ⭐⭐⭐⭐ | Médio |
| **P7** | **Cálculo de carga poluidora** | `compute_load(df, param, flow_col="vazao", unit_out="kg_dia")` | Operação vetorial simples | Essencial para gestão de bacias e PBHC | ⭐⭐⭐⭐ | Baixo |
| **P8** | **Probabilidade de excedência** | `exceedance_prob(df, param, threshold, by="ponto")` | Frequência relativa + IC bootstrap | Análise de risco ambiental e licenciamento | ⭐⭐⭐ | Baixo |
| **P9** | **Integração com dados públicos ANA/HidroWeb** | `fetch_hidroweb(codigo_estacao, data_ini, data_fim)` | HTTP via `httr2`; CRAN Suggests | Reprodutibilidade total — dados sem download manual | ⭐⭐⭐⭐ | Alto |
| **P10** | **Exportação para Excel com formatação condicional** | `export_wq_xlsx(df, file, highlight_violations=TRUE)` | `openxlsx2` em Suggests | Formato padrão de órgãos ambientais no Brasil | ⭐⭐⭐ | Baixo-médio |
| **P11** | **Índice de Qualidade para Irrigação (IQI)** | `iqi(df)` baseado em FAO/Ayers & Westcot (1985) | Novo módulo de índice | Brasil é 3° maior irrigante do mundo — uso agronômico | ⭐⭐⭐ | Médio |
| **P12** | **Mapa coroplético de qualidade (IQA/IET por ponto)** | `plot_map_quality(df, index="IQA", palette="RdYlGn")` | Extender `plot_map()` com colorização por índice | Visualização essencial para relatórios técnicos | ⭐⭐⭐ | Baixo |
| **P13** | **Suporte a dados de cianobactérias e floração** | `blooms_risk(df, classe="2")` baseado em CONAMA 357 + OMS | Novo módulo especializado | Cianobactérias — emergência ambiental no Nordeste | ⭐⭐⭐ | Médio |
| **P14** | **Relatório bilíngue PT/EN completo** | Template Rmd com seções em PT-BR; `render_report(lang="pt")` | Parâmetro `lang` no template | Agências brasileiras exigem relatórios em PT | ⭐⭐⭐ | Médio |
| **P15** | **Análise multivariada básica (PCA de parâmetros)** | `wq_pca(df, params=NULL, plot=TRUE)` wrapper sobre `stats::prcomp` | Wrapper simples; retorna `ggplot` | Identifica gradientes de qualidade entre pontos | ⭐⭐⭐ | Baixo |
| **P16** | **Balneabilidade (CONAMA 274/2000)** | `balnear_check(df, uso="recreacao_contato_primario")` | Novo módulo; tabela de limites separada | Gestão de praias fluviais e reservatórios | ⭐⭐ | Baixo-médio |
| **P17** | **Lifecycle badges nas funções experimentais** | Adicionar `{lifecycle}` como Suggests; `@lifecycle::experimental` em `nsfwqi()` | Boa prática CRAN/rOpenSci | Comunicação clara ao usuário sobre maturidade | ⭐⭐ | Baixo |
| **P18** | **Remover `testando.R` e limpar `wq_buranhem.R`** | Deletar arquivo vazio; unificar documentação do dataset | Higiene de código CRAN | Sem impacto científico, mas necessário para CRAN | ⭐ | Muito baixo |

---

## 5. Detalhe das funcionalidades prioritárias (P0–P3)

### P0 — Corrigir `iqa()` para média geométrica

A função `iqa_official()` já implementa corretamente o produto geométrico com OD calculado como % saturação (temperatura + altitude). A correção consiste em:

1. Renomear `iqa_official()` para público, adicionando-a como `method = "CETESB"` em `iqa()`
2. Tornar `"CETESB"` o default e renomear o método atual para `"NSF_approx"`
3. Adicionar argumento `altitude_m = 0` ao `iqa()` público (para cálculo de saturação do OD em altitudes elevadas)
4. Atualizar testes: os valores de IQA mudarão com a correção
5. Documentar a diferença nos dois métodos na vignette

### P1 — `conama_freq_check()`

```r
# Exemplo de assinatura proposta:
conama_freq_check <- function(
  df,
  classe = "2",
  by = c("ponto"),        # agrupamento (ponto, rio, ano, etc.)
  date_col = "data",      # para extrair o ano automaticamente
  min_n = 6,              # mínimo de amostras para aplicar a regra
  threshold = 0.80        # fração mínima de conformidade
)
# Retorna: tibble com ponto, ano, parametro, n, n_ok, pct_ok,
#          freq_conforme (logical), aplicou_regra (logical: n >= min_n)
```

A função deve agregar por `by + ano`, contar total de amostras e amostras conformes por parâmetro, e aplicar a regra dos 80% apenas quando n ≥ 6 (caso contrário, retornar `NA` para `freq_conforme` e sinalizar `aplicou_regra = FALSE`).

### P2 — Expandir `conama_limits.csv`

Parâmetros faltantes a incluir (Tabela I, CONAMA 357/2005):

- Metais: Al, As, Ba, Cd, Co, Cr total, Cu, Fe dissolvido, Mn, Hg, Ni, Pb, Se, Zn
- Inorgânicos: cloretos, fluoretos, sulfatos, sulfetos, nitratos (como N), nitritos, amônia (como N, pH-dependente)
- Orgânicos: fenóis totais, surfactantes, óleos e graxas, BTEX, PAH
- Radiológico: radioatividade (alfa/beta total)

Cada parâmetro acrescenta linhas ao CSV com as quatro classes. Estimativa: ~120 novas linhas.

### P3 — Análise sazonal

```r
# Calendários hidrológicos por região do Brasil
assign_season <- function(df, date_col = "data",
                          region = c("nordeste","sudeste","sul",
                                     "centro_oeste","norte","custom"),
                          dry_months = NULL)   # para "custom"
# Retorna df com coluna season: "chuvoso" / "seco"

compare_seasons <- function(df, param,
                             season_col = "season",
                             test = c("wilcoxon","t_test","kruskal"),
                             by = c("ponto"),
                             plot = TRUE)
# Retorna: tibble com estatísticas + p-valor + ggplot de boxplots comparativos
```

Referências de calendário hidrológico: ANA (2019) — Atlas Estiagens; Marengo et al. (2011).

---

## 6. Checklist para submissão ao CRAN (v0.9.0)

### Correções obrigatórias antes da submissão

- [ ] **P0**: Corrigir `iqa()` para usar `method = "CETESB"` (geométrico) como default
- [ ] **P18**: Deletar `R/testando.R`; resolver duplicidade `wq_buranhem.R`
- [ ] **P5**: Atualizar `nsfwqi()` — remover menção a "prototype" ou adicionar lifecycle badge
- [ ] Atualizar todos os testes afetados pela correção do IQA (valores mudarão)
- [ ] Rodar `devtools::check(args = "--as-cran")` com 0 errors/warnings/notes
- [ ] Rodar `urlchecker::url_check()` — verificar links no README/vignettes
- [ ] Atualizar `DESCRIPTION`: bumpar versão para `0.9.0`, atualizar `Date`
- [ ] Atualizar `NEWS.md` com todas as mudanças da v0.9.0
- [ ] Atualizar `CITATION.cff` com nova versão e data

### Para cada nova função adicionada

- [ ] Roxygen completo (`@param`, `@return`, `@examples`, `@family`, `@seealso`)
- [ ] Pelo menos 1 teste `testthat` com casos normais e casos extremos (NA, n < min_n, etc.)
- [ ] Incluir na vignette relevante ou criar vignette própria se módulo novo
- [ ] Verificar que exemplos rodam em < 5 segundos (limite CRAN)

### Checklist GitHub (pré-push para main)

- [ ] Branch de desenvolvimento `dev/v0.9.0` → PR → revisão → merge em `main`
- [ ] Tag `v0.9.0` após merge
- [ ] Rodar `pkgdown::build_site()` e conferir referência de funções
- [ ] Atualizar `README.md` e `README-pt.md` com novas funções e badge de versão
- [ ] Conferir que o DOI Zenodo está atualizado (novo release dispara novo DOI)

---

## 7. Versioning sugerido

| Versão | Conteúdo | Alvo |
|---|---|---|
| **v0.8.3** | Hotfix: corrigir `iqa()` (P0) + deletar `testando.R` (P18) | CRAN imediato |
| **v0.9.0** | P1 (`conama_freq_check`) + P2 (tabela CONAMA expandida) + P4 (`plot_iet`) + P7 (`compute_load`) + P8 (`exceedance_prob`) + P12 (`plot_map_quality`) | CRAN ~4–6 semanas |
| **v1.0.0** | P3 (análise sazonal) + P5 (NSF WQI completo) + P6 (Mann-Kendall sazonal) + P10 (Excel export) + P14 (relatório PT bilíngue) + P15 (PCA) | CRAN ~3–4 meses |
| **v1.1.0** | P9 (ANA/HidroWeb) + P11 (IQI irrigação) + P13 (cianobactérias) + P16 (balneabilidade) | CRAN ~6 meses |

---

## 8. Referências metodológicas para as novas funções

- Brown, R.M. et al. (1970). A Water Quality Index — Do We Dare? *Water and Sewage Works*, 117, 339–343.
- CETESB (2021). *Qualidade das Águas Superficiais no Estado de São Paulo*. CETESB, São Paulo. [IQA metodologia]
- CONAMA (2005). Resolução 357/2005 — Classifica os corpos d'água e estabelece condições para seu enquadramento. DOU.
- CONAMA (2000). Resolução 274/2000 — Balneabilidade.
- Hirsch, R.M., Slack, J.R., Smith, R.A. (1982). Techniques of trend analysis for monthly water quality data. *Water Resources Research*, 18(1), 107–121.
- Lamparelli, M.C. (2004). *Grau de trofia em corpos d'água do estado de São Paulo*. Tese de Doutorado, USP.
- Carlson, R.E. (1977). A trophic state index for lakes. *Limnology and Oceanography*, 22(2), 361–369.
- Ayers, R.S., Westcot, D.W. (1985). *Water quality for agriculture*. FAO Irrigation and Drainage Paper 29 Rev. 1.
- Marengo, J.A. et al. (2011). The drought of 2010 in the context of historical droughts in the Amazon region. *Geophysical Research Letters*, 38.
- ANA (2019). *Atlas Estiagens: secas e estiagens no Brasil*. Agência Nacional de Águas, Brasília.
