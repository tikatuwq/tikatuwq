# Relatório de implementação: motor IGAM experimental

> **Atualização (2026-10-16):** o mantenedor autorizou a liberação. O
> método IGAM agora está disponível em `iqa(df, method = "IGAM")`, com
> `available = TRUE` e `reference_validated = TRUE` (reprodução de IQAs
> publicados, **não** homologação). Ver `IGAM_RELEASE_READINESS.md` e
> `inst/methodology/IGAM_VALIDATION.md`. O texto abaixo registra o estado
> anterior à liberação.

- **Data:** 2026-10-15
- **Branch:** `fix/iqa-scientific-stabilization`, sem commit, push, tag nem
  release.
- **Situação:** **implementação experimental baseada no SCQA, com a correção
  k = 1,5 fundamentada na literatura.** Não está liberada em `iqa()`.

## 1. Resumo

| Item | Situação |
|---|---|
| Implementado | Motor IGAM independente (`.iqa_igam_compute()`, interno). Usa as 9 equações do SCQA 2005, turbidez com k = 1,5, nitrato com unidade explícita, P → PO₄ explícito, qs de temperatura = 92, regras de ausência do IGAM, censura rastreável e classificação IGAM |
| Reproduzido historicamente | 15/15 IQAs da Pampulha, 3º trim. 2017 (o conjunto da investigação). RMSE 0,02765; erro máx. 0,04475 |
| Validado em campanhas externas (pré-registrado) | 34/34 IQAs: Pampulha 3º trim. 2018 e 4º trim. 2017. RMSE 0,0298; erro máx. 0,0489. **Mesma sub-bacia** |
| Não validado | Outras bacias e regiões; nitrato alto (quase todo censurado); cloreto ausente; microbiológico < 1; regra de arredondamento |
| Bloqueado | `iqa(df, method = "IGAM")` continua gerando `tikatuwq_method_unavailable`. Estado: `available = FALSE`, `reference_validated = FALSE` |

## 2. Alterações por arquivo

### Código do pacote

- **`R/iqa_igam.R`**
  - Especificação atualizada:
    - `implemented = TRUE`;
    - `temperature_qs = 92`;
    - `turbidity_k = 1.5`;
    - `blocked_reason` só com as limitações atuais.
  - Subíndices `.igam_cs()` e `.qi_igam_*()`.
  - `.igam_check_inputs()`: agora com cloreto, temperatura e
    `phosphorus_input`.
  - `.iqa_igam_compute()`: o motor.
  - `iqa_methods()`: `limitation` e `reference` atualizados.
- **`R/utils_sanitize.R`:** novo alias canônico `cloreto`. Não altera a
  resolução de nenhuma coluna existente.
- **Documentação (roxygen):** só texto, em `R/iqa.R`, `R/iqa_equations.R` e
  `R/compare_iqa.R`. Os Rd foram regenerados (`iqa`, `iqa_components`,
  `iqa_methods`, `compare_iqa`).
- **Não alterados:**
  - as equações, os pesos e a lógica do motor CETESB;
  - `nsfwqi()`;
  - CONAMA, IET, balneabilidade, Mann-Kendall e relatórios;
  - a API de `iqa()`, sem `classification`.

### Testes

- **`tests/testthat/test-iqa-igam-engine.R`** (novo), cobrindo:
  - fronteiras dos qi e turbidez em 0, 20, 25, 30, 40, 50, 60, 80, 100 e
    100 + ε;
  - os 15 casos e os 34 externos;
  - unidade do nitrato; P, ortofosfato e NT;
  - ausentes e cloreto;
  - censura e valores negativos;
  - qs = 92;
  - `iqa_methods()`;
  - `compare_iqa()` com a ordem dos métodos invertida.
- **`tests/testthat/fixtures/igam_referencia_pampulha.csv`:** 49 registros de
  entradas oficiais com o IQA publicado.
- **`tests/testthat/test-iqa-igam-compare.R`:** expectativas de estado
  atualizadas (`implemented = TRUE`, qs = 92) e entradas válidas com cloreto e
  temperatura.

### Documentação e auditoria

- `NEWS.md`, `README.md` e `README-pt.md`.
- Novos em `inst/audit/`:
  - `IGAM_VALIDACAO_TURBIDEZ_1_5.md`;
  - `IGAM_METODOLOGIA_E_UNIDADES.md`;
  - este relatório;
  - `referencias_igam/` (anexos recebidos, com `LEIA-ME.md`).
- Atualizados com nota de topo: `VALIDACAO_IGAM.md` e `IGAM_DIVERGENCIAS.md`.
- Novos em `audit/`:
  - scripts: `igam_15_casos.R`, `igam_extrair_validacao_externa.R`,
    `igam_validacao_externa.R`, `regressao_cetesb_pos_igam.R`;
  - dados: `dados/PRE_REGISTRO_VALIDACAO_EXTERNA.md`,
    `dados/igam_pampulha_validacao_externa_entradas.csv`, e `dados/FONTES.md`
    atualizado;
  - resultados: `resultados/igam_15_casos_reproducao.csv`,
    `igam_validacao_externa*.csv`, `igam_turbidez_curvas_k.csv` e
    `regressao_cetesb_pos_igam.csv`.

### Anexos

- **Recebidos:**
  - `DOSSIE_TURBIDEZ_IGAM_SCQA_2005.md`;
  - `IGAM_VALIDACAO_COEFICIENTE_1_5.csv`.
- **Não recebidos:**
  - `IGAM_TURBIDEZ_COMPARACAO.csv`: recriado a partir do motor;
  - `RELATORIO_VALIDACAO_IGAM_TURBIDEZ_15_CASOS.md`: substituído por
    `IGAM_VALIDACAO_TURBIDEZ_1_5.md`.

## 3. Regressão e verificações

- **Regressão CETESB** (`audit/regressao_cetesb_pos_igam.R`): 131/131 linhas
  com **diferença 0** no IQA bruto (4 casas), no IQA exibido e na classe.
  - Três linhas de fronteira do fósforo (PO₄ = 1, 5 e 10) foram gravadas no
    CSV de referência com 15 dígitos. Relidas, caem 1 ulp acima da fronteira e
    mudam de trecho (diferença aparente de até 0,13).
  - Com o valor exato (PO₄/3,066), a diferença é zero.
  - É um artefato de precisão do arquivo de referência, não do código. A
    descontinuidade do qi de fósforo nessas fronteiras é preexistente.
- **Testes:** `devtools::test()` com 36 arquivos e 171 testes: 0 falhas, 0
  erros e 5 pulados (preexistentes). Incluem os testes de `nsfwqi()` (modos
  estrito e parcial), sem alteração.
- **Check:** `R CMD check --as-cran --no-manual` (R no Windows; Pandoc do
  RStudio, porque o PATH da sessão não tem Pandoc): **0 erros, 0 avisos e 2
  notas**, ambas preexistentes:
  1. a URL da tese de Lamparelli (teses.usp.br) estourou o tempo de espera
     (libcurl 28; rede);
  2. arquivos fora do padrão na raiz (`atividade.R`,
     `scratch_save_baselines.R`, `scratch_spec.txt`, `user_spec.txt`,
     `user_spec_utf8.txt`), que são do mantenedor e não foram tocados.
- **Manual em PDF:** não foi gerado (sem `pdflatex`).
- **`devtools::document()`:** executado.

## 4. Estado de `iqa_methods()`

| method | implemented | documented | reference_validated | available |
|---|---|---|---|---|
| CETESB | TRUE | TRUE | TRUE | TRUE |
| IGAM | **TRUE** | TRUE | **FALSE** | **FALSE** |
| CETESB_legacy_approx | TRUE | TRUE | FALSE | TRUE |
| NSF_approx | TRUE | TRUE | FALSE | TRUE |

`implemented = TRUE` **não** significa validação institucional.

## 5. Recomendação de liberação (não executada)

**Critérios do dossiê:**

| Critério | Situação |
|---|---|
| 1. Transcrição conferida | Bucci (2011) e Nunes (2008) conferidos visualmente; Affonso (2016) só por texto. **Sem confirmação institucional** |
| 2. Testes determinísticos | Feito |
| 3. Reprocessamento dos 15 casos | Feito, 15/15 |
| 4. Validação externa com critérios prévios | 34/34, mas na **mesma sub-bacia** |
| 5. Ausências e censura registradas | Feito; o check foi executado |
| 6. Publicação | Não feita |

**Opções do mantenedor:**

- **(A) Manter não liberado** até obter uma destas duas coisas:
  - confirmação do IGAM/SEMAD sobre k (1,5 ou 15) e sobre as regras de
    cloreto ausente, de censura e de arredondamento;
  - validação em outra bacia (por exemplo, a rede estadual do Projeto Águas
    de Minas, com IQA publicado por amostra).
- **(B) Liberar como experimental:**
  - `available = TRUE`, mantendo `reference_validated = FALSE`;
  - exigir `nitrate_unit` e cloreto;
  - avisar na primeira chamada que k = 1,5 é uma hipótese de correção;
  - expor os argumentos em `iqa()`, o que é mudança de API e exige aprovação.

**Recomendação técnica: (A).** A evidência numérica é forte (49/49), mas vem de
uma única sub-bacia, com nitrato quase sempre censurado, e não há confirmação
institucional do coeficiente.

## 6. Pendências

1. Confirmação do IGAM sobre k = 1,5. A pergunta sugerida está em
   `IGAM_VALIDACAO_TURBIDEZ_1_5.md`.
2. Validação em outra bacia ou região, incluindo amostras com nitrato
   quantificado e alto.
3. Regras institucionais para:
   - cloreto ausente;
   - microbiológico < 1 NMP/100 mL;
   - dados censurados;
   - arredondamento.
4. Decidir a API pública do IGAM em `iqa()`: argumentos `nitrate_unit`,
   `phosphorus_input` e cloreto.
