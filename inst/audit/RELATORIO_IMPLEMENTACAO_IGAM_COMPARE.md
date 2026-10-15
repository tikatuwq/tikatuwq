# Relatório de implementação: IGAM, compare_iqa(), ANA e nsfwqi() — tikatuwq 0.11.0

- **Data:** 2026-10-15 (3ª rodada)
- **Linha de base:** commit `324ff54`
  - testes: 33 arquivos, 146 testes, 455 verificações, 0 falhas;
  - snapshots de IQA e NSFWQI gravados antes de qualquer mudança.
- **Ambiente:** R 4.5.1 ucrt, Windows 11 x64; testthat 3.2.3, devtools 2.4.5;
  pandoc do RStudio; sem LaTeX.

## 1. Resumo

| Etapa | Resultado | Situação |
|---|---|---|
| 0. Inventário e linha de base | Snapshots de IQA (131 amostras) e NSFWQI (25) do `324ff54` | Concluído |
| 1. Validação CETESB | `VALIDACAO_CETESB.md`, `FRONTEIRAS_QI.csv` (34 fronteiras: 30 do tipo b, 4 do tipo c, nenhuma do tipo a) | Concluído; 2 pendências científicas (pH 9–10; curva de ΔT) |
| 2. IGAM | Especificação, regra de ausentes, diagnóstico de entradas e erro classificado | **Bloqueado:** equações SCQA (2005) não obtidas |
| 3. API `method` / ANA | Escala segue `method`; `classification` depreciado com aviso de classificação cruzada; `iqa_methods()`; ANA sem método próprio | Concluído |
| 4. `compare_iqa()` | Comparador pareado com elegibilidade, motivos, estatísticas e transições de classe | Concluído (sem pares reais enquanto o IGAM estiver bloqueado) |
| 5. `nsfwqi()` / `pesos` | Limitações documentadas; correção do modo estrito; TSS não substitui ST; primeiros testes | Concluído |
| 6. Índices customizados | `docs/PROPOSTA_INDICES_CUSTOMIZADOS.md` | Somente proposta |
| 7. Testes e check | 35 arquivos, 160 testes, 550 verificações, 0 falhas; `R CMD check`: 0/0/1 | Concluído |

## 2. Decisões e fontes

| Decisão | Fonte | Documento |
|---|---|---|
| Não implementar os qi do IGAM | SCQA (SEMAD/PNMA II, 2005) não obtido; descrito no estudo Piabanha (2014) como regressões próprias | `VALIDACAO_IGAM.md` |
| Rótulos e cortes IGAM | IGAM (2017), relatório Pampulha, p. 9 | idem |
| Regra de ausentes do IGAM (repartição igual) | Portal Infohidro/IGAM | idem; o caso com vários ausentes é **interpretação** |
| Sem `method = "ANA"` | Portais ANA (sem equações; quem calcula não é declarado) | `ANA_VS_CETESB.md` |
| `classification` depreciado em `iqa()` | Prompt: `method` define motor e escala | `?iqa`, seção "Metodo x classificacao" |
| Novo canônico `fosfato_total` | Regra 1 (fosfato ≠ ortofosfato ≠ P total) | `R/utils_sanitize.R` |
| `nsfwqi()` estrito → `NA` sem coluna | Contrato documentado de `na_rm = FALSE` | NEWS; `REGRESSAO_NSFWQI_0_11_0.csv` |
| `nsfwqi()` sem TSS como ST | Regra 1 | NEWS |

## 3. Comandos (todos a partir de arquivo ou em linha única; ver o diagnóstico do segfault em `RELATORIO_TESTES_CHECK.md`)

```
Rscript audit/iqa_snapshot.R <worktree 324ff54> base_iqa.csv     # linha de base
Rscript audit/nsfwqi_snapshot.R <worktree 324ff54> base_nsf.csv
Rscript audit/iqa_boundaries.R                                    # FRONTEIRAS_QI.csv
Rscript audit/iqa_snapshot.R . now_iqa.csv; Rscript audit/iqa_compare.R base_iqa.csv now_iqa.csv <tmp>
Rscript audit/nsfwqi_snapshot.R . new_nsf.csv                     # REGRESSAO_NSFWQI_0_11_0.csv
Rscript -e 'devtools::document()'
Rscript -e '...testthat::test_dir("tests/testthat", ...)'          # audit/resultados/testthat.log
RSTUDIO_PANDOC=... Rscript -e 'devtools::check(args = c("--as-cran","--no-manual"), ...)'
```

## 4. Resultados de regressão desta rodada

- **IQA CETESB:** idêntico à linha de base nas 131 amostras (valor exibido,
  valor bruto e classe; diferença 0).
- **NSFWQI estrito:** 20 de 25 linhas mudaram (todas do `wq_demo`, de valor
  para `NA`, porque faltam nitrato e sólidos totais). NSFWQI parcial e
  aritmético: nenhuma mudança.
- **Classificação dentro de `iqa()`:** sem mudança numérica. Chamadas com
  `classification` passam a emitir aviso.

## 5. Testes

| Arquivo | O que cobre |
|---|---|
| `test-iqa-igam-compare.R` (novo; 8 testes) | IGAM bloqueado com erro classificado e diagnóstico; especificação; repartição de pesos; analitos não substituídos; escala seguindo `method`; `compare_iqa()` com IGAM indisponível, validações, 0 e 1 linha, `details`; estatísticas pareadas com resultados **sintéticos** (montagem e fórmulas, não validação científica) |
| `test-nsfwqi.R` (novo; 6 testes) | Pesos, agregações, máximo de 90, TSS, modo estrito, conversão P → PO₄, vazio |
| `test-iqa-audit-validation.R` (ajustado) | Avisos de depreciação e classificação cruzada |

**Teste de software × validação científica:** nenhuma validação científica
do IGAM foi possível. Os testes do comparador verificam a lógica, não
resultados institucionais.

## 6. Falhas e pendências abertas

| # | Prioridade | Item | O que falta |
|---|---|---|---|
| 1 | **Bloqueio** | Motor IGAM | Documento SCQA (2005), unidade do nitrato e fixtures com IQA publicado pelo IGAM |
| 2 | P1 | pH 9–10 (CETESB) | Decisão do mantenedor |
| 3 | P1 | Curva de ΔT (CETESB `delta`/`reference`) | Decisão: manter experimental, desativar ou ajustar com aprovação |
| 4 | P2 | `_pkgdown.yml` | Incluir `compare_iqa` e `iqa_methods` na referência. O arquivo tem mudanças anteriores não commitadas que não são minhas, por isso não o editei |
| 5 | P2 | `docs/PROPOSTA_INDICES_CUSTOMIZADOS.md` | `docs/` é saída do pkgdown; mover se a proposta for aceita |
| 6 | P2 | `cran-comments.md` | Hoje contém só `devtools::submit_cran()` (mudança anterior, não minha) |
| 7 | P2 | Checks externos | win-builder, macOS e manual PDF (falta LaTeX local) |

## 7. Recomendação de publicação

**Apta para revisão, não para release.** O motor CETESB está estável e
inalterado nesta rodada. IGAM e ANA estão documentados honestamente, e o
comparador está pronto, mas só tem utilidade científica quando o IGAM for
desbloqueado. Antes de publicar:
- resolver os itens 2, 3, 4, 6 e 7;
- decidir se a 0.11.0 sai com `method = "IGAM"` declarado e bloqueado ou se a
  rota fica para a versão que trouxer o SCQA. Recomendação: declarada e
  bloqueada, porque `iqa_methods()` e o erro deixam o estado explícito.

Sem push, tag, submissão ao CRAN nem depósito no Zenodo.
