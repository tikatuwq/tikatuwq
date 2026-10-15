# Relatório de testes e checks — tikatuwq 0.11.0

- **Data:** 2026-10-15
- **Ambiente:** R 4.5.1 (2025-06-13 ucrt), x86_64-w64-mingw32, Windows 11 x64
  (build 26200); testthat 3.2.3, devtools 2.4.5, rcmdcheck 1.4.0; pandoc do
  RStudio (`RSTUDIO_PANDOC`); **sem LaTeX** (manual em PDF não gerado localmente).
- **Working tree:** branch `fix/iqa-scientific-stabilization`, base `4e88fa9`.
  Os logs completos estão em `audit/resultados/` (fora do pacote).

## 1. testthat

```
Rscript -e 'suppressMessages(devtools::load_all(".", quiet=TRUE));
  r <- testthat::test_dir("tests/testthat", reporter="summary",
                          stop_on_failure=FALSE, load_package="none"); ...'
```

| Arquivos | Testes (`test_that`) | Verificações aprovadas | Falhas | Erros | Pulados | Avisos |
|---:|---:|---:|---:|---:|---:|---:|
| 35 | 160 | 550 | 0 | 0 | 17 | 11 |

(3ª rodada; na 2ª rodada eram 33 arquivos, 146 testes e 455 verificações.
Novos: `test-iqa-igam-compare.R` e `test-nsfwqi.R`.)

- **Pulados (17):** 15 com `skip_on_cran()` (`mk_seasonal`, `param_*`,
  `plot_map`), 1 teste vazio e 1 fallback sem leaflet (leaflet está instalado).
  Um teste de `plot_map_quality()` que antes era pulado em silêncio passou a
  ser obrigatório e passa.
- **Avisos (11), todos esperados e anteriores a esta rodada:** 2 avisos de
  valor negativo em `test-clean_units.R` (o próprio teste os provoca), 1
  conversão de unidade em `clean_units` e 8 de `mk_seasonal()` ("menos de 2
  estações") com o `wq_demo`.
- **Mudanças na suíte desta rodada:**
  - gabaritos externos IBEAS/von Sperling (21 qi e o IQA = 81);
  - divergência log10 fixada em teste;
  - classe calculada sobre o IQA não arredondado;
  - marcação experimental da temperatura;
  - regras literais do Apêndice D;
  - IET de Lamparelli contra as Tabelas 5 e 6;
  - Carlson nos valores de definição.

  Nenhum gabarito antigo teve a tolerância aumentada. O teste de
  monotonicidade de NT foi restrito a [0, 100], e o degrau em 100 mg/L é
  testado à parte, com justificativa.

## 2. R CMD check

```
RSTUDIO_PANDOC=".../quarto/bin/tools" Rscript -e 'devtools::check(
  args = c("--as-cran", "--no-manual"), error_on = "never",
  check_dir = "audit/resultados/check")'
```

Resultado (3ª rodada): **0 erros, 0 avisos, 1 nota.**

- **NOTE:** "Non-standard files/directories found at top level": `atividade.R`,
  `scratch_save_baselines.R`, `scratch_spec.txt`, `user_spec.txt`,
  `user_spec_utf8.txt`. São arquivos não versionados do mantenedor ou de
  sessões anteriores; não foram apagados. Resolver antes da submissão
  (apagar ou incluir no `.Rbuildignore`).
- Numa execução anterior apareceu também a nota "unable to verify current
  time", que depende do ambiente (acesso à rede) e não do pacote.
- Exemplos, exemplos com `--run-donttest`, testes (11 s) e reconstrução das
  vinhetas: OK. Tarball de 294 KB.
- **Não executado:** `--as-cran` com manual em PDF (falta LaTeX), win-builder,
  macOS e rhub. Isso precisa ser feito antes da submissão.

## 3. Outras verificações

| Verificação | Comando | Resultado |
|---|---|---|
| URLs | `urlchecker::url_check()` (pandoc no PATH) | 13 URLs, todas corretas |
| Ortografia | `spelling::spell_check_package()` | Cerca de 1.600 linhas, quase todas palavras em português num pacote com `Language` inglês. Questão antiga; não bloqueia (o CRAN verifica a ortografia do DESCRIPTION) |
| Implementação independente | `Rscript audit/iqa_independent.R` | 3000 entradas, max \|dif\| = 1,4e-14, 0 classes diferentes |
| Regressão 0.10.0 × 0.11.0 | `audit/iqa_snapshot.R` em worktree de `4e88fa9` + `audit/iqa_compare.R` | `REGRESSAO_0_10_0_0_11_0.csv` (131 linhas) |
| Fronteiras | `Rscript audit/iqa_boundaries.R` | `fronteiras_qi.csv` |
| Sensibilidade | `Rscript audit/iqa_sensitivity.R` | `sensibilidade_variantes.csv` |
| Matriz de evidências | `Rscript audit/iqa_matrix.R` | `MATRIZ_VALIDACAO_QI.csv` (44 linhas) |

## 4. Diagnóstico do *exit 139* (segfault)

Reproduzido com script mínimo, **sem o pacote**:

| Comando (Git Bash → `Rscript.exe` 4.5.1) | Resultado |
|---|---|
| `Rscript -e '<quebra de linha>print(1)<quebra de linha>'` | segfault (139), 3 de 3 execuções |
| `Rscript -e 'print(1)<quebra>print(2)<quebra>print(3)'` | exit 0, mas **só a 1ª linha é executada** |
| `Rscript -e 'print(1)' -e 'print(2)'` | correto |
| Mesmo código em arquivo `.R` | correto, 3 de 3 execuções |

Conclusão: é um defeito do tratamento de argumentos com quebra de linha do
`Rscript.exe` no Windows (chamado pelo MSYS/Git Bash), **não do tikatuwq**.
Todos os testes e checks deste relatório foram executados em linha única
(com `;`) ou a partir de arquivo. Recomendação: nunca usar `-e` com várias
linhas neste ambiente.
