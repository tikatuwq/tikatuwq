# Dados IGAM por amostra: só no arquivo de pesquisa privado

Por decisão do projeto, os 49 registros históricos do IGAM usados na
validação do motor IGAM **não são redistribuídos**: não estão no pacote nem
no histórico git publicável. Isso vale para as entradas, os IQAs publicados
por estação, as altitudes e os resultados por amostra.

O histórico desta branch foi reconstruído em 2026-10-16 sem esses arquivos.
Ver `audit/RELATORIO_SANITIZACAO_GIT_0_11_0.md`.

## Arquivos que existem só no arquivo privado

- **Extratos:** `audit/dados/igam_pampulha_2017q3_entradas.csv` e
  `audit/dados/igam_pampulha_validacao_externa_entradas.csv`.
- **Resultados por amostra:** `audit/resultados/auditoria_final_49_casos.csv`,
  `igam_15_casos_reproducao.csv`, `igam_validacao_externa.csv`,
  `igam_divergencias_por_amostra.csv` e
  `pre_release_0_11_0/smoke.log`.
- **Scripts exploratórios com dados embutidos:** `audit/igam_scqa_validation.R`,
  `igam_divergencias.R`, `igam_15_casos.R` e `igam_validacao_externa.R`.
- **Relatórios de pesquisa com valores por amostra:**
  `inst/audit/IGAM_DIVERGENCIAS.md`, `IGAM_RELEASE_READINESS.md`,
  `IGAM_VALIDACAO_TURBIDEZ_1_5.md`, `SCQA_FONTES_E_EQUACOES.md`,
  `IGAM_VALIDACAO_REFERENCIAS.csv` e `referencias_igam/`.
- **Fixture:** `tests/testthat/fixtures/igam_referencia_pampulha.csv`.

Alguns relatórios agregados mantidos (por exemplo, `inst/audit/VALIDACAO_IGAM.md`)
ainda citam esses arquivos pelo nome. Eles estão no arquivo privado.

## Onde estão

- **Arquivo de pesquisa privado do mantenedor:** `<PRIVATE_RESEARCH_DIR>/`,
  fora deste repositório, com `MANIFESTO_SHA1.txt`.
- **`copias_do_repositorio_ae7f98a/`:** as versões exatas desses arquivos.
- **`dados_privados/`:** as entradas que os scripts de reprodução leem.

## Reprodução sem versionar dados

```
Rscript audit/igam_extrair_validacao_externa.R <dir_privado> <2018_3trim.xlsx> <2017.xlsx> <2018_4trim.xlsx>
Rscript audit/auditoria_final_49.R <dir_privado> [<2017.xlsx>]
```

- `<dir_privado>` deve estar **fora** do repositório; os scripts recusam
  outro local.
- As fontes e os SHA-1 estão em `audit/dados/FONTES.md`.
