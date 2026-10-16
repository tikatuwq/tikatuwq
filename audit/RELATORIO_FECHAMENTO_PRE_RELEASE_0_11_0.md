# Fechamento pré-release: tikatuwq 0.11.0, dados IGAM e testes reproduzíveis

- **Data:** 2026-10-16
- **Mandato:** `PROMPT_TIKATUWQ_0_11_0_DADOS_IGAM_PRE_RELEASE.md`. Autoriza
  mudanças técnicas e commits locais. **Sem autorização de publicação.**
- **Local:** `audit/`. O relatório contém o estado do git e caminhos locais.
  `audit/` está fora do tarball.

## 1. Resumo executivo

**Veredito: PRONTA COM RESSALVAS.**

### Verificado

- **Tarball sem dados do IGAM:** o tarball final (`89d3c97`, SHA-256
  `81cce9d3b4a56ad6cbc9f084bf6e55d764b2ae1f4dba38cd2ecc6f2d5419f3cf`, 175
  entradas) **não contém nenhum dos 49 registros do IGAM**, nem planilhas,
  nem tabelas por amostra. Isso foi verificado de duas formas:
  - lista de arquivos;
  - busca dos códigos de estação e das assinaturas numéricas de cada registro
    (§2.3).
- **Validação histórica:** continua documentada, com métricas agregadas,
  fontes com SHA-1 e procedimento de reprodução, em
  `inst/methodology/IGAM_VALIDATION.md`.
- **Testes offline:** 181 testes, 0 falhas. Usam só dados sintéticos, e o IQA
  esperado é calculado de forma independente, em Python.
- **CETESB:** 131/131 sem diferença.
- **Instalação limpa a partir do tarball:** ok.
- **`R CMD check --as-cran` sobre o tarball:** 0 erros, 0 avisos, **1 NOTE de
  ambiente**: "unable to verify current time". Os serviços de hora consultados
  pelo R (worldtimeapi.org e worldclockapi.com) estão inacessíveis desta
  máquina agora; no check anterior (`bdfb0b4`, mesmo dia) esse item passou.
  A verificação não foi desligada.

### Ressalvas

1. **Git:** os registros do IGAM **continuam rastreados no git**, em 19
   arquivos de `audit/`, `inst/audit/` e `tests/`, em **commits locais não
   enviados**. `origin/main` e `origin/gh-pages` não têm nenhum (§2.4). Se a
   branch for enviada como está, os dados serão publicados pelo histórico.
   **Decisão do mantenedor antes do push.**
2. **Plataformas:** win-builder e macOS não foram executados. O manual PDF não
   foi gerado.
3. **NOTE de hora:** é de ambiente. Repetir o check com rede antes da
   submissão.
4. **Limitações científicas do IGAM** (§6).

## 2. Dados do IGAM: o que saiu do tarball, onde ficou e o que segue rastreado

### 2.1 Classificação dos arquivos

| Tipo | Arquivos | Antes no tarball | Agora |
|---|---|---|---|
| **Oficiais do IGAM** (planilhas e relatórios) | `2017.xlsx`, `Dados_monitoramento 2018_3°/4° trimestre`, relatórios trimestrais | Não (nunca estiveram no repositório) | Preservados em `<PRIVATE_RESEARCH_DIR>\fontes_originais\igam\` |
| **Derivados por amostra** (extratos e resultados) | `tests/testthat/fixtures/igam_referencia_pampulha.csv` (49) | **Sim** | Excluído do build |
| | `inst/audit/IGAM_VALIDACAO_REFERENCIAS.csv` (15) | **Sim** | Excluído |
| | `inst/audit/referencias_igam/` (CSV com 15) | **Sim** | Excluído |
| | `inst/methodology/IGAM_VALIDATION.md` (tabela com 49) | **Sim** | Tabela **removida**; métricas agregadas mantidas |
| | `inst/audit/IGAM_DIVERGENCIAS.md`, `IGAM_RELEASE_READINESS.md`, `IGAM_VALIDACAO_TURBIDEZ_1_5.md`, `SCQA_FONTES_E_EQUACOES.md` (valores por amostra) | **Sim** | Excluídos |
| | `README.md`/`README-pt.md` e `?iqa`: exemplo com as entradas reais de uma amostra | **Sim** | Trocado por exemplo **sintético** |
| | `audit/dados/*`, `audit/resultados/*` | Não (`audit/` já era excluído) | Inalterado |
| **Relatórios IGAM só com agregados** | `VALIDACAO_IGAM.md`, `IGAM_METODOLOGIA_E_UNIDADES.md`, `RELATORIO_IGAM_IMPLEMENTACAO.md` | Sim | Excluídos: citam os relatórios acima; a nota metodológica os substitui |
| | `RELATORIO_IMPLEMENTACAO_IGAM_COMPARE.md` | Sim | **Mantido** (citado por `?nsfwqi`; sem registros) |
| **Sintéticos** | Amostras dos testes IGAM (`test-iqa-igam-*.R`) | Sim | Mantidos e ampliados |
| **Outros dados** | `wq_demo` (INEMA), `exemplo_chamagunga.csv`, CSVs de auditoria CETESB/NSF | Sim | Inalterados (fora do escopo IGAM) |

### 2.2 Preservação, sem exclusão destrutiva

- **Nada foi apagado nem movido.** Todos os arquivos continuam no repositório
  de desenvolvimento.
- **Arquivo de pesquisa externo:** `<PRIVATE_RESEARCH_DIR>\`,
  fora do git, com `LEIA-ME.md` e `MANIFESTO_SHA1.txt` (29 arquivos).
  - `fontes_originais/igam/`: 3 planilhas e 3 relatórios. **Antes desta
    rodada existiam só no scratchpad temporário da sessão.** Os SHA-1
    conferem com `audit/dados/FONTES.md`.
  - `fontes_originais/literatura/`: Bucci 2011, Nunes 2008, Affonso 2016 e
    Martins 2016.
  - `copias_do_repositorio/`: cópias, com o mesmo caminho relativo, dos 13
    arquivos ou diretórios com registros.
- **Não preservados aqui** (não estavam disponíveis nesta sessão): o
  relatório Pampulha do 3º trimestre de 2017 (handle 2370) e o PDF do SCQA
  2005. As URLs estão em `FONTES.md`.
- **Teste da cópia:** `audit/auditoria_final_49.R`, executado com a planilha
  da cópia arquivada, reproduziu os mesmos 49/49.

### 2.3 Verificação do tarball final

- **Arquivos de dados presentes:** `data/wq_demo.rda`; os CSVs CETESB/NSF de
  `inst/audit` (FRONTEIRAS_QI, MATRIZ_VALIDACAO_QI, REGRESSAO_*,
  sensibilidade_variantes); `inst/extdata/*`; `build/vignette.rds`. **Nenhum é
  do IGAM.**
- **Códigos de estação `PV[0-9]{3}`:** 0 ocorrências.
- **Assinaturas numéricas:** para cada um dos 49 registros, foram buscados
  como tokens exatos o cloreto, os sólidos totais, a turbidez e o IQA
  publicado. Houve 1 coincidência de 3 dos 4 valores, em
  `inst/doc/tikatuwq-workflow.html`. Era **falso positivo**: uma temperatura
  do `wq_demo`, um trecho de base64 e IDs de linha do HTML.
- **Pacote instalado:** só contém `methodology/IGAM_VALIDATION.md` e
  `audit/RELATORIO_IMPLEMENTACAO_IGAM_COMPARE.md` (ambos agregados) e nenhum
  código de estação.
- **Comparação:** antes, o tarball tinha 8 arquivos com registros e 189
  entradas.

### 2.4 Exposição no git, ainda não resolvida

**Remoto:** `origin/main` e `origin/gh-pages` têm **0** arquivos com
registros. Os dados entraram a partir do commit **local** `7b5a9f0`, e nenhum
dos commits desde `dcbfe7e` foi enviado.

**Rastreados em HEAD (19 arquivos):**
- `audit/dados/igam_pampulha_2017q3_entradas.csv` e
  `audit/dados/igam_pampulha_validacao_externa_entradas.csv`;
- `audit/resultados/{auditoria_final_49_casos,igam_15_casos_reproducao,igam_validacao_externa,igam_divergencias_por_amostra}.csv`;
- `audit/AUDITORIA_FINAL_PRE_RELEASE_0_11_0.md`, `audit/dados/FONTES.md` e os
  scripts `audit/*.R` (códigos de estação e altitudes);
- `inst/audit/IGAM_VALIDACAO_REFERENCIAS.csv`,
  `inst/audit/referencias_igam/IGAM_VALIDACAO_COEFICIENTE_1_5.csv`,
  `inst/audit/IGAM_DIVERGENCIAS.md`, `IGAM_RELEASE_READINESS.md`,
  `IGAM_VALIDACAO_TURBIDEZ_1_5.md` e `SCQA_FONTES_E_EQUACOES.md`;
- `tests/testthat/fixtures/igam_referencia_pampulha.csv`.

O `.Rbuildignore` tira esses arquivos do tarball, **mas não do GitHub**.
Nenhuma ação destrutiva foi executada.

**Opções (decisão do mantenedor):**

- **(A) Recomendada, antes do primeiro push:** reescrever **só a série local
  não enviada** (`dcbfe7e..HEAD`) sem esses arquivos. Por exemplo,
  `git rebase -i` ou `git filter-repo --path ... --invert-paths` sobre a
  branch local, mantendo as cópias em `<PRIVATE_RESEARCH_DIR>`.
  - É **reescrita de histórico**: não a fiz por falta de autorização.
  - Como nada foi enviado, não afeta ninguém.
- **(B)** `git rm --cached <arquivos>` e entradas no `.gitignore`, num commit
  novo.
  - Os arquivos somem do HEAD, mas **continuam no histórico de `7b5a9f0`
    em diante**.
  - Se a branch for enviada, os dados ficam públicos pelo histórico.
  - **Não basta** se o objetivo é não expor os dados.
- **(C)** Manter os dados num repositório privado de pesquisa e enviar ao
  GitHub só uma branch reconstruída sem eles (equivale a A).

## 3. Arquivos alterados e commits locais

**Estado antes:** HEAD `52a8501`.
- **Modificados do mantenedor:** `CRAN-SUBMISSION`, `_pkgdown.yml` (parte
  preparada no índice e parte não), `docs/*` e
  `audit/resultados/{00check,rcmdcheck,testthat}.log`.
- **Não rastreados:** `atividade.R`, `scratch_*`, `user_spec*`,
  `tests/baseline_*.csv`, `tests/testthat/Rplots.pdf` e
  `docs/EXEMPLOS_TIKATUWQ.html`.

| Commit | Assunto | Arquivos |
|---|---|---|
| `7fd6853` | **(a) Redistribuição.** IGAM fora do tarball | `.Rbuildignore`, `inst/methodology/IGAM_VALIDATION.md`, `audit/{auditoria_final_49,igam_15_casos,igam_extrair_validacao_externa,igam_validacao_externa}.R` |
| `7bafa77` | **(b) Testes.** Sintéticos com valores independentes; `mk_seasonal` real | `tests/testthat/test-iqa-igam-{api,engine}.R`, `tests/testthat/test-mk_seasonal.R`, `audit/igam_valores_esperados_sinteticos.py` |
| `89d3c97` | **(c) Documentação e pkgdown** | `R/iqa.R` (só o exemplo roxygen), `man/iqa.Rd`, `R/iqa_igam.R` (só comentários), `README.md`, `README-pt.md`, `NEWS.md`, `inst/audit/IQA_AUDITORIA.md`, `_pkgdown.yml` |
| (seguinte) | Este relatório e os logs | `audit/RELATORIO_FECHAMENTO_PRE_RELEASE_0_11_0.md`, `audit/resultados/pre_release_0_11_0_fechamento/` |

**Diffs relevantes:**

- **Equações, constantes, pesos e regras:** **nenhuma alteração** nos motores
  CETESB e IGAM. As mudanças em `R/` são o exemplo e os comentários.
- **`.Rbuildignore`:** 10 regras novas para o IGAM. Também removi as 5 linhas
  de comentário que eu tinha posto no `bdfb0b4`, porque o
  `pkgbuild`/`devtools` lê cada linha como expressão regular e um parêntese
  sem fechamento derrubou o `document()`. As regras antigas continuam
  válidas: todas as expressões compilam.
- **`_pkgdown.yml`:** entraram as edições pendentes do mantenedor, sem
  alteração. Elas indexam todas as 46 exportações (incluindo `iqa_methods`,
  `compare_iqa` e `iqa_components`) e reorganizam as seções;
  `pkgdown::check_pkgdown()` deu "No problems found".
- **`CRAN-SUBMISSION` e os 3 logs antigos:** **não** foram commitados nem
  alterados.

## 4. Testes, regressão, smoke e check

Logs integrais em `audit/resultados/pre_release_0_11_0_fechamento/`.

### `devtools::test()`

| | Arquivos | Testes | Expectativas | Falhas | Erros | Avisos | Pulados |
|---|---|---|---|---|---|---|---|
| Antes (`52a8501`) | 37 | 182 | 801 | 0 | 0 | 11 | 5 (1 vazio) |
| **Depois** | 37 | **181** | **803** | 0 | 0 | 11 | **4** |

**Testes pulados (4):**
- 3 de `render_report`, que exigem pandoc no PATH. Com pandoc, 2 rodam e
  passam.
- 1 de fallback do leaflet, por desenho.

**O que mudou:**
- **Removidos:** os 3 testes que liam os 49 registros reais.
- **Novos:**
  - integração completa com 7 amostras sintéticas, cobrindo todos os trechos
    dos 9 subíndices e um parâmetro ausente;
  - controle k = 15 sobre uma amostra sintética.
- **Corrigido:** o teste vazio de `mk_seasonal`.
- **Avisos (11):** vêm de `clean_units` e `mk_seasonal` e já existiam.

**Valores esperados independentes:** o IQA esperado de cada amostra sintética
foi calculado por `audit/igam_valores_esperados_sinteticos.py`, uma
transcrição própria em Python que não usa o código do pacote. Os valores
ficam fixados nos testes com tolerância de 1e-10.

| Amostra | IQA bruto | 1 casa | Classe |
|---|---|---|---|
| A1 | 75,479124819573599 | 75,5 | Bom |
| A2 | 17,054590358404308 | 17,1 | Muito Ruim |
| A3 | 38,214773570129871 | 38,2 | Ruim |
| A4 | 74,843036542785725 | 74,8 | Bom |
| A5 | 53,248447741776744 | 53,2 | Médio |
| B1 | 59,080906158297132 | 59,1 | Médio |
| B2 | 28,673386078401698 | 28,7 | Ruim |

**`mk_seasonal`:** usa uma série mensal sintética de 5 anos, estritamente
crescente. Os valores esperados foram calculados à mão: S = 120, Var(S) = 200
e Z = 119/√200 = 8,4146, significativo e "crescente". O controle decrescente
dá S = −120 e Z simétrico.

**Offline:** nenhum teste baixa nada; o pacote não tem chamadas de rede.

### Regressão CETESB

- **Resultado:** 131 linhas, com máx. |Δ bruto| = 0, máx. |Δ exibido| = 0 e 0
  classes diferentes (`regressao_cetesb.log`).

### Build e check

- **`R CMD build`:** `tikatuwq_0.11.0.tar.gz`, 175 entradas (`build.log`).
- **`R CMD check --as-cran --no-manual` sobre o tarball** (R 4.5.1 ucrt,
  Windows 11 x64, pandoc 3.8.3):
  - **0 erros, 0 avisos, 1 NOTE**: "checking for future file timestamps ...
    NOTE / unable to verify current time";
  - a NOTE se repetiu em duas execuções
    (`00check_execucao1.log`, `00check.log`);
  - teste de rede: `curl` para worldtimeapi.org e worldclockapi.com dá código
    000 (sem conexão);
  - é de ambiente: o check de 2026-10-16 no `bdfb0b4` passou esse item;
  - viabilidade de entrada no CRAN (URLs), exemplos, testes e vinhetas: OK.
- **Testes dentro do check (modo CRAN):** FAIL 0, WARN 11, SKIP 16 (15
  `skip_on_cran` e 1 fallback do leaflet), PASS 736. Nenhum teste de IQA ou
  IGAM é pulado.

### Instalação limpa e smoke tests

A partir do tarball, numa biblioteca temporária, com `Rscript --vanilla`
(`install.log`, `smoke.log`):

| Verificação | Resultado |
|---|---|
| CETESB | 74,96, "Boa" |
| IGAM (amostra sintética A1) | 75,5, "Bom"; bruto 75,47912, igual ao valor do Python |
| Sem `nitrate_unit` | Erro explicativo |
| `classification` em `iqa()` | Erro de migração |
| `iqa_methods()` | IGAM `available = TRUE` |
| `compare_iqa()` | 3 pares; P4 inelegível por falta de cloreto |
| `classify_iqa()` | Ok |
| Nota metodológica | Instalada |
| `citation()` | DOI de conceito 10.5281/zenodo.17407296 |

## 5. Referências e documentação metodológica

- **Nota:** `inst/methodology/IGAM_VALIDATION.md` (distribuída). Contém:
  - equações e domínios por página do SCQA;
  - unidades, pesos e políticas do pacote;
  - a justificativa da turbidez (15 × 1,5);
  - métricas agregadas (15 + 34);
  - limitações;
  - as fontes do IGAM com handles e SHA-1;
  - o procedimento de reprodução;
  - a semântica de `iqa_methods()`.
- **Referências bibliográficas:**
  - SEMAD/UCEMG/PNMA II (2005), *SCQA — Estabelecimento das Equações do IQA*,
    Relatório 1;
  - IGAM, página "Índice de Qualidade das Águas – IQA";
  - IGAM, Repositório Institucional, handles 3077, 3049, 3050, 2370, 2927 e
    2365;
  - BUCCI, M. M. H. S. (2011), dissertação, UFJF, p. 51;
  - NUNES, D. G. (2008), dissertação, UFV, Quadro 10, p. 52;
  - AFFONSO, D. F. S. (2016), UFV;
  - MARTINS, G. L. A. (2016), UFVJM, p. 56.

  As URLs foram verificadas nas rodadas anteriores. Nenhum metadado foi
  inventado.
- **DOI:** o DOI de conceito do Zenodo foi mantido (`inst/CITATION`,
  `CITATION.cff`, READMEs). Nenhum DOI de versão é atribuído à 0.11.0.
- **Mudança incompatível:** `classification` em `iqa()`. Está documentada
  na NEWS, em `?iqa`, nos READMEs e em `cran-comments.md`, com orientação para
  `classify_iqa()` (inclusive na chamada posicional da 0.10.0).

## 6. Limitações científicas

1. **Abrangência:** a validação do IGAM se concentra na sub-bacia da
   Pampulha (49 registros, 3 campanhas, 2017–2018). Não há validação
   geográfica.
2. **Homologação:** não há homologação nem confirmação formal do IGAM; o
   coeficiente 1,5 vem da literatura.
3. **Nitrato:** quase sempre censurado nos dados. Os trechos > 10 mg/L NO₃ só
   são verificados por testes sintéticos.
4. **Políticas do pacote, sem regra do IGAM:**
   - censura no limite;
   - cloreto obrigatório, sem valor padrão;
   - microbiológico < 1 → `NA`;
   - classe sobre o valor arredondado;
   - regra de desempate do arredondamento não verificada.
5. **Pendências CETESB documentadas** (risco baixo):
   - pH 9–10, com duas transcrições divergentes (efeito ≤ 0,9 ponto);
   - curva de ΔT dos modos experimentais, sem fonte.
6. **Metadados do método:** ficam em atributos do R, que se perdem em
   `merge()` e na exportação CSV (ressalva já registrada na auditoria final).

## 7. Não executado / o que falta para o CRAN

- **win-builder e macOS:** não executados.
- **Manual PDF:** não gerado (sem LaTeX).
- **Check de URL e hora:** repetir com rede para eliminar a NOTE de ambiente.
- **Dependências reversas:** não verificadas.
- **Git:** decidir a exposição dos dados (§2.4) antes de qualquer push.

## 8. Confirmação

**Nenhum** push, tag, release, envio ao CRAN, depósito no Zenodo ou
publicação de site foi feito. Os commits `7fd6853`, `7bafa77`, `89d3c97` e o
deste relatório são **locais**.
