# Saneamento do histórico git local: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Mandato:** `PROMPT_TIKATUWQ_LIMPEZA_SEGURA_HISTORICO_GIT_0_11_0.md`.
  - **Autorizada:** reescrita controlada só da série local não publicada.
  - **Não autorizados:** push (inclusive forçado), tags, releases, CRAN,
    Zenodo e site.
- **Distribuição deste relatório:** está em `audit/`, que fica fora do
  tarball. Tem só informações agregadas e comandos, sem nenhum registro
  individual.

## Parecer

**PRONTA PARA REVISÃO DO MANTENEDOR.**

A branch local `release/0.11.0-limpa` tem as seguintes propriedades:
- preserva todas as funcionalidades da 0.11.0;
- nenhum dos seus commits publicáveis contém os registros por amostra do
  IGAM;
- testes, regressão CETESB, build e check sobre o tarball foram refeitos
  nela;
- a branch original, o backup e os dados de pesquisa estão intactos;
- nada foi publicado.

**Ressalvas (não bloqueiam a revisão):**
- win-builder, macOS e o manual PDF não foram executados;
- o check tem 1 nota de ambiente (servidores de hora inacessíveis);
- limitações científicas já documentadas (§9).

## 1. Base remota conferida

| Item | Valor |
|---|---|
| Remoto | `origin` = https://github.com/tikatuwq/tikatuwq.git |
| `origin/main` (`git ls-remote`, antes e depois) | `4e88fa9b6aaf2ff53a9a9c4203110fedfbde9d61` (Release 0.10.0) |
| `origin/gh-pages` (antes e depois) | `0800938eca47225f4b063cadca2e4ac8b4388d37` |
| Ancestral comum (`git merge-base HEAD origin/main`) | `4e88fa9`. **Não** é `dcbfe7e`, como o relatório anterior presumia: havia 18 commits locais não enviados |
| Branch original | `fix/iqa-scientific-stabilization` @ `ae7f98a`, sem upstream; **mantida intacta, não publicar** |

**Remoto sem registros do IGAM:**
- foram varridos todos os blobs alcançáveis por `origin/main` e
  `origin/gh-pages`: 97 commits e 1136 blobs;
- resultado: 0 códigos de estação e 0 marcadores de tabela IGAM;
- houve 3 coincidências de assinatura numérica, todas falsos positivos:
  2 CSS do font-awesome e 1 TSV do INEMA, com valores em linhas distintas.

## 2. Backup e restauração

- **Local:** `<BACKUP_DIR>\`, fora de
  qualquer pasta publicável. **Não compartilhar.**
- **Conteúdo:**

| Arquivo | Conteúdo |
|---|---|
| `tikatuwq_todas_refs.bundle` | `git bundle --all`. `git bundle verify`: ok. O clone de teste com `fsck --full` restaurou `ae7f98a` e `4e88fa9` |
| `tikatuwq_copia_integral/` | Cópia completa do repositório: `.git` e árvore de trabalho com as alterações não commitadas e os arquivos pessoais. 2347 arquivos, SHA-256 idênticos aos do original (`MANIFESTO_SHA256_copia_integral.txt`) |
| `alteracoes_nao_preparadas.patch`, `alteracoes_preparadas.patch` | Diff binário das alterações não commitadas |
| `refs_no_momento_do_backup.txt`, `git_status_no_momento_do_backup.txt` | Refs e status no momento do backup |
| `MANIFESTO_SHA256_backup.txt` | SHA-256 dos itens acima |

- **Restauração:** use `git clone <backup>/tikatuwq_todas_refs.bundle <destino>`
  ou copie `tikatuwq_copia_integral/`.

## 3. Arquivo de pesquisa

- **Local:** `<PRIVATE_RESEARCH_DIR>\`, fora do git.
- **Integridade:** `MANIFESTO_SHA1.txt` com 60 arquivos; `sha1sum -c` ok.
  Os SHA-1 conferem com `audit/dados/FONTES.md`.
- **Fontes que faltavam:**
  - SCQA 2005 (as duas cópias do Internet Archive, com SHA-1 base32 iguais
    aos documentados);
  - relatório Pampulha do 3º trim. 2017 (handle 2370; SHA-1 registrado).

  Ambas foram **obtidas e arquivadas** nesta rodada. **Nenhuma fonte falta
  agora.**
- **Novas pastas** (nada foi sobrescrito):
  - `copias_do_repositorio_ae7f98a/`: versões exatas, como em `ae7f98a`, de
    todos os arquivos retirados ou saneados;
  - `dados_privados/`: extratos, altitudes das estações e IQAs publicados,
    lidos pelos scripts de reprodução, mais as saídas desta rodada em
    `saidas_*`.

## 4. Escopo retirado

### Varredura da série original

- **Abrangência:** todos os objetos novos de
  `HEAD ^origin/main ^origin/gh-pages`: 18 commits e 196 blobs.
- **Critérios:**
  - códigos de estação (`PVnnn`, com três dígitos);
  - marcadores de tabela IGAM;
  - assinatura numérica de cada um dos 49 registros: pelo menos 3 de 4
    valores (cloreto, sólidos totais, turbidez e IQA publicado) como tokens
    exatos.
- **Resultado:** 41 blobs marcados. Os 8 primeiros commits locais
  (`e775872` até `dcbfe7e`) têm **0** blobs com dados. A contaminação começa
  em `7b5a9f0`.

### Tipos de conteúdo retirados

Retirados de **toda** a história publicável (as versões exatas estão em
`copias_do_repositorio_ae7f98a/`):

| Tipo | Caminhos |
|---|---|
| Extratos das planilhas | 2 CSVs em `audit/dados/` |
| Resultados por amostra | 4 CSVs em `audit/resultados/` e 1 log de smoke test com o resultado de uma amostra real |
| Scripts exploratórios com estações, altitudes ou saídas por amostra | 4 scripts em `audit/` |
| Relatórios de pesquisa com valores por amostra | 4 arquivos `.md` e 1 `.csv` em `inst/audit/`, mais `inst/audit/referencias_igam/` (3 arquivos) |
| Fixture de testes | `tests/testthat/fixtures/igam_referencia_pampulha.csv` |
| **Versões intermediárias** de arquivos mantidos | Tabela de 49 linhas da nota metodológica; exemplo com entradas reais em README, `?iqa` e `man/iqa.Rd`; testes que liam a fixture. Elas **não** aparecem na branch nova, porque cada arquivo entra direto na versão final limpa |

### Saneados e mantidos

- **Relatórios:** `audit/AUDITORIA_FINAL_PRE_RELEASE_0_11_0.md` (tabela de
  limítrofes e exemplo real substituídos por agregados) e
  `audit/RELATORIO_FECHAMENTO_PRE_RELEASE_0_11_0.md` (código de estação
  retirado).
- **Fontes:** `audit/dados/FONTES.md`, só com fontes e hashes, sem a lista
  de estações.
- **Scripts de reprodução:** `audit/igam_extrair_validacao_externa.R` e
  `audit/auditoria_final_49.R`.
  - **Antes:** embutiam estações, altitudes e IQAs publicados.
  - **Agora:** leem esses dados de um diretório privado fora do
    repositório, gravam as saídas por amostra lá e recusam um diretório
    dentro do repositório.
- **Novo:** `audit/DADOS_IGAM_PRIVADOS.md`, que lista o que existe só no
  arquivo privado.
- **`.gitignore`:** padrões que impedem que dados por amostra do IGAM voltem
  a ser versionados.

### Preservados sem alteração

`R/`, `man/`, os testes sintéticos, `NAMESPACE`, `DESCRIPTION`, `NEWS.md`,
READMEs, `_pkgdown.yml`, `inst/CITATION`, `CITATION.cff`, `cran-comments.md`,
`.Rbuildignore`, a documentação agregada e as citações.

## 5. Reconstrução

- **Técnica:**
  - `git worktree add -b release/0.11.0-limpa <LOCAL_WORKDIR>/tikatuwq_limpo 4e88fa9`,
    numa árvore separada; a árvore de trabalho do mantenedor não foi tocada;
  - `git cherry-pick` dos 8 commits limpos;
  - depois, commits temáticos montados com `git checkout ae7f98a -- <caminhos
    limpos>`, mais os arquivos saneados.
- **Por que esta técnica:**
  - cada arquivo entra direto na sua versão **final limpa**, verificada pela
    varredura, então nenhuma árvore intermediária contém dados;
  - repassar os 10 commits contaminados exigiria sanear cada versão
    intermediária, com mais risco de omissão;
  - os commits contaminados **não** são ancestrais da branch nova.

### Correspondência

| Original (não publicar) | Novo | Finalidade |
|---|---|---|
| `e775872` | `65fe8dd` | Árvore idêntica |
| `ede5c03` | `a0ba15d` | Árvore idêntica |
| `bcf54ae` | `9286735` | Árvore idêntica |
| `342d931` | `fc3e770` | Árvore idêntica |
| `324ff54` | `23707fe` | Árvore idêntica |
| `7391ea3` | `5338991` | Árvore idêntica |
| `5c0328f` | `af3ac62` | Árvore idêntica |
| `dcbfe7e` | `dc368cf` | Árvore idêntica |
| `7b5a9f0`, `87abd74`, `7bafa77` (código e testes) | `578015e` | Motor IGAM, API e testes sintéticos |
| `179819a`, `89d3c97` e a parte de documentação de `7b5a9f0` | `0ac7041` | NEWS, READMEs, citação, cran-comments, pkgdown, nota metodológica |
| `bdfb0b4`, `7fd6853` | `26dc2ae` | `.Rbuildignore` e `.gitignore` |
| `fbdab57`, `52a8501`, `ae7f98a` e as partes de auditoria de `7b5a9f0` e `7fd6853` | `0bc84cf` | Relatórios agregados, resultados agregados, scripts sem dados |
| — | (seguinte) | Este relatório e os logs de validação |

### Comparação funcional

`git diff ae7f98a release/0.11.0-limpa`:
- **Idênticos byte a byte:** `R/`, `man/`, os arquivos de teste, `NAMESPACE`,
  `DESCRIPTION`, `data/`, `vignettes/`, `inst/extdata`, `inst/CITATION`,
  READMEs, `NEWS.md`, `_pkgdown.yml`, `CITATION.cff`, `cran-comments.md` e
  `.Rbuildignore`.
- **Diferenças intencionais:**
  - 20 remoções (§4);
  - 5 arquivos saneados (2 relatórios, `FONTES.md`, 2 scripts);
  - 1 arquivo novo;
  - `.gitignore`;
  - o parágrafo de reprodução da nota metodológica.

## 6. Evidências de limpeza

| Verificação | Resultado |
|---|---|
| Varredura de blobs de `release/0.11.0-limpa ^origin/main ^origin/gh-pages` (12 commits novos, 155 blobs) | **0** códigos de estação; **0** assinaturas de registro, exceto 1 falso positivo já existente (`spelling.log`, números de linha, num commit original limpo); 7 menções **só ao nome** de um arquivo ou de uma coluna (`.Rbuildignore`, relatórios, scripts que leem a coluna `IQA_publicado`) |
| Caminhos retirados em **qualquer** árvore dos 12 commits novos (`git ls-tree -r`) | **0** ocorrências |
| Tarball construído de um clone limpo da branch (174 entradas, SHA-256 `9c076d44f13475b3a2912f05d4c481762eb29e0e36ca3913f53e9bc6d80c9692`) | 0 códigos de estação; 0 marcadores; nenhuma fixture, CSV ou relatório IGAM por amostra. 1 coincidência de assinatura na vinheta HTML: falso positivo já analisado (temperatura do `wq_demo`, base64, números de linha) |
| Remotos | `ls-remote` igual antes e depois; nenhum push |

**Limitação da varredura:** é textual. Ela não detecta dados transformados,
como valores recalculados, imagens, conteúdo comprimido ou outra
formatação. Para reduzir esse risco, todo arquivo que tocava os dados IGAM
foi revisado na classificação do §4, e os arquivos retirados foram retirados
**inteiros**.

## 7. Validações na branch limpa

Logs em `audit/resultados/sanitizacao_0_11_0/`.

| Etapa | Resultado |
|---|---|
| `devtools::document()` | Nenhuma alteração em `man/` ou `NAMESPACE` |
| `devtools::test()` | 37 arquivos, 181 testes, 803 verificações, **0 falhas, 0 erros**, 11 avisos (antigos), 4 pulados (3 por falta de pandoc no PATH, 1 por desenho). Inclui os sintéticos IGAM e o `mk_seasonal()` efetivo |
| Regressão CETESB | 131/131: máx. \|Δ bruto\| = 0, \|Δ exibido\| = 0, 0 classes diferentes |
| `R CMD build` (clone limpo da branch) | `tikatuwq_0.11.0.tar.gz` |
| `R CMD check --as-cran --no-manual` | **0 erros, 0 avisos, 1 NOTE** (ver abaixo) |
| Testes dentro do check (modo CRAN) | FAIL 0, WARN 11, SKIP 16, PASS 736 |
| Instalação a partir do tarball (biblioteca limpa) e smoke tests | CETESB 74,96 "Boa"; IGAM sintético 75,5 "Bom" (bruto 75,47912, igual ao cálculo independente); erro explicativo sem `nitrate_unit`; erro de migração de `classification`; `iqa_methods()`; `compare_iqa()` com 3 pares; `classify_iqa()`; nota metodológica instalada; `citation()` com DOI de conceito |
| `pkgdown::check_pkgdown()` | "No problems found". As 46 exportações estão no índice (igual à branch original) |
| Metadados | Versão 0.11.0; DOI de conceito `10.5281/zenodo.17407296` em `inst/CITATION` e `CITATION.cff`. O DOI `…17407297` aparece só no comentário que explica ser o da v0.7.0 |

**Notas ambientais:**
- **"unable to verify current time":** os servidores de hora usados pelo R
  estão inacessíveis desta máquina. Já ocorreu na rodada anterior; em
  `bdfb0b4` o item passou.
- **Check feito no próprio worktree:** dá também a NOTE "hidden files:
  .git", porque num worktree `.git` é um arquivo. O resultado reportado é o
  do **clone limpo**, onde essa NOTE não ocorre.
- **Manual PDF:** não gerado, por falta de LaTeX (`--no-manual`).

**Validação histórica externa 49/49:**
- **Leitura:** foi reexecutada lendo **somente** de
  `<PRIVATE_RESEARCH_DIR>/dados_privados/`.
- **Resultado:** 49/49 iguais na 1ª casa; erro máximo 0,048876; 6
  limítrofes entre 0,0457 e 0,0489.
- **Saídas por amostra:** gravadas lá. O resultado é **idêntico** ao
  arquivado de `ae7f98a`, e o novo extrato é idêntico célula a célula ao
  anterior.
- **No repositório:** só a tabela agregada
  (`audit/resultados/auditoria_final_49_metricas.csv`, sem alteração).

## 8. Arquivos do mantenedor

- **Árvore de trabalho original** (`<LOCAL_WORKDIR>\tikatuwq`, branch original):
  - `git status` idêntico ao do backup;
  - o diff das alterações não commitadas é idêntico ao patch salvo;
  - estão preservados `CRAN-SUBMISSION`, `docs/`, os 3 logs antigos e os
    arquivos não rastreados (`atividade.R`, `scratch_*`, `user_spec*`,
    `tests/baseline_*`, `Rplots.pdf`, `docs/EXEMPLOS_TIKATUWQ.html`).
- **Na branch limpa:** essas alterações **não** foram incluídas.
  `CRAN-SUBMISSION` e `docs/` ficam na versão de `origin/main`.

## 9. Pendências

- **Plataformas:** win-builder, macOS e manual PDF não executados.
- **Rede:** refazer o check com acesso aos servidores de hora.
- **Decisões do mantenedor:**
  - revisar a branch `release/0.11.0-limpa`;
  - autorizar o primeiro push dela (a branch original **não** deve ser
    enviada);
  - decidir o destino das alterações não commitadas (`CRAN-SUBMISSION`,
    `docs/`).
- **Limitações científicas** (sem mudança): validação IGAM só na Pampulha;
  sem homologação do IGAM; regras de censura, cloreto e arredondamento são
  políticas do pacote; pH 9–10 e ΔT da CETESB pendentes.
- **Limpeza futura, opcional e só com autorização:** depois da publicação da
  branch limpa, o mantenedor pode apagar a branch original e o worktree
  locais. Até lá, eles ficam como recuperação.

## 10. Confirmação

**Nenhum** push, force-push, tag, release, envio ao CRAN, depósito no Zenodo
ou publicação de site foi feito. A branch original, o backup e os dados de
pesquisa não foram apagados.
