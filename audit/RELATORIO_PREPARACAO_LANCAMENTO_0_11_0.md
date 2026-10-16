# Prontidão de lançamento: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Mandato:** `PROMPT_PREPARACAO_PUBLICACAO_TIKATUWQ_0_11_0.md`.
  - **Autorizados:** preparação local e commits locais justificados.
  - **Não autorizados:** push, tag, release, envios externos, DOI, ORCID,
    CRAN e site.
- **Local:** `audit/`, fora do tarball.
- **Plano de publicação:** `audit/PLANO_PUBLICACAO_GITHUB_ZENODO_ORCID_CRAN_0_11_0.md`.

## 1. Veredito

**PRONTA COM RESSALVAS** para a decisão humana sobre o portão A (GitHub).
**Ainda não está pronta para submissão ao CRAN.**

### Critérios atendidos

Cada item foi verificado nesta execução:
- **Histórico:** nenhum commit novo nem arquivo publicável contém registros
  por amostra do IGAM.
- **Testes offline:** 181 testes, 0 falhas.
- **Regressão CETESB:** 131/131 sem diferença.
- **IGAM:** 49/49 em agregado, a partir do arquivo privado.
- **`R CMD check --as-cran`** sobre o tarball de um clone limpo: 0 erros, 0
  avisos, 1 nota de ambiente.
- **Instalação limpa e smoke tests:** ok.
- **pkgdown:** `check_pkgdown()` sem problemas e site gerado só localmente,
  sem erros de exemplo nem links quebrados.
- **Metadados:** coerentes (versão 0.11.0 e DOI de conceito).

### Ressalvas

Nenhuma bloqueia o portão A. Todas bloqueiam o portão E (CRAN) até serem
resolvidas:
1. **Não executados:** win-builder (R-devel e release), macOS e Linux.
2. **Manual PDF:** bloqueado por ambiente (sem LaTeX).
3. **Nota "unable to verify current time":** de ambiente. Os serviços de
   hora externos estão fora do ar; o CRAN responde normalmente.
4. **Risco de automação:** um merge na `main` dispara a publicação do site
   (workflow `pkgdown.yaml`). Os portões A e F estão acoplados; o plano
   explica como separá-los.

## 2. Identificação

| Item | Valor |
|---|---|
| Branch candidata | `release/0.11.0-limpa` (worktree `<LOCAL_WORKDIR>\tikatuwq_limpo`) |
| HEAD inicial desta rodada | `3974fb4` (confere com o relatado) |
| Commit testado (build, check, testes) | **`5c7d05b7cc8434470743f1f0f94fe78092aa31f2`** |
| Commit final (só acrescenta `cran-comments.md` e `audit/`, ambos fora do tarball) | registrado na §9 |
| Ancestral remoto | `4e88fa9` (`origin/main`). `git ls-remote` no início e no fim: `main` = `4e88fa9`, `gh-pages` = `0800938` |
| Commits à frente de `origin/main` | 14 no commit testado; 15 com o commit deste relatório |
| Tarball | `tikatuwq_0.11.0.tar.gz`, 174 entradas, **SHA-256 `179111437223be212b11b1f7d74b6296f5bace5df36b97052f65faa1cb484ef1`**, construído de um clone limpo de `5c7d05b` |

**Branch que nunca deve ser publicada:** `fix/iqa-scientific-stabilization`
(`ae7f98a`). O histórico dela contém os registros do IGAM.

**Fora da branch, deliberadamente:**
- as alterações não commitadas da árvore original (`CRAN-SUBMISSION` com o
  registro da 0.10.0, `docs/` regenerado, logs antigos e arquivos pessoais);
- o arquivo privado de pesquisa;
- o backup.

Recomendações:
- **`docs/`:** não incorporar. O site é gerado pelo workflow; regenerar só no
  portão F.
- **`CRAN-SUBMISSION`:** é regravado pelo `devtools` ao submeter. A versão da
  branch (0.9.0) é antiga e está fora do tarball; não reutilizar como
  registro da 0.11.0.

**Diff público em relação a `origin/main`:**
- `R/`, `man/`, `tests/`, `vignettes/`;
- documentação (README, NEWS, citações, pkgdown);
- `inst/audit/` e `inst/methodology/`;
- `audit/` (relatórios, scripts sem dados, resultados agregados, logs);
- `.Rbuildignore`, `.gitignore`;
- DESCRIPTION, só a descrição.

## 3. Segurança do material publicável

| Verificação | Resultado |
|---|---|
| Ancestrais | Nenhum dos 10 commits contaminados (`7b5a9f0` … `ae7f98a`) é ancestral de HEAD (`git merge-base --is-ancestor`) |
| Varredura de blobs em `HEAD ^origin/main ^origin/gh-pages` (todas as versões intermediárias) | 0 códigos de estação; 0 assinaturas de registro. Restam só menções de nome de arquivo ou coluna e 1 falso positivo antigo (`spelling.log`, números de linha) |
| Caminhos retirados em qualquer árvore | 0 |
| Binários novos na faixa | Só `inst/audit/curvas_qi_pacote.png`. Inspecionada visualmente: curvas qi do motor **CETESB**, sem dados do IGAM |
| `docs/`, vinhetas, `.rda` | `.rda` sem alteração; vinhetas e `docs/PROPOSTA_INDICES_CUSTOMIZADOS.md` são de commits originais limpos e passaram na varredura |
| Tarball final | 0 códigos de estação; 0 marcadores; 0 caminhos pessoais (`<LOCAL_WORKDIR>`, `C:\Users`, `AppData`); 0 padrões de segredo. 1 coincidência de assinatura na vinheta HTML: falso positivo conhecido |
| Pastas privadas | `<PRIVATE_RESEARCH_DIR>` e o backup ficam fora de qualquer repositório |
| Scripts de auditoria | Recusam diretório dentro do repositório; gravam saídas por amostra só em `<dir_privado>/saidas_*` |
| `.Rbuildignore` × `.gitignore` | Tratados separadamente. O histórico foi saneado por reconstrução, não por `git rm --cached` |

**Limitação:** a varredura é textual. Dados transformados, em imagem ou
comprimidos podem escapar a ela. Por isso o único binário novo foi
inspecionado à mão.

## 4. Verificações executadas

**Ambiente:**
- Windows 11 x64 (build 26200), R 4.5.1 (ucrt), Rtools 4.5;
- devtools 2.4.5, testthat 3.2.3, roxygen2 7.3.3, pkgdown 2.1.3,
  rcmdcheck 1.4.0, rmarkdown 2.30, knitr 1.51;
- pandoc 3.8.3 (do RStudio);
- **sem LaTeX**.

Logs em `audit/resultados/preparacao_lancamento_0_11_0/`. Nos logs, os caminhos locais do usuário foram mascarados (`<TMP>`, `<HOME>`, `<dir_privado>`); nenhum outro conteúdo foi alterado.

| Verificação | Status | Evidência |
|---|---|---|
| `devtools::document()` (clone limpo) | **PASSOU** | Nenhuma alteração em `man/` ou `NAMESPACE` (`document.log`) |
| `devtools::test()` | **PASSOU** | 37 arquivos, 181 testes, 803 verificações, 0 falhas, 0 erros, 11 avisos (antigos), 4 pulados por ambiente ou desenho (`testthat.log`) |
| Regressão CETESB | **PASSOU** | 131/131, máx. \|Δ\| = 0, 0 classes diferentes (`regressao_cetesb.log`) |
| Testes sintéticos IGAM com valores independentes | **PASSOU** | Incluídos no `test()` e no check |
| IGAM 49/49, a partir do arquivo privado | **PASSOU** | Só agregados: 49/49 iguais na 1ª casa, máx. 0,048876, 6 limítrofes (`igam_49_agregado.log`; caminho privado mascarado) |
| `R CMD build` (clone limpo) | **PASSOU** | `build.log`, SHA-256 na §2 |
| `R CMD check --as-cran --no-manual` | **PASSOU com 1 NOTE** | 0 E / 0 W / 1 N (hora). Viabilidade de entrada no CRAN e URLs OK; exemplos e `--run-donttest` OK; testes OK (736 passaram, 16 pulados no modo CRAN); vinhetas OK (`00check.log`) |
| Manual PDF | **BLOQUEADO POR AMBIENTE** | Rd → LaTeX ok; "pdflatex is not available" (`manual_pdf.log`) |
| Nota de hora | **NOTE (ambiente)** | worldtimeapi sem resposta, worldclockapi HTTP 503, CRAN HTTP 200 (`servidores_hora.log`). A verificação **não** foi desligada |
| Instalação do tarball em biblioteca limpa | **PASSOU** | `install.log` |
| Smoke tests | **PASSOU** | CETESB 74,96 "Boa"; IGAM sintético 75,5 "Bom" (= cálculo independente); erros de `nitrate_unit` e migração; `iqa_methods`; `compare_iqa` (3 pares); `classify_iqa`; nota metodológica; `citation()` (`smoke.log`) |
| `pkgdown::check_pkgdown()` e `build_site()` local | **PASSOU** | "No problems found"; 132 arquivos; páginas de `compare_iqa`, `iqa_methods` e `iqa` presentes; 0 erros de exemplo; 0 links internos quebrados (`pkgdown.log`). O site foi gerado num diretório temporário e **não** foi publicado |
| Dependências reversas | **PASSOU** | Nenhuma no CRAN (o CRAN tem a 0.10.0) (`revdeps.log`) |
| Linux | **NÃO EXECUTADO** | O WSL Ubuntu não tem R; o Docker não está ativo. Comandos no plano |
| macOS | **NÃO EXECUTADO** | Sem acesso local; serviços remotos não autorizados |
| win-builder (R-devel e release) | **NÃO EXECUTADO** | Envio externo não autorizado |

**Observações sobre o ambiente local:**
- A biblioteca do usuário tem o `tikatuwq` **0.8.2** instalado. Por isso o
  site foi construído com `install = TRUE`, para usar a 0.11.0.
- Num diretório de nome longo, o pkgdown falha pelo limite de 260
  caracteres do Windows. O site local foi gerado a partir de um clone em
  caminho curto.

## 5. Metadados

| Item | Situação |
|---|---|
| `DESCRIPTION` | Version 0.11.0. Autoria, mantenedor e ORCIDs já existentes; MIT; URL; BugReports; R ≥ 4.1. **Corrigido (`5c7d05b`):** a descrição não mencionava o motor IGAM nem `compare_iqa()`. Alteração só de texto, testada |
| `LICENSE` / `LICENSE.md` | MIT, "2025 Vinicius Saraiva Santos". Coerente com o DESCRIPTION |
| `inst/CITATION` | DOI de conceito `10.5281/zenodo.17407296`, ano 2026, "R package version 0.11.0". O DOI `…17407297` aparece só no comentário, como v0.7.0, **confirmado agora pela API do Zenodo** |
| `CITATION.cff` | version 0.11.0; doi de conceito; sem `date-released` (não lançada). Sugestão para depois: acrescentar ORCID ao autor e alinhar o título com o DESCRIPTION. Não alterei, por ser conteúdo editorial do mantenedor |
| READMEs | Exemplos CETESB, IGAM e `compare_iqa` sintéticos e com unidades; limitações; DOI de conceito |
| `NEWS.md` | Mudança incompatível (`classification` → `classify_iqa()`); motor IGAM (não homologado); `compare_iqa()`; `nitrate_unit`; regras de fósforo, cloreto e microbiologia; pendências conhecidas |
| `inst/methodology/IGAM_VALIDATION.md` | Equações, fontes por página, justificativa do 1,5, 49/49 só em agregado, separação entre testes sintéticos e validação histórica, semântica de `iqa_methods()` |
| `iqa_methods()` | `reference_validated = TRUE` documentado como reprodução de valores publicados, **não** homologação |
| `_pkgdown.yml` | 46/46 exportações (`check_pkgdown` ok) |
| `cran-comments.md` | **Atualizado** com os resultados desta rodada (commit da §9): 1 NOTE explicada; pendências; dependências reversas |
| `CRAN-SUBMISSION` | Registro antigo (0.9.0) na branch, fora do tarball. Não reutilizar |

## 6. Zenodo, ORCID, GitHub e CRAN (resumo)

Os detalhes estão no plano.

- **Zenodo:**
  - o conceito `10.5281/zenodo.17407296` tem **uma** versão: 0.7.0, DOI
    `10.5281/zenodo.17407297`, de 2025-10-21;
  - não há outros registros com "tikatuwq";
  - o DOI de conceito redireciona para a versão mais recente (`17407297`);
  - modo do depósito anterior: **não confirmado**. Há indício forte de envio
    manual (arquivos `.tar.gz` e `.zip` montados à mão, sem identificador
    relacionado ao GitHub). É preciso conferir na conta;
  - fluxo recomendado: **nova versão manual** a partir do registro
    `17407297`.
- **ORCID** (registro público 0009-0007-1387-7927): 2 trabalhos, **nenhum** é
  o software do Zenodo. Itens privados não aparecem, então é preciso
  conferir na conta antes de incluir.
- **GitHub:** um merge na `main` dispara a publicação do site. As tags e
  releases não disparam nada nos workflows atuais. O que acontece no Zenodo
  depende de a integração estar desligada, o que precisa ser verificado.

## 7. Bloqueadores e ressalvas

**Bloqueadores reais:** nenhum para o portão A. Para o portão E (CRAN):
1. win-builder R-devel/release;
2. manual PDF;
3. preferencialmente Linux e macOS.

**Ressalvas aceitas:**
- a NOTE de hora (ambiente);
- 11 avisos antigos em testes de `clean_units` e `mk_seasonal`;
- validação IGAM restrita à Pampulha e sem homologação (documentada);
- pH 9–10 e ΔT da CETESB pendentes (documentados, risco baixo);
- metadados em atributos, que se perdem em `merge()` e CSV.

**Ações externas: todas NÃO EXECUTADAS.**
- push, PR, tag, release;
- Zenodo (versão ou reserva de DOI);
- ORCID;
- CRAN;
- site;
- win-builder, R-hub, CI.

## 8. Artefatos locais

- **Tarball testado:** `tikatuwq_0.11.0.tar.gz`, SHA-256
  `179111437223be212b11b1f7d74b6296f5bace5df36b97052f65faa1cb484ef1`, do
  commit `5c7d05b`.
  - Está no scratchpad desta sessão, que é temporário.
  - Para submeter, **reconstruir** a partir do commit final e conferir o
    conteúdo (§9). Um novo build muda o SHA-256 por causa do carimbo
    `Packaged`.
- **Site local:** `<USER_HOME>\AppData\Local\Temp\tkw_site_0110`
  (descartável).
- **Logs:** `audit/resultados/preparacao_lancamento_0_11_0/`.

## 9. Commits locais desta rodada

| Hash | Conteúdo |
|---|---|
| `5c7d05b` | `DESCRIPTION`: a descrição passa a mencionar o motor IGAM e `compare_iqa()` (só texto) |
| (seguinte) | `cran-comments.md` atualizado, este relatório, o plano e os logs (todos fora do tarball) |
