# Validação final do RC2: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Mandato:** `PROMPT_VALIDACAO_FINAL_RC2_TIKATUWQ_0_11_0.md`. Autoriza
  testes locais e o envio **só** do RC2 ao win-builder (R-devel e
  R-release) e ao mac builder (R-release). Nenhuma publicação.
- **Logs:** cópias saneadas em `audit/resultados/validacao_final_rc2_0_11_0/`.
  Os originais estão no backup privado.

## 1. Identidade

| Item | Valor |
|---|---|
| Branch | `release/0.11.0-publicavel`, no worktree `<LOCAL_WORKDIR>/tikatuwq_publicavel`. O `<LOCAL_WORKDIR>/tikatuwq_limpo` citado no prompt contém a `release/0.11.0-limpa`, que não é publicável, e não foi usado |
| HEAD no início | `072e4579444fb66dcdffa5d857b4d5e33251808b`, árvore limpa |
| Commit do conteúdo do pacote | `14d015ab34274f250795f480fb2b289284ed5f1e`. Commits posteriores só mudam `audit/`, `cran-comments.md` e `README-pt.md`, todos fora do tarball pelo `.Rbuildignore` |
| Remoto (`git ls-remote`) | `main` = `4e88fa9b6aaf2ff53a9a9c4203110fedfbde9d61`, `gh-pages` = `0800938eca47225f4b063cadca2e4ac8b4388d37`, inalterados. `4e88fa9` é ancestral do HEAD, então o avanço da `main` seria fast-forward |
| Commits proibidos como ancestrais | `ae7f98a`, `3f28fa7` e `0defe7f` **não** são ancestrais |
| RC2 | `<LOCAL_WORKDIR>/tikatuwq_release_candidato_0_11_0/rc2_14d015a/tikatuwq_0.11.0.tar.gz` |
| SHA-256 | **`0775fc48e6659c7698f7043cbcb9a906c56482219d45a5196373cc4f282b793d`**, conferido no início, antes de cada um dos 3 envios e no Linux |
| Tamanho e entradas | 317.353 bytes; 174 entradas (161 arquivos) |

## 2. RC1 × RC2

- **Lista de arquivos:** idêntica (174 entradas).
- **Arquivos diferentes:** só `README.md`, `NEWS.md` e `DESCRIPTION`. No
  `DESCRIPTION`, só a linha `Packaged:` muda: `18:58:42` no RC1, `22:33:24` no
  RC2.
- **Código, `man/`, `tests/`, `data/`, `inst/` e vinhetas:** idênticos byte a
  byte.

**RC2 × git `14d015a`** (150 arquivos versionados presentes no tarball):
- Todos idênticos ao git, exceto o fim de linha. O clone no Windows usa CRLF,
  e o RC1 foi gerado do mesmo modo.
- O `DESCRIPTION` difere só pelo que o `R CMD build` gera: reformatação de
  `Imports`, `Suggests` e `URL`, e os campos `NeedsCompilation`, `Packaged` e
  `Author`.
- Arquivos que só existem no tarball, gerados no build: `build/` e
  `inst/doc/`.
- `README.md` e `NEWS.md` do RC2 são iguais aos do `14d015a`.

**Conclusão:** nenhuma diferença inesperada de implementação.

## 3. Privacidade e dados do IGAM

| Verificação | Resultado |
|---|---|
| Caminhos pessoais e pastas privadas no RC2 | 0 |
| Códigos de estação `PVnnn` no RC2 | 0 |
| Segredos (tokens, chaves, `password=`/`token=`) no RC2 | 0 |
| Planilhas ou binários de dados (`xls*`, `ods`, `pdf`, `zip`) no RC2 | 0 |
| Assinaturas dos 49 registros IGAM (≥ 3 de 4 valores) e marcadores de tabela no RC2 | 1 coincidência numérica em `inst/doc/tikatuwq-workflow.html`. É o falso positivo já conhecido, num arquivo idêntico ao do RC1 |
| Mesma varredura no histórico `release/0.11.0-publicavel ^origin/main ^origin/gh-pages` (23 commits, 252 blobs) | Só menções a nomes de arquivo e coluna, e o falso positivo antigo em `spelling.log`. 0 dados |
| `Packaged: … ; <usuário>` | Metadado gerado pelo `R CMD build`, presente também no RC1. Não editado |
| Arquivo privado de pesquisa | `MANIFESTO_SHA1.txt`: 65 arquivos inalterados. Foi usado só para ler a tabela de referência da varredura. Nada foi copiado |

## 4. Matriz de execução

| Ambiente | Situação para o RC2 | ERROR | WARNING | NOTE |
|---|---|---|---|---|
| Windows 11 local, R 4.5.1, `--as-cran` com manual PDF | **Executado** sobre o tarball recém-construído, de SHA idêntico ao RC2 (rodada anterior, `RELATORIO_TESTES_EXTERNOS_0_11_0.md` §6) | 0 | 0 | 0 |
| Linux: WSL Ubuntu 26.04, R 4.5.3 (micromamba), `--as-cran --no-manual` (sem LaTeX) | **Executado** sobre o RC2 exato, sem rebuild | 0 | 0 | 0 |
| macOS R-release: R 4.6.1 Patched r90311, aarch64, macOS Tahoe 26.6, Apple M1 | **Executado** sobre o RC2 | 0 | 0 | 0 (Status OK) |
| win-builder R-devel | **Enviado** às 23:16 UTC; **resultado pendente** (e-mail ao mantenedor) | — | — | — |
| win-builder R-release | **Enviado** às 23:16 UTC; **resultado pendente** | — | — | — |
| win-builder R-devel e R-release com o RC1 | *Resultado de outra versão*, não do RC2 | 0 | 0 | 1 cada |

**Linux** (`linux/linux_resumo.log`):
- Testes no modo CRAN: 736 aprovados, 16 pulados, 0 falhas.
- Exemplos, vinhetas e "CRAN incoming feasibility" ok.
- Não apareceu a NOTE de horário. O teste de rede deu worldtimeapi.org HTTP
  000, worldclockapi.com 200 e CRAN 200.
- Smoke na biblioteca limpa:
  - CETESB: 74,96 "Boa";
  - IGAM: 75,5 "Bom", com valor bruto igual ao valor independente (Python) até
    1e-10;
  - erro sem `nitrate_unit`;
  - `iqa_methods()`: CETESB e IGAM `available` e `reference_validated`;
  - `compare_iqa()`: 3 pares válidos (P4 inelegível no IGAM por falta de
    cloreto);
  - `classify_iqa()` nas escalas IGAM e CETESB;
  - `citation()` com o DOI de conceito.

**macOS** (`mac_release/`):
- O cliente envia o arquivo exato por POST multipart a
  `mac.r-project.org/macbuilder/v1/submit` (script em
  `audit/scripts/testes_externos_0_11_0.R`).
- O serviço não informa o hash recebido. Prova de identidade: o pacote
  instalado no builder (`results.tar.bz2`) tem
  `Packaged: 2026-10-16 22:33:24 UTC`, igual ao RC2, e o `NEWS.md` instalado é
  idêntico ao do RC2. O serviço **não reconstruiu** o pacote.
- Biblioteca de dependências do CRAN presente (`RLIBS` com `CRANlib`).
- Exemplos, testes, vinhetas e manual PDF ok.

**win-builder:**
- **Ainda sem resultado.** Os resultados do RC1 **não** são usados como
  resultado do RC2.
- Com o RC1, a única NOTE foi "Possibly misspelled words in DESCRIPTION:
  Minas / Gerais". O `DESCRIPTION` de origem é idêntico no RC2, então é
  provável que ela se repita, mas isso só vale depois de confirmado.

## 5. NOTEs

| NOTE | Onde | Classificação |
|---|---|---|
| `Possibly misspelled words in DESCRIPTION: Gerais (25:62), Minas (25:56)` | win-builder, com o RC1. Para o RC2, aguardando | Falso positivo: nome próprio do estado, escrito corretamente. Justificado no `cran-comments.md`. `DESCRIPTION` não alterado |
| Nenhuma | Windows, Linux e macOS com o RC2 | — |

## 6. Documentação

- **README.md** (no tarball), **README-pt.md** (fora do tarball) e **NEWS.md**
  (no tarball) descrevem a 0.11.0:
  - correção do IQA CETESB;
  - mudança do IET: Lamparelli segundo a CETESB, separado para rios e
    reservatórios, e Carlson com logaritmo natural;
  - `nsfwqi()`: aproximação, com `NA` no modo estrito;
  - motor IGAM (reprodução sem homologação, dados não redistribuídos);
  - `iqa_methods()`, `compare_iqa()` e `classify_iqa()`;
  - resultados de verificação.
- O README-pt.md ganhou o link "Read in English", para ficar simétrico ao
  "Ler em Português" do README.md. Está fora do tarball e não muda o RC2.
- O `cran-comments.md` está fora do tarball (`.Rbuildignore`). Foi atualizado
  com os resultados do RC2 e marca o win-builder como **PENDENTE**.

## 7. CETESB 131 e IGAM 49 (não repetidos)

Não foram repetidos nesta rodada. O motivo:
- `R/`, `tests/`, `inst/` e `data/` do RC2 são idênticos byte a byte aos do
  RC1;
- o RC1, por sua vez, é idêntico ao commit `21069f5`, validado com CETESB
  131/131 e IGAM 49/49 (`RELATORIO_VALIDACAO_CANDIDATO_0_11_0.md`);
- a suíte de testes, que inclui a regressão CETESB e os casos sintéticos do
  IGAM, passou no Linux e no macOS com o RC2.

## 8. Congelamento

O **RC2 está congelado**. Qualquer alteração em arquivo distribuído (código,
`man/`, vinhetas, `README.md`, `NEWS.md`, `DESCRIPTION`) exige um novo tarball,
um novo SHA-256 e nova reavaliação.

## 9. Parecer

**APTA COM RESSALVAS.**

**Razões verificáveis:**
- identidade do RC2 confirmada;
- diferenças para o RC1 restritas à documentação e ao metadado de build;
- nenhum dado privado ou do IGAM;
- 0/0/0 no Windows, no Linux e no macOS.

**Ressalva única:** faltam os resultados do win-builder R-devel e R-release
para o RC2.
- **Passa a APTA** se ambos derem 0 ERROR e 0 WARNING, com no máximo a NOTE
  "Minas Gerais".
- **Fica BLOQUEADA** em caso de ERROR, WARNING ou NOTE nova.
- Os resultados devem ser registrados aqui e no `cran-comments.md`.

## 10. Próximo portão: GitHub `main` e site (não executado)

**Pré-verificação, só leitura:**
- `git ls-remote origin refs/heads/main` deve dar `4e88fa9`; se der outro
  valor, parar.
- `4e88fa9` deve ser ancestral do HEAD.
- `ae7f98a` e `3f28fa7` não podem ser ancestrais.

**Decisão sobre o site:** o push na `main` dispara o `pkgdown.yaml`, que
publica o site.
- **Recomendado:** autorizar o GitHub e o site juntos.
- **Alternativa:** desativar o workflow antes do push.

**Push:**
```
git push origin release/0.11.0-publicavel:refs/heads/main
```
- Sem `--force`, `--all` ou `--tags`.
- Depois, conferir que `git ls-remote origin refs/heads/main` dá o HEAD local.

**Depois do push:**
- acompanhar o R-CMD-check e o pkgdown no Actions;
- conferir o site publicado.

**Tag, release, Zenodo, CRAN e ORCID:** são portões separados
(`PLANO_PORTOES_PUBLICACAO_0_11_0.md`).

> Nenhum push, Pull Request, merge, tag, release, deploy pkgdown, depósito ou reserva de DOI, alteração no ORCID ou submissão ao CRAN foi realizado nesta etapa. Os únicos envios externos foram os do RC2 ao win-builder (R-devel e R-release) e ao mac builder (R-release), autorizados. Nenhum DOI da 0.11.0 foi criado; o DOI de conceito `10.5281/zenodo.17407296` segue como identificador do projeto.
