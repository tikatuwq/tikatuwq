# Saneamento final de caminhos locais no histórico: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Mandato:** `PROMPT_TIKATUWQ_011_SANITIZACAO_FINAL_E_TESTES_EXTERNOS.md`,
  com autorização expressa do mantenedor para sanear a série local não
  publicada.
- **Marcadores usados neste documento e nos arquivos saneados:**
  `<USER_HOME>`, `<LOCAL_WORKDIR>`, `<PRIVATE_RESEARCH_DIR>`, `<BACKUP_DIR>`.

## 1. Pré-verificação

| Item | Valor |
|---|---|
| Data (UTC) | 2026-10-16T18:28Z |
| Branch de origem | `release/0.11.0-limpa` @ `3f28fa72b5039c02811f18904b06560c17cd0568`, árvore limpa (só `tests/testthat/Rplots.pdf`, gerado pelos testes, não rastreado) |
| Remoto (`git ls-remote`, antes e depois) | `main` = `4e88fa9b6aaf2ff53a9a9c4203110fedfbde9d61`; `gh-pages` = `0800938eca47225f4b063cadca2e4ac8b4388d37` |
| Ancestral comum | `4e88fa9` |
| Commits locais não publicados | 17 |
| Worktrees | árvore original (`fix/iqa-scientific-stabilization`, intocada); `release/0.11.0-limpa` (preservada); `release/0.11.0-publicavel` (nova) |

## 2. Backup (novo; o anterior foi preservado)

- **Local:** um novo `<BACKUP_DIR>`, desta rodada, **fora de qualquer
  repositório**.
- **Conteúdo:**
  - bundle com todas as refs (`git bundle verify` ok);
  - `refs.txt` e o status das duas árvores de trabalho;
  - cópia exata, no `3f28fa7`, dos 22 arquivos a sanear;
  - inventário inicial;
  - manifesto SHA-256 de 28 arquivos.
- **Restauração testada:** clone descartável do bundle com `fsck` sem erros.
  Ele restaura `release/0.11.0-limpa` (`3f28fa7`) e a branch original
  (`ae7f98a`).
- **Logs integrais desta rodada** (com dados da máquina): ficam **só** no
  backup. No repositório entram apenas as cópias saneadas.

## 3. Inventário (antes)

- **Varredura de todos os blobs** de `release/0.11.0-limpa ^origin/main
  ^origin/gh-pages` e das mensagens de commit: 17 commits, 212 blobs.
- **Ocorrências por categoria** (número de blobs):

| Categoria | Blobs |
|---|---|
| Usuário do Windows | 15 |
| Scratchpad temporário | 15 |
| Diretório de trabalho local | 10 |
| Pasta privada de pesquisa | 5 |
| Pasta de backup | 1 |

- **Mensagens de commit:** 0 ocorrências.
- **Arquivos afetados:** 22 caminhos do repositório, **todos em `audit/`**:
  6 relatórios e 16 logs.
  - É o relato anterior de "17 arquivos", ampliado pela varredura de todas as
    versões.
  - Lista em `audit/resultados/validacao_candidato_0_11_0/inventario_caminhos_antes.txt`
    (só categorias e caminhos relativos do repositório).
- **Nenhum arquivo do pacote** (`R/`, `man/`, `tests/`, `data/`, vinhetas,
  metadados) foi atingido.

## 4. Estratégia

- **Branch nova:** `release/0.11.0-publicavel`, criada a partir de `4e88fa9`.
  A `release/0.11.0-limpa` ficou **intacta** como recuperação.
- **Reconstrução commit a commit:** os 17 commits foram refeitos na mesma
  ordem, com as mesmas mensagens, o mesmo autor e a mesma data de autor.
  - Cada blob **introduzido** em cada commit passa por um saneamento
    determinístico **antes** de entrar no objeto do git. Assim, nenhuma
    versão intermediária contém os caminhos.
  - Só foram usados comandos internos do git (`hash-object --no-filters`,
    índice temporário, `write-tree`, `commit-tree`, `update-ref`): bytes
    exatos, sem conversão de fim de linha, e sem `filter-repo`, rebase ou
    reescrita de ref remoto.
- **Proteção:** o script **aborta** se o saneamento tentar alterar arquivos
  protegidos: `R/`, `man/`, `tests/`, `data/`, `inst/extdata/`,
  `vignettes/`, `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, `README*`,
  `_pkgdown.yml`, `CITATION.cff`, `inst/CITATION`, `inst/methodology/`.
  Não abortou.
- **Regras de substituição:**
  - caminhos do scratchpad da sessão → `<LOCAL_WORKDIR>`;
  - caminhos com o usuário do Windows ou do WSL → `<USER_HOME>`;
  - pasta privada de pesquisa → `<PRIVATE_RESEARCH_DIR>`;
  - pastas de backup → `<BACKUP_DIR>`;
  - prefixo do diretório de trabalho local → `<LOCAL_WORKDIR>`.

  Referências científicas, DOIs, títulos e caminhos relativos do
  repositório não foram tocados.

## 5. Correspondência de commits

`audit/resultados/validacao_candidato_0_11_0/mapa_commits_limpa_para_publicavel.tsv`:

| Original (`release/0.11.0-limpa`) | Novo (`release/0.11.0-publicavel`) | Substituições |
|---|---|---|
| `65fe8dd` | `6ad00ce` | 0 |
| `a0ba15d` | `2647356` | 0 |
| `9286735` | `5a7d586` | 0 |
| `fc3e770` | `9bda416` | 1 |
| `23707fe` | `1f9f57f` | 0 |
| `5338991` | `ff7e2f0` | 0 |
| `af3ac62` | `e9a7783` | 0 |
| `dc368cf` | `8a6b0cc` | 1 |
| `578015e` | `884225b` | 0 |
| `0ac7041` | `763176a` | 0 |
| `26dc2ae` | `077b169` | 0 |
| `0bc84cf` | `95723cb` | 17 (12 arquivos) |
| `3974fb4` | `6f0c768` | 11 (6 arquivos) |
| `5c7d05b` | `62041e7` | 0 |
| `68441b7` | `51ab50d` | 11 (2 arquivos) |
| `0defe7f` | `56ac5d7` | 4 (1 arquivo) |
| `3f28fa7` | **`21069f5f6013375350f23b1462c8cc7f47f8bf89`** | 1 |

- **Hashes:** todos são novos, como esperado: o pai muda, e com ele todos os
  hashes seguintes.
- **Commits seguintes:** os desta rodada (relatórios e logs) vêm depois do
  `21069f5`; os hashes estão na mensagem final.

## 6. Varredura final

**Faixa:** todos os blobs de `release/0.11.0-publicavel ^origin/main
^origin/gh-pages` e as mensagens dos 17 commits.

| Verificação | Resultado |
|---|---|
| Usuário do Windows ou WSL, pasta de pesquisa, pasta de backup | **0** |
| Identificadores do scratchpad (UUID da sessão, diretório da sessão) em qualquer árvore dos 17 commits | **0** |
| Resíduo genérico | 5 blobs mantêm `<USER_HOME>\AppData\Local\Temp\…` (sem usuário e sem nome privado) e 1 menção a uma pasta temporária `tkw_site_0110`. Aceito: não é pessoal |
| Registros individuais do IGAM (códigos de estação, assinaturas dos 49 registros, marcadores de tabela) | **0** dados. Restam menções a nomes de arquivo ou coluna e o falso positivo antigo em `spelling.log` (números de linha) |
| Segredos (tokens do GitHub, chaves privadas, AWS, `password=`/`token=` literais) no diff da série | **0** |
| Commits contaminados como ancestrais (os 10 da branch original e os da `release/0.11.0-limpa`) | **0** |
| Workflows `.github/` alterados na série | **0** |
| Binários novos | Só `inst/audit/curvas_qi_pacote.png` (curvas qi CETESB, inspecionada antes) |

**Limitação:** a varredura é textual. O único binário foi inspecionado à mão.

## 7. Equivalência funcional

Comparação `release/0.11.0-limpa` × `release/0.11.0-publicavel`:
- **Idênticos byte a byte:** `R/`, `man/`, `tests/`, `data/`,
  `inst/extdata/`, `vignettes/`, `DESCRIPTION`, `NAMESPACE`, `NEWS.md`,
  `README*`, `_pkgdown.yml`, `CITATION.cff`, `inst/CITATION`,
  `inst/methodology/`, `inst/audit/`, `.Rbuildignore`, `.gitignore`,
  `.github/` e `cran-comments.md` (antes dos commits desta rodada).
- **Diferentes:** 22 arquivos de `audit/`, cada um **exatamente igual** ao
  saneamento aplicado à versão da branch limpa. Verificado arquivo por
  arquivo, sem nenhuma outra alteração.
- **Tarball:** o candidato tem os mesmos 161 arquivos do tarball testado do
  `0defe7f`; a única diferença é a linha `Packaged:` do `DESCRIPTION`.

## 8. Fast-forward

`origin/main` (`4e88fa9`) é ancestral de `release/0.11.0-publicavel`, então
uma futura atualização da `main` pode ser **fast-forward, sem `--force`**.
Verificação local; nada foi enviado.

## 9. Situação

- **Branches:** `release/0.11.0-limpa` (`3f28fa7`) e
  `fix/iqa-scientific-stabilization` (`ae7f98a`) ficam como recuperação e
  **não devem ser publicadas**.
- **Dados:** os dois backups e o arquivo privado de pesquisa estão intactos.
