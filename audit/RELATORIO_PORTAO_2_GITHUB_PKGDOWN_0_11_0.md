# Portão 2: GitHub `main` e site pkgdown, tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Mandato:** `PROMPT_PORTAO_GITHUB_PKGDOWN_TIKATUWQ_0_11_0.md`, com
  autorização expressa do mantenedor para um push fast-forward de
  `release/0.11.0-publicavel` na `main` (sem PR e sem force) e para o deploy
  automático do site.
- **Fora do mandato:** tag, release, CRAN, Zenodo e ORCID.

## 1. Pré-voo

| Item | Valor |
|---|---|
| Worktree | `<LOCAL_WORKDIR>/tikatuwq_publicavel` (`git rev-parse --show-toplevel`); branch `release/0.11.0-publicavel`; `git status --porcelain=v1` vazio |
| HEAD no início | `f6ee8ab693773fd075e90e6013da34ea4fe1f2ec` |
| `origin` | `https://github.com/tikatuwq/tikatuwq.git` (fetch e push) |
| `origin/main` (`git ls-remote`) | `4e88fa9b6aaf2ff53a9a9c4203110fedfbde9d61`, como esperado. `gh-pages` = `0800938eca47225f4b063cadca2e4ac8b4388d37` |
| Fast-forward | `git merge-base --is-ancestor 4e88fa9 HEAD`: sim |
| Commits proibidos | `ae7f98a`, `3f28fa7` e `0defe7f`: nenhum é ancestral |
| RC2 | SHA-256 `0775fc48e6659c7698f7043cbcb9a906c56482219d45a5196373cc4f282b793d`, preservado |
| Arquivos distribuídos | Nenhum mudou desde `14d015a` (a origem do RC2). Os commits seguintes só mudam `audit/`, `cran-comments.md`, `README-pt.md` e `docs/`, todos no `.Rbuildignore` |

## 2. Achados do pré-voo e decisão do mantenedor

1. **O GitHub Pages serve a pasta `docs/` da `main`.** Indícios:
   - o push do `4e88fa9` disparou o "pages build and deployment", o modo de
     publicação a partir de branch;
   - o `Last-Modified` do site coincide com esse push.

   O site mostrava **0.8.2**, porque o `docs/` versionado era de 2025-12-27.
   O deploy por Actions do `pkgdown.yaml` (`actions/deploy-pages`) **não**
   publica com essa configuração.
2. **`R-CMD-check.yaml` falha desde a 0.10.0, por configuração do workflow.**
   - Anotações públicas: `Unexpected input(s) 'build-args'` (a action aceita
     `build_args`), `object 'check' not found` e
     `Input required and not supplied: path`.
   - Não é falha do pacote, que tem 0/0/0 em 5 ambientes.
3. **`pkgdown.yaml` falha no passo "Build site" desde a 0.10.0.** Sem token não
   foi possível ler o log completo.

**Decisão do mantenedor:** reconstruir o `docs/` localmente e publicá-lo no
mesmo push. Os workflows **não** foram alterados (proibido neste mandato).

## 3. Commits locais acrescentados antes do push

| Commit | Conteúdo |
|---|---|
| `01e977c` | Resultados definitivos do win-builder com o RC2, no relatório final do RC2 e no `cran-comments.md` (que deixa de ter PENDING) |
| `824e790` | `docs/` reconstruído com pkgdown 2.1.3 e pandoc 3.8.3, a partir de um clone limpo do `01e977c`. 83 arquivos, todos em `docs/` |
| este relatório | `audit/` |

**Sobre o `docs/` reconstruído:**
- versão exibida 0.11.0;
- os 50 `.Rd` têm página, e as 46 funções exportadas estão no índice de
  referência, inclusive `compare_iqa`, `iqa_methods` e `iqa_components`, que
  faltavam no site antigo;
- a página inicial, o `README-pt.html` e `news/index.html` trazem Lamparelli e
  Carlson, `nsfwqi()` e a ressalva de que o IGAM "não é homologado";
- nenhuma página diz que a 0.11.0 está no CRAN.

**Página nova no site:** `EXEMPLOS_TIKATUWQ.html`, gerada do
`EXEMPLOS_TIKATUWQ.md`, que já era público na `main` desde o `008f623`. Ela
declara ter sido executada com a v0.10.0 e não tem conteúdo do IGAM.

## 4. Varredura do que passa a ser alcançável pela `main`

**Faixa:** `release/0.11.0-publicavel ^origin/main ^origin/gh-pages`, com 26
commits e 345 blobs, antes deste relatório.

| Verificação | Resultado |
|---|---|
| Usuário do Windows ou WSL, pasta privada, pasta de backup, e-mail institucional, mensagens de commit | 0 |
| Resíduo genérico | 6 blobs com `<USER_HOME>\AppData\Local\Temp\…`, sem usuário nem UUID de sessão. Já aceito no saneamento |
| Registros individuais do IGAM | 0 dados. Só menções a nomes de arquivo ou coluna, o falso positivo antigo em `spelling.log`, e mais um abaixo |
| `docs/reference/plot_trend-1.png` marcado como `PVnnn` | **Falso positivo.** Os bytes brutos do PNG não contêm `PV` seguido de 3 dígitos; a coincidência surge porque o detector descarta bytes UTF-8 inválidos. A imagem é o exemplo sintético de `plot_trend()` (ponto "P1", turbidez), inspecionada visualmente |
| CSV do IGAM em `audit/resultados/` | Só métricas agregadas e curvas (≤ 17 linhas cada). Nenhum registro por amostra |
| Segredos (tokens do GitHub, chaves privadas, AWS) no diff da série | 0 |
| Workflows `.github/` alterados | 0 |
| Binários novos | `inst/audit/curvas_qi_pacote.png` (curvas da CETESB) e as figuras dos exemplos em `docs/reference/` (`plot_iet`, `wq_pca`, `plot_trend`) |

## 5. Resultados do RC2 (`RELATORIO_VALIDACAO_FINAL_RC2_0_11_0.md`)

| Ambiente | E / W / N |
|---|---|
| Windows 11 local, R 4.5.1, com manual PDF | 0 / 0 / 0 |
| Linux Ubuntu 26.04, R 4.5.3, `--no-manual` | 0 / 0 / 0 |
| macOS R-release, R 4.6.1 Patched, Apple M1 | 0 / 0 / 0 |
| win-builder R-devel (2026-10-14 r90673) | 0 / 0 / 1 |
| win-builder R-release (R 4.6.1) | 0 / 0 / 1 |

**A NOTE:** `Possibly misspelled words in DESCRIPTION: Gerais, Minas`. É o
nome próprio do estado, escrito corretamente, e está justificado no
`cran-comments.md`.

## 6. Execução

Comandos, sem credenciais:

```
git ls-remote origin refs/heads/main          # imediatamente antes: 4e88fa9…
git push origin refs/heads/release/0.11.0-publicavel:refs/heads/main
git ls-remote origin refs/heads/main          # deve igualar o HEAD enviado
```

O resultado do push, os workflows e o site estão na seção 7.
