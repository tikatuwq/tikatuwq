# Portões de publicação: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Natureza:** **nada abaixo foi executado.** Cada portão exige autorização
  expressa e separada do mantenedor.
- **Documentos substituídos:** este plano substitui, para a branch nova, os
  planos anteriores (`PLANO_LANCAMENTO_DIRETO_0_11_0.md` e
  `PLANO_PUBLICACAO_GITHUB_ZENODO_ORCID_CRAN_0_11_0.md`). O rascunho de
  metadados do Zenodo está no segundo.

## Regras permanentes

- **Fonte única de publicação:** `release/0.11.0-publicavel`.
- **Nunca publicar:**
  - `release/0.11.0-limpa`, porque os logs antigos dela têm caminhos
    pessoais;
  - `fix/iqa-scientific-stabilization`, porque tem dados do IGAM.
- **Comandos proibidos:** `--force`, `--all`, `--mirror`, `--tags` e
  `git push` sem refspec.
- **Divergência no remoto:** se `origin/main` ≠ `4e88fa9`, parar. Nada de
  merge ou rebase automático.

## Portão 1: testes externos (win-builder e macOS)

**O que é transmitido:**
- só o tarball candidato (código-fonte do pacote) a
  `win-builder.r-project.org` ou `mac.r-project.org`;
- nada de dados do IGAM, dados privados, caminhos pessoais ou segredos
  (verificado).

**Acesso aos resultados:**
- **win-builder:** envia um e-mail ao **mantenedor do DESCRIPTION** com um
  link para os resultados, que ficam alguns dias no servidor e são acessíveis
  por esse link;
- **mac builder:** devolve um link de resultados.

**Verificações antes do envio** (endereços consultados só por HEAD em
2026-10-16, HTTP 200):
- funções disponíveis no `devtools` 2.4.5 instalado: `check_win_devel()`,
  `check_win_release()`, `check_win_oldrelease()` e `check_mac_release()`;
- **não existe** `check_mac_devel()`.

**Script preparado e testado só em modo plano:**
`audit/scripts/testes_externos_0_11_0.R`.
- Confere o SHA-256 do tarball e recusa se divergir.
- **Sem** `TKW_AUTORIZO_TESTES_EXTERNOS=SIM`, só mostra o plano e sai.
- Para o win-builder, envia o **tarball exato** por FTP (`R-devel` ou
  `R-release`).
- Para o macOS, usa `devtools::check_mac_release()` sobre o clone do mesmo
  commit.

**Execução futura**, num clone limpo de `release/0.11.0-publicavel` e só
depois da autorização:

```
TKW_AUTORIZO_TESTES_EXTERNOS=SIM Rscript audit/scripts/testes_externos_0_11_0.R \
  <LOCAL_WORKDIR>/tikatuwq_release_candidato_0_11_0/tikatuwq_0.11.0.tar.gz \
  92cfc6c759d83f917774fa1cc0499b450ab14abc4cc25363721e65f18c3844c6 win-devel
# repetir com win-release e mac-release, se desejado
```

**Alternativa manual:** usar o formulário
https://win-builder.r-project.org/upload.aspx com o mesmo tarball, conferindo
o SHA-256 antes.

**Como interpretar:**
- **ERROR ou WARNING:** bloqueiam o CRAN; corrigir e repetir.
- **NOTE:** avaliar uma a uma. "New submission" não se aplica (o pacote já
  está no CRAN). Notas de hora ou URL podem ser de ambiente.

**R-hub v2 é opcional.** Ele usa GitHub Actions no repositório, o que exige
push de um workflow, um portão à parte.

## Portão 2: GitHub (`main`), junto com a decisão sobre o site

**Pré-verificação (só leitura):**

```
git ls-remote origin refs/heads/main            # = 4e88fa9b6aaf2ff53a9a9c4203110fedfbde9d61
git merge-base --is-ancestor origin/main release/0.11.0-publicavel && echo ff-ok
git merge-base --is-ancestor ae7f98a release/0.11.0-publicavel && echo PARAR || echo ok
git merge-base --is-ancestor 3f28fa7 release/0.11.0-publicavel && echo PARAR || echo ok
```

Repetir também as varreduras de caminhos e de dados do IGAM sobre
`release/0.11.0-publicavel ^origin/main ^origin/gh-pages`.

**Gatilhos confirmados:**
- `.github/workflows/pkgdown.yaml`: `on: push: branches [main, master]`, com
  deploy em `actions/deploy-pages` quando `github.ref == refs/heads/main`.
  **O push na `main` publica o site.**
- `.github/workflows/R-CMD-check.yaml`: push e pull_request para
  `main`/`master` (CI no GitHub Actions; sem deploy).

**Decisão necessária antes do push** (cada opção exige autorização):

| Opção | Como | Consequência |
|---|---|---|
| (a) | Autorizar **GitHub e site juntos** | Recomendada; site já validado localmente |
| (b) | Antes do push, `gh workflow disable pkgdown --repo tikatuwq/tikatuwq`; depois, `gh workflow enable pkgdown` | O site fica na versão antiga até reativar |
| (c) | Commit que muda o gatilho para `workflow_dispatch` | Muda o comportamento futuro do repositório |

**Push** (fast-forward, refspec explícito):

```
git push origin release/0.11.0-publicavel:refs/heads/main
git ls-remote origin refs/heads/main    # deve igualar o HEAD local
```

- **Se for recusado** por não ser fast-forward: parar.
- **Reversão:** o estado anterior é `4e88fa9`. Voltar a `main` exigiria
  force-push e uma nova autorização; preferir um commit corretivo.

## Portão 3: CRAN

**Pré-condições:**
- portão 1 com win-builder R-devel **sem ERROR nem WARNING**;
- macOS, de preferência;
- `cran-comments.md` atualizado com os resultados **reais** dos testes
  externos. Hoje ele os marca como não executados.

**Submissão:** `devtools::submit_cran()` ou o formulário web, **só** com
autorização.
- Confirmar pelo e-mail enviado ao mantenedor.
- Commitar o `CRAN-SUBMISSION` gerado.

**Se o CRAN pedir correções:**
- antes de qualquer DOI ou release: corrigir e reenviar;
- depois: lançar a **0.11.1**, sem substituir arquivos publicados.

## Portão 4: release e tag no GitHub

**Antes:** pedir ao mantenedor que confirme, na conta do Zenodo, que a
**integração GitHub–Zenodo** para `tikatuwq/tikatuwq` está **desligada**.
Não presumir. Ligada, ela pode criar um registro automático com um
**conceito novo** (duplicata).

```
git tag -a v0.11.0 <commit-na-main> -m "tikatuwq 0.11.0"
git push origin refs/tags/v0.11.0
gh release create v0.11.0 --repo tikatuwq/tikatuwq --verify-tag --notes-file <NEWS 0.11.0> <tarball final>
```

**Reversão:** antes do Zenodo, `gh release delete v0.11.0 --cleanup-tag`.

## Portão 5: Zenodo

- Usar **New version** no registro existente
  (https://zenodo.org/records/17407297). Isso **preserva** o DOI de conceito
  `10.5281/zenodo.17407296`. **Não criar conceito novo.**
- Enviar o mesmo tarball da release ou do CRAN e conferir o SHA-256.
- Metadados (rascunho no plano anterior) a confirmar:
  - afiliação (IFBA × UFSB);
  - contribuidor;
  - texto sem alegar homologação do IGAM.
- O DOI da 0.11.0 só existe depois de publicar.
- Reserva de DOI: dispensável; se usada, só no depósito manual.

## Portão 6: ORCID

Depois do depósito definitivo:
1. Verificar os trabalhos existentes, **inclusive privados**.
2. Acrescentar o software uma única vez, com o DOI de conceito como
   identidade geral. Opcionalmente, incluir o DOI da 0.11.0 para
   reprodutibilidade.

Nenhuma alteração foi feita.

## Ordem e dependências

| Ordem | Sequência |
|---|---|
| **Recomendada** | Portão 1 → Portão 2 → Portão 3 → (após o aceite) Portão 4 → Portão 5 → Portão 6 |
| Alternativa | Portão 2 → Portão 4 → Portão 5 → Portão 6 → Portão 1 → Portão 3 |

**Risco da alternativa:** um DOI imutável pode ficar divergente do CRAN, e o
caminho passa a ser lançar a 0.11.1.

O ORCID não precisa esperar o CRAN. O Zenodo não precisa ser o último.

## Situação

Todos os portões estão **NÃO EXECUTADOS**.

> Nenhum push, Pull Request, merge, tag, release, deploy pkgdown, upload para serviço externo, depósito ou reserva de DOI, alteração no ORCID ou submissão ao CRAN foi realizado nesta etapa.
