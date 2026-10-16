# Plano de lançamento direto (mantenedor único, sem PR): tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Natureza:** **nada abaixo foi executado.** Cada portão exige ordem
  expressa e separada do mantenedor.
- **Relação com o plano anterior:** substitui a parte GitHub de
  `PLANO_PUBLICACAO_GITHUB_ZENODO_ORCID_CRAN_0_11_0.md`, eliminando a PR. As
  seções Zenodo, ORCID e CRAN daquele documento continuam válidas e estão
  resumidas aqui.

## Regras permanentes

- **Fonte:** publicar **apenas** a partir de `release/0.11.0-limpa`.
- **Branch proibida:** `fix/iqa-scientific-stabilization` (`ae7f98a`), nunca
  como fonte de push.
- **Comandos proibidos:** `--force`, `--all`, `--mirror` e `--tags`. Sempre
  usar refspec explícito.
- **Divergência no remoto:** se `origin/main` ≠ `4e88fa9`, **parar**. Não
  fazer merge nem rebase automático.

## Etapa 1: aprovação técnica (mantenedor)

**Base:**
- `RELATORIO_TESTES_MULTIPLATAFORMA_0_11_0.md`
- `RELATORIO_PRONTIDAO_PUBLICACAO_SEM_PR_0_11_0.md`

**Decisões a tomar:**
- (a) aceitar os caminhos locais nos logs antigos, ou autorizar uma
  reconstrução leve para mascará-los (§4 do relatório de prontidão);
- (b) como tratar o deploy automático do site (Etapa 2);
- (c) ordem entre CRAN, release e Zenodo (Etapa 6).

**Sem win-builder e macOS, não declarar aptidão irrestrita ao CRAN.**

## Etapa 2: publicação controlada na `main` (portão GitHub + site)

### Verificações antes do push (só leitura)

```
cd <LOCAL_WORKDIR>/tikatuwq_limpo
git ls-remote origin refs/heads/main          # deve ser 4e88fa9b6aaf2ff53a9a9c4203110fedfbde9d61
git branch --show-current                     # release/0.11.0-limpa
git merge-base --is-ancestor origin/main HEAD && echo "fast-forward possivel"
git merge-base --is-ancestor ae7f98a HEAD && echo PARAR || echo "sem ancestral contaminado"
git log --oneline origin/main..HEAD           # so a serie limpa
```

Antes do push, repetir também a varredura de blobs
(`HEAD ^origin/main ^origin/gh-pages`).

### Atenção ao site

Um push na `main` dispara dois workflows:
- `R-CMD-check.yaml`: CI no GitHub Actions (Ubuntu);
- `pkgdown.yaml`: build e **deploy do GitHub Pages**.

Escolha **uma** das opções (cada uma exige autorização):

| Opção | Como | Consequência |
|---|---|---|
| **A (recomendada)** | Autorizar **GitHub e site no mesmo portão** | O site é atualizado logo após o push. Já foi validado localmente (`check_pkgdown` e `build_site` sem erros) |
| B | Antes do push, desativar o workflow: `gh workflow disable pkgdown --repo tikatuwq/tikatuwq` (configuração remota; exige autorização própria). Reativar depois com `gh workflow enable pkgdown` | O site fica na versão antiga até a reativação |
| C | Commit local que muda o gatilho do `pkgdown.yaml` para `workflow_dispatch` (alteração de workflow; exige autorização) | O deploy passa a ser manual. Muda o comportamento futuro do repositório |

**Nenhum workflow foi alterado preventivamente.**

### Push (fast-forward, sem PR, sem force)

```
git push origin release/0.11.0-limpa:refs/heads/main
git ls-remote origin refs/heads/main          # deve ser igual a git rev-parse HEAD
```

- **Se for recusado (não fast-forward):** parar e reportar. Não usar
  `--force`.
- **Retorno:** a `main` só avançou por fast-forward; o estado anterior é
  `4e88fa9`. Voltar a `main` exigiria force-push, que só pode ser feito com
  nova autorização. Preferir um commit corretivo.

### Opcional

Enviar também a branch para referência
(`git push origin release/0.11.0-limpa:refs/heads/release/0.11.0`). Não é
necessário.

## Etapa 3: release no GitHub (portão próprio)

**Pré-condição:** a `main` publicada e o tarball estabilizado. De preferência
depois do aceite do CRAN (ver Etapa 6).

```
git tag -a v0.11.0 <commit-na-main> -m "tikatuwq 0.11.0"
git push origin refs/tags/v0.11.0
gh release create v0.11.0 --repo tikatuwq/tikatuwq --verify-tag \
  --title "tikatuwq 0.11.0" --notes-file <secao 0.11.0 do NEWS.md> <tarball final>
```

- **Notas da release:**
  - mudança incompatível (`classification` → `classify_iqa()`);
  - motor IGAM (reprodução documentada, **não homologada**; 49/49 só
    Pampulha);
  - `compare_iqa()`;
  - limitações conhecidas.
- **Antes de criar a release:** confirmar na conta Zenodo (Settings → GitHub)
  que a integração com `tikatuwq/tikatuwq` está **desligada**. Ligada, ela
  criaria um registro automático e talvez um **conceito novo** (duplicata).
- **Workflows:** nenhum dos atuais reage a tags ou releases.
- **Retorno:** antes do Zenodo, `gh release delete v0.11.0 --cleanup-tag`.

## Etapa 4: Zenodo (portão próprio)

**Situação atual** (leitura pública):
- conceito `10.5281/zenodo.17407296`, com uma versão: 0.7.0, DOI
  `10.5281/zenodo.17407297`;
- indício forte de depósito manual, **não confirmado**.

**Passos:**
1. **Na conta:** confirmar que o depósito existente foi manual e que a
   integração GitHub está desligada.
2. Abrir https://zenodo.org/records/17407297 → **New version**. Isso preserva
   o DOI de conceito.
3. Enviar o tarball final (o mesmo da release ou do CRAN) e conferir o
   SHA-256.
4. **Metadados:** usar o rascunho em
   `PLANO_PUBLICACAO_GITHUB_ZENODO_ORCID_CRAN_0_11_0.md`. Pontos a
   confirmar:
   - afiliação (IFBA no registro de 2025 × UFSB no README);
   - inclusão do contribuidor do DESCRIPTION;
   - licença MIT;
   - descrição sem alegar homologação do IGAM.
5. **Publicar.** O DOI da 0.11.0 só existe depois da publicação; registrá-lo
   então.
6. **Reserva de DOI:** desnecessária, porque o pacote cita o DOI de conceito.
   Se for feita, é só no rascunho manual; um DOI reservado não está
   publicado.

## Etapa 5: ORCID (portão próprio)

**Depois** de publicada a nova versão no Zenodo:
1. Verificar os trabalhos existentes, **inclusive privados**.
2. Acrescentar o software uma única vez:
   - **DOI de conceito** como identidade geral;
   - DOI da 0.11.0 como referência de reprodutibilidade, se desejado.
3. Caminho: Works → Add → Add DOI, ou Search & link → DataCite.

Não depende do CRAN.

## Etapa 6: CRAN (portão próprio)

**Pré-condições:**
- **win-builder R-devel** (`devtools::check_win_devel()`; envia o tarball,
  então exige autorização) **sem ERROR nem WARNING**;
- de preferência macOS (`devtools::check_mac_release()`);
- `cran-comments.md` atualizado com esses resultados reais.

**Submissão:** `devtools::submit_cran()` ou o formulário web, **só** com
autorização. O mantenedor confirma pelo e-mail recebido. Depois, commitar o
`CRAN-SUBMISSION` regravado.

**Ordem: nenhuma sequência é obrigatória.**

| Variante | Ordem |
|---|---|
| Recomendada (artefato estabilizado) | Etapa 2 (GitHub e site) → checks remotos → Etapa 6 (CRAN) → após o aceite: Etapa 3 (release) → Etapa 4 (Zenodo) → Etapa 5 (ORCID) |
| Alternativa | Etapa 2 → Etapa 3 → Etapa 4 → Etapa 5 → Etapa 6 |

**Risco da alternativa:** se o CRAN pedir correção, o arquivo da 0.11.0 no
Zenodo, que tem DOI imutável, diverge do CRAN. Nesse caso, lançar uma
**0.11.1**; não substituir o arquivo silenciosamente.

O ORCID não precisa esperar o CRAN, e o Zenodo não precisa ser o último.
**Não criar um DOI de conceito novo.**

## Situação dos portões

| Portão | Status |
|---|---|
| GitHub (`main`) + site | NÃO EXECUTADO |
| Release/tag | NÃO EXECUTADO |
| Zenodo | NÃO EXECUTADO |
| ORCID | NÃO EXECUTADO |
| CRAN | NÃO EXECUTADO |
| win-builder, macOS, R-hub | NÃO EXECUTADO (pendente de autorização externa) |

> Nenhum push, Pull Request, merge, tag, release, deploy pkgdown, upload para serviço externo, depósito ou reserva de DOI, alteração no ORCID ou submissão ao CRAN foi realizado nesta etapa.
