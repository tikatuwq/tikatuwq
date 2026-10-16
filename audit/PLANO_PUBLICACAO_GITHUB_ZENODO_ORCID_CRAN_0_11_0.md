# Plano de publicação: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Natureza:** este documento é **só um plano**. Nenhum passo abaixo foi
  executado. Cada portão exige uma ordem expressa e separada do mantenedor.

## 0. Regras gerais para todos os portões

- **Branch:** publicar **somente** `release/0.11.0-limpa`.
- **Branch proibida:** `fix/iqa-scientific-stabilization` (`ae7f98a`) nunca
  vai para push, PR, tag nem release. O histórico dela contém registros do
  IGAM.
- **Refspec:** sempre explícito. Nunca usar `git push` sem argumentos,
  `--all`, `--mirror` nem `--tags`.
- **Antes de qualquer push:**

  ```
  git -C <LOCAL_WORKDIR>/tikatuwq_limpo fetch origin                     # leitura
  git -C <LOCAL_WORKDIR>/tikatuwq_limpo log --oneline origin/main..release/0.11.0-limpa
  git -C <LOCAL_WORKDIR>/tikatuwq_limpo merge-base --is-ancestor ae7f98a release/0.11.0-limpa && echo PARAR || echo ok
  ```

  - O `log` deve listar **só** os commits da série limpa.
  - O `merge-base` deve imprimir "ok".
  - Se `origin/main` tiver avançado além de `4e88fa9`, **parar e reavaliar**.
    Não reconciliar automaticamente.

## 1. Ordem recomendada e alternativa

| Ordem | Portões | Prós | Contras |
|---|---|---|---|
| **Recomendada** | A (GitHub: branch e PR, **sem merge**) → checks remotos → E (CRAN) → após aceite: merge, B (tag e release), C (Zenodo), D (ORCID), F (site) | A tag, a release e o DOI de versão apontam exatamente para a versão **aceita** pelo CRAN. Se o CRAN pedir mudanças, nada imutável terá sido criado com conteúdo divergente | A publicação no Zenodo espera o CRAN |
| Pedida no mandato | A → B → C → D → E → F | O DOI de versão sai antes da submissão | Se o CRAN pedir correções, o DOI 0.11.0 fica com conteúdo diferente do publicado no CRAN, e o caminho passa a ser lançar uma 0.11.1. Versões do Zenodo não podem ter os arquivos trocados livremente depois de publicadas |

A escolha da ordem é do mantenedor.

## 2. Portões

### A) GitHub: envio da branch e revisão

**Pré-condição:** autorização expressa para o portão A.

```
git -C <LOCAL_WORKDIR>/tikatuwq_limpo push origin release/0.11.0-limpa:refs/heads/release/0.11.0
gh pr create --repo tikatuwq/tikatuwq --base main --head release/0.11.0 \
  --title "tikatuwq 0.11.0" --body-file <texto da PR>
```

- **CI:** abrir a PR dispara o workflow `R-CMD-check.yaml` (Ubuntu, no
  GitHub Actions). É um check remoto e passa a valer com a autorização deste
  portão.
- **Atenção: merge na `main` = publicação do site.** O `pkgdown.yaml` roda em
  todo push na `main` e faz o deploy do GitHub Pages. Para manter os portões
  A e F separados, há duas opções:
  - **(i)** desativar o workflow antes do merge (`gh workflow disable
    pkgdown --repo tikatuwq/tikatuwq`) e reativar no portão F;
  - **(ii)** só fazer o merge quando F também estiver autorizado.
- **Merge:** a `main` (`4e88fa9`) é ancestral da branch, então é possível
  fast-forward. Pela PR, usar "Rebase and merge" ou "Create a merge commit".
  **Não** usar "Squash" se quiser manter o histórico por assunto.
- **Retorno:** se algo errado for enviado, apagar a branch remota
  (`git push origin --delete release/0.11.0`) e avisar o mantenedor.
  Conteúdo já enviado pode ter sido copiado ou guardado em cache por
  terceiros; a remoção não garante o apagamento.

### B) GitHub: tag e release

**Pré-condições:**
- o merge da `main` foi concluído e verificado;
- autorização expressa;
- de preferência, depois do aceite do CRAN.

```
git -C <LOCAL_WORKDIR>/tikatuwq_limpo fetch origin
git -C <LOCAL_WORKDIR>/tikatuwq_limpo tag -a v0.11.0 <commit-na-main> -m "tikatuwq 0.11.0"
git -C <LOCAL_WORKDIR>/tikatuwq_limpo push origin refs/tags/v0.11.0
gh release create v0.11.0 --repo tikatuwq/tikatuwq --verify-tag --title "tikatuwq 0.11.0" \
  --notes-file <trecho 0.11.0 do NEWS.md> <tarball submetido ao CRAN>
```

- **Risco de duplicação no Zenodo:** se a integração GitHub–Zenodo estiver
  **ligada** para `tikatuwq/tikatuwq`, a release cria **automaticamente** um
  registro no Zenodo. Ele pode nascer como registro novo, com **outro
  conceito**, fora de `17407296`. **Antes deste portão:** na conta Zenodo,
  abrir GitHub (Settings → GitHub) e confirmar que o interruptor de
  `tikatuwq/tikatuwq` está **desligado**.
- **Workflows:** nenhum dos atuais reage a tags ou releases.
- **Verificação:** `git ls-remote origin refs/tags/v0.11.0` e o SHA-256 do
  arquivo anexado igual ao do tarball submetido.
- **Retorno:** antes de qualquer arquivamento, apagar a release e a tag
  (`gh release delete v0.11.0 --cleanup-tag`). Depois do Zenodo, não apagar.

### C) Zenodo: nova versão no conceito existente

**Fatos (leitura pública em 2026-10-16):**
- conceito `10.5281/zenodo.17407296`, **uma versão**: 0.7.0, DOI
  `10.5281/zenodo.17407297`, de 2025-10-21;
- a busca não encontrou outros registros;
- o DOI de conceito resolve para `zenodo.org/records/17407297`, a mais
  recente.

**Documentação oficial:**
- cada nova versão é "um registro completamente novo, com metadados,
  arquivos e identificador persistente próprios", ligado às outras versões
  ([manage-versions](https://help.zenodo.org/docs/deposit/manage-versions/));
- dá para reservar um DOI no rascunho de um depósito manual ("Get a DOI
  now!"), e "se você apagar o rascunho, o DOI reservado se perde"
  ([reserve-doi](https://help.zenodo.org/docs/deposit/describe-records/reserve-doi/));
- "não é possível pré-reservar DOIs antes de usar a integração GitHub"
  ([suporte](https://support.zenodo.org/help/en-gb/24-github-integration/73-can-i-pre-reserved-a-doi-before-a-github-release)).

O comportamento do DOI de conceito (resolver para a versão mais recente) foi
confirmado empiricamente e por fontes secundárias (OpenAIRE, The Turing Way).

**Modo do depósito anterior: NÃO CONFIRMADO.**
- **Indício forte de envio manual:** os arquivos são `tikatuwq_0.7.0.tar.gz`,
  `tikatuwq_docs_0.7.0.zip` e `tikatuwq_extras_0.7.0.zip`, sem o zip
  automático da tag e sem `related_identifiers` apontando para o GitHub.
- **Confirmar na conta:** em My uploads, abrir o registro e ver a origem; em
  Settings → GitHub, ver o estado da integração.

| Opção | Continuidade do conceito `17407296` | Risco de duplicar registro | Reserva prévia de DOI | Adequação |
|---|---|---|---|---|
| **(a) Nova versão manual** (botão **New version** no registro `17407297`) | Garantida: a nova versão herda o conceito | Baixo | Possível (rascunho) | **Recomendada**: igual ao modo provável do 0.7.0 |
| (b) Integração GitHub–Zenodo | Não garantida: se o repositório não estiver ligado ao registro existente, a release cria um **conceito novo** | **Alto** | Impossível antes da release | Não recomendada sem confirmar na conta como ela ligaria ao conceito existente |

**Passos da opção (a)** (só após autorização):
1. Abrir https://zenodo.org/records/17407297 logado e clicar **New version**.
2. Enviar o **mesmo** tarball submetido ao CRAN (ou aceito por ele) e
   conferir o checksum. Usar "Import files" só se for reaproveitar arquivos.
3. Preencher os metadados do rascunho abaixo.
4. Publicar. O DOI de versão é atribuído na publicação.
5. **Reserva de DOI:** desnecessária, porque o pacote cita o DOI de conceito.
   Se for usada, um DOI reservado **não** está publicado nem registrado até a
   publicação.
6. **Retorno:** antes de publicar, descartar o rascunho (o DOI reservado se
   perde). Depois de publicado, só editar metadados; correções de arquivo
   exigem nova versão.

**Rascunho local de metadados** (não é um rascunho no Zenodo):

```json
{
  "title": "tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil",
  "version": "0.11.0",
  "upload_type": "software",
  "creators": [
    {"name": "Saraiva Santos, Vinicius",
     "orcid": "0009-0007-1387-7927",
     "affiliation": "Instituto Federal da Bahia"}
  ],
  "description": "R package for freshwater quality assessment in Brazil. Version 0.11.0 corrects CETESB IQA sub-index equations and trophic state indices, adds an independent engine reproducing the IGAM (Minas Gerais) IQA (a documented reproduction, not endorsed by IGAM; it reproduces 49/49 published IGAM values from the Pampulha sub-basin), adds compare_iqa() and iqa_methods(), and removes the classification argument of iqa() (breaking change; use classify_iqa()). See NEWS.md.",
  "keywords": ["water quality", "IQA", "IGAM", "CETESB", "NSFWQI", "IET", "CONAMA 357/2005", "R package", "Brazil"],
  "license": "MIT",
  "related_identifiers": [
    {"relation": "isSupplementTo", "identifier": "https://github.com/tikatuwq/tikatuwq/tree/v0.11.0"},
    {"relation": "isIdenticalTo", "identifier": "https://CRAN.R-project.org/package=tikatuwq"}
  ],
  "files": ["tikatuwq_0.11.0.tar.gz (SHA-256 a registrar no momento do envio)"]
}
```

**Pontos que o mantenedor deve confirmar:**
- **Afiliação:** o registro 0.7.0 usa "Instituto Federal da Bahia"; o README
  cita a UFSB (pós-graduação). Não inventei afiliação.
- **Contribuidor:** o DESCRIPTION lista Fabricio Berton Zanchi como `ctb`,
  com ORCID; não está no registro 0.7.0. Decidir se entra como contribuidor.
- **Financiamento:** nenhum foi comprovado nas fontes, então nenhum foi
  incluído.
- **Relação com o CRAN:** a linha `isIdenticalTo` só vale se o arquivo for
  idêntico ao do CRAN; caso contrário, remover.
- **Arquivos extras:** os zips de documentação, como na 0.7.0, são opcionais.

### D) ORCID

**Fatos:** o registro **público** 0009-0007-1387-7927 tem 2 trabalhos, e
nenhum é o software do Zenodo. Itens privados não aparecem na API pública.

**Fonte oficial:** a página de suporte "Add works using an identifier"
retornou HTTP 403 nesta execução. Os passos abaixo **não foram conferidos na
fonte oficial nesta rodada**.

**Estratégia recomendada:**
- **Obra geral:** um único trabalho com o **DOI de conceito**
  `10.5281/zenodo.17407296` (tipo Software).
- **Versão específica:** opcional; acrescentar o DOI de versão da 0.11.0
  quando ele existir.

**Procedimento manual** (após o portão C e com autorização):
1. Entrar no ORCID → Works → **Add** → **Add DOI** (ou "Search & link" →
   DataCite, porque os DOIs do Zenodo são DataCite).
2. Antes, listar os trabalhos **inclusive privados**, para não duplicar.
3. Se o Zenodo ou o DataCite tiverem permissão de atualização automática,
   conferir se o item já entrou sozinho.

Nenhum cadastro foi alterado.

### E) CRAN

**Pré-condições:**
- autorização expressa;
- win-builder R-devel e R-release sem erros nem avisos (`devtools::check_win_devel()`
  e `devtools::check_win_release()`; **enviam o tarball a um serviço externo**,
  então exigem autorização própria);
- preferencialmente macOS (`devtools::check_mac_release()`, também externo);
- **manual PDF:** instalar LaTeX (por exemplo, `tinytex::install_tinytex()`)
  e rodar `R CMD check --as-cran` **sem** `--no-manual`;
- refazer o check com rede para eliminar a NOTE de hora;
- atualizar `cran-comments.md` com esses resultados.

**Requisitos da checklist oficial**
([submission_checklist](https://cran.r-project.org/web/packages/submission_checklist.html)):
- `R CMD check --as-cran` num R-devel atual;
- a descrição deve ser informativa;
- nomes de software entre aspas simples; funções como `foo()`;
- referências no formato `<doi:...>`;
- `Authors@R` com papéis e ORCID.

O DESCRIPTION atende a esses pontos. Win-builder e R-hub são **suplementares
recomendados**, não exigência formal universal.

**Envio:** pelo formulário web ou `devtools::submit_cran()`, **só** com
autorização.
- O e-mail de confirmação vai para o mantenedor (`vinisaraiva@gmail.com`).
- O `CRAN-SUBMISSION` é regravado automaticamente; commitá-lo depois.

**Mensagem ao CRAN:** usar o `cran-comments.md`. Ele declara a mudança
incompatível, as mudanças de resultado e a ausência de dependências reversas.

**Retorno:** se o CRAN pedir ajustes, corrigir na branch, refazer os checks e
reenviar com a mesma versão (antes do aceite) ou como 0.11.1 (depois).

### F) Site (pkgdown)

**Pré-condição:** autorização expressa.

- **Mecanismo atual:** o workflow `pkgdown.yaml` roda em push na `main` e faz
  o deploy do GitHub Pages (`actions/deploy-pages`).
- **Branch `gh-pages`:** existe (`0800938`). Conferir em Settings → Pages
  qual é a fonte ativa (Actions ou branch) para evitar dois mecanismos.
- **Se o workflow foi desativado no portão A:** reativá-lo
  (`gh workflow enable pkgdown`) e disparar, ou fazer um push na `main`.
- **Verificação prévia (já feita localmente nesta rodada):** `build_site()`
  sem erros e sem links quebrados.
- **Retorno:** reverter pelo histórico do deploy do Pages ou refazer o deploy
  a partir do commit anterior.

## 3. Verificações multiplataforma ainda pendentes

| Plataforma | Comando ou procedimento (futuro) | Requer autorização externa? |
|---|---|---|
| Windows R-devel/release | `devtools::check_win_devel()`, `devtools::check_win_release()` | Sim (win-builder) |
| macOS | `devtools::check_mac_release()` | Sim (mac builder) |
| Linux local | No WSL Ubuntu: `sudo apt install r-base-dev pandoc`, instalar as dependências e rodar `R CMD check --as-cran` num clone limpo. Ou, com o Docker ativo, `docker run --rm -v <clone>:/pkg rocker/r-devel R CMD check --as-cran /pkg/<tarball>` | Não (local; baixa dependências públicas) |
| R-hub v2 | `rhub::rhub_setup()` e `rhub::rhub_check()`. Usa GitHub Actions no repositório (exige commit de workflow e push) | Sim |
| CI do GitHub | Roda sozinho na PR (portão A) | Junto com A |

## 4. Situação

Todos os portões A–F estão **NÃO EXECUTADOS**. Aguardando decisão do
mantenedor.
