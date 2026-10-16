# Prontidão para publicação, fluxo de mantenedor único: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Branch:** `release/0.11.0-limpa`.
- **HEAD de partida:** `0defe7f` (confere com o informado).
- **Base remota:** `origin/main` = `4e88fa9`, conferida por `git ls-remote`
  no início e no fim. `gh-pages` = `0800938`.
- **Commits à frente de `origin/main`:** 16 (17 com o desta rodada).

## 1. Parecer

### Por destino

| Destino | Status | Justificativa |
|---|---|---|
| **GitHub (atualizar a `main`)** | **APTO, sujeito à autorização e a uma decisão** | Histórico sem dados do IGAM; testes e checks ok. **Decisão pendente:** caminhos locais em logs de rodadas anteriores (§4) |
| **Site (pkgdown)** | **APTO, acoplado ao GitHub** | `check_pkgdown` ok e site local sem erros (rodada anterior). Um push na `main` faz o deploy automaticamente |
| **Release / tag** | **APTO tecnicamente** | Depende do commit final na `main`. Nenhuma tag foi criada |
| **Zenodo** | **APTO para preparar** | Rascunho de metadados no plano. Depende de confirmar, na conta, o depósito manual e a integração desligada |
| **ORCID** | **AGUARDA o Zenodo** | O software não está no ORCID público. Conferir itens privados |
| **CRAN** | **AINDA NÃO APTO DE FORMA INCONDICIONAL** | Windows (com manual) e Linux aprovados localmente. Faltam win-builder R-devel e macOS (pendentes de autorização externa). A NOTE de hora deve sumir com rede adequada |

### Geral

**PRONTO COM RESSALVAS** para o portão GitHub/site. **Ainda não pronto de
forma incondicional para o CRAN.**

## 2. Checklist

| Item | Status | Evidência |
|---|---|---|
| Histórico publicável sem registros por amostra do IGAM (16 commits, todos os blobs) | ✅ | Varredura: 0 códigos de estação, 0 assinaturas (só o falso positivo antigo em `spelling.log`); 0 commits contaminados como ancestrais |
| Sem segredos no diff da série | ✅ | 0 padrões encontrados |
| Tarball sem dados do IGAM, sem caminhos pessoais e sem segredos | ✅ | `RELATORIO_TESTES_MULTIPLATAFORMA_0_11_0.md` §3 |
| `R CMD check --as-cran` com manual (Windows) | ✅ 0/0/1 | NOTE de hora, de ambiente |
| `R CMD check --as-cran` (Linux) | ✅ 0/0/1 | idem |
| Manual PDF | ✅ | 58 páginas |
| Testes, CETESB 131/131, IGAM sintético e 49/49 privado | ✅ | Logs desta rodada |
| Instalação limpa e smoke (Windows e Linux) | ✅ | |
| Metadados (§3) | ✅ | |
| win-builder, macOS | ⏳ | Pendente de autorização externa |

## 3. Metadados e ciência

- **Versão e DOI:**
  - `DESCRIPTION` Version 0.11.0; `NAMESPACE` sem alteração (`document()`
    não muda nada); 46 exportações indexadas no pkgdown.
  - DOI de conceito `10.5281/zenodo.17407296` em `inst/CITATION`,
    `CITATION.cff` e READMEs.
  - `10.5281/zenodo.17407297` aparece só como v0.7.0. Nenhum DOI novo foi
    inventado.
- **Turbidez IGAM:** coeficiente **1,5** em `R/iqa_igam.R`, conferido no teste
  sintético. Referências Bucci (2011, p. 51) e Nunes (2008, Quadro 10) na nota
  metodológica.
- **Validação:** apresentada como reprodução de valores publicados (49/49,
  só Pampulha), **não** como homologação.
- **API conferida no smoke test:** `iqa(method = "IGAM")`, `iqa_methods()`,
  `compare_iqa()`, `classify_iqa()`, `citation()`; erros de `nitrate_unit` e
  de migração de `classification`.
- **Testes sintéticos:** não dependem de dados oficiais nem de rede. A
  fixture real foi retirada da branch.
- **Limitações documentadas:**
  - CETESB: pH 9–10 e ΔT;
  - IGAM: só Pampulha; censura, cloreto e arredondamento são políticas do
    pacote;
  - metadados em atributos do R.
- **Licença:** MIT, coerente.
- **`wq_demo`:** documentado como subconjunto real de dados de monitoramento
  do INEMA. Sem indicação objetiva de risco novo; já era distribuído na 0.10.0.
- **`CRAN-SUBMISSION`:** a branch tem um registro antigo (0.9.0), fora do
  tarball. **Não foi alterado.** O `devtools` regrava o arquivo ao submeter.
  `docs/` não foi incorporado.

## 4. Riscos e decisões pendentes

1. **Caminhos locais no histórico publicável (decisão do mantenedor).**
   - 17 arquivos de `audit/` (relatórios e logs de rodadas anteriores, em
     commits desta série) contêm caminhos com o nome de usuário do
     Windows e os nomes das pastas privadas de pesquisa e de backup.
   - **Não há dados neles.** Nenhum desses caminhos está em
     `origin/main`.
   - Mascarar só no HEAD **não** os tira do histórico. A alternativa é
     reconstruir de novo a série local antes do primeiro push (reescrita de
     histórico, **exige nova autorização**).
   - **Recomendação:** risco baixo. Aceitar, ou autorizar uma reconstrução
     leve que mascare os caminhos.
2. **O push na `main` publica o site.** Ver o plano.
3. **Integração GitHub–Zenodo:** confirmar na conta que está desligada antes
   de criar a release.
4. **NOTE de hora:** de ambiente. Repetir o check com outra rede ou com o
   win-builder.
5. **Links de outro pacote ou de tópico interno no PDF** (`resume_wq.Rd`):
   cosméticos e já existentes na 0.10.0. Melhoria futura.
6. **Ferramentas instaladas nesta rodada** (todas locais e reversíveis):
   - TinyTeX no perfil do usuário (`tinytex::uninstall_tinytex()`);
   - ambiente R no WSL em `~/mm` (`rm -rf ~/mm`);
   - `tinytex` 0.61 numa biblioteca temporária do scratchpad.

## 5. Commits locais desta rodada

**Nenhuma correção de código ou de metadados foi necessária.** O único commit
local registra os relatórios e os logs (`audit/`, fora do tarball); o hash
está na mensagem final.

Desde o commit testado `0defe7f`, só arquivos de `audit/` mudaram. Pela
equivalência já provada, o conteúdo do tarball é o mesmo.
