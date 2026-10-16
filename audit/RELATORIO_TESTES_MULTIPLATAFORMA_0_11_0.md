# Testes multiplataforma: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Mandato:** `PROMPT_TIKATUWQ_011_TESTES_MULTIPLATAFORMA_SEM_PR.md`.
  Testes locais; **nenhum envio externo**.
- **Commit testado:** `0defe7f928c02e5f511d4bcd187be9d7a9c173e0` (branch
  `release/0.11.0-limpa`), sempre a partir de **clones limpos**, não do
  worktree.
- **Logs:** em `audit/resultados/testes_multiplataforma_0_11_0/`. Os caminhos
  locais foram mascarados (`<TMP>`, `<HOME>`, `<HOME_WSL>`, `<BIBLIOTECA>`).

## 1. Matriz

| Ambiente | SO | R | Comando | Resultado | Evidência | Bloqueio |
|---|---|---|---|---|---|---|
| Windows (local) | Windows 11 x64, build 26200 | 4.5.1 (ucrt), Rtools 4.5 | `R CMD build` (clone limpo) + `R CMD check --as-cran` **com manual** | **EXECUTADO/APROVADO**: 0 ERROR, 0 WARNING, 1 NOTE (hora, de ambiente). Manual PDF ok; manual HTML ok | `00check_com_manual.log`, `check_com_manual.log`, `check_testthat.Rout` (736 passaram, 16 pulados no modo CRAN, 0 falhas) | — |
| Windows: manual PDF isolado | idem | idem | `R CMD Rd2pdf` | **EXECUTADO/APROVADO**: 58 páginas, 283.521 bytes, SHA-256 `e5cbd486cc1fd0a7fe5a4afcb5fa1d8cdf57e3b7a185e94d9aaddb740c1ba1de`. Abre normalmente e contém versão 0.11.0 e `compare_iqa` | `manual_pdf.log`. PDF só local, não distribuído | — |
| Windows: testes | idem | idem | `devtools::test()` | **EXECUTADO/APROVADO**: 37 arquivos, 181 testes, 806 verificações, 0 falhas, 0 erros, 2 pulados (leaflet por desenho; `render_report` por desenho) | `testthat.log` | — |
| Windows: CETESB | idem | idem | `audit/regressao_cetesb_pos_igam.R` | **EXECUTADO/APROVADO**: 131/131, \|Δ\| = 0, classes iguais | `regressao_cetesb.log` | — |
| Windows: IGAM 49/49 (arquivo privado externo) | idem | idem | `audit/auditoria_final_49.R <dir_privado> <2017.xlsx>` | **EXECUTADO/APROVADO**: 49/49 na 1ª casa; máx. 0,048876; 6 limítrofes | `igam_49_agregado.log`, só agregados | — |
| Windows: instalação limpa e smoke | idem | idem | `R CMD INSTALL` em biblioteca temporária + `Rscript --vanilla` | **EXECUTADO/APROVADO** | `install.log`, `smoke.log` | — |
| Windows: pkgdown | idem | idem | `pkgdown::check_pkgdown()` | **EXECUTADO/APROVADO**: "No problems found" | `pkgdown_check.log` | — |
| **Linux (local, WSL)** | Ubuntu 26.04 LTS, kernel 6.18 WSL2, locale C.UTF-8 | **4.5.3** (conda-forge), pandoc 3.12.1 | Clone limpo → `R CMD build` → `R CMD check --as-cran --no-manual` | **EXECUTADO/APROVADO**: 0 ERROR, 0 WARNING, 1 NOTE (hora). Testes no modo CRAN: 736 passaram, 0 falhas; exemplos e vinhetas ok | `linux_resumo.log`, `linux_00check.log`, `linux_check_testthat.Rout` | Sem LaTeX no Linux. O manual foi verificado no Windows |
| Linux: instalação limpa e smoke | idem | idem | `R CMD INSTALL` + `Rscript --vanilla` | **EXECUTADO/APROVADO**: CETESB 74,96 "Boa"; IGAM 75,5 "Bom", igual ao cálculo independente até 1e-10 | `linux_smoke.log` | — |
| macOS | — | — | `devtools::check_mac_release()` (mac builder) | **PENDENTE DE AUTORIZAÇÃO EXTERNA** | — | Não há macOS local; exige envio do tarball |
| win-builder (R-devel e R-release) | — | R-devel | `devtools::check_win_devel()`, `devtools::check_win_release()` | **PENDENTE DE AUTORIZAÇÃO EXTERNA** | — | Exige envio do tarball |
| R-hub v2 | — | vários | `rhub::rhub_setup()` / `rhub_check()` | **PENDENTE DE AUTORIZAÇÃO EXTERNA** | — | Usa GitHub Actions no repositório (exige push de workflow) |

## 2. Detalhes e decisões de ambiente

### Manual PDF

O TinyTeX foi instalado **localmente**, no perfil do usuário
(`%APPDATA%\TinyTeX`), sem privilégio de administrador.

**Etapas da instalação:**
1. O `tinytex` 0.57 da biblioteca do usuário apontava para URLs antigas (404).
   Em vez de atualizá-lo, instalei o `tinytex` 0.61 numa **biblioteca
   temporária** e usei-o para instalar o TinyTeX (build "daily" da release
   oficial `rstudio/tinytex-releases`).
2. A primeira tentativa do manual falhou por falta da fonte Courier
   (`pcrr8t.tfm`) e do `makeindex`. Instalei com `tlmgr install courier
   makeindex`; `times`, `helvetic`, `psnfss` e `inconsolata` já estavam
   presentes. **Nenhum Rd foi alterado.**
3. **Para desfazer:** `tinytex::uninstall_tinytex()`. O instalador do TinyTeX
   pode ter acrescentado o seu diretório `bin` ao PATH do usuário.

**Avisos `pdfTeX warning (dest)`:** 3 destinos de link ausentes no PDF:
`param_analysis` (tópico `\keyword{internal}`, fora do PDF), `dplyr::across()`
e `dplyr::summarise()` (outro pacote), todos em `man/resume_wq.Rd`.
- **Já existiam na 0.10.0 publicada.**
- No HTML os links funcionam, e o check aprovou o PDF.
- **Melhoria futura, não bloqueadora.**

### Linux

- **Por que sem `apt`:** o WSL Ubuntu permite `sudo` sem senha, mas instalar o
  R pelo `apt` seria instalação de sistema com privilégio, que a ordem pede
  para planejar e não executar silenciosamente.
- **Alternativa usada, sem privilégio:** o `micromamba` 2.9.0 (binário
  oficial, SHA-256
  `366cd9cd8be14df1ab8ed50352a82111082a36686b2d389fdb79a92c3fafb3e3`) com
  ambiente conda-forge em `~/mm` (2,5 GB): R 4.5.3, dependências de Imports e
  Suggests, pandoc e qpdf.
- **Para desfazer:** `rm -rf ~/mm ~/tkw_linux_check` no WSL.
- **Primeira execução do check:** falhou em "package dependencies" porque eu
  não tinha incluído o `pkgdown` (Suggests) no ambiente. O erro era do
  ambiente, não do pacote. Corrigi instalando `r-pkgdown` e **refiz o check
  sem desligar `_R_CHECK_FORCE_SUGGESTS_`**.
- **Segunda execução:** a sessão do WSL encerrou (código 255) depois do
  check; o smoke test foi refeito em seguida, com sucesso, sobre o mesmo
  tarball.
- **Docker:** está instalado, mas o daemon estava parado. Não foi iniciado,
  porque não era necessário.

### NOTE "unable to verify current time"

Ocorreu no Windows e no Linux, com o check de horário **habilitado**.

**Teste de rede** (2026-10-16T16:16Z): worldtimeapi.org (http e https) e
worldclockapi.com sem resposta (HTTP 000; antes, 503); cran.r-project.org
HTTP 200 (`servidores_hora.log`).

**Conclusão:** é de ambiente. Os serviços de hora consultados pelo R estão
indisponíveis, e não há defeito no pacote. O resultado é **0/0/1**, não
0/0/0.

### Avisos do testthat × check

- Os **11 avisos** em `devtools::test()` são emitidos pelas funções testadas
  (`clean_units`, `mk_seasonal`) dentro dos testes.
- O `R CMD check` teve **0 WARNING**.
- São contagens diferentes e não devem ser confundidas.

## 3. Artefato candidato

| Item | Valor |
|---|---|
| Tarball (Windows, commit `0defe7f`) | `tikatuwq_0.11.0.tar.gz`, 316.776 bytes, 174 entradas, **SHA-256 `0cc14b3ecf4690250e830e33026854cdf3a78f042f3e6cd409c7c0e20b1bd535`** (inventário em `tarball_inventario.txt`) |
| Tarball (Linux, mesmo commit) | SHA-256 `41f0adacacc3def5f06fc3e918d5f3ea333b648496c44c7ed78d6f9071bcc923`. Difere por carimbos de build e plataforma; o conteúdo vem do mesmo commit |
| Varredura do tarball (refeita nesta rodada, sobre este arquivo) | 0 códigos de estação; 0 caminhos pessoais; 0 marcadores de tabela IGAM. 1 coincidência de assinatura na vinheta HTML: falso positivo conhecido (temperatura do `wq_demo`, base64, números de linha) |

## 4. Resumo

| Item | Status |
|---|---|
| Windows check com manual | **PASSOU** (1 NOTE de ambiente) |
| Linux check | **PASSOU** (1 NOTE de ambiente) |
| Manual PDF | **PASSOU** |
| Testes e regressões | **PASSOU** |
| macOS, win-builder, R-hub | **PENDENTE DE AUTORIZAÇÃO EXTERNA** |
