# Validação do candidato a release: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Commit validado:** `21069f5f6013375350f23b1462c8cc7f47f8bf89`, de
  `release/0.11.0-publicavel`, sempre a partir de **clone limpo**.
- **Logs saneados:** `audit/resultados/validacao_candidato_0_11_0/`. Os logs
  integrais ficam só no backup privado.

## 1. Parecer

**APTA PARA AUTORIZAR TESTES EXTERNOS.** Fatos:
- o histórico publicável não tem caminhos pessoais, nomes de pastas
  privadas, dados individuais do IGAM nem segredos;
- o código e a documentação do usuário são idênticos à branch já validada;
- Windows e Linux aprovados localmente;
- o tarball candidato foi verificado.

## 2. Resultados

| Verificação | Ambiente | Resultado | Evidência |
|---|---|---|---|
| `devtools::document()` | Windows, R 4.5.1 | Sem alteração em `man/` ou `NAMESPACE` | `document.log` |
| `devtools::test()` | Windows | 37 arquivos, 181 testes, 806 verificações, **0 falhas**, 0 erros, 2 pulados por desenho; 11 avisos emitidos pelas funções testadas (não são WARNING do check) | `testthat.log` |
| Regressão CETESB | Windows | **131/131**, \|Δ bruto\| = 0, \|Δ exibido\| = 0, classes iguais | `regressao_cetesb.log` |
| Validação histórica IGAM (lida **só** do arquivo privado externo) | Windows | **49/49** na 1ª casa; máx. 0,048876; 6 limítrofes; extrato de 2017 idêntico à planilha. Só agregados | `igam_49_agregado.log` |
| `R CMD build` + `R CMD check --as-cran` **com manual PDF** | Windows 11 x64, R 4.5.1, Rtools 4.5, pandoc 3.8.3, TinyTeX | **Status: OK: 0 ERROR, 0 WARNING, 0 NOTE.** PDF e HTML do manual ok; exemplos, testes (736 passaram, 16 pulados no modo CRAN, 0 falhas) e vinhetas ok; viabilidade de entrada no CRAN e URLs ok; "future file timestamps" ok | `00check.log`, `check.log`, `check_testthat.Rout` |
| `R CMD build` + `R CMD check --as-cran --no-manual` | Ubuntu 26.04 (WSL2), R 4.5.3 (conda-forge), C.UTF-8 | **0 ERROR, 0 WARNING, 1 NOTE** (hora, ver §3). Testes no modo CRAN: 736 passaram, 0 falhas; exemplos e vinhetas ok | `linux_resumo.log`, `linux_00check.log`, `linux_check_testthat.Rout` |
| Instalação limpa e smoke | Windows | ok: CETESB 74,96 "Boa"; IGAM 75,5 "Bom"; erro de `nitrate_unit`; erro de migração de `classification`; `iqa_methods`; `compare_iqa` (3 pares; P4 inelegível por falta de cloreto); `classify_iqa`; nota metodológica; `citation()` com DOI de conceito | `install.log`, `smoke.log` |
| Instalação limpa e smoke | Linux | ok; o IGAM sintético bate com o valor independente (Python) até 1e-10 | `linux_smoke.log` |
| `pkgdown::check_pkgdown()` | Windows | "No problems found" | `pkgdown_check.log` |
| Metadados | — | Version 0.11.0; DOI de conceito `10.5281/zenodo.17407296`; MIT; autoria e ORCIDs do DESCRIPTION; descrição menciona o motor IGAM como reprodução não homologada | Smoke e `citation()` |

## 3. NOTE "unable to verify current time"

- **Windows:** passou (`future file timestamps ... OK`) com a checagem
  habilitada.
- **Linux, alguns minutos depois:** NOTE.
- **Teste de rede** (2026-10-16T19:02Z): worldtimeapi.org e worldclockapi.com
  responderam HTTP 000, e cran.r-project.org HTTP 200 (`servidores_hora.log`).
- **Conclusão:** o serviço de hora é instável. É uma nota de **ambiente**,
  não do pacote. Resultado reportado como 0/0/0 (Windows) e 0/0/1 (Linux),
  sem desligar a checagem.

## 4. Tarball candidato para testes externos

| Item | Valor |
|---|---|
| Arquivo | `tikatuwq_0.11.0.tar.gz` (versão 0.11.0) |
| Commit de origem | `21069f5f6013375350f23b1462c8cc7f47f8bf89` |
| SHA-256 | **`92cfc6c759d83f917774fa1cc0499b450ab14abc4cc25363721e65f18c3844c6`** |
| Tamanho e entradas | 316.798 bytes; 174 entradas (`tarball_inventario.txt`) |
| Local | `<LOCAL_WORKDIR>/tikatuwq_release_candidato_0_11_0/`, fora de qualquer repositório, com `SHA256SUMS.txt` e `ORIGEM.txt` |
| Varredura | 0 códigos de estação; 0 caminhos pessoais ou privados; 0 segredos; 0 marcadores de tabela IGAM. 1 coincidência numérica na vinheta HTML: falso positivo conhecido |
| Equivalência | Mesmos 161 arquivos do tarball validado do `0defe7f`, idênticos exceto a linha `Packaged:` |
| Linux | O tarball reconstruído no Linux tem outro SHA-256 (`e2a7e397…`) por carimbos de build e plataforma. É do mesmo commit |

## 5. Limitações

**Científicas** (sem mudança; documentadas):
- IGAM validado só na Pampulha, sem homologação;
- censura, cloreto e arredondamento são políticas do pacote;
- CETESB: pH 9–10 e ΔT pendentes;
- metadados em atributos do R.

**Documentação:** 3 links que só não funcionam no PDF (`resume_wq.Rd`), já
existentes na 0.10.0. Melhoria futura.

**Testes externos ainda não executados** (pendentes de autorização):
- win-builder;
- macOS;
- R-hub (opcional).

## 6. Ferramentas locais usadas (rodadas anteriores, reaproveitadas)

Todas sem privilégio de administrador e reversíveis:
- TinyTeX no perfil do usuário;
- ambiente micromamba no WSL (`~/mm`).
