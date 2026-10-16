# Auditoria final de pré-lançamento: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Mandato:** `PROMPT_AUDITORIA_FINAL_TIKATUWQ_0_11_0.md`. Autoriza
  auditoria, correções comprovadas e commits locais. **Não** autoriza
  publicação.
- **Local deste relatório:** `audit/`, e não `inst/audit/`, porque ele contém
  o estado do git, caminhos locais e logs da máquina do mantenedor, que não
  devem ser distribuídos. `audit/` está no `.Rbuildignore`.

## 1. Resumo executivo

**Veredito: PRONTA COM RESSALVAS** para a decisão humana de publicação.

### O que foi verificado

Todas as verificações foram **refeitas nesta execução**, não copiadas de
rodadas anteriores:
- a API corresponde à documentação;
- o IGAM reproduz 49/49 IQAs publicados, recalculados a partir das planilhas
  oficiais pela API pública;
- a regressão CETESB das 131 linhas tem diferença zero;
- `compare_iqa()` funciona com os dois métodos;
- o `R CMD check --as-cran` sobre o tarball do commit final dá **Status: OK**
  (0/0/0);
- a instalação a partir do tarball numa biblioteca limpa, com smoke tests,
  funcionou.

### Ressalvas que impedem "PRONTA PARA AUTORIZAÇÃO" plena

1. **Plataformas:** win-builder e macOS não foram executados. O manual PDF não
   foi gerado (sem LaTeX).
2. **Alcance do IGAM:** não é homologado pelo IGAM e foi validado numa única
   sub-bacia.
3. **Arquivos do mantenedor ainda não commitados**, que precisam de decisão
   (§2): `CRAN-SUBMISSION` e `_pkgdown.yml`.
4. **Metadados de método:** são atributos R e se perdem em `merge()` e na
   exportação CSV (§7, M3).

## 2. Estado do git

### Antes

- **HEAD:** `fbdab57`.
- **Base da série 0.11.0:** `dcbfe7e`.
- **Commits locais anteriores:** `7b5a9f0` e `fbdab57`.
- **Modificados e não commitados (do mantenedor):**
  - `CRAN-SUBMISSION` (registro da submissão da 0.10.0);
  - `_pkgdown.yml` (parte já preparada no índice, parte não);
  - `cran-comments.md`;
  - `docs/*` (site gerado);
  - `audit/resultados/{00check,rcmdcheck,testthat}.log`.
- **Não rastreados (do mantenedor):** `atividade.R`,
  `scratch_save_baselines.R`, `scratch_spec.txt`, `user_spec.txt`,
  `user_spec_utf8.txt`, `tests/baseline_*.csv`, `tests/testthat/Rplots.pdf` e
  `docs/EXEMPLOS_TIKATUWQ.html`.

### Commits locais desta rodada (não enviados)

| Hash | Assunto |
|---|---|
| `87abd74` | `fix(iqa)`: erro de migração para a chamada posicional da 0.10.0; testes de borda do IGAM |
| `179819a` | `docs`: NEWS, READMEs, citação (DOI de conceito) e cran-comments |
| `bdfb0b4` | `build`: regras precisas no `.Rbuildignore` |
| (seguinte) | `docs(audit)`: este relatório, script e resultados da auditoria final |

### Preservados sem commit

- **Arquivos não tocados:** `CRAN-SUBMISSION`, `_pkgdown.yml`, `docs/`, os
  três logs antigos e todos os não rastreados.
- **`cran-comments.md`:** foi reescrito, como a ordem pede, e commitado
  (`179819a`).

**Decisões pendentes do mantenedor:**
- Commitar o `_pkgdown.yml` da árvore de trabalho. É essa versão que passa no
  `pkgdown::check_pkgdown()`; a versão do HEAD não lista várias exportações.
- Commitar o `CRAN-SUBMISSION`, que registra corretamente a submissão da
  0.10.0.

## 3. Inventário (seção 1 da ordem)

| Item | Encontrado | Evidência / caminho | Situação | Ação |
|---|---|---|---|---|
| Versão | 0.11.0 | `DESCRIPTION` | Ok | — |
| Exportações | 46 | `getNamespaceExports()`. Inclui `iqa`, `iqa_components`, `iqa_methods`, `compare_iqa` e `classify_iqa` | Ok | — |
| `iqa()`: assinatura × Rd | Iguais | `formals()` = argumentos do Rd: `df, method, altitude_m, …, pesos, nitrate_unit, ...` | Ok | — |
| Remoção de `classification` | Erro de migração | `iqa(d, classification=)` e `scheme=` → mensagem com `classify_iqa()` | Ok | — |
| Chamada posicional da 0.10.0 `iqa(df, "CETESB", "legacy")` | Mensagem genérica de `altitude_m` | `R/iqa.R` | **Corrigido** | `87abd74`: mensagem de migração + teste + NEWS + `?iqa` |
| `classify_iqa(classification=)` | Inalterada; `scheme` depreciado com aviso | Smoke test | Ok | — |
| DOI em `inst/CITATION` e nos READMEs | `10.5281/zenodo.17407297`, citado como "v0.10.0" | API do Zenodo: 17407297 é o DOI da **v0.7.0** (2025-10-21); o conceito é 17407296 | **Corrigido** | `179819a`: DOI de conceito, com explicação |
| `CITATION.cff` | `version: 0.11.0`, `date-released: 2025-09-04` | Data incoerente para uma versão não lançada | **Corrigido** | Sem `date-released`; DOI de conceito |
| `cran-comments.md` | Dizia que o IGAM estava indisponível; contagens antigas | — | **Reescrito** | `179819a` |
| `_pkgdown.yml` | HEAD sem `iqa_methods` e `compare_iqa`; árvore de trabalho completa | `pkgdown::check_pkgdown()`: "No problems found" | Pendente (mantenedor) | Commitar a versão da árvore |
| Artefatos em `inst/` distribuídos desde a 0.9.0 | `NAMESPACE.bak`, `CRAN-SUBMISSION` (0.8.2), `run_check_cran.R`, `ANALISE_TECNICA_TIKATUWQ.md` (0.8.2) | `git log` → `2fef67f` | **Excluídos do build** (não apagados) | `bdfb0b4` |
| Arquivos soltos na raiz e em `tests/` | `atividade.R`, `scratch_*`, `user_spec*`, `tests/baseline_*`, `Rplots.pdf` | NOTE do check anterior | **Excluídos do build** | `bdfb0b4` |
| Material de auditoria distribuído | `inst/audit/` (312 KB) e `inst/methodology/` | Referenciado por `system.file()` na documentação | Apropriado | — |
| Dados do IGAM distribuídos | `inst/audit/IGAM_VALIDACAO_REFERENCIAS.csv` e `tests/testthat/fixtures/igam_referencia_pampulha.csv` | O repositório do IGAM não declara licença (campo de direitos ausente nos itens 3077, 3049 e 2927) | Ressalva | §7, M5 |

## 4. Matriz IGAM (seção 2 da ordem)

| Parâmetro | Equação / regra | Fonte | Teste | Status | Ressalva |
|---|---|---|---|---|---|
| Independência do CETESB | `.iqa_igam_compute()` e `.iqa_igam_route()` não chamam `.qi_cetesb_*` nem `iqa_components()` | Inspeção do corpo das funções nesta execução | `test-iqa-igam-api.R` ("CETESB inalterado") | ✅ | — |
| Turbidez | 90,37e^(−0,0169Tu) − 1,5·cos(0,0571(Tu−30)) + 10,22e^(−0,231Tu) − 0,8, cosseno em radianos (Tu ≤ 100); 5 (Tu > 100) | SCQA p. 10 (imprime 15); Bucci 2011, p. 51; Nunes 2008, Quadro 10 (1,5; conferidos visualmente) | Tu = 0 → 100,003; Tu = 100 → 16,859 (trecho ≤); 100 + 1e-9 → 5; −1 e Inf → `NA` com aviso; `NA` e `NaN` → `NA`. Todos tratados como ausentes, com peso redistribuído e motivo na linha | ✅ | O coeficiente 1,5 não tem confirmação formal do IGAM |
| Nitrato | Curva em mg/L NO₃. `nitrate_unit = "N"` converte × 62,004/14,007 | SCQA, Tabela 2.1; portal IGAM, Tabela 1 | Erro sem `nitrate_unit`; `nt_total` não substitui | ✅ | Trechos > 10 mg/L NO₃ não exercitados por dados publicados |
| Fósforo | `fosfato_total` (PO₄) direto; `p_total` exige `phosphorus_basis` ("P" × 94,9714/30,9738) | SCQA p. 9 | Erros de ambiguidade e de contradição; ortofosfato não substitui | ✅ | — |
| Temperatura | qs = 92 constante; opções de ΔT do CETESB → erro | Portal IGAM (Projeto Águas de Minas) | Testado | ✅ | O SCQA dá 93 em ΔT = 0 (outro contexto) |
| OD | Cs do SCQA (T, cloreto, altitude); y₃ com multiplicação | SCQA p. 3–4; curva do portal | Cloreto ou temperatura ausente → `NA` com motivo; OD `NaN`, `Inf` ou −1 → `NA` | ✅ | **Política do pacote:** sem cloreto padrão |
| Microbiológico | *E. coli* ou termotolerantes, sem fator; com as duas colunas, exige `microbial_type`; < 1 → `NA` | SCQA p. 5; relatório IGAM 2017 | Testado | ✅ | **Política do pacote:** < 1 e censura |
| Ausências | Sem OD ou microbiológico → sem IQA; demais → peso repartido igualmente | Portal IGAM | Testado | ✅ | A repartição com mais de um ausente é interpretação do texto |
| Opções do CETESB | `allow_partial`, `pesos`, `temperature_*` e `censor_policy = "preserve"` → erro inteligível | — | Testado | ✅ | — |
| Censura | `censor_policy` registrado; valores censurados por linha em `attr(, "iqa_igam_censored")` | **Política do pacote** | Testado | ✅ | O IGAM não documenta a regra |
| Arredondamento e classe | IQA com 1 casa; classe IGAM sobre o valor arredondado | Política do pacote (como o IGAM publica) | 49/49 | ✅ | A regra de desempate do IGAM não foi verificada (menor distância a x,x5 = 0,0011) |
| Metadados | `attr`: método, versão, referência, nota, configuração e censura | Mantidos em `[`, `mutate`, `filter`, `rbind`, `bind_rows`, `left_join` e `as_tibble`; **perdidos** em `merge()` e na exportação CSV | — | ⚠️ | §7, M3 |

**Hipóteses operacionais sem respaldo institucional:**
- cloreto obrigatório, sem valor padrão;
- censura no limite;
- microbiológico < 1 → `NA`;
- valores inválidos tratados como ausentes;
- classe sobre o valor arredondado;
- repartição do peso com vários ausentes.

## 5. Revalidação numérica (seção 3 da ordem)

- **Insumos:** os SHA-1 das cinco fontes originais (planilhas de 2017, 2018T3
  e 2018T4; relatórios 2927 e 2365) conferem com `audit/dados/FONTES.md`.
- **Extratos:**
  - a nova extração do conjunto externo a partir das planilhas é idêntica ao
    CSV versionado (`git diff` vazio);
  - o extrato dos 15 casos de 2017 é idêntico à planilha nos 10 parâmetros e
    15 linhas (`audit/auditoria_final_49.R`).
- **Cálculo:** feito pela **API pública** `iqa(method = "IGAM", nitrate_unit =
  "N", phosphorus_basis = "P")`.
- **Tabela por amostra:** `audit/resultados/auditoria_final_49_casos.csv`. Traz
  entradas, publicado, bruto (`%.17g`), arredondado, erro com sinal e absoluto,
  elegibilidade, motivo e distância ao desempate.

| Grupo | n | Iguais na 1ª casa | ≤ 0,05 | MAE | RMSE | Viés | Máx. | Limítrofes (> 0,045) | Testam k |
|---|---|---|---|---|---|---|---|---|---|
| Investigação (3º trim. 2017) | 15 | 15 | 15 | 0,02324 | 0,02765 | +0,00120 | 0,04475 | 0 | 13 |
| Externa pré-registrada | 34 | 34 | 34 | 0,02620 | 0,02976 | −0,00113 | 0,04888 | 6 | 32 |
| — dos quais 3º trim. 2018 | 17 | 17 | 17 | 0,02871 | 0,03222 | −0,00453 | 0,04888 | 4 | 15 |
| — dos quais 4º trim. 2017 | 17 | 17 | 17 | 0,02370 | 0,02708 | +0,00226 | 0,04622 | 2 | 17 |
| **Total** | **49** | **49** | **49** | 0,02529 | 0,02913 | −0,00042 | **0,04888** | 6 | 45 |

**Alegações conferidas:**

| Alegação | Resultado |
|---|---|
| 49/49 iguais na 1ª casa | Confirmada |
| Erro máximo de 0,0489, nos externos | Confirmada (0,048876) |
| Seis casos limítrofes | Confirmada, todos nos externos |

**Casos limítrofes:** seis casos, todos nos externos (4 do 3º trim. 2018 e 2
do 4º trim. 2017), com erros absolutos entre 0,0457 e 0,0489. *A tabela por
amostra foi retirada do histórico publicável em 2026-10-16 e está no arquivo
de pesquisa privado; ver `audit/DADOS_IGAM_PRIVADOS.md`.*

Nesses seis casos, uma diferença de cerca de 0,001 a 0,004 no cálculo do IGAM
mudaria o valor arredondado. Por isso a concordância é **sensível a detalhes
não documentados** (por exemplo, a precisão interna e a ordem de
arredondamento do IGAM). A tolerância não foi alterada.

**Natureza dos 49 casos:**
- os 15 da investigação participaram da escolha da hipótese;
- os 34 posteriores foram pré-registrados;
- **nenhum dos 49 é validação geográfica**: são três campanhas da mesma
  sub-bacia (Pampulha).

**Regressão CETESB:**
- 131 linhas: máx. |Δ bruto| = 0, máx. |Δ exibido| = 0, 0 classes
  diferentes (`audit/regressao_cetesb_pos_igam.R`);
- as três linhas de fronteira do fósforo usam a entrada exata, documentada no
  próprio CSV.

**Ambiente:**
- R 4.5.1 (ucrt), Windows 11 x64, build 26200;
- tikatuwq 0.11.0 (`bdfb0b4`);
- pacotes: devtools 2.4.5, testthat 3.2.3, roxygen2 7.3.3, rcmdcheck 1.4.0,
  rmarkdown 2.30, knitr 1.51, readxl 1.4.5, dplyr 1.2.1, pkgdown 2.1.3;
- pandoc 3.8.3 (do RStudio).

## 6. API e exemplos executados (seção 4 da ordem)

Executados a partir do **tarball instalado** numa biblioteca temporária limpa
(`Rscript --vanilla`). Log: `audit/resultados/pre_release_0_11_0/smoke.log` (mantido só no arquivo de pesquisa privado, porque contém o resultado de uma amostra real).

```r
d <- data.frame(od = 6.5, temperatura = 25, dbo = 2, coliformes = 200, ph = 7.2,
                nt_total = 1.2, p_total = 0.05, turbidez = 8, solidos_totais = 120)
iqa(d)                                     # 74.96, "Boa", metodo CETESB
iqa(g, method = "IGAM", altitude_m = ..., nitrate_unit = "N", phosphorus_basis = "P")
#   amostra real do IGAM: entradas e resultado retirados do historico publicavel
#   em 2026-10-16 (arquivo de pesquisa privado). O exemplo distribuido hoje e
#   sintetico (README e ?iqa: IQA 75.5, "Bom").
iqa_methods()                              # IGAM: implemented/documented/reference_validated/available = TRUE
compare_iqa(cd, methods = c("CETESB", "IGAM"), id_cols = "ponto",
            phosphorus_basis = "P", nitrate_unit = "N", altitude_m = 800)
#   4 linhas preservadas e na mesma ordem; P4 inelegivel para o IGAM
#   ("cloreto ausente (saturacao de OD)"); 3 pares; media IGAM - CETESB = -0.5587;
#   RMSE = 0.6061; transicoes Otima->Bom, Boa->Medio, Ruim->Ruim
```

- **Bordas de `compare_iqa()` cobertas por testes:**
  - IDs duplicados rejeitados;
  - zero, uma e duas linhas, com correlação `NA` abaixo de 3 pares;
  - ordem invertida dos métodos;
  - método indisponível (simulado);
  - diferenças só em pares válidos;
  - `df` não modificado.
- **`iqa_methods()`:** a semântica de `reference_validated` está documentada
  em `?iqa_methods` (reprodução de valores publicados no escopo de
  `limitation`; **não** é homologação). `NSF_approx` e `CETESB_legacy_approx`
  continuam com `reference_validated = FALSE` e `limitation` de "approximate
  curves".

## 7. Checks (seção 7 da ordem)

Logs integrais em `audit/resultados/pre_release_0_11_0/`.

| Etapa | Resultado | Log |
|---|---|---|
| `devtools::document()` | Só o `man/iqa.Rd` mudou (nota de migração); NAMESPACE inalterado | `document.log` |
| `devtools::test()` | 37 arquivos, 182 testes, 801 expectativas, **0 falhas, 0 erros**, 5 pulados (detalhes abaixo) | `testthat.log` |
| `R CMD build` (commit `bdfb0b4`) | `tikatuwq_0.11.0.tar.gz`, SHA-256 `08275375e51500924d241ce764f5d649e230043e1046e98f249afd36d859a2bb` | `build.log` |
| Conteúdo do tarball | Sem `.claude`, `scratch*`, `user_spec*`, `atividade.R`, baselines, `Rplots.pdf`, `NAMESPACE.bak`, `CRAN-SUBMISSION`, `run_check_cran.R`, `docs/`, `audit/`, logs, `.Rhistory`, `.RData`, `.Rproj` ou credenciais | Inspeção com `tar -tzf` do tarball final (189 entradas) |
| `R CMD check --as-cran --no-manual` sobre o tarball | **Status: OK** (0 erros, 0 avisos, 0 notas), incluindo a viabilidade de entrada no CRAN (URLs) e a reconstrução das vinhetas | `00check.log`, `check.log` |
| Testes dentro do check (modo CRAN) | FAIL 0, WARN 11, SKIP 17 (15 por `skip_on_cran`, 1 teste vazio, 1 fallback do leaflet), PASS 733. Nenhum teste de IQA ou IGAM é pulado | `check_testthat.Rout` |
| Instalação do tarball em biblioteca limpa | DONE | `install.log` |
| Smoke tests | CETESB, IGAM, `iqa_methods`, `compare_iqa`, `classify_iqa`, nota metodológica instalada, `citation()` | `smoke.log` |
| `pkgdown::check_pkgdown()` (árvore de trabalho) | "No problems found". O site não foi gerado nem publicado | — |
| `git diff --check` | Só espaços finais intencionais do Markdown (quebra de linha), já existentes | — |
| win-builder e macOS | **Não executados** | — |
| Manual PDF | **Não gerado** (sem LaTeX) | — |

**Testes pulados em `devtools::test()`:**
- 3 de `render_report`: exigem pandoc no PATH. Com pandoc, 2 rodam e passam e
  1 é pulado por desenho.
- 1 de fallback do leaflet: por desenho.
- 1 de `mk_seasonal`: teste vazio (§8, M4).

**Avisos (11):** vêm de `test-clean_units.R` e `test-mk_seasonal.R`. Esses
arquivos são anteriores a esta série e não foram alterados. Os avisos são
emitidos pelas próprias funções testadas; esta auditoria não investigou se
cada um é esperado.

**Ambiente:** o pandoc não está no PATH desta máquina. Foi usado o do RStudio
via `PATH`/`RSTUDIO_PANDOC`. Sem ele, o check emite uma NOTE ("README.md or
NEWS.md cannot be checked without pandoc").

## 8. Problemas

### Corrigidos nesta rodada

| # | Severidade | Problema | Correção |
|---|---|---|---|
| C1 | Média | A chamada posicional da 0.10.0 dava um erro genérico | Mensagem de migração específica + teste (`87abd74`) |
| C2 | Média | O DOI citado (17407297) é o DOI da v0.7.0, mas aparecia como v0.10.0 em `inst/CITATION` e nos READMEs | DOI de conceito 17407296, com explicação (`179819a`) |
| C3 | Baixa | `CITATION.cff` com `date-released` de 2025 para a 0.11.0 | Campo removido (`179819a`) |
| C4 | Baixa | `cran-comments.md` desatualizado (dizia que o IGAM estava indisponível) | Reescrito com os resultados desta rodada (`179819a`) |
| C5 | Baixa | Artefatos antigos em `inst/` distribuídos desde a 0.9.0; arquivos soltos (NOTE) | `.Rbuildignore` preciso (`bdfb0b4`) |
| C6 | Baixa | NEWS sem as pendências conhecidas (pH 9–10, ΔT, limites do IGAM) e sem destaque da mudança incompatível no topo | Seção "Known open items" e resumo no topo (`179819a`) |

### Ainda abertos

| # | Severidade | Problema | Proposta mínima |
|---|---|---|---|
| M1 | Média (lançamento) | win-builder, macOS e manual PDF não executados | Rodar `devtools::check_win_devel()`, `check_win_release()` e `check_mac_release()` antes da submissão |
| M2 | Média (científica) | IGAM sem confirmação institucional do coeficiente 1,5; validado só na Pampulha; nitrato quase todo censurado | Já documentado. Consultar o IGAM e validar em outras bacias |
| M3 | Baixa–média | Os metadados de método (atributos) se perdem em `merge()` e na exportação CSV; o método não aparece em nenhuma coluna | Decisão do mantenedor: documentar em `?iqa` que se deve guardar `attr(, "iqa_calculation_method")` e `attr(, "components")` antes de exportar, ou acrescentar uma coluna opcional de método. Não foi feito agora, para não mudar a saída no candidato a release |
| M4 | Baixa (fora do IQA) | O teste `mk_seasonal()` "tendencia crescente" é vazio: a asserção fica dentro de um `if` que a série não aciona | Ajustar a série ou tornar a asserção incondicional |
| M5 | Baixa | Dados do IGAM (49 linhas) distribuídos sem licença explícita localizada | São dados públicos de órgão estadual, citados com a fonte. Confirmar com o IGAM, se o mantenedor julgar necessário |
| M6 | Baixa (decisão) | `_pkgdown.yml` e `CRAN-SUBMISSION` não commitados | Commitar a versão da árvore de trabalho (o check do pkgdown passa nela) |
| M7 | Baixa (científica, CETESB) | pH 9–10: duas transcrições divergem (efeito ≤ 0,9 ponto, sem mudança de classe nos testes). A curva de ΔT dos modos experimentais não tem fonte (o padrão qi = 94 não é afetado) | Já documentado na NEWS, em `?iqa` e em `inst/audit`. **Risco para o lançamento: baixo** |

## 9. Documentação e citações

- **Atualizadas:**
  - `README.md` e `README-pt.md`: exemplos CETESB, IGAM e `compare_iqa()` com
    argumentos reais; resultados e limitações; citação;
  - `NEWS.md`;
  - `?iqa`;
  - `inst/CITATION` e `CITATION.cff`;
  - `cran-comments.md`.
- **Nota metodológica:** `inst/methodology/IGAM_VALIDATION.md` (verificada, sem
  alteração nesta rodada).
- **URLs:** a checagem de URLs do `--as-cran` passou nesta execução. O DOI de
  conceito resolve (HTTP 302 → registro do Zenodo).
- **Mudança incompatível:** está descrita no topo e na seção de motores da
  NEWS, em `?iqa`, nos READMEs e em `cran-comments.md`. A migração da chamada
  posicional também está coberta.

## 10. Recomendação (não executada)

- **GitHub (push da branch e PR para `main`):** possível depois de o
  mantenedor decidir M6 e revisar os 6 commits da série 0.11.0 (`7b5a9f0` até
  o desta auditoria).
- **CRAN:** só depois de M1 (win-builder, macOS e manual PDF). Rever
  `cran-comments.md` com esses resultados.
- **Zenodo:** depositar a 0.11.0 só após o lançamento. Então atualizar
  `inst/CITATION`, `CITATION.cff` e os READMEs, se o mantenedor preferir citar
  o DOI de versão.
- **Nenhuma** publicação, tag, release, depósito ou atualização de site foi
  feita.
