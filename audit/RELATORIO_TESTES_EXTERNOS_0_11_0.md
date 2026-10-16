# Testes externos: tikatuwq 0.11.0

- **Data:** 2026-10-16
- **Autorização:** expressa do mantenedor, limitada ao envio do pacote para
  testes externos (win-builder R-devel, win-builder R-release, macOS
  R-release).
- **Origem:** `release/0.11.0-publicavel`. O pacote foi validado no commit
  `21069f5`; os envios saíram de um clone limpo do `347ed71`, que só muda o
  script de envio.
- **Logs:** cópias em `audit/resultados/testes_externos_0_11_0/`. O
  saneamento não precisou substituir nada: os logs só têm caminhos dos
  servidores remotos. Os originais estão no backup privado.

## 1. O que foi enviado

| Item | Valor |
|---|---|
| Arquivo | `tikatuwq_0.11.0.tar.gz` (o tarball candidato, sem reconstrução) |
| SHA-256 | `92cfc6c759d83f917774fa1cc0499b450ab14abc4cc25363721e65f18c3844c6`, conferido pelo script **antes de cada envio** (4 envios) |
| Conteúdo | Só o pacote-fonte. Nenhum dado privado do IGAM, arquivo de pesquisa, caminho pessoal ou segredo (varredura da rodada anterior) |
| Script | `audit/scripts/testes_externos_0_11_0.R` |

**Mudança no script:** o envio ao macOS usava
`devtools::check_mac_release()`, que reconstrói o pacote. Passou a enviar o
tarball exato ao mesmo endpoint
(`https://mac.r-project.org/macbuilder/v1/submit`), para que o SHA-256 conferido
seja o do arquivo testado. Commit `347ed71`; nada do pacote mudou.

**Aviso inofensivo:** a mensagem "encerrando conexão não utilizada" vem da
conexão aberta para calcular o SHA-256.

## 2. Resultados

| Plataforma | Ambiente | ERROR | WARNING | NOTE | Link (expira em ~72 h) |
|---|---|---|---|---|---|
| win-builder R-devel | R-devel 2026-10-14 r90673 ucrt; x86_64-w64-mingw32; Windows Server 2022 x64 (build 20348); gcc 14.3.0; UTF-8 | 0 | 0 | **1** | https://win-builder.r-project.org/ecT1o4oOmm0t |
| win-builder R-release | R 4.6.1 (2026-06-24 ucrt); mesmo ambiente | 0 | 0 | **1** | https://win-builder.r-project.org/sNTP0UbxGG5c |
| macOS R-release (2ª tentativa) | R 4.6.1 Patched (2026-07-27 r90311); aarch64-apple-darwin23; macOS Tahoe 26.6; Mac mini M1; clang 17 | 0 | 0 | 0 (**Status: OK**) | https://mac.R-project.org/macbuilder/results/1792187386-689f0694a077f120/ |
| macOS R-release (1ª tentativa) | mesmo builder | 1 | 0 | 0 | https://mac.R-project.org/macbuilder/results/1792179189-afcfe3bbd8088de3/ |

**Nos três checks concluídos:**
- instalação ok;
- exemplos, testes (`testthat.R`), vinhetas e reconstrução das vinhetas ok;
- manual PDF ok;
- manual HTML ok (só no Windows).

**Win-builder:** os logs de R-devel e R-release são idênticos, exceto tempos
e espaçamento. Instalação: 9–10 s; check: 117–121 s.

**macOS:** o mac builder não roda "CRAN incoming feasibility". Essa parte é
coberta pelo win-builder.

## 3. NOTE do win-builder (R-devel e R-release)

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Vinicius Saraiva Santos <vinisaraiva@gmail.com>'

Possibly misspelled words in DESCRIPTION:
  Gerais (25:62)
  Minas (25:56)
```

- **Causa:** o corretor ortográfico do CRAN não reconhece o nome do estado
  "Minas Gerais", na frase "the 'IQA' of the Minas Gerais water agency
  'IGAM'". A 0.10.0 não tinha essa frase.
- **Por que não apareceu localmente:** o corretor usado pelo CRAN (aspell ou
  hunspell) não está instalado nas máquinas locais, então essa etapa não rodou
  nelas.
- **Gravidade:** falso positivo. O nome está correto. É uma NOTE comum, que o
  CRAN aceita com explicação.
- **Correção proposta (recomendada, já aplicada):** explicar no
  `cran-comments.md`. O pacote e o tarball não mudam.
- **Alternativa (não aplicada):** escrever `'Minas Gerais'` entre aspas
  simples no DESCRIPTION, o que tira as palavras do corretor.
  - Exige um novo tarball, um novo SHA-256 e repetir as validações local e
    externa.
  - Só com autorização específica.

## 4. Falha do serviço no macOS (1ª tentativa)

```
* checking package dependencies ... ERROR
Packages required but not available:
  'dplyr', 'readr', 'tibble', 'rlang', 'ggplot2', 'tidyr', 'lubridate',
  'stringr', 'glue', 'scales', 'broom', 'purrr'
```

- **Investigação:**
  - Na 1ª tentativa, a variável `RLIBS` do builder só tinha a biblioteca de
    trabalho, sem a biblioteca CRAN (`CRANlib`). Nenhuma dependência estava
    disponível, nem as básicas (`rlang`, `glue`).
  - A verificação oficial do CRAN marca OK para a 0.10.0, com as mesmas
    dependências, em `r-release-macos-arm64` e `r-release-macos-x86_64`. O
    `dplyr` também está OK nesses ambientes (consulta de 2026-10-16).
- **Conclusão:** falha de infraestrutura do mac builder, não do pacote.
- **Correção:** reenvio único do mesmo tarball, de novo conferido. Na 2ª
  tentativa, `RLIBS` já incluía `CRANlib` e o check terminou com Status OK.
  Nenhuma alteração no pacote.

## 5. Não executado

- **win-builder R-oldrelease:** não foi pedido.
- **macOS R-devel:** o mac builder só oferece R-release pelo endpoint usado.
- **R-hub:** opcional e exige push de um workflow, o que é outro portão.

## 6. Parecer

**APTA PARA O PRÓXIMO PORTÃO DE PUBLICAÇÃO.**
- Nenhuma plataforma externa tem ERROR ou WARNING atribuível ao pacote.
- A única NOTE é um falso positivo de grafia, explicado no
  `cran-comments.md`.
- Somando a validação local, há 5 plataformas verificadas sem ERROR nem
  WARNING:
  - Windows local, 0/0/0 com manual;
  - Linux local, 0/0/1 com a NOTE de ambiente;
  - win-builder R-devel, 0/0/1;
  - win-builder R-release, 0/0/1;
  - macOS R-release, 0/0/0.

**Próximo portão** (`PLANO_PORTOES_PUBLICACAO_0_11_0.md`): o Portão 2 (push
fast-forward na `main`, com a decisão sobre o site) ou o Portão 3 (CRAN). Os
dois dependem de nova autorização expressa.

> Nenhum push, Pull Request, merge, tag, release, deploy pkgdown, depósito ou reserva de DOI, alteração no ORCID ou submissão ao CRAN foi realizado nesta etapa. O único envio externo foi o do tarball candidato ao win-builder e ao mac builder, autorizado.
