PROMPT PARA CURSOR — “Projeto CRAN-Ready do pacote R tikatuwq”

Contexto: Este repositório contém um pacote R chamado tikatuwq (análise de qualidade da água no contexto brasileiro). Quero torná-lo pronto para CRAN. Você (Cursor) deve:

Ler toda a árvore do projeto.

Executar checagens estáticas e propor patches mínimos necessários.

Garantir que o pacote passa em R CMD check --as-cran sem ERROR/WARNING e com NOTES justificáveis.

Não introduzir dependências pesadas desnecessárias.

Respeitar as políticas CRAN abaixo.

Tarefas (em ordem)
A. Metadados e Esqueleto

DESCRIPTION

Verifique e, se preciso, corrija os campos:

Package: tikatuwq

Title: Water Quality Analysis Tools for the Brazilian Context (inglês, curto, sem ponto final)

Version: (ex.: 0.4.2 para nova submissão)

Authors@R: com autor e cre + e-mail; add ORCID se houver

Description: em inglês; sem jargão; explique IQA, IET, CONAMA 357/2005, relatórios

License: MIT + file LICENSE (e arquivo LICENSE presente)

Encoding: UTF-8

Roxygen: list(markdown = TRUE)

RoxygenNote: atualizado

Depends: R (>= 4.1) (ou o mínimo real)

Imports: apenas o que o código usa

Suggests: knitr, rmarkdown, testthat (>= 3.0.0)

VignetteBuilder: knitr

URL: GitHub + pkgdown

BugReports: issues do GitHub

Final do arquivo precisa de quebra de linha.

NAMESPACE

Regenerar via roxygen2. Exportar só o necessário. Sem S3method/exportPattern soltos.

Arquivos de apoio

Criar/atualizar:

.Rbuildignore (incluir: ^.*\.Rproj$, ^\.Rproj\.user$, ^\.github$, ^README\.Rmd$, ^pkgdown$, ^docs$, ^cran-comments\.md$, ^CODE_OF_CONDUCT\.md$, ^\.lintr$, ^\.devcontainer$)

.gitignore (padrão R: *.Rproj.user, .Rhistory, .RData, .Ruserdata, inst/doc, doc, Meta, etc.)

NEWS.md (changelog conciso)

cran-comments.md (resumo do check local, win-builder e rhub; notas conhecidas)

inst/CITATION (entrada padrão para citação do pacote)

CODE_OF_CONDUCT.md (opcional, recomendado)

README.Rmd → README.md (já temos template)

CI (opcional, recomendado)

Adicionar GitHub Actions:

R-CMD-check.yaml (workflow padrão r-lib/actions)

pkgdown.yaml (deploy do site para GitHub Pages)

B. Código e Políticas CRAN

Objetivo: Garantir que exemplos, testes, vignettes e funções respeitem estas regras:

Sem I/O externo por padrão

Não baixar arquivos nem acessar rede em exemplos/vignettes/testes.

Se inevitável, não faça; use \donttest{} ou simule dados.

Escrita de arquivo: somente em tempdir(). Procure no código por write*, file*, dir* e ajustar.

Exemplos rápidos (< 5 s)

Em @examples, use datasets pequenos embutidos (data/) ou tibble inline.

Se algum exemplo é lento, envolver em \donttest{}.

Sem interatividade em exemplos (nada de readline(), menu(), View()).

Vignettes reprodutíveis

Usar dados pequenos. Sem downloads. Sem chamadas externas.

Setar seed quando houver aleatoriedade.

Testes

testthat com testes determinísticos, sem rede/arquivos externos.

Se algum teste depende de internet, marque com testthat::skip_on_cran()/skip_if_offline().

Internacionalização e mensagens

Mensagens/erros em inglês.

Não alterar options() globalmente; se alterar, restaure no on.exit().

Portabilidade

Sem caminhos absolutos.

Evitar dependência de locale; se relevante, usar Sys.setlocale() com on.exit().

Non-ASCII

Código-fonte e docs em UTF-8.

Evitar caracteres especiais em DESCRIPTION (fora de UTF-8).

Dependências

Remover pacotes não usados.

Usar utils::, stats::, etc., com @importFrom ou chamadas qualificados.

Render de relatórios

render_report() não deve escrever fora de tempdir() por padrão.

Permita output_dir custom, mas padrão = tempdir().

Sem chamadas a rede. Sem caminhos hardcoded.

Datasets

Incluir data/wq_demo.rda (10–30 linhas) + R/data_wq_demo.R com docs (roxygen).

Usar wq_demo nos exemplos/vignette.

Estilo

Consistência de nomes (snake_case).

Linhas < 100–120 colunas quando possível.

C. Documentação roxygen2

Todas as funções exportadas: @title, @description, @param, @return, @examples rápidos, @references se houver.

Atualizar docs: devtools::document().

D. Vignette principal

vignettes/tikatuwq.Rmd com pipeline:

Load dataset wq_demo

iqa(), iet_lamparelli(), conama_check(), conama_summary(), plot_iqa()

render_report() escrevendo em tempdir()

Sem internet, sem arquivos externos

E. README

Confirmar seção Installation com one-liner GitHub; bloco CRAN (comentado “when available”).

Exemplo mínimo executável rápido.

Buscas e refactors automáticos (faça e proponha diffs)

Escrita fora de tempdir()

Procurar por regex: write|save|sink|png|pdf|jpeg|tiff|svg|html|rmarkdown::render

Se write sem tempdir(), refatorar para aceitar output_dir e default tempdir().

Acesso à internet

Procurar: download\\.file|curl|httr|RCurl|url\\(|readLines\\(.*http|utils::browseURL

Em exemplos/tests/vignettes: remover ou \donttest{} / skip_on_cran().

Caminhos absolutos

Procurar: ^/[[:alnum:]]|C:\\\\|\\\\Users\\\\|/home/

Refatorar para relativos ou tempdir().

Mudança global de opções

Procurar: options\\(|par\\(|setwd\\(

Se necessário, encapsular e restaurar com on.exit().

Tempo de execução dos exemplos

Se code chunk pesado, envolver em \donttest{} e usar wq_demo.

Mensagens em inglês

Procurar mensagens em PT-BR e converter para EN nas funções exportadas.

Arquivos modelo (gerar se faltarem)
1) .github/workflows/R-CMD-check.yaml
name: R-CMD-check
on:
  push: { branches: [main, master] }
  pull_request: { branches: [main, master] }
jobs:
  R-CMD-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - uses: r-lib/actions/setup-r-dependencies@v2
      - uses: r-lib/actions/check-r-package@v2

2) .github/workflows/pkgdown.yaml
name: pkgdown
on:
  push: { branches: [main, master] }
jobs:
  pkgdown:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - uses: r-lib/actions/setup-pandoc@v2
      - uses: r-lib/actions/setup-r-dependencies@v2
        with: { extra-packages: any::pkgdown, needs: website }
      - run: pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE)
        shell: Rscript {0}
      - uses: actions/upload-pages-artifact@v3
      - uses: actions/deploy-pages@v4
        if: github.ref == 'refs/heads/main'
        with: { preview: false }
permissions:
  contents: write
  pages: write
  id-token: write

3) cran-comments.md
## Test environments
- local: R x.y.z, macOS/Windows/Linux
- GitHub Actions: ubuntu-latest (R-release)
- win-builder: devel, release
- rhub: Linux/macOS/Windows

## R CMD check
- 0 ERRORs | 0 WARNINGs | <= 1 NOTE(s)

## Notes
- Possibly one NOTE about non-ASCII author name; all files are UTF-8 encoded.
- No examples/vignettes access the internet. All file writes go to tempdir() by default.

4) NEWS.md
# tikatuwq 0.4.2
- CRAN readiness: docs, examples, vignette and checks updated.
- Added demo dataset `wq_demo`.
- `render_report()` now writes to tempdir() by default and avoids network calls.

5) inst/CITATION
citHeader("To cite tikatuwq in publications use:")

citEntry(
  entry    = "Manual",
  title    = "tikatuwq: Water Quality Analysis Tools for the Brazilian Context",
  author   = person("Vinicius", "Saraiva Santos"),
  year     = "2025",
  note     = "R package version 0.4.2",
  url      = "https://github.com/tikatuwq/tikatuwq"
)

6) R/data_wq_demo.R (doc do dataset)
#' Demo water quality dataset
#'
#' A tiny example dataset used in examples and vignette.
#'
#' @format A data frame with 20 rows and 10 variables:
#' \describe{
#'   \item{site}{chr}
#'   \item{date}{Date}
#'   \item{ph}{numeric}
#'   \item{od_mgL}{numeric}
#'   \item{turbidez_NTU}{numeric}
#'   \item{dbo_mgL}{numeric}
#'   \item{coliformes_100ml}{integer}
#'   \item{p_total_mgL}{numeric}
#'   \item{nt_total_mgL}{numeric}
#'   \item{temp_C}{numeric}
#' }
#' @source Simulated for package examples.
"wq_demo"


Nota: Gere wq_demo em data/ como .rda (UTF-8), 20 linhas, valores plausíveis.

Comandos a executar (automatize no Cursor ao final)
# gerar docs
R -q -e "devtools::document()"

# check local estilo CRAN
R -q -e "devtools::check(args = c('--as-cran'))"


Opcional (se conseguir executar remoto):

R -q -e "devtools::check_win_devel(); devtools::check_win_release()"
R -q -e "rhub::check()"

Critérios de aceitação (fail se não cumprir)

R CMD check --as-cran: 0 ERROR | 0 WARNING.

Nenhum exemplo/vignette/teste acessa internet.

Toda escrita de arquivo por funções exportadas usa tempdir() por padrão.

Exemplos executam em < 5 s cada e usam wq_demo ou tibble inline.

DESCRIPTION válido e limpo (sem campos faltando, UTF-8 ok).

NAMESPACE regenerado e consistente.

README.md compila a partir do README.Rmd e os exemplos rodam.

render_report() sem rede, saída padrão em tempdir(), retorno do caminho do arquivo.

Dependências somente as necessárias; chamadas qualificadas (utils::, stats::) ou @importFrom.

Mensagens/erros das funções em inglês.

Entregáveis esperados

Lista de diffs por arquivo (patches aplicados).

Saída resumida do devtools::check('--as-cran').

Itens ainda pendentes (se houver) com sugestão objetiva de correção.

Aplique as mudanças e me mostre os diffs. Se encontrar algo ambíguo, proponha duas opções e marque como “decisão necessária”.