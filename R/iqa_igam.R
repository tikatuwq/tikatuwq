# R/iqa_igam.R
# Nucleo do metodo IGAM (Minas Gerais), independente do metodo CETESB.
# (ASCII-only no codigo)
#
# ESTADO: BLOQUEADO para calculo numerico. As equacoes dos qi usadas pelo IGAM
# foram estabelecidas no SCQA (SEMAD/UCEMG/PNMA II, 2005, "Sistema de Calculo
# da Qualidade da Agua - Estabelecimento das equacoes do IQA", Relatorio I),
# por regressao sobre as curvas da NSF e estudos brasileiros. O documento nao
# foi obtido e nenhuma fonte acessivel reproduz as equacoes. Nao se reutilizam
# as equacoes CETESB por pressuposicao (o IGAM usa nitrato e fosfato total, e
# o SCQA tem regressoes proprias).
#
# Implementado (com fonte): especificacao de parametros e pesos, regra de
# dados ausentes, qs de temperatura, classes e validacao de entradas.
# Ver inst/audit/VALIDACAO_IGAM.md.

# Especificacao do metodo IGAM. 'status' de cada item segue as categorias da
# auditoria: a = oficial literal; b = secundaria; d = nao verificado/ausente.
.iqa_igam_spec <- list(
  method = "IGAM",
  version = "Projeto Aguas de Minas / SCQA (SEMAD, 2005)",
  available = FALSE,
  blocked_reason = paste(
    "As equacoes dos qi do IGAM (SCQA, SEMAD/PNMA II, 2005) nao foram obtidas",
    "de fonte primaria; o calculo IGAM nao pode ser executado sem inventar",
    "coeficientes."
  ),
  # Parametros: portal Infohidro/IGAM (acesso 2026-10-15) e relatorio IGAM
  # Pampulha 3o trim. 2017, p. 9 (a)
  parameters = c(
    od = "oxigenio dissolvido (% saturacao)",
    microbiologico = "coliformes termotolerantes ou E. coli (NMP/100 mL)",
    ph = "pH",
    dbo = "DBO5,20 (mg/L)",
    nitrato = "nitrato (unidade NO3 x N-NO3 nao confirmada em fonte primaria)",
    fosfato_total = "fosfato total (mg/L PO4)",
    temperatura = "variacao de temperatura (qs constante = 92)",
    turbidez = "turbidez (UNT)",
    solidos_totais = "solidos totais (mg/L)"
  ),
  # Pesos: Tabela 1 do portal IGAM e imagem; valores citados como "IGAM (2004)"
  # em Geografias (UFMG) 6(2), 2010, Tabela 1 (b)
  weights = c(
    od = 0.17, microbiologico = 0.15, ph = 0.12, dbo = 0.10, nitrato = 0.10,
    fosfato_total = 0.10, temperatura = 0.10, turbidez = 0.08, solidos_totais = 0.08
  ),
  weights_status = "b",
  # Portal IGAM: sem OD e/ou coliformes/E. coli o indicador nao e calculado (a)
  mandatory = c("od", "microbiologico"),
  # Portal IGAM: qs da variacao de temperatura constante e igual a 92 (a)
  temperature_qs = 92,
  # Relatorio IGAM 2017, p. 9 (a): Muito Ruim (0 <= IQA <= 25), Ruim (25 < IQA
  # <= 50), Medio (50 < IQA <= 70), Bom (70 < IQA <= 90), Excelente (90 < IQA <= 100)
  classification = "igam",
  qi_equations = NULL,
  qi_status = "d"
)

#' Pesos efetivos do IGAM com parametros ausentes
#'
#' Regra do portal IGAM: "o peso (do parametro ausente) e repartido igualmente
#' entre os demais parametros". Para mais de um ausente, a soma dos pesos
#' ausentes e repartida igualmente (interpretacao; a fonte so descreve o caso
#' geral). Sem OD ou microbiologico o indice nao e calculado.
#'
#' @param available Vetor logico nomeado (nomes de .iqa_igam_spec$weights).
#' @return Vetor de pesos efetivos (0 para ausentes) ou NULL se um parametro
#'   obrigatorio faltar.
#' @keywords internal
#' @noRd
.igam_effective_weights <- function(available, spec = .iqa_igam_spec) {
  w <- spec$weights
  available <- available[names(w)]
  available[is.na(available)] <- FALSE
  if (!all(available[spec$mandatory])) return(NULL)
  if (all(available)) return(w)
  missing_w <- sum(w[!available])
  out <- ifelse(available, w + missing_w / sum(available), 0)
  names(out) <- names(w)
  out
}

#' Verifica as colunas exigidas pelo metodo IGAM
#'
#' Nao usa substitutos automaticos: nitrogenio total nao substitui nitrato e
#' fosforo total so e aceito com conversao explicita para fosfato (PO4).
#' @return Lista com colunas resolvidas e mensagens de problemas.
#' @keywords internal
#' @noRd
.igam_check_inputs <- function(df) {
  cols <- list(
    od = .resolve_col(df, "od"),
    microbiologico = if (!is.null(.resolve_col(df, "e_coli"))) .resolve_col(df, "e_coli") else .resolve_col(df, "coliformes_termotolerantes"),
    ph = .resolve_col(df, "ph"),
    dbo = .resolve_col(df, "dbo"),
    nitrato = .resolve_col(df, "n_nitrato"),
    fosfato_total = .resolve_col(df, "fosfato_total"),
    turbidez = .resolve_col(df, "turbidez"),
    solidos_totais = .resolve_col(df, "solidos_totais")
  )
  problems <- character(0)
  if (is.null(cols$nitrato)) {
    problems <- c(problems, if (!is.null(.resolve_col(df, "nt_total"))) {
      "IGAM requires nitrate ('nitrato'); total nitrogen ('nt_total') is a different analyte and is not used as a substitute."
    } else "IGAM requires nitrate ('nitrato').")
  }
  if (is.null(cols$fosfato_total)) {
    problems <- c(problems, if (!is.null(.resolve_col(df, "p_total"))) {
      "IGAM uses total phosphate as PO4 ('fosfato_total'); 'p_total' (as P) is not converted implicitly."
    } else if (!is.null(.resolve_col(df, "p_ortofosfato"))) {
      "IGAM uses total phosphate ('fosfato_total'); orthophosphate is a different analyte."
    } else "IGAM requires total phosphate as PO4 ('fosfato_total').")
  }
  if (is.null(cols$solidos_totais) && !is.null(.resolve_col(df, "tds"))) {
    problems <- c(problems, "IGAM requires total solids; TDS ('tds') is not interchangeable.")
  }
  if (is.null(cols$od)) problems <- c(problems, "IGAM requires dissolved oxygen ('od'): the index is not computed without it.")
  if (is.null(cols$microbiologico)) problems <- c(problems, "IGAM requires thermotolerant coliforms or E. coli: the index is not computed without them.")
  list(cols = cols, problems = problems)
}

#' Erro de metodo indisponivel (classe propria, para compare_iqa())
#' @keywords internal
#' @noRd
.iqa_method_unavailable <- function(method, reason, df = NULL) {
  msg <- paste0("method = '", method, "' is not available: ", reason)
  if (!is.null(df) && method == "IGAM") {
    chk <- .igam_check_inputs(df)
    if (length(chk$problems)) msg <- paste0(msg, " Input issues also found: ", paste(chk$problems, collapse = " "))
  }
  msg <- paste0(msg, " See iqa_methods() and inst/audit/VALIDACAO_IGAM.md.")
  structure(class = c("tikatuwq_method_unavailable", "error", "condition"),
            list(message = msg, call = NULL, method = method))
}

#' Metodos de calculo do IQA disponiveis no pacote
#'
#' @description
#' Lista os motores de calculo do IQA, sua situacao de validacao e a escala de
#' classificacao que cada um aplica por padrao. Use para verificar, antes de
#' calcular, se um metodo esta disponivel.
#'
#' @return Data frame com as colunas \code{method}, \code{available},
#'   \code{status}, \code{classification} e \code{reference}.
#'
#' @details
#' \itemize{
#'   \item \code{"CETESB"}: padrao. Equacoes que aproximam as curvas da CETESB
#'     (Apendice D 2023); ver \code{system.file("audit", "VALIDACAO_CETESB.md",
#'     package = "tikatuwq")}.
#'   \item \code{"IGAM"}: \strong{bloqueado}. Pesos, regra de ausentes, qs de
#'     temperatura e classes estao documentados, mas as equacoes dos qi (SCQA,
#'     SEMAD 2005) nao foram obtidas de fonte primaria.
#'   \item \code{"CETESB_legacy_approx"} e \code{"NSF_approx"}: rotas legadas
#'     por curvas interpoladas aproximadas.
#' }
#' A ANA nao tem metodo proprio no pacote: o portal da ANA usa os parametros e
#' pesos da CETESB, mas a equivalencia das curvas nao foi verificada (ver
#' \code{system.file("audit", "ANA_VS_CETESB.md", package = "tikatuwq")}).
#'
#' @examples
#' iqa_methods()
#' @export
iqa_methods <- function() {
  data.frame(
    method = c("CETESB", "IGAM", "CETESB_legacy_approx", "NSF_approx"),
    available = c(TRUE, .iqa_igam_spec$available, TRUE, TRUE),
    status = c("default; equations approximate the CETESB curves (see audit)",
               "blocked: qi equations (SCQA, SEMAD 2005) not obtained",
               "legacy; interpolated approximate curves",
               "legacy; interpolated approximate curves, arithmetic mean"),
    classification = c("cetesb", "igam", "cetesb", "cetesb"),
    reference = c("CETESB (2023) Apendice D; CPRH PNMA II; von Sperling (2007) via IBEAS (2015)",
                  "IGAM (portal Infohidro; relatorio 2017); SEMAD/PNMA II (2005) SCQA",
                  "-", "-"),
    stringsAsFactors = FALSE
  )
}
