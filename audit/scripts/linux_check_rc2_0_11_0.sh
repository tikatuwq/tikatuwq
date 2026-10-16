#!/usr/bin/env bash
# Check Linux do RC2 EXATO (sem reconstruir). WSL Ubuntu, R do conda-forge em ~/mm, sem sudo.
# Uso: bash linux_rc2.sh <tarball_windows_montado> <sha256> <saida_logs>
set -u
TB="$1"; ESP="$2"; OUT="$3"
export PATH="$HOME/mm/envs/r45/bin:$PATH"
export LANG=C.UTF-8 LC_ALL=C.UTF-8
W="$HOME/tkw_linux_rc2"
rm -rf "$W"; mkdir -p "$W"; cd "$W"
{
  echo "== ambiente"; . /etc/os-release; echo "$PRETTY_NAME | $(uname -sr)"; R --version | head -1
  echo "pandoc: $(pandoc --version 2>/dev/null | head -1)"; echo "locale: $LANG"
  echo "pdflatex: $(command -v pdflatex || echo ausente)"
  cp "$TB" tikatuwq_0.11.0.tar.gz
  SHA=$(sha256sum tikatuwq_0.11.0.tar.gz | cut -d' ' -f1)
  echo "sha256: $SHA"
  if [ "$SHA" != "$ESP" ]; then echo "SHA DIVERGENTE: PARAR"; exit 1; fi
  echo "SHA confere com o RC2"
  if command -v pdflatex >/dev/null; then MAN=""; else MAN="--no-manual"; fi
  echo "== R CMD check --as-cran $MAN (sobre o tarball RC2, sem rebuild)"
  R CMD check --as-cran $MAN tikatuwq_0.11.0.tar.gz > check.log 2>&1
  grep -E "Status|NOTE|WARNING|ERROR|incoming|timestamps|examples \.\.\.|tests \.\.\.|vignettes \.\.\.|re-building" tikatuwq.Rcheck/00check.log
  grep -E "\[ FAIL" tikatuwq.Rcheck/tests/testthat.Rout | tail -1
  cp tikatuwq.Rcheck/00check.log "$OUT/linux_00check.log"
  cp tikatuwq.Rcheck/tests/testthat.Rout "$OUT/linux_check_testthat.Rout"
  echo "== teste de rede dos servidores de hora (para classificar eventual NOTE)"
  for u in http://worldtimeapi.org/api/timezone/etc/UTC http://worldclockapi.com/api/json/utc/now https://cran.r-project.org; do
    echo "$u -> HTTP $(curl -s -o /dev/null -m 15 -w '%{http_code}' "$u") em $(date -u +%FT%TZ)"
  done
  echo "== instalacao em biblioteca limpa e smoke"
  mkdir -p lib; R CMD INSTALL --library=lib tikatuwq_0.11.0.tar.gz > install.log 2>&1 && echo "install: DONE" || { echo "install: FALHOU"; tail -20 install.log; }
  Rscript --vanilla -e '
    library(tikatuwq, lib.loc = "lib")
    cat("tikatuwq", as.character(packageVersion("tikatuwq", lib.loc = "lib")), "\n")
    d <- data.frame(od = 6.5, temperatura = 25, dbo = 2, coliformes = 200, ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8, solidos_totais = 120)
    o <- iqa(d); cat("CETESB:", o$IQA, as.character(o$IQA_status), "\n")
    g <- data.frame(od = 7, temperatura = 25, cloreto = 10, e_coli = 100, ph = 7.3, dbo = 2, nitrato = 1, p_total = 0.1, turbidez = 5, solidos_totais = 100)
    og <- iqa(g, method = "IGAM", altitude_m = 0, nitrate_unit = "N", phosphorus_basis = "P", details = TRUE)
    cat("IGAM:", og$IQA, as.character(og$IQA_status), sprintf("bruto %.12f", attr(og, "components")$IQA_unrounded), "\n")
    cat("igual ao valor independente (Python):", isTRUE(all.equal(attr(og, "components")$IQA_unrounded, 75.479124819573599, tolerance = 1e-10)), "\n")
    cat("erro sem nitrate_unit:", tryCatch({iqa(g, method = "IGAM", phosphorus_basis = "P"); "nao"}, error = function(e) "sim"), "\n")
    print(iqa_methods()[, c("method", "available", "reference_validated")], row.names = FALSE)
    m <- data.frame(ponto = c("P1", "P2", "P3", "P4"), od = c(7, 5, 3, 6), temperatura = 24, cloreto = c(10, 30, 60, NA),
      coliformes = c(100, 2000, 20000, 500), ph = c(7.2, 7.5, 7, 7.4), dbo = c(2, 5, 15, 3), nt_total = c(1, 3, 8, 2),
      nitrato = c(0.5, 1, 2, 0.8), p_total = c(0.05, 0.2, 0.8, 0.1), turbidez = c(5, 20, 60, 10), solidos_totais = c(100, 200, 400, 150))
    cmp <- compare_iqa(m, methods = c("CETESB", "IGAM"), id_cols = "ponto", phosphorus_basis = "P", nitrate_unit = "N", altitude_m = 800)
    print(cmp[, c("ponto", "IQA_CETESB", "IQA_IGAM", "par_valido")], row.names = FALSE)
    cat("pares validos:", sum(cmp$par_valido), "\n")
    print(classify_iqa(c(20, 50.1, 95), "igam"))
    print(classify_iqa(c(20, 50.1, 95), "cetesb"))
    print(citation("tikatuwq"), style = "text")
  ' 2>&1
} 2>&1 | tee "$OUT/linux_resumo.log"
