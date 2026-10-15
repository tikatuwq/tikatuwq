# Validação do método IGAM — tikatuwq 0.11.0

- **Data:** 2026-10-15
- **Estado:** **BLOQUEADO para cálculo numérico.** `iqa(df, method = "IGAM")` e
  `iqa_components(df, method = "IGAM")` geram erro de classe
  `tikatuwq_method_unavailable`. `iqa_methods()` mostra o status.
- **Código:** `R/iqa_igam.R` (independente de `R/iqa_equations.R`; não reaproveita
  nenhum coeficiente CETESB).
- **Testes:** `tests/testthat/test-iqa-igam-compare.R`.

## 1. Motivo do bloqueio

As equações dos qi do IGAM foram estabelecidas no documento:

> SEMAD; UCEMG / PNMA II (2005). *Sistema de Cálculo da Qualidade da Água
> (SCQA) — Estabelecimento das equações do Índice de Qualidade das Águas
> (IQA)*. Relatório I. Belo Horizonte, junho de 2005.

Segundo trabalhos que o citam (ver seção 4), o SCQA criou **equações próprias**
por regressão polinomial (Excel) sobre as curvas médias da NSF e estudos
brasileiros. **Elas não são as equações da CETESB.** O documento não foi
encontrado online (portal IGAM, repositório institucional, buscas por título),
e nenhuma fonte acessível transcreve as equações. Também não há caso numérico
oficial do IGAM com entradas e IQA publicados que permita validar.

Sem as equações, implementar o cálculo exigiria:
- reutilizar as curvas da CETESB, o que é proibido: o IGAM usa nitrato e
  fosfato, e o SCQA tem regressões próprias; ou
- inventar ou digitalizar curvas, o que também é proibido.

**Por isso o motor não foi habilitado.**

## 2. O que foi verificado e implementado

| Elemento | Valor no pacote | Fonte | Status |
|---|---|---|---|
| Parâmetros | OD (% sat.), coliformes termotolerantes ou *E. coli*, pH, DBO, **nitrato**, **fosfato total**, variação de temperatura, turbidez, sólidos totais | Portal Infohidro/IGAM, "Índice de Qualidade das Águas – IQA" (acesso 2026-10-15); IGAM, *Monitoramento da qualidade das águas superficiais na sub-bacia do ribeirão Pampulha — 3º trimestre 2017*, p. 9 (cita *E. coli*) | (a) |
| Pesos | 0,17 OD; 0,15 microbiológico; 0,12 pH; 0,10 DBO, nitrato, fosfato, temperatura; 0,08 turbidez, ST | Portal IGAM: tabela em imagem. Valores citados como "IGAM (2004)" em *Geografias* (UFMG) 6(2), 2010, Tabela 1 | (b) |
| Obrigatórios | Sem OD e/ou coliformes/*E. coli*, o indicador não é calculado | Portal IGAM (texto) | (a) |
| Demais ausentes | "o peso é repartido igualmente entre os demais parâmetros" | Portal IGAM (texto) | (a) para um ausente; com vários, soma repartida igualmente (**interpretação**) |
| Temperatura | qs constante = 92 | Portal IGAM (texto) | (a) |
| Classes | Muito Ruim [0, 25]; Ruim (25, 50]; Médio (50, 70]; Bom (70, 90]; Excelente (90, 100] | IGAM 2017, p. 9 (texto: "Muito Ruim (0 ≤ IQA ≤ 25), Ruim (25 < IQA ≤ 50)…") | (a). Resolve a pendência CL-01 |
| Equações dos qi | — | SCQA 2005 (não obtido) | **(d) bloqueado** |
| Unidade do nitrato | mg/L NO₃ (*Geografias* 2010) × N-NO₃ | Não confirmada em fonte primária | **(d)** |
| Conversões | Nenhuma implícita: `nt_total` não substitui nitrato; `p_total` (P) e ortofosfato não substituem fosfato total; TDS não substitui ST | Regra do projeto | — |

Implementado em `R/iqa_igam.R`:
- `.iqa_igam_spec`: especificação, com o status de cada item;
- `.igam_effective_weights()`: regra de ausentes, com testes;
- `.igam_check_inputs()`: diagnóstico de entradas, sem substituição de analitos;
- `.iqa_method_unavailable()`: erro classificado;
- `iqa_methods()` (exportada).

Nome canônico novo: `fosfato_total` (aliases `po4_total`, `total_phosphate_po4`).
É distinto de `p_ortofosfato` e de `p_total`.

## 3. Como desbloquear

1. Obter o SCQA (SEMAD/PNMA II, 2005), Relatório I, junto ao IGAM ou SEMAD,
   ou o manual do programa SCQA.
2. Transcrever as equações com dupla verificação. Confirmar a unidade do
   nitrato e a forma microbiológica atual (*E. coli* desde quando).
3. Montar fixtures independentes: amostras com entradas e IQA publicados pelo
   IGAM. A Série Histórica do Repositório Institucional IGAM traz dados
   primários desde 1997, e os relatórios trazem IQA por estação.
4. Implementar os qi em `R/iqa_igam.R`, ligar `available = TRUE` e validar
   contra as fixtures antes de documentar como disponível.

## 4. Fontes consultadas

- IGAM. Índice de Qualidade das Águas – IQA. https://portalinfohidro.igam.mg.gov.br/w/indice-de-qualidade-das-aguas-iqa (acesso 2026-10-15).
- IGAM (2017). *Monitoramento da qualidade das águas superficiais na sub-bacia do ribeirão Pampulha — Relatório trimestral, 3º trimestre 2017*, seção 3, p. 9. https://repositorioigam.meioambiente.mg.gov.br/bitstream/123456789/2370/1/
- *Geografias* (UFMG), 6(2), 2010, pp. 134–147: tabela de pesos "IGAM (2004)". https://periodicos.ufmg.br/index.php/geografias/article/download/13301/10533/35366
- XII Simpósio de Recursos Hídricos do Nordeste (2014), estudo do rio Piabanha: descreve o SCQA (regressões sobre as curvas da NSF; temperatura próxima de zero; classes NSF/IGAM). https://www.comitepiabanha.org.br/piabanha/trabalhos-academicos//academico/42014%20-%20(SETTA%20CANESIN%20SILVA%20VILLAS-BOAS%202014).pdf
- Busca sem sucesso pelo SCQA 2005 (título exato, portal IGAM, repositório IGAM, Google Scholar indireto).
