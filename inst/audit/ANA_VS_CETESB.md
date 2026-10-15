# ANA × CETESB — o pacote deve ter `method = "ANA"`?

- **Data:** 2026-10-15
- **Decisão:** **não** criar `method = "ANA"` nem `R/iqa_ana.R`.
  `classify_iqa(x, classification = "ana")` continua disponível, porque as
  faixas atuais da ANA têm fonte e coincidem com as da CETESB.

## 1. Fontes primárias

| Fonte | Conteúdo relevante |
|---|---|
| ANA, Portal de Qualidade das Águas — IQA. https://qualidadedaagua.ana.gov.br/iqa.html (acesso 2026-10-15) | 9 parâmetros: temperatura, pH, OD, DBO, coliformes, NT, PT, sólidos totais, turbidez. Faixas Péssima 0–19, Ruim 19–36, Regular 36–51, Boa 51–79, Ótima 79–100. "O cálculo do IQA requer medição de todos os parâmetros" |
| ANA, Portal PNQA — Indicadores: Índice de Qualidade das Águas. https://www.ana.gov.br/portalpnqa/indicadores-indice-aguas.aspx (acesso 2026-10-15) | Origem NSF (1970), uso pela CETESB desde 1975. Pesos: OD 0,17; coliformes 0,15; pH 0,12; DBO 0,10; temperatura 0,10; NT 0,10; PT 0,10; turbidez 0,08; resíduo total 0,08. Curvas como figura ("Fonte: ANA, 2004"), **sem equações**. Faixas por estado (abaixo) |

## 2. Matriz de comparação

| Item | CETESB (Apêndice D 2023) | ANA | Situação |
|---|---|---|---|
| Parâmetros | 9 (NT, PT, resíduo total) | Os mesmos 9 | Igual (a) |
| Unidades | NT mg/L N; PT → PO₄ (× 3,066); ST mg/L; CT/*E. coli* | Não especificadas nas páginas | **Não verificado** |
| Pesos | 0,17/0,15/0,12/0,10×4/0,08×2 | Os mesmos | Igual (a) |
| Agregação | ∏ qi^wi | ∏ qi^wi | Igual (a) |
| Curvas qi | Figura 1 (gráficas) | Figura "ANA, 2004" (gráficas) | **Não verificado:** as figuras não foram comparadas ponto a ponto e nenhuma tem equações |
| Temperatura | "melhor qualidade" (q^w = 1,58) | Não especificado | **Não verificado** |
| Ausentes | IQA inviabilizado | "requer medição de todos os parâmetros" | Igual (a) |
| *E. coli* | × 1,25 | Não especificado | **Não verificado** |
| Classes (portal atual) | 19/36/51/79, fechadas à direita | 0–19 / 19–36 / 36–51 / 51–79 / 79–100 | Mesmos cortes; inclusividade da ANA não explícita |
| Classes PNQA por estado | — | BA, CE, ES, GO, MS, PB, **PE**, **SP**: 0–19, 20–36, 37–51, 52–79, 80–100. AL, **MG**, MT, PR, RJ, RN, RS: 0–25, 26–50, 51–70, 71–90, 91–100 | O 1º grupo é a escala CETESB em inteiros; o 2º equivale aos cortes do IGAM |
| Quem calcula | CETESB | Não declarado: a ANA divulga índices de redes estaduais, cada uma com o seu método | **Não verificado** |

## 3. Conclusão

- A ANA **divulga** o IQA no formato CETESB (parâmetros, pesos, agregação,
  regra de ausentes) e as faixas de cada grupo de estados. Ela não publica um
  algoritmo numérico próprio com equações.
- Equivalência integral **não comprovada**: curvas (só figuras), unidades,
  temperatura e conversão de *E. coli* não estão especificadas nas fontes da
  ANA. Pelas regras do projeto, **não se expõe `method = "ANA"`**, nem como
  alias. Um alias faria o usuário acreditar que reproduz cálculos divulgados
  pela ANA, que podem vir de redes estaduais com métodos próprios (o IGAM, por
  exemplo).
- `classify_iqa(x, classification = "ana")`: mantido. As faixas do portal atual
  têm fonte (a) e equivalem às da CETESB.
- As faixas antigas "20/37/52/79" citadas na especificação são as do 1º grupo
  do PNQA lidas como limites inferiores; não são uma escala distinta.
- `compare_iqa()` não aceita `"ANA"`.
