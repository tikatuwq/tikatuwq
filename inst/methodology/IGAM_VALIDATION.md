# IQA-IGAM no tikatuwq: metodologia, fontes e validação

- **Versão:** tikatuwq 0.11.0
- **Data:** 2026-10-16
- **Uso:** `iqa(df, method = "IGAM", ...)`

> **Natureza da implementação.** O `tikatuwq` implementa uma **reprodução
> computacional documentada** do Índice de Qualidade das Águas usado pelo
> Instituto Mineiro de Gestão das Águas (IGAM). Ela se baseia no SCQA (2005),
> na documentação institucional do Projeto Águas de Minas e em literatura
> acadêmica. **Não é software homologado, certificado ou endossado pelo
> IGAM.**

## 1. Origem e escopo

- **Equações dos subíndices (qᵢ):** SEMAD/UCEMG/PNMA II. *Sistema de Cálculo
  da Qualidade da Água (SCQA) — Estabelecimento das Equações do Índice de
  Qualidade das Águas (IQA)*. Relatório 1. Belo Horizonte, jun. 2005.
  - Cópia examinada: a hospedada pelo IGAM em
    `http://aguas.igam.mg.gov.br/aguas/downloads/SCQA_final.pdf`, arquivada no
    Internet Archive (snapshots 2007-03-10 e 2007-08-04, SHA-1 conferido com o
    índice do arquivo).
  - Transcrição dupla (leitura visual e extração de texto), com as
    equações na §3 desta nota.
- **Regras operacionais:** IGAM, página "Índice de Qualidade das Águas – IQA"
  (https://igam.mg.gov.br/w/indice-de-qualidade-das-aguas-iqa, acesso em
  2026-10-15). Ela define:
  - os pesos (Tabela 1);
  - a unidade do nitrato (mg/L NO₃⁻);
  - qs de temperatura constante = 92 no Projeto Águas de Minas;
  - que sem OD e/ou microbiológico o índice não é calculado;
  - que o peso de outro parâmetro ausente é repartido igualmente.
- **Classes:** SCQA (Tabela 2.2) e IGAM (2017), relatório Pampulha, p. 9.

## 2. Método de cálculo, classe e comparação com a CETESB

- **Motor independente:** `method = "IGAM"` é um motor próprio, com curvas,
  pesos, unidades e regras de ausência próprios. Não é o motor CETESB com
  outros rótulos.
- **Escala de classe:** `classify_iqa(x, "igam")` só aplica a escala IGAM a um
  número. Classificar pela escala IGAM um IQA calculado pela CETESB **não**
  produz um IQA-IGAM.
- **Comparação:** `compare_iqa(df, methods = c("CETESB", "IGAM"))` calcula os
  dois métodos sobre as mesmas linhas. As estatísticas usam só as amostras
  elegíveis nos dois. Diferenças de classe aparecem como transições, não como
  "concordância de rótulos".

## 3. Equações, domínios, unidades e pesos

As páginas citadas são as do SCQA 2005.

**Agregação:** IQA = ∏ qᵢ^wᵢ.

| Parâmetro | Peso | Unidade da curva | Equação (domínio) | Pág. |
|---|---|---|---|---|
| Saturação de OD (Cs) | — | mg/L; T em °C; Cl em mg/L; H em m | Cs = (14,2·e^(−0,0212T) − 0,0016·Cl·e^(−0,0264T))·(0,994 − 0,0001042·H); OD% = 100·OD/Cs | 3 |
| Oxigênio dissolvido | 0,17 | % de saturação | 100·sen²(y₁) − (2,5·sen y₂ − 0,018·OD% + 6,86)·sen y₃ + 12/(e^y₄ + e^y₅), com y₁ = 0,01396·OD% + 0,0873, y₂ = π/56·(OD% − 27), y₃ = π/85·(OD% − 15)*, y₄ = (OD% − 65)/10, y₅ = (65 − OD%)/10 (OD% ≤ 100); −0,00777142857142832·OD%² + 1,27854285714278·OD% + 49,8817148572 (100 < OD% ≤ 140); 47 (> 140) | 4 |
| Microbiológico (*E. coli* ou coliformes termotolerantes) | 0,15 | NMP/100 mL | 98,24034 − 34,7145·L + 2,614267·L² + 0,107821·L³, com L = log₁₀ (1 ≤ CF ≤ 10⁵); 3 (> 10⁵) | 5 |
| pH | 0,12 | — | 2 (≤ 2); −37,1085 + 41,91277·pH − 15,7043·pH² + 2,417486·pH³ − 0,091252·pH⁴ (2–6,9); −4,69365 − 21,4593·pH − 68,4561·pH² + 21,638886·pH³ − 1,59165·pH⁴ (6,9–7,1); −7698,19 + 3262,031·pH − 499,494·pH² + 33,1551·pH³ − 0,810613·pH⁴ (7,1–12); 3 (> 12) | 6 |
| DBO₅,₂₀ | 0,10 | mg/L | 100,9571 − 10,7121·x + 0,49544·x² − 0,011167·x³ + 0,0001·x⁴ (≤ 30); 2 (> 30) | 7 |
| Nitrato | 0,10 | **mg/L NO₃** | −5,1·x + 100,17 (≤ 10); −22,853·ln x + 101,18 (10–60); 10¹⁰·x^(−5,1161) (60–90); 1 (> 90) | 8 |
| Fosfato total | 0,10 | **mg/L PO₄** | 79,7·(x + 0,821)^(−1,15) (≤ 10); 5 (> 10) | 9 |
| Variação de temperatura | 0,10 | — | **qs = 92** constante (Projeto Águas de Minas). O SCQA dá 93 em ΔT = 0 | 12; portal |
| Turbidez | 0,08 | UNT | 90,37·e^(−0,0169·Tu) − **1,5**·cos(0,0571·(Tu − 30)) + 10,22·e^(−0,231·Tu) − 0,8, cosseno em radianos (≤ 100); 5 (> 100) | 10 (ver §4) |
| Sólidos totais | 0,08 | mg/L | 133,17·e^(−0,0027·ST) − 53,17·e^(−0,0141·ST) − 6,2·e^(−0,00462·ST)·sen(0,0146·ST) (≤ 500); 30 (> 500) | 11 |

\* O SCQA imprime um traço em y₃. A leitura como multiplicação é a única que
reproduz as Figuras 2.1 e 2.2 do SCQA e a curva do portal IGAM (q ≈ 3 em 0 %).

**Limiares:**
- O valor no limiar pertence ao trecho inferior (≤).
- As descontinuidades da própria fonte são mantidas, não suavizadas:

| Parâmetro | Ponto | Comportamento |
|---|---|---|
| OD | 100 % | 99,6 → 100,02 |
| pH | 6,9 e 7,1 | Saltos de cerca de −2,9 e −2,8 |
| Turbidez | 100 UNT | 16,86 → 5 |
| Sólidos totais | 500 mg/L | 33,95 → 30 |

**Truncamento:** os qᵢ não são limitados a 100, porque o SCQA não o define. Os
máximos são 100,02 (OD), 100,17 (nitrato) e 100,003 (turbidez).

## 4. Divergência 15 × 1,5 no termo do cosseno da turbidez

- **Versões do SCQA:** as cópias examinadas, inclusive as hospedadas pelo
  IGAM, imprimem **−15·cos(0,0571·(Tu − 30))**. Essa expressão:
  - não é monotônica (32,5 em 40 UNT e 37,0 em 80 UNT);
  - não reproduz a Figura 2.13 do próprio SCQA nem a curva do portal IGAM;
  - erra em até 1,5 ponto os IQAs publicados.
- **Publicações acadêmicas com −1,5:**
  - **BUCCI, M. M. H. S.** *Caracterização da qualidade da água na represa
    Dr. João Penido (Juiz de Fora, MG).* Dissertação (Mestrado em Ecologia),
    Universidade Federal de Juiz de Fora, 2011. **p. 51**, item g.
    https://repositorio.ufjf.br/jspui/bitstream/ufjf/2078/1/mariamagalyheidenreichsilvabucci.pdf
    — A página foi conferida visualmente. A autora informa ter usado o
    programa de cálculo do IQA cedido pelo IGAM, e a mesma página traz qs de
    temperatura = 92.
  - **NUNES, D. G.** *Modelagem da autodepuração e qualidade da água do rio
    Turvo Sujo.* Dissertação (Mestrado em Engenharia Agrícola), Universidade
    Federal de Viçosa, 2008. **Quadro 10 (continuação), p. 52.**
    https://locus.ufv.br/server/api/core/bitstreams/923c64e4-e7c7-4984-ad7d-76fa708db3be/content
    — Conferido visualmente.
- **Outras publicações divergem.** Por exemplo, MARTINS (UFVJM, 2016, p. 56)
  reproduz o 15. AFFONSO (UFV, 2016) traz 1,5, mas com outros erros de
  transcrição.
- **Escolha:** o pacote adota **1,5**, por dois motivos:
  1. o coeficiente está registrado em publicações acadêmicas, uma delas
     vinculada ao programa do IGAM;
  2. ele reproduz os IQAs publicados pelo IGAM (§5), sem estimar nenhum
     coeficiente a partir desses valores.
- **O que esta escolha não é:** **não** é confirmação formal do IGAM. As cópias
  históricas do SCQA não foram corrigidas: a expressão com 15 continua
  documentada, só para auditoria.

## 5. Validação: 49 IQAs publicados pelo IGAM

### Dados e conjuntos

- **Entradas:** planilhas oficiais *Séries Históricas* do IGAM.
- **IQAs publicados:** relatórios trimestrais da sub-bacia do ribeirão
  Pampulha.
- **Fontes e SHA-1:** ver "Dados não redistribuídos" adiante.
- **Conjuntos:**
  - **15 casos da investigação:** 3º trimestre de 2017. Foram usados para
    diagnosticar a turbidez, então são **reprodução histórica**, não validação
    independente.
  - **34 casos externos:** 3º trimestre de 2018 e 4º trimestre de 2017. Foram
    selecionados com critérios **pré-registrados** antes de qualquer cálculo
    (`audit/dados/PRE_REGISTRO_VALIDACAO_EXTERNA.md`), sem exclusões.

### Configuração usada (fixa)

- Nitrato informado em mg/L N, convertido para NO₃.
- Fósforo total informado em mg/L P, convertido para PO₄.
- Valores censurados no limite.
- Altitude da estação conforme a planilha do IGAM.

### Métricas (valor não arredondado − publicado)

| Conjunto | n | Iguais após arredondar a 1 casa | Dentro de ±0,05 | MAE | RMSE | Viés | Erro máx. |
|---|---|---|---|---|---|---|---|
| Investigação (3º trim. 2017) | 15 | 15 | 15 | 0,0232 | 0,0277 | +0,0012 | 0,0448 |
| Externa (3º trim. 2018 e 4º trim. 2017) | 34 | 34 | 34 | 0,0262 | 0,0298 | −0,0011 | 0,0489 |
| **Total** | **49** | **49** | **49** | 0,0253 | 0,0291 | −0,0004 | **0,048876** |

**Casos limítrofes:**
- Seis valores brutos ficaram a menos de 0,005 da tolerância de 0,05 (erros
  absolutos de 0,0457 a 0,0489).
- A menor distância de um valor bruto a um ponto de desempate do arredondamento
  (x,x5) foi 0,0011. Por isso a regra de arredondamento do IGAM não foi posta à
  prova.

**Casos que testam o coeficiente da turbidez:** 45 dos 49. Os outros 4 têm
turbidez > 100 UNT e q = 5 independentemente do coeficiente.

**Controles de sensibilidade** (não usados no motor):

| Variante | Iguais (15 + 34) | Erro máx. |
|---|---|---|
| Turbidez com 15 | 2/15 e 5/34 | 1,51 e 2,41 |
| Curva de turbidez CETESB | 8/15 e 19/34 | 0,15 e 0,20 |

**Caso suplementar (fora das métricas):** uma estação do 3º trimestre de 2017
ficou fora dos 15 porque o valor estava ilegível no relatório de 2017. Com o
valor publicado no relatório de 2018, o cálculo também coincidiu na 1ª casa.
Esse caso tem turbidez > 100 UNT, então não testa o coeficiente.

### Dados não redistribuídos

- **Fora do pacote:** os 49 registros (entradas e IQA publicados por amostra)
  são dados do IGAM e **não acompanham o pacote distribuído**. Também não
  estão no pacote as planilhas originais nem as tabelas por amostra.
- **O que está documentado:** só as métricas agregadas acima.
- **Testes do pacote:** os testes automatizados distribuídos usam **dados
  sintéticos**, com valores esperados calculados de forma independente (ver
  §9). A reprodução dos 49 casos é uma **validação histórica externa**, feita e
  arquivada fora da distribuição.

**Fontes para reproduzir** (Repositório Institucional do IGAM,
https://repositorioigam.meioambiente.mg.gov.br; acesso em 2026-10-15):

| Conjunto | Entradas (planilha *Séries Históricas*, aba "São Francisco") | IQA publicado (relatório trimestral da sub-bacia do ribeirão Pampulha) |
|---|---|---|
| 3º trim. 2017 (15) | `2017.xlsx`, handle 123456789/3077 (SHA-1 `b205ea95ec278a6879a9a1239d07fd99b009ae61`) | handle 123456789/2370, Tabela 7 |
| 3º trim. 2018 (17) | `Dados_monitoramento 2018_3° trimestre (1).xlsx`, handle 123456789/3049 (SHA-1 `793a017800a76fa77ec63f103acce4faefd04576`) | handle 123456789/2927 (SHA-1 `34df8cdc7f3708aa6ce7ad3dd3988d7d48ec0276`) |
| 4º trim. 2017 (17) | `2017.xlsx` (acima), registros de out–dez | handle 123456789/2365 (SHA-1 `700c8d91288131dba0f6e37ca44e95375552abc6`) |
| Altitude das estações | `Dados_monitoramento 2018_4° trimestre (2).xlsx`, handle 123456789/3050 (SHA-1 `f0b165ff6b62b3b2976d210099e90fa9f6c9bf53`) | — |

**Procedimento:**
- No repositório de desenvolvimento (https://github.com/tikatuwq/tikatuwq), os
  scripts `audit/igam_extrair_validacao_externa.R` e
  `audit/auditoria_final_49.R` extraem as entradas das planilhas e recalculam
  os 49 IQAs pela API pública.
- Os scripts **não contêm dados**. As planilhas, os IQAs publicados e as
  altitudes das estações são lidos de um diretório informado pelo usuário,
  fora do repositório. As saídas por amostra também são gravadas fora do
  repositório.
- Os scripts não baixam nada e recusam gravar dentro do repositório.
- Os critérios pré-registrados estão em
  `audit/dados/PRE_REGISTRO_VALIDACAO_EXTERNA.md`, no mesmo repositório.

## 6. Entradas e políticas do pacote

Estas são decisões operacionais do `tikatuwq`, **não** regras do IGAM.

- **Nitrato:** `nitrate_unit` é **obrigatório** (`"N"` ou `"NO3"`). Não há
  unidade presumida. Nitrogênio total nunca substitui nitrato.
- **Fósforo:**
  - `fosfato_total` (PO₄) é usado diretamente;
  - `p_total` exige `phosphorus_basis` explícito (`"P"` converte ×
    94,9714/30,9738; `"PO4"` não converte);
  - ortofosfato nunca substitui fosfato total.
- **Microbiológico:** *E. coli* ou coliformes termotolerantes, sem fator de
  conversão. Com as duas colunas presentes, é preciso informar
  `microbial_type`.
- **Cloreto:** obrigatório para a saturação de OD. **Sem valor padrão.** Sem
  cloreto (ou sem temperatura), o OD não é calculado e, portanto, o IQA também
  não, com o motivo na linha.
- **Microbiológico < 1 NMP/100 mL:** fora do domínio do SCQA → `NA`, com
  motivo.
- **Censura:** `censor_policy`, com padrão `"limit"`; `"preserve"` não é
  aceito. A política e os valores censurados por linha ficam nos atributos.
  `"limit"` reproduziu os 49 casos (35 com algum dado censurado), mas não é
  uma regra documentada pelo IGAM.
- **Valores fisicamente inválidos** (negativos, pH fora de 0–14): `NA` com
  aviso, tratados como ausentes.
- **Não se aplicam (geram erro):**
  - `allow_partial`, porque vale a regra de ausência do IGAM;
  - `pesos`;
  - as opções de temperatura da CETESB.
- **Saída:**
  - `IQA` com 1 casa, como o IGAM publica;
  - a classe IGAM é atribuída sobre esse valor;
  - `details = TRUE` traz o valor bruto, os qᵢ e os pesos efetivos.

## 7. Limitações

1. **Abrangência:** todas as 49 observações vêm da **sub-bacia da Pampulha**
   (2017–2018) e do mesmo órgão. Não há validação em outras bacias.
2. **Nitrato:** foi quase sempre censurado (< 0,02 mg/L N). Os trechos de
   nitrato acima de 10 mg/L NO₃ não foram exercitados por dados publicados.
3. **Confirmação institucional:** **não há** confirmação formal do IGAM sobre
   o coeficiente 1,5 nem sobre as regras de cloreto ausente, censura e
   arredondamento.
4. **Saturação de OD:** usa a equação do SCQA, que exige cloreto. Outras
   formulações de Cs não são equivalentes.
5. **Temperatura:** o qs = 92 vale para o Projeto Águas de Minas. Não há
   cálculo de ΔT.

## 8. Validação futura desejável

- Confirmação, pelo IGAM/SEMAD, do coeficiente usado no programa
  institucional.
- IQAs publicados de outras bacias da rede estadual, sobretudo com nitrato
  quantificado.
- Regras institucionais para dados censurados, cloreto ausente e
  arredondamento.

## 9. Testes distribuídos e situação em `iqa_methods()`

- **Testes automatizados** (`tests/testthat/test-iqa-igam-*.R`):
  - funcionam offline e usam só **dados sintéticos**;
  - cobrem os nove subíndices nos pontos abaixo, igual e acima de cada
    limiar, com uma transcrição das equações independente do código do
    pacote;
  - cobrem as regras de entrada (unidade do nitrato, base do fósforo,
    cloreto, colunas microbiológicas, censura, ausências, argumentos
    incompatíveis) e `compare_iqa()`;
  - incluem amostras sintéticas completas, cujo IQA esperado foi calculado
    **fora do pacote**, por um programa independente em Python
    (`audit/igam_valores_esperados_sinteticos.py` no repositório de
    desenvolvimento). Os valores ficam fixados nos testes com tolerância
    predefinida.
- **Distinção:** esses testes verificam a implementação. Eles **não
  substituem** a validação histórica dos 49 casos (§5), que é externa e
  arquivada fora da distribuição.
- **`iqa_methods()`:**
  - `available = TRUE`: o método pode ser usado em `iqa()`;
  - `reference_validated = TRUE`: o motor reproduz IQAs **publicados** pelo
    IGAM, no escopo descrito em `limitation` (49 registros, sub-bacia da
    Pampulha). **Não** significa homologação, certificação ou validação em
    outras bacias;
  - `limitation` resume as restrições.
