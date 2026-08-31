# PROMPT MESTRE — Auditoria, correção científica e atualização do pacote `tikatuwq`

## Objetivo geral

Atue simultaneamente como:

- **maintainer sênior de pacotes R/CRAN**;
- **cientista de qualidade da água** com conhecimento de IQA/CETESB, NSF WQI e legislação brasileira;
- **auditor de reprodutibilidade científica**;
- **revisor crítico e imparcial**.

Sua missão é **auditar, corrigir, validar e preparar uma nova versão do pacote `tikatuwq`**, corrigindo problemas metodológicos identificados no cálculo do IQA, no NSF WQI, no tratamento de valores censurados e na verificação de conformidade com a Resolução CONAMA 357/2005.

O pacote será utilizado em **artigos científicos**, portanto o critério principal não é apenas “o código rodar”, mas sim:

> **o resultado numérico precisa ser cientificamente rastreável, reproduzível, metodologicamente correto e defensável perante revisores.**

Não aplique atalhos, não invente equações, não faça correções silenciosas e não preserve comportamento antigo incorreto apenas para evitar breaking changes.

---

# 1. CONTEXTO E ESTADO ATUAL

## 1.1 Versão de referência

A versão atualmente publicada no CRAN é:

```text
tikatuwq 0.9.0
Publicação CRAN: 2026-06-09
```

Ela introduziu uma correção importante:

```r
IQA = prod(Qi ^ Wi)
```

substituindo a agregação aritmética anterior por **produtório geométrico ponderado** para o IQA CETESB.

Entretanto, uma auditoria do código 0.9.0 encontrou problemas metodológicos ainda relevantes.

## 1.2 Atenção: repositório de desenvolvimento e CRAN podem estar dessincronizados

Foi observado que o repositório:

```text
https://github.com/tikatuwq/tikatuwq
```

pode estar com `main` atrás da versão efetivamente publicada no CRAN, enquanto o espelho:

```text
https://github.com/cran/tikatuwq
```

contém o código-fonte da versão 0.9.0 publicada.

### Regra obrigatória

**NÃO comece alterando arquivos do `main` sem antes estabelecer qual é a fonte de verdade.**

Antes de qualquer mudança:

1. verificar `DESCRIPTION`;
2. verificar tags/releases;
3. comparar `tikatuwq/tikatuwq` com `cran/tikatuwq`;
4. localizar o commit correspondente ao CRAN 0.9.0;
5. identificar se existem mudanças mais novas no repositório principal que não foram para o CRAN;
6. preservar qualquer evolução legítima posterior;
7. criar um branch de trabalho dedicado.

Sugestão:

```text
fix/cetesb-validation-v0.10.0
```

Se o `main` estiver realmente atrás do CRAN 0.9.0, primeiro sincronizar/reconciliar o código, **sem apagar histórico nem mudanças legítimas**.

Antes das correções, registrar:

```text
- SHA/commit de origem
- versão do DESCRIPTION
- resultado de R CMD check
- número de testes
- testes que já falham antes da intervenção
```

---

# 2. PRIORIDADE MÁXIMA — IQA CETESB

O método oficial precisa ser tratado como uma implementação científica independente, não como uma simples adaptação aproximada do NSF.

A implementação final precisa reproduzir o método utilizado por CETESB/INEMA.

---

# 3. PARÂMETROS OFICIAIS DO IQA CETESB

O IQA CETESB utiliza nove componentes.

Use como conjunto canônico:

| Componente | Peso |
|---|---:|
| Oxigênio dissolvido | 0.17 |
| Coliformes termotolerantes / equivalente validado de E. coli | 0.15 |
| pH | 0.12 |
| DBO5 | 0.10 |
| Nitrogênio total | 0.10 |
| Fósforo total | 0.10 |
| Variação de temperatura | 0.10 |
| Turbidez | 0.08 |
| Sólidos totais / resíduo total | 0.08 |

Confirme:

```r
sum(weights) == 1
```

---

# 4. ERRO CRÍTICO: `tds` NÃO É SÓLIDOS TOTAIS

A implementação atual utiliza:

```text
tds
```

como componente de peso 0.08.

Isso deve ser corrigido.

O IQA CETESB utiliza:

```text
solidos_totais
```

ou:

```text
residuo_total
```

e **não Total Dissolved Solids (TDS)**.

### Implementação obrigatória

O nome canônico deve ser:

```r
solidos_totais
```

Aceitar aliases semanticamente equivalentes, por exemplo:

```text
residuo_total
total_solids
solidos_total
```

### Não aceitar silenciosamente

```text
tds
solidos_dissolvidos
total_dissolved_solids
```

como equivalentes a sólidos totais.

Se o usuário fornecer `tds` em modo CETESB estrito, emitir erro ou mensagem inequívoca:

```text
CETESB IQA requires total solids (solidos_totais/residuo_total).
TDS (total dissolved solids) is not interchangeable with total solids.
```

O comportamento legado pode ser mantido apenas em um método explicitamente identificado como legado.

---

# 5. AGREGAÇÃO OFICIAL

O cálculo deve ser:

\[
IQA = \prod_{i=1}^{9} q_i^{w_i}
\]

Não utilizar média aritmética no método CETESB.

Criar teste matemático independente que compare o resultado do código com um cálculo direto:

```r
expected <- prod(qi ^ weights)
expect_equal(result, expected, tolerance = ...)
```

---

# 6. O MÉTODO PADRÃO ATUAL `CETESB` NÃO PODE MAIS USAR CURVAS “APPROXIMATE”

O arquivo atual `R/iqa_curves.R` declara explicitamente suas tabelas como aproximadas.

Essas tabelas podem ser preservadas para compatibilidade histórica, mas:

> **não devem continuar sendo a implementação padrão denominada simplesmente `CETESB`.**

Recomendação de arquitetura:

```text
method = "CETESB"
```

deve significar:

> implementação CETESB validada.

O comportamento antigo pode ser preservado como algo como:

```text
"CETESB_legacy_approx"
```

ou nome equivalente claramente marcado como legado/deprecated.

Se `NSF_approx` estiver sendo usado como nome para uma média aritmética aplicada às curvas aproximadas CETESB, esse nome também é conceitualmente ambíguo e deve ser depreciado ou renomeado.

---

# 7. AUDITORIA COMPLETA DAS EQUAÇÕES `Qi`

Auditar **cada equação, cada intervalo, cada operador, cada base exponencial e cada unidade**.

Não assumir que o código existente está correto.

Arquivos atuais envolvidos:

```text
R/iqa.R
R/iqa_curves.R
R/iqa_equations.R
```

Criar preferencialmente helpers internos independentes por parâmetro, por exemplo:

```r
.qi_cetesb_od()
.qi_cetesb_coliformes()
.qi_cetesb_ph()
.qi_cetesb_dbo()
.qi_cetesb_nt()
.qi_cetesb_pt()
.qi_cetesb_temperatura()
.qi_cetesb_turbidez()
.qi_cetesb_solidos_totais()
```

ou uma arquitetura equivalente, desde que fique testável e auditável.

---

# 8. CORREÇÕES JÁ IDENTIFICADAS NAS EQUAÇÕES

## 8.1 DBO

Foi identificado no código um uso do tipo:

```r
99 * 10^(-0.1232728 * C)
```

A formulação de referência usada para validação é:

```r
99.96 * exp(-0.1232728 * C)
```

para a faixa correspondente.

Não trocar `exp()` por `10^()`.

Auditar todas as demais faixas de DBO.

---

## 8.2 Nitrogênio total

Foi identificado uso incorreto de base 10 em trecho que deve usar exponencial natural.

Auditar especialmente:

```text
60 < N <= 100
```

e garantir que a base exponencial esteja de acordo com a equação de referência.

---

## 8.3 Fósforo total

O laboratório normalmente reporta:

```text
mg P/L
```

mas a curva correspondente é aplicada sobre fosfato.

Implementar explicitamente:

\[
PO_4 = P_{total} \times 3.066
\]

quando a entrada estiver em `mg P/L`.

### API recomendada

Não adivinhar a base química.

Usar argumento explícito, por exemplo:

```r
phosphorus_basis = c("P", "PO4")
```

Default para os dados brasileiros:

```r
"P"
```

Documentar claramente a transformação.

As equações exponenciais também precisam utilizar a base correta (`exp`, quando aplicável), e não `10^`.

---

# 9. TEMPERATURA: SEPARAR TEMPERATURA MEDIDA PELA SONDA DE VARIAÇÃO TÉRMICA DO IQA

Este ponto deve ser tratado com máxima clareza porque é uma fonte fácil de erro conceitual.

## 9.1 O que a sonda realmente mede

Na rotina de monitoramento, sondas multiparamétricas normalmente fornecem apenas um valor como:

```text
Temperatura da água = 27.8 °C
```

Esse valor representa a **temperatura absoluta da água no instante e local da medição**.

Ele NÃO informa, por si só:

```text
temperatura de equilíbrio
temperatura de referência
temperatura natural esperada
variação térmica causada por lançamento
ΔT do componente térmico do IQA
```

Portanto, a biblioteca **não pode inferir `delta_temperatura` a partir de uma única leitura da sonda**.

É proibido adotar silenciosamente qualquer transformação do tipo:

```r
delta_temperatura <- temperatura_agua
```

ou interpretar diretamente 27 °C, 29 °C ou 31 °C como entrada da curva de variação térmica do IQA.

## 9.2 Duas variáveis conceitualmente diferentes

A implementação deve separar explicitamente:

```text
temperatura_agua
```

que é a temperatura medida pela sonda e pode ser utilizada, entre outras finalidades, no cálculo da concentração/% de saturação de oxigênio dissolvido;

e:

```text
delta_temperatura
```

que representa o afastamento térmico em relação a uma referência metodologicamente válida e só deve existir quando essa referência realmente for conhecida.

Conceitualmente:

\[
\Delta T = T_{agua} - T_{referencia}
\]

A biblioteca não deve inventar `T_referencia`.

## 9.3 Quando existe uma referência térmica válida

Somente calcular `delta_temperatura` automaticamente quando o usuário fornecer uma referência explícita e defensável, por exemplo:

```text
- temperatura a montante de um lançamento térmico;
- temperatura de controle determinada pelo desenho amostral;
- valor de referência definido pelo estudo e documentado metodologicamente.
```

Uma possível API avançada seria:

```r
temperature_method = "reference"
temperature_reference = ...
```

ou:

```r
temperature_method = "delta"
delta_temperatura = ...
```

Quando `temperature_method = "reference"`:

```r
delta_temperatura <- temperatura_agua - temperature_reference
```

A decisão e a referência utilizada devem ser registradas nos metadados do resultado.

## 9.4 Comportamento operacional CETESB quando só existe a temperatura da sonda

Nos dados rotineiros de monitoramento, como os produzidos por sondas e por bases como INEMA/SEIA, frequentemente existe apenas `temperatura_agua`.

Nessa situação, **não transformar a temperatura absoluta em ΔT**.

Para o modo oficial CETESB adotado nesta biblioteca, seguir a convenção operacional documentada da metodologia quando não houver afastamento térmico especificamente caracterizado:

```text
Qi_temperatura = 94
```

Assim, o comportamento padrão deve ser conceitualmente equivalente a:

```r
temperature_method = "cetesb_default"
```

onde:

```text
temperatura_agua
    -> continua sendo usada no cálculo da saturação de OD

Qi_temperatura
    -> recebe 94 quando não houver delta térmico validamente informado
```

Isso NÃO significa que a temperatura medida pela sonda seja 94 nem que 94 °C seja utilizado. `94` é o **subíndice de qualidade `Qi` do componente térmico** adotado na condição operacional correspondente.

## 9.5 Fluxo obrigatório

A implementação deve refletir a seguinte lógica:

```text
SONDA
  |
  +--> temperatura_agua (°C)
  |       |
  |       +--> utilizada para cálculo de saturação de OD
  |
  +--> NÃO gera automaticamente delta_temperatura

COMPONENTE TÉRMICO DO IQA
  |
  +--> existe delta_temperatura ou referência térmica válida?
          |
          +--> SIM -> calcular Qi_temperatura pela curva/equação apropriada
          |
          +--> NÃO -> aplicar convenção CETESB documentada: Qi_temperatura = 94
```

## 9.6 Nomes de colunas e aliases

Nome canônico recomendado:

```r
temperatura_agua
```

Aceitar por compatibilidade aliases como:

```text
temperatura
temp
water_temperature
```

mas documentar que todos significam **temperatura absoluta da água**.

Para a variável térmica do IQA usar nome independente:

```r
delta_temperatura
```

com aliases explícitos apenas se semanticamente equivalentes, por exemplo:

```text
temp_change
delta_t
temperature_change
```

Nunca tratar `temperatura`, `temp` ou `temperatura_agua` como alias de `delta_temperatura`.

## 9.7 Corrigir bug estrutural de `iqa_official()`

Na implementação atual, quando existem simultaneamente:

```text
od
temperatura
```

a temperatura pode ser usada para calcular a saturação do OD enquanto o componente de peso 0.10 da temperatura desaparece do conjunto `Q`.

Isso faz um suposto IQA de nove componentes funcionar, na prática, com oito componentes e possível reponderação implícita.

Esse bug deve ser eliminado.

A implementação oficial precisa conter separadamente:

```text
Qi_OD
Qi_temperatura
```

O fato de `temperatura_agua` ser utilizada no cálculo de `Qi_OD` não elimina nem substitui `Qi_temperatura`.

## 9.8 Testes obrigatórios específicos de temperatura

Criar testes que garantam:

### Caso A — somente temperatura da sonda

Entrada:

```text
temperatura_agua = 29 °C
sem delta_temperatura
sem temperatura_referencia
```

Esperado:

```text
Qi_temperatura = 94
```

A temperatura de 29 °C pode influenciar a saturação de OD, mas **não deve ser aplicada diretamente à curva de ΔT**.

### Caso B — alteração apenas da temperatura da água

Mantendo `delta_temperatura` ausente, variar:

```text
25 °C -> 30 °C
```

Esperado:

```text
Qi_temperatura permanece 94
```

mas `Qi_OD` pode mudar porque a concentração de saturação de OD depende da temperatura da água.

### Caso C — delta térmico fornecido

Entrada:

```text
temperature_method = "delta"
delta_temperatura = valor conhecido
```

Esperado:

```text
Qi_temperatura calculado pela curva/equação térmica
```

### Caso D — referência térmica fornecida

Entrada:

```text
temperatura_agua = 29
temperature_reference = 27
```

Esperado:

```text
delta_temperatura = 2
```

e somente então aplicar a curva térmica correspondente.

### Caso E — impedir inferência incorreta

Um dataset com:

```text
temperatura_agua = 29
```

não pode gerar internamente:

```text
delta_temperatura = 29
```

Criar teste de regressão específico para impedir que esse erro retorne em versões futuras.

---

# 10. OXIGÊNIO DISSOLVIDO

O subíndice de OD não deve usar simplesmente OD em mg/L diretamente em uma tabela aproximada.

Calcular:

```text
concentração de saturação
% de saturação
Qi_OD
```

considerando:

```text
temperatura da água
altitude
OD medido
```

A equação de saturação atualmente existente deve ser novamente auditada contra a referência.

Suportar altitude de maneira transparente.

Idealmente aceitar:

```r
altitude_m = scalar
```

e, se viável sem complicar excessivamente a API:

```r
altitude_col = "altitude"
```

ou vetor por linha.

Criar testes para:

```text
0–50%
50–85%
85–100%
100–140%
>140%
```

incluindo pontos exatamente nas fronteiras.

---

# 11. TURBIDEZ

A implementação atual precisa ser corrigida.

A estrutura de referência a validar é:

```text
0–25 NTU
25–100 NTU
>100 NTU
```

e não a lógica atual aproximada com faixas 0–150 / 150–500.

Validar as equações:

```r
0 <= Turb <= 25:
100.17 - 2.67*Turb + 0.03775*Turb^2
```

```r
25 < Turb <= 100:
84.76 * exp(-0.016206*Turb)
```

```r
Turb > 100:
5
```

Confirmar limites e inclusividade na fonte adotada.

---

# 12. SÓLIDOS TOTAIS

Implementar a curva de **sólidos totais**, não TDS.

Estrutura de referência:

```r
0 <= ST <= 150:
79.75 + 0.166*ST - 0.001088*ST^2
```

```r
150 < ST <= 500:
101.67 - 0.13917*ST
```

```r
ST > 500:
32
```

Criar testes exatamente em:

```text
0
150
500
```

e imediatamente abaixo/acima das fronteiras.

---

# 13. pH

A função atual deve sofrer auditoria coeficiente por coeficiente.

Há indícios de divergências entre:

```text
76.36 versus 77.36
10^ versus exp
equações das faixas 8.5–9 e 9–10
```

Não corrigir com base em memória.

### Regra

Comparar:

1. documentação CETESB;
2. fontes técnicas de consolidação das equações;
3. curvas oficiais;
4. benchmark INEMA.

Se houver divergência entre fontes secundárias, documentar a decisão e escolher a formulação que:

- seja melhor suportada pela referência metodológica;
- reproduza os resultados oficiais;
- seja coerente com a curva CETESB.

Adicionar comentário/citação no código explicando a fonte.

---

# 14. COLIFORMES E E. COLI

O IQA tradicional utiliza coliformes termotolerantes.

O nome genérico:

```text
coliformes
```

pode ser ambíguo.

Preferir nome canônico:

```r
coliformes_termotolerantes
```

Manter aliases antigos apenas por compatibilidade.

A metodologia CETESB atual informa que resultados de **E. coli** podem ser convertidos para utilização na curva aplicando fator:

```r
1.25
```

quando explicitamente informado que o dado é E. coli.

### Não converter automaticamente um campo genérico

Adicionar, se implementado:

```r
microbial_type = c("thermotolerant_coliforms", "e_coli")
```

ou interface equivalente.

A transformação precisa ficar registrada no resultado/metadado.

---

# 15. NÃO PERMITIR “IQA OFICIAL PARCIAL” SILENCIOSAMENTE

A metodologia oficial utiliza nove componentes.

O comportamento:

```r
na_rm = TRUE
```

que remove variáveis ausentes e renormaliza pesos não pode ser apresentado como se fosse o mesmo IQA oficial.

### Comportamento recomendado

No modo:

```r
method = "CETESB"
```

o padrão deve ser estrito.

Se faltar qualquer componente obrigatório:

```text
erro claro
```

ou `IQA = NA` com motivo explícito, conforme a API escolhida.

Se for mantido um cálculo parcial, ele deve exigir opt-in explícito, por exemplo:

```r
allow_partial = TRUE
```

e o resultado deve informar:

```text
IQA_partial = TRUE
n_components_used
components_missing
```

Nunca renormalizar silenciosamente e chamar o resultado simplesmente de “IQA CETESB oficial”.

---

# 16. CLASSIFICAÇÃO DO IQA

A função atual:

```r
classify_iqa()
```

usa aproximadamente:

```text
25 / 50 / 70 / 90
```

Essas faixas correspondem a outro esquema e não devem ser o default CETESB.

## Default CETESB atual

Implementar:

```text
Péssima: IQA <= 19
Ruim:    19 < IQA <= 36
Regular: 36 < IQA <= 51
Boa:     51 < IQA <= 79
Ótima:   79 < IQA <= 100
```

Testar exatamente:

```text
19
19 + epsilon
36
36 + epsilon
51
51 + epsilon
79
79 + epsilon
100
```

## Compatibilidade

Se necessário manter o esquema antigo:

```r
scheme = "legacy"
```

mas o default deve ser:

```r
scheme = "cetesb"
```

Também é desejável permitir:

```r
breaks = ...
labels = ...
```

para aplicações de outras instituições/estados, desde que não comprometa a API.

---

# 17. BENCHMARK OBRIGATÓRIO CONTRA RESULTADOS OFICIAIS DO INEMA

Este é o teste de aceitação científica central.

Utilizar uma fixture reproduzível da:

```text
INEMA / SEIA
Campanha 3 de 2024
Rio Buranhém
```

Pontos e IQAs oficiais publicados:

| Ponto | IQA oficial |
|---|---:|
| FBS-BRH-250 | 77 |
| FBS-BRH-450 | 70 |
| FBS-BRH-300 | 72 |
| FBS-BRH-500 | 75 |

Uma implementação independente corrigida produziu aproximadamente:

| Ponto | Cálculo corrigido |
|---|---:|
| FBS-BRH-250 | 76.60 |
| FBS-BRH-450 | 69.50 |
| FBS-BRH-300 | 71.63 |
| FBS-BRH-500 | 75.08 |

Portanto, ao arredondar como no relatório oficial:

```text
77
70
72
75
```

## Tratamento de valores censurados para este benchmark

Para reproduzir o procedimento observado no relatório do INEMA, usar o **próprio limite reportado**:

```text
<3    -> 3
<1    -> 1
<0.02 -> 0.02
```

e não metade do limite.

Isso é específico da reprodução do procedimento oficial e deve ser explicitamente configurado.

### Critério de aprovação

O teste deve satisfazer simultaneamente:

```text
round(IQA_calculado) == IQA_INEMA
```

para os quatro pontos,

e preferencialmente:

```text
abs(IQA_calculado - IQA_referencia_continua) <= 0.6
```

onde a referência contínua são os valores aproximados apresentados acima.

### Regra de integridade

Não hardcode o IQA final.

A fixture precisa conter os **parâmetros brutos oficiais** e o teste deve chamar a API pública real do pacote.

---

# 18. CRIAR UMA CAMADA DE AUDITORIA DOS COMPONENTES DO IQA

Para uso científico, o pacote deve permitir saber **por que** um IQA resultou em determinado valor.

Adicionar uma opção como:

```r
details = TRUE
```

ou uma nova função:

```r
iqa_components()
```

que retorne, por observação:

```text
valor bruto
valor transformado
Qi
Wi
Qi^Wi
```

para cada um dos nove componentes.

Também registrar:

```text
método
versão/esquema
altitude
política de censura
hipótese de temperatura
base do fósforo
tipo microbiológico
número de componentes
```

Isso será extremamente útil para validação, artigo, suporte e futuras auditorias.

---

# 19. VALORES CENSURADOS: `<LD`, `<LOQ`, `ND`

A implementação atual converte valores censurados precocemente para um número e pode perder o qualificador original.

Isso reduz a rastreabilidade.

## Objetivo

Preservar informação como:

```text
<0.02
ND
<LD
<LOQ
```

e permitir política explícita.

Sugestão de políticas:

```text
preserve
limit
half_limit
zero
na
```

Evitar nomes pouco claros como `ld2`.

Se `ld2` for mantido por compatibilidade, depreciar gradualmente em favor de:

```text
half_limit
```

## Importante

Não descrever `half_limit` como universalmente “correto” ou “conservador”.

É uma regra de substituição, e a escolha depende do objetivo da análise.

Para o benchmark INEMA:

```text
policy = "limit"
```

é obrigatório.

## Arquitetura desejável

Ao ler dados, preservar também:

```text
censored
qualifier
reported_limit
raw_value
```

por meio de colunas auxiliares, objeto estruturado ou metadados.

Não destruir a informação original na etapa de importação.

---

# 20. `read_wq()` E `validate_wq()`

Atualizar candidatos numéricos e aliases para reconhecer:

```text
solidos_totais
residuo_total
coliformes_termotolerantes
e_coli
temperatura_agua
delta_temperatura
altitude
```

Atualizar `validate_wq()`.

O conjunto atualmente exigido contém `tds`; isso precisa mudar para o modo CETESB.

Não tornar `tds` sinônimo silencioso de `solidos_totais`.

---

# 21. REPARO AUTOMÁTICO DE pH

O código atual possui heurística semelhante a:

```text
72 -> 7.2
```

quando pH > 14.

Esse tipo de correção é plausível para erro de separador decimal, mas é uma **inferência**.

Para uso científico, evitar modificação silenciosa.

Sugestão:

```r
repair_ph = FALSE
```

por padrão em modo estrito,

ou manter a detecção com:

```text
warning + provenance
```

e exigir opt-in para correção.

Se o valor for alterado, registrar o valor original.

---

# 22. `clean_units()` — CORRIGIR DOCUMENTAÇÃO E SEMÂNTICA

Atualmente há inconsistência interna:

- a documentação diz em um ponto que existem conversões;
- em outro diz que “no actual conversions are performed”;
- o código efetivamente possui conversões.

Além disso, `units_map` é descrito como unidade de destino, porém a lógica parece tratá-lo como unidade de origem e converter para a unidade padrão.

Corrigir essa ambiguidade.

Preferir nomenclatura inequívoca, por exemplo:

```r
source_units = list(
    p_total = "ug/L",
    temperatura = "K"
)
```

convertendo para unidades padrão internas.

Se manter `units_map`, documentar de maneira exata o que o valor significa.

Adicionar testes de conversão numérica reais.

---

# 23. NSF WQI — CORREÇÃO HISTÓRICA E METODOLÓGICA

A versão 0.9.0 contém uma afirmação problemática:

```text
Brown et al. (1970) = weighted geometric mean
```

A literatura histórica distingue:

```text
Brown et al. 1970 -> formulação aditiva/aritmética ponderada original
McClelland / revisão posterior da NSF -> formulação multiplicativa/geometrica
```

Não atribuir a formulação geométrica simplesmente a Brown 1970 sem qualificação.

## Corrigir `nsfwqi()`

Arquivos:

```text
R/nsfwqi.R
man/nsfwqi.Rd
README
vignettes
NEWS
```

### API recomendada

Algo como:

```r
aggregation = c(
    "brown1970_arithmetic",
    "mcclelland_geometric"
)
```

ou nomenclatura equivalente cientificamente correta.

### Parâmetros NSF

Auditar os nove parâmetros originais e pesos.

O código atual usa:

```text
sst / solidos_suspensos
```

para o peso 0.07.

O NSF WQI tradicional usa **total solids**, não “suspended solids” como substituição automática.

Corrigir.

### Curvas Qi do NSF

O código atual usa `ifelse()` extremamente simplificado com poucos degraus.

Isso não deve ser descrito como reprodução fiel das curvas originais.

Implementar uma das opções:

1. curvas/equações devidamente referenciadas; ou
2. tabelas de interpolação suficientemente detalhadas e documentadas.

Manter versão aproximada somente se claramente rotulada como aproximada/legada.

### Não misturar CETESB e NSF

Deixar claro que:

```text
CETESB IQA
NSF WQI original
NSF WQI multiplicativo posterior
```

são implementações relacionadas historicamente, porém não idênticas.

---

# 24. CONAMA 357/2005 — ERRO NA REGRA DE FREQUÊNCIA

A função atual:

```r
conama_freq_check()
```

generaliza a ideia de:

```text
>=80% das amostras
>=6 amostras/ano
```

para todos os parâmetros.

Isso é metodologicamente incorreto.

Na Resolução CONAMA 357/2005, a regra de:

```text
80% ou mais de pelo menos 6 amostras no período de um ano
```

é explicitada em contextos específicos — por exemplo, critérios microbiológicos — e não como uma regra estatística universal para turbidez, OD, DBO, fósforo etc.

### Corrigir

A função deve saber **qual tipo de critério legal se aplica a cada parâmetro**.

---

# 25. REESTRUTURAR `conama_limits.csv`

Adicionar metadados suficientes para evitar interpretação por “qualquer linha que passar”.

Sugestão de campos:

```text
classe
parametro
unidade
min
max
criterion_type
environment
use_context
condition_parameter
condition_min
condition_max
frequency_threshold
minimum_samples
period
sampling_frequency
legal_basis
notes
```

Não é obrigatório usar exatamente esses nomes, mas a estrutura precisa representar as condições legais.

---

# 26. FÓSFORO TOTAL NA CONAMA — CONTEXTO HIDROLÓGICO

Hoje existem várias linhas para fósforo:

```text
lêntico
intermediário
lótico
```

O algoritmo atual pode marcar como conforme se **qualquer** uma das linhas for satisfeita.

Isso significa que pode escolher automaticamente o limite mais permissivo.

Isso é errado.

### Implementar contexto obrigatório

Por exemplo:

```r
environment = c(
    "lotic",
    "lentic",
    "intermediate"
)
```

ou coluna equivalente no dataset.

Sem contexto suficiente:

```text
não decidir silenciosamente
```

Retornar algo como:

```text
context_required
```

ou erro controlado.

---

# 27. AMÔNIA — LIMITE DEPENDENTE DO pH

Os limites de nitrogênio amoniacal variam conforme o pH.

O algoritmo não pode fazer:

```text
TRUE se atender qualquer uma das faixas
```

porque isso tende a selecionar o limite mais permissivo.

A linha aplicável deve ser escolhida com base no pH da própria amostra.

Criar testes cobrindo cada faixa de pH.

---

# 28. COLIFORMES CLASSE 3 — CONTEXTO DE USO

Há diferentes limites conforme o uso:

```text
recreação de contato secundário
dessedentação animal
demais usos
```

Não selecionar o limite máximo automaticamente.

Exigir `use_context` ou equivalente.

---

# 29. RECREAÇÃO DE CONTATO PRIMÁRIO

Para balneabilidade de contato primário, utilizar a lógica específica da:

```text
CONAMA 274/2000
```

e a função existente:

```r
balnear_check()
```

quando apropriado.

A análise de CONAMA 357 não deve fingir que o critério genérico de coliformes substitui a norma de balneabilidade.

---

# 30. TESTES CIENTÍFICOS — REFAZER A ESTRATÉGIA

Os testes atuais do IQA são insuficientes.

Exemplo de problema atual:

```text
testar apenas se IQA está entre 0 e 100
```

não demonstra que a fórmula esteja correta.

Também há teste com nomes de colunas que não correspondem aos componentes oficiais e uso de `na_rm=TRUE`, permitindo que um teste “passe” mesmo sem testar nove componentes.

## Criar testes de referência

Sugestão de arquivos:

```text
tests/testthat/test-iqa-cetesb-formulas.R
tests/testthat/test-iqa-cetesb-benchmark-inema.R
tests/testthat/test-iqa-classification.R
tests/testthat/test-iqa-missing-components.R
tests/testthat/test-iqa-censored.R
tests/testthat/test-nsfwqi-methods.R
tests/testthat/test-conama-context.R
tests/testthat/test-conama-frequency.R
tests/testthat/test-units.R
```

---

# 31. TESTES DE FRONTEIRA PARA TODOS OS `Qi`

Para cada função piecewise testar:

```text
limite - epsilon
limite
limite + epsilon
```

Verificar:

- valor esperado;
- ausência de branch errado;
- comportamento de continuidade quando matematicamente esperado;
- limites 0–100;
- `NA`;
- valores negativos fisicamente impossíveis.

Não use apenas snapshots.

Use expectativas numéricas derivadas da equação.

---

# 32. TESTES ESTRUTURAIS DO IQA

Adicionar testes que garantam:

### 32.1 Nove pesos

```r
expect_equal(sum(weights), 1)
expect_length(weights, 9)
```

### 32.2 Temperatura realmente entra no produtório

Alterar apenas `Qi_temperatura` deve alterar o IQA.

### 32.3 Temperatura da água e Qi da temperatura são coisas distintas

Alterar a temperatura da água deve poder afetar a saturação de OD, sem transformar automaticamente `29 °C` em um baixo `Qi_temperatura`.

### 32.4 TDS não deve produzir IQA oficial

Dataset com apenas `tds` e sem `solidos_totais` deve falhar em modo estrito.

### 32.5 Fósforo é convertido corretamente

Teste:

```r
P -> PO4 = P * 3.066
```

### 32.6 E. coli

Se o suporte for implementado:

```r
Ecoli_equivalent = Ecoli * 1.25
```

quando explicitamente selecionado.

---

# 33. TESTE DE REGRESSÃO ENTRE VERSÕES

Para documentar breaking change, calcular os mesmos exemplos com:

```text
0.9.0 legacy
0.10.0 corrected
```

e gerar tabela interna/documental.

Não exigir que os novos resultados permaneçam próximos dos antigos quando os antigos estavam metodologicamente errados.

O objetivo é documentar a mudança, não reproduzir o erro.

---

# 34. API RECOMENDADA PARA `iqa()`

Não é obrigatório seguir exatamente esta assinatura, mas a API deve representar explicitamente as decisões metodológicas.

Exemplo:

```r
iqa(
  df,
  method = "CETESB",
  classification = "cetesb",
  altitude_m = 0,
  phosphorus_basis = "P",
  microbial_type = "thermotolerant_coliforms",
  temperature_method = "cetesb_default",
  temperature_reference = NULL,
  delta_temperature = NULL,
  censor_policy = NULL,
  allow_partial = FALSE,
  details = FALSE
)
```

Se houver uma solução mais elegante e compatível com R/CRAN, pode ser adotada.

---

# 35. METADADOS NO RESULTADO

Registrar atributos ou colunas informativas, por exemplo:

```text
iqa_method
iqa_classification_scheme
iqa_complete
iqa_components_used
iqa_censor_policy
iqa_temperature_method
iqa_temperature_reference
iqa_delta_temperature
iqa_phosphorus_basis
iqa_microbial_type
```

Não precisa poluir o dataframe padrão; atributos ou objeto complementar são aceitáveis.

O objetivo é possibilitar reprodutibilidade.

---

# 36. DOCUMENTAÇÃO

Atualizar:

```text
DESCRIPTION
NEWS.md
README.md
README-pt.md (se existir)
CITATION.cff
inst/CITATION
man/
vignettes/
_pkgdown.yml
site pkgdown
```

### Remover informações desatualizadas

Foi observado que:

- CRAN está em 0.9.0;
- partes do site/README/citação ainda podem mencionar 0.8.0 ou 0.8.2.

Sincronizar tudo.

---

# 37. DOCUMENTAR BREAKING CHANGES

Recomendo versão:

```text
0.10.0
```

e não `0.9.1`, porque as mudanças alteram resultados científicos e semântica da API.

No `NEWS.md`, explicar claramente:

```text
- Corrected CETESB IQA parameter semantics.
- Replaced TDS with total solids for official CETESB IQA.
- Corrected Qi equations and interval definitions.
- Added phosphorus P -> PO4 conversion.
- Separated probe-measured water temperature from the temperature-deviation subindex; absolute water temperature is never interpreted as ΔT, and the documented CETESB default uses Qi_temperature = 94 when no valid thermal reference is available.
- Corrected CETESB IQA classification ranges.
- Added official INEMA benchmark tests.
- Corrected handling of contextual CONAMA criteria.
- Corrected historical attribution and aggregation options for NSF WQI.
- Improved censored-value provenance.
```

---

# 38. MIGRATION GUIDE

Criar uma seção:

```text
Migrating from 0.9.x to 0.10.0
```

Exemplos:

### Antes

```r
df$tds
iqa(df, na_rm = TRUE)
```

### Depois

```r
df$solidos_totais
iqa(df, method = "CETESB")
```

Explicar por que o valor pode mudar.

---

# 39. REFERÊNCIAS PRIORITÁRIAS

## Fonte metodológica CETESB

Apêndice oficial de metodologia:

```text
https://cetesb.sp.gov.br/aguas-interiores/wp-content/uploads/sites/12/2024/09/Apendice-D-Metodologia-de-Calculo-dos-Indices-de-Qualidade-das-Aguas-2023.pdf
```

Apêndice CETESB anterior:

```text
https://cetesb.sp.gov.br/aguasinteriores/wp-content/uploads/sites/12/2021/09/Apendice-E-Indice-de-Qualidade-das-Aguas.pdf
```

## ANA

```text
https://qualidadedaagua.ana.gov.br/iqa.html
```

## CONAMA 357/2005

Usar a versão oficial disponibilizada pelo CONAMA/MMA.

Não usar blogs como autoridade normativa.

## CRAN

```text
https://CRAN.R-project.org/package=tikatuwq
https://github.com/cran/tikatuwq
```

## Histórico NSF

Para distinguir agregação aditiva original de formulação multiplicativa posterior, consultar fontes históricas e revisões técnicas, incluindo relatório de agregação da EPA e literatura que distingue Brown et al. (1970) de McClelland/formulação posterior.

## Equações analíticas

Quando a CETESB apresentar apenas curvas gráficas e for necessário usar equações analíticas consolidadas, documentar claramente a fonte usada.

Não misturar equações de diferentes variantes sem validação.

O benchmark INEMA é a validação operacional final.

---

# 40. HIERARQUIA DE EVIDÊNCIA

Quando houver conflito entre fontes:

1. **texto legal oficial**, para CONAMA;
2. **metodologia CETESB oficial atual**, para definição do IQA;
3. **resultado oficial INEMA independente**, para benchmark;
4. documentação ANA;
5. literatura técnica revisada;
6. dissertações/teses que consolidam equações;
7. implementações de terceiros apenas como comparação.

Nunca escolher uma fórmula apenas porque “parece familiar”.

---

# 41. CONAMA — TESTES OBRIGATÓRIOS

Criar fixtures artificiais simples para demonstrar:

## Fósforo

O mesmo valor deve poder resultar em:

```text
conforme em ambiente lótico
não conforme em ambiente lêntico
```

quando os limites legais forem diferentes.

## NH3

O mesmo NH3 com pH diferente deve selecionar limites diferentes.

## Coliformes classe 3

O mesmo resultado microbiológico deve ser avaliado de acordo com o uso informado.

## Frequência

Uma série de turbidez não deve receber automaticamente a regra legal de 80%/6 como se essa regra fosse universal.

Criar teste que evite regressão desse erro.

---

# 42. REVISÃO DO `DESCRIPTION`

A descrição atual afirma, de modo amplo, que o pacote implementa:

```text
the legal frequency rule (Art. 15, 80% conformity over six or more samples per year)
```

Reescrever para não sugerir que essa regra vale universalmente para todos os parâmetros.

A descrição precisa refletir exatamente o comportamento implementado.

---

# 43. REVISÃO DO `nsfwqi()` NO `DESCRIPTION`

Não declarar:

```text
Brown (1970) + geometric aggregation
```

sem distinção histórica.

Se forem implementadas duas variantes, descrevê-las claramente.

---

# 44. DOCUMENTAÇÃO DAS LIMITAÇÕES

Adicionar seção explícita:

```text
Methodological notes and limitations
```

Explicar:

- IQA não substitui avaliação individual de parâmetros;
- IQA não é equivalente à conformidade CONAMA;
- um valor bom de IQA pode coexistir com violações regulatórias;
- resultados parciais não são IQA CETESB completo;
- política de censura pode alterar o índice;
- classificações podem variar entre instituições/esquemas;
- TDS não é sólidos totais;
- NSF WQI e CETESB IQA não devem ser tratados como sinônimos.

Isso é particularmente importante para o artigo científico que utilizará a biblioteca.

---

# 45. TESTES DO PACKAGE

Executar no mínimo:

```r
devtools::document()
testthat::test_local()
devtools::check()
```

e:

```bash
R CMD build .
R CMD check --as-cran tikatuwq_*.tar.gz
```

Meta:

```text
0 errors
0 warnings
0 notes
```

Quando alguma NOTE for inevitável por ambiente, explicar precisamente; não esconder.

---

# 46. VERIFICAR EXEMPLOS E VIGNETTES

Todas as chamadas antigas de:

```r
iqa()
```

precisam ser revisadas.

Especial atenção a exemplos que ainda usam:

```text
tds
```

como componente oficial.

Atualizar dataset `wq_demo` ou criar campos adequados se ele não tiver `solidos_totais`.

Não fabricar sólidos totais a partir de TDS apenas para fazer o exemplo funcionar.

Se o dataset não contiver os nove componentes oficiais:

- usar outro fixture apropriado; ou
- demonstrar explicitamente o modo parcial, rotulando-o como parcial.

---

# 47. `wq_demo`

Auditar se `wq_demo` realmente contém os campos necessários e se seus nomes/unidades estão corretos.

Se ele tiver:

```text
tds
```

mas não:

```text
solidos_totais
```

não dizer que é um exemplo completo do IQA CETESB.

Considerar criar um pequeno dataset de validação separado, por exemplo:

```text
wq_iqa_validation
```

com os quatro registros INEMA de 2024, caso a licença/origem permita redistribuição.

Se não for apropriado incluir dados oficiais diretamente no pacote, manter fixture mínima de testes em `tests/testthat/fixtures/` com fonte e rastreabilidade.

---

# 48. PROVENIÊNCIA DOS DADOS DE BENCHMARK

A fixture INEMA deve conter metadados suficientes:

```text
source
report/campaign
year
river
point
official_iqa
censor_policy_for_reproduction
```

Não depender de rede durante os testes do CRAN.

A fixture deve estar local no pacote.

---

# 49. NÃO PUBLICAR AUTOMATICAMENTE

O agente pode:

- criar branch;
- editar arquivos;
- executar testes;
- criar commits locais;
- gerar relatório de diff.

Mas:

> **não fazer merge no `main`, não criar release pública, não submeter ao CRAN e não alterar Zenodo sem aprovação explícita do responsável pelo pacote.**

---

# 50. ENTREGÁVEIS ESPERADOS DO AGENTE

Ao terminar, apresentar obrigatoriamente:

## 50.1 Auditoria inicial

Tabela:

```text
arquivo
problema
severidade
impacto científico
correção proposta
```

## 50.2 Arquivos alterados

Listar todos.

## 50.3 Mudanças metodológicas

Explicar de forma simples e técnica.

## 50.4 Benchmark INEMA

Tabela obrigatória:

| Ponto | IQA INEMA | tikatuwq 0.9.0 | nova versão | diferença nova |
|---|---:|---:|---:|---:|

Usar, para comparação da versão antiga, aproximadamente os valores já observados:

| Ponto | INEMA | 0.9.0 atual observado |
|---|---:|---:|
| FBS-BRH-250 | 77 | ~84.1 |
| FBS-BRH-450 | 70 | ~79.3 |
| FBS-BRH-300 | 72 | ~81.6 |
| FBS-BRH-500 | 75 | ~80.7 |

Recalcular esses valores diretamente no ambiente ao invés de confiar cegamente nesta tabela.

## 50.5 Resultado dos testes

Informar:

```text
número de testes
passes
fails
skips
R CMD check
```

## 50.6 Breaking changes

Lista objetiva.

## 50.7 Migration guide

Código antes/depois.

## 50.8 Release notes

Texto pronto para `NEWS.md`.

## 50.9 Pendências

Se algum ponto metodológico permanecer ambíguo, não esconder.

Listar:

```text
questão
fontes conflitantes
decisão temporária
o que falta para fechar
```

---

# 51. CRITÉRIOS DE ACEITAÇÃO FINAL

A atualização só será considerada pronta quando TODOS os critérios abaixo forem atendidos:

- [ ] O método `CETESB` usa produtório geométrico ponderado.
- [ ] Os nove componentes oficiais entram efetivamente no cálculo.
- [ ] `tds` não é usado como substituto de sólidos totais no método oficial.
- [ ] `solidos_totais` possui curva correta e testada.
- [ ] A turbidez utiliza os intervalos corretos.
- [ ] DBO/N/P utilizam as bases exponenciais corretas.
- [ ] Fósforo é convertido de P para PO4 quando aplicável.
- [ ] `temperatura_agua` representa apenas a temperatura absoluta medida pela sonda.
- [ ] A biblioteca não infere `delta_temperatura` a partir de uma única leitura de `temperatura_agua`.
- [ ] Na ausência de referência térmica/ΔT, o modo CETESB documentado utiliza `Qi_temperatura = 94`.
- [ ] Quando uma referência térmica válida é fornecida, `delta_temperatura` é calculado explicitamente e registrado.
- [ ] Alterar somente `temperatura_agua` pode alterar `Qi_OD`, mas não `Qi_temperatura` no modo `cetesb_default`.
- [ ] O componente de temperatura não desaparece do produtório.
- [ ] OD é convertido para % de saturação.
- [ ] A classificação CETESB default é 19/36/51/79.
- [ ] O benchmark INEMA arredonda para 77/70/72/75.
- [ ] Política de valores censurados é explícita e auditável.
- [ ] Modo parcial, se existir, não é apresentado como IQA oficial completo.
- [ ] `nsfwqi()` distingue corretamente as variantes históricas de agregação.
- [ ] NSF WQI não usa sólidos suspensos como substituto silencioso de total solids.
- [ ] `conama_freq_check()` não aplica 80%/6 universalmente.
- [ ] Fósforo CONAMA usa contexto lótico/lêntico/intermediário.
- [ ] NH3 seleciona limite conforme pH.
- [ ] Coliformes classe 3 usa contexto de uso.
- [ ] Balneabilidade primária é tratada pela CONAMA 274/2000.
- [ ] `clean_units()` tem semântica e documentação consistentes.
- [ ] README, vignettes, man pages, NEWS e DESCRIPTION estão sincronizados.
- [ ] Site pkgdown não exibe versão/citação obsoleta.
- [ ] Testes científicos cobrem valores de referência e fronteiras.
- [ ] `R CMD check --as-cran` passa sem erros/warnings.
- [ ] Nenhuma publicação/release/CRAN é feita sem aprovação final.

---

# 52. RESULTADO CIENTÍFICO DESEJADO

Após a atualização, precisamos poder afirmar de forma defensável em artigo:

> The computational implementation of the CETESB Water Quality Index was independently benchmarked against officially published INEMA values. Four Buranhém River site-campaign calculations were reproduced within approximately 0.5 WQI units, with identical integer classifications after applying the documented censoring convention.

A biblioteca deve ser capaz de sustentar essa frase por meio de:

```text
código
fixture
teste automatizado
documentação
proveniência
```

---

# 53. PRINCÍPIO FINAL

Não otimize para preservar resultados antigos.

Otimize para:

```text
correção científica
rastreabilidade
reprodutibilidade
clareza metodológica
compatibilidade futura
qualidade CRAN
```

Se durante a implementação surgir qualquer evidência de que uma correção proposta neste prompt está errada, **não a aplique cegamente**.

Faça a verificação documental, mostre a evidência e proponha a correção metodologicamente mais defensável.

O pacote deve sair desta atualização não apenas “funcionando”, mas **validado como ferramenta científica**.
