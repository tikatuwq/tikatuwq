# Proposta: índices de qualidade definidos pelo pesquisador

- **Status:** proposta para discussão. **Não implementada.**
- **Data:** 2026-10-15 · tikatuwq 0.11.0
- **Aviso de local:** `docs/` é a pasta de saída do pkgdown. Um
  `pkgdown::clean_site()` pode apagar este arquivo. Se a proposta for aceita,
  movê-la para `inst/design/` ou para uma issue.

## 1. Problema

Pesquisadores querem calcular índices que o pacote não traz (variantes
estaduais, índices de bacia, versões experimentais), com a mesma
rastreabilidade do IQA CETESB. Hoje só existe `iqa(pesos = ...)`, que troca os
pesos e mantém as curvas CETESB. Isso não é um índice arbitrário e marca o
resultado como `custom_weights` em `iqa_nonstandard`.

## 2. Requisitos

1. Um índice é definido por **dados declarativos** (não por código vindo de
   fora do processo R do usuário) e pode ser salvo e compartilhado.
2. Cada definição tem identificador, versão, autor e fonte, e os resultados
   carregam essa proveniência.
3. Índices do usuário nunca se passam por oficiais: ficam num namespace
   separado (`user:<id>`) e são marcados como tal na saída.
4. Validação forte na definição: unidades, domínios, pesos (soma, sinal),
   monotonicidade declarada e cobertura total do domínio pelos trechos.
5. Reprodutibilidade: um hash da definição canônica fica junto do resultado.

## 3. API proposta

```r
def <- wqi_definition(
  id = "meu_iqa_bacia_x", version = "1.0.0",
  title = "IQA adaptado para a bacia X", authors = "Fulano (2026)",
  source = "doi:10.xxxx/yyyy",
  parameters = list(
    od  = wqi_param(unit = "%sat", weight = 0.2,
                    qi = wqi_piecewise(breaks = c(0, 50, 85, 100, 140, Inf),
                                       forms = list(poly(c(3, 0.34, 0.008095, 1.35252e-5)), ...),
                                       right = TRUE)),
    dbo = wqi_param(unit = "mg/L", weight = 0.1, qi = wqi_table(x = c(...), y = c(...),
                                                               interpolation = "linear")),
    ...
  ),
  aggregation = "weighted_geometric",       # ou "weighted_arithmetic"
  missing = wqi_missing(policy = "none" | "proportional" | "equal_redistribution",
                        mandatory = c("od")),
  classes = wqi_classes(breaks = c(0, 25, 50, 70, 90, 100), labels = c(...), right = TRUE)
)
validate_wqi(def)                 # erros descritivos; nao calcula nada
register_wqi(def)                 # registro por sessao (ambiente interno)
calculate_wqi(df, "user:meu_iqa_bacia_x", details = TRUE)
write_wqi(def, "meu_iqa.json"); def2 <- read_wqi("meu_iqa.json")
```

### Formas de qi permitidas (fechadas, sem `eval`)

`const(v)`, `poly(coefs)`, `exp_form(a, b)` (a·e^(b·x)), `power(a, b)` (a·x^b),
`log_form(a, b, base = c("e", "10"))` (a + b·log x), `table` com interpolação
linear e extrapolação proibida ou constante declarada. A **base do logaritmo é
obrigatória e explícita**, a lição do erro log10 × ln no IQA CETESB.

## 4. Segurança

- Nada de `eval(parse())`, fórmulas em texto livre ou funções R desserializadas
  de arquivo. Só os construtores fechados da seção 3.
- Leitura de JSON ou YAML com esquema validado. Campos desconhecidos geram
  erro, não são ignorados.
- Sem download automático de definições a partir de URLs.

## 5. Isolamento dos índices oficiais

- `iqa()` continua só com os motores do pacote (CETESB; IGAM quando
  desbloqueado). `calculate_wqi()` nunca aceita `"CETESB"` ou `"IGAM"`.
- A saída de `calculate_wqi()` tem `attr(, "wqi_origin") = "user"`, o hash da
  definição e `iqa_nonstandard = "user_defined_index"`.
- `compare_iqa()` só aceitaria índices do usuário com `allow_user = TRUE`,
  marcados nos nomes das colunas.

## 6. Alternativas mais simples

| Alternativa | Prós | Contras |
|---|---|---|
| A. Não implementar; documentar como montar um índice com `iqa_components()` | Custo zero | Sem validação nem proveniência |
| B. Só tabelas (x, qi) com interpolação linear | Simples, seguro, serializável | Não representa equações publicadas exatamente |
| C. Proposta completa (seção 3) | Cobre equações publicadas e tabelas | Maior custo de manutenção e de testes |

**Recomendação:** começar por B, desenhada para evoluir para C.

## 7. Riscos e custos

- Usuários podem publicar resultados de índices mal definidos como se fossem
  oficiais. Mitigação: marcação obrigatória na saída e no `print`.
- Manutenção do esquema e dos validadores; compatibilidade entre versões do
  formato (campo `schema_version`).
- Testes de propriedade (cobertura de domínio, monotonicidade declarada) para
  cada construtor.

## 8. Critérios para implementar

1. Pelo menos dois casos de uso reais documentados (por exemplo, o IGAM, se o
   SCQA continuar indisponível como motor oficial, ou um índice de bacia
   publicado).
2. Motores oficiais (CETESB e IGAM) estáveis e validados antes.
3. Esquema revisado por um segundo mantenedor.
4. Sem novas dependências obrigatórias (JSON via `jsonlite` em Suggests).
