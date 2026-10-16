# Validação do método IGAM — tikatuwq 0.11.0

> **6ª rodada (2026-10-16):** método IGAM **liberado** em `iqa()`. Ver
> `IGAM_RELEASE_READINESS.md`.
>
> **5ª rodada (2026-10-15):** este documento registra o estado da 4ª rodada.
> - **Novo estado:** motor IGAM **implementado em caráter experimental**
>   (`implemented = TRUE`) e **não liberado** (`available = FALSE`,
>   `reference_validated = FALSE`).
> - **Ver:** `RELATORIO_IGAM_IMPLEMENTACAO.md`,
>   `IGAM_VALIDACAO_TURBIDEZ_1_5.md` e `IGAM_METODOLOGIA_E_UNIDADES.md`.

- **Data:** 2026-10-15 (4ª rodada)
- **Estado final:** **BLOQUEADO.** `iqa(df, method = "IGAM")` e
  `iqa_components(df, method = "IGAM")` geram erro de classe
  `tikatuwq_method_unavailable`.
- **`iqa_methods()`:** IGAM com `implemented = FALSE`, `documented = TRUE`,
  `reference_validated = FALSE` e `available = FALSE`.
- **Fontes e equações:** `SCQA_FONTES_E_EQUACOES.md`.
- **Validação externa:** `IGAM_VALIDACAO_REFERENCIAS.csv`.
- **Script reproduzível:** `audit/igam_scqa_validation.R`, com dados em
  `audit/dados/`.

## 1. O que mudou desde a 3ª rodada

- **O SCQA 2005 foi obtido e autenticado.** O mantenedor forneceu uma
  reprodução, e a cópia oficial que estava no servidor do IGAM foi recuperada
  do Internet Archive, com SHA-1 conferido, em duas versões de 2007.
- **As 9 equações foram transcritas duas vezes** e checadas contra as figuras
  do próprio documento.
- **Foram encontrados 15 pares oficiais:** entradas da série histórica do IGAM
  e o IQA publicado pelo IGAM (Pampulha, 3º trimestre de 2017).

## 2. Critério de desbloqueio e situação

| Requisito | Situação |
|---|---|
| Evidência rastreável para todas as equações | 8 de 9: sim. **Turbidez: não.** A equação impressa é refutada pela figura do próprio documento e pelos resultados publicados |
| Unidades verificadas | Fosfato: o IGAM usa P × 3,066 (empírico). Nitrato: **não resolvido** (SCQA em NO₃; dados em N; os dados não discriminam) |
| Temperatura | **Não resolvido:** 93 (SCQA 2005) × 92 (portal atual). As duas são declarações institucionais de datas diferentes |
| Dados ausentes | Portal: OD e coliformes obrigatórios; peso dos demais repartido igualmente. Com vários ausentes, é **interpretação** |
| Referências independentes reproduzidas | 2 de 15 dentro do arredondamento (ambas com turbidez > 100 UNT). Nas outras 13 a equação de turbidez influi e os erros vão até 1,5 ponto |

**Conclusão:** os requisitos essenciais não foram cumpridos (turbidez, nitrato,
temperatura). O motor não foi incorporado ao pacote. A transcrição fica só no
script de auditoria, para reprodução e para o próximo passo.

## 3. Decisão sobre a temperatura

O SCQA 2005 (p. 12) dá qs = 93, com a justificativa de que ΔT ≅ 0. O portal
Infohidro/IGAM atual dá 92 "no âmbito do Projeto Águas de Minas". São
documentos institucionais de épocas diferentes, e nenhum explica a mudança.
**Não se escolhe um dos valores:** os dois ficam em
`.iqa_igam_spec$temperature_qs`. Os 15 pares não distinguem os valores, porque
o efeito no IQA (fator (93/92)^0,1 ≈ 1,001) é menor que o arredondamento
publicado.

## 4. Dados ausentes, limites e domínio

- **Sem OD ou microbiológico:** o IQA não é calculado (portal IGAM).
- **Outro parâmetro ausente:** o peso é repartido igualmente entre os
  presentes, conforme o portal; para vários ausentes a repartição da soma é
  interpretação. Implementado e testado em `.igam_effective_weights()`.
- **Classes** (SCQA, Tabela 2.2; IGAM 2017): Muito Ruim 0 ≤ IQA ≤ 25; Ruim
  (25, 50]; Médio (50, 70]; Bom (70, 90]; Excelente (90, 100]. Implementadas em
  `classify_iqa(classification = "igam")`.
- **Não definidos pelo SCQA:** CF < 1 NMP/100 mL; cloreto ausente; censura;
  negativos. A validação usou o limite como valor para os censurados.

## 5. Próximos passos para desbloquear

1. Obter do IGAM a equação de turbidez efetivamente usada: código do SCQA/Java,
   planilha de cálculo atual ou errata.
2. Confirmar com o IGAM o qs de temperatura vigente e a conversão do nitrato.
3. Ampliar a validação com amostras de nitrato acima do limite de detecção e
   turbidez em 5–100 UNT de outros trimestres ou bacias. O script aceita novos
   pares sem alteração.
4. Só então incorporar as equações ao pacote, repetir a validação e ligar
   `available = TRUE`.
