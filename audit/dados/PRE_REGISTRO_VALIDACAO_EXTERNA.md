# Pré-registro: validação externa do motor IGAM (k = 1,5)

- **Registrado em:** 2026-10-15
- **Momento:** depois de baixar os arquivos e **antes** de extrair as entradas
  ou calcular qualquer IQA destes conjuntos.
- **Regra:** o motor não é alterado depois de ver estes resultados.

## Conjuntos

- **E1:** Pampulha, 3º trimestre de 2018.
  - IQA publicado: IGAM, relatório trimestral (handle 123456789/2927;
    SHA-1 `34df8cdc7f3708aa6ce7ad3dd3988d7d48ec0276`).
  - Entradas: Séries Históricas, 3º trimestre de 2018 (handle
    123456789/3049; SHA-1 `793a017800a76fa77ec63f103acce4faefd04576`).
- **E2:** Pampulha, 4º trimestre de 2017.
  - IQA publicado: IGAM, relatório trimestral (handle 123456789/2365;
    SHA-1 `700c8d91288131dba0f6e37ca44e95375552abc6`).
  - Entradas: Séries Históricas 2017, `2017.xlsx` (handle 123456789/3077;
    SHA-1 `b205ea95ec278a6879a9a1239d07fd99b009ae61`), registros do
    4º trimestre.

Os dois conjuntos são de campanhas diferentes da campanha usada na
investigação (3º trimestre de 2017). São da **mesma sub-bacia**, portanto não
são validação em outra região.

## Inclusão e exclusão (fixadas agora)

- **Inclusão:** toda estação com IQA numérico publicado para a campanha no
  relatório e com exatamente um registro da mesma estação no trimestre
  correspondente da planilha.
- **Exclusão:** só nestes casos, sempre listados com o motivo:
  1. sem IQA publicado ou valor ilegível;
  2. registro ausente ou ambíguo (mais de um no trimestre);
  3. altitude da estação desconhecida.
- **Parâmetros:**
  - um parâmetro não obrigatório ausente segue a regra de redistribuição do
    IGAM (a amostra **não** é excluída);
  - falta de OD, microbiológico, temperatura ou cloreto torna a amostra
    "não calculável pelo motor". Ela é reportada, não descartada.
- **Altitude:** coluna "Altitude" da planilha da campanha. Se ela não existir,
  usa-se a planilha do 4º trimestre de 2018 (metadado da estação), como na
  investigação.

## Configuração fixa do motor

- Turbidez com k = 1,5.
- Nitrato em mg/L N, convertido para NO₃ (× 62,004/14,007).
- Fósforo total P → PO₄ (× 94,9714/30,9738).
- qs de temperatura = 92.
- *E. coli* sem fator.
- Censura no limite (`"limit"`).
- Cs do SCQA com cloreto, temperatura e altitude.

## Métricas e critérios

- **Métricas:**
  - n incluído, n calculável;
  - n com reprodução exata (|IQA − publicado| < 0,05 e mesmo valor
    arredondado a 1 casa);
  - MAE, RMSE, erro médio com sinal e erro máximo.
- **Estratos:** turbidez ≤ 100 contra > 100; presença de dado censurado.
- **Reprodução exata:** valor arredondado a 1 casa igual ao publicado.
- **Critério de aproximação (declarado como tal):** |erro| ≤ 0,15.
- **Tolerâncias:** nenhuma é ampliada depois.
- **Tipos de falha:** cada desvio acima de 0,05 é classificado como falha de
  fórmula, dado incompatível, censura ou falta de informação, quando a causa
  puder ser identificada. Sem causa identificada, fica "não explicado".
