# Fontes dos dados de referência usados na validação IGAM

- **O que não está aqui:** os dados por amostra (extratos das planilhas, IQA
  publicado por estação, altitudes) **não são versionados**. Estão no arquivo
  de pesquisa privado (ver `audit/DADOS_IGAM_PRIVADOS.md`).
- **O que está aqui:** só as fontes e os hashes, para quem quiser obter e
  conferir os originais.
- **Acesso a todas as fontes:** 2026-10-15 e 2026-10-16. Repositório
  Institucional do IGAM: https://repositorioigam.meioambiente.mg.gov.br

| Conjunto | Entradas (planilha *Séries Históricas*, aba "São Francisco") | IQA publicado (relatório trimestral da sub-bacia do ribeirão Pampulha) |
|---|---|---|
| 3º trim. 2017 (15 casos da investigação) | `2017.xlsx`, handle 123456789/3077, SHA-1 `b205ea95ec278a6879a9a1239d07fd99b009ae61` | handle 123456789/2370, Tabela 7, SHA-1 `5bb7f7bd9f383b12e00e8a332838663cb4226fb7` (download de 2026-10-16) |
| 3º trim. 2018 (17 casos, pré-registrados) | `Dados_monitoramento 2018_3° trimestre (1).xlsx`, handle 123456789/3049, SHA-1 `793a017800a76fa77ec63f103acce4faefd04576` | handle 123456789/2927, tabela-síntese (coluna 2018), SHA-1 `34df8cdc7f3708aa6ce7ad3dd3988d7d48ec0276` |
| 4º trim. 2017 (17 casos, pré-registrados) | `2017.xlsx` (acima), registros de out–dez/2017 | handle 123456789/2365, Tabela 7 (coluna 2017), SHA-1 `700c8d91288131dba0f6e37ca44e95375552abc6` |
| Altitude das estações | `Dados_monitoramento 2018_4° trimestre (2).xlsx`, handle 123456789/3050, coluna "Altitude", SHA-1 `f0b165ff6b62b3b2976d210099e90fa9f6c9bf53` | — |

**Outras fontes:**

- **SCQA 2005 (cópia oficial):** `http://aguas.igam.mg.gov.br/aguas/downloads/SCQA_final.pdf`,
  no Internet Archive. SHA-1 (base32) `YJCHBS7MGLSITXEUNOAUG2I45OE635LK`
  (snapshot `20070310185501`) e `UJNWIAB3MREESMFFR77S2ARYGQURWTMH`
  (`20070804014027`).
- **Unidades:** relatório IGAM 2017, Anexo I (nitrato em mg/L N; cloreto em
  mg/L Cl; turbidez em NTU).
- **Critérios de seleção da validação externa:**
  `PRE_REGISTRO_VALIDACAO_EXTERNA.md`.

São dados públicos de órgão estadual, usados para verificação científica e
citados com a fonte. O repositório do IGAM não declara licença nos itens
usados.
