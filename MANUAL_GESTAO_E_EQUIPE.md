# Manual Completo de Gestão Editorial, Dados e Automação
## Jornal Econômico do Tocantins

**Editor-Chefe:** Lorenzo Costa  
**Arquitetura do Sistema:** R (`sidrar`, `readxl`, `jsonlite`) + Quarto Dashboard + LaTeX (MiKTeX) + GitHub Actions + Substack

---

## Sumário Executivo: Como a Arquitetura Funciona

O **Jornal Econômico do Tocantins** opera com uma arquitetura de **Fonte Única de Verdade (Single Source of Truth)** dividida em duas camadas dentro do script [`scripts/atualizar_dados.R`](scripts/atualizar_dados.R):

1. **Camada 1 — Extração Centralizada de Pesquisas Oficiais (Topo do `atualizar_dados.R`, antes da Aba 1):**
   Extrai todas as tabelas e séries históricas completas (tanto os indicadores que aparecem no painel quanto o grande volume de dados extras que não cabem no painel, mas que abastecem os **Boletins Flash**, os **Dossiês Anuais** e as **Cartas de Conjuntura**).
2. **Camada 2 — Montagem das Listas por Aba do Painel (`# --- ABA 1: VISÃO GERAL ---` em diante):**
   Consome os objetos já extraídos na Camada 1 para alimentar automaticamente o painel online (`index.qmd` $\rightarrow$ `index.html`), enquanto todas as tabelas analíticas completas ficam salvas na lista `pesquisas_extras` dentro de `dados/painel_dados.rds`.

```
┌─────────────────────────────────────────────────────────────────────────┐
│ PASSO 1: EXTRAÇÃO CENTRALIZADA DE PESQUISAS (scripts/atualizar_dados.R) │
│ • Planilha SEPLAN-TO: dados/brutos/pib_trimestral_seplan.xlsx (8 abas)  │
│ • API IBGE/SIDRA (sidrar): PIB Brasil (1621, 1846, 5932)                │
│ • API IBGE/SIDRA (sidrar): PNAD Contínua (4099, 6374, 5436)             │
│ • API Banco Central (SGS/BCB): Selic Meta (432) e IPCA 12m (13522)      │
└───────────────────────────────────┬─────────────────────────────────────┘
                                    │ gera automaticamente
                                    ▼
                     ┌──────────────────────────────┐
                     │    dados/painel_dados.rds    │
                     │  • Listas das Abas do Painel │
                     │  • Lista 'pesquisas_extras'  │
                     └──────────────┬───────────────┘
                                    │
          ┌─────────────────────────┴─────────────────────────┐
          ▼                                                   ▼
┌───────────────────────────────────┐     ┌───────────────────────────────────────────┐
│ PAINEL DO JORNAL (index.qmd)      │     │ MOTOR EDITORIAL (scripts/compilar_boletins.R) │
│ • Atualiza todos os ValueBoxes    │     │ • Lê 'painel_dados.rds' + 'pesquisas_extras'│
│ • Ordena 1º/2º/3º Setor PIB (TO)  │     │ • Escreve a "Análise Robótica" sozinha    │
│ • Exibe avisos de ajuste sazonal  │     │ • Desenha gráficos (PDF + PNG) e tabelas  │
└───────────────────────────────────┘     └─────────────────────┬─────────────────────┘
                                                                ▲
                                                                │ junta com
                                          ┌─────────────────────┴─────────────────────┐
                                          │ ANÁLISE HUMANA DA EQUIPE                  │
                                          │ (boletins/textos_equipe/*.txt)            │
                                          │ • Texto em português puro (sem código!)   │
                                          └─────────────────────┬─────────────────────┘
                                                                │ compila via MiKTeX
                                                                ▼
                                          ┌───────────────────────────────────────────┐
                                          │ SAÍDA FINAL AUTOMÁTICA                    │
                                          │ • Boletins Flash (Site HTML + PDF)        │
                                          │ • Dossiês Anuais (Site HTML + PDF)        │
                                          │ • Cartas de Conjuntura (PDF p/ Assinantes)│
                                          └───────────────────────────────────────────┘
```

---

## PARTE 1: Estrutura Padronizada de Temas e Publicações do Jornal

As publicações periódicas seguem uma divisão clara entre três produtos editoriais:

### 1. Boletins Flash (Mensais / Trimestrais) e Dossiês Anuais (Compilado Anual)
Possuem **8 divisões temáticas padronizadas** que espelham as abas do jornal, permitindo ao leitor acompanhar tanto o pulso mensal/trimestral (**Boletim Flash**, 1 coluna, Azul Marinho `#0F2537`, público no site em HTML + PDF) quanto o consolidado de fechamento do ano (**Dossiê Anual**, 2 colunas estilo revista, Bordô Imperial `#4A1525`, público no site em HTML + PDF):

| Nº | Tema Padronizado | Código `TIPO:` (Boletim Flash) | Código `TIPO:` (Dossiê Anual) |
| :-: | :--- | :--- | :--- |
| **1** | **Panorama Econômico do Tocantins** *(Overview Geral / Visão Geral em documento único)* | `flash_panorama` | `dossie_panorama` |
| **2** | **PIB (TO)** | `flash_pib` | `dossie_pib` |
| **3** | **Mercado de Trabalho** | `flash_trabalho` *(ou `flash_caged`)* | `dossie_trabalho` *(ou `dossie_emprego`)* |
| **4** | **Comércio Exterior** | `flash_comex` | `dossie_comex` *(ou `fechamento_comex`)* |
| **5** | **Arrecadação e Finanças** | `flash_financas` *(ou `flash_icms`)* | `dossie_financas` |
| **6** | **Dinâmica Empresarial** | `flash_empresas` | `dossie_empresas` |
| **7** | **Câmbio e Mercados** | `flash_mercados` | `dossie_mercados` |
| **8** | **IPER-TO** | `flash_iper` | `dossie_iper` |

### 2. Cartas de Conjuntura ("Coringas" — Exclusivas para Assinantes)
- **Código:** `TIPO: carta_conjuntura` (ou qualquer variação `carta_*`).
- **Formato:** Periódico em **2 colunas estilo revista**, paleta **Verde-Petróleo (`#12352B`) & Dourado (`#C8942B`)**.
- **Regra Editorial:** As Cartas de Conjuntura **não possuem categorias fixas separadas** — são publicações **"coringas"** sobre a conjuntura e estrutura econômica do Tocantins, lançadas a qualquer momento à medida que saem pesquisas conjunturais ou estruturais mais extensas do IBGE (como **PAM** — Produção Agrícola Municipal, **PPM** — Pesquisa da Pecuária Municipal, **PEVS** — Extração Vegetal e Silvicultura, Contas Regionais, etc.).
- **Acesso Restrito:** O PDF é gerado automaticamente em `boletins/publicados/` para envio exclusivo aos assinantes por e-mail (Substack), **não aparecendo no Giro de Boletins nem aberto no site**.

---

## PARTE 2: Mapeamento das Extrações de Dados em `scripts/atualizar_dados.R`

Toda a captação de dados das pesquisas oficiais ocorre na **Seção 1 do `scripts/atualizar_dados.R`**, antes do início da montagem das listas das abas (`# --- ABA 1: VISÃO GERAL ---`).

### Pesquisa 1: PIB Trimestral do Tocantins (SEPLAN-TO — Planilha Excel Local + Dessazonalização X-13ARIMA-SEATS)
Como a SEPLAN-TO não possui API para o PIB Trimestral e disponibiliza os resultados em arquivo Excel (`.xlsx`), o sistema foi programado para reconhecer automaticamente toda a estrutura da planilha e executar em seguida a dessazonalização econométrica:

- **Onde colocar a planilha nova da SEPLAN-TO:**
  - **Local oficial:** Coloque na pasta **`dados/brutos/`** com o nome **`dados/brutos/pib_trimestral_seplan.xlsx`**.
  - **Detecção automática inteligente:** Mesmo que você baixe o arquivo do site da SEPLAN-TO mantendo o nome original longo (ex: `tabelas_completas_das_contas_trimestrais_tocantins_...xlsx`) e o solte em **`dados/brutos/`** (ou até mesmo em **`scripts/`**), a função `sincronizar_planilha_pib_seplan()` detecta o arquivo mais recente e o copia/sincroniza automaticamente para `dados/brutos/pib_trimestral_seplan.xlsx`!
- **Script Oficial de Dessazonalização (`scripts/dessazonalizacao_pib.R`):**
  - Chamado automaticamente dentro de `scripts/atualizar_dados.R` toda vez que o jornal é atualizado.
  - Lê a aba **`"Série Encadeada"`** de `dados/brutos/pib_trimestral_seplan.xlsx` e aplica o método **X-13ARIMA-SEATS** (`seasonal::seas` com `transform.function = "auto"`, `regression.aictest = c("td", "easter")`, detecção de outliers e busca em grid de melhor especificação ARIMA `encontrar_melhor_arima` para a Indústria) sobre as **6 colunas**:
    1. **`Agropecuária`** (Valor Adicionado)
    2. **`Indústria`** (Valor Adicionado)
    3. **`Serviços`** (Valor Adicionado)
    4. **`VA Total`** (Valor Adicionado Total)
    5. **`Impostos`** (Impostos Líquidos sobre Produtos)
    6. **`PIB Total`** (PIB a preços de mercado)
  - Gera automaticamente:
    - **`dados/brutos/pib_to_dessazonalizado.xlsx`**: Planilha Excel completa contendo as abas `Serie_Ajuste_Sazonal`, `Trimestre_Contra_Anterior` (variações trimestrais `%`), `Dados_Originais_e_Dessaz` e as matrizes de decomposição X-13 (`Tendencia_Ciclo`, `Fator_Sazonal` e `Irregular`).
    - **`dados/pib_to_dessazonalizado.rds`**: Base binária que abastece diretamente o `atualizar_dados.R`, as boxes do painel e os boletins.
- **Abas extraídas e consolidadas pelo R (`pib_to_planilha`):**
  1. `"Base Móvel Ajustada"`
  2. `"Série Encadeada"`
  3. `"Taxa Trimestral"`
  4. `"Acum. em 4 trimestres"`
  5. `"Tx. Acumulada ao Longo do Ano"`
  6. `"Valores Correntes"` $\rightarrow$ A coluna **`PIB`** alimenta o valor nominal da box **`"PIB Nominal (TO)"`** e a coluna **`Impostos`** alimenta o valor nominal da box **`"Impostos (TO)"`** na aba `PIB (TO)`.
  7. `"Série com Ajuste Sazonal"` $\rightarrow$ Preenchida automaticamente pelo `scripts/dessazonalizacao_pib.R` (X-13ARIMA-SEATS).
  8. `"Trimestre contra Trimestre Ant."` $\rightarrow$ Preenchida automaticamente pelo `scripts/dessazonalizacao_pib.R` (`((I_t / I_{t-1}) - 1) * 100`), alimentando todas as variações trimestrais dessazonalizadas do painel e o gráfico comparativo **`Dinâmica de Crescimento: Tocantins vs Brasil`** na aba `PIB (TO)`.
- **Variações Dessazonalizadas e Avisos Visuais no Painel:**
  - Na aba **Visão Geral**, a box chama-se **`"Variação trimestral do PIB"`** (com um ícone circular `i` discreto de `15x15px` logo após a palavra `"PIB"`, sem comprimir a caixa nem encostar na logo) e o gráfico abaixo chama-se **`"Arrecadação ICMS e PIB trimestral"`** (com a linha amarela padronizada como **`"PIB trimestral"`**). Ao passar o mouse ou tocar no celular, o popover global (`z-index` máximo sobre todo o painel) explica ao leitor que a variação foi calculada pelo *Jornal Econômico do Tocantins* mediante processo estatístico próprio de dessazonalização (X-13ARIMA-SEATS), não sendo publicada oficialmente pela SEPLAN-TO.
  - Na aba **PIB (TO)**, a variação percentual abaixo do valor nominal de **`"PIB Nominal (TO)"`** replica automaticamente a **`"Variação trimestral do PIB"`** da Visão Geral, e a variação abaixo de **`"Impostos (TO)"`** vem da série dessazonalizada de Impostos.
  - **Ordenação Dinâmica dos 3 Grandes Setores (`Agropecuária`, `Serviços`, `Indústria`):** No bloco `var_setores_pib_to` do `atualizar_dados.R`, o sistema ordena automaticamente do maior para o menor valor (`sort(..., decreasing = TRUE)`), posicionando sempre a maior variação no topo (**`1º <Setor>`**), a intermediária no meio (**`2º <Setor>`**) e a menor embaixo (**`3º <Setor>`**), ajustando também os ícones (`chevron-double-up`, `chevron-up`, `chevron-down`).
  - **Aviso Único Centralizado na Aba `PIB (TO)`:** Na parte inferior abaixo do gráfico central, há um único selo interativo centralizado explicando que as variações estaduais (TO) passaram por dessazonalização própria do jornal, enquanto a variação do PIB do Brasil é dessazonalizada oficialmente pelo IBGE.

---

### Pesquisa 2: PIB do Brasil — Contas Nacionais Trimestrais (IBGE via pacote `sidrar`)
Extraída automaticamente da API do SIDRA/IBGE sem necessidade de baixar planilhas:

1. **Tabela 1621** (*Série encadeada do índice de volume trimestral com ajuste sazonal — Base: média 1995 = 100*, variável `584`, classificação `c11255`):
   - `90707` (*PIB a preços de mercado*) $\rightarrow$ Pareado trimestre a trimestre com a aba `"Série Encadeada"` da SEPLAN-TO para alimentar o gráfico comparativo **`Dinâmica de Crescimento: Tocantins vs Brasil`** na aba `PIB (TO)`.
   - `90687` (*Agropecuária - total*), `90691` (*Indústria - total*) e `90696` (*Serviços - total*) $\rightarrow$ Extraídos e salvos em `painel_dados$pesquisas_extras$pib_brasil_1621` para uso nos boletins e cartas.
2. **Tabela 1846** (*Valores a preços correntes — Milhões de Reais*, variável `585`, setor `90707` *PIB a preços de mercado*):
   - Alimenta o valor nominal da box **`"PIB Nominal (Brasil)"`** na aba `PIB (TO)` e fica salva em `pesquisas_extras$pib_brasil_1846`.
3. **Tabela 5932** (*Taxa de variação do índice de volume trimestral — %*, variável `6564` *Taxa trimestre contra trimestre imediatamente anterior (%)*, setor `90707` *PIB a preços de mercado*):
   - Alimenta a variação percentual exibida abaixo do valor nominal na box **`"PIB Nominal (Brasil)"`** na aba `PIB (TO)` e fica salva em `pesquisas_extras$pib_brasil_5932`.

---

### Pesquisa 3: PNAD Contínua Trimestral (IBGE via pacote `sidrar`)
Extraída automaticamente da API do SIDRA/IBGE:

1. **Tabela 4099** (*Taxas de desocupação e de subutilização da força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade — %*, variável `4099` *Taxa de desocupação*):
   - Extraída para **todos os 27 Estados (`n3/all`) e Brasil (`n1/all`)**.
   - O dado do **Tocantins (`UF = 17`)** alimenta automaticamente a box **`"Taxa de Desocupação"`** tanto na aba **`Visão Geral`** quanto na aba **`Mercado de Trabalho`**.
   - A base completa com todos os Estados e o Brasil fica salva em `painel_dados$pesquisas_extras$pnad_4099_desocupacao` para rankings e comparações nos boletins.
2. **Tabela 6374** (*Média de horas habitualmente trabalhadas por semana e efetivamente trabalhadas na semana de referência, por posição na ocupação*, variável `8186` *Média de horas habitualmente trabalhadas por semana no trabalho principal*, classificação `c2399`: `35423` *Empregado*, `35426` *Empregador*, `98272` *Conta própria*, `40089` *Trabalhador familiar auxiliar*):
   - Extraída para **Tocantins (`n3/17`) e Brasil (`n1/all`)** e salva em `painel_dados$pesquisas_extras$pnad_6374_horas` para uso nos boletins.
3. **Tabela 5436** (*Rendimento médio mensal real e nominal das pessoas de 14 anos ou mais de idade ocupadas, por sexo*, variável `5932` *Rendimento médio mensal real habitualmente recebido*, sexo `c2/6794` *Total*):
   - Extraída para **Tocantins (`n3/17`) e Brasil (`n1/all`)**.
   - O valor do **Tocantins (`UF = 17`)** alimenta automaticamente a box **`"Rendimento Médio"`** na aba **`Mercado de Trabalho`**, e a série completa (TO + Brasil) fica salva em `painel_dados$pesquisas_extras$pnad_5436_rendimento` para os boletins.

---

### Pesquisa 3B: Novo CAGED — Microdados Mensais do MTE (`scripts/extrair_novo_caged.R`)
Extraída diretamente do servidor FTP oficial de Microdados do PDET/MTE (`ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/`) pelo script dedicado **[`scripts/extrair_novo_caged.R`](scripts/extrair_novo_caged.R)** (que também é chamado automaticamente por `scripts/atualizar_dados.R`):

1. **Fluxo Limpo no SSD `D:` com Exclusão Imediata dos Microdados Brutos:**
   - Todos os downloads e descompactações ocorrem exclusivamente em **`D:\Journal\dados\tmp_microdados`** (jamais utilizando o disco `C:`).
   - Para cada competência mensal `AAAAMM`, o script baixa os 3 arquivos oficiais do MTE:
     - **`CAGEDMOVAAAAMM.7z`**: Movimentações declaradas **Dentro do Prazo** (`competenciamov == competenciadec`).
     - **`CAGEDFORAAAAMM.7z`**: Movimentações declaradas **Fora do Prazo** (`competenciadec == AAAAMM`, atualizando e corrigindo competências passadas `competenciamov < AAAAMM`).
     - **`CAGEDEXCAAAAMM.7z`**: **Exclusões** de declarações enviadas em competências anteriores (`competenciaexc == AAAAMM`, estornando o saldo na `competenciamov` original).
   - O script descompacta o `.txt`, filtra em menos de 1 segundo apenas os registros do **Tocantins (`uf == 17`)** via `data.table::fread`, salva na base histórica compacta (`dados/novo_caged_to_base.rds`, que ocupa menos de `2 MB` para 12 meses inteiros!) e **exclui imediatamente** os arquivos `.7z` e `.txt` brutos do computador.
2. **Reconciliação Automática de Competências (Dentro do Prazo vs. Fora do Prazo vs. Exclusões):**
   - A cada novo mês divulgado pelo MTE, o arquivo `CAGEDFOR` daquele mês é incorporado à base histórica do Tocantins e **recalcula automaticamente os saldos passados pela competência real da movimentação (`competenciamov`)**, garantindo 100% de consistência contábil tanto na série mensal do painel quanto no fechamento dos **Dossiês Anuais**.
   - A tabela de auditoria completa (`Admitidos_Dentro_Prazo`, `Desligados_Dentro_Prazo`, `Saldo_Dentro_Prazo`, `Saldo_Fora_Prazo`, `Saldo_Exclusoes` e `Saldo_Consolidado`) fica disponível em `dados_painel$pesquisas_extras$novo_caged$auditoria_series_mensais`, além do detalhamento específico das revisões trazidas pela última divulgação em `fora_prazo_ultima_divulgacao`.
3. **Alimentação Automática da Aba `Mercado de Trabalho` no Painel (`index.qmd`):**
   - **ValueBox `"Saldo Novo CAGED"`:** Exibe o saldo líquido de vínculos formais do mês vigente.
   - **Gráfico de Linhas `"Evolução do Saldo de Empregos (Novo CAGED)"`:** Exibe a série mensal consolidada das últimas 12 competências (com eixo Y padronizado como `"Saldo Novo CAGED"`).
   - **Gráfico de Barras Horizontais `"Evolução do Saldo de Empregos por Setor (Novo CAGED)"`:** Exibe o saldo nos **5 Grandes Setores Econômicos** (`Serviços` [Seções H–U], `Construção` [Seção F], `Comércio` [Seção G], `Agropecuária` [Seção A] e `Indústria` [Seções B–E]).
   - **Tabela Municipal `"Panorama Municipal (Estoque e Geração de Vagas)"`:** Contempla os **139 municípios do Tocantins** mapeados pelos códigos IBGE de 6 dígitos (`17xxxx`), alterando **automaticamente** o cabeçalho da coluna de saldo para o mês/ano vigente da leitura (ex: **`"Saldo (Novo CAGED) 09/26"`**).
4. **Recortes Analíticos Extras para os Boletins, Cartas e Dossiês (`dados_painel$pesquisas_extras$novo_caged`):**
   - **Geral (Todas as Seções e 21 Seções CNAE 2.0):** `resumo_geral_mes`, `resumo_acumulado_ano`, `secoes_cnae_mes_vigente` e `secoes_cnae_acumulado_ano` (com Admitidos, Desligados, Saldo e Salário Médio).
   - **Salário Mensal Declarado Médio e Mediano:** Calculado para **Admitidos**, **Desligados** e **Total Geral** (com filtro metodológico contra valores zerados ou erros de digitação extremos), no mês e no acumulado do ano.
   - **Por Idade / Faixa Etária (`por_faixa_etaria_mes` e `por_faixa_etaria_ano`):** Admitidos, Desligados, Saldo, Idade Média e Salário Médio nas 7 faixas etárias oficiais (`Até 17 anos`, `18 a 24 anos`, `25 a 29 anos`, `30 a 39 anos`, `40 a 49 anos`, `50 a 64 anos`, `65 anos ou mais`).
   - **Por Sexo (`por_sexo_mes` e `por_sexo_ano`):** Admitidos, Desligados, Saldo e Salário Médio para `Homem` e `Mulher`.
   - **Por Grau de Instrução (`por_grau_instrucao_mes`/`ano` e `por_escolaridade_grupo_mes`/`ano`):** Admitidos, Desligados, Saldo e Salário Médio tanto nas 13 categorias detalhadas do MTE (`Analfabeto` até `Doutorado`) quanto nos 6 grandes grupos de escolaridade.
   - **Por Tipo de Estabelecimento (`por_tipo_estabelecimento_mes` e `por_tipo_estabelecimento_ano`):** Admitidos, Desligados, Saldo e Salário Médio para `CNPJ`, `CAEPF (Pessoa Física / Produtor Rural)`, `CNO (Cadastro Nacional de Obra)` e `CEI (CAGED)`.
   - **Todos os 139 Municípios do Tocantins (`municipios_139_to`):** Admitidos, Desligados, Saldo e Salário Médio no mês vigente e no acumulado do ano para cada um dos 139 municípios tocantinenses.
5. **Como Rodar no Dia da Divulgação Mensal do Novo CAGED:**
   - Basta executar o script completo do jornal (`render_site.ps1`) ou rodar diretamente:
     ```powershell
     & "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" "D:\Journal\scripts\extrair_novo_caged.R"
     ```
   - Ele detectará automaticamente o novo mês no FTP do MTE (ou você pode informar a competência explicitamente, ex: `Rscript "D:\Journal\scripts\extrair_novo_caged.R" 202610`), baixará os microdados, filtrará o Tocantins, **excluirá os microdados brutos**, atualizará `painel_dados.rds` e recompilará os boletins automaticamente.

---

### Pesquisa 3C: RAIS — Microdados Anuais de Vínculos e Estabelecimentos do MTE (`scripts/extrair_rais.R`)
Extraída diretamente dos microdados oficiais do Ministério do Trabalho e Emprego (`ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/`), com módulo dedicado em **[`scripts/extrair_rais.R`](scripts/extrair_rais.R)** integrado automaticamente ao `scripts/atualizar_dados.R`:

1. **Fluxo Limpo e Econômico em Disco (Zero Acúmulo de Microdados Brutos):**
   - Baixa temporariamente na pasta `D:\Journal\dados\tmp_microdados` os arquivos `RAIS_VINC_PUB_NORTE.7z` (Vínculos da Região Norte) e `RAIS_ESTAB_PUB.7z` (Estabelecimentos do Brasil).
   - Descompacta, filtra imediatamente apenas os registros do **Tocantins (`UF == 17` / municípios `170000` a `179999`)** e **exclui imediatamente os arquivos brutos `.7z`, `.COMT` e `.txt`**, liberando 100% do espaço temporário.
   - Mantém apenas a base filtrada enxuta do Tocantins (`dados/rais_to_base.rds`) e o pacote completo de indicadores e tabelas (`dados/rais_to_resumo.rds`).
2. **Filtro Metodológico Oficial de Vínculos Abandonados (`RAIS_VINC_PUB_NORTE`):**
   - Seguindo estritamente a metodologia do Painel Online da RAIS/MTE, contabiliza no estoque anual exclusivamente os **vínculos ativos em 31/12 (`ind_vinculo_ativo_31_12_codigo == 1`)** que **NÃO são vínculos abandonados (`ind_vinculo_abandonado_codigo == 0`)**.
   - Na RAIS 2025 do Tocantins, dos `664.703` registros totais no ano, `222.683` foram desligados ao longo do ano e `4.440` eram vínculos abandonados (excluídos), resultando no estoque oficial exato de **`437.580` vínculos ativos em 31/12** — número que bate na unidade com a soma de `Qtd Vínculos Ativos` da base de estabelecimentos (`437.580`).
3. **Indicadores Alimentados Automaticamente no Painel do Jornal (`index.qmd`):**
   - **Tabela Municipal (`Panorama Municipal — Estoque e Geração de Vagas`):** Preenche automaticamente a coluna dinâmica `Estoque (RAIS) AAAA` (ex: `Estoque (RAIS) 2025`) para **todos os 139 municípios do Tocantins**, ao lado da coluna `Saldo (Novo CAGED) MM/AA`, além de destacar no cabeçalho da tabela o **Estoque Total Anual no Estado (`437.580` vínculos ativos)**.
4. **Indicadores Completos Salvos para os Boletins, Cartas e Dossiês (`pesquisas_extras$rais` e `dados/rais_to_resumo.rds`):**
   - **Parte A — Vínculos Ativos em 31/12 (sem abandonados):**
     - **Estoque Total Anual e Remuneração Média no Estado e nos 139 Municípios (`resumo_vinculos_to` e `vinculos_por_municipio`):** Estoque de vínculos ativos, desagregação entre CLT e Estatutários, Remuneração Média Nominal Anual (`Vl Rem Média Nom > 0`, `R$ 4.022,93` no TO), Remuneração Média de Dezembro (`R$ 4.221,46` no TO), Remuneração Média em Salários Mínimos e Idade Média para o Estado e cada um dos **139 municípios**.
     - **Por Faixa Etária (`vinculos_por_faixa_etaria`):** Estoque, participação (%) e remuneração média nas 8 faixas oficiais da RAIS (`10 a 14 anos` até `65 anos ou mais`).
     - **Por Raça/Cor (`vinculos_por_raca_cor`):** Estoque, participação (%) e remuneração média por raça/cor (`Parda`, `Branca`, `Preta`, `Amarela`, `Indígena`, `Não Identificado`).
     - **Por Gênero/Sexo (`vinculos_por_sexo`):** Estoque, participação (%) e remuneração média entre `Homem` e `Mulher`.
     - **Por Grau de Instrução (`vinculos_por_grau_instrucao` e `vinculos_por_escolaridade_grupo`):** Estoque, participação (%) e remuneração média nos 11 níveis detalhados (`Analfabeto` a `Doutorado`) e nos 6 grupos agregados.
     - **Por Grandes Seções da CNAE 2.0 (`vinculos_por_secao_cnae`, `vinculos_por_grande_setor`, `vinculos_secao_cnae_por_municipio`):** Estoque, participação (%) e remuneração média nas **21 Seções da CNAE 2.0 (`A` a `U`)** e nos 5 Grandes Setores, para o Estado e por Município.
   - **Parte B — Estabelecimentos (`RAIS_ESTAB_PUB` — Estado e todos os 139 Municípios):**
     - **Número Total de Estabelecimentos (`resumo_estab_to` e `estab_por_municipio`):** Total de estabelecimentos declarados (`91.318` no TO), estabelecimentos com atividade no ano (`45.254`), estabelecimentos empregadores com vínculos ativos em 31/12 (`33.198`), RAIS Negativa e optantes pelo Simples Nacional, no Estado e nos **139 municípios**.
     - **Vínculos e Estabelecimentos por Natureza Jurídica (`estab_por_natureza_grupo_to`, `estab_por_natureza_detalhada_to`, `estab_por_natureza_grupo_mun`, `estab_por_natureza_detalhada_mun`):** Tanto pelos Macrogrupos CONCLA (`Administração Pública`, `Entidades Empresariais`, `Entidades sem Fins Lucrativos`, `Pessoas Físicas e Outras Organizações`, `Não Identificado / CAEPF`) quanto pelos códigos detalhados de 4 dígitos, para o Estado e por Município.
     - **Quantidade de Estabelecimentos (e Vínculos) por Tamanho do Estabelecimento em 31/12 (`estab_por_tamanho_to` e `estab_por_tamanho_mun`):** Nas 10 faixas de tamanho de acordo com empregados ativos em 31/12 (`Zero vínculos`, `Até 4`, `De 5 a 9`, `De 10 a 19`, `De 20 a 49`, `De 50 a 99`, `De 100 a 249`, `De 250 a 499`, `De 500 a 999`, `1.000 ou mais`), para o Estado e por Município.
     - **Quantidade de Estabelecimentos (e Vínculos) por Tipo de Estabelecimento (`estab_por_tipo_to` e `estab_por_tipo_mun`):** Por `CNPJ`, `CAEPF (Pessoa Física / Produtor Rural)` e `CNO / CEI (Cadastro Nacional de Obras)`, para o Estado e por Município.
5. **Como Rodar no Dia da Divulgação Anual da RAIS:**
   - Basta executar o script completo do jornal (`render_site.ps1`) ou rodar diretamente:
     ```powershell
     & "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" "D:\Journal\scripts\extrair_rais.R"
     ```
   - Ele detectará automaticamente o ano mais recente no FTP do MTE (ou você pode informar o ano explicitamente, ex: `Rscript "D:\Journal\scripts\extrair_rais.R" 2026`), baixará os microdados no SSD `D:`, filtrará o Tocantins, excluirá os vínculos abandonados, **apagará os microdados brutos**, atualizará `painel_dados.rds` e recompilará os boletins automaticamente.

---

### Pesquisa 4: Comércio Exterior — API Oficial Comex Stat / MDIC (`https://api-comexstat.mdic.gov.br`)
Extraída diretamente via API REST oficial do Ministério do Desenvolvimento, Indústria, Comércio e Serviços (MDIC) usando o pacote `httr`, com controle inteligente de *rate-limit* e sincronização incremental em `dados/comex_stat_cache.rds` (só baixa novamente da API quando o endpoint `/general/dates/updated` indica divulgação de novo mês pelo MDIC):

1. **Tocantins (`filter = "state", values = [17]`, valores em US$ FOB):**
   - **Série Mensal Total (`pesquisas_extras$comex_to_mensal`):** Agrega mês a mês o total exportado (`Exportacoes_FOB_USD`), o total importado (`Importacoes_FOB_USD`), o **Saldo Comercial calculado** (`Saldo_FOB_USD = Exportacoes - Importacoes`) e a Corrente de Comércio (`Corrente_FOB_USD`).
   - **3 ValueBoxes da Aba `Comércio Exterior`:** Alimentadas automaticamente pelos valores do último mês divulgado pelo Comex Stat (`vb_exportacoes`, `vb_importacoes` e `vb_saldo`).
   - **Top 3 Produtos Exportados e Top 3 Produtos Importados (`comex$df_exp_top3` e `comex$df_imp_top3`):** Extraídos pela **Posição SH4 (`heading`)**, exibindo **apenas o nome do produto (sem o código numérico SH4)** nos gráficos horizontais do painel (`Pódio de Exportações` e `Pódio de Importações`).
   - **Top 10 Posições SH4 do Tocantins (Exportação e Importação):** Salvos tanto para o último mês (`pesquisas_extras$comex_to_top10_exp_sh4` e `comex_to_top10_imp_sh4`) quanto para o acumulado do ano (`comex_to_top10_exp_sh4_ano` e `comex_to_top10_imp_sh4_ano`) para uso nos boletins e dossiês.
   - **TODOS os Países Parceiros do Tocantins (`comex$df_exp_mapa`, `comex$df_imp_mapa` e `pesquisas_extras$comex_to_paises_*`):** Mapeados automaticamente pelo dicionário oficial SISCOMEX (`dados/brutos/PAIS_SISCOMEX.csv`) para o código ISO-A3 de 3 letras (`CHN`, `USA`, `DEU`, `RUS`, `ESP`, etc.), colorindo **todos** os países compradores e fornecedores do Tocantins no Mapa-Múndi interativo da aba `Comércio Exterior`.
2. **Brasil (Total Nacional — sem países, conforme definido):**
   - **Série Mensal Total (`pesquisas_extras$comex_br_mensal`):** Total exportado mensal, total importado mensal, **Saldo Comercial calculado** (`Exportações - Importações`) e Corrente de Comércio em US$ FOB.
   - **Top 10 Posições SH4 do Brasil (Exportação e Importação):** Salvos para o último mês (`pesquisas_extras$comex_br_top10_exp_sh4` e `comex_br_top10_imp_sh4`) e para o acumulado no ano (`comex_br_top10_exp_sh4_ano` e `comex_br_top10_imp_sh4_ano`) para compor os boletins, dossiês e cartas de conjuntura.

### 2.6. Arrecadação e Finanças Públicas (SEFAZ-TO, CONFAZ Dados Abertos e Tesouro Nacional — FPE)
O script modular **[`scripts/extrair_arrecadacao_to.R`](scripts/extrair_arrecadacao_to.R)** (chamado automaticamente por `scripts/atualizar_dados.R` ou executável de forma independente) consolida as receitas estaduais do Tocantins combinando três fontes oficiais:
1. **Portal da SEFAZ-TO (`https://www.to.gov.br/sefaz/arrecadacao-de-icms-do-estado-por-tributo-e-por-atividade-economica/40uwqy972dvx`):**
   - Raspa automaticamente os links de download dos boletins anuais/mensais (`2024`, `2025`, `2026` e futuros), baixa os PDFs temporariamente em `D:\Journal\dados\tmp_microdados\`, extrai todas as 34 rubricas tributárias e de atividade econômica nos 12 meses + colunas acumuladas (incluindo o resgate integral do ano fechado de `2023`) e **exclui os PDFs temporários imediatamente**.
   - Caso um ano passado já esteja completo com 12 meses salvos em `dados/arrecadacao_to_base.rds` (~17 KB com compressão `xz`), o script pula o download daquele ano, baixando apenas o ano corrente.
2. **Boletim de Arrecadação dos Tributos Estaduais — Dados Abertos / CONFAZ (`https://dados.gov.br/dados/conjuntos-dados/boletim-de-arrecadacao-dos-tributos-estaduais`):**
   - Consulta automaticamente a API JSON do painel oficial CONFAZ (`sg_uf == "TO"`) para obter a série histórica do CONFAZ e a partição entre **Comércio Atacadista** (`co_tipo = 131`) e **Comércio Varejista** (`co_tipo = 132`).
3. **Transferências Obrigatórias da União — FPE (`https://www.tesourotransparente.gov.br/ckan/dataset/transferencias-obrigatorias-da-uniao`):**
   - Consulta a API CKAN do Tesouro Transparente (STN), baixa temporariamente `fpe.csv` em `D:\Journal\dados\tmp_microdados\`, filtra os repasses mensais líquidos do **FPE para o Tocantins (`UF == "TO"`)** e exclui o CSV temporário imediatamente.
4. **Indicadores do Painel Online (`index.qmd`):**
   - **Aba `Visão Geral`:** Atualiza automaticamente a ValueBox **"Arrecadação ICMS (Var. Mensal)"** (`visao_geral$vb_icms_var`) com a variação percentual do mês mais recente contra o mês imediatamente anterior, além da série trimestral do gráfico **"Arrecadação ICMS e PIB trimestral"**.
   - **Aba `Arrecadação e Finanças`:** Atualiza automaticamente as 4 ValueBoxes do mês mais recente (**"Receita Total (Mês)"**, **"Arrecadação ICMS"**, **"Repasses Federais (FPE)"** e **"IPVA e Outros"**), o gráfico de área empilhada (**"Evolução das Receitas"**) e o Treemap (**"Composição do ICMS por Setor Econômico"**).
5. **Base Histórica Mensal, Anual e por Grande Setor CNAE 2.0 (`dados/arrecadacao_to_resumo.rds` — ~19,6 KB):**
   - Mantém **3 anos de base mensal completa** (`2024`, `2025` e `2026` — 33 meses) e **4 anos de base anual para Dossiês** (`2023`, `2024`, `2025` e `2026`) nas tabelas `mensal_tributos`, `anual_tributos`, `mensal_icms_atividades`, `anual_icms_atividades`, `mensal_icms_cnae` e `anual_icms_cnae` (mapeadas para as Seções da CNAE 2.0).

### 2.7. Dinâmica Empresarial (CNPJ / Receita Federal, Simples Nacional e Mapa de Empresas)
O script modular **[`scripts/extrair_cnpj_to.R`](scripts/extrair_cnpj_to.R)** (chamado automaticamente por `scripts/atualizar_dados.R` ou executável de forma independente) extrai os dados oficiais de empresas do Tocantins e de seus **139 municípios** sem consumir espaço com microdados brutos no SSD:
1. **Motor Receita Federal / SERPRO (`wss://dd.serpro.gov.br/publico/app/...`):**
   - **App 1 (`7979697b-ad3d-4b28-a5bf-9cd48ea9eae7`):** Detecta automaticamente a competência mais recente publicada pela Receita Federal, extrai o estoque de **CNPJs ativos**, **aberturas no mês**, **baixas**, **saldo dos últimos 12 meses**, série mensal/anual histórica (`2020`–atual), **Porte** (`MEI`, `ME`, `EPP`, `Demais`), **Tipo de Estabelecimento** (`Matriz` vs. `Filial`), **Natureza Jurídica** e toda a desagregação dos **139 municípios do Tocantins**.
   - **App 2 (`2a7344b2-670f-46d4-8632-7d262923aee0`):** Extrai empresas ativas por **Faixa de Capital Social**, **CNAE 2.0** (Grande Setor, Seção e Divisão), além do mapeamento completo de **Sócios (QSA)** e **Responsáveis** por gênero, faixa etária, tipo (PF/PJ/Estrangeiro), qualificação e município.
   - **App 3 (`314cd578-2c59-45c4-894f-186422f7f5cd` — Mapa de Empresas / REDESIM):** Extrai os tempos médios de **Abertura**, **Viabilidade** e **Registro** (em horas) no ano corrente (`2026`) e em série histórica mensal/anual para o Tocantins, Brasil e municípios.
2. **Portal Oficial do Simples Nacional (`SINAC / Receita Federal`):**
   - Consulta automaticamente o total atualizado de **optantes pelo Simples Nacional** no Tocantins e em cada um dos seus 139 municípios.
3. **Armazenamento Ultra-Compacto (`compress = "xz"`):**
   - Salva todas as tabelas em **[`dados/cnpj_to_base.rds`](dados/cnpj_to_base.rds)** (~23 KB) e **[`dados/cnpj_to_resumo.rds`](dados/cnpj_to_resumo.rds)** (~24 KB), alimentando automaticamente o painel (`index.qmd` — abas `Visão Geral` e `Dinâmica Empresarial`) e os boletins (`scripts/compilar_boletins.R`).

---

## PARTE 3: Onde Fica a "Análise Robótica" e Como Escrever a "Análise Humana"

### 1. Onde fica a "Análise Robótica" (Estatística Descritiva Automática)?
Fica no arquivo **[`scripts/compilar_boletins.R`](scripts/compilar_boletins.R)**, dentro da função `gerar_conteudo_automatico`. Ela lê `dados/painel_dados.rds`, ordena os dados, calcula participações e variações e monta automaticamente a **Seção 1 (Panorama Estatístico do Período)**, incluindo os 4 KPIs de destaque, os gráficos vetoriais (em PDF para o LaTeX e PNG para o HTML) e as tabelas formatadas.

### 2. Onde a equipe coloca a "Análise Humana"?
Na pasta **[`boletins/textos_equipe/`](boletins/textos_equipe/)**, criando um arquivo `.txt` simples seguindo o modelo:

```text
TIPO: flash_comex
TITULO: Título Principal da Publicação (Manchete)
SUBTITULO: Subtítulo explicando o foco da análise desta edição
AUTOR: Nome do Analista | Equipe Jornal Econômico do Tocantins
DATA: 03/11/2026
PERIODO: Outubro/2026
DESTAQUE_CURTO: Frase síntese de 2 linhas que aparece no quadro de destaque no topo.
=== ANALISE_CONJUNTURA ===
Primeiro parágrafo da análise humana da equipe...

Segundo parágrafo (separado por uma linha em branco)...

=== PERSPECTIVAS ===
Parágrafos de perspectivas e cenários para os próximos meses...
```

---

## PARTE 4: Como Atualizar Tudo (Localmente ou pelo GitHub)

### Opção A: No Seu Computador (1 Comando Único no SSD `D:\Journal`)
Todo o projeto está alocado fisicamente no **SSD `D:\Journal`** (com junções transparentes em `c:\Users\Lorenzo Costa\Desktop\Trabalhos\Journal` e pasta temporária dedicada em `D:\Journal\dados\tmp_microdados` para não consumir espaço do disco `C:`). Sempre que quiser puxar novas divulgações (Novo CAGED, Comex Stat, SIDRA/IBGE, SEPLAN-TO) e recompilar todo o site e boletins, execute no PowerShell:

```powershell
powershell -ExecutionPolicy Bypass -File "D:\Journal\scripts\render_site.ps1"
```

### Opção B: Pelo Navegador no GitHub (Para a Equipe)
Basta criar ou editar um arquivo `.txt` em `boletins/textos_equipe/` no GitHub e clicar em **Commit changes**. O GitHub Actions ([`.github/workflows/atualizar_jornal.yml`](.github/workflows/atualizar_jornal.yml)) compila o PDF e atualiza o site automaticamente!
