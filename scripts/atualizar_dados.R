# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/atualizar_dados.R
# Objetivo:
#   1. Extrair centralizadamente todas as pesquisas oficiais (SEPLAN-TO, IBGE/SIDRA,
#      BCB/SGS, etc.) ANTES da montagem das listas das abas do painel.
#   2. Alimentar automaticamente as caixas (ValueBoxes) e gráficos do painel
#      online (index.qmd) mantendo 100% de compatibilidade com a estrutura.
#   3. Armazenar em 'pesquisas_extras' todas as tabelas e recortes analíticos
#      adicionais que não cabem no painel, mas que abastecem os Boletins Flash,
#      os Dossiês Anuais e as Cartas de Conjuntura.
# ==============================================================================

# Garante diretório raiz no SSD D: e pastas temporárias fora do disco C:
if (!file.exists("index.qmd") && dir.exists("D:/Journal")) {
  setwd("D:/Journal")
}
dir_tmp_proj <- normalizePath("dados/tmp_microdados", winslash = "/", mustWork = FALSE)
dir.create(dir_tmp_proj, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMP = dir_tmp_proj, TEMP = dir_tmp_proj, TMPDIR = dir_tmp_proj)

suppressPackageStartupMessages({
  for (pkg in c("jsonlite", "readxl", "sidrar", "httr", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
  library(jsonlite)
  library(readxl)
  library(sidrar)
  library(httr)
  library(dplyr)
})

if (file.exists("scripts/utils_debug_log.R")) {
  source("scripts/utils_debug_log.R")
  limpar_registro_diagnostico()
}

cat("========================================================\n")
cat("Iniciando rotina de extracao e atualizacao - Tocantins...\n")
cat("Data/Hora:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")
cat("========================================================\n")

# Garante que as pastas de dados existam
dir.create("dados", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/brutos", recursive = TRUE, showWarnings = FALSE)

# Carrega base anterior (se existir) para fallback seguro (offline) e detecção de novidades
arquivo_rds <- "dados/painel_dados.rds"
dados_antigos <- if (file.exists(arquivo_rds)) readRDS(arquivo_rds) else NULL

# Funções auxiliares de formatação no padrão brasileiro
fmt_num_br <- function(x, casas = 1) {
  format(round(as.numeric(x), casas), big.mark = ".", decimal.mark = ",", nsmall = casas, trim = TRUE)
}

fmt_moeda_mi_br <- function(valor_mi, casas = 2) {
  v <- as.numeric(valor_mi)
  if (is.na(v)) return("N/D")
  if (abs(v) >= 1e6) {
    paste0("R$ ", fmt_num_br(v / 1e6, casas), " Tri")
  } else if (abs(v) >= 1e3) {
    paste0("R$ ", fmt_num_br(v / 1e3, casas), " Bi")
  } else {
    paste0("R$ ", fmt_num_br(v, casas), " Mi")
  }
}

fmt_seta_var <- function(var_pct, casas = 1, com_sinal = FALSE) {
  v <- as.numeric(var_pct)
  if (is.na(v)) return("")
  seta  <- if (v > 0) "▲" else if (v < 0) "▼" else "="
  sinal <- if (com_sinal) { if (v > 0) "+ " else if (v < 0) "- " else "" } else ""
  val_str <- fmt_num_br(abs(v), casas)
  if (!com_sinal && v < 0) {
    # Quando sem sinal explícito separado, mantém o sinal negativo caso preferível ou apenas a seta ▼
    return(paste0(seta, " ", val_str, "%"))
  }
  paste0(seta, " ", sinal, val_str, "%")
}

# Converte código de trimestre da SEPLAN ("2010.I", "2026.IV") para código SIDRA ("201001", "202604")
converter_periodo_seplan_para_sidra <- function(periodos) {
  partes <- strsplit(as.character(periodos), "\\.")
  vapply(partes, function(p) {
    if (length(p) < 2) return(NA_character_)
    tri <- match(trimws(p[2]), c("I", "II", "III", "IV"))
    if (is.na(tri)) return(NA_character_)
    sprintf("%s0%d", trimws(p[1]), tri)
  }, character(1))
}

# Padroniza rótulo de trimestre ("2010.I" ou "201001" -> "T1 2010") para todo o jornal
padronizar_rotulo_trimestre <- function(periodos) {
  partes <- strsplit(as.character(periodos), "\\.")
  vapply(partes, function(p) {
    if (length(p) == 2) {
      tri <- match(trimws(p[2]), c("I", "II", "III", "IV", "1", "2", "3", "4"))
      if (tri > 4) tri <- tri - 4
      if (!is.na(tri)) return(sprintf("T%d %s", tri, trimws(p[1])))
    }
    p_str <- trimws(as.character(p[1]))
    if (grepl("^[0-9]{6}$", p_str)) {
      ano <- substr(p_str, 1, 4)
      tri <- as.integer(substr(p_str, 5, 6))
      return(sprintf("T%d %s", tri, ano))
    }
    p_str
  }, character(1))
}

# Wrapper seguro para consulta ao SIDRA via string de API com log de debug detalhado
consultar_sidra_seguro <- function(api_path, descricao = api_path, fallback = NULL) {
  cat(sprintf("-> Extraindo SIDRA/IBGE [%s]...\n", descricao))
  t0 <- proc.time()[["elapsed"]]
  tryCatch({
    df <- suppressMessages(sidrar::get_sidra(api = api_path))
    dt <- proc.time()[["elapsed"]] - t0
    if (!is.null(df) && nrow(df) > 0) {
      df$Valor <- suppressWarnings(as.numeric(df$Valor))
      if (exists("log_ok")) log_ok(descricao, sprintf("API SIDRA retornou %d registros.", nrow(df)), dt, "atualizar_dados.R")
      return(df)
    }
    if (exists("log_aviso")) {
      log_aviso(descricao,
                sprintf("Retorno vazio em '%s'. Usando cache local.", api_path),
                "Verifique disponibilidade da tabela no portal SIDRA/IBGE.",
                "atualizar_dados.R")
    }
    fallback
  }, error = function(e) {
    if (exists("log_erro_debug")) {
      log_erro_debug("atualizar_dados.R", descricao, api_path, conditionMessage(e),
                     "Usando cache local",
                     sprintf("Verifique se os parametros '%s' estao ativos na API do SIDRA/IBGE.", api_path))
    } else {
      message(sprintf("   [AVISO] Falha ao consultar SIDRA (%s): %s. Usando cache.", descricao, e$message))
    }
    fallback
  })
}

# ==============================================================================
# 1. EXTRAÇÃO CENTRALIZADA DAS PESQUISAS OFICIAIS (ANTES DAS ABAS DO PAINEL)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1. PESQUISA 1: PIB TRIMESTRAL DO TOCANTINS (Planilha Local SEPLAN-TO)
#      Arquivo: dados/brutos/pib_trimestral_seplan.xlsx
#      Para atualizar: basta baixar o XLSX no site da SEPLAN-TO e salvar com
#      esse nome na pasta 'dados/brutos/'.
# ------------------------------------------------------------------------------
caminho_xlsx_pib_to <- "dados/brutos/pib_trimestral_seplan.xlsx"

ler_aba_pib_seplan <- function(caminho, nome_aba) {
  if (!file.exists(caminho)) return(NULL)
  tryCatch({
    raw <- as.data.frame(suppressMessages(
      readxl::read_excel(caminho, sheet = nome_aba, skip = 3, col_names = FALSE)
    ))
    if (ncol(raw) < 7) return(NULL)
    raw <- raw[, 1:7]
    colnames(raw) <- c("Periodo", "Agropecuaria", "Industria", "Servicos", "VA_Total", "Impostos", "PIB")
    raw$Periodo <- trimws(as.character(raw$Periodo))
    raw <- raw[!is.na(raw$Periodo) & grepl("^[0-9]{4}\\.(I|II|III|IV)$", raw$Periodo), ]
    for (col in c("Agropecuaria", "Industria", "Servicos", "VA_Total", "Impostos", "PIB")) {
      raw[[col]] <- suppressWarnings(as.numeric(raw[[col]]))
    }
    raw$Trimestre_Cod <- converter_periodo_seplan_para_sidra(raw$Periodo)
    raw$Periodo       <- padronizar_rotulo_trimestre(raw$Periodo)
    rownames(raw) <- NULL
    raw
  }, error = function(e) {
    message(sprintf("   [AVISO] Nao foi possivel ler a aba '%s' da planilha SEPLAN: %s", nome_aba, e$message))
    NULL
  })
}

cat("-> Lendo planilha do PIB Trimestral SEPLAN-TO (dados/brutos/pib_trimestral_seplan.xlsx)...\n")
# Executa o script oficial de dessazonalização X-13ARIMA-SEATS (scripts/dessazonalizacao_pib.R),
# que também sincroniza automaticamente qualquer nova planilha SEPLAN colocada em 'dados/brutos/' ou 'scripts/'
resultado_dessaz_pib_to <- tryCatch({
  source("scripts/dessazonalizacao_pib.R", local = TRUE)
  if (exists("resultado_dessaz_pib_to")) resultado_dessaz_pib_to else readRDS("dados/pib_to_dessazonalizado.rds")
}, error = function(e) {
  message(sprintf("   [AVISO] Falha ao rodar dessazonalizacao_pib.R (%s). Tentando cache RDS...", e$message))
  if (file.exists("dados/pib_to_dessazonalizado.rds")) readRDS("dados/pib_to_dessazonalizado.rds") else NULL
})

pib_to_planilha <- list(
  base_movel_ajustada       = ler_aba_pib_seplan(caminho_xlsx_pib_to, "Base Móvel Ajustada"),
  serie_encadeada           = ler_aba_pib_seplan(caminho_xlsx_pib_to, "Série Encadeada"),
  taxa_trimestral           = ler_aba_pib_seplan(caminho_xlsx_pib_to, "Taxa Trimestral"),
  acum_4_trimestres         = ler_aba_pib_seplan(caminho_xlsx_pib_to, "Acum. em 4 trimestres"),
  tx_acumulada_ano          = ler_aba_pib_seplan(caminho_xlsx_pib_to, "Tx. Acumulada ao Longo do Ano"),
  valores_correntes         = ler_aba_pib_seplan(caminho_xlsx_pib_to, "Valores Correntes"),
  serie_ajuste_sazonal      = if (!is.null(resultado_dessaz_pib_to)) resultado_dessaz_pib_to$serie_ajuste_sazonal else ler_aba_pib_seplan(caminho_xlsx_pib_to, "Série com Ajuste Sazonal"),
  trimestre_contra_anterior = if (!is.null(resultado_dessaz_pib_to)) resultado_dessaz_pib_to$trimestre_contra_anterior else ler_aba_pib_seplan(caminho_xlsx_pib_to, "Trimestre contra Trimestre Ant."),
  decomposicao_x13          = if (!is.null(resultado_dessaz_pib_to)) resultado_dessaz_pib_to$decomposicao else NULL
)

# Extrai o último valor nominal do PIB (TO) e de Impostos (TO) da aba "Valores Correntes"
df_vc_to <- pib_to_planilha$valores_correntes
if (!is.null(df_vc_to) && nrow(df_vc_to) > 0) {
  ult_vc_to <- tail(df_vc_to[!is.na(df_vc_to$PIB), ], 1)
  pib_nominal_to_mi      <- ult_vc_to$PIB[1]
  impostos_nominal_to_mi <- ult_vc_to$Impostos[1]
  periodo_pib_to_atual   <- ult_vc_to$Periodo[1]
} else {
  pib_nominal_to_mi      <- 25753.64
  impostos_nominal_to_mi <- 2262.995
  periodo_pib_to_atual   <- "2026.I"
}

# Variações trimestrais com ajuste sazonal próprio do jornal (SEPLAN-TO):
# Nota: Se a aba "Trimestre contra Trimestre Ant." já estiver preenchida pelo script de
#       dessazonalização, usa automaticamente a última linha; caso contrário, mantém os
#       valores dessazonalizados calculados pelo jornal.
df_dessaz_to <- pib_to_planilha$trimestre_contra_anterior
tem_dessaz_planilha <- !is.null(df_dessaz_to) && nrow(df_dessaz_to) > 0 && any(!is.na(df_dessaz_to$PIB))

if (tem_dessaz_planilha) {
  ult_dz <- tail(df_dessaz_to[!is.na(df_dessaz_to$PIB), ], 1)
  var_pib_to_dessaz      <- ult_dz$PIB[1]
  var_impostos_to_dessaz <- ult_dz$Impostos[1]
  var_setores_pib_to     <- c(
    "Agropecuária" = ult_dz$Agropecuaria[1],
    "Serviços"     = ult_dz$Servicos[1],
    "Indústria"    = ult_dz$Industria[1]
  )
} else {
  # Valores dessazonalizados pelo jornal (aguardando integração do script de dessazonalização)
  var_pib_to_dessaz      <- 3.2
  var_impostos_to_dessaz <- 5.2
  var_setores_pib_to     <- c(
    "Agropecuária" = 8.2,
    "Serviços"     = 4.1,
    "Indústria"    = -1.5
  )
}

# Ordenação dinâmica (maleável) dos 3 grandes setores do PIB (TO): do maior (1º no topo) para o menor (3º embaixo)
setores_ordenados <- sort(var_setores_pib_to, decreasing = TRUE)
setores_ranking_pib_to <- lapply(seq_along(setores_ordenados), function(pos) {
  nome_setor <- names(setores_ordenados)[pos]
  val_num    <- unname(setores_ordenados[pos])
  icone_box  <- if (pos == 1 && val_num > 0) {
    "chevron-double-up"
  } else if (val_num > 0) {
    "chevron-up"
  } else if (val_num < 0) {
    "chevron-down"
  } else {
    "dash"
  }
  list(
    posicao   = pos,
    setor     = nome_setor,
    titulo    = sprintf("%dº %s", pos, nome_setor),
    valor_num = val_num,
    valor_fmt = fmt_seta_var(val_num, casas = 1, com_sinal = TRUE),
    icone     = icone_box
  )
})

# Formatação das boxes de PIB (TO)
str_var_pib_to_vg  <- paste0(if (var_pib_to_dessaz >= 0) "+" else "", fmt_num_br(var_pib_to_dessaz, 1), "%")
str_vb_pib_nom_to  <- paste0(fmt_moeda_mi_br(pib_nominal_to_mi, 1), " ", fmt_seta_var(var_pib_to_dessaz, 1))
str_vb_impostos_to <- paste0(fmt_moeda_mi_br(impostos_nominal_to_mi, 2), " ", fmt_seta_var(var_impostos_to_dessaz, 1))

# ------------------------------------------------------------------------------
# 1.2. PESQUISA 2: PIB DO BRASIL (Contas Nacionais Trimestrais - IBGE via sidrar)
#      - Tabela 1621: Série encadeada do índice de volume trimestral c/ ajuste sazonal
#                     (Setores: 90687 Agropecuária, 90691 Indústria, 90696 Serviços, 90707 PIB)
#      - Tabela 1846: Valores a preços correntes (Milhões de Reais) (90707 PIB)
#      - Tabela 5932: Taxa trimestre contra trimestre imediatamente anterior (%) (90707 PIB)
# ------------------------------------------------------------------------------
cache_extra <- if (!is.null(dados_antigos) && !is.null(dados_antigos$pesquisas_extras)) dados_antigos$pesquisas_extras else list()

sidra_1621_pib_br <- consultar_sidra_seguro(
  api_path  = "/t/1621/n1/all/v/584/p/all/c11255/90687,90691,90696,90707",
  descricao = "Tabela 1621 - PIB Brasil Serie Encadeada (PIB e Setores)",
  fallback  = cache_extra$pib_brasil_1621
)

sidra_1846_pib_br_corr <- consultar_sidra_seguro(
  api_path  = "/t/1846/n1/all/v/585/p/all/c11255/90707",
  descricao = "Tabela 1846 - PIB Brasil Valores Correntes (R$ Milhoes)",
  fallback  = cache_extra$pib_brasil_1846
)

sidra_5932_pib_br_var <- consultar_sidra_seguro(
  api_path  = "/t/5932/n1/all/v/6564/p/all/c11255/90707",
  descricao = "Tabela 5932 - PIB Brasil Taxa Trimestre/Trimestre Anterior (%)",
  fallback  = cache_extra$pib_brasil_5932
)

# Contas Regionais do Brasil (IBGE - Tabela 5938):
# - Série anual de PIB a preços correntes (37), Participação no Norte (530),
#   Participação no Brasil (496), Impostos líquidos (543), VA Total (498),
#   VA Agropecuária (513), VA Indústria (517), VA Serviços excl. Adm (6575) e VA Adm Pública (525).
sidra_5938_contas_reg_uf <- consultar_sidra_seguro(
  api_path  = "/t/5938/n1/all/n2/1/n3/17/v/37,530,496,543,498,513,517,6575,525/p/last%2010",
  descricao = "Tabela 5938 - Contas Regionais do Brasil (UF Tocantins, Norte e Brasil)",
  fallback  = cache_extra$pib_contas_regionais_5938
)

# PIB dos Municípios do Tocantins (IBGE - Tabela 5938 - Nível Municipal):
# - PIB a preços correntes (37) e Participação no PIB da UF (497) para os 139 municípios
sidra_5938_contas_reg_mun <- consultar_sidra_seguro(
  api_path  = "/t/5938/n6/in%20n3%2017/v/37,497/p/last%201",
  descricao = "Tabela 5938 - PIB dos 139 Municipios do Tocantins (Contas Regionais)",
  fallback  = cache_extra$pib_contas_regionais_mun_5938
)

# Estrutura tabela analítica pronta das Contas Regionais do Tocantins (últimos anos)
df_contas_regionais_to <- NULL
if (!is.null(sidra_5938_contas_reg_uf) && nrow(sidra_5938_contas_reg_uf) > 0) {
  tryCatch({
    col_nivel_cod <- names(sidra_5938_contas_reg_uf)[grepl("Unidade da Federa|Grande Regi.*C.*digo", names(sidra_5938_contas_reg_uf))][1]
    col_var_cod   <- names(sidra_5938_contas_reg_uf)[grepl("Vari.*vel.*C.*digo", names(sidra_5938_contas_reg_uf))][1]
    col_ano       <- names(sidra_5938_contas_reg_uf)[grepl("^Ano$", names(sidra_5938_contas_reg_uf))][1]
    df_cr_to <- sidra_5938_contas_reg_uf[sidra_5938_contas_reg_uf[[col_nivel_cod]] == "17" & !is.na(sidra_5938_contas_reg_uf$Valor), ]
    anos_cr  <- sort(unique(as.character(df_cr_to[[col_ano]])))
    extrair_var_cr <- function(ano_ref, cod_v) {
      v <- df_cr_to$Valor[df_cr_to[[col_ano]] == ano_ref & as.character(df_cr_to[[col_var_cod]]) == as.character(cod_v)]
      if (length(v) > 0 && !is.na(v[1])) as.numeric(v[1]) else NA_real_
    }
    df_contas_regionais_to <- do.call(rbind, lapply(anos_cr, function(a) {
      pib_mil  <- extrair_var_cr(a, "37")
      imp_mil  <- extrair_var_cr(a, "543")
      va_mil   <- extrair_var_cr(a, "498")
      agro_mil <- extrair_var_cr(a, "513")
      ind_mil  <- extrair_var_cr(a, "517")
      serv_mil <- extrair_var_cr(a, "6575")
      adm_mil  <- extrair_var_cr(a, "525")
      part_n   <- extrair_var_cr(a, "530")
      part_br  <- extrair_var_cr(a, "496")
      data.frame(
        Ano                  = as.integer(a),
        PIB_Corrente_Mi      = round(pib_mil / 1000, 2),
        Impostos_Mi          = round(imp_mil / 1000, 2),
        VA_Total_Mi          = round(va_mil / 1000, 2),
        VA_Agro_Mi           = round(agro_mil / 1000, 2),
        VA_Industria_Mi      = round(ind_mil / 1000, 2),
        VA_Servicos_Priv_Mi  = round(serv_mil / 1000, 2),
        VA_Adm_Publica_Mi    = round(adm_mil / 1000, 2),
        Part_Norte_Pct       = round(part_n, 2),
        Part_Brasil_Pct      = round(part_br, 2),
        stringsAsFactors     = FALSE
      )
    }))
    df_contas_regionais_to <- df_contas_regionais_to[order(df_contas_regionais_to$Ano), ]
    df_contas_regionais_to$Var_Nominal_PIB_Pct <- c(
      NA_real_,
      round(((df_contas_regionais_to$PIB_Corrente_Mi[-1] / head(df_contas_regionais_to$PIB_Corrente_Mi, -1)) - 1) * 100, 2)
    )
  }, error = function(e) NULL)
}

# Ranking dos Top 15 Municípios do Tocantins no PIB Municipal (Contas Regionais IBGE)
df_contas_regionais_mun_top10 <- NULL
if (!is.null(sidra_5938_contas_reg_mun) && nrow(sidra_5938_contas_reg_mun) > 0) {
  tryCatch({
    col_mun_nome <- names(sidra_5938_contas_reg_mun)[grepl("^Munic.*pio$", names(sidra_5938_contas_reg_mun))][1]
    col_mun_cod  <- names(sidra_5938_contas_reg_mun)[grepl("^Munic.*pio.*C.*digo", names(sidra_5938_contas_reg_mun))][1]
    col_var_cod  <- names(sidra_5938_contas_reg_mun)[grepl("Vari.*vel.*C.*digo", names(sidra_5938_contas_reg_mun))][1]
    col_ano      <- names(sidra_5938_contas_reg_mun)[grepl("^Ano$", names(sidra_5938_contas_reg_mun))][1]
    ano_mun_ref  <- tail(sort(unique(as.character(sidra_5938_contas_reg_mun[[col_ano]]))), 1)
    df_m37  <- sidra_5938_contas_reg_mun[as.character(sidra_5938_contas_reg_mun[[col_var_cod]]) == "37", ]
    df_m497 <- sidra_5938_contas_reg_mun[as.character(sidra_5938_contas_reg_mun[[col_var_cod]]) == "497", ]
    df_mun_join <- merge(
      df_m37[, c(col_mun_cod, col_mun_nome, "Valor")],
      df_m497[, c(col_mun_cod, "Valor")],
      by = col_mun_cod, suffixes = c("_PIB", "_Part")
    )
    df_mun_join$Municipio       <- sub("\\s*-\\s*TO$", "", as.character(df_mun_join[[col_mun_nome]]))
    df_mun_join$Ano_Ref         <- as.integer(ano_mun_ref)
    df_mun_join$PIB_Corrente_Mi <- round(as.numeric(df_mun_join$Valor_PIB) / 1000, 2)
    df_mun_join$Part_UF_Pct     <- round(as.numeric(df_mun_join$Valor_Part), 2)
    df_mun_join <- df_mun_join[order(-df_mun_join$PIB_Corrente_Mi), ]
    df_mun_join$Posicao <- seq_len(nrow(df_mun_join))
    df_contas_regionais_mun_top10 <- head(
      df_mun_join[, c("Posicao", "Municipio", "Ano_Ref", "PIB_Corrente_Mi", "Part_UF_Pct")],
      15
    )
    rownames(df_contas_regionais_mun_top10) <- NULL
  }, error = function(e) NULL)
}

# Extrai valor nominal do PIB Brasil (Tabela 1846) + variação trimestral (Tabela 5932) para a box "PIB Nominal (Brasil)"
if (!is.null(sidra_1846_pib_br_corr) && nrow(sidra_1846_pib_br_corr) > 0) {
  df_1846_val <- sidra_1846_pib_br_corr[!is.na(sidra_1846_pib_br_corr$Valor), ]
  pib_br_nominal_mi <- tail(df_1846_val$Valor, 1)
} else {
  pib_br_nominal_mi <- 3425728
}

if (!is.null(sidra_5932_pib_br_var) && nrow(sidra_5932_pib_br_var) > 0) {
  df_5932_val <- sidra_5932_pib_br_var[!is.na(sidra_5932_pib_br_var$Valor), ]
  pib_br_var_trim <- tail(df_5932_val$Valor, 1)
} else {
  pib_br_var_trim <- 0.5
}

str_vb_pib_nom_br <- paste0(fmt_moeda_mi_br(pib_br_nominal_mi, 2), " ", fmt_seta_var(pib_br_var_trim, 1))

# Série de variação trimestral dessazonalizada do PIB (TO) para os gráficos (Visão Geral e PIB TO)
trimestres_painel <- c("T3 2024", "T4 2024", "T1 2025", "T2 2025", "T3 2025", "T4 2025", "T1 2026")
codigos_trim_painel <- c("202403", "202404", "202501", "202502", "202503", "202504", "202601")

serie_var_pib_to_dessaz_painel <- if (tem_dessaz_planilha && all(codigos_trim_painel %in% df_dessaz_to$Trimestre_Cod)) {
  round(df_dessaz_to$PIB[match(codigos_trim_painel, df_dessaz_to$Trimestre_Cod)], 2)
} else if (tem_dessaz_planilha && nrow(df_dessaz_to) >= length(trimestres_painel)) {
  round(tail(df_dessaz_to$PIB[!is.na(df_dessaz_to$PIB)], length(trimestres_painel)), 2)
} else {
  c(1.8, 1.9, 2.2, 2.9, 2.5, 3.1, var_pib_to_dessaz)
}

# Constrói o data.frame comparativo (Tocantins vs Brasil) para o gráfico central da aba PIB (TO)
# utilizando a VARIAÇÃO TRIMESTRAL (%) DO NÚMERO-ÍNDICE COM AJUSTE SAZONAL:
# - Brasil: calculada diretamente da variação do número-índice da Tabela 1621 do IBGE ((I_t / I_{t-1} - 1) * 100).
# - Tocantins: calculada pelo script oficial de dessazonalização X-13ARIMA-SEATS (scripts/dessazonalizacao_pib.R).
df_sas_to <- pib_to_planilha$serie_ajuste_sazonal
tem_sas_planilha <- !is.null(df_sas_to) && nrow(df_sas_to) > 1 && sum(!is.na(df_sas_to$PIB)) > 1
df_pib_comparativo_historico <- NULL

if (!is.null(sidra_1621_pib_br)) {
  df_br_90707 <- sidra_1621_pib_br[
    sidra_1621_pib_br[["Setores e subsetores (Código)"]] == "90707" & !is.na(sidra_1621_pib_br$Valor),
  ]
  df_br_90707 <- df_br_90707[order(df_br_90707[["Trimestre (Código)"]]), ]
  # Calcula a variação trimestral (%) a partir do número-índice sazonalmente ajustado da Tabela 1621
  df_br_90707$Var_Pct <- c(NA_real_, ((df_br_90707$Valor[-1] / head(df_br_90707$Valor, -1)) - 1) * 100)
  
  if (tem_sas_planilha || tem_dessaz_planilha) {
    if (tem_dessaz_planilha) {
      df_to_var <- df_dessaz_to[!is.na(df_dessaz_to$PIB), c("Trimestre_Cod", "Periodo", "PIB")]
      colnames(df_to_var)[3] <- "Var_TO"
    } else {
      df_sas_val <- df_sas_to[!is.na(df_sas_to$PIB), ]
      df_sas_val$Var_TO <- c(NA_real_, ((df_sas_val$PIB[-1] / head(df_sas_val$PIB, -1)) - 1) * 100)
      df_to_var <- df_sas_val[!is.na(df_sas_val$Var_TO), c("Trimestre_Cod", "Periodo", "Var_TO")]
    }
    df_merge_pib <- merge(
      df_to_var,
      df_br_90707[, c("Trimestre (Código)", "Var_Pct")],
      by.x = "Trimestre_Cod",
      by.y = "Trimestre (Código)"
    )
    df_merge_pib <- df_merge_pib[order(df_merge_pib$Trimestre_Cod), ]
    
    # Série histórica completa (T2 2010 em diante) para boletins, dossiês e cartas de conjuntura
    niveis_hist <- df_merge_pib$Periodo
    df_pib_comparativo_historico <- data.frame(
      Ano    = factor(rep(niveis_hist, 2), levels = niveis_hist),
      Indice = c(round(df_merge_pib$Var_TO, 2), round(df_merge_pib$Var_Pct, 2)),
      Regiao = rep(c("Tocantins", "Brasil"), each = nrow(df_merge_pib))
    )
    
    # Janela trimestral padronizada do painel (mesmos 7 trimestres da aba Visão Geral)
    df_merge_janela <- tail(df_merge_pib, length(trimestres_painel))
    niveis_periodo  <- df_merge_janela$Periodo
    df_pib_comparativo <- data.frame(
      Ano    = factor(rep(niveis_periodo, 2), levels = niveis_periodo),
      Indice = c(round(df_merge_janela$Var_TO, 2), round(df_merge_janela$Var_Pct, 2)),
      Regiao = rep(c("Tocantins", "Brasil"), each = nrow(df_merge_janela))
    )
  } else {
    df_br_recorte <- df_br_90707[match(codigos_trim_painel, df_br_90707[["Trimestre (Código)"]]), ]
    var_br_recorte <- if (nrow(df_br_recorte) == length(codigos_trim_painel) && all(!is.na(df_br_recorte$Var_Pct))) {
      round(df_br_recorte$Var_Pct, 2)
    } else {
      c(0.90, 0.08, 1.26, 0.36, 0.11, 0.25, 1.07)
    }
    df_pib_comparativo <- data.frame(
      Ano    = factor(rep(trimestres_painel, 2), levels = trimestres_painel),
      Indice = c(serie_var_pib_to_dessaz_painel, var_br_recorte),
      Regiao = rep(c("Tocantins", "Brasil"), each = length(trimestres_painel))
    )
  }
} else if (!is.null(dados_antigos) && !is.null(dados_antigos$pib$df_pib)) {
  df_pib_comparativo <- dados_antigos$pib$df_pib
} else {
  anos_fallback <- 2015:2026
  df_pib_comparativo <- data.frame(
    Ano    = factor(rep(anos_fallback, 2), levels = anos_fallback),
    Indice = c(100, 102, 105, 108, 112, 115, 110, 118, 125, 130, 136, 142,
               100,  98,  99, 101, 103, 105, 100, 104, 107, 110, 112, 115),
    Regiao = rep(c("Tocantins", "Brasil"), each = length(anos_fallback))
  )
}

# Constrói tabela Lado a Lado: Séries Dessazonalizadas Setoriais do Tocantins (X-13ARIMA-SEATS)
# vs. Brasil (Tabela 1621 do IBGE: 90687 Agropecuária, 90691 Indústria, 90696 Serviços, 90707 PIB)
df_pib_setores_dessaz_to_vs_br <- NULL
if (!is.null(sidra_1621_pib_br) && !is.null(df_sas_to) && !is.null(df_dessaz_to)) {
  tryCatch({
    extrair_br_1621 <- function(cod_setor) {
      d <- sidra_1621_pib_br[
        sidra_1621_pib_br[["Setores e subsetores (Código)"]] == cod_setor & !is.na(sidra_1621_pib_br$Valor),
      ]
      d <- d[order(d[["Trimestre (Código)"]]), ]
      d$Var_QoQ_BR <- c(NA_real_, round(((d$Valor[-1] / head(d$Valor, -1)) - 1) * 100, 2))
      d$Indice_BR  <- round(d$Valor, 2)
      d[, c("Trimestre (Código)", "Indice_BR", "Var_QoQ_BR")]
    }
    br_agro <- extrair_br_1621("90687")
    br_ind  <- extrair_br_1621("90691")
    br_serv <- extrair_br_1621("90696")
    br_pib  <- extrair_br_1621("90707")
    
    trims_comuns <- sort(intersect(df_dessaz_to$Trimestre_Cod[!is.na(df_dessaz_to$PIB)], br_pib[["Trimestre (Código)"]]))
    df_pib_setores_dessaz_to_vs_br <- do.call(rbind, lapply(trims_comuns, function(tc) {
      idx_sas <- match(tc, df_sas_to$Trimestre_Cod)
      idx_var <- match(tc, df_dessaz_to$Trimestre_Cod)
      idx_ba  <- match(tc, br_agro[["Trimestre (Código)"]])
      idx_bi  <- match(tc, br_ind[["Trimestre (Código)"]])
      idx_bs  <- match(tc, br_serv[["Trimestre (Código)"]])
      idx_bp  <- match(tc, br_pib[["Trimestre (Código)"]])
      data.frame(
        Trimestre_Cod    = tc,
        Periodo          = df_dessaz_to$Periodo[idx_var],
        TO_Agro_Indice   = round(df_sas_to$Agropecuaria[idx_sas], 2),
        TO_Agro_QoQ      = round(df_dessaz_to$Agropecuaria[idx_var], 2),
        BR_Agro_Indice   = br_agro$Indice_BR[idx_ba],
        BR_Agro_QoQ      = br_agro$Var_QoQ_BR[idx_ba],
        TO_Ind_Indice    = round(df_sas_to$Industria[idx_sas], 2),
        TO_Ind_QoQ       = round(df_dessaz_to$Industria[idx_var], 2),
        BR_Ind_Indice    = br_ind$Indice_BR[idx_bi],
        BR_Ind_QoQ       = br_ind$Var_QoQ_BR[idx_bi],
        TO_Serv_Indice   = round(df_sas_to$Servicos[idx_sas], 2),
        TO_Serv_QoQ      = round(df_dessaz_to$Servicos[idx_var], 2),
        BR_Serv_Indice   = br_serv$Indice_BR[idx_bs],
        BR_Serv_QoQ      = br_serv$Var_QoQ_BR[idx_bs],
        TO_PIB_Indice    = round(df_sas_to$PIB[idx_sas], 2),
        TO_PIB_QoQ       = round(df_dessaz_to$PIB[idx_var], 2),
        BR_PIB_Indice    = br_pib$Indice_BR[idx_bp],
        BR_PIB_QoQ       = br_pib$Var_QoQ_BR[idx_bp],
        stringsAsFactors = FALSE
      )
    }))
  }, error = function(e) NULL)
}

# ------------------------------------------------------------------------------
# 1.3. PESQUISA 3: PNAD CONTÍNUA TRIMESTRAL (IBGE via sidrar)
#      - Tabela 4099: Taxa de desocupação (%) - Todos os Estados e Brasil
#      - Tabela 6374: Média de horas habitualmente trabalhadas por posição na ocupação - TO e Brasil
#      - Tabela 5436: Rendimento médio mensal real habitual no trabalho principal (R$) - TO e Brasil
# ------------------------------------------------------------------------------
sidra_4099_desocupacao <- consultar_sidra_seguro(
  api_path  = "/t/4099/n1/all/n3/all/v/4099/p/last%2012",
  descricao = "Tabela 4099 - PNAD Taxa de Desocupacao (Brasil e 27 UFs)",
  fallback  = cache_extra$pnad_4099_desocupacao
)

sidra_6374_horas <- consultar_sidra_seguro(
  api_path  = "/t/6374/n1/all/n3/17/v/8186/p/last%2012/c2399/35423,35426,98272,40089",
  descricao = "Tabela 6374 - PNAD Horas Habitualmente Trabalhadas por Posicao (TO e Brasil)",
  fallback  = cache_extra$pnad_6374_horas
)

sidra_5436_rendimento <- consultar_sidra_seguro(
  api_path  = "/t/5436/n1/all/n3/17/v/5932/p/last%2012/c2/6794",
  descricao = "Tabela 5436 - PNAD Rendimento Medio Mensal Real Habitual (TO e Brasil)",
  fallback  = cache_extra$pnad_5436_rendimento
)

# Extrai a última Taxa de Desocupação do Tocantins (Código UF == "17") da Tabela 4099
if (!is.null(sidra_4099_desocupacao) && nrow(sidra_4099_desocupacao) > 0) {
  df_desoc_to <- sidra_4099_desocupacao[
    sidra_4099_desocupacao[["Brasil e Unidade da Federação (Código)"]] == "17" &
      !is.na(sidra_4099_desocupacao$Valor),
  ]
  taxa_desocupacao_to_atual <- paste0(fmt_num_br(tail(df_desoc_to$Valor, 1), 1), "%")
} else {
  taxa_desocupacao_to_atual <- "4,1%"
}

# Extrai o último Rendimento Médio Mensal Real Habitual do Tocantins (Código UF == "17") da Tabela 5436
if (!is.null(sidra_5436_rendimento) && nrow(sidra_5436_rendimento) > 0) {
  df_rend_to <- sidra_5436_rendimento[
    sidra_5436_rendimento[["Brasil e Unidade da Federação (Código)"]] == "17" &
      !is.na(sidra_5436_rendimento$Valor),
  ]
  rendimento_medio_to_atual <- paste0("R$ ", fmt_num_br(tail(df_rend_to$Valor, 1), 0))
} else {
  rendimento_medio_to_atual <- "R$ 3.351"
}

# ------------------------------------------------------------------------------
# 1.3.C. PESQUISA 3C: RAIS / MTE (Microdados Anuais de Vínculos e Estabelecimentos)
#        - Lê/baixa microdados RAIS_VINC_PUB_NORTE e RAIS_ESTAB_PUB, filtra UF == 17,
#          exclui vínculos abandonados em 31/12, remove microdados brutos (.7z/.COMT)
#          e gera estoque e remuneração para o Estado e os 139 municípios.
# ------------------------------------------------------------------------------
resumo_rais <- NULL
tryCatch({
  .chamado_por_atualizar_dados <- TRUE
  if (file.exists("scripts/extrair_rais.R")) {
    source("scripts/extrair_rais.R", local = TRUE)
  }
  if (file.exists("dados/rais_to_resumo.rds")) {
    resumo_rais <- readRDS("dados/rais_to_resumo.rds")
  }
}, error = function(e) {
  message(sprintf("   [AVISO] Erro ao processar RAIS (%s). Tentando cache RDS...", e$message))
  if (file.exists("dados/rais_to_resumo.rds")) {
    resumo_rais <<- readRDS("dados/rais_to_resumo.rds")
  }
})

# ------------------------------------------------------------------------------
# 1.3.B. PESQUISA 3B: NOVO CAGED / MTE (Microdados Mensais - Tocantins e 139 Municípios)
#        - Lê/baixa microdados CAGEDMOV (Dentro do Prazo), CAGEDFOR (Fora do Prazo)
#          e CAGEDEXC (Exclusões), filtra UF == 17, exclui brutos (.7z/.txt) e
#          reconcilia competências passadas automaticamente.
# ------------------------------------------------------------------------------
resumo_caged <- NULL
tryCatch({
  .chamado_por_atualizar_dados <- TRUE
  if (file.exists("scripts/extrair_novo_caged.R")) {
    source("scripts/extrair_novo_caged.R", local = TRUE)
  }
  if (file.exists("dados/novo_caged_to_resumo.rds")) {
    resumo_caged <- readRDS("dados/novo_caged_to_resumo.rds")
  }
}, error = function(e) {
  message(sprintf("   [AVISO] Erro ao processar Novo CAGED (%s). Tentando cache RDS...", e$message))
  if (file.exists("dados/novo_caged_to_resumo.rds")) {
    resumo_caged <<- readRDS("dados/novo_caged_to_resumo.rds")
  }
})

# ------------------------------------------------------------------------------
# 1.4. PESQUISA 4: COMÉRCIO EXTERIOR (API Oficial Comex Stat / MDIC)
#      Endpoint Oficial: https://api-comexstat.mdic.gov.br
#      - Tocantins (UF = 17):
#        * Série mensal total (Exportações FOB US$, Importações FOB US$ e Saldo calculado)
#        * Top 3 SH4 Exportados e Importados para o painel (apenas nome do produto, sem código SH4)
#        * Top 10 SH4 Exportados e Importados (para os boletins e dossiês)
#        * TODOS os países de destino de exportação e origem de importação (mapeados para ISO-3
#          para colorir integralmente o Mapa-Múndi da seção)
#      - Brasil (Total Nacional, sem países):
#        * Série mensal total (Exportações FOB US$, Importações FOB US$ e Saldo calculado)
#        * Top 10 SH4 Exportados e Importados (para os boletins, dossiês e cartas)
# ------------------------------------------------------------------------------
fmt_usd_fob_br <- function(valor_usd, casas = 2, com_sinal = FALSE) {
  v <- as.numeric(valor_usd)
  if (is.na(v)) return("N/D")
  sinal <- if (com_sinal) { if (v > 0) "+ " else if (v < 0) "- " else "" } else if (v < 0) "- " else ""
  va <- abs(v)
  if (va >= 1e9) {
    paste0(sinal, "US$ ", fmt_num_br(va / 1e9, casas), " Bi")
  } else if (va >= 1e6) {
    paste0(sinal, "US$ ", fmt_num_br(va / 1e6, casas), " Mi")
  } else if (va >= 1e3) {
    paste0(sinal, "US$ ", fmt_num_br(va / 1e3, 1), " mil")
  } else {
    paste0(sinal, "US$ ", fmt_num_br(va, 0))
  }
}

# Remove qualquer código SH4 e ajusta cláusulas aduaneiras extensas para o eixo Y do painel
limpar_nome_sh4_painel <- function(nomes_sh4, quebrar_linha = FALSE) {
  res <- vapply(as.character(nomes_sh4), function(txt) {
    t <- trimws(sub("^[0-9]{4}\\s*[-–:]?\\s*", "", txt))
    # Simplifica prefixo aduaneiro repetitivo mantendo o qualificador químico essencial (azotados, potássicos, etc.)
    t <- sub("^Adubos \\(fertilizantes\\) minerais ou qu[ií]micos,\\s*", "Adubos (fertilizantes) ", t, ignore.case = TRUE)
    # Remove cláusulas aduaneiras secundárias muito longas mantendo o nome oficial da posição SH4
    t <- sub(";.*$", "", t)
    t <- sub(",\\s*de largura igual ou superior.*$", "", t, ignore.case = TRUE)
    t <- sub(",\\s*mesmo refinados.*$", "", t, ignore.case = TRUE)
    t <- sub(",\\s*inclu[ií]d[ao]s?.*$", "", t, ignore.case = TRUE)
    t <- sub("\\s*\\(inclu[ií]d[ao].*?\\)", "", t, ignore.case = TRUE)
    t <- sub(",\\s*em formas brutas.*$", "", t, ignore.case = TRUE)
    t <- sub("\\s*\\(exceto.*$", "", t, ignore.case = TRUE)
    t <- trimws(gsub("\\s+", " ", t))
    if (quebrar_linha && nchar(t) > 30) {
      linhas <- strwrap(t, width = 28)
      if (length(linhas) > 2) {
        linhas <- c(linhas[1], paste0(linhas[2], "..."))
      }
      t <- paste(linhas, collapse = "<br>")
    }
    t
  }, character(1), USE.NAMES = FALSE)
  if (any(duplicated(res))) {
    res <- make.unique(res, sep = " ")
  }
  res
}

extrair_comex_stat_mdic <- function(caminho_cache = "dados/comex_stat_cache.rds") {
  cat("-> Verificando API Oficial Comex Stat / MDIC (https://api-comexstat.mdic.gov.br)...\n")
  cache_atual <- if (file.exists(caminho_cache)) readRDS(caminho_cache) else NULL
  
  info_remota <- tryCatch({
    res_dt <- jsonlite::fromJSON("https://api-comexstat.mdic.gov.br/general/dates/updated")
    res_dt$data
  }, error = function(e) NULL)
  
  tem_kg_cache  <- !is.null(cache_atual$to_exp) && ("metricKG" %in% names(cache_atual$to_exp)) &&
                   !is.null(cache_atual$to_imp) && ("metricKG" %in% names(cache_atual$to_imp))
  tem_mun_cache <- !is.null(cache_atual$to_mun_exp) && !is.null(cache_atual$to_mun_imp)
  
  # Se o cache já está atualizado com o mesmo mês/data do MDIC e possui as 6 tabelas com metricKG, usa imediatamente
  if (!is.null(cache_atual) && !is.null(info_remota) &&
      identical(cache_atual$info$updated, info_remota$updated) &&
      identical(cache_atual$info$year, info_remota$year) &&
      identical(cache_atual$info$monthNumber, info_remota$monthNumber) &&
      tem_kg_cache && tem_mun_cache &&
      !is.null(cache_atual$br_exp_sh4) && !is.null(cache_atual$br_imp_sh4)) {
    cat(sprintf("   [OK] Cache Comex Stat completo (UF, Municipios e Peso KG) sincronizado (%s/%s, atualizado em %s).\n",
                info_remota$monthNumber, info_remota$year, info_remota$updated))
    return(cache_atual)
  }
  
  if (is.null(info_remota)) {
    if (!is.null(cache_atual)) {
      message("   [AVISO] Sem conexao com api-comexstat.mdic.gov.br. Usando cache local.")
      return(cache_atual)
    }
    return(NULL)
  }
  
  post_comex_seguro <- function(body, rotulo, endpoint = "https://api-comexstat.mdic.gov.br/general?language=pt") {
    for (tentativa in 1:6) {
      if (tentativa > 1) Sys.sleep(16)
      res <- tryCatch(
        httr::POST(
          endpoint,
          body = body, encode = "json", httr::content_type_json(), httr::timeout(50)
        ),
        error = function(e) NULL
      )
      if (!is.null(res) && httr::status_code(res) == 200) {
        parsed <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
        df <- parsed$data$list
        if (!is.null(df) && nrow(df) > 0) {
          if ("metricFOB" %in% names(df)) df$metricFOB <- suppressWarnings(as.numeric(df$metricFOB))
          if ("metricKG"  %in% names(df)) df$metricKG  <- suppressWarnings(as.numeric(df$metricKG))
          if (exists("log_ok")) log_ok(paste0("Comex Stat [", rotulo, "]"), sprintf("%d registros extraidos.", nrow(df)), NA_real_, "atualizar_dados.R")
          return(df)
        }
      }
      Sys.sleep(15)
    }
    stop(sprintf("Limite de tentativas excedido na API Comex Stat [%s]", rotulo))
  }
  
  ult_ano <- as.character(info_remota$year)
  ult_mes <- as.character(info_remota$monthNumber)
  ult_ym  <- paste0(ult_ano, "-", ult_mes)
  ano_ant <- as.character(as.integer(ult_ano) - 1L)
  mesma_divulgacao <- !is.null(cache_atual) &&
                      identical(cache_atual$info$updated, info_remota$updated) &&
                      identical(cache_atual$info$year, info_remota$year) &&
                      identical(cache_atual$info$monthNumber, info_remota$monthNumber)
  
  novo_cache <- if (mesma_divulgacao) cache_atual else list(info = info_remota)
  novo_cache$info <- info_remota
  
  tryCatch({
    cat(sprintf("   -> Sincronizando base Comex Stat MDIC c/ Peso Liquido (KG) e Municipios (ate %s)...\n", ult_ym))
    
    if (is.null(novo_cache$to_exp) || !("metricKG" %in% names(novo_cache$to_exp))) {
      Sys.sleep(4)
      novo_cache$to_exp <- post_comex_seguro(list(
        flow = "export", monthDetail = TRUE,
        period = list(from = paste0(ano_ant, "-01"), to = ult_ym),
        filters = list(list(filter = "state", values = list(17))),
        details = list("heading", "country"), metrics = list("metricFOB", "metricKG")
      ), "TO_EXP_FOB_KG")
      saveRDS(novo_cache, caminho_cache)
    }
    
    if (is.null(novo_cache$to_imp) || !("metricKG" %in% names(novo_cache$to_imp))) {
      Sys.sleep(14)
      novo_cache$to_imp <- post_comex_seguro(list(
        flow = "import", monthDetail = TRUE,
        period = list(from = paste0(ano_ant, "-01"), to = ult_ym),
        filters = list(list(filter = "state", values = list(17))),
        details = list("heading", "country"), metrics = list("metricFOB", "metricKG")
      ), "TO_IMP_FOB_KG")
      saveRDS(novo_cache, caminho_cache)
    }
    
    if (is.null(novo_cache$br_exp_sh4) || !("metricKG" %in% names(novo_cache$br_exp_sh4))) {
      Sys.sleep(14)
      novo_cache$br_exp_sh4 <- post_comex_seguro(list(
        flow = "export", monthDetail = TRUE,
        period = list(from = paste0(ult_ano, "-01"), to = ult_ym),
        details = list("heading"), metrics = list("metricFOB", "metricKG")
      ), "BR_EXP_SH4_FOB_KG")
      saveRDS(novo_cache, caminho_cache)
    }
    
    if (is.null(novo_cache$br_imp_sh4) || !("metricKG" %in% names(novo_cache$br_imp_sh4))) {
      Sys.sleep(14)
      novo_cache$br_imp_sh4 <- post_comex_seguro(list(
        flow = "import", monthDetail = TRUE,
        period = list(from = paste0(ult_ano, "-01"), to = ult_ym),
        details = list("heading"), metrics = list("metricFOB", "metricKG")
      ), "BR_IMP_SH4_FOB_KG")
      saveRDS(novo_cache, caminho_cache)
    }
    
    if (is.null(novo_cache$to_mun_exp)) {
      Sys.sleep(14)
      novo_cache$to_mun_exp <- post_comex_seguro(list(
        flow = "export", monthDetail = TRUE,
        period = list(from = paste0(ano_ant, "-01"), to = ult_ym),
        filters = list(list(filter = "state", values = list(17))),
        details = list("city", "country", "heading"), metrics = list("metricFOB", "metricKG")
      ), "TO_MUN_EXP", endpoint = "https://api-comexstat.mdic.gov.br/cities?language=pt")
      saveRDS(novo_cache, caminho_cache)
    }
    
    if (is.null(novo_cache$to_mun_imp)) {
      Sys.sleep(14)
      novo_cache$to_mun_imp <- post_comex_seguro(list(
        flow = "import", monthDetail = TRUE,
        period = list(from = paste0(ano_ant, "-01"), to = ult_ym),
        filters = list(list(filter = "state", values = list(17))),
        details = list("city", "country", "heading"), metrics = list("metricFOB", "metricKG")
      ), "TO_MUN_IMP", endpoint = "https://api-comexstat.mdic.gov.br/cities?language=pt")
      saveRDS(novo_cache, caminho_cache)
    }
    
    saveRDS(novo_cache, caminho_cache)
    novo_cache
  }, error = function(e) {
    if (exists("log_erro_debug")) {
      log_erro_debug("atualizar_dados.R", "Comex Stat API", "https://api-comexstat.mdic.gov.br", conditionMessage(e),
                     "Usando cache parcial/anterior",
                     "Verifique rate-limit da API api-comexstat.mdic.gov.br.")
    } else {
      message(sprintf("   [AVISO] Erro ao atualizar Comex Stat (%s). Usando cache.", e$message))
    }
    if (!is.null(novo_cache$to_exp)) novo_cache else cache_atual
  })
}

comex_bruto <- extrair_comex_stat_mdic()

# Carrega dicionário oficial de países SISCOMEX -> ISO-A3 (3 letras) para o Mapa-Múndi
caminho_dic_paises <- "dados/brutos/PAIS_SISCOMEX.csv"
dic_paises_iso <- if (file.exists(caminho_dic_paises)) {
  read.csv2(caminho_dic_paises, fileEncoding = "Latin1", stringsAsFactors = FALSE)
} else {
  NULL
}

mapear_pais_iso3 <- function(nomes_paises) {
  if (is.null(dic_paises_iso)) return(rep(NA_character_, length(nomes_paises)))
  idx <- match(trimws(as.character(nomes_paises)), trimws(dic_paises_iso$NO_PAIS))
  iso <- dic_paises_iso$CO_PAIS_ISOA3[idx]
  iso[iso == "ZZZ"] <- NA_character_
  iso
}

# Inicializa variáveis analíticas adicionais de Comércio Exterior
df_to_mun_exp_mes         <- NULL
df_to_mun_imp_mes         <- NULL
df_to_mun_exp_ano         <- NULL
df_to_mun_imp_ano         <- NULL
df_to_top3_exp_paises_mes <- NULL
df_to_top3_imp_paises_mes <- NULL
df_to_top3_exp_paises_ano <- NULL
df_to_top3_imp_paises_ano <- NULL
df_comex_termos_troca_to  <- NULL

# Processa as tabelas do Comex Stat (Tocantins e Brasil)
if (!is.null(comex_bruto) && !is.null(comex_bruto$to_exp) && !is.null(comex_bruto$to_imp)) {
  ult_ano_cx <- as.character(comex_bruto$info$year)
  ult_mes_cx <- as.character(comex_bruto$info$monthNumber)
  periodo_comex_ref <- sprintf("%s/%s", ult_mes_cx, ult_ano_cx)
  
  df_to_e <- comex_bruto$to_exp
  df_to_i <- comex_bruto$to_imp
  df_br_e <- comex_bruto$br_exp_sh4
  df_br_i <- comex_bruto$br_imp_sh4
  if (!("metricKG" %in% names(df_to_e))) df_to_e$metricKG <- NA_real_
  if (!("metricKG" %in% names(df_to_i))) df_to_i$metricKG <- NA_real_
  if (!is.null(df_br_e) && !("metricKG" %in% names(df_br_e))) df_br_e$metricKG <- NA_real_
  if (!is.null(df_br_i) && !("metricKG" %in% names(df_br_i))) df_br_i$metricKG <- NA_real_
  
  # 1. Série Mensal Total, Peso Líquido (KG), Preço Médio (US$/Kg) e Saldo Comercial — TOCANTINS
  mensal_to_e <- df_to_e %>%
    group_by(year, monthNumber) %>%
    summarise(
      Exportacoes_FOB_USD = sum(metricFOB, na.rm = TRUE),
      Exportacoes_KG      = sum(as.numeric(metricKG), na.rm = TRUE),
      .groups = "drop"
    )
  mensal_to_i <- df_to_i %>%
    group_by(year, monthNumber) %>%
    summarise(
      Importacoes_FOB_USD = sum(metricFOB, na.rm = TRUE),
      Importacoes_KG      = sum(as.numeric(metricKG), na.rm = TRUE),
      .groups = "drop"
    )
  
  df_comex_to_mensal <- full_join(mensal_to_e, mensal_to_i, by = c("year", "monthNumber")) %>%
    mutate(
      Exportacoes_FOB_USD    = coalesce(Exportacoes_FOB_USD, 0),
      Importacoes_FOB_USD    = coalesce(Importacoes_FOB_USD, 0),
      Exportacoes_KG         = coalesce(Exportacoes_KG, 0),
      Importacoes_KG         = coalesce(Importacoes_KG, 0),
      Exportacoes_Ton        = round(Exportacoes_KG / 1000, 2),
      Importacoes_Ton        = round(Importacoes_KG / 1000, 2),
      Saldo_FOB_USD          = Exportacoes_FOB_USD - Importacoes_FOB_USD,
      Corrente_FOB_USD       = Exportacoes_FOB_USD + Importacoes_FOB_USD,
      Preco_Medio_Exp_USD_Kg = ifelse(Exportacoes_KG > 0, round(Exportacoes_FOB_USD / Exportacoes_KG, 3), NA_real_),
      Preco_Medio_Imp_USD_Kg = ifelse(Importacoes_KG > 0, round(Importacoes_FOB_USD / Importacoes_KG, 3), NA_real_),
      Kg_Por_Mil_USD_Exp     = ifelse(Exportacoes_FOB_USD > 0, round((Exportacoes_KG / Exportacoes_FOB_USD) * 1000, 2), NA_real_),
      Kg_Por_Mil_USD_Imp     = ifelse(Importacoes_FOB_USD > 0, round((Importacoes_KG / Importacoes_FOB_USD) * 1000, 2), NA_real_),
      Periodo                = paste0(monthNumber, "/", year)
    ) %>%
    arrange(year, monthNumber) %>%
    as.data.frame()
  
  # Tabela síntese do Indicador de Densidade de Valor (US$ FOB / Kg e Termos de Troca) do Tocantins
  df_comex_termos_troca_to <- df_comex_to_mensal %>%
    mutate(
      Preco_Medio_Exp_USD_Ton = round(Preco_Medio_Exp_USD_Kg * 1000, 2),
      Preco_Medio_Imp_USD_Ton = round(Preco_Medio_Imp_USD_Kg * 1000, 2),
      Razao_Termos_Troca      = ifelse(!is.na(Preco_Medio_Imp_USD_Kg) & Preco_Medio_Imp_USD_Kg > 0,
                                       round(Preco_Medio_Exp_USD_Kg / Preco_Medio_Imp_USD_Kg, 3), NA_real_)
    )
  
  # Valores do último mês de referência para as 3 ValueBoxes do painel (Tocantins)
  ult_linha_to <- tail(df_comex_to_mensal, 1)
  val_exp_to_mes_usd   <- ult_linha_to$Exportacoes_FOB_USD[1]
  val_imp_to_mes_usd   <- ult_linha_to$Importacoes_FOB_USD[1]
  val_saldo_to_mes_usd <- ult_linha_to$Saldo_FOB_USD[1] # Calculado: Exportações - Importações
  
  str_vb_exp_to   <- fmt_usd_fob_br(val_exp_to_mes_usd, casas = 2, com_sinal = FALSE)
  str_vb_imp_to   <- fmt_usd_fob_br(val_imp_to_mes_usd, casas = 2, com_sinal = FALSE)
  str_vb_saldo_to <- fmt_usd_fob_br(val_saldo_to_mes_usd, casas = 2, com_sinal = TRUE)
  
  # 2. Série Mensal Total e Saldo Comercial Calculado — BRASIL (US$ FOB e KG)
  mensal_br_e <- df_br_e %>%
    group_by(year, monthNumber) %>%
    summarise(
      Exportacoes_FOB_USD = sum(metricFOB, na.rm = TRUE),
      Exportacoes_KG      = sum(as.numeric(metricKG), na.rm = TRUE),
      .groups = "drop"
    )
  mensal_br_i <- df_br_i %>%
    group_by(year, monthNumber) %>%
    summarise(
      Importacoes_FOB_USD = sum(metricFOB, na.rm = TRUE),
      Importacoes_KG      = sum(as.numeric(metricKG), na.rm = TRUE),
      .groups = "drop"
    )
  
  df_comex_br_mensal <- full_join(mensal_br_e, mensal_br_i, by = c("year", "monthNumber")) %>%
    mutate(
      Exportacoes_FOB_USD    = coalesce(Exportacoes_FOB_USD, 0),
      Importacoes_FOB_USD    = coalesce(Importacoes_FOB_USD, 0),
      Exportacoes_KG         = coalesce(Exportacoes_KG, 0),
      Importacoes_KG         = coalesce(Importacoes_KG, 0),
      Saldo_FOB_USD          = Exportacoes_FOB_USD - Importacoes_FOB_USD,
      Corrente_FOB_USD       = Exportacoes_FOB_USD + Importacoes_FOB_USD,
      Preco_Medio_Exp_USD_Kg = ifelse(Exportacoes_KG > 0, round(Exportacoes_FOB_USD / Exportacoes_KG, 3), NA_real_),
      Preco_Medio_Imp_USD_Kg = ifelse(Importacoes_KG > 0, round(Importacoes_FOB_USD / Importacoes_KG, 3), NA_real_),
      Periodo                = paste0(monthNumber, "/", year)
    ) %>%
    arrange(year, monthNumber) %>%
    as.data.frame()
  
  # 3. Função para gerar Top N Posições SH4 (com Valor FOB US$, Peso Líquido KG/Ton e US$/Kg)
  gerar_ranking_sh4 <- function(df_fluxo, top_n = 10, apenas_ultimo_mes = TRUE) {
    df_rec <- if (apenas_ultimo_mes) {
      df_fluxo %>% filter(year == ult_ano_cx, monthNumber == ult_mes_cx)
    } else {
      df_fluxo %>% filter(year == ult_ano_cx)
    }
    if (!("metricKG" %in% names(df_rec))) df_rec$metricKG <- NA_real_
    tot_fob <- sum(df_rec$metricFOB, na.rm = TRUE)
    tot_kg  <- sum(as.numeric(df_rec$metricKG), na.rm = TRUE)
    df_rank <- df_rec %>%
      group_by(headingCode, heading) %>%
      summarise(
        Valor_FOB_USD = sum(metricFOB, na.rm = TRUE),
        Peso_Kg       = sum(as.numeric(metricKG), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Valor_FOB_USD)) %>%
      head(top_n) %>%
      mutate(
        SH4_Codigo          = as.character(headingCode),
        Produto_SH4         = trimws(sub("^[0-9]{4}\\s*[-–:]?\\s*", "", as.character(heading))),
        Produto_Curto       = limpar_nome_sh4_painel(heading, quebrar_linha = FALSE),
        Produto_Painel      = limpar_nome_sh4_painel(heading, quebrar_linha = TRUE),
        Valor_FOB_Mi        = round(Valor_FOB_USD / 1e6, 2),
        Participacao_Pct    = if (tot_fob > 0) round((Valor_FOB_USD / tot_fob) * 100, 2) else 0,
        Peso_Ton            = round(Peso_Kg / 1000, 2),
        Part_Peso_Pct       = if (tot_kg > 0) round((Peso_Kg / tot_kg) * 100, 2) else 0,
        Preco_Medio_USD_Kg  = ifelse(Peso_Kg > 0, round(Valor_FOB_USD / Peso_Kg, 2), NA_real_),
        Preco_Medio_USD_Ton = ifelse(Peso_Kg > 0, round((Valor_FOB_USD / Peso_Kg) * 1000, 2), NA_real_),
        Kg_Por_Mil_USD_FOB  = ifelse(Valor_FOB_USD > 0, round((Peso_Kg / Valor_FOB_USD) * 1000, 2), NA_real_)
      ) %>%
      select(
        SH4_Codigo, Produto_SH4, Produto_Curto, Produto_Painel,
        Valor_FOB_USD, Valor_FOB_Mi, Participacao_Pct,
        Peso_Kg, Peso_Ton, Part_Peso_Pct, Preco_Medio_USD_Kg, Preco_Medio_USD_Ton, Kg_Por_Mil_USD_FOB
      ) %>%
      as.data.frame()
    df_rank
  }
  
  # Top 10 SH4 Tocantins (Último Mês e Acumulado no Ano)
  df_to_top10_exp_sh4_mes <- gerar_ranking_sh4(df_to_e, top_n = 10, apenas_ultimo_mes = TRUE)
  df_to_top10_imp_sh4_mes <- gerar_ranking_sh4(df_to_i, top_n = 10, apenas_ultimo_mes = TRUE)
  df_to_top10_exp_sh4_ano <- gerar_ranking_sh4(df_to_e, top_n = 10, apenas_ultimo_mes = FALSE)
  df_to_top10_imp_sh4_ano <- gerar_ranking_sh4(df_to_i, top_n = 10, apenas_ultimo_mes = FALSE)
  
  # Top 10 SH4 Brasil (Último Mês e Acumulado no Ano)
  df_br_top10_exp_sh4_mes <- gerar_ranking_sh4(df_br_e, top_n = 10, apenas_ultimo_mes = TRUE)
  df_br_top10_imp_sh4_mes <- gerar_ranking_sh4(df_br_i, top_n = 10, apenas_ultimo_mes = TRUE)
  df_br_top10_exp_sh4_ano <- gerar_ranking_sh4(df_br_e, top_n = 10, apenas_ultimo_mes = FALSE)
  df_br_top10_imp_sh4_ano <- gerar_ranking_sh4(df_br_i, top_n = 10, apenas_ultimo_mes = FALSE)
  
  # Top 3 SH4 Tocantins para o Painel (sem código SH4, ordenado ascendente para que o 1º fique no topo do gráfico horizontal)
  top3_e_raw <- head(df_to_top10_exp_sh4_mes, 3)
  top3_e_ord <- top3_e_raw[order(top3_e_raw$Valor_FOB_USD), ]
  df_exp_top3_painel <- data.frame(
    Produto          = top3_e_ord$Produto_Painel,
    Produto_Completo = top3_e_ord$Produto_SH4,
    Valor            = round(top3_e_ord$Valor_FOB_USD / 1e6, 2),
    Valor_FOB_USD    = top3_e_ord$Valor_FOB_USD,
    Rotulo           = paste0("US$ ", fmt_num_br(top3_e_ord$Valor_FOB_USD / 1e6, 2), " Mi"),
    stringsAsFactors = FALSE
  )
  
  top3_i_raw <- head(df_to_top10_imp_sh4_mes, 3)
  top3_i_ord <- top3_i_raw[order(top3_i_raw$Valor_FOB_USD), ]
  df_imp_top3_painel <- data.frame(
    Produto          = top3_i_ord$Produto_Painel,
    Produto_Completo = top3_i_ord$Produto_SH4,
    Valor            = round(top3_i_ord$Valor_FOB_USD / 1e6, 2),
    Valor_FOB_USD    = top3_i_ord$Valor_FOB_USD,
    Rotulo           = paste0("US$ ", fmt_num_br(top3_i_ord$Valor_FOB_USD / 1e6, 2), " Mi"),
    stringsAsFactors = FALSE
  )
  
  # 4. TODOS os Países Parceiros do Tocantins (Exportação e Importação) com Peso KG e US$/Kg
  agrupar_paises_to <- function(df_fluxo, apenas_ultimo_mes = TRUE) {
    df_rec <- if (apenas_ultimo_mes) {
      df_fluxo %>% filter(year == ult_ano_cx, monthNumber == ult_mes_cx)
    } else {
      df_fluxo %>% filter(year == ult_ano_cx)
    }
    if (!("metricKG" %in% names(df_rec))) df_rec$metricKG <- NA_real_
    tot_fob <- sum(df_rec$metricFOB, na.rm = TRUE)
    tot_kg  <- sum(as.numeric(df_rec$metricKG), na.rm = TRUE)
    df_p <- df_rec %>%
      group_by(country) %>%
      summarise(
        Valor_FOB_USD = sum(metricFOB, na.rm = TRUE),
        Peso_Kg       = sum(as.numeric(metricKG), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(Valor_FOB_USD > 0) %>%
      arrange(desc(Valor_FOB_USD)) %>%
      mutate(
        Pais               = mapear_pais_iso3(country),
        Nome               = as.character(country),
        Valor              = round(Valor_FOB_USD / 1e6, 3),
        Participacao_Pct   = if (tot_fob > 0) round((Valor_FOB_USD / tot_fob) * 100, 2) else 0,
        Peso_Ton           = round(Peso_Kg / 1000, 2),
        Part_Peso_Pct      = if (tot_kg > 0) round((Peso_Kg / tot_kg) * 100, 2) else 0,
        Preco_Medio_USD_Kg = ifelse(Peso_Kg > 0, round(Valor_FOB_USD / Peso_Kg, 2), NA_real_)
      ) %>%
      filter(!is.na(Pais) & nzchar(Pais)) %>%
      select(Pais, Nome, Valor, Valor_FOB_USD, Participacao_Pct, Peso_Kg, Peso_Ton, Part_Peso_Pct, Preco_Medio_USD_Kg) %>%
      as.data.frame()
    df_p
  }
  
  df_to_paises_exp_mes <- agrupar_paises_to(df_to_e, apenas_ultimo_mes = TRUE)
  df_to_paises_imp_mes <- agrupar_paises_to(df_to_i, apenas_ultimo_mes = TRUE)
  df_to_paises_exp_ano <- agrupar_paises_to(df_to_e, apenas_ultimo_mes = FALSE)
  df_to_paises_imp_ano <- agrupar_paises_to(df_to_i, apenas_ultimo_mes = FALSE)
  
  # 5. Matriz Cruzada: Top 3 Produtos SH4 de Exportação e Importação x Países Parceiros (Carta de Conjuntura)
  gerar_top3_produtos_por_pais <- function(df_fluxo, top_prod = 3, top_paises = 5, apenas_ultimo_mes = FALSE) {
    df_rec <- if (apenas_ultimo_mes) {
      df_fluxo %>% filter(year == ult_ano_cx, monthNumber == ult_mes_cx)
    } else {
      df_fluxo %>% filter(year == ult_ano_cx)
    }
    if (nrow(df_rec) == 0) return(NULL)
    if (!("metricKG" %in% names(df_rec))) df_rec$metricKG <- NA_real_
    tot_uf_fob <- sum(df_rec$metricFOB, na.rm = TRUE)
    top_sh4 <- df_rec %>%
      group_by(headingCode, heading) %>%
      summarise(Prod_FOB = sum(metricFOB, na.rm = TRUE), Prod_KG = sum(as.numeric(metricKG), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Prod_FOB)) %>%
      head(top_prod)
    if (nrow(top_sh4) == 0) return(NULL)
    res_list <- lapply(seq_len(nrow(top_sh4)), function(i) {
      cod_h  <- top_sh4$headingCode[i]
      nom_h  <- limpar_nome_sh4_painel(top_sh4$heading[i], quebrar_linha = FALSE)
      fob_pr <- top_sh4$Prod_FOB[i]
      df_sub <- df_rec %>%
        filter(headingCode == cod_h) %>%
        group_by(country) %>%
        summarise(
          Valor_FOB_USD = sum(metricFOB, na.rm = TRUE),
          Peso_Kg       = sum(as.numeric(metricKG), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Valor_FOB_USD)) %>%
        head(top_paises) %>%
        mutate(
          Posicao_Produto      = i,
          SH4_Codigo           = as.character(cod_h),
          Produto_Curto        = nom_h,
          Pais_Parceiro        = as.character(country),
          Valor_FOB_Mi         = round(Valor_FOB_USD / 1e6, 2),
          Peso_Ton             = round(Peso_Kg / 1000, 2),
          Preco_Medio_USD_Kg   = ifelse(Peso_Kg > 0, round(Valor_FOB_USD / Peso_Kg, 2), NA_real_),
          Part_No_Produto_Pct  = if (fob_pr > 0) round((Valor_FOB_USD / fob_pr) * 100, 2) else 0,
          Part_No_Total_UF_Pct = if (tot_uf_fob > 0) round((Valor_FOB_USD / tot_uf_fob) * 100, 2) else 0
        ) %>%
        select(
          Posicao_Produto, SH4_Codigo, Produto_Curto, Pais_Parceiro,
          Valor_FOB_USD, Valor_FOB_Mi, Peso_Kg, Peso_Ton, Preco_Medio_USD_Kg,
          Part_No_Produto_Pct, Part_No_Total_UF_Pct
        ) %>%
        as.data.frame()
      df_sub
    })
    do.call(rbind, res_list)
  }
  
  df_to_top3_exp_paises_mes <- gerar_top3_produtos_por_pais(df_to_e, top_prod = 3, top_paises = 5, apenas_ultimo_mes = TRUE)
  df_to_top3_imp_paises_mes <- gerar_top3_produtos_por_pais(df_to_i, top_prod = 3, top_paises = 5, apenas_ultimo_mes = TRUE)
  df_to_top3_exp_paises_ano <- gerar_top3_produtos_por_pais(df_to_e, top_prod = 3, top_paises = 5, apenas_ultimo_mes = FALSE)
  df_to_top3_imp_paises_ano <- gerar_top3_produtos_por_pais(df_to_i, top_prod = 3, top_paises = 5, apenas_ultimo_mes = FALSE)
  
  # 6. Top 10 Municípios Exportadores e Importadores do Tocantins (Carta de Conjuntura)
  gerar_ranking_municipios_comex <- function(df_mun_fluxo, top_n = 10, apenas_ultimo_mes = FALSE) {
    if (is.null(df_mun_fluxo) || nrow(df_mun_fluxo) == 0) return(NULL)
    df_rec <- if (apenas_ultimo_mes) {
      df_mun_fluxo %>% filter(year == ult_ano_cx, monthNumber == ult_mes_cx)
    } else {
      df_mun_fluxo %>% filter(year == ult_ano_cx)
    }
    if (nrow(df_rec) == 0) return(NULL)
    if (!("metricKG" %in% names(df_rec))) df_rec$metricKG <- NA_real_
    tot_fob <- sum(df_rec$metricFOB, na.rm = TRUE)
    tot_kg  <- sum(as.numeric(df_rec$metricKG), na.rm = TRUE)
    
    # Principal produto e principal país de cada município
    prod_principal <- df_rec %>%
      group_by(noMunMinsgUf, heading) %>%
      summarise(FOB = sum(metricFOB, na.rm = TRUE), .groups = "drop") %>%
      arrange(noMunMinsgUf, desc(FOB)) %>%
      group_by(noMunMinsgUf) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(noMunMinsgUf, Principal_Produto = limpar_nome_sh4_painel(heading, quebrar_linha = FALSE))
    
    pais_principal <- df_rec %>%
      group_by(noMunMinsgUf, country) %>%
      summarise(FOB = sum(metricFOB, na.rm = TRUE), .groups = "drop") %>%
      arrange(noMunMinsgUf, desc(FOB)) %>%
      group_by(noMunMinsgUf) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(noMunMinsgUf, Principal_Pais = as.character(country))
    
    df_m <- df_rec %>%
      group_by(noMunMinsgUf) %>%
      summarise(
        Valor_FOB_USD = sum(metricFOB, na.rm = TRUE),
        Peso_Kg       = sum(as.numeric(metricKG), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(Valor_FOB_USD > 0) %>%
      arrange(desc(Valor_FOB_USD)) %>%
      head(top_n) %>%
      left_join(prod_principal, by = "noMunMinsgUf") %>%
      left_join(pais_principal, by = "noMunMinsgUf") %>%
      mutate(
        Posicao            = row_number(),
        Municipio          = sub("\\s*-\\s*TO$", "", as.character(noMunMinsgUf)),
        Valor_FOB_Mi       = round(Valor_FOB_USD / 1e6, 2),
        Participacao_Pct   = if (tot_fob > 0) round((Valor_FOB_USD / tot_fob) * 100, 2) else 0,
        Peso_Ton           = round(Peso_Kg / 1000, 2),
        Part_Peso_Pct      = if (tot_kg > 0) round((Peso_Kg / tot_kg) * 100, 2) else 0,
        Preco_Medio_USD_Kg = ifelse(Peso_Kg > 0, round(Valor_FOB_USD / Peso_Kg, 2), NA_real_)
      ) %>%
      select(
        Posicao, Municipio, Valor_FOB_USD, Valor_FOB_Mi, Participacao_Pct,
        Peso_Kg, Peso_Ton, Part_Peso_Pct, Preco_Medio_USD_Kg,
        Principal_Produto, Principal_Pais
      ) %>%
      as.data.frame()
    df_m
  }
  
  df_to_mun_exp_mes <- gerar_ranking_municipios_comex(comex_bruto$to_mun_exp, top_n = 10, apenas_ultimo_mes = TRUE)
  df_to_mun_imp_mes <- gerar_ranking_municipios_comex(comex_bruto$to_mun_imp, top_n = 10, apenas_ultimo_mes = TRUE)
  df_to_mun_exp_ano <- gerar_ranking_municipios_comex(comex_bruto$to_mun_exp, top_n = 10, apenas_ultimo_mes = FALSE)
  df_to_mun_imp_ano <- gerar_ranking_municipios_comex(comex_bruto$to_mun_imp, top_n = 10, apenas_ultimo_mes = FALSE)
  
  # Construção inteligente das camadas do Mapa-Múndi (evita que a camada de importação pinte por cima
  # dos grandes destinos exportadores como a China, exibindo ambos os fluxos no tooltip quando houver):
  df_mapa_join <- full_join(
    df_to_paises_exp_mes[, c("Pais", "Nome", "Valor_FOB_USD")],
    df_to_paises_imp_mes[, c("Pais", "Nome", "Valor_FOB_USD")],
    by = "Pais", suffix = c("_Exp", "_Imp")
  ) %>%
    mutate(
      Nome      = coalesce(Nome_Exp, Nome_Imp),
      Exp_USD   = coalesce(Valor_FOB_USD_Exp, 0),
      Imp_USD   = coalesce(Valor_FOB_USD_Imp, 0),
      Saldo_USD = Exp_USD - Imp_USD
    )
  
  montar_hover_pais <- function(nome, exp_usd, imp_usd, saldo_usd) {
    linhas <- paste0("<b>", nome, "</b>")
    if (exp_usd > 0) linhas <- paste0(linhas, "<br>Exportações TO: ", fmt_usd_fob_br(exp_usd, 2))
    if (imp_usd > 0) linhas <- paste0(linhas, "<br>Importações TO: ", fmt_usd_fob_br(imp_usd, 2))
    status_str <- if (saldo_usd > 0) {
      paste0("<br><span style='color:#b8860b;'><b>Saldo:</b> ", fmt_usd_fob_br(saldo_usd, 2, com_sinal = TRUE), " (Exportações superam)</span>")
    } else if (saldo_usd < 0) {
      paste0("<br><span style='color:#0f2537;'><b>Saldo:</b> ", fmt_usd_fob_br(saldo_usd, 2, com_sinal = TRUE), " (Importações superam)</span>")
    } else {
      "<br><b>Saldo:</b> US$ 0 (Equilíbrio)"
    }
    paste0(linhas, status_str)
  }
  
  df_mapa_join$Hover <- mapply(montar_hover_pais, df_mapa_join$Nome, df_mapa_join$Exp_USD, df_mapa_join$Imp_USD, df_mapa_join$Saldo_USD, USE.NAMES = FALSE)
  
  # Regra de Cores no Mapa-Múndi:
  # - Amarelo: países onde as Exportações excedem as Importações (Exp_USD >= Imp_USD & Exp_USD > 0)
  # - Azul Escuro: países onde as Importações excedem as Exportações (Imp_USD > Exp_USD & Imp_USD > 0)
  df_exp_mapa_painel <- df_mapa_join %>%
    filter(Exp_USD >= Imp_USD & Exp_USD > 0) %>%
    arrange(desc(Exp_USD)) %>%
    transmute(
      Pais          = Pais,
      Nome          = Nome,
      Valor         = round(Exp_USD / 1e6, 2),
      Valor_FOB_USD = Exp_USD,
      Hover         = Hover
    ) %>%
    as.data.frame()
  
  df_imp_mapa_painel <- df_mapa_join %>%
    filter(Imp_USD > Exp_USD & Imp_USD > 0) %>%
    arrange(desc(Imp_USD)) %>%
    transmute(
      Pais          = Pais,
      Nome          = Nome,
      Valor         = round(Imp_USD / 1e6, 2),
      Valor_FOB_USD = Imp_USD,
      Hover         = Hover
    ) %>%
    as.data.frame()
} else if (!is.null(dados_antigos) && !is.null(dados_antigos$comex)) {
  periodo_comex_ref        <- "09/2026"
  str_vb_exp_to            <- dados_antigos$comex$vb_exportacoes
  str_vb_imp_to            <- dados_antigos$comex$vb_importacoes
  str_vb_saldo_to          <- dados_antigos$comex$vb_saldo
  df_exp_mapa_painel       <- dados_antigos$comex$df_exp_mapa
  df_imp_mapa_painel       <- dados_antigos$comex$df_imp_mapa
  df_exp_top3_painel       <- dados_antigos$comex$df_exp_top3
  df_imp_top3_painel       <- dados_antigos$comex$df_imp_top3
  df_comex_to_mensal       <- cache_extra$comex_to_mensal
  df_comex_br_mensal       <- cache_extra$comex_br_mensal
  df_to_top10_exp_sh4_mes  <- cache_extra$comex_to_top10_exp_sh4
  df_to_top10_imp_sh4_mes  <- cache_extra$comex_to_top10_imp_sh4
  df_to_top10_exp_sh4_ano  <- cache_extra$comex_to_top10_exp_sh4_ano
  df_to_top10_imp_sh4_ano  <- cache_extra$comex_to_top10_imp_sh4_ano
  df_br_top10_exp_sh4_mes  <- cache_extra$comex_br_top10_exp_sh4
  df_br_top10_imp_sh4_mes  <- cache_extra$comex_br_top10_imp_sh4
  df_br_top10_exp_sh4_ano  <- cache_extra$comex_br_top10_exp_sh4_ano
  df_br_top10_imp_sh4_ano  <- cache_extra$comex_br_top10_imp_sh4_ano
  df_to_paises_exp_mes      <- cache_extra$comex_to_paises_exp
  df_to_paises_imp_mes      <- cache_extra$comex_to_paises_imp
  df_to_paises_exp_ano      <- cache_extra$comex_to_paises_exp_ano
  df_to_paises_imp_ano      <- cache_extra$comex_to_paises_imp_ano
  df_to_mun_exp_mes         <- cache_extra$comex_to_mun_exp
  df_to_mun_imp_mes         <- cache_extra$comex_to_mun_imp
  df_to_mun_exp_ano         <- cache_extra$comex_to_mun_exp_ano
  df_to_mun_imp_ano         <- cache_extra$comex_to_mun_imp_ano
  df_to_top3_exp_paises_mes <- cache_extra$comex_to_top3_exp_por_pais
  df_to_top3_imp_paises_mes <- cache_extra$comex_to_top3_imp_por_pais
  df_to_top3_exp_paises_ano <- cache_extra$comex_to_top3_exp_por_pais_ano
  df_to_top3_imp_paises_ano <- cache_extra$comex_to_top3_imp_por_pais_ano
  df_comex_termos_troca_to  <- cache_extra$comex_termos_troca_to
}

# ------------------------------------------------------------------------------
# 1.4-B. PESQUISA 4-B: ARRECADAÇÃO E FINANÇAS PÚBLICAS (SEFAZ-TO, CONFAZ E STN/FPE)
#        - Script dedicado: scripts/extrair_arrecadacao_to.R
#        - Fontes: Boletim Tributário SEFAZ-TO (PDFs anuais/mensais),
#                  Boletim de Arrecadação dos Tributos Estaduais (CONFAZ / Dados Abertos) e
#                  Transferências Obrigatórias da União - FPE (Tesouro Transparente / STN)
#        - Armazenamento enxuto em 'dados/arrecadacao_to_resumo.rds' e 'dados/arrecadacao_to_base.rds'
# ------------------------------------------------------------------------------
resumo_arrecadacao_to <- tryCatch({
  source("scripts/extrair_arrecadacao_to.R", local = TRUE)
  if (exists("resumo_arrecadacao_to") && !is.null(resumo_arrecadacao_to)) {
    resumo_arrecadacao_to
  } else if (file.exists("dados/arrecadacao_to_resumo.rds")) {
    readRDS("dados/arrecadacao_to_resumo.rds")
  } else {
    NULL
  }
}, error = function(e) {
  message(sprintf("   [AVISO] Erro ao executar extrair_arrecadacao_to.R (%s). Usando cache local.", e$message))
  if (file.exists("dados/arrecadacao_to_resumo.rds")) readRDS("dados/arrecadacao_to_resumo.rds") else NULL
})

# ------------------------------------------------------------------------------
# 1.4-C. PESQUISA 4-C: DINÂMICA EMPRESARIAL — CNPJ, SIMPLES, SÓCIOS E MAPA DE EMPRESAS
#        - Script dedicado: scripts/extrair_cnpj_to.R
#        - Fontes: Receita Federal do Brasil / SERPRO (Base CNPJ, Estabelecimentos,
#                  Capital Social, CNAE 2.0 e Sócios QSA), Portal SINAC/Simples Nacional
#                  e Painel Mapa de Empresas (REDESIM — Tempos de Abertura, Viabilidade e Registro)
#        - Armazenamento enxuto em 'dados/cnpj_to_resumo.rds' e 'dados/cnpj_to_base.rds'
# ------------------------------------------------------------------------------
resumo_cnpj_to <- tryCatch({
  arq_cnpj_res <- "dados/cnpj_to_resumo.rds"
  if (file.exists(arq_cnpj_res) && Sys.getenv("FORCAR_CNPJ_TO") != "1" &&
      as.numeric(difftime(Sys.time(), file.info(arq_cnpj_res)$mtime, units = "hours")) < 24) {
    cat("-> [OK] Base CNPJ / Simples / QSA / Mapa de Empresas ja sincronizada nas ultimas 24h (dados/cnpj_to_resumo.rds).\n")
    if (exists("log_ok")) log_ok("CNPJ / Simples / QSA / Mapa Empresas", "Cache RDS sincronizado (< 24h)", 0.05, "atualizar_dados.R")
    readRDS(arq_cnpj_res)
  } else {
    source("scripts/extrair_cnpj_to.R", local = TRUE)
    extrair_cnpj_tocantins(diretorio_raiz = getwd(), forcar_atualizacao = TRUE)
  }
}, error = function(e) {
  message(sprintf("   [AVISO] Erro ao executar extrair_cnpj_to.R (%s). Usando cache local.", e$message))
  if (file.exists("dados/cnpj_to_resumo.rds")) readRDS("dados/cnpj_to_resumo.rds") else NULL
})

# ------------------------------------------------------------------------------
# 1.4-D. PESQUISA 4-D: PRODUÇÃO AGROPECUÁRIA, FLORESTAL E PROTEÍNA ANIMAL (IBGE/SIDRA)
#        - Script dedicado: scripts/extrair_agro_to.R
#        - Fontes: LSPA (6588), PAM (5457 UF e Municípios), PPM (3939 Efetivos UF/Mun e 74),
#                  PEVS (5930, 291, 289), Abate Trimestral (1092, 1093, 1094),
#                  Leite Trimestral (1086) e Ovos de Galinha Trimestral (915).
#        - Armazenamento enxuto em 'dados/agro_to_resumo.rds'
# ------------------------------------------------------------------------------
resumo_agro_to <- tryCatch({
  .chamado_por_atualizar_dados <- TRUE
  if (file.exists("scripts/extrair_agro_to.R")) {
    source("scripts/extrair_agro_to.R", local = TRUE)
    extrair_agro_tocantins(diretorio_raiz = getwd(), forcar_atualizacao = FALSE)
  } else if (file.exists("dados/agro_to_resumo.rds")) {
    readRDS("dados/agro_to_resumo.rds")
  } else {
    NULL
  }
}, error = function(e) {
  message(sprintf("   [AVISO] Erro ao executar extrair_agro_to.R (%s). Usando cache local.", e$message))
  if (file.exists("dados/agro_to_resumo.rds")) readRDS("dados/agro_to_resumo.rds") else NULL
})

# ------------------------------------------------------------------------------
# 1.5. PESQUISA 5: BANCO CENTRAL DO BRASIL (SGS/BCB - Selic e IPCA)
# ------------------------------------------------------------------------------
buscar_serie_bcb <- function(codigo_sgs, ultimos_n = 12) {
  url <- sprintf(
    "https://api.bcb.gov.br/dados/serie/bcdata.sgs.%d/dados/ultimos/%d?formato=json",
    codigo_sgs, ultimos_n
  )
  tryCatch({
    df <- jsonlite::fromJSON(url)
    df$valor <- as.numeric(df$valor)
    return(df)
  }, error = function(e) {
    message(sprintf("Aviso: Nao foi possivel consultar serie SGS %d (%s). Usando cache.", codigo_sgs, e$message))
    return(NULL)
  })
}

serie_selic <- buscar_serie_bcb(432, ultimos_n = 1)
selic_atual <- if (!is.null(serie_selic) && nrow(serie_selic) > 0) {
  paste0(format(tail(serie_selic$valor, 1), decimal.mark = ",", nsmall = 2), "% a.a.")
} else {
  "10,75% a.a."
}

serie_ipca <- buscar_serie_bcb(13522, ultimos_n = 1)
ipca_12m_atual <- if (!is.null(serie_ipca) && nrow(serie_ipca) > 0) {
  paste0(format(tail(serie_ipca$valor, 1), decimal.mark = ",", nsmall = 2), "%")
} else {
  "4,42%"
}

# ==============================================================================
# 2. ESTRUTURAÇÃO DOS DADOS POR ABA DO PAINEL (ALIMENTADO PELAS EXTRAÇÕES ACIMA)
# ==============================================================================

trimestres <- c("T3 2024", "T4 2024", "T1 2025", "T2 2025", "T3 2025", "T4 2025", "T1 2026")

# --- ABA 1: VISÃO GERAL ---
str_vb_icms_var_vg <- if (!is.null(resumo_arrecadacao_to) && !is.null(resumo_arrecadacao_to$vb_icms_var)) {
  resumo_arrecadacao_to$vb_icms_var
} else {
  "-5,25%"
}

serie_var_icms_trim_painel <- c(2.1, 2.4, 2.8, 3.5, 3.2, 3.8, 4.1)
if (!is.null(resumo_arrecadacao_to) && !is.null(resumo_arrecadacao_to$df_icms_trimestral)) {
  df_it <- resumo_arrecadacao_to$df_icms_trimestral
  df_it$Var_Ref <- coalesce(df_it$Var_YoY_Pct, df_it$Var_QoQ_Pct)
  idx_it <- match(trimestres, df_it$Trimestre_Rotulo)
  vals_it <- df_it$Var_Ref[idx_it]
  if (any(!is.na(vals_it))) {
    serie_var_icms_trim_painel <- ifelse(is.na(vals_it), serie_var_icms_trim_painel, round(vals_it, 2))
  }
}

str_vb_novos_cnpjs_vg <- if (!is.null(resumo_cnpj_to) && !is.null(resumo_cnpj_to$indicadores_painel$vb_novos_cnpjs)) {
  resumo_cnpj_to$indicadores_painel$vb_novos_cnpjs
} else {
  "2.760"
}

# Gráfico "Dinâmica Industrial e Emprego": Abertura de CNPJs (Receita Federal) x Admissões (Novo CAGED)
# Ambas as séries são mensais e convertidas em ÍNDICE (base 100 = primeiro mês em comum), tornando-as comparáveis.
df_chart2_vg <- NULL
if (!is.null(resumo_cnpj_to) && !is.null(resumo_cnpj_to$tabelas_boletins$serie_mensal_to) &&
    !is.null(resumo_caged) && !is.null(resumo_caged$auditoria_series_mensais)) {
  df_cm  <- resumo_cnpj_to$tabelas_boletins$serie_mensal_to
  df_cm$Competencia_Num <- df_cm$Ano_Num * 100L + df_cm$Mes_Num
  df_cg  <- resumo_caged$auditoria_series_mensais
  df_cg$Competencia_Num <- as.integer(df_cg$Competencia)
  comps_comuns <- sort(intersect(df_cm$Competencia_Num, df_cg$Competencia_Num))
  comps_comuns <- tail(comps_comuns, 12)
  if (length(comps_comuns) >= 3) {
    ab_cnpj <- df_cm$Aberturas[match(comps_comuns, df_cm$Competencia_Num)]
    adm_cg  <- df_cg$Admitidos_Consolidado[match(comps_comuns, df_cg$Competencia_Num)]
    rotulos_meses <- sprintf("%s/%s",
                             c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")[comps_comuns %% 100L],
                             substr(as.character(comps_comuns %/% 100L), 3, 4))
    df_chart2_vg <- data.frame(
      Periodo = factor(rep(rotulos_meses, 2), levels = rotulos_meses),
      Valor   = c(round(ab_cnpj / ab_cnpj[1] * 100, 1),
                  round(adm_cg  / adm_cg[1]  * 100, 1)),
      Serie   = rep(c("Abertura de CNPJs", "Admissões (Novo CAGED)"), each = length(comps_comuns)),
      stringsAsFactors = FALSE
    )
  }
}
if (is.null(df_chart2_vg) && !is.null(dados_antigos) && !is.null(dados_antigos$visao_geral$df_chart2) &&
    "Periodo" %in% names(dados_antigos$visao_geral$df_chart2)) {
  df_chart2_vg <- dados_antigos$visao_geral$df_chart2
}
if (is.null(df_chart2_vg)) {
  rot_fb <- c("Out/25","Nov/25","Dez/25","Jan/26","Fev/26","Mar/26","Abr/26","Mai/26","Jun/26","Jul/26","Ago/26","Set/26")
  df_chart2_vg <- data.frame(
    Periodo = factor(rep(rot_fb, 2), levels = rot_fb),
    Valor   = rep(100, 24),
    Serie   = rep(c("Abertura de CNPJs", "Admissões (Novo CAGED)"), each = 12),
    stringsAsFactors = FALSE
  )
}

visao_geral <- list(
  vb_crescimento_pib = str_var_pib_to_vg,
  vb_desocupacao     = taxa_desocupacao_to_atual,
  vb_icms_var        = str_vb_icms_var_vg,
  vb_novos_cnpjs     = str_vb_novos_cnpjs_vg,
  df_chart1 = data.frame(
    Trimestre = factor(rep(trimestres, 2), levels = trimestres),
    Valor = c(serie_var_icms_trim_painel,            # Arrecadação ICMS (SEFAZ-TO)
              serie_var_pib_to_dessaz_painel),       # PIB trimestral (dessazonalizado X-13 pelo jornal)
    Serie = rep(c("Arrecadação ICMS", "PIB trimestral"), each = length(trimestres))
  ),
  df_chart2 = df_chart2_vg
)

# --- ABA 2: PIB (TO) ---
pib <- list(
  periodo_referencia = periodo_pib_to_atual,
  vb_pib_nominal_to  = str_vb_pib_nom_to,
  vb_impostos_to     = str_vb_impostos_to,
  vb_pib_nominal_br  = str_vb_pib_nom_br,
  vb_setor_agro      = fmt_seta_var(var_setores_pib_to[["Agropecuária"]], 1, com_sinal = TRUE),
  vb_setor_servicos  = fmt_seta_var(var_setores_pib_to[["Serviços"]], 1, com_sinal = TRUE),
  vb_setor_industria = fmt_seta_var(var_setores_pib_to[["Indústria"]], 1, com_sinal = TRUE),
  setores_ranking    = setores_ranking_pib_to,
  df_pib             = df_pib_comparativo,
  df_pib_historico   = df_pib_comparativo_historico
)

# --- ABA 3: MERCADO DE TRABALHO ---
meses_caged <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
ano_rais_atual        <- if (!is.null(resumo_rais) && !is.null(resumo_rais$ano_rais)) resumo_rais$ano_rais else 2025L
nome_col_estoque_rais <- if (!is.null(resumo_rais) && !is.null(resumo_rais$nome_coluna_estoque_mun)) resumo_rais$nome_coluna_estoque_mun else sprintf("Estoque (RAIS) %d", ano_rais_atual)
str_vb_estoque_rais   <- if (!is.null(resumo_rais) && !is.null(resumo_rais$vb_estoque_rais_to)) resumo_rais$vb_estoque_rais_to else "437.580"
str_vb_estab_rais     <- if (!is.null(resumo_rais) && !is.null(resumo_rais$vb_estabelecimentos_ativos_to)) resumo_rais$vb_estabelecimentos_ativos_to else "33.198"
str_vb_rem_rais       <- if (!is.null(resumo_rais) && !is.null(resumo_rais$vb_remuneracao_media_to)) resumo_rais$vb_remuneracao_media_to else "R$ 4.022,93"

if (!is.null(resumo_caged)) {
  df_mun_painel_final <- resumo_caged$df_municipios_painel
  if (!is.null(resumo_rais) && !is.null(resumo_rais$vinculos_por_municipio) && !is.null(resumo_caged$municipios_139_to)) {
    df_mun_painel_final <- resumo_caged$municipios_139_to %>%
      select(cod_mun_6, Municipio, Saldo_Mes) %>%
      left_join(
        resumo_rais$vinculos_por_municipio %>% select(cod_mun_6, Estoque_Vinculos),
        by = "cod_mun_6"
      ) %>%
      mutate(
        Estoque_Vinculos = coalesce(as.integer(Estoque_Vinculos), 0L),
        Saldo_Mes        = coalesce(as.integer(Saldo_Mes), 0L)
      ) %>%
      arrange(desc(Estoque_Vinculos), desc(Saldo_Mes), Municipio) %>%
      transmute(
        Município    = Municipio,
        Estoque_Temp = Estoque_Vinculos,
        Saldo_Temp   = Saldo_Mes
      )
    names(df_mun_painel_final)[2] <- nome_col_estoque_rais
    names(df_mun_painel_final)[3] <- resumo_caged$nome_coluna_saldo_mun
  }
  
  trabalho <- list(
    ano_rais                = ano_rais_atual,
    nome_coluna_estoque_mun = nome_col_estoque_rais,
    vb_estoque_rais         = str_vb_estoque_rais,
    vb_estab_ativos_rais    = str_vb_estab_rais,
    vb_remuneracao_rais     = str_vb_rem_rais,
    competencia_caged       = resumo_caged$competencia_vigente,
    mes_ano_caged           = resumo_caged$mes_ano_curto,
    mes_ano_extenso_caged   = resumo_caged$mes_ano_extenso,
    nome_coluna_saldo_mun   = resumo_caged$nome_coluna_saldo_mun,
    vb_saldo_caged          = resumo_caged$vb_saldo_caged,
    vb_admitidos_caged      = fmt_num_br(resumo_caged$resumo_geral_mes$admitidos_consolidado, 0),
    vb_desligados_caged     = fmt_num_br(resumo_caged$resumo_geral_mes$desligados_consolidado, 0),
    vb_salario_medio_caged  = paste0("R$ ", fmt_num_br(resumo_caged$resumo_geral_mes$salario_medio_admitidos, 2)),
    vb_desocupacao          = taxa_desocupacao_to_atual,
    vb_rendimento           = rendimento_medio_to_atual,
    df_caged                = resumo_caged$df_caged_painel,
    df_setores              = resumo_caged$df_setores_painel,
    df_municipios           = df_mun_painel_final
  )
} else {
  trabalho <- list(
    ano_rais                = ano_rais_atual,
    nome_coluna_estoque_mun = nome_col_estoque_rais,
    vb_estoque_rais         = str_vb_estoque_rais,
    vb_estab_ativos_rais    = str_vb_estab_rais,
    vb_remuneracao_rais     = str_vb_rem_rais,
    competencia_caged       = 202609L,
    mes_ano_caged           = "09/26",
    mes_ano_extenso_caged   = "Setembro/2026",
    nome_coluna_saldo_mun   = "Saldo (Novo CAGED) 09/26",
    vb_saldo_caged          = "+716",
    vb_admitidos_caged      = "11.263",
    vb_desligados_caged     = "10.547",
    vb_salario_medio_caged  = "R$ 2.117,32",
    vb_desocupacao          = taxa_desocupacao_to_atual,
    vb_rendimento           = rendimento_medio_to_atual,
    df_caged = data.frame(
      Mes   = factor(meses_caged, levels = meses_caged),
      Saldo = c(500, 800, 1200, 900, 1100, 1500, 1300, 1600, 716, 1400, 1100, -200),
      Serie = "Saldo Mensal"
    ),
    df_setores = data.frame(
      Setor = c("Serviços", "Construção", "Comércio", "Agropecuária", "Indústria"),
      Saldo = c(590, 211, 121, -187, -19)
    ),
    df_municipios = data.frame(
      Município = c("Palmas", "Araguaína", "Gurupi", "Porto Nacional", "Paraíso do Tocantins",
                    "Colinas do Tocantins", "Guaraí", "Formoso do Araguaia", "Lagoa da Confusão", "Dianópolis"),
      `Estoque (RAIS) 2025` = c(220673, 47010, 20492, 14172, 12288, 5707, 3608, 3387, 3137, 3122),
      `Saldo (Novo CAGED) 09/26` = c(306, -42, 4, 241, 178, 86, 48, 32, -88, 54),
      check.names = FALSE
    )
  )
}

# --- ABA 4: COMÉRCIO EXTERIOR ---
comex <- list(
  periodo_referencia = periodo_comex_ref,
  vb_exportacoes     = str_vb_exp_to,
  vb_importacoes     = str_vb_imp_to,
  vb_saldo           = str_vb_saldo_to,
  df_exp_mapa        = df_exp_mapa_painel,
  df_imp_mapa        = df_imp_mapa_painel,
  df_exp_top3        = df_exp_top3_painel,
  df_imp_top3        = df_imp_top3_painel
)

# --- ABA 5: ARRECADAÇÃO E FINANÇAS ---
if (!is.null(resumo_arrecadacao_to)) {
  financas <- list(
    competencia_sefaz     = resumo_arrecadacao_to$competencia_vigente,
    mes_ano_sefaz         = resumo_arrecadacao_to$mes_ano_curto,
    mes_ano_extenso_sefaz = resumo_arrecadacao_to$mes_ano_extenso,
    data_emissao_sefaz    = resumo_arrecadacao_to$data_emissao_sefaz,
    vb_receita_total      = resumo_arrecadacao_to$vb_receita_total,
    vb_icms               = resumo_arrecadacao_to$vb_icms,
    vb_icms_var_mensal    = resumo_arrecadacao_to$vb_icms_var,
    vb_icms_var_anual     = resumo_arrecadacao_to$vb_icms_var_anual,
    vb_fpe                = resumo_arrecadacao_to$vb_fpe,
    vb_ipva               = resumo_arrecadacao_to$vb_ipva,
    vb_ipva_puro          = resumo_arrecadacao_to$vb_ipva_puro,
    df_receitas           = resumo_arrecadacao_to$df_receitas_painel,
    df_setores_icms       = resumo_arrecadacao_to$df_setores_icms_painel
  )
} else {
  meses_fin <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set")
  financas <- list(
    competencia_sefaz     = 202609L,
    mes_ano_sefaz         = "09/26",
    mes_ano_extenso_sefaz = "Setembro/2026",
    data_emissao_sefaz    = "20/10/2026",
    vb_receita_total      = "R$ 1,19 Bi",
    vb_icms               = "R$ 566,48 Mi",
    vb_icms_var_mensal    = "-5,25%",
    vb_icms_var_anual     = "+1,14%",
    vb_fpe                = "R$ 438,14 Mi",
    vb_ipva               = "R$ 189,35 Mi",
    vb_ipva_puro          = "R$ 53,89 Mi",
    df_receitas = data.frame(
      Mes  = factor(meses_fin, levels = meses_fin),
      ICMS = c(532.94, 480.83, 478.30, 526.27, 557.88, 557.50, 572.90, 597.86, 566.48),
      FPE  = c(586.09, 801.29, 493.86, 560.41, 686.57, 567.74, 419.61, 440.34, 438.14),
      IPVA = c(141.78, 128.08, 179.96, 178.18, 169.39, 174.16, 286.65, 212.48, 189.35)
    ),
    df_setores_icms = data.frame(
      Setor = c("Combustíveis", "Comércio", "Indústria", "Energia Elétrica", "Demais Atividades", "Comunicação", "Transportes", "Agropecuária"),
      Pai   = rep("", 8),
      Valor = c(172.81, 172.77, 103.08, 63.73, 30.11, 9.20, 8.27, 6.52)
    )
  )
}

# --- ABA 6: DINÂMICA EMPRESARIAL ---
if (!is.null(resumo_cnpj_to) && !is.null(resumo_cnpj_to$indicadores_painel)) {
  ip_cnpj <- resumo_cnpj_to$indicadores_painel
  md_cnpj <- resumo_cnpj_to$metadados
  empresarial <- list(
    competencia_cnpj           = md_cnpj$competencia_formatada,
    mes_ano_extenso_cnpj       = paste0(md_cnpj$mes_referencia_nome, "/", md_cnpj$ano_referencia),
    ano_referencia_cnpj        = md_cnpj$ano_referencia,
    vb_cnpjs_ativos            = ip_cnpj$vb_cnpjs_ativos,
    vb_saldo_empresas          = ip_cnpj$vb_saldo_empresas,
    vb_mei                     = ip_cnpj$vb_mei,
    vb_me                      = ip_cnpj$vb_me,
    vb_me_com_mei              = ip_cnpj$vb_me_com_mei,
    vb_optantes_simples        = ip_cnpj$vb_optantes_simples,
    texto_centro_rosca         = ip_cnpj$texto_centro_rosca,
    tempo_abertura_h           = ip_cnpj$tempo_abertura_h,
    tempo_viabilidade_h        = ip_cnpj$tempo_viabilidade_h,
    tempo_registro_h           = ip_cnpj$tempo_registro_h,
    media_brasil_h             = ip_cnpj$media_brasil_h,
    media_brasil_viabilidade_h = ip_cnpj$media_brasil_viabilidade_h,
    media_brasil_registro_h    = ip_cnpj$media_brasil_registro_h,
    df_ativos                  = ip_cnpj$df_ativos,
    df_porte                   = ip_cnpj$df_porte
  )
} else {
  empresarial <- list(
    competencia_cnpj           = "Out/2026",
    mes_ano_extenso_cnpj       = "Outubro/2026",
    ano_referencia_cnpj        = 2026L,
    vb_cnpjs_ativos            = "167.565",
    vb_saldo_empresas          = "+ 15.608",
    vb_mei                     = "52,9%",
    vb_me                      = "34,0%",
    vb_me_com_mei              = "86,9%",
    vb_optantes_simples        = "164.996",
    texto_centro_rosca         = "<b>167,6 Mil</b><br>Ativas",
    tempo_abertura_h           = 18.5,
    tempo_viabilidade_h        = 15.7,
    tempo_registro_h           = 2.7,
    media_brasil_h             = 26.2,
    media_brasil_viabilidade_h = 11.4,
    media_brasil_registro_h    = 14.8,
    df_ativos = data.frame(
      Mes   = factor(meses_caged, levels = meses_caged),
      Valor = c(153261, 154868, 156489, 158130, 159609, 161148, 162445, 163802, 165018, 166261, 166900, 167565),
      Serie = "Empresas Ativas"
    ),
    df_porte = data.frame(
      Porte = c("MEI", "Microempresa (ME)", "Pequeno Porte (EPP)", "Não Informado / Demais"),
      Total = c(88650, 56958, 11896, 10061)
    )
  )
}

# --- ABA 7: CÂMBIO E MERCADOS (INDICADORES BCB COMPLEMENTARES AO TEMPO REAL) ---
mercados_macro <- list(
  selic_meta = selic_atual,
  ipca_12m   = ipca_12m_atual
)

# ------------------------------------------------------------------------------
# 3. ACERVO DE BASES COMPLETAS PARA BOLETINS FLASH, DOSSIÊS E CARTAS DE CONJUNTURA
# ------------------------------------------------------------------------------
pesquisas_extras <- list(
  pib_to_seplan                   = pib_to_planilha,
  pib_brasil_1621                 = sidra_1621_pib_br,
  pib_brasil_1846                 = sidra_1846_pib_br_corr,
  pib_brasil_5932                 = sidra_5932_pib_br_var,
  pib_contas_regionais_5938       = sidra_5938_contas_reg_uf,
  pib_contas_regionais_mun_5938   = sidra_5938_contas_reg_mun,
  pib_contas_regionais_to_df      = df_contas_regionais_to,
  pib_contas_regionais_mun_top10  = df_contas_regionais_mun_top10,
  pib_setores_dessaz_to_vs_br     = df_pib_setores_dessaz_to_vs_br,
  pnad_4099_desocupacao           = sidra_4099_desocupacao,
  pnad_6374_horas                 = sidra_6374_horas,
  pnad_5436_rendimento            = sidra_5436_rendimento,
  novo_caged                      = resumo_caged,
  rais                            = resumo_rais,
  arrecadacao_to                  = resumo_arrecadacao_to,
  cnpj_to                         = resumo_cnpj_to,
  agro_to                         = resumo_agro_to,
  comex_to_mensal                 = df_comex_to_mensal,
  comex_br_mensal                 = df_comex_br_mensal,
  comex_termos_troca_to           = df_comex_termos_troca_to,
  comex_to_top10_exp_sh4          = df_to_top10_exp_sh4_mes,
  comex_to_top10_imp_sh4          = df_to_top10_imp_sh4_mes,
  comex_to_top10_exp_sh4_ano      = df_to_top10_exp_sh4_ano,
  comex_to_top10_imp_sh4_ano      = df_to_top10_imp_sh4_ano,
  comex_br_top10_exp_sh4          = df_br_top10_exp_sh4_mes,
  comex_br_top10_imp_sh4          = df_br_top10_imp_sh4_mes,
  comex_br_top10_exp_sh4_ano      = df_br_top10_exp_sh4_ano,
  comex_br_top10_imp_sh4_ano      = df_br_top10_imp_sh4_ano,
  comex_to_paises_exp             = df_to_paises_exp_mes,
  comex_to_paises_imp             = df_to_paises_imp_mes,
  comex_to_paises_exp_ano         = df_to_paises_exp_ano,
  comex_to_paises_imp_ano         = df_to_paises_imp_ano,
  comex_to_mun_exp                = df_to_mun_exp_mes,
  comex_to_mun_imp                = df_to_mun_imp_mes,
  comex_to_mun_exp_ano            = df_to_mun_exp_ano,
  comex_to_mun_imp_ano            = df_to_mun_imp_ano,
  comex_to_top3_exp_por_pais      = df_to_top3_exp_paises_mes,
  comex_to_top3_imp_por_pais      = df_to_top3_imp_paises_mes,
  comex_to_top3_exp_por_pais_ano  = df_to_top3_exp_paises_ano,
  comex_to_top3_imp_por_pais_ano  = df_to_top3_imp_paises_ano
)

# ------------------------------------------------------------------------------
# 4. CONTROLE INTELIGENTE DE DATAS DE ATUALIZAÇÃO (SELO "NOVO")
# ------------------------------------------------------------------------------
datas_padrao <- list(
  dt_pib_tri   = "14/10/2026",
  dt_pib_br    = "10/10/2026",
  dt_caged     = "10/10/2026",
  dt_rais      = "10/10/2026",
  dt_pnad      = "10/10/2026",
  dt_comex     = "14/10/2026",
  dt_fazenda   = "20/10/2026",
  dt_receita   = "12/11/2026",
  dt_mapa_empr = "12/11/2026"
)

hoje_str <- format(Sys.Date(), "%d/%m/%Y")

if (!is.null(dados_antigos)) {
  datas_padrao <- dados_antigos$datas
  if (!identical(dados_antigos$pib, pib))                 datas_padrao$dt_pib_tri <- hoje_str
  if (!identical(dados_antigos$trabalho, trabalho))       datas_padrao$dt_caged   <- hoje_str
  if (!identical(dados_antigos$pesquisas_extras$rais$ano_rais, resumo_rais$ano_rais) ||
      !identical(dados_antigos$trabalho$vb_estoque_rais, trabalho$vb_estoque_rais)) {
    datas_padrao$dt_rais <- hoje_str
  }
  if (!identical(dados_antigos$comex, comex))             datas_padrao$dt_comex   <- hoje_str
  if (!identical(dados_antigos$financas, financas))       datas_padrao$dt_fazenda <- hoje_str
  if (!identical(dados_antigos$empresarial, empresarial)) {
    datas_padrao$dt_receita   <- hoje_str
    datas_padrao$dt_mapa_empr <- hoje_str
  }
}

# ------------------------------------------------------------------------------
# 5. SALVAMENTO DOS DADOS CONSOLIDADOS (.RDS e .JSON)
# ------------------------------------------------------------------------------
painel_dados <- list(
  ultima_atualizacao = format(Sys.Date(), "%d/%m/%Y"),
  datas              = datas_padrao,
  visao_geral        = visao_geral,
  pib                = pib,
  trabalho           = trabalho,
  comex              = comex,
  financas           = financas,
  empresarial        = empresarial,
  mercados_macro     = mercados_macro,
  pesquisas_extras   = pesquisas_extras
)

saveRDS(painel_dados, file = arquivo_rds)

# No JSON leve do front-end salvamos os indicadores do painel
painel_json <- painel_dados
painel_json$pesquisas_extras <- NULL
jsonlite::write_json(painel_json, path = "dados/indicadores_to.json", pretty = TRUE, auto_unbox = TRUE)

cat("========================================================\n")
cat("Resumo dos indicadores extraidos para o painel:\n")
cat("  - Variacao Trimestral PIB (TO) [Dessaz.]:", visao_geral$vb_crescimento_pib, "\n")
cat("  - PIB Nominal (TO) [SEPLAN]:             ", pib$vb_pib_nominal_to, "\n")
cat("  - Impostos (TO) [SEPLAN]:                ", pib$vb_impostos_to, "\n")
cat("  - PIB Nominal (Brasil) [SIDRA 1846/5932]:", pib$vb_pib_nominal_br, "\n")
cat("  - Ranking Setores PIB (TO):              ",
    paste(sapply(pib$setores_ranking, function(s) paste0(s$titulo, " (", s$valor_fmt, ")")), collapse = " | "), "\n")
cat("  - Taxa de Desocupacao (TO) [SIDRA 4099]: ", trabalho$vb_desocupacao, "\n")
cat("  - Rendimento Medio (TO) [SIDRA 5436]:    ", trabalho$vb_rendimento, "\n")
cat(sprintf("  - RAIS (TO) [%d]:                      Estoque Ativo: %s | Estab. Ativos: %s | Rem. Media: %s\n",
            trabalho$ano_rais, trabalho$vb_estoque_rais, trabalho$vb_estab_ativos_rais, trabalho$vb_remuneracao_rais))
cat(sprintf("  - Novo CAGED (TO) [%s]:                Adm: %s | Desl: %s | Saldo: %s | Sal. Medio Adm: %s\n",
            trabalho$mes_ano_caged, trabalho$vb_admitidos_caged, trabalho$vb_desligados_caged,
            trabalho$vb_saldo_caged, trabalho$vb_salario_medio_caged))
cat(sprintf("  - Comex Stat (TO) [%s]:              Exp: %s | Imp: %s | Saldo: %s\n",
            comex$periodo_referencia, comex$vb_exportacoes, comex$vb_importacoes, comex$vb_saldo))
cat(sprintf("  - Arrecadacao SEFAZ/STN (TO) [%s]:    Rec. Total: %s | ICMS: %s (Var. Mes: %s) | FPE: %s | IPVA+Outros: %s\n",
            financas$mes_ano_sefaz, financas$vb_receita_total, financas$vb_icms,
            visao_geral$vb_icms_var, financas$vb_fpe, financas$vb_ipva))
cat(sprintf("  - Dinamica Empresarial (TO) [%s]:  Ativos: %s | Novos Mes: %s | Saldo 12m: %s | MEI: %s | ME: %s | Simples: %s | Abertura: %sh (Viab: %sh | Reg: %sh)\n",
            empresarial$competencia_cnpj, empresarial$vb_cnpjs_ativos, visao_geral$vb_novos_cnpjs,
            empresarial$vb_saldo_empresas, empresarial$vb_mei, empresarial$vb_me,
            empresarial$vb_optantes_simples,
            format(empresarial$tempo_abertura_h, decimal.mark = ","),
            format(empresarial$tempo_viabilidade_h, decimal.mark = ","),
            format(empresarial$tempo_registro_h, decimal.mark = ",")))
if (exists("imprimir_resumo_diagnostico")) {
  imprimir_resumo_diagnostico("atualizar_dados.R")
}
cat("Sucesso! Arquivos 'dados/painel_dados.rds' e 'dados/indicadores_to.json' atualizados.\n")
cat("========================================================\n")
