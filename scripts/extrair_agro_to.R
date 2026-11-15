# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/extrair_agro_to.R
# Objetivo:
#   Extrair, limpar, consolidar e salvar de forma enxuta (compress = "xz") todas
#   as pesquisas agropecuárias, florestais e de proteína animal do IBGE/SIDRA
#   para o Estado do Tocantins (UF = 17) e seus municípios, abastecendo a
#   Seção 6 (Produção Agropecuária) e a Seção 8 (Análise Interseccional) da
#   Carta de Conjuntura:
#     1. LSPA  (Tabela 6588) - Levantamento Sistemático da Produção Agrícola (Mensal)
#     2. PAM   (Tabela 5457) - Produção Agrícola Municipal (Anual - UF e Top 10 Mun.)
#     3. PPM   (Tabelas 3939 e 74) - Pesquisa da Pecuária Municipal (Rebanhos e Origem Animal)
#     4. PEVS  (Tabelas 5930, 291 e 289) - Silvicultura e Extração Vegetal (Anual)
#     5. ABATE (Tabelas 1092, 1093 e 1094) - Abate Trimestral (Bovinos, Suínos e Frangos)
#     6. LEITE (Tabela 1086) - Pesquisa Trimestral do Leite (Captação e Industrialização)
#     7. OVOS  (Tabela 915)  - Produção de Ovos de Galinha - POG (Trimestral)
# ==============================================================================

if (file.exists("scripts/utils_debug_log.R")) {
  source("scripts/utils_debug_log.R", local = FALSE)
}

extrair_agro_tocantins <- function(diretorio_raiz = getwd(), forcar_atualizacao = FALSE) {
  caminho_rds <- file.path(diretorio_raiz, "dados", "agro_to_resumo.rds")
  cache_antigo <- if (file.exists(caminho_rds)) readRDS(caminho_rds) else NULL

  if (!forcar_atualizacao && !is.null(cache_antigo) &&
      !is.null(cache_antigo$metadados$data_extracao) &&
      identical(as.character(cache_antigo$metadados$data_extracao), as.character(Sys.Date()))) {
    log_ok("Agro IBGE (Cache Diário)", "Base 'dados/agro_to_resumo.rds' já atualizada hoje.", 0.01, "extrair_agro_to.R")
    return(cache_antigo)
  }

  log_secao("MÓDULO AGROPECUÁRIO E FLORESTAL (IBGE / SIDRA) - TOCANTINS")

  consultar_sidra_agro <- function(api_path, bloco_nome, fallback_df = NULL) {
    executar_bloco_seguro(
      script        = "extrair_agro_to.R",
      bloco         = bloco_nome,
      origem        = paste0("SIDRA API ", api_path),
      expr = {
        df <- suppressMessages(sidrar::get_sidra(api = api_path))
        if (!is.null(df) && nrow(df) > 0) {
          df$Valor <- suppressWarnings(as.numeric(df$Valor))
        }
        df
      },
      fallback      = fallback_df,
      como_corrigir = sprintf("Teste manualmente get_sidra(api='%s') ou verifique estabilidade da API SIDRA/IBGE.", api_path)
    )
  }

  # ----------------------------------------------------------------------------
  # 1. LSPA (Tabela 6588) - Levantamento Sistemático da Produção Agrícola (Mensal)
  #    Variáveis: 35 (Produção - t), 216 (Área colhida - ha), 36 (Rendimento médio - kg/ha)
  # ----------------------------------------------------------------------------
  raw_lspa <- consultar_sidra_agro(
    "/t/6588/n3/17/v/35,216,36/p/last%202/c48/all",
    "LSPA 6588 (Safra Mensal TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$lspa else NULL
  )

  df_lspa_resumo <- NULL
  mes_lspa_ref   <- "N/D"
  if (!is.null(raw_lspa) && nrow(raw_lspa) > 0) {
    meses_disp   <- sort(unique(raw_lspa[["Mês (Código)"]]))
    ult_mes_cod  <- tail(meses_disp, 1)
    df_ult_lspa  <- raw_lspa[raw_lspa[["Mês (Código)"]] == ult_mes_cod, ]
    mes_lspa_ref <- as.character(df_ult_lspa[["Mês"]][1])

    df_prod <- df_ult_lspa[df_ult_lspa[["Variável (Código)"]] == "35" & !is.na(df_ult_lspa$Valor),
                           c("Produto das lavouras", "Valor")]
    df_area <- df_ult_lspa[df_ult_lspa[["Variável (Código)"]] == "216" & !is.na(df_ult_lspa$Valor),
                           c("Produto das lavouras", "Valor")]
    df_rend <- df_ult_lspa[df_ult_lspa[["Variável (Código)"]] == "36" & !is.na(df_ult_lspa$Valor),
                           c("Produto das lavouras", "Valor")]
    names(df_prod)[2] <- "Producao_Ton"
    names(df_area)[2] <- "Area_Colhida_Ha"
    names(df_rend)[2] <- "Rendimento_Kg_Ha"

    df_m <- merge(df_prod, df_area, by = "Produto das lavouras", all = TRUE)
    df_m <- merge(df_m, df_rend, by = "Produto das lavouras", all = TRUE)
    names(df_m)[1] <- "Produto_Bruto"
    df_m$Produto <- trimws(sub("^[0-9.]+\\s*", "", as.character(df_m$Produto_Bruto)))
    df_m <- df_m[!is.na(df_m$Producao_Ton) & df_m$Producao_Ton > 0, ]
    df_lspa_resumo <- df_m[order(-df_m$Producao_Ton), c("Produto", "Producao_Ton", "Area_Colhida_Ha", "Rendimento_Kg_Ha")]
    rownames(df_lspa_resumo) <- NULL
  }

  # ----------------------------------------------------------------------------
  # 2. PAM (Tabela 5457) - Produção Agrícola Municipal (Anual - Estado e Top 10 Mun.)
  #    Variáveis: 214 (Qtd produzida - t), 215 (Valor da produção - Mil R$),
  #               216 (Área colhida - ha), 112 (Rendimento médio - kg/ha)
  # ----------------------------------------------------------------------------
  raw_pam_uf <- consultar_sidra_agro(
    "/t/5457/n3/17/v/214,215,216,112/p/last%202/c782/all",
    "PAM 5457 (Lavouras UF TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$pam_uf else NULL
  )

  df_pam_culturas <- NULL
  ano_pam_ref     <- "N/D"
  if (!is.null(raw_pam_uf) && nrow(raw_pam_uf) > 0) {
    anos_pam    <- sort(unique(raw_pam_uf[["Ano (Código)"]]))
    ult_ano_pam <- tail(anos_pam, 1)
    ant_ano_pam <- if (length(anos_pam) >= 2) anos_pam[length(anos_pam) - 1] else NA_character_
    ano_pam_ref <- ult_ano_pam
    col_prod_nm <- "Produto das lavouras temporárias e permanentes"

    df_u <- raw_pam_uf[raw_pam_uf[["Ano (Código)"]] == ult_ano_pam, ]
    p_qtd  <- df_u[df_u[["Variável (Código)"]] == "214", c(col_prod_nm, "Valor")]; names(p_qtd)[2]  <- "Producao_Ton"
    p_val  <- df_u[df_u[["Variável (Código)"]] == "215", c(col_prod_nm, "Valor")]; names(p_val)[2]  <- "Valor_Mil_RS"
    p_area <- df_u[df_u[["Variável (Código)"]] == "216", c(col_prod_nm, "Valor")]; names(p_area)[2] <- "Area_Colhida_Ha"
    p_rend <- df_u[df_u[["Variável (Código)"]] == "112", c(col_prod_nm, "Valor")]; names(p_rend)[2] <- "Rendimento_Kg_Ha"

    m_pam <- Reduce(function(x, y) merge(x, y, by = col_prod_nm, all = TRUE), list(p_qtd, p_val, p_area, p_rend))
    if (!is.na(ant_ano_pam)) {
      df_a <- raw_pam_uf[raw_pam_uf[["Ano (Código)"]] == ant_ano_pam & raw_pam_uf[["Variável (Código)"]] == "214", c(col_prod_nm, "Valor")]
      names(df_a)[2] <- "Producao_Ton_Ant"
      m_pam <- merge(m_pam, df_a, by = col_prod_nm, all.x = TRUE)
      m_pam$Var_Prod_Pct <- ifelse(!is.na(m_pam$Producao_Ton_Ant) & m_pam$Producao_Ton_Ant > 0,
                                   round(((m_pam$Producao_Ton / m_pam$Producao_Ton_Ant) - 1) * 100, 2), NA_real_)
    } else {
      m_pam$Var_Prod_Pct <- NA_real_
    }
    names(m_pam)[1] <- "Cultura"
    m_pam$Cultura <- gsub("\\*$", "", trimws(as.character(m_pam$Cultura)))
    m_pam <- m_pam[m_pam$Cultura != "Total" & !is.na(m_pam$Valor_Mil_RS) & m_pam$Valor_Mil_RS > 0, ]
    m_pam$Valor_Mi_RS <- round(m_pam$Valor_Mil_RS / 1000, 2)
    df_pam_culturas <- head(m_pam[order(-m_pam$Valor_Mil_RS), c("Cultura", "Producao_Ton", "Area_Colhida_Ha", "Rendimento_Kg_Ha", "Valor_Mi_RS", "Var_Prod_Pct")], 12)
    rownames(df_pam_culturas) <- NULL
  }

  # PAM Municipal (Valor Total da Produção Agrícola e Soja/Milho/Arroz nos 139 municípios do TO)
  # Códigos c782: 0 (Total), 40124 (Soja), 40122 (Milho), 40102 (Arroz)
  raw_pam_mun <- consultar_sidra_agro(
    "/t/5457/n6/in%20n3%2017/v/214,215,216/p/last%201/c782/0,40124,40122,40102",
    "PAM 5457 (Top Municípios Agrícolas TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$pam_mun else NULL
  )

  df_pam_mun_top10 <- NULL
  if (!is.null(raw_pam_mun) && nrow(raw_pam_mun) > 0) {
    col_p <- "Produto das lavouras temporárias e permanentes (Código)"
    df_tot_val  <- raw_pam_mun[raw_pam_mun[[col_p]] == "0" & raw_pam_mun[["Variável (Código)"]] == "215", c("Município", "Valor")]
    df_tot_area <- raw_pam_mun[raw_pam_mun[[col_p]] == "0" & raw_pam_mun[["Variável (Código)"]] == "216", c("Município", "Valor")]
    df_soja_ton <- raw_pam_mun[raw_pam_mun[[col_p]] == "40124" & raw_pam_mun[["Variável (Código)"]] == "214", c("Município", "Valor")]
    df_milho_ton<- raw_pam_mun[raw_pam_mun[[col_p]] == "40122" & raw_pam_mun[["Variável (Código)"]] == "214", c("Município", "Valor")]
    names(df_tot_val)[2]   <- "Valor_Total_Mil_RS"
    names(df_tot_area)[2]  <- "Area_Colhida_Ha"
    names(df_soja_ton)[2]  <- "Soja_Ton"
    names(df_milho_ton)[2] <- "Milho_Ton"

    mm <- Reduce(function(x, y) merge(x, y, by = "Município", all = TRUE),
                 list(df_tot_val, df_tot_area, df_soja_ton, df_milho_ton))
    mm$Municipio <- sub("\\s*-\\s*TO$", "", as.character(mm[["Município"]]))
    mm$Valor_Total_Mi_RS <- round(coalesce(mm$Valor_Total_Mil_RS, 0) / 1000, 2)
    tot_est_val <- sum(mm$Valor_Total_Mil_RS, na.rm = TRUE)
    mm$Participacao_Pct <- if (tot_est_val > 0) round((coalesce(mm$Valor_Total_Mil_RS, 0) / tot_est_val) * 100, 2) else 0
    mm <- mm[order(-mm$Valor_Total_Mi_RS), ]
    df_pam_mun_top10 <- head(mm[, c("Municipio", "Area_Colhida_Ha", "Soja_Ton", "Milho_Ton", "Valor_Total_Mi_RS", "Participacao_Pct")], 10)
    rownames(df_pam_mun_top10) <- NULL
  }

  # ----------------------------------------------------------------------------
  # 3. PPM (Tabelas 3939 e 74) - Pesquisa da Pecuária Municipal (Rebanhos e Origem Animal)
  # ----------------------------------------------------------------------------
  raw_ppm_reb <- consultar_sidra_agro(
    "/t/3939/n3/17/v/105/p/last%203/c79/all",
    "PPM 3939 (Efetivo de Rebanhos TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$ppm_reb else NULL
  )

  df_ppm_rebanhos <- NULL
  ano_ppm_ref     <- "N/D"
  if (!is.null(raw_ppm_reb) && nrow(raw_ppm_reb) > 0) {
    anos_ppm    <- sort(unique(raw_ppm_reb[["Ano (Código)"]]))
    ult_ano_ppm <- tail(anos_ppm, 1)
    ant_ano_ppm <- if (length(anos_ppm) >= 2) anos_ppm[length(anos_ppm) - 1] else NA_character_
    ano_ppm_ref <- ult_ano_ppm

    r_u <- raw_ppm_reb[raw_ppm_reb[["Ano (Código)"]] == ult_ano_ppm, c("Tipo de rebanho", "Valor")]
    names(r_u) <- c("Rebanho", "Cabecas_Atual")
    if (!is.na(ant_ano_ppm)) {
      r_a <- raw_ppm_reb[raw_ppm_reb[["Ano (Código)"]] == ant_ano_ppm, c("Tipo de rebanho", "Valor")]
      names(r_a) <- c("Rebanho", "Cabecas_Anterior")
      r_u <- merge(r_u, r_a, by = "Rebanho", all.x = TRUE)
      r_u$Var_Anual_Pct <- ifelse(!is.na(r_u$Cabecas_Anterior) & r_u$Cabecas_Anterior > 0,
                                  round(((r_u$Cabecas_Atual / r_u$Cabecas_Anterior) - 1) * 100, 2), NA_real_)
    } else {
      r_u$Cabecas_Anterior <- NA_real_
      r_u$Var_Anual_Pct    <- NA_real_
    }
    r_u <- r_u[!is.na(r_u$Cabecas_Atual) & r_u$Cabecas_Atual > 0, ]
    df_ppm_rebanhos <- r_u[order(-r_u$Cabecas_Atual), ]
    rownames(df_ppm_rebanhos) <- NULL
  }

  raw_ppm_bov_mun <- consultar_sidra_agro(
    "/t/3939/n6/in%20n3%2017/v/105/p/last%201/c79/2670",
    "PPM 3939 (Top 10 Municípios Rebanho Bovino TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$ppm_bov_mun else NULL
  )

  df_ppm_bov_mun_top10 <- NULL
  if (!is.null(raw_ppm_bov_mun) && nrow(raw_ppm_bov_mun) > 0) {
    bm <- raw_ppm_bov_mun[!is.na(raw_ppm_bov_mun$Valor), c("Município", "Valor")]
    bm$Municipio <- sub("\\s*-\\s*TO$", "", as.character(bm[["Município"]]))
    bm$Cabecas_Bovinos <- bm$Valor
    tot_bov <- sum(bm$Cabecas_Bovinos, na.rm = TRUE)
    bm$Participacao_Pct <- if (tot_bov > 0) round((bm$Cabecas_Bovinos / tot_bov) * 100, 2) else 0
    bm <- bm[order(-bm$Cabecas_Bovinos), c("Municipio", "Cabecas_Bovinos", "Participacao_Pct")]
    df_ppm_bov_mun_top10 <- head(bm, 10)
    rownames(df_ppm_bov_mun_top10) <- NULL
  }

  raw_ppm_orig <- consultar_sidra_agro(
    "/t/74/n3/17/v/106,215/p/last%202/c80/all",
    "PPM 74 (Produção de Origem Animal TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$ppm_orig else NULL
  )

  df_ppm_origem_animal <- NULL
  if (!is.null(raw_ppm_orig) && nrow(raw_ppm_orig) > 0) {
    ult_ano_74 <- tail(sort(unique(raw_ppm_orig[["Ano (Código)"]])), 1)
    d74 <- raw_ppm_orig[raw_ppm_orig[["Ano (Código)"]] == ult_ano_74, ]
    q74 <- d74[d74[["Variável (Código)"]] == "106", c("Tipo de produto de origem animal", "Unidade de Medida", "Valor")]
    v74 <- d74[d74[["Variável (Código)"]] == "215", c("Tipo de produto de origem animal", "Valor")]
    names(q74) <- c("Produto_Animal", "Unidade", "Quantidade")
    names(v74) <- c("Produto_Animal", "Valor_Mil_RS")
    m74 <- merge(q74, v74, by = "Produto_Animal", all = TRUE)
    m74 <- m74[m74$Produto_Animal != "Total" & !is.na(m74$Valor_Mil_RS) & m74$Valor_Mil_RS > 0, ]
    m74$Valor_Mi_RS <- round(m74$Valor_Mil_RS / 1000, 2)
    df_ppm_origem_animal <- m74[order(-m74$Valor_Mil_RS), c("Produto_Animal", "Quantidade", "Unidade", "Valor_Mi_RS")]
    rownames(df_ppm_origem_animal) <- NULL
  }

  # ----------------------------------------------------------------------------
  # 4. PEVS (Tabelas 5930, 291 e 289) - Silvicultura e Extração Vegetal (Anual)
  # ----------------------------------------------------------------------------
  raw_pevs_area <- consultar_sidra_agro(
    "/t/5930/n3/17/v/6549/p/last%202/c734/all",
    "PEVS 5930 (Área de Silvicultura TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$pevs_area else NULL
  )

  raw_pevs_silv <- consultar_sidra_agro(
    "/t/291/n3/17/v/142,143/p/last%202/c194/all",
    "PEVS 291 (Produção e Valor da Silvicultura TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$pevs_silv else NULL
  )

  raw_pevs_ext <- consultar_sidra_agro(
    "/t/289/n3/17/v/144,145/p/last%202/c193/all",
    "PEVS 289 (Extração Vegetal TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$pevs_ext else NULL
  )

  df_pevs_resumo <- NULL
  ano_pevs_ref   <- "N/D"
  if (!is.null(raw_pevs_ext) && nrow(raw_pevs_ext) > 0) {
    ult_ano_pevs <- tail(sort(unique(raw_pevs_ext[["Ano (Código)"]])), 1)
    ano_pevs_ref <- ult_ano_pevs
    de <- raw_pevs_ext[raw_pevs_ext[["Ano (Código)"]] == ult_ano_pevs, ]
    qe <- de[de[["Variável (Código)"]] == "144", c("Tipo de produto extrativo", "Unidade de Medida", "Valor")]
    ve <- de[de[["Variável (Código)"]] == "145", c("Tipo de produto extrativo", "Valor")]
    names(qe) <- c("Produto", "Unidade", "Quantidade")
    names(ve) <- c("Produto", "Valor_Mil_RS")
    me <- merge(qe, ve, by = "Produto", all = TRUE)
    me$Modalidade <- "Extrativismo Vegetal"

    if (!is.null(raw_pevs_silv) && nrow(raw_pevs_silv) > 0) {
      ds <- raw_pevs_silv[raw_pevs_silv[["Ano (Código)"]] == ult_ano_pevs, ]
      qs <- ds[ds[["Variável (Código)"]] == "142", c("Tipo de produto da silvicultura", "Unidade de Medida", "Valor")]
      vs <- ds[ds[["Variável (Código)"]] == "143", c("Tipo de produto da silvicultura", "Valor")]
      names(qs) <- c("Produto", "Unidade", "Quantidade")
      names(vs) <- c("Produto", "Valor_Mil_RS")
      ms <- merge(qs, vs, by = "Produto", all = TRUE)
      ms$Modalidade <- "Silvicultura"
      me <- rbind(ms, me)
    }
    me$Produto_Limpo <- trimws(sub("^[0-9.]+\\s*-\\s*", "", as.character(me$Produto)))
    me <- me[!grepl("^Total$", me$Produto_Limpo, ignore.case = TRUE) & !is.na(me$Valor_Mil_RS) & me$Valor_Mil_RS > 0, ]
    me <- me[order(-me$Valor_Mil_RS), c("Modalidade", "Produto_Limpo", "Quantidade", "Unidade", "Valor_Mil_RS")]
    df_pevs_resumo <- head(me, 12)
    rownames(df_pevs_resumo) <- NULL
  }

  # ----------------------------------------------------------------------------
  # 5. PESQUISAS TRIMESTRAIS DE PROTEÍNA ANIMAL (ABATE 1092/1093/1094, LEITE 1086, OVOS 915)
  # ----------------------------------------------------------------------------
  raw_abate_bov <- consultar_sidra_agro(
    "/t/1092/n3/17/v/284,285/p/last%208/c12716/115236/c18/992/c12529/118225",
    "Abate Bovinos 1092 (Trimestral TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$abate_bov else NULL
  )
  raw_abate_sui <- consultar_sidra_agro(
    "/t/1093/n3/17/v/284,285/p/last%208/c12716/115236/c12529/118225",
    "Abate Suínos 1093 (Trimestral TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$abate_sui else NULL
  )
  raw_abate_fra <- consultar_sidra_agro(
    "/t/1094/n3/17/v/284,285/p/last%208/c12716/115236/c12529/118225",
    "Abate Frangos 1094 (Trimestral TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$abate_fra else NULL
  )
  raw_leite <- consultar_sidra_agro(
    "/t/1086/n3/17/v/282,283/p/last%208/c12716/115236/c12529/118225",
    "Pesquisa do Leite 1086 (Trimestral TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$leite else NULL
  )
  raw_ovos <- consultar_sidra_agro(
    "/t/915/n3/17/v/29,1988/p/last%208/c12716/115236",
    "Produção de Ovos POG 915 (Trimestral TO)",
    if (!is.null(cache_antigo)) cache_antigo$brutos$ovos else NULL
  )

  # Consolida tabela trimestral de Proteína Animal, Leite e Ovos (últimos 8 trimestres)
  extrair_serie_trim <- function(df_raw, cod_var) {
    if (is.null(df_raw) || nrow(df_raw) == 0) return(data.frame(Trimestre_Cod = character(0), Trimestre = character(0), Valor = numeric(0)))
    d <- df_raw[df_raw[["Variável (Código)"]] == as.character(cod_var) & !is.na(df_raw$Valor),
                c("Trimestre (Código)", "Trimestre", "Valor")]
    names(d) <- c("Trimestre_Cod", "Trimestre", "Valor")
    d[order(d$Trimestre_Cod), ]
  }

  s_bov_cab  <- extrair_serie_trim(raw_abate_bov, "284"); names(s_bov_cab)[3] <- "Bovinos_Cabecas"
  s_bov_peso <- extrair_serie_trim(raw_abate_bov, "285"); names(s_bov_peso)[3]<- "Bovinos_Carcaca_Kg"
  s_sui_cab  <- extrair_serie_trim(raw_abate_sui, "284"); names(s_sui_cab)[3] <- "Suinos_Cabecas"
  s_fra_cab  <- extrair_serie_trim(raw_abate_fra, "284"); names(s_fra_cab)[3] <- "Frangos_Cabecas"
  s_fra_peso <- extrair_serie_trim(raw_abate_fra, "285"); names(s_fra_peso)[3]<- "Frangos_Carcaca_Kg"
  s_leite_adq<- extrair_serie_trim(raw_leite,     "282"); names(s_leite_adq)[3]<- "Leite_Adquirido_Mil_L"
  s_ovos_duz <- extrair_serie_trim(raw_ovos,      "29");  names(s_ovos_duz)[3] <- "Ovos_Mil_Duzias"

  lista_trim <- list(s_bov_cab, s_bov_peso[, c("Trimestre_Cod", "Bovinos_Carcaca_Kg")],
                     s_sui_cab[, c("Trimestre_Cod", "Suinos_Cabecas")],
                     s_fra_cab[, c("Trimestre_Cod", "Frangos_Cabecas")],
                     s_fra_peso[, c("Trimestre_Cod", "Frangos_Carcaca_Kg")],
                     s_leite_adq[, c("Trimestre_Cod", "Leite_Adquirido_Mil_L")],
                     s_ovos_duz[, c("Trimestre_Cod", "Ovos_Mil_Duzias")])

  df_pecuaria_trimestral <- Reduce(function(x, y) merge(x, y, by = "Trimestre_Cod", all = TRUE), lista_trim)
  if (!is.null(df_pecuaria_trimestral) && nrow(df_pecuaria_trimestral) > 0) {
    df_pecuaria_trimestral <- df_pecuaria_trimestral[order(df_pecuaria_trimestral$Trimestre_Cod), ]
    df_pecuaria_trimestral$Bovinos_Carcaca_Ton <- round(df_pecuaria_trimestral$Bovinos_Carcaca_Kg / 1000, 1)
    df_pecuaria_trimestral$Frangos_Carcaca_Ton <- round(df_pecuaria_trimestral$Frangos_Carcaca_Kg / 1000, 1)
    # Rótulo curto T1 2026
    df_pecuaria_trimestral$Periodo_Curto <- sprintf(
      "T%s %s",
      substr(df_pecuaria_trimestral$Trimestre_Cod, 6, 6),
      substr(df_pecuaria_trimestral$Trimestre_Cod, 1, 4)
    )
    rownames(df_pecuaria_trimestral) <- NULL
  }

  resumo_agro <- list(
    metadados = list(
      data_extracao = as.character(Sys.Date()),
      mes_lspa      = mes_lspa_ref,
      ano_pam       = ano_pam_ref,
      ano_ppm       = ano_ppm_ref,
      ano_pevs      = ano_pevs_ref
    ),
    lspa_safra_to          = df_lspa_resumo,
    pam_culturas_to        = df_pam_culturas,
    pam_municipios_top10   = df_pam_mun_top10,
    ppm_rebanhos_to        = df_ppm_rebanhos,
    ppm_bovinos_mun_top10  = df_ppm_bov_mun_top10,
    ppm_origem_animal_to   = df_ppm_origem_animal,
    pevs_resumo_to         = df_pevs_resumo,
    pecuaria_trimestral_to = df_pecuaria_trimestral,
    brutos = list(
      lspa        = raw_lspa,
      pam_uf      = raw_pam_uf,
      pam_mun     = raw_pam_mun,
      ppm_reb     = raw_ppm_reb,
      ppm_bov_mun = raw_ppm_bov_mun,
      ppm_orig    = raw_ppm_orig,
      pevs_area   = raw_pevs_area,
      pevs_silv   = raw_pevs_silv,
      pevs_ext    = raw_pevs_ext,
      abate_bov   = raw_abate_bov,
      abate_sui   = raw_abate_sui,
      abate_fra   = raw_abate_fra,
      leite       = raw_leite,
      ovos        = raw_ovos
    )
  )

  saveRDS(resumo_agro, caminho_rds, compress = "xz")
  log_ok("Salvar Agro RDS", sprintf("Arquivo '%s' salvo com sucesso.", caminho_rds), NA_real_, "extrair_agro_to.R")
  resumo_agro
}

if (sys.nframe() == 0L) {
  .libPaths(c("C:/Users/Lorenzo Costa/AppData/Local/R/win-library/4.4", .libPaths()))
  suppressPackageStartupMessages({ library(sidrar); library(dplyr) })
  extrair_agro_tocantins(diretorio_raiz = "D:/Journal", forcar_atualizacao = TRUE)
}
