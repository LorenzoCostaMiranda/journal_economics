# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/extrair_arrecadacao_to.R
# Objetivo:
#   1. Extrair automaticamente os dados oficiais de Arrecadação Tributária e
#      Finanças Públicas do Estado do Tocantins a partir de três fontes oficiais:
#      a) SEFAZ-TO (Arrecadação Geral por Tributos e por Atividade Econômica):
#         https://www.to.gov.br/sefaz/arrecadacao-de-icms-do-estado-por-tributo-e-por-atividade-economica/40uwqy972dvx
#      b) Boletim de Arrecadação dos Tributos Estaduais (CONFAZ / Dados Abertos):
#         https://dados.gov.br/dados/conjuntos-dados/boletim-de-arrecadacao-dos-tributos-estaduais
#      c) Secretaria do Tesouro Nacional (STN / Tesouro Transparente CKAN - FPE):
#         https://www.tesourotransparente.gov.br/ckan/dataset/transferencias-obrigatorias-da-uniao
#   2. Baixar temporariamente os arquivos brutos em 'D:/Journal/dados/tmp_microdados/',
#      processar no R, salvar pelo menos 3 anos completos de série mensal e 4 anos
#      de série anual em formato ultra-compacto (.rds com compressão XZ) e excluir
#      imediatamente os arquivos brutos temporários para não ocupar espaço no SSD.
#   3. Alimentar automaticamente:
#      - Aba 'Visão Geral': Box "Arrecadação ICMS (Var. Mensal)" e Gráfico
#        "Arrecadação ICMS e PIB trimestral".
#      - Aba 'Arrecadação e Finanças': Boxes "Receita Total (Mês)", "Arrecadação ICMS",
#        "Repasses Federais (FPE)" e "IPVA e Outros", Gráfico de Área Empilhada
#        "Evolução das Receitas" e Treemap "Composição do ICMS por Setor Econômico".
#      - Boletins Flash, Cartas de Conjuntura e Dossiês Anuais: tabelas mensais e
#        anuais por tributo e por Grandes Setores e Seções da CNAE 2.0.
# ==============================================================================

options(timeout = 300)

# Garante diretório raiz no SSD D: e pastas temporárias fora do disco C:
if (!file.exists("index.qmd") && dir.exists("D:/Journal")) {
  setwd("D:/Journal")
}
dir_tmp_arr <- normalizePath("dados/tmp_microdados", winslash = "/", mustWork = FALSE)
dir.create(dir_tmp_arr, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMP = dir_tmp_arr, TEMP = dir_tmp_arr, TMPDIR = dir_tmp_arr)

suppressPackageStartupMessages({
  for (pkg in c("data.table", "dplyr", "httr", "jsonlite", "pdftools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
    }
  }
  library(data.table)
  library(dplyr)
  library(httr)
  library(jsonlite)
  library(pdftools)
})

# ------------------------------------------------------------------------------
# 1. FUNÇÕES AUXILIARES DE FORMATAÇÃO E MAPEAMENTO CNAE 2.0
# ------------------------------------------------------------------------------
fmt_int_arr <- function(x) {
  format(round(as.numeric(x), 0), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_dec_arr <- function(x, casas = 2) {
  format(round(as.numeric(x), casas), big.mark = ".", decimal.mark = ",", nsmall = casas, scientific = FALSE, trim = TRUE)
}

fmt_moeda_mi_arr <- function(valor_mi, casas = 2) {
  v <- as.numeric(valor_mi)
  if (is.na(v)) return("N/D")
  if (abs(v) >= 1e6) {
    paste0("R$ ", fmt_dec_arr(v / 1e6, casas), " Tri")
  } else if (abs(v) >= 1e3) {
    paste0("R$ ", fmt_dec_arr(v / 1e3, casas), " Bi")
  } else {
    paste0("R$ ", fmt_dec_arr(v, casas), " Mi")
  }
}

parse_num_sefaz_to <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "-" || x == "") return(0)
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  val <- suppressWarnings(as.numeric(x))
  ifelse(is.na(val), 0, val)
}

# Padroniza nomes das rubricas da SEFAZ-TO e mapeia para Grandes Setores / Seções CNAE 2.0
normalizar_rubrica_sefaz <- function(rubrica, secao) {
  r <- trimws(gsub("\\s+", " ", rubrica))
  if (grepl("^Comb", r, ignore.case = TRUE)) r <- "Combustíveis"
  if (grepl("^Com.rcio", r, ignore.case = TRUE)) r <- "Comércio"
  if (grepl("^Ind.stria", r, ignore.case = TRUE)) r <- "Indústria"
  if (grepl("^Pecu.ria", r, ignore.case = TRUE)) r <- "Pecuária"
  if (grepl("^Comunica", r, ignore.case = TRUE)) r <- "Comunicação"
  if (grepl("^Rct\\. D.v\\. Ativa", r, ignore.case = TRUE)) r <- "Rct. Dív. Ativa"
  if (grepl("^IPVA D.v\\. Ativa", r, ignore.case = TRUE)) r <- "IPVA Dív. Ativa"
  if (grepl("^D.v\\. Ativa - Outros", r, ignore.case = TRUE)) r <- "Dív. Ativa - Outros"
  if (grepl("^Subtotal - Dedu", r, ignore.case = TRUE)) r <- "Subtotal - Deduções"
  if (grepl("^Restitui..o IRRF", r, ignore.case = TRUE)) r <- "Restituição IRRF"
  if (grepl("^Multa por A\\.I", r, ignore.case = TRUE)) r <- "Multa por A.I."
  r
}

mapear_cnae_icms <- function(rubrica) {
  dplyr::case_when(
    rubrica %in% c("Agricultura", "Pecuária", "Silvic/Explor", "Pesca") ~
      "Agropecuária (Seção A CNAE 2.0)",
    rubrica == "Indústria" ~
      "Indústria Geral e Construção (Seções B, C e F CNAE 2.0)",
    rubrica == "Combustíveis" ~
      "Combustíveis e Lubrificantes (Divisão 19 / Seção G CNAE 2.0)",
    rubrica == "Energia" ~
      "Eletricidade e Gás (Seção D CNAE 2.0)",
    rubrica == "Comércio" ~
      "Comércio Atacadista e Varejista (Seção G CNAE 2.0)",
    rubrica %in% c("Transportes", "Transp. Ferr.") ~
      "Transporte, Armazenagem e Correio (Seção H CNAE 2.0)",
    rubrica == "Comunicação" ~
      "Informação e Comunicação (Seção J CNAE 2.0)",
    TRUE ~
      "Demais Serviços e Outras Atividades (Seções I, K-S CNAE 2.0)"
  )
}

mapear_grande_setor_icms <- function(rubrica) {
  dplyr::case_when(
    rubrica %in% c("Agricultura", "Pecuária", "Silvic/Explor", "Pesca") ~ "Agropecuária",
    rubrica == "Indústria" ~ "Indústria",
    rubrica == "Combustíveis" ~ "Combustíveis",
    rubrica == "Energia" ~ "Energia Elétrica",
    rubrica == "Comércio" ~ "Comércio",
    rubrica %in% c("Transportes", "Transp. Ferr.") ~ "Transportes",
    rubrica == "Comunicação" ~ "Comunicação",
    TRUE ~ "Demais Atividades"
  )
}

# ------------------------------------------------------------------------------
# 2. RASPAGEM E PARSER AUTOMÁTICO DOS BOLETINS MENSAIS/ANUAIS DA SEFAZ-TO
# ------------------------------------------------------------------------------
descobrir_links_sefaz_to <- function(anos_alvo = 2024:as.integer(format(Sys.Date(), "%Y"))) {
  url_indice <- "https://www.to.gov.br/sefaz/arrecadacao-de-icms-do-estado-por-tributo-e-por-atividade-economica/40uwqy972dvx"
  
  # Links conhecidos de fallback caso o portal SEFAZ-TO passe por instabilidade momentânea
  links_fallback <- list(
    "2024" = "https://central.to.gov.br/download/410665",
    "2025" = "https://central.to.gov.br/download/460981",
    "2026" = "https://central.to.gov.br/download/491527"
  )
  
  resultado <- list()
  html_idx <- tryCatch({
    res <- httr::GET(url_indice, httr::timeout(30))
    if (httr::status_code(res) == 200) httr::content(res, "text", encoding = "UTF-8") else NULL
  }, error = function(e) NULL)
  
  for (ano in anos_alvo) {
    ano_chr <- as.character(ano)
    url_download <- NULL
    
    if (!is.null(html_idx)) {
      # Procura link para a subpágina do ano (ex: /sefaz/2026/5rsxi8lfs1kq)
      padrao_sub <- sprintf('href="((https?://www\\.to\\.gov\\.br)?/sefaz/%s/[a-zA-Z0-9_-]+)"', ano_chr)
      m_sub <- regmatches(html_idx, regexec(padrao_sub, html_idx))[[1]]
      if (length(m_sub) >= 2) {
        url_sub <- m_sub[2]
        if (!grepl("^https?://", url_sub)) url_sub <- paste0("https://www.to.gov.br", url_sub)
        html_sub <- tryCatch({
          r_sub <- httr::GET(url_sub, httr::timeout(30))
          if (httr::status_code(r_sub) == 200) httr::content(r_sub, "text", encoding = "UTF-8") else NULL
        }, error = function(e) NULL)
        
        if (!is.null(html_sub)) {
          # Captura todos os links central.to.gov.br/download/ID na página do ano
          # O primeiro link da tabela "1. ARRECADAÇÃO GERAL - POR TRIBUTOS E ATIVIDADES ECONÔMICAS" é sempre o principal
          m_dl <- regmatches(html_sub, gregexpr('https?://central\\.to\\.gov\\.br/download/[0-9]+', html_sub))[[1]]
          if (length(m_dl) >= 1) {
            url_download <- m_dl[1]
          }
        }
      }
    }
    
    if (is.null(url_download) && !is.null(links_fallback[[ano_chr]])) {
      url_download <- links_fallback[[ano_chr]]
    }
    if (!is.null(url_download)) {
      resultado[[ano_chr]] <- url_download
    }
  }
  resultado
}

parse_pdf_arrecadacao_sefaz_to <- function(pdf_path, ano_ref) {
  txt <- suppressMessages(pdftools::pdf_text(pdf_path))[1]
  linhas <- unlist(strsplit(txt, "\r?\n"))
  
  # Extrai Data de Emissão do rodapé (se existir)
  dt_emissao <- NA_character_
  m_dt <- regmatches(txt, regexec("Data de Emiss[^:]*:\\s*([0-9]{2}/[0-9]{2}/[0-9]{2,4})", txt))[[1]]
  if (length(m_dt) >= 2) {
    partes_dt <- unlist(strsplit(m_dt[2], "/"))
    if (nchar(partes_dt[3]) == 2) partes_dt[3] <- paste0("20", partes_dt[3])
    dt_emissao <- paste(partes_dt, collapse = "/")
  }
  
  secao_atual <- "ICMS"
  lista_mensal <- list()
  lista_anual  <- list()
  
  for (ln in linhas) {
    ln_trim <- trimws(ln)
    if (!nzchar(ln_trim)) next
    if (grepl("^(ARRECADA|POR TRIBUTOS|Atividade Econ|Set /|Dez /|Ago|Nov|Fonte:|Nota|Data de Emiss|1-Var)", ln_trim)) next
    
    m <- regexpr("\\s+(-|-?[0-9]{1,3}(\\.[0-9]{3})*(,[0-9]+)?)(\\s+(-|-?[0-9]{1,3}(\\.[0-9]{3})*(,[0-9]+)?)){15,18}$", ln_trim)
    if (m > 0) {
      rubrica_raw <- trimws(substr(ln_trim, 1, m - 1))
      cauda       <- trimws(substr(ln_trim, m, nchar(ln_trim)))
      tokens      <- unlist(strsplit(cauda, "\\s+"))
      
      if (length(tokens) >= 18) {
        rubrica <- normalizar_rubrica_sefaz(rubrica_raw, secao_atual)
        
        # Classifica se é linha de memo do ICMS (Multa por A.I. / ICMS menos Multa por A.I)
        secao_linha <- secao_atual
        if (secao_atual == "Outros_Tributos" && grepl("Multa por A\\.I", rubrica, ignore.case = TRUE)) {
          secao_linha <- "ICMS_Memo"
        }
        
        vals_mes        <- vapply(tokens[1:12], parse_num_sefaz_to, numeric(1))
        mes_preenchido  <- tokens[1:12] != "-"
        var_mes_ant     <- parse_num_sefaz_to(tokens[13])
        val_mes_ano_ant <- parse_num_sefaz_to(tokens[14])
        var_yoy_mes     <- parse_num_sefaz_to(tokens[15])
        acum_ano_atual  <- parse_num_sefaz_to(tokens[16])
        acum_ano_ant    <- parse_num_sefaz_to(tokens[17])
        var_acum_yoy    <- parse_num_sefaz_to(tokens[18])
        rep_acum_pct    <- if (length(tokens) >= 19 && tokens[19] != "-") parse_num_sefaz_to(tokens[19]) else NA_real_
        
        # Determina quais meses estão efetivamente publicados na tabela
        # (Um mês é considerado publicado no boletim se no TOTAL GERAL ou Subtotal - ICMS o token != '-')
        for (mes_idx in 1:12) {
          lista_mensal[[length(lista_mensal) + 1]] <- data.frame(
            Ano               = as.integer(ano_ref),
            Mes               = as.integer(mes_idx),
            Competencia       = as.integer(ano_ref * 100L + mes_idx),
            Secao             = secao_linha,
            Rubrica           = rubrica,
            Valor_RS          = vals_mes[mes_idx],
            Token_Bruto       = tokens[mes_idx],
            Publicado_Mes     = mes_preenchido[mes_idx],
            stringsAsFactors  = FALSE
          )
        }
        
        lista_anual[[length(lista_anual) + 1]] <- data.frame(
          Ano                  = as.integer(ano_ref),
          Secao                = secao_linha,
          Rubrica              = rubrica,
          Var_Ultimo_Mes_Pct   = var_mes_ant,
          Valor_Mes_Ano_Ant_RS = val_mes_ano_ant,
          Var_YoY_Ult_Mes_Pct  = var_yoy_mes,
          Acumulado_Ano_RS     = acum_ano_atual,
          Acumulado_Ano_Ant_RS = acum_ano_ant,
          Var_Acum_YoY_Pct     = var_acum_yoy,
          Rep_Ativ_Acum_Pct    = rep_acum_pct,
          Data_Emissao_SEFAZ   = dt_emissao,
          stringsAsFactors     = FALSE
        )
        
        if (rubrica == "Subtotal - ICMS") {
          secao_atual <- "Outros_Tributos"
        } else if (rubrica == "Subtotal - Outros") {
          secao_atual <- "Deducoes"
        } else if (rubrica == "Subtotal - Deduções") {
          secao_atual <- "Total_Geral"
        }
      }
    }
  }
  
  df_m <- bind_rows(lista_mensal)
  df_a <- bind_rows(lista_anual)
  
  # Filtra apenas os meses em que houve publicação no boletim (Subtotal - ICMS publicado)
  meses_ativos <- df_m %>%
    filter(Rubrica == "Subtotal - ICMS", Publicado_Mes, Valor_RS > 0) %>%
    pull(Mes) %>%
    unique()
  
  df_m <- df_m %>%
    filter(Mes %in% meses_ativos) %>%
    select(-Token_Bruto, -Publicado_Mes)
  
  list(
    ano          = ano_ref,
    meses_ativos = sort(meses_ativos),
    dt_emissao   = dt_emissao,
    mensal       = df_m,
    anual        = df_a
  )
}

# ------------------------------------------------------------------------------
# 3. EXTRAÇÃO DO BOLETIM CONFAZ (DADOS ABERTOS) - DETALHE ATACADISTA / VAREJISTA
# ------------------------------------------------------------------------------
extrair_confaz_to <- function() {
  rk       <- "8d568ffd-e9ec-420b-a0a8-0fc286725476"
  model_id <- 8734205L
  url_q    <- "https://wabi-brazil-south-api.analysis.windows.net/public/reports/querydata?synchronous=true"
  
  parse_dsr <- function(json_txt, col_names) {
    obj   <- jsonlite::fromJSON(json_txt, simplifyVector = FALSE)
    ds    <- obj$results[[1]]$result$data$dsr$DS[[1]]
    dm    <- ds$PH[[1]]$DM0
    if (is.null(dm) || length(dm) == 0) return(NULL)
    dicts <- ds$ValueDicts
    schema <- dm[[1]]$S
    n_cols <- length(schema)
    dict_names <- vapply(schema, function(s) if (!is.null(s$DN)) s$DN else "", character(1))
    rows_list <- vector("list", length(dm))
    prev_vals <- rep(NA_character_, n_cols)
    for (i in seq_along(dm)) {
      item <- dm[[i]]
      c_vec <- item$C
      r_mask <- if (!is.null(item$R)) as.integer(item$R) else 0L
      u_mask <- if (!is.null(item[["\u00d8"]])) as.integer(item[["\u00d8"]]) else 0L
      curr_vals <- prev_vals
      c_idx <- 1L
      for (col_i in seq_len(n_cols)) {
        bit <- bitwShiftL(1L, col_i - 1L)
        if (bitwAnd(u_mask, bit) != 0L) {
          curr_vals[col_i] <- NA_character_
        } else if (bitwAnd(r_mask, bit) == 0L) {
          raw_v <- c_vec[[c_idx]]
          c_idx <- c_idx + 1L
          dn <- dict_names[col_i]
          if (nzchar(dn) && !is.null(dicts[[dn]])) {
            curr_vals[col_i] <- as.character(dicts[[dn]][[as.integer(raw_v) + 1L]])
          } else {
            curr_vals[col_i] <- as.character(raw_v)
          }
        }
      }
      prev_vals <- curr_vals
      rows_list[[i]] <- curr_vals
    }
    df <- as.data.frame(do.call(rbind, rows_list), stringsAsFactors = FALSE)
    names(df) <- col_names
    df
  }
  
  tryCatch({
    body_tipos <- list(
      version = "1.0.0",
      queries = list(list(
        Query = list(Commands = list(list(
          SemanticQueryDataShapeCommand = list(
            Query = list(
              Version = 2L,
              From = list(list(Name = "t", Entity = "tblTipoArrecadacao", Type = 0L)),
              Select = list(
                list(Column = list(Expression = list(SourceRef = list(Source = "t")), Property = "CD_TIP_ARRECAD"), Name = "co_tipo"),
                list(Column = list(Expression = list(SourceRef = list(Source = "t")), Property = "NM_TIP_ARRECAD"), Name = "nm_tipo"),
                list(Column = list(Expression = list(SourceRef = list(Source = "t")), Property = "NM_SETOR"), Name = "nm_setor"),
                list(Column = list(Expression = list(SourceRef = list(Source = "t")), Property = "NM_GRUPO_SETOR"), Name = "nm_grupo"),
                list(Column = list(Expression = list(SourceRef = list(Source = "t")), Property = "TIP_TRIBUTO"), Name = "tip_tributo")
              )
            ),
            Binding = list(
              Primary = list(Groupings = list(list(Projections = list(0L, 1L, 2L, 3L, 4L)))),
              DataReduction = list(DataVolume = 4L, Primary = list(Window = list(Count = 500L))),
              Version = 1L
            )
          )
        ))),
        ModelId = model_id
      )),
      cancelQueries = list(),
      modelId = model_id
    )
    
    res_t <- httr::POST(
      url_q,
      httr::add_headers(`X-PowerBI-ResourceKey` = rk, `Content-Type` = "application/json", Accept = "application/json"),
      body = jsonlite::toJSON(body_tipos, auto_unbox = TRUE),
      httr::timeout(30)
    )
    if (httr::status_code(res_t) != 200) return(NULL)
    df_tipos <- parse_dsr(httr::content(res_t, "text", encoding = "UTF-8"),
                          c("co_tipo", "nm_tipo", "nm_setor", "nm_grupo", "tip_tributo"))
    df_tipos$co_tipo <- as.integer(df_tipos$co_tipo)
    
    body_to <- list(
      version = "1.0.0",
      queries = list(list(
        Query = list(Commands = list(list(
          SemanticQueryDataShapeCommand = list(
            Query = list(
              Version = 2L,
              From = list(list(Name = "a", Entity = "bi_sigdef arrecadacao", Type = 0L)),
              Select = list(
                list(Column = list(Expression = list(SourceRef = list(Source = "a")), Property = "ano_arrecadacao"), Name = "ano"),
                list(Column = list(Expression = list(SourceRef = list(Source = "a")), Property = "mes_arrecadacao"), Name = "mes"),
                list(Column = list(Expression = list(SourceRef = list(Source = "a")), Property = "co_tipo_arrecadacao"), Name = "co_tipo"),
                list(Aggregation = list(Expression = list(Column = list(Expression = list(SourceRef = list(Source = "a")), Property = "va_arrecadacao")), Function = 0L), Name = "valor")
              ),
              Where = list(
                list(Condition = list(In = list(
                  Expressions = list(list(Column = list(Expression = list(SourceRef = list(Source = "a")), Property = "sg_uf"))),
                  Values = list(list(list(Literal = list(Value = "'TO'"))))
                ))),
                list(Condition = list(In = list(
                  Expressions = list(list(Column = list(Expression = list(SourceRef = list(Source = "a")), Property = "ano_arrecadacao"))),
                  Values = lapply(as.character(2023:(as.integer(format(Sys.Date(), "%Y")) + 1L)), function(y) list(list(Literal = list(Value = paste0("'", y, "'")))))
                )))
              )
            ),
            Binding = list(
              Primary = list(Groupings = list(list(Projections = list(0L, 1L, 2L, 3L)))),
              DataReduction = list(DataVolume = 4L, Primary = list(Window = list(Count = 5000L))),
              Version = 1L
            )
          )
        ))),
        ModelId = model_id
      )),
      cancelQueries = list(),
      modelId = model_id
    )
    
    res_to <- httr::POST(
      url_q,
      httr::add_headers(`X-PowerBI-ResourceKey` = rk, `Content-Type` = "application/json", Accept = "application/json"),
      body = jsonlite::toJSON(body_to, auto_unbox = TRUE),
      httr::timeout(30)
    )
    if (httr::status_code(res_to) != 200) return(NULL)
    df_to <- parse_dsr(httr::content(res_to, "text", encoding = "UTF-8"),
                       c("ano", "mes", "co_tipo", "valor"))
    if (is.null(df_to)) return(NULL)
    df_to$ano     <- as.integer(df_to$ano)
    df_to$mes     <- as.integer(df_to$mes)
    df_to$co_tipo <- as.integer(df_to$co_tipo)
    df_to$valor   <- as.numeric(df_to$valor)
    df_to %>% left_join(df_tipos, by = "co_tipo")
  }, error = function(e) {
    message(sprintf("   [AVISO] Falha ao consultar API CONFAZ (%s).", e$message))
    NULL
  })
}

# ------------------------------------------------------------------------------
# 4. EXTRAÇÃO AUTOMÁTICA DOS REPASSES DO FPE (TESOURO TRANSPARENTE / STN)
# ------------------------------------------------------------------------------
extrair_fpe_to_stn <- function(dir_tmp = "dados/tmp_microdados") {
  url_fpe_default <- "https://www.tesourotransparente.gov.br/ckan/dataset/f85b6632-1c9c-4beb-9e60-72e91156c984/resource/d99de819-0efe-4653-9a6d-1f46c208e740/download/fpe.csv"
  
  # Tenta descobrir dinamicamente via API CKAN do Tesouro Transparente caso o resource ID mude
  url_fpe <- tryCatch({
    res_ckan <- httr::GET("https://www.tesourotransparente.gov.br/ckan/api/3/action/package_show?id=transferencias-obrigatorias-da-uniao", httr::timeout(20))
    if (httr::status_code(res_ckan) == 200) {
      pkg <- jsonlite::fromJSON(httr::content(res_ckan, "text", encoding = "UTF-8"), simplifyVector = FALSE)
      recursos <- pkg$result$resources
      u_found <- NULL
      for (r in recursos) {
        if (toupper(trimws(as.character(r$name))) == "FPE") {
          u_found <- as.character(r$url)
          break
        }
      }
      if (!is.null(u_found) && nzchar(u_found)) u_found else url_fpe_default
    } else {
      url_fpe_default
    }
  }, error = function(e) url_fpe_default)
  
  tmp_csv <- file.path(dir_tmp, sprintf("fpe_stn_%d.csv", Sys.getpid()))
  on.exit({
    if (file.exists(tmp_csv)) unlink(tmp_csv, force = TRUE)
  }, add = TRUE)
  
  tryCatch({
    utils::download.file(url_fpe, tmp_csv, mode = "wb", quiet = TRUE)
    if (!file.exists(tmp_csv) || file.info(tmp_csv)$size < 1000) return(NULL)
    
    linhas <- readLines(tmp_csv, encoding = "latin1", warn = FALSE)
    idx_hdr <- grep("^ESTADOS;UF;", linhas)[1]
    if (is.na(idx_hdr)) return(NULL)
    
    dt <- data.table::fread(text = linhas[idx_hdr:length(linhas)], sep = ";")
    to_row <- dt[UF == "TO"]
    if (nrow(to_row) == 0) return(NULL)
    
    meses_pt <- c("janeiro" = 1L, "fevereiro" = 2L, "março" = 3L, "marco" = 3L,
                  "abril" = 4L, "maio" = 5L, "junho" = 6L, "julho" = 7L,
                  "agosto" = 8L, "setembro" = 9L, "outubro" = 10L,
                  "novembro" = 11L, "dezembro" = 12L)
    
    cols_mes <- setdiff(names(to_row), c("ESTADOS", "UF"))
    registros_fpe <- list()
    
    for (col_nm in cols_mes) {
      if (grepl("^V[0-9]+$", col_nm)) next
      partes <- unlist(strsplit(tolower(trimws(col_nm)), "/"))
      if (length(partes) != 2) next
      m_num <- unname(meses_pt[partes[1]])
      y_raw <- suppressWarnings(as.integer(partes[2]))
      if (is.na(m_num) || is.na(y_raw)) next
      ano_full <- if (y_raw < 100L) {
        if (y_raw >= 90L) 1900L + y_raw else 2000L + y_raw
      } else {
        y_raw
      }
      if (ano_full < 2022L) next
      
      val_fpe <- parse_num_sefaz_to(to_row[[col_nm]][1])
      if (val_fpe > 0) {
        registros_fpe[[length(registros_fpe) + 1]] <- data.frame(
          Ano         = as.integer(ano_full),
          Mes         = as.integer(m_num),
          Competencia = as.integer(ano_full * 100L + m_num),
          FPE_RS      = val_fpe,
          stringsAsFactors = FALSE
        )
      }
    }
    bind_rows(registros_fpe) %>% arrange(Competencia)
  }, error = function(e) {
    message(sprintf("   [AVISO] Falha ao extrair FPE do Tesouro Transparente (%s).", e$message))
    NULL
  })
}

# ------------------------------------------------------------------------------
# 5. PIPELINE PRINCIPAL DE EXTRAÇÃO, CONSOLIDAÇÃO E LIMPEZA TEMPORÁRIA
# ------------------------------------------------------------------------------
executar_extracao_arrecadacao_to <- function(
  anos_alvo              = 2024:as.integer(format(Sys.Date(), "%Y")),
  caminho_base_rds       = "dados/arrecadacao_to_base.rds",
  caminho_resumo_rds     = "dados/arrecadacao_to_resumo.rds",
  dir_tmp                = "dados/tmp_microdados",
  forcar_reprocessamento = FALSE
) {
  dir.create(dirname(caminho_base_rds), recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_tmp, recursive = TRUE, showWarnings = FALSE)
  
  cat("========================================================\n")
  cat("-> Iniciando Extracao Automatica de Arrecadacao e Financas (TO)...\n")
  
  base_existente <- if (file.exists(caminho_base_rds) && !forcar_reprocessamento) {
    tryCatch(readRDS(caminho_base_rds), error = function(e) NULL)
  } else {
    NULL
  }
  
  # 5.1. Extrai PDFs anuais/mensais da SEFAZ-TO
  links_sefaz <- descobrir_links_sefaz_to(anos_alvo)
  dados_sefaz_por_ano <- if (!is.null(base_existente) && !is.null(base_existente$sefaz_por_ano)) {
    base_existente$sefaz_por_ano
  } else {
    list()
  }
  
  ano_corrente <- as.integer(format(Sys.Date(), "%Y"))
  
  for (ano in anos_alvo) {
    ano_chr <- as.character(ano)
    ja_completo <- !is.null(dados_sefaz_por_ano[[ano_chr]]) &&
      ano < ano_corrente &&
      length(dados_sefaz_por_ano[[ano_chr]]$meses_ativos) == 12
    
    if (ja_completo && !forcar_reprocessamento) {
      cat(sprintf("   [OK] Ano %d ja consolidado na base local (12 meses). Pulando download.\n", ano))
      next
    }
    
    url_pdf <- links_sefaz[[ano_chr]]
    if (is.null(url_pdf)) next
    
    # Se já houver um PDF temporário na pasta tmp_microdados (ex: recém-baixado), usa e depois exclui
    arq_pdf_existente <- file.path(dir_tmp, sprintf("sefaz_arrecadacao_%d.pdf", ano))
    arq_pdf_tmp       <- file.path(dir_tmp, sprintf("sefaz_arrecadacao_%d_%d.pdf", ano, Sys.getpid()))
    
    baixou_ok <- FALSE
    if (file.exists(arq_pdf_existente) && file.info(arq_pdf_existente)$size > 5000) {
      file.copy(arq_pdf_existente, arq_pdf_tmp, overwrite = TRUE)
      baixou_ok <- TRUE
    } else {
      cat(sprintf("   [DOWNLOAD] Baixando Boletim SEFAZ-TO (%d): %s ...\n", ano, url_pdf))
      tryCatch({
        utils::download.file(url_pdf, arq_pdf_tmp, mode = "wb", quiet = TRUE)
        if (file.exists(arq_pdf_tmp) && file.info(arq_pdf_tmp)$size > 5000) {
          baixou_ok <- TRUE
        }
      }, error = function(e) {
        message(sprintf("   [AVISO] Falha ao baixar PDF SEFAZ-TO %d: %s", ano, e$message))
      })
    }
    
    if (baixou_ok) {
      parsed_ano <- tryCatch({
        parse_pdf_arrecadacao_sefaz_to(arq_pdf_tmp, ano)
      }, error = function(e) {
        message(sprintf("   [AVISO] Erro ao ler PDF SEFAZ-TO %d: %s", ano, e$message))
        NULL
      })
      
      # Exclui imediatamente os PDFs temporários para liberar espaço no SSD
      if (file.exists(arq_pdf_tmp)) unlink(arq_pdf_tmp, force = TRUE)
      if (file.exists(arq_pdf_existente)) unlink(arq_pdf_existente, force = TRUE)
      
      if (!is.null(parsed_ano) && nrow(parsed_ano$mensal) > 0) {
        dados_sefaz_por_ano[[ano_chr]] <- parsed_ano
        cat(sprintf("   [OK] SEFAZ-TO %d processado (%d meses ativos; emissao: %s) e PDF temporario excluido.\n",
                    ano, length(parsed_ano$meses_ativos),
                    ifelse(is.na(parsed_ano$dt_emissao), "N/D", parsed_ano$dt_emissao)))
      }
    }
  }
  
  # 5.2. Extrai dados abertos do CONFAZ (Boletim de Arrecadação dos Tributos Estaduais)
  cat("   [API] Consultando Boletim de Arrecadacao CONFAZ (Dados Abertos)...\n")
  df_confaz_to <- extrair_confaz_to()
  if (is.null(df_confaz_to) && !is.null(base_existente) && !is.null(base_existente$confaz_to)) {
    df_confaz_to <- base_existente$confaz_to
  }
  
  # 5.3. Extrai Repasses do FPE (Tesouro Transparente / STN)
  cat("   [API] Consultando Repasses do FPE para o Tocantins (Tesouro Nacional)...\n")
  df_fpe_to <- extrair_fpe_to_stn(dir_tmp = dir_tmp)
  if (is.null(df_fpe_to) && !is.null(base_existente) && !is.null(base_existente$fpe_to)) {
    df_fpe_to <- base_existente$fpe_to
  }
  
  # Limpa quaisquer arquivos temporários residuais de testes na pasta tmp_microdados
  arqs_residuais <- list.files(dir_tmp, pattern = "^(fpe_test|tr26|confaz_|sefaz_arrecadacao_)", full.names = TRUE)
  if (length(arqs_residuais) > 0) {
    unlink(arqs_residuais, force = TRUE)
  }
  
  if (length(dados_sefaz_por_ano) == 0) {
    if (file.exists(caminho_resumo_rds)) {
      cat("   [FALLBACK] Usando resumo local existente em", caminho_resumo_rds, "\n")
      return(readRDS(caminho_resumo_rds))
    }
    stop("Nao foi possivel obter dados de arrecadacao da SEFAZ-TO.")
  }
  
  # Salva a base consolidada bruta/estruturada em formato ultra-compacto (compress = "xz")
  base_consolidada <- list(
    ultima_extracao = format(Sys.time(), "%d/%m/%Y %H:%M:%S"),
    sefaz_por_ano   = dados_sefaz_por_ano,
    confaz_to       = df_confaz_to,
    fpe_to          = df_fpe_to
  )
  saveRDS(base_consolidada, file = caminho_base_rds, compress = "xz")
  
  # ----------------------------------------------------------------------------
  # 6. CONSTRUÇÃO DAS SÉRIES MENSAIS, ANUAIS E POR SETOR CNAE 2.0
  # ----------------------------------------------------------------------------
  df_mensal_all <- bind_rows(lapply(dados_sefaz_por_ano, function(x) x$mensal)) %>%
    arrange(Competencia, Secao, Rubrica)
  
  df_anual_all <- bind_rows(lapply(dados_sefaz_por_ano, function(x) x$anual)) %>%
    arrange(Ano, Secao, Rubrica)
  
  meses_nomes_curtos  <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  meses_nomes_extenso <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                           "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
  
  # 6.1. Tabela Mensal de Tributos Estaduais + FPE + Receita Total (2024 a 2026)
  # Pivota as rubricas principais por mês/ano
  df_mensal_tributos <- df_mensal_all %>%
    group_by(Ano, Mes, Competencia) %>%
    summarise(
      ICMS_RS             = sum(Valor_RS[Secao == "ICMS" & Rubrica == "Subtotal - ICMS"], na.rm = TRUE),
      IPVA_Principal_RS   = sum(Valor_RS[Secao == "Outros_Tributos" & Rubrica == "IPVA"], na.rm = TRUE),
      IPVA_Div_Ativa_RS   = sum(Valor_RS[Secao == "Outros_Tributos" & Rubrica == "IPVA Dív. Ativa"], na.rm = TRUE),
      IPVA_Total_RS       = IPVA_Principal_RS + IPVA_Div_Ativa_RS,
      ITCD_RS             = sum(Valor_RS[Secao == "Outros_Tributos" & Rubrica == "ITCD"], na.rm = TRUE),
      IRRF_Total_RS       = sum(Valor_RS[Secao == "Outros_Tributos" & Rubrica %in% c("IRRF", "IRRF outros rendim.")], na.rm = TRUE),
      Taxas_RS            = sum(Valor_RS[Secao == "Outros_Tributos" & Rubrica == "Taxa"], na.rm = TRUE),
      Div_Ativa_Outros_RS = sum(Valor_RS[Secao == "Outros_Tributos" & Rubrica == "Dív. Ativa - Outros"], na.rm = TRUE),
      Outras_Receitas_RS  = sum(Valor_RS[Secao == "Outros_Tributos" & Rubrica == "Outras Receitas"], na.rm = TRUE),
      Subtotal_Outros_RS  = sum(Valor_RS[Secao == "Outros_Tributos" & Rubrica == "Subtotal - Outros"], na.rm = TRUE),
      Deducoes_RS         = sum(Valor_RS[Secao == "Deducoes" & Rubrica == "Subtotal - Deduções"], na.rm = TRUE),
      Receita_Propria_RS  = sum(Valor_RS[Secao == "Total_Geral" & Rubrica == "TOTAL GERAL"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Competencia)
  
  if (!is.null(df_fpe_to) && nrow(df_fpe_to) > 0) {
    df_mensal_tributos <- df_mensal_tributos %>%
      left_join(df_fpe_to %>% select(Competencia, FPE_RS), by = "Competencia")
  } else {
    df_mensal_tributos$FPE_RS <- NA_real_
  }
  
  df_mensal_tributos <- df_mensal_tributos %>%
    mutate(
      Mes_Nome             = meses_nomes_curtos[Mes],
      Periodo              = sprintf("%s/%d", meses_nomes_curtos[Mes], Ano),
      Trimestre_Num        = ceiling(Mes / 3),
      Trimestre_Cod        = sprintf("%d0%d", Ano, ceiling(Mes / 3)),
      Trimestre_Rotulo     = sprintf("T%d %d", ceiling(Mes / 3), Ano),
      FPE_RS               = coalesce(FPE_RS, 0),
      Receita_Total_RS     = Receita_Propria_RS + FPE_RS,
      ICMS_Mi              = round(ICMS_RS / 1e6, 2),
      IPVA_Total_Mi        = round(IPVA_Total_RS / 1e6, 2),
      ITCD_Mi              = round(ITCD_RS / 1e6, 2),
      IRRF_Total_Mi        = round(IRRF_Total_RS / 1e6, 2),
      Taxas_Mi             = round(Taxas_RS / 1e6, 2),
      Subtotal_Outros_Mi   = round(Subtotal_Outros_RS / 1e6, 2),
      Receita_Propria_Mi   = round(Receita_Propria_RS / 1e6, 2),
      FPE_Mi               = round(FPE_RS / 1e6, 2),
      Receita_Total_Mi     = round(Receita_Total_RS / 1e6, 2),
      Var_Mensal_ICMS_Pct  = round((ICMS_RS / lag(ICMS_RS) - 1) * 100, 2),
      Var_YoY_ICMS_Pct     = round((ICMS_RS / lag(ICMS_RS, 12) - 1) * 100, 2),
      Var_Mensal_Rec_Pct   = round((Receita_Total_RS / lag(Receita_Total_RS) - 1) * 100, 2),
      Var_YoY_Rec_Pct      = round((Receita_Total_RS / lag(Receita_Total_RS, 12) - 1) * 100, 2)
    ) %>%
    as.data.frame()
  
  # 6.2. Série Anual de Tributos para os Dossiês Anuais (2023, 2024, 2025 e Acumulado 2026)
  # Resgata também o ano de 2023 a partir da coluna 'Acumulado_Ano_Ant_RS' do boletim de 2024!
  lista_anos_dossie <- list()
  
  if ("2024" %in% names(dados_sefaz_por_ano)) {
    a24 <- dados_sefaz_por_ano[["2024"]]$anual
    fpe_2023 <- if (!is.null(df_fpe_to)) sum(df_fpe_to$FPE_RS[df_fpe_to$Ano == 2023], na.rm = TRUE) else 0
    icms_23  <- sum(a24$Acumulado_Ano_Ant_RS[a24$Secao == "ICMS" & a24$Rubrica == "Subtotal - ICMS"], na.rm = TRUE)
    ipva_23  <- sum(a24$Acumulado_Ano_Ant_RS[a24$Secao == "Outros_Tributos" & a24$Rubrica %in% c("IPVA", "IPVA Dív. Ativa")], na.rm = TRUE)
    itcd_23  <- sum(a24$Acumulado_Ano_Ant_RS[a24$Secao == "Outros_Tributos" & a24$Rubrica == "ITCD"], na.rm = TRUE)
    irrf_23  <- sum(a24$Acumulado_Ano_Ant_RS[a24$Secao == "Outros_Tributos" & a24$Rubrica %in% c("IRRF", "IRRF outros rendim.")], na.rm = TRUE)
    taxa_23  <- sum(a24$Acumulado_Ano_Ant_RS[a24$Secao == "Outros_Tributos" & a24$Rubrica == "Taxa"], na.rm = TRUE)
    outr_23  <- sum(a24$Acumulado_Ano_Ant_RS[a24$Secao == "Outros_Tributos" & a24$Rubrica == "Subtotal - Outros"], na.rm = TRUE)
    ded_23   <- sum(a24$Acumulado_Ano_Ant_RS[a24$Secao == "Deducoes" & a24$Rubrica == "Subtotal - Deduções"], na.rm = TRUE)
    recp_23  <- sum(a24$Acumulado_Ano_Ant_RS[a24$Secao == "Total_Geral" & a24$Rubrica == "TOTAL GERAL"], na.rm = TRUE)
    
    lista_anos_dossie[["2023"]] <- data.frame(
      Ano                = 2023L,
      Periodo_Anual      = "2023 (Jan-Dez)",
      Meses_Cobertos     = 12L,
      ICMS_RS            = icms_23,
      IPVA_Total_RS      = ipva_23,
      ITCD_RS            = itcd_23,
      IRRF_Total_RS      = irrf_23,
      Taxas_RS           = taxa_23,
      Subtotal_Outros_RS = outr_23,
      Deducoes_RS        = ded_23,
      Receita_Propria_RS = recp_23,
      FPE_RS             = fpe_2023,
      Receita_Total_RS   = recp_23 + fpe_2023,
      Var_ICMS_YoY_Pct   = NA_real_,
      Var_RecTot_YoY_Pct = NA_real_,
      stringsAsFactors   = FALSE
    )
  }
  
  for (ano_k in sort(as.integer(names(dados_sefaz_por_ano)))) {
    obj_k <- dados_sefaz_por_ano[[as.character(ano_k)]]
    ak    <- obj_k$anual
    n_m   <- length(obj_k$meses_ativos)
    ult_m <- max(obj_k$meses_ativos)
    
    fpe_k <- if (!is.null(df_fpe_to)) {
      sum(df_fpe_to$FPE_RS[df_fpe_to$Ano == ano_k & df_fpe_to$Mes %in% obj_k$meses_ativos], na.rm = TRUE)
    } else 0
    fpe_ant_k <- if (!is.null(df_fpe_to)) {
      sum(df_fpe_to$FPE_RS[df_fpe_to$Ano == (ano_k - 1L) & df_fpe_to$Mes %in% obj_k$meses_ativos], na.rm = TRUE)
    } else 0
    
    icms_k     <- sum(ak$Acumulado_Ano_RS[ak$Secao == "ICMS" & ak$Rubrica == "Subtotal - ICMS"], na.rm = TRUE)
    icms_ant_k <- sum(ak$Acumulado_Ano_Ant_RS[ak$Secao == "ICMS" & ak$Rubrica == "Subtotal - ICMS"], na.rm = TRUE)
    ipva_k     <- sum(ak$Acumulado_Ano_RS[ak$Secao == "Outros_Tributos" & ak$Rubrica %in% c("IPVA", "IPVA Dív. Ativa")], na.rm = TRUE)
    itcd_k     <- sum(ak$Acumulado_Ano_RS[ak$Secao == "Outros_Tributos" & ak$Rubrica == "ITCD"], na.rm = TRUE)
    irrf_k     <- sum(ak$Acumulado_Ano_RS[ak$Secao == "Outros_Tributos" & ak$Rubrica %in% c("IRRF", "IRRF outros rendim.")], na.rm = TRUE)
    taxa_k     <- sum(ak$Acumulado_Ano_RS[ak$Secao == "Outros_Tributos" & ak$Rubrica == "Taxa"], na.rm = TRUE)
    outr_k     <- sum(ak$Acumulado_Ano_RS[ak$Secao == "Outros_Tributos" & ak$Rubrica == "Subtotal - Outros"], na.rm = TRUE)
    ded_k      <- sum(ak$Acumulado_Ano_RS[ak$Secao == "Deducoes" & ak$Rubrica == "Subtotal - Deduções"], na.rm = TRUE)
    recp_k     <- sum(ak$Acumulado_Ano_RS[ak$Secao == "Total_Geral" & ak$Rubrica == "TOTAL GERAL"], na.rm = TRUE)
    recp_ant_k <- sum(ak$Acumulado_Ano_Ant_RS[ak$Secao == "Total_Geral" & ak$Rubrica == "TOTAL GERAL"], na.rm = TRUE)
    
    rotulo_anual <- if (n_m == 12) {
      sprintf("%d (Jan-Dez)", ano_k)
    } else {
      sprintf("%d (Acum. Jan-%s)", ano_k, meses_nomes_curtos[ult_m])
    }
    
    var_icms_yoy_k <- if (icms_ant_k > 0) round(((icms_k / icms_ant_k) - 1) * 100, 2) else NA_real_
    var_rect_yoy_k <- if ((recp_ant_k + fpe_ant_k) > 0) round((((recp_k + fpe_k) / (recp_ant_k + fpe_ant_k)) - 1) * 100, 2) else NA_real_
    
    lista_anos_dossie[[as.character(ano_k)]] <- data.frame(
      Ano                = as.integer(ano_k),
      Periodo_Anual      = rotulo_anual,
      Meses_Cobertos     = as.integer(n_m),
      ICMS_RS            = icms_k,
      IPVA_Total_RS      = ipva_k,
      ITCD_RS            = itcd_k,
      IRRF_Total_RS      = irrf_k,
      Taxas_RS           = taxa_k,
      Subtotal_Outros_RS = outr_k,
      Deducoes_RS        = ded_k,
      Receita_Propria_RS = recp_k,
      FPE_RS             = fpe_k,
      Receita_Total_RS   = recp_k + fpe_k,
      Var_ICMS_YoY_Pct   = var_icms_yoy_k,
      Var_RecTot_YoY_Pct = var_rect_yoy_k,
      stringsAsFactors   = FALSE
    )
  }
  
  df_anual_tributos <- bind_rows(lista_anos_dossie) %>%
    arrange(Ano) %>%
    mutate(
      ICMS_Mi            = round(ICMS_RS / 1e6, 2),
      IPVA_Total_Mi      = round(IPVA_Total_RS / 1e6, 2),
      ITCD_Mi            = round(ITCD_RS / 1e6, 2),
      IRRF_Total_Mi      = round(IRRF_Total_RS / 1e6, 2),
      Taxas_Mi           = round(Taxas_RS / 1e6, 2),
      Subtotal_Outros_Mi = round(Subtotal_Outros_RS / 1e6, 2),
      Receita_Propria_Mi = round(Receita_Propria_RS / 1e6, 2),
      FPE_Mi             = round(FPE_RS / 1e6, 2),
      Receita_Total_Mi   = round(Receita_Total_RS / 1e6, 2)
    ) %>%
    as.data.frame()
  
  # 6.3. Detalhamento Mensal e Anual do ICMS por Atividade Econômica e Grande Setor CNAE 2.0
  # Calcula a proporção histórica Comércio Atacadista vs Varejista no CONFAZ para o Tocantins
  prop_atacadista_confaz <- 0.456
  if (!is.null(df_confaz_to) && nrow(df_confaz_to) > 0) {
    df_com_split <- df_confaz_to %>%
      filter(co_tipo %in% c(131L, 132L), valor > 0) %>%
      group_by(co_tipo) %>%
      summarise(tot = sum(valor, na.rm = TRUE), .groups = "drop")
    if (nrow(df_com_split) == 2 && sum(df_com_split$tot) > 0) {
      prop_atacadista_confaz <- df_com_split$tot[df_com_split$co_tipo == 131L] / sum(df_com_split$tot)
    }
  }
  
  df_mensal_icms_atividades <- df_mensal_all %>%
    filter(Secao == "ICMS", Rubrica != "Subtotal - ICMS") %>%
    mutate(
      Mes_Nome         = meses_nomes_curtos[Mes],
      Periodo          = sprintf("%s/%d", meses_nomes_curtos[Mes], Ano),
      Secao_CNAE       = mapear_cnae_icms(Rubrica),
      Grande_Setor     = mapear_grande_setor_icms(Rubrica),
      Valor_Mi         = round(Valor_RS / 1e6, 3)
    ) %>%
    group_by(Competencia) %>%
    mutate(
      Participacao_ICMS_Pct = round((Valor_RS / sum(Valor_RS, na.rm = TRUE)) * 100, 2)
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  df_mensal_icms_cnae <- df_mensal_icms_atividades %>%
    group_by(Ano, Mes, Competencia, Periodo, Grande_Setor, Secao_CNAE) %>%
    summarise(
      Valor_RS = sum(Valor_RS, na.rm = TRUE),
      Valor_Mi = round(sum(Valor_RS, na.rm = TRUE) / 1e6, 2),
      .groups  = "drop"
    ) %>%
    group_by(Competencia) %>%
    mutate(
      Participacao_Pct = round((Valor_RS / sum(Valor_RS, na.rm = TRUE)) * 100, 2)
    ) %>%
    ungroup() %>%
    arrange(Competencia, desc(Valor_RS)) %>%
    as.data.frame()
  
  # Série anual por Atividade e por Seção CNAE 2.0 (incluindo 2023 resgatado de 2024)
  lista_anual_ativ <- list()
  if ("2024" %in% names(dados_sefaz_por_ano)) {
    a24_icms <- dados_sefaz_por_ano[["2024"]]$anual %>%
      filter(Secao == "ICMS", Rubrica != "Subtotal - ICMS")
    tot_23 <- sum(a24_icms$Acumulado_Ano_Ant_RS, na.rm = TRUE)
    lista_anual_ativ[["2023"]] <- a24_icms %>%
      transmute(
        Ano                  = 2023L,
        Rubrica              = Rubrica,
        Grande_Setor         = mapear_grande_setor_icms(Rubrica),
        Secao_CNAE           = mapear_cnae_icms(Rubrica),
        Acumulado_Ano_RS     = Acumulado_Ano_Ant_RS,
        Acumulado_Ano_Ant_RS = NA_real_,
        Acumulado_Ano_Mi     = round(Acumulado_Ano_Ant_RS / 1e6, 2),
        Var_Acum_YoY_Pct     = NA_real_,
        Participacao_Pct     = if (tot_23 > 0) round((Acumulado_Ano_Ant_RS / tot_23) * 100, 2) else 0
      )
  }
  
  for (ano_k in sort(as.integer(names(dados_sefaz_por_ano)))) {
    ak_icms <- dados_sefaz_por_ano[[as.character(ano_k)]]$anual %>%
      filter(Secao == "ICMS", Rubrica != "Subtotal - ICMS")
    tot_k <- sum(ak_icms$Acumulado_Ano_RS, na.rm = TRUE)
    lista_anual_ativ[[as.character(ano_k)]] <- ak_icms %>%
      transmute(
        Ano                  = as.integer(ano_k),
        Rubrica              = Rubrica,
        Grande_Setor         = mapear_grande_setor_icms(Rubrica),
        Secao_CNAE           = mapear_cnae_icms(Rubrica),
        Acumulado_Ano_RS     = Acumulado_Ano_RS,
        Acumulado_Ano_Ant_RS = Acumulado_Ano_Ant_RS,
        Acumulado_Ano_Mi     = round(Acumulado_Ano_RS / 1e6, 2),
        Var_Acum_YoY_Pct     = Var_Acum_YoY_Pct,
        Participacao_Pct     = if (tot_k > 0) round((Acumulado_Ano_RS / tot_k) * 100, 2) else 0
      )
  }
  
  df_anual_icms_atividades <- bind_rows(lista_anual_ativ) %>%
    arrange(Ano, desc(Acumulado_Ano_RS)) %>%
    as.data.frame()
  
  df_anual_icms_cnae <- df_anual_icms_atividades %>%
    group_by(Ano, Grande_Setor, Secao_CNAE) %>%
    summarise(
      Acumulado_Ano_RS     = sum(Acumulado_Ano_RS, na.rm = TRUE),
      Acumulado_Ano_Ant_RS = if (all(is.na(Acumulado_Ano_Ant_RS))) NA_real_ else sum(Acumulado_Ano_Ant_RS, na.rm = TRUE),
      Acumulado_Ano_Mi     = round(sum(Acumulado_Ano_RS, na.rm = TRUE) / 1e6, 2),
      .groups = "drop"
    ) %>%
    group_by(Ano) %>%
    mutate(
      Participacao_Pct = round((Acumulado_Ano_RS / sum(Acumulado_Ano_RS, na.rm = TRUE)) * 100, 2)
    ) %>%
    ungroup() %>%
    mutate(
      Var_Anual_Pct = ifelse(!is.na(Acumulado_Ano_Ant_RS) & Acumulado_Ano_Ant_RS > 0,
                             round(((Acumulado_Ano_RS / Acumulado_Ano_Ant_RS) - 1) * 100, 2),
                             NA_real_)
    ) %>%
    arrange(Ano, desc(Acumulado_Ano_RS)) %>%
    as.data.frame()
  
  # ----------------------------------------------------------------------------
  # 7. INDICADORES E TABELAS PRONTOS PARA O PAINEL ONLINE (index.qmd)
  # ----------------------------------------------------------------------------
  ult_linha_mes <- tail(df_mensal_tributos[df_mensal_tributos$ICMS_RS > 0, ], 1)
  ult_ano       <- ult_linha_mes$Ano[1]
  ult_mes       <- ult_linha_mes$Mes[1]
  ult_comp      <- ult_linha_mes$Competencia[1]
  
  mes_ano_curto   <- sprintf("%02d/%s", ult_mes, substr(as.character(ult_ano), 3, 4))
  mes_ano_extenso <- sprintf("%s/%d", meses_nomes_extenso[ult_mes], ult_ano)
  
  # Variação mensal do ICMS no mês mais recente (para a box na aba Visão Geral)
  obj_ult_ano <- dados_sefaz_por_ano[[as.character(ult_ano)]]
  var_icms_mes_oficial <- obj_ult_ano$anual %>%
    filter(Secao == "ICMS", Rubrica == "Subtotal - ICMS") %>%
    pull(Var_Ultimo_Mes_Pct)
  var_icms_mes_num <- if (length(var_icms_mes_oficial) > 0 && !is.na(var_icms_mes_oficial[1])) {
    var_icms_mes_oficial[1]
  } else {
    ult_linha_mes$Var_Mensal_ICMS_Pct[1]
  }
  str_vb_icms_var <- paste0(if (var_icms_mes_num >= 0) "+" else "", fmt_dec_arr(var_icms_mes_num, 2), "%")
  
  # Variação YoY e Acumulada do ICMS no ano vigente
  var_icms_yoy_mes <- obj_ult_ano$anual %>%
    filter(Secao == "ICMS", Rubrica == "Subtotal - ICMS") %>%
    pull(Var_YoY_Ult_Mes_Pct)
  var_icms_acum_ano <- obj_ult_ano$anual %>%
    filter(Secao == "ICMS", Rubrica == "Subtotal - ICMS") %>%
    pull(Var_Acum_YoY_Pct)
  
  # 4 ValueBoxes da aba Arrecadação e Finanças (para o mês mais recente)
  str_vb_receita_total <- fmt_moeda_mi_arr(ult_linha_mes$Receita_Total_Mi[1], 2)
  str_vb_icms          <- fmt_moeda_mi_arr(ult_linha_mes$ICMS_Mi[1], 2)
  str_vb_fpe           <- fmt_moeda_mi_arr(ult_linha_mes$FPE_Mi[1], 2)
  str_vb_ipva_outros   <- fmt_moeda_mi_arr(ult_linha_mes$Subtotal_Outros_Mi[1], 2)
  str_vb_ipva_puro     <- fmt_moeda_mi_arr(ult_linha_mes$IPVA_Total_Mi[1], 2)
  
  # Gráfico de Área Empilhada: Evolução Mensal das Receitas no Ano Vigente (em R$ Milhões)
  df_rec_ano_vig <- df_mensal_tributos %>%
    filter(Ano == ult_ano) %>%
    arrange(Mes)
  
  df_receitas_painel <- data.frame(
    Mes  = factor(df_rec_ano_vig$Mes_Nome, levels = df_rec_ano_vig$Mes_Nome),
    ICMS = round(df_rec_ano_vig$ICMS_Mi, 2),
    FPE  = round(df_rec_ano_vig$FPE_Mi, 2),
    IPVA = round(df_rec_ano_vig$Subtotal_Outros_Mi, 2), # Representa a soma de IPVA e Outros Tributos
    IPVA_Puro = round(df_rec_ano_vig$IPVA_Total_Mi, 2),
    Receita_Total = round(df_rec_ano_vig$Receita_Total_Mi, 2),
    stringsAsFactors = FALSE
  )
  
  # Gráfico Treemap: Composição do ICMS por Setor Econômico no Mês Vigente
  df_treemap_mes <- df_mensal_icms_cnae %>%
    filter(Competencia == ult_comp) %>%
    arrange(desc(Valor_RS)) %>%
    transmute(
      Setor            = Grande_Setor,
      Secao_CNAE       = Secao_CNAE,
      Pai              = "",
      Valor            = round(Valor_RS / 1e6, 2),
      Valor_RS         = Valor_RS,
      Participacao_Pct = Participacao_Pct
    ) %>%
    as.data.frame()
  
  # Série Trimestral de Variação do ICMS para o Gráfico 1 da aba Visão Geral
  df_icms_trimestral <- df_mensal_tributos %>%
    group_by(Ano, Trimestre_Num, Trimestre_Cod, Trimestre_Rotulo) %>%
    summarise(
      Meses_Trim = n(),
      ICMS_Trim_RS = sum(ICMS_RS, na.rm = TRUE),
      ICMS_Trim_Mi = round(sum(ICMS_RS, na.rm = TRUE) / 1e6, 2),
      Receita_Total_Trim_Mi = round(sum(Receita_Total_RS, na.rm = TRUE) / 1e6, 2),
      .groups = "drop"
    ) %>%
    filter(Meses_Trim == 3) %>%
    arrange(Trimestre_Cod) %>%
    mutate(
      Var_QoQ_Pct = round((ICMS_Trim_RS / lag(ICMS_Trim_RS) - 1) * 100, 2),
      Var_YoY_Pct = round((ICMS_Trim_RS / lag(ICMS_Trim_RS, 4) - 1) * 100, 2)
    ) %>%
    as.data.frame()
  
  # Detalhamento completo do mês vigente (todas as 34 linhas da SEFAZ-TO) para Boletins Flash
  df_detalhe_mes_vigente <- df_mensal_all %>%
    filter(Competencia == ult_comp) %>%
    left_join(
      obj_ult_ano$anual %>%
        select(Secao, Rubrica, Var_Ultimo_Mes_Pct, Valor_Mes_Ano_Ant_RS, Var_YoY_Ult_Mes_Pct,
               Acumulado_Ano_RS, Acumulado_Ano_Ant_RS, Var_Acum_YoY_Pct, Rep_Ativ_Acum_Pct),
      by = c("Secao", "Rubrica")
    ) %>%
    mutate(
      Valor_Mi            = round(Valor_RS / 1e6, 2),
      Acumulado_Ano_Mi    = round(Acumulado_Ano_RS / 1e6, 2),
      Acumulado_Ano_Ant_Mi = round(Acumulado_Ano_Ant_RS / 1e6, 2)
    ) %>%
    as.data.frame()
  
  resumo_arrecadacao <- list(
    ultima_extracao            = format(Sys.time(), "%d/%m/%Y %H:%M:%S"),
    dt_emissao_sefaz           = ifelse(!is.null(obj_ult_ano$dt_emissao) && !is.na(obj_ult_ano$dt_emissao),
                                        obj_ult_ano$dt_emissao, format(Sys.Date(), "%d/%m/%Y")),
    ano_vigente                = ult_ano,
    mes_vigente                = ult_mes,
    competencia_vigente        = ult_comp,
    mes_ano_curto              = mes_ano_curto,
    mes_ano_extenso            = mes_ano_extenso,
    vb_icms_var                = str_vb_icms_var,
    var_icms_mes_pct           = var_icms_mes_num,
    var_icms_yoy_mes_pct       = if (length(var_icms_yoy_mes) > 0) var_icms_yoy_mes[1] else NA_real_,
    var_icms_acum_ano_pct      = if (length(var_icms_acum_ano) > 0) var_icms_acum_ano[1] else NA_real_,
    vb_receita_total           = str_vb_receita_total,
    vb_icms                    = str_vb_icms,
    vb_fpe                     = str_vb_fpe,
    vb_ipva                    = str_vb_ipva_outros,
    vb_ipva_puro               = str_vb_ipva_puro,
    resumo_mes_vigente         = ult_linha_mes,
    df_receitas_painel         = df_receitas_painel,
    df_setores_icms_painel     = df_treemap_mes,
    df_icms_trimestral         = df_icms_trimestral,
    mensal_tributos            = df_mensal_tributos,
    anual_tributos             = df_anual_tributos,
    mensal_icms_atividades     = df_mensal_icms_atividades,
    anual_icms_atividades      = df_anual_icms_atividades,
    mensal_icms_cnae           = df_mensal_icms_cnae,
    anual_icms_cnae            = df_anual_icms_cnae,
    detalhe_sefaz_mes_vigente  = df_detalhe_mes_vigente,
    confaz_historico_to        = df_confaz_to,
    prop_comercio_atacadista   = round(prop_atacadista_confaz * 100, 2),
    prop_comercio_varejista    = round((1 - prop_atacadista_confaz) * 100, 2)
  )
  
  saveRDS(resumo_arrecadacao, file = caminho_resumo_rds, compress = "xz")
  
  cat("--------------------------------------------------------\n")
  cat(sprintf("Resumo Arrecadacao e Financas (TO) - Competencia %s:\n", mes_ano_extenso))
  cat(sprintf("  - Receita Total do Mes (Propria + FPE): %s (R$ %s)\n",
              str_vb_receita_total, fmt_int_arr(ult_linha_mes$Receita_Total_RS[1])))
  cat(sprintf("  - Arrecadacao ICMS no Mes:              %s (Var. Mensal: %s | YoY: %s%% | Acum. %d: %s%%)\n",
              str_vb_icms, str_vb_icms_var,
              fmt_dec_arr(resumo_arrecadacao$var_icms_yoy_mes_pct, 2),
              ult_ano, fmt_dec_arr(resumo_arrecadacao$var_icms_acum_ano_pct, 2)))
  cat(sprintf("  - Repasses Federais (FPE) no Mes:       %s (R$ %s)\n",
              str_vb_fpe, fmt_int_arr(ult_linha_mes$FPE_RS[1])))
  cat(sprintf("  - IPVA e Outros Tributos no Mes:        %s (IPVA puro: %s | IRRF: R$ %s Mi | ITCD: R$ %s Mi | Taxas: R$ %s Mi)\n",
              str_vb_ipva_outros, str_vb_ipva_puro,
              fmt_dec_arr(ult_linha_mes$IRRF_Total_Mi[1], 2),
              fmt_dec_arr(ult_linha_mes$ITCD_Mi[1], 2),
              fmt_dec_arr(ult_linha_mes$Taxas_Mi[1], 2)))
  cat(sprintf("  - Base historica salva:                 %d meses (2024-%d) e %d anos (2023-%d) em '%s'\n",
              nrow(df_mensal_tributos), ult_ano, nrow(df_anual_tributos), ult_ano, caminho_resumo_rds))
  cat("========================================================\n")
  
  invisible(resumo_arrecadacao)
}

# Executa automaticamente se chamado diretamente ou via source()
resumo_arrecadacao_to <- tryCatch({
  executar_extracao_arrecadacao_to()
}, error = function(e) {
  message(sprintf("[AVISO] Erro ao executar extrair_arrecadacao_to.R: %s", e$message))
  if (file.exists("dados/arrecadacao_to_resumo.rds")) readRDS("dados/arrecadacao_to_resumo.rds") else NULL
})
