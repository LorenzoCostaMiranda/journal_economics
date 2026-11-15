# ==============================================================================
# SCRIPT: extrair_cnpj_to.R
# OBJETIVO: Extracao 100% automatizada, enxuta e sem desperdicio de disco dos
#           dados de CNPJ, Estabelecimentos (Matrizes e Filiais), Porte (MEI,
#           ME, EPP, Demais), Capital Social, CNAE 2.0 (Secao e Divisao),
#           Simples Nacional (SINAC/SIMEI), Quadro de Socios e Administradores
#           (QSA) e Tempos do Mapa de Empresas (Abertura, Viabilidade e Registro)
#           para o Estado do Tocantins e seus 139 municipios.
#
# ARQUITETURA DE ARMAZENAMENTO INTELIGente (SSD D:\Journal):
#   1. Qualquer arquivo temporario bruto (.zip/.csv) porventura baixado em
#      D:\Journal\dados\tmp_microdados e processado em memoria e EXCLUIDO
#      imediatamente via on.exit(), mantendo consumo zero de arquivos brutos.
#   2. A coleta principal conecta-se diretamente aos motores oficiais em memoria:
#      - SERPRO / Receita Federal / Mapa de Empresas (Qlik Sense Engine JSON-RPC
#        API em wss://dd.serpro.gov.br/publico/app/<id>):
#          * App 1 (7979697b-ad3d-4b28-a5bf-9cd48ea9eae7): CNPJ + Estabelecimentos
#            + Simples/MEI + Natureza Juridica + Aberturas/Baixas/Estoque (UF/Mun)
#          * App 2 (2a7344b2-670f-46d4-8632-7d262923aee0): Capital Social + CNAE
#            Fiscal (Secao e Divisao) + Socios (QSA) e Responsaveis
#          * App 3 (314cd578-2c59-45c4-894f-186422f7f5cd): Tempo Medio de
#            Abertura, Viabilidade e Registro (REDESIM / Mapa de Empresas)
#      - Receita Federal — Estatisticas SINAC / SIMEI
#        (https://www8.receita.fazenda.gov.br/SimplesNacional/Aplicacoes/ATBHE/estatisticasSinac.app/):
#          * Optantes pelo Simples Nacional e SIMEI por UF e nos 139 municipios
#            do Tocantins.
#   3. Salva apenas objetos .rds ultra-compactados (compress = "xz") em:
#      - D:\Journal\dados\cnpj_to_base.rds
#      - D:\Journal\dados\cnpj_to_resumo.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(websocket)
  library(later)
  library(rvest)
  library(xml2)
})

extrair_cnpj_tocantins <- function(diretorio_raiz = "D:/Journal", forcar_atualizacao = TRUE) {
  cat("======================================================================\n")
  cat(" INICIANDO EXTRACAO AUTOMATICA: CNPJ / SIMPLES / SOCIOS / MAPA EMPRESAS\n")
  cat("======================================================================\n")

  dir_dados <- file.path(diretorio_raiz, "dados")
  dir_tmp   <- file.path(dir_dados, "tmp_microdados")
  if (!dir.exists(dir_dados)) dir.create(dir_dados, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir_tmp))   dir.create(dir_tmp,   recursive = TRUE, showWarnings = FALSE)

  arq_base   <- file.path(dir_dados, "cnpj_to_base.rds")
  arq_resumo <- file.path(dir_dados, "cnpj_to_resumo.rds")

  # Garantia estrita de limpeza de quaisquer microdados temporarios em D:\Journal\dados\tmp_microdados
  limpar_temporarios_cnpj <- function() {
    arqs_tmp <- list.files(
      dir_tmp,
      pattern = "(Empresas|Estabelecimentos|Socios|Simples|cnpj).*\\.(zip|csv|txt|ESTABELE|EMPRECSV|SOCIOCSV|SIMPLES)$",
      ignore.case = TRUE,
      full.names = TRUE
    )
    if (length(arqs_tmp) > 0) {
      cat(" -> [Limpeza de Disco] Removendo", length(arqs_tmp), "arquivo(s) bruto(s) temporario(s) de CNPJ...\n")
      try(unlink(arqs_tmp, force = TRUE), silent = TRUE)
    }
  }
  on.exit(limpar_temporarios_cnpj(), add = TRUE)

  # ----------------------------------------------------------------------------
  # Helpers de conexao ao motor Qlik Sense (SERPRO / Receita Federal)
  # ----------------------------------------------------------------------------
  qlik_connect <- function(app_id, sheet_id, timeout_conn = 20) {
    full_url <- sprintf("https://dd.serpro.gov.br/publico/sense/app/%s/sheet/%s/state/analysis", app_id, sheet_id)
    r1 <- GET(full_url, user_agent("Mozilla/5.0"), timeout(timeout_conn))
    r2 <- GET(
      "https://dd.serpro.gov.br/publico/qps/csrftoken?xrfkey=0123456789abcdef",
      add_headers(`X-Qlik-Xrfkey` = "0123456789abcdef"),
      user_agent("Mozilla/5.0"),
      timeout(timeout_conn)
    )
    csrf_tok <- headers(r2)[["qlik-csrf-token"]]
    ck_df <- cookies(r2)
    ck_str <- paste(sprintf("%s=%s", ck_df$name, ck_df$value), collapse = "; ")
    ws_url <- sprintf("wss://dd.serpro.gov.br/publico/app/%s?qlik-csrf-token=%s", app_id, csrf_tok)

    env <- new.env(parent = emptyenv())
    env$connected <- FALSE
    env$closed <- FALSE
    env$responses <- list()
    env$req_id <- 0L

    ws <- WebSocket$new(
      ws_url,
      headers = list(Cookie = ck_str, Origin = "https://dd.serpro.gov.br", `User-Agent` = "Mozilla/5.0"),
      autoConnect = FALSE
    )

    ws$onOpen(function(event) {
      env$connected <- TRUE
    })
    ws$onMessage(function(event) {
      res <- fromJSON(event$data, simplifyVector = FALSE)
      if (!is.null(res$id)) {
        env$responses[[as.character(res$id)]] <- res
      }
    })
    ws$onError(function(event) {
      env$closed <- TRUE
    })
    ws$onClose(function(event) {
      env$closed <- TRUE
    })

    ws$connect()
    t0 <- Sys.time()
    while (!env$connected && !env$closed && as.numeric(difftime(Sys.time(), t0, units = "secs")) < timeout_conn) {
      later::run_now(0.05)
    }
    if (!env$connected) stop("Nao foi possivel abrir WebSocket com dd.serpro.gov.br para app ", app_id)

    rpc <- function(handle, method, params = list(), timeout_s = 30) {
      env$req_id <- env$req_id + 1L
      id_str <- as.character(env$req_id)
      msg <- toJSON(list(
        jsonrpc = "2.0",
        id = env$req_id,
        handle = as.integer(handle),
        method = method,
        params = params
      ), auto_unbox = TRUE)
      ws$send(msg)
      t_start <- Sys.time()
      while (is.null(env$responses[[id_str]]) && !env$closed &&
             as.numeric(difftime(Sys.time(), t_start, units = "secs")) < timeout_s) {
        later::run_now(0.05)
      }
      env$responses[[id_str]]
    }

    res_open <- rpc(-1L, "OpenDoc", list(app_id))
    doc_h <- res_open$result$qReturn$qHandle
    list(ws = ws, doc_h = doc_h, rpc = rpc)
  }

  qlik_hypercube <- function(conn, dims, exprs, max_rows = 500) {
    qDims <- lapply(dims, function(d) {
      list(
        qDef = list(
          qFieldDefs = list(d),
          qSortCriterias = list(list(qSortByNumeric = 1L, qSortByAscii = 1L))
        ),
        qNullSuppression = TRUE
      )
    })
    qMeas <- lapply(names(exprs), function(nm) {
      list(qDef = list(qDef = exprs[[nm]], qLabel = nm))
    })
    res_obj <- conn$rpc(conn$doc_h, "CreateSessionObject", list(list(
      qInfo = list(qType = "myhc"),
      qHyperCubeDef = list(
        qDimensions = qDims,
        qMeasures = qMeas,
        qInitialDataFetch = list(list(
          qTop = 0L,
          qLeft = 0L,
          qHeight = as.integer(max_rows),
          qWidth = as.integer(length(dims) + length(exprs))
        ))
      )
    )))
    obj_h <- res_obj$result$qReturn$qHandle
    res_lay <- conn$rpc(obj_h, "GetLayout", list())
    mat <- res_lay$result$qLayout$qHyperCube$qDataPages[[1]]$qMatrix
    if (length(mat) == 0) return(data.frame())
    rows <- lapply(mat, function(r) {
      vals <- list()
      for (i in seq_along(dims)) {
        vals[[dims[i]]] <- if (!is.null(r[[i]]$qText)) r[[i]]$qText else ""
      }
      for (j in seq_along(exprs)) {
        idx <- length(dims) + j
        nm <- names(exprs)[j]
        num_val <- suppressWarnings(as.numeric(r[[idx]]$qNum))
        vals[[nm]] <- if (!is.null(r[[idx]]$qNum) && !is.na(num_val) && !is.nan(num_val)) {
          num_val
        } else {
          0
        }
      }
      as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    })
    do.call(rbind, rows)
  }

  qlik_get_var <- function(conn, var_name) {
    rv <- conn$rpc(conn$doc_h, "GetVariableByName", list(var_name))
    vh <- rv$result$qReturn$qHandle
    if (is.null(vh)) return(NA_character_)
    rl <- conn$rpc(vh, "GetLayout", list())
    as.character(rl$result$qLayout$qText)
  }

  # Mapa auxiliar de meses em portugues
  meses_pt <- c(
    "Janeiro" = 1L, "Fevereiro" = 2L, "Março" = 3L, "Abril" = 4L,
    "Maio" = 5L, "Junho" = 6L, "Julho" = 7L, "Agosto" = 8L,
    "Setembro" = 9L, "Outubro" = 10L, "Novembro" = 11L, "Dezembro" = 12L
  )
  meses_abrev <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

  formatar_inteiro_br <- function(x) {
    format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  }

  # ----------------------------------------------------------------------------
  # ETAPA 1: App 1 — Registro de Empresas (CNPJ + Estabelecimentos + MEI + Mun)
  # ----------------------------------------------------------------------------
  cat(" -> [1/4] Conectando ao Motor Receita Federal / SERPRO (App 1: CNPJ & Estabelecimentos)...\n")
  resumo_extraido <- tryCatch({
    c1 <- qlik_connect("7979697b-ad3d-4b28-a5bf-9cd48ea9eae7", "09f13ad4-bddd-42c7-a88b-ac7201783544")
    on.exit(try(c1$ws$close(), silent = TRUE), add = TRUE)

    ano_var_str <- qlik_get_var(c1, "vAnoReferencia")
    ano_max_busca <- max(
      suppressWarnings(as.integer(ano_var_str)),
      as.integer(format(Sys.Date(), "%Y")) + 1L,
      2026L,
      na.rm = TRUE
    )

    # Limpa selecoes default da planilha e seleciona UF = TO
    c1$rpc(c1$doc_h, "ClearAll", list(FALSE))
    f_uf1 <- c1$rpc(c1$doc_h, "GetField", list("UF"))$result$qReturn$qHandle
    c1$rpc(f_uf1, "SelectValues", list(list(list(qText = "TO")), FALSE, FALSE))

    # 1.1 Serie Historica Mensal de Aberturas e Baixas (2020 ate ano_max_busca)
    # Executada primeiro para detectar automaticamente o ultimo mes/ano disponivel na base!
    anos_hist <- as.character(seq(2020L, ano_max_busca))
    anos_set_str <- paste0("'", anos_hist, "'", collapse = ",")

    df_abertas_m <- qlik_hypercube(
      c1,
      dims = c("Ano de Abertura", "Mês de Abertura"),
      exprs = list(
        Aberturas = sprintf("Count({$<[Ano de Abertura]={%s}, tip_situacao={'1','2','9'}>} [Num. Empresa])", anos_set_str),
        Aberturas_MEI = sprintf("Count({$<[Ano de Abertura]={%s}, [Opção MEI]={'MEI'}, tip_situacao={'1','2','9'}>} [Num. Empresa])", anos_set_str),
        Aberturas_Matriz = sprintf("Count({$<[Ano de Abertura]={%s}, [Tipo Estabelecimento]={'Matriz'}, tip_situacao={'1','2','9'}>} [Num. Empresa])", anos_set_str)
      ),
      max_rows = 300
    )

    df_baixas_m <- qlik_hypercube(
      c1,
      dims = c("Ano de Baixa", "Mês de Baixa"),
      exprs = list(
        Baixas = sprintf("Count({$<[Ano de Baixa]={%s}, tip_situacao={2}>} [Num. Empresa])", anos_set_str),
        Baixas_MEI = sprintf("Count({$<[Ano de Baixa]={%s}, [Opção MEI]={'MEI'}, tip_situacao={2}>} [Num. Empresa])", anos_set_str)
      ),
      max_rows = 300
    )

    names(df_abertas_m)[1:2] <- c("Ano", "Mes_Nome")
    names(df_baixas_m)[1:2]  <- c("Ano", "Mes_Nome")

    df_mensal <- merge(df_abertas_m, df_baixas_m, by = c("Ano", "Mes_Nome"), all = TRUE)
    df_mensal$Ano_Num <- suppressWarnings(as.integer(df_mensal$Ano))
    df_mensal$Mes_Num <- unname(meses_pt[df_mensal$Mes_Nome])
    df_mensal <- df_mensal[!is.na(df_mensal$Ano_Num) & !is.na(df_mensal$Mes_Num), ]
    for (col_num in c("Aberturas", "Aberturas_MEI", "Aberturas_Matriz", "Baixas", "Baixas_MEI")) {
      df_mensal[[col_num]][is.na(df_mensal[[col_num]])] <- 0
    }
    df_mensal <- df_mensal[df_mensal$Aberturas > 0 | df_mensal$Baixas > 0, ]
    df_mensal <- df_mensal[order(df_mensal$Ano_Num, df_mensal$Mes_Num), ]
    rownames(df_mensal) <- NULL

    # Detecta automaticamente a competencia mais recente com movimentacao na Receita Federal
    ult_disp <- df_mensal[nrow(df_mensal), ]
    ano_ref_num <- as.integer(ult_disp$Ano_Num)
    mes_ref_num <- as.integer(ult_disp$Mes_Num)
    mes_ref_str <- as.character(ult_disp$Mes_Nome)
    ano_ref_str <- as.character(ano_ref_num)

    cat(sprintf("    Competencia oficial detectada na Receita Federal: %s/%s (%02d/%d)\n",
                mes_ref_str, ano_ref_str, mes_ref_num, ano_ref_num))

    # 1.2 Estoque Ativo por Porte, Opcao MEI e Tipo de Estabelecimento (Matriz vs Filial)
    df_porte_det <- qlik_hypercube(
      c1,
      dims = c("Porte", "Opção MEI", "Tipo Estabelecimento"),
      exprs = list(
        Ativas = "Count({$<tip_situacao={1}>} [Num. Empresa])",
        Abertas_Ano = sprintf("Count({$<[Ano de Abertura]={'%d'}, tip_situacao={'1','2','9'}>} [Num. Empresa])", ano_ref_num),
        Baixas_Ano  = sprintf("Count({$<[Ano de Baixa]={'%d'}, tip_situacao={2}>} [Num. Empresa])", ano_ref_num)
      )
    )

    total_ativas_to <- sum(df_porte_det$Ativas, na.rm = TRUE)
    total_mei_to    <- sum(df_porte_det$Ativas[df_porte_det$`Opção MEI` == "MEI"], na.rm = TRUE)
    total_me_exc_to <- sum(df_porte_det$Ativas[df_porte_det$Porte == "Microempresa" & df_porte_det$`Opção MEI` != "MEI"], na.rm = TRUE)
    total_me_all_to <- total_mei_to + total_me_exc_to
    total_epp_to    <- sum(df_porte_det$Ativas[grepl("Pequeno", df_porte_det$Porte, ignore.case = TRUE)], na.rm = TRUE)
    total_demais_to <- sum(df_porte_det$Ativas[df_porte_det$Porte == "Outras" | (!grepl("Microempresa|Pequeno", df_porte_det$Porte, ignore.case = TRUE) & df_porte_det$`Opção MEI` != "MEI")], na.rm = TRUE)

    total_matrizes_to <- sum(df_porte_det$Ativas[df_porte_det$`Tipo Estabelecimento` == "Matriz"], na.rm = TRUE)
    total_filiais_to  <- sum(df_porte_det$Ativas[df_porte_det$`Tipo Estabelecimento` == "Filial"], na.rm = TRUE)

    # Tabela consolidada de Matriz vs Filial
    df_matriz_filial <- aggregate(
      cbind(Ativas, Abertas_Ano, Baixas_Ano) ~ `Tipo Estabelecimento`,
      data = df_porte_det,
      FUN = sum,
      na.rm = TRUE
    )
    names(df_matriz_filial)[1] <- "Tipo_Estabelecimento"
    df_matriz_filial$Saldo_Ano <- df_matriz_filial$Abertas_Ano - df_matriz_filial$Baixas_Ano
    df_matriz_filial$Participacao_Pct <- round(df_matriz_filial$Ativas / total_ativas_to * 100, 2)

    # Tabela de Porte para o Grafico de Rosca do Painel Online e Boletins
    df_porte_painel <- data.frame(
      Porte = c("MEI", "Microempresa (ME)", "Pequeno Porte (EPP)", "Não Informado / Demais"),
      Total = c(total_mei_to, total_me_exc_to, total_epp_to, total_demais_to),
      Participacao_Pct = round(c(total_mei_to, total_me_exc_to, total_epp_to, total_demais_to) / total_ativas_to * 100, 2),
      stringsAsFactors = FALSE
    )

    df_mensal$Saldo <- df_mensal$Aberturas - df_mensal$Baixas
    df_mensal$Saldo_MEI <- df_mensal$Aberturas_MEI - df_mensal$Baixas_MEI
    df_mensal$Competencia <- sprintf("%04d-%02d", df_mensal$Ano_Num, df_mensal$Mes_Num)
    df_mensal$Periodo_Rotulo <- sprintf("%s/%s", meses_abrev[df_mensal$Mes_Num], substr(df_mensal$Ano, 3, 4))

    # Reconstroi a serie exata do estoque de CNPJs ativos ao final de cada mes:
    # Estoque[t] = Estoque[T] - soma(Saldo[t+1 ... T])
    n_m <- nrow(df_mensal)
    estoque_vec <- numeric(n_m)
    estoque_vec[n_m] <- total_ativas_to
    if (n_m >= 2) {
      for (k in seq(n_m - 1L, 1L, by = -1L)) {
        estoque_vec[k] <- estoque_vec[k + 1L] - df_mensal$Saldo[k + 1L]
      }
    }
    df_mensal$Estoque_Ativo <- estoque_vec

    # Indicadores do mes mais recente e dos ultimos 12 meses
    ult_linha <- df_mensal[n_m, ]
    novos_cnpjs_mes <- ult_linha$Aberturas
    baixas_cnpjs_mes <- ult_linha$Baixas
    saldo_cnpjs_mes <- ult_linha$Saldo

    idx_12m <- seq(max(1L, n_m - 11L), n_m)
    df_ult_12m <- df_mensal[idx_12m, ]
    aberturas_12m <- sum(df_ult_12m$Aberturas, na.rm = TRUE)
    baixas_12m    <- sum(df_ult_12m$Baixas, na.rm = TRUE)
    saldo_12m     <- aberturas_12m - baixas_12m

    # Serie Anual Consolidada (2020-2026) para Dossies Anuais
    df_anual <- aggregate(
      cbind(Aberturas, Aberturas_MEI, Aberturas_Matriz, Baixas, Baixas_MEI, Saldo, Saldo_MEI) ~ Ano_Num,
      data = df_mensal,
      FUN = sum,
      na.rm = TRUE
    )
    names(df_anual)[1] <- "Ano"
    # Adiciona estoque ativo ao final de cada ano
    est_fim_ano <- sapply(df_anual$Ano, function(a) {
      sub_a <- df_mensal[df_mensal$Ano_Num == a, ]
      sub_a$Estoque_Ativo[nrow(sub_a)]
    })
    df_anual$Estoque_Ativo_Final <- est_fim_ano

    # 1.3 Natureza Juridica (Estoque Ativo, Aberturas e Baixas no Ano)
    df_natjur <- qlik_hypercube(
      c1,
      dims = c("Natureza Jurídica"),
      exprs = list(
        Ativas = "Count({$<tip_situacao={1}>} [Num. Empresa])",
        Abertas_Ano = sprintf("Count({$<[Ano de Abertura]={'%d'}, tip_situacao={'1','2','9'}>} [Num. Empresa])", ano_ref_num),
        Baixas_Ano  = sprintf("Count({$<[Ano de Baixa]={'%d'}, tip_situacao={2}>} [Num. Empresa])", ano_ref_num)
      ),
      max_rows = 100
    )
    names(df_natjur)[1] <- "Natureza_Juridica"
    df_natjur$Saldo_Ano <- df_natjur$Abertas_Ano - df_natjur$Baixas_Ano
    df_natjur$Participacao_Pct <- round(df_natjur$Ativas / total_ativas_to * 100, 2)
    df_natjur <- df_natjur[order(-df_natjur$Ativas), ]
    rownames(df_natjur) <- NULL

    # 1.4 Desagregacao Municipal Completa (139 Municipios do Tocantins)
    # Monta expressao para os ultimos 12 meses dinamicamente
    meses_ano_atual <- df_ult_12m$Mes_Nome[df_ult_12m$Ano_Num == ano_ref_num]
    meses_ano_ant   <- df_ult_12m$Mes_Nome[df_ult_12m$Ano_Num == (ano_ref_num - 1L)]

    expr_ab_12m <- sprintf(
      "Count({$<[Ano de Abertura]={'%d'}, [Mês de Abertura]={%s}, tip_situacao={'1','2','9'}>} [Num. Empresa])",
      ano_ref_num, paste0("'", meses_ano_atual, "'", collapse = ",")
    )
    expr_bx_12m <- sprintf(
      "Count({$<[Ano de Baixa]={'%d'}, [Mês de Baixa]={%s}, tip_situacao={2}>} [Num. Empresa])",
      ano_ref_num, paste0("'", meses_ano_atual, "'", collapse = ",")
    )
    if (length(meses_ano_ant) > 0) {
      expr_ab_12m <- paste0(
        expr_ab_12m, " + ",
        sprintf(
          "Count({$<[Ano de Abertura]={'%d'}, [Mês de Abertura]={%s}, tip_situacao={'1','2','9'}>} [Num. Empresa])",
          ano_ref_num - 1L, paste0("'", meses_ano_ant, "'", collapse = ",")
        )
      )
      expr_bx_12m <- paste0(
        expr_bx_12m, " + ",
        sprintf(
          "Count({$<[Ano de Baixa]={'%d'}, [Mês de Baixa]={%s}, tip_situacao={2}>} [Num. Empresa])",
          ano_ref_num - 1L, paste0("'", meses_ano_ant, "'", collapse = ",")
        )
      )
    }

    df_mun <- qlik_hypercube(
      c1,
      dims = c("cod_municipio_ibge_cadastro", "Município"),
      exprs = list(
        Ativas      = "Count({$<tip_situacao={1}>} [Num. Empresa])",
        MEI         = "Count({$<tip_situacao={1}, [Opção MEI]={'MEI'}>} [Num. Empresa])",
        ME          = "Count({$<tip_situacao={1}, [Porte]={'Microempresa'}, [Opção MEI]={'Não-MEI'}>} [Num. Empresa])",
        Demais      = "Count({$<tip_situacao={1}, [Porte]={'Outras'}>} [Num. Empresa])",
        Matrizes    = "Count({$<tip_situacao={1}, [Tipo Estabelecimento]={'Matriz'}>} [Num. Empresa])",
        Filiais     = "Count({$<tip_situacao={1}, [Tipo Estabelecimento]={'Filial'}>} [Num. Empresa])",
        Abertas_Mes = sprintf("Count({$<[Ano de Abertura]={'%d'}, [Mês de Abertura]={'%s'}, tip_situacao={'1','2','9'}>} [Num. Empresa])", ano_ref_num, mes_ref_str),
        Baixas_Mes  = sprintf("Count({$<[Ano de Baixa]={'%d'}, [Mês de Baixa]={'%s'}, tip_situacao={2}>} [Num. Empresa])", ano_ref_num, mes_ref_str),
        Abertas_Ano = sprintf("Count({$<[Ano de Abertura]={'%d'}, tip_situacao={'1','2','9'}>} [Num. Empresa])", ano_ref_num),
        Baixas_Ano  = sprintf("Count({$<[Ano de Baixa]={'%d'}, tip_situacao={2}>} [Num. Empresa])", ano_ref_num),
        Abertas_12m = expr_ab_12m,
        Baixas_12m  = expr_bx_12m
      ),
      max_rows = 200
    )
    names(df_mun)[1:2] <- c("cod_municipio_ibge", "Municipio_UF")
    df_mun$Municipio <- sub(" - TO$", "", df_mun$Municipio_UF)
    df_mun$EPP <- pmax(0, df_mun$Ativas - df_mun$MEI - df_mun$ME - df_mun$Demais)
    df_mun$Saldo_Mes <- df_mun$Abertas_Mes - df_mun$Baixas_Mes
    df_mun$Saldo_Ano <- df_mun$Abertas_Ano - df_mun$Baixas_Ano
    df_mun$Saldo_12m <- df_mun$Abertas_12m - df_mun$Baixas_12m
    df_mun$Pct_MEI   <- round(ifelse(df_mun$Ativas > 0, df_mun$MEI / df_mun$Ativas * 100, 0), 2)
    df_mun$Pct_ME    <- round(ifelse(df_mun$Ativas > 0, df_mun$ME  / df_mun$Ativas * 100, 0), 2)
    df_mun$Participacao_Estado_Pct <- round(df_mun$Ativas / total_ativas_to * 100, 2)
    df_mun <- df_mun[order(-df_mun$Ativas), ]
    rownames(df_mun) <- NULL

    try(c1$ws$close(), silent = TRUE)

    # --------------------------------------------------------------------------
    # ETAPA 2: App 2 — Capital Social, CNAE 2.0 (Secao e Divisao) e Socios (QSA)
    # --------------------------------------------------------------------------
    cat(" -> [2/4] Conectando ao Motor Receita Federal / SERPRO (App 2: Capital Social, CNAE 2.0 e Socios QSA)...\n")
    c2 <- qlik_connect("2a7344b2-670f-46d4-8632-7d262923aee0", "uJkjqm")
    on.exit(try(c2$ws$close(), silent = TRUE), add = TRUE)

    c2$rpc(c2$doc_h, "ClearAll", list(FALSE))
    f_uf2 <- c2$rpc(c2$doc_h, "GetField", list("UF"))$result$qReturn$qHandle
    c2$rpc(f_uf2, "SelectValues", list(list(list(qText = "TO")), FALSE, FALSE))

    # 2.1 Empresas Ativas por Faixa de Capital Social (e cruzamento por Porte)
    df_cap <- qlik_hypercube(
      c2,
      dims = c("Faixa de valor do Capital Social"),
      exprs = list(
        Empresas_Ativas = "Count({$<tip_situacao={1}>} DISTINCT [Num. Empresa])",
        MEI_Ativas      = "Count({$<tip_situacao={1}, [Opção MEI]={'MEI'}>} DISTINCT [Num. Empresa])",
        Matrizes_Ativas = "Count({$<tip_situacao={1}, [Tipo Estabelecimento]={'Matriz'}>} DISTINCT [Num. Empresa])",
        Filiais_Ativas  = "Count({$<tip_situacao={1}, [Tipo Estabelecimento]={'Filial'}>} DISTINCT [Num. Empresa])"
      )
    )
    names(df_cap)[1] <- "Faixa_Capital_Bruta"
    # Na base SERPRO/RFB, o campo vlr_capital_social e armazenado com 2 digitos decimais implicitos (centavos):
    # 80000 = R$ 800,00 | 360000 = R$ 3.600,00 | 1000000 = R$ 10.000,00 | 10000000 = R$ 100.000,00
    rotulos_cap <- c(
      "1-80000"          = "Até R$ 800,00",
      "80001-360000"     = "De R$ 800,01 a R$ 3.600,00",
      "360001-500000"    = "De R$ 3.600,01 a R$ 5.000,00",
      "500001-1000000"   = "De R$ 5.000,01 a R$ 10.000,00",
      "1000001-10000000" = "De R$ 10.000,01 a R$ 100.000,00",
      "10000000+"        = "Acima de R$ 100.000,00"
    )
    df_cap$Faixa_Capital_Social <- ifelse(
      df_cap$Faixa_Capital_Bruta %in% names(rotulos_cap),
      unname(rotulos_cap[df_cap$Faixa_Capital_Bruta]),
      df_cap$Faixa_Capital_Bruta
    )
    df_cap$Participacao_Pct <- round(df_cap$Empresas_Ativas / sum(df_cap$Empresas_Ativas, na.rm = TRUE) * 100, 2)

    df_cap_porte <- qlik_hypercube(
      c2,
      dims = c("Faixa de valor do Capital Social", "Porte"),
      exprs = list(
        Empresas_Ativas = "Count({$<tip_situacao={1}>} DISTINCT [Num. Empresa])"
      )
    )
    names(df_cap_porte)[1] <- "Faixa_Capital_Bruta"
    df_cap_porte$Faixa_Capital_Social <- ifelse(
      df_cap_porte$Faixa_Capital_Bruta %in% names(rotulos_cap),
      unname(rotulos_cap[df_cap_porte$Faixa_Capital_Bruta]),
      df_cap_porte$Faixa_Capital_Bruta
    )

    # 2.2 Empresas Ativas por Secao CNAE 2.0 e Grande Setor Economico
    df_cnae_sec <- qlik_hypercube(
      c2,
      dims = c("cod_secao", "des_secao"),
      exprs = list(
        Empresas_Ativas = "Count({$<tip_situacao={1}>} DISTINCT [Num. Empresa])",
        MEI_Ativas      = "Count({$<tip_situacao={1}, [Opção MEI]={'MEI'}>} DISTINCT [Num. Empresa])",
        Abertas_Ano     = sprintf("Count({$<[Ano de Abertura]={'%d'}, tip_situacao={'1','2','9'}>} DISTINCT [Num. Empresa])", ano_ref_num),
        Baixas_Ano      = sprintf("Count({$<[Ano de Baixa]={'%d'}, tip_situacao={2}>} DISTINCT [Num. Empresa])", ano_ref_num)
      ),
      max_rows = 50
    )
    df_cnae_sec$Saldo_Ano <- df_cnae_sec$Abertas_Ano - df_cnae_sec$Baixas_Ano
    df_cnae_sec$Participacao_Pct <- round(df_cnae_sec$Empresas_Ativas / sum(df_cnae_sec$Empresas_Ativas, na.rm = TRUE) * 100, 2)

    mapear_grande_setor <- function(sec) {
      ifelse(sec == "A", "Agropecuária",
      ifelse(sec %in% c("B", "C", "D", "E"), "Indústria",
      ifelse(sec == "F", "Construção",
      ifelse(sec == "G", "Comércio", "Serviços"))))
    }
    df_cnae_sec$Grande_Setor <- mapear_grande_setor(df_cnae_sec$cod_secao)
    df_cnae_sec <- df_cnae_sec[order(-df_cnae_sec$Empresas_Ativas), ]
    rownames(df_cnae_sec) <- NULL

    df_grande_setor <- aggregate(
      cbind(Empresas_Ativas, MEI_Ativas, Abertas_Ano, Baixas_Ano, Saldo_Ano) ~ Grande_Setor,
      data = df_cnae_sec,
      FUN = sum,
      na.rm = TRUE
    )
    df_grande_setor$Participacao_Pct <- round(df_grande_setor$Empresas_Ativas / sum(df_grande_setor$Empresas_Ativas, na.rm = TRUE) * 100, 2)
    df_grande_setor <- df_grande_setor[order(-df_grande_setor$Empresas_Ativas), ]
    rownames(df_grande_setor) <- NULL

    # 2.3 Empresas Ativas por Divisao CNAE 2.0 (87 divisoes)
    df_cnae_div <- qlik_hypercube(
      c2,
      dims = c("cod_secao", "cod_divisao_formatado", "des_divisao"),
      exprs = list(
        Empresas_Ativas = "Count({$<tip_situacao={1}>} DISTINCT [Num. Empresa])",
        MEI_Ativas      = "Count({$<tip_situacao={1}, [Opção MEI]={'MEI'}>} DISTINCT [Num. Empresa])",
        Abertas_Ano     = sprintf("Count({$<[Ano de Abertura]={'%d'}, tip_situacao={'1','2','9'}>} DISTINCT [Num. Empresa])", ano_ref_num),
        Baixas_Ano      = sprintf("Count({$<[Ano de Baixa]={'%d'}, tip_situacao={2}>} DISTINCT [Num. Empresa])", ano_ref_num)
      ),
      max_rows = 200
    )
    df_cnae_div$Grande_Setor <- mapear_grande_setor(df_cnae_div$cod_secao)
    df_cnae_div$Saldo_Ano <- df_cnae_div$Abertas_Ano - df_cnae_div$Baixas_Ano
    df_cnae_div$Participacao_Pct <- round(df_cnae_div$Empresas_Ativas / sum(df_cnae_div$Empresas_Ativas, na.rm = TRUE) * 100, 2)
    df_cnae_div <- df_cnae_div[order(-df_cnae_div$Empresas_Ativas), ]
    rownames(df_cnae_div) <- NULL

    # 2.4 Mapeamento Completo de Socios (QSA) e Responsaveis
    df_soc_genero <- qlik_hypercube(
      c2,
      dims = c("Gênero Sócio"),
      exprs = list(
        Qtd_Socios      = "Count({$<tip_situacao={1}>} cpfcnpj_qsa)",
        Empresas_Ativas = "Count({$<tip_situacao={1}>} DISTINCT [Num. Empresa])"
      )
    )
    names(df_soc_genero)[1] <- "Genero_Socio"
    df_soc_genero$Participacao_Socios_Pct <- round(df_soc_genero$Qtd_Socios / sum(df_soc_genero$Qtd_Socios, na.rm = TRUE) * 100, 2)

    df_soc_faixa_etaria <- qlik_hypercube(
      c2,
      dims = c("Faixa Etária Sócio"),
      exprs = list(
        Qtd_Socios      = "Count({$<tip_situacao={1}>} cpfcnpj_qsa)",
        Empresas_Ativas = "Count({$<tip_situacao={1}>} DISTINCT [Num. Empresa])"
      )
    )
    names(df_soc_faixa_etaria)[1] <- "Faixa_Etaria_Socio"
    df_soc_faixa_etaria$Participacao_Socios_Pct <- round(df_soc_faixa_etaria$Qtd_Socios / sum(df_soc_faixa_etaria$Qtd_Socios, na.rm = TRUE) * 100, 2)

    df_soc_qualif <- qlik_hypercube(
      c2,
      dims = c("desc_tipo_qsa", "Qualificação do Sócio"),
      exprs = list(
        Qtd_Socios      = "Count({$<tip_situacao={1}>} cpfcnpj_qsa)",
        Empresas_Ativas = "Count({$<tip_situacao={1}>} DISTINCT [Num. Empresa])"
      ),
      max_rows = 100
    )
    names(df_soc_qualif)[1:2] <- c("Tipo_Socio", "Qualificacao_Socio")
    df_soc_qualif$Participacao_Socios_Pct <- round(df_soc_qualif$Qtd_Socios / sum(df_soc_qualif$Qtd_Socios, na.rm = TRUE) * 100, 2)
    df_soc_qualif <- df_soc_qualif[order(-df_soc_qualif$Qtd_Socios), ]
    rownames(df_soc_qualif) <- NULL

    df_resp_perfil <- qlik_hypercube(
      c2,
      dims = c("Gênero Responsável", "Faixa Etária Responsável"),
      exprs = list(
        Empresas_Ativas = "Count({$<tip_situacao={1}>} DISTINCT [Num. Empresa])",
        MEI_Ativas      = "Count({$<tip_situacao={1}, [Opção MEI]={'MEI'}>} DISTINCT [Num. Empresa])"
      )
    )
    names(df_resp_perfil)[1:2] <- c("Genero_Responsavel", "Faixa_Etaria_Responsavel")
    df_resp_perfil$Participacao_Pct <- round(df_resp_perfil$Empresas_Ativas / sum(df_resp_perfil$Empresas_Ativas, na.rm = TRUE) * 100, 2)

    # Socios e Responsaveis por Municipio (139 municipios do TO)
    df_soc_mun <- qlik_hypercube(
      c2,
      dims = c("Município"),
      exprs = list(
        Total_Socios_QSA    = "Count({$<tip_situacao={1}>} cpfcnpj_qsa)",
        Socios_Homens       = "Count({$<tip_situacao={1}, [Gênero Sócio]={'Masculino'}>} cpfcnpj_qsa)",
        Socias_Mulheres     = "Count({$<tip_situacao={1}, [Gênero Sócio]={'Feminino'}>} cpfcnpj_qsa)",
        Resp_Homens         = "Count({$<tip_situacao={1}, [Gênero Responsável]={'Masculino'}>} DISTINCT [Num. Empresa])",
        Resp_Mulheres       = "Count({$<tip_situacao={1}, [Gênero Responsável]={'Feminino'}>} DISTINCT [Num. Empresa])"
      ),
      max_rows = 200
    )
    names(df_soc_mun)[1] <- "Municipio_UF"
    df_soc_mun$Pct_Mulheres_Responsaveis <- round(
      ifelse((df_soc_mun$Resp_Homens + df_soc_mun$Resp_Mulheres) > 0,
             df_soc_mun$Resp_Mulheres / (df_soc_mun$Resp_Homens + df_soc_mun$Resp_Mulheres) * 100, 0), 2
    )

    # Socios por Secao CNAE 2.0 (para boletins)
    df_soc_cnae <- qlik_hypercube(
      c2,
      dims = c("cod_secao", "des_secao"),
      exprs = list(
        Total_Socios_QSA = "Count({$<tip_situacao={1}>} cpfcnpj_qsa)",
        Socios_Homens    = "Count({$<tip_situacao={1}, [Gênero Sócio]={'Masculino'}>} cpfcnpj_qsa)",
        Socias_Mulheres  = "Count({$<tip_situacao={1}, [Gênero Sócio]={'Feminino'}>} cpfcnpj_qsa)"
      ),
      max_rows = 50
    )
    df_soc_cnae$Pct_Socias_Mulheres <- round(
      ifelse(df_soc_cnae$Total_Socios_QSA > 0, df_soc_cnae$Socias_Mulheres / df_soc_cnae$Total_Socios_QSA * 100, 0), 2
    )
    df_soc_cnae <- df_soc_cnae[order(-df_soc_cnae$Total_Socios_QSA), ]
    rownames(df_soc_cnae) <- NULL

    try(c2$ws$close(), silent = TRUE)

    # --------------------------------------------------------------------------
    # ETAPA 3: App 3 — Mapa de Empresas: Tempo de Abertura, Viabilidade e Registro
    # --------------------------------------------------------------------------
    cat(" -> [3/4] Conectando ao Motor Mapa de Empresas / REDESIM (App 3: Tempos em 2026)...\n")
    c3 <- qlik_connect("314cd578-2c59-45c4-894f-186422f7f5cd", "fed74ebc-0972-4bd9-9855-8e16921ce752")
    on.exit(try(c3$ws$close(), silent = TRUE), add = TRUE)

    c3$rpc(c3$doc_h, "ClearAll", list(FALSE))

    # 3.1 Serie Anual (2019-2026): Tocantins vs Brasil
    df_tempo_anual <- qlik_hypercube(
      c3,
      dims = c("Ano"),
      exprs = list(
        TO_Horas_Abertura    = "Avg({$<UF={'TO'}>} qtd_horas_abertura)",
        TO_Horas_Viabilidade = "Avg({$<UF={'TO'}>} qtd_horas_viabilidade)",
        TO_Horas_Registro    = "Avg({$<UF={'TO'}>} qtd_horas_registro)",
        BR_Horas_Abertura    = "Avg({$<UF=>} qtd_horas_abertura)",
        BR_Horas_Viabilidade = "Avg({$<UF=>} qtd_horas_viabilidade)",
        BR_Horas_Registro    = "Avg({$<UF=>} qtd_horas_registro)"
      ),
      max_rows = 30
    )
    df_tempo_anual <- df_tempo_anual[order(as.integer(df_tempo_anual$Ano)), ]
    rownames(df_tempo_anual) <- NULL

    # 3.2 Serie Mensal (2024-2026): Tocantins vs Brasil
    df_tempo_mensal <- qlik_hypercube(
      c3,
      dims = c("Ano", "Mês"),
      exprs = list(
        TO_Horas_Abertura    = "Avg({$<UF={'TO'}, Ano={'2024','2025','2026'}>} qtd_horas_abertura)",
        TO_Horas_Viabilidade = "Avg({$<UF={'TO'}, Ano={'2024','2025','2026'}>} qtd_horas_viabilidade)",
        TO_Horas_Registro    = "Avg({$<UF={'TO'}, Ano={'2024','2025','2026'}>} qtd_horas_registro)",
        BR_Horas_Abertura    = "Avg({$<UF=, Ano={'2024','2025','2026'}>} qtd_horas_abertura)",
        BR_Horas_Viabilidade = "Avg({$<UF=, Ano={'2024','2025','2026'}>} qtd_horas_viabilidade)",
        BR_Horas_Registro    = "Avg({$<UF=, Ano={'2024','2025','2026'}>} qtd_horas_registro)"
      ),
      max_rows = 100
    )
    names(df_tempo_mensal)[1:2] <- c("Ano", "Mes_Nome")
    df_tempo_mensal$Ano_Num <- suppressWarnings(as.integer(df_tempo_mensal$Ano))
    df_tempo_mensal$Mes_Num <- unname(meses_pt[df_tempo_mensal$Mes_Nome])
    df_tempo_mensal <- df_tempo_mensal[!is.na(df_tempo_mensal$Ano_Num) & !is.na(df_tempo_mensal$Mes_Num) & df_tempo_mensal$Ano_Num >= 2024, ]
    df_tempo_mensal <- df_tempo_mensal[order(df_tempo_mensal$Ano_Num, df_tempo_mensal$Mes_Num), ]
    rownames(df_tempo_mensal) <- NULL

    # 3.3 Tempos por Municipio do Tocantins em 2026
    f_uf3 <- c3$rpc(c3$doc_h, "GetField", list("UF"))$result$qReturn$qHandle
    c3$rpc(f_uf3, "SelectValues", list(list(list(qText = "TO")), FALSE, FALSE))

    df_tempo_mun <- qlik_hypercube(
      c3,
      dims = c("Município"),
      exprs = list(
        Horas_Abertura_2026    = sprintf("Avg({$<Ano={'%d'}>} qtd_horas_abertura)", ano_ref_num),
        Horas_Viabilidade_2026 = sprintf("Avg({$<Ano={'%d'}>} qtd_horas_viabilidade)", ano_ref_num),
        Horas_Registro_2026    = sprintf("Avg({$<Ano={'%d'}>} qtd_horas_registro)", ano_ref_num)
      ),
      max_rows = 200
    )
    names(df_tempo_mun)[1] <- "Municipio_UF"

    try(c3$ws$close(), silent = TRUE)

    # --------------------------------------------------------------------------
    # ETAPA 4: Receita Federal — Estatisticas SINAC (Simples Nacional) por UF e Mun
    # --------------------------------------------------------------------------
    cat(" -> [4/4] Consultando Portal Oficial do Simples Nacional (SINAC / Receita Federal)...\n")
    dados_sinac <- tryCatch({
      u0 <- "https://www8.receita.fazenda.gov.br/SimplesNacional/Aplicacoes/ATBHE/estatisticasSinac.app/Default.aspx"
      u1 <- "https://www8.receita.fazenda.gov.br/SimplesNacional/Aplicacoes/ATBHE/estatisticasSinac.app/EstatisticasOptantesPorDataMunicipio.aspx?tipoConsulta=1&anoConsulta="
      u_mun <- "https://www8.receita.fazenda.gov.br/SimplesNacional/Aplicacoes/ATBHE/estatisticasSinac.app/ConsultaMunicipios.aspx"

      r0 <- GET(u0, timeout(20))
      r1 <- GET(u1, add_headers(Referer = u0), timeout(20))
      doc1 <- read_html(content(r1, "text", encoding = "UTF-8"))
      dt_txt <- html_text(html_node(doc1, "#ctl00_ctl00_Conteudo_AntesTabela_lblDataEstatistica"))
      dt_sinac <- regmatches(dt_txt, regexpr("[0-9]{2}/[0-9]{2}/[0-9]{4}", dt_txt))

      vs1  <- html_attr(html_node(doc1, "#__VIEWSTATE"), "value")
      vsg1 <- html_attr(html_node(doc1, "#__VIEWSTATEGENERATOR"), "value")
      ev1  <- html_attr(html_node(doc1, "#__EVENTVALIDATION"), "value")

      # Consulta 1: Optantes pelo Simples Nacional por UF
      r2_uf <- POST(
        u1,
        add_headers(Referer = u1, Origin = "https://www8.receita.fazenda.gov.br"),
        body = list(
          `__VIEWSTATE` = vs1, `__VIEWSTATEGENERATOR` = vsg1, `__EVENTVALIDATION` = ev1,
          `ctl00$ctl00$Conteudo$AntesTabela$droplistOpcao` = "Por UF",
          `ctl00$ctl00$Conteudo$AntesTabela$droplistMes` = "",
          `ctl00$ctl00$Conteudo$AntesTabela$btnExibir` = "Exibir Dados",
          `ctl00$ctl00$Conteudo$AposTabela$ddlUf` = "UFNS",
          `ctl00$ctl00$Conteudo$AposTabela$hfMunicipios` = "",
          `ctl00$ctl00$Conteudo$AposTabela$hfUF` = "",
          `ctl00$ctl00$Conteudo$AposTabela$hfCheck` = "false"
        ),
        encode = "form", timeout(20)
      )
      doc2_uf <- read_html(content(r2_uf, "text", encoding = "UTF-8"))
      r3_uf <- POST(
        u1,
        add_headers(Referer = u1, Origin = "https://www8.receita.fazenda.gov.br"),
        body = list(
          `__VIEWSTATE` = html_attr(html_node(doc2_uf, "#__VIEWSTATE"), "value"),
          `__VIEWSTATEGENERATOR` = html_attr(html_node(doc2_uf, "#__VIEWSTATEGENERATOR"), "value"),
          `__EVENTVALIDATION` = html_attr(html_node(doc2_uf, "#__EVENTVALIDATION"), "value"),
          `ctl00$ctl00$Conteudo$AntesTabela$droplistOpcao` = "Por UF",
          `ctl00$ctl00$Conteudo$AntesTabela$txtDtIni` = dt_sinac,
          `ctl00$ctl00$Conteudo$AntesTabela$btnExibir` = "Exibir Dados",
          `ctl00$ctl00$Conteudo$AposTabela$ddlUf` = "UFNS",
          `ctl00$ctl00$Conteudo$AposTabela$hfMunicipios` = "",
          `ctl00$ctl00$Conteudo$AposTabela$hfUF` = "",
          `ctl00$ctl00$Conteudo$AposTabela$hfCheck` = "false"
        ),
        encode = "form", timeout(20)
      )
      tabs_uf <- html_table(read_html(content(r3_uf, "text", encoding = "UTF-8")), fill = TRUE)
      df_sinac_uf <- as.data.frame(tabs_uf[[3]], stringsAsFactors = FALSE)
      names(df_sinac_uf) <- c("UF", "Optantes_Simples")
      df_sinac_uf$Optantes_Simples <- as.numeric(gsub("[^0-9]", "", df_sinac_uf$Optantes_Simples))

      # Consulta 2: Optantes pelo Simples Nacional nos 139 municipios do TO
      r_m <- POST(u_mun, add_headers(Referer = u1), body = list(uf = "TO"), encode = "form", timeout(15))
      mun_vals <- html_attr(html_nodes(read_html(content(r_m, "text", encoding = "UTF-8")), "option"), "value")
      hf_mun_str <- paste0("'", mun_vals, "'", collapse = ",")

      r1_m <- GET(u1, add_headers(Referer = u0), timeout(20))
      doc1_m <- read_html(content(r1_m, "text", encoding = "UTF-8"))
      r2_m <- POST(
        u1,
        add_headers(Referer = u1, Origin = "https://www8.receita.fazenda.gov.br"),
        body = list(
          `__VIEWSTATE` = html_attr(html_node(doc1_m, "#__VIEWSTATE"), "value"),
          `__VIEWSTATEGENERATOR` = html_attr(html_node(doc1_m, "#__VIEWSTATEGENERATOR"), "value"),
          `__EVENTVALIDATION` = html_attr(html_node(doc1_m, "#__EVENTVALIDATION"), "value"),
          `ctl00$ctl00$Conteudo$AntesTabela$droplistOpcao` = "Por Município",
          `ctl00$ctl00$Conteudo$AntesTabela$droplistMes` = "",
          `ctl00$ctl00$Conteudo$AntesTabela$btnExibir` = "Exibir Dados",
          `ctl00$ctl00$Conteudo$AposTabela$ddlUf` = "TO",
          `ctl00$ctl00$Conteudo$AposTabela$hfMunicipios` = hf_mun_str,
          `ctl00$ctl00$Conteudo$AposTabela$hfUF` = "TO",
          `ctl00$ctl00$Conteudo$AposTabela$hfCheck` = "true"
        ),
        encode = "form", timeout(20)
      )
      doc2_m <- read_html(content(r2_m, "text", encoding = "UTF-8"))
      r3_m <- POST(
        u1,
        add_headers(Referer = u1, Origin = "https://www8.receita.fazenda.gov.br"),
        body = list(
          `__VIEWSTATE` = html_attr(html_node(doc2_m, "#__VIEWSTATE"), "value"),
          `__VIEWSTATEGENERATOR` = html_attr(html_node(doc2_m, "#__VIEWSTATEGENERATOR"), "value"),
          `__EVENTVALIDATION` = html_attr(html_node(doc2_m, "#__EVENTVALIDATION"), "value"),
          `ctl00$ctl00$Conteudo$AntesTabela$droplistOpcao` = "Por Município",
          `ctl00$ctl00$Conteudo$AntesTabela$txtDtIni` = dt_sinac,
          `ctl00$ctl00$Conteudo$AntesTabela$btnExibir` = "Exibir Dados",
          `ctl00$ctl00$Conteudo$AposTabela$ddlUf` = "TO",
          `ctl00$ctl00$Conteudo$AposTabela$hfMunicipios` = hf_mun_str,
          `ctl00$ctl00$Conteudo$AposTabela$hfUF` = "TO",
          `ctl00$ctl00$Conteudo$AposTabela$hfCheck` = "true"
        ),
        encode = "form", timeout(20)
      )
      tabs_m <- html_table(read_html(content(r3_m, "text", encoding = "UTF-8")), fill = TRUE)
      df_sinac_mun <- as.data.frame(tabs_m[[3]], stringsAsFactors = FALSE)
      names(df_sinac_mun) <- c("Municipio_SINAC", "Optantes_Simples_SINAC")
      df_sinac_mun$Optantes_Simples_SINAC <- as.numeric(gsub("[^0-9]", "", df_sinac_mun$Optantes_Simples_SINAC))

      list(
        data_posicao = dt_sinac,
        uf = df_sinac_uf,
        municipios_to = df_sinac_mun
      )
    }, error = function(e) {
      cat("    [Aviso SINAC] Falha parcial ao consultar portal SINAC:", conditionMessage(e), "\n")
      NULL
    })

    # Integra Socios e Tempos do Mapa de Empresas na tabela municipal de 139 municipios
    df_mun <- merge(df_mun, df_soc_mun, by = "Municipio_UF", all.x = TRUE)
    df_mun <- merge(df_mun, df_tempo_mun, by = "Municipio_UF", all.x = TRUE)

    # Integra Simples Nacional (SINAC) por nome normalizado (sem acentos / maiusculo)
    normalizar_nome <- function(x) {
      x_clean <- toupper(trimws(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")))
      gsub("[^A-Z0-9 ]", "", x_clean)
    }
    if (!is.null(dados_sinac) && !is.null(dados_sinac$municipios_to)) {
      df_s_m <- dados_sinac$municipios_to
      df_s_m <- df_s_m[df_s_m$Municipio_SINAC != "TOTAL", ]
      df_s_m$chave_mun <- normalizar_nome(df_s_m$Municipio_SINAC)
      df_mun$chave_mun <- normalizar_nome(df_mun$Municipio)
      df_mun <- merge(df_mun, df_s_m[, c("chave_mun", "Optantes_Simples_SINAC")], by = "chave_mun", all.x = TRUE)
      df_mun$chave_mun <- NULL
    } else {
      df_mun$Optantes_Simples_SINAC <- df_mun$MEI + df_mun$ME + df_mun$EPP
    }
    df_mun$Optantes_Simples_Nao_MEI <- pmax(0, df_mun$Optantes_Simples_SINAC - df_mun$MEI)
    df_mun$Pct_Simples_SINAC <- round(ifelse(df_mun$Ativas > 0, df_mun$Optantes_Simples_SINAC / df_mun$Ativas * 100, 0), 2)
    df_mun <- df_mun[order(-df_mun$Ativas), ]
    rownames(df_mun) <- NULL

    # Valores de Tempo de Abertura em 2026 (Ano atual)
    linha_tempo_2026 <- df_tempo_anual[df_tempo_anual$Ano == as.character(ano_ref_num), ]
    if (nrow(linha_tempo_2026) == 0) linha_tempo_2026 <- df_tempo_anual[nrow(df_tempo_anual), ]

    tempo_abertura_2026_to    <- round(as.numeric(linha_tempo_2026$TO_Horas_Abertura[1]), 1)
    tempo_viabilidade_2026_to <- round(as.numeric(linha_tempo_2026$TO_Horas_Viabilidade[1]), 1)
    tempo_registro_2026_to    <- round(as.numeric(linha_tempo_2026$TO_Horas_Registro[1]), 1)

    tempo_abertura_2026_br    <- round(as.numeric(linha_tempo_2026$BR_Horas_Abertura[1]), 1)
    tempo_viabilidade_2026_br <- round(as.numeric(linha_tempo_2026$BR_Horas_Viabilidade[1]), 1)
    tempo_registro_2026_br    <- round(as.numeric(linha_tempo_2026$BR_Horas_Registro[1]), 1)

    ult_tempo_m <- df_tempo_mensal[nrow(df_tempo_mensal), ]
    tempo_abertura_mes_to    <- round(as.numeric(ult_tempo_m$TO_Horas_Abertura[1]), 1)
    tempo_viabilidade_mes_to <- round(as.numeric(ult_tempo_m$TO_Horas_Viabilidade[1]), 1)
    tempo_registro_mes_to    <- round(as.numeric(ult_tempo_m$TO_Horas_Registro[1]), 1)

    total_optantes_simples_to <- if (!is.null(dados_sinac) && !is.null(dados_sinac$uf)) {
      val_to <- dados_sinac$uf$Optantes_Simples[dados_sinac$uf$UF == "TO"]
      if (length(val_to) > 0 && !is.na(val_to[1])) val_to[1] else (total_mei_to + total_me_exc_to + total_epp_to)
    } else {
      total_mei_to + total_me_exc_to + total_epp_to
    }

    pct_mei <- round(total_mei_to / total_ativas_to * 100, 1)
    pct_me  <- round(total_me_exc_to / total_ativas_to * 100, 1)
    pct_me_total <- round(total_me_all_to / total_ativas_to * 100, 1)
    pct_epp <- round(total_epp_to / total_ativas_to * 100, 1)
    pct_demais <- round(total_demais_to / total_ativas_to * 100, 1)

    # Data.frame para o grafico de linha "Evolucao Mensal de CNPJs Ativos" no painel online
    df_ativos_painel <- data.frame(
      Mes = factor(df_ult_12m$Periodo_Rotulo, levels = df_ult_12m$Periodo_Rotulo),
      Valor = df_ult_12m$Estoque_Ativo,
      Serie = "Empresas Ativas",
      stringsAsFactors = FALSE
    )

    texto_centro_rosca <- sprintf(
      "<b>%s Mil</b><br>Ativas",
      format(round(total_ativas_to / 1000, 1), decimal.mark = ",", nsmall = 1)
    )

    list(
      metadados = list(
        fonte_cnpj = "Receita Federal do Brasil / SERPRO — Painel Mapa de Empresas e Base CNPJ",
        fonte_simples = "Receita Federal do Brasil — Estatísticas SINAC / Simples Nacional",
        fonte_tempos = "MDIC / DREI / REDESIM — Painel Mapa de Empresas (Tempo de Abertura)",
        ano_referencia = ano_ref_num,
        mes_referencia_num = mes_ref_num,
        mes_referencia_nome = mes_ref_str,
        competencia_formatada = sprintf("%s/%d", meses_abrev[mes_ref_num], ano_ref_num),
        data_posicao_sinac = if (!is.null(dados_sinac)) dados_sinac$data_posicao else NA_character_,
        extraido_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      indicadores_painel = list(
        # Aba Visao Geral
        vb_novos_cnpjs = formatar_inteiro_br(novos_cnpjs_mes),
        novos_cnpjs_mes_num = novos_cnpjs_mes,
        baixas_cnpjs_mes_num = baixas_cnpjs_mes,
        saldo_cnpjs_mes_num = saldo_cnpjs_mes,
        # Aba Dinamica Empresarial
        vb_cnpjs_ativos = formatar_inteiro_br(total_ativas_to),
        cnpjs_ativos_num = total_ativas_to,
        vb_saldo_empresas = paste0(ifelse(saldo_12m >= 0, "+ ", "- "), formatar_inteiro_br(abs(saldo_12m))),
        saldo_12m_num = saldo_12m,
        aberturas_12m_num = aberturas_12m,
        baixas_12m_num = baixas_12m,
        vb_mei = paste0(format(pct_mei, decimal.mark = ",", nsmall = 1), "%"),
        pct_mei_num = pct_mei,
        qtd_mei_num = total_mei_to,
        vb_me = paste0(format(pct_me, decimal.mark = ",", nsmall = 1), "%"),
        vb_me_com_mei = paste0(format(pct_me_total, decimal.mark = ",", nsmall = 1), "%"),
        pct_me_num = pct_me,
        pct_me_com_mei_num = pct_me_total,
        qtd_me_num = total_me_exc_to,
        qtd_epp_num = total_epp_to,
        pct_epp_num = pct_epp,
        qtd_demais_num = total_demais_to,
        pct_demais_num = pct_demais,
        qtd_matrizes_num = total_matrizes_to,
        qtd_filiais_num = total_filiais_to,
        optantes_simples_sinac_num = total_optantes_simples_to,
        vb_optantes_simples = formatar_inteiro_br(total_optantes_simples_to),
        texto_centro_rosca = texto_centro_rosca,
        # Mapa de Empresas (Tempos em 2026)
        tempo_abertura_h = tempo_abertura_2026_to,
        tempo_viabilidade_h = tempo_viabilidade_2026_to,
        tempo_registro_h = tempo_registro_2026_to,
        media_brasil_h = tempo_abertura_2026_br,
        media_brasil_viabilidade_h = tempo_viabilidade_2026_br,
        media_brasil_registro_h = tempo_registro_2026_br,
        tempo_abertura_mes_recente_h = tempo_abertura_mes_to,
        tempo_viabilidade_mes_recente_h = tempo_viabilidade_mes_to,
        tempo_registro_mes_recente_h = tempo_registro_mes_to,
        # Data.frames prontos para os graficos do painel
        df_ativos = df_ativos_painel,
        df_porte = df_porte_painel
      ),
      tabelas_boletins = list(
        serie_mensal_to = df_mensal,
        serie_ultimos_12m_to = df_ult_12m,
        serie_anual_to = df_anual,
        porte_e_estabelecimento_detalhe = df_porte_det,
        matriz_vs_filial_to = df_matriz_filial,
        natureza_juridica_to = df_natjur,
        capital_social_to = df_cap,
        capital_social_por_porte_to = df_cap_porte,
        cnae_grande_setor_to = df_grande_setor,
        cnae_secao_to = df_cnae_sec,
        cnae_divisao_to = df_cnae_div,
        simples_nacional_sinac = dados_sinac,
        socios_qsa = list(
          por_genero = df_soc_genero,
          por_faixa_etaria = df_soc_faixa_etaria,
          por_tipo_e_qualificacao = df_soc_qualif,
          responsaveis_por_genero_idade = df_resp_perfil,
          por_secao_cnae = df_soc_cnae,
          por_municipio = df_soc_mun
        ),
        mapa_empresas_tempos = list(
          anual_to_vs_br = df_tempo_anual,
          mensal_to_vs_br = df_tempo_mensal,
          municipios_to_2026 = df_tempo_mun
        ),
        municipios_to = df_mun
      )
    )
  }, error = function(e) {
    cat(" -> [Erro na extracao online CNPJ]:", conditionMessage(e), "\n")
    if (file.exists(arq_resumo)) {
      cat(" -> Carregando base local existente:", arq_resumo, "\n")
      return(readRDS(arq_resumo))
    }
    stop(e)
  })

  # Salva bases otimizadas com compressao maxima xz
  cat(" -> Salvando bases compactadas (.rds com compress = 'xz')...\n")
  saveRDS(resumo_extraido$tabelas_boletins, arq_base, compress = "xz")
  saveRDS(resumo_extraido, arq_resumo, compress = "xz")

  tam_base_kb   <- round(file.info(arq_base)$size / 1024, 1)
  tam_resumo_kb <- round(file.info(arq_resumo)$size / 1024, 1)

  cat("======================================================================\n")
  cat(" EXTRACAO DE CNPJ / DINAMICA EMPRESARIAL CONCLUIDA COM SUCESSO!\n")
  cat(sprintf(" - Competencia mais recente: %s (%s)\n",
              resumo_extraido$metadados$competencia_formatada,
              resumo_extraido$metadados$mes_referencia_nome))
  cat(sprintf(" - CNPJs Ativos no Tocantins (Estoque): %s (Matrizes: %s | Filiais: %s)\n",
              resumo_extraido$indicadores_painel$vb_cnpjs_ativos,
              formatar_inteiro_br(resumo_extraido$indicadores_painel$qtd_matrizes_num),
              formatar_inteiro_br(resumo_extraido$indicadores_painel$qtd_filiais_num)))
  cat(sprintf(" - Novos CNPJs no mes: %s | Saldo ult. 12 meses: %s\n",
              resumo_extraido$indicadores_painel$vb_novos_cnpjs,
              resumo_extraido$indicadores_painel$vb_saldo_empresas))
  cat(sprintf(" - Participacao MEI: %s (%s) | Microempresas (ME): %s (%s) | EPP: %s%%\n",
              resumo_extraido$indicadores_painel$vb_mei,
              formatar_inteiro_br(resumo_extraido$indicadores_painel$qtd_mei_num),
              resumo_extraido$indicadores_painel$vb_me,
              formatar_inteiro_br(resumo_extraido$indicadores_painel$qtd_me_num),
              format(resumo_extraido$indicadores_painel$pct_epp_num, decimal.mark = ",")))
  cat(sprintf(" - Optantes pelo Simples Nacional (SINAC): %s\n",
              resumo_extraido$indicadores_painel$vb_optantes_simples))
  cat(sprintf(" - Mapa de Empresas (2026) — Abertura: %sh (Viabilidade: %sh | Registro: %sh) vs Brasil: %sh\n",
              format(resumo_extraido$indicadores_painel$tempo_abertura_h, decimal.mark = ","),
              format(resumo_extraido$indicadores_painel$tempo_viabilidade_h, decimal.mark = ","),
              format(resumo_extraido$indicadores_painel$tempo_registro_h, decimal.mark = ","),
              format(resumo_extraido$indicadores_painel$media_brasil_h, decimal.mark = ",")))
  cat(sprintf(" - Espaco em disco no SSD D:: cnpj_to_base.rds (%s KB) | cnpj_to_resumo.rds (%s KB)\n",
              tam_base_kb, tam_resumo_kb))
  cat("======================================================================\n")

  invisible(resumo_extraido)
}

# Se executado diretamente via Rscript, roda a extracao imediatamente
if (sys.nframe() == 0L) {
  extrair_cnpj_tocantins(diretorio_raiz = "D:/Journal", forcar_atualizacao = TRUE)
}
