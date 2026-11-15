# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/dessazonalizacao_pib.R
# Objetivo:
#   1. Ler a série encadeada do PIB Trimestral do Tocantins (aba "Série Encadeada"
#      do arquivo 'dados/brutos/pib_trimestral_seplan.xlsx').
#   2. Aplicar o ajuste sazonal via X-13ARIMA-SEATS (pacote 'seasonal') para:
#      - Agropecuária (Valor Adicionado)
#      - Indústria (Valor Adicionado + busca em grid de melhor modelo ARIMA)
#      - Serviços (Valor Adicionado)
#      - Valor Adicionado Total (VA Total)
#      - Impostos sobre Produtos
#      - PIB Total
#   3. Calcular a Série Encadeada com Ajuste Sazonal e a Taxa de Variação
#      Trimestre contra Trimestre Imediatamente Anterior (%).
#   4. Exportar as séries dessazonalizadas, variações trimestrais e matrizes de
#      decomposição X-13 (Tendência-Ciclo d12, Fator Sazonal d10 e Irregular d13)
#      para alimentar automaticamente o 'scripts/atualizar_dados.R', o painel e
#      as publicações (Boletins Flash, Dossiês Anuais e Cartas de Conjuntura).
# ==============================================================================

if (!file.exists("index.qmd") && dir.exists("D:/Journal")) {
  setwd("D:/Journal")
}

suppressPackageStartupMessages({
  for (pkg in c("seasonal", "readxl", "writexl", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
  library(seasonal)
  library(readxl)
  library(writexl)
  library(dplyr)
})

# Garante que as pastas de dados existam na raiz do projeto
dir.create("dados", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/brutos", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Localização inteligente da planilha do PIB Trimestral da SEPLAN-TO:
# O local oficial é 'dados/brutos/pib_trimestral_seplan.xlsx'.
# Se o usuário soltar uma planilha nova baixada da SEPLAN (ex: 'tabelas_completas_...')
# dentro de 'dados/brutos/' ou 'scripts/', o script detecta e sincroniza automaticamente!
# ------------------------------------------------------------------------------
sincronizar_planilha_pib_seplan <- function() {
  caminho_padrao <- "dados/brutos/pib_trimestral_seplan.xlsx"
  
  candidatos <- c(
    list.files("dados/brutos", pattern = "(contas_trimestrais|pib_trimestral).*\\.xlsx$", full.names = TRUE, ignore.case = TRUE),
    list.files("scripts",      pattern = "(contas_trimestrais|pib_trimestral).*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
  )
  candidatos <- candidatos[!grepl("^~\\$", basename(candidatos))]
  candidatos_novos <- setdiff(normalizePath(candidatos, winslash = "/", mustWork = FALSE),
                              normalizePath(caminho_padrao, winslash = "/", mustWork = FALSE))
  
  if (length(candidatos_novos) > 0) {
    # Escolhe o arquivo mais recente caso haja mais de um
    info_arq <- file.info(candidatos_novos)
    arq_mais_recente <- rownames(info_arq)[which.max(info_arq$mtime)]
    if (!file.exists(caminho_padrao) || file.info(arq_mais_recente)$mtime > file.info(caminho_padrao)$mtime) {
      file.copy(arq_mais_recente, caminho_padrao, overwrite = TRUE)
      cat(sprintf("-> Planilha SEPLAN sincronizada de '%s' para '%s'.\n", arq_mais_recente, caminho_padrao))
    }
    # Se o arquivo estava na pasta 'scripts/', remove a cópia avulsa para manter 'scripts/' limpa
    if (dirname(arq_mais_recente) == "scripts" && file.exists(caminho_padrao)) {
      try(file.remove(arq_mais_recente), silent = TRUE)
    }
  }
  return(caminho_padrao)
}

caminho_planilha_seplan <- sincronizar_planilha_pib_seplan()

# ------------------------------------------------------------------------------
# Leitura da aba "Série Encadeada" da planilha oficial da SEPLAN-TO
# ------------------------------------------------------------------------------
ler_serie_encadeada_seplan <- function(caminho) {
  if (!file.exists(caminho)) {
    stop(sprintf("Arquivo nao encontrado em '%s'. Coloque a planilha do PIB Trimestral na pasta 'dados/brutos/'.", caminho))
  }
  abas <- readxl::excel_sheets(caminho)
  if ("Série Encadeada" %in% abas) {
    raw <- as.data.frame(suppressMessages(
      readxl::read_excel(caminho, sheet = "Série Encadeada", skip = 3, col_names = FALSE)
    ))[, 1:7]
    colnames(raw) <- c("Periodo", "AGROPECUÁRIA", "INDÚSTRIA", "SERVIÇOS", "VA TOTAL", "IMPOSTOS", "PIB TOTAL")
    raw$Periodo <- trimws(as.character(raw$Periodo))
    raw <- raw[!is.na(raw$Periodo) & grepl("^[0-9]{4}\\.(I|II|III|IV)$", raw$Periodo), ]
    for (col in c("AGROPECUÁRIA", "INDÚSTRIA", "SERVIÇOS", "VA TOTAL", "IMPOSTOS", "PIB TOTAL")) {
      raw[[col]] <- suppressWarnings(as.numeric(raw[[col]]))
    }
    rownames(raw) <- NULL
    return(raw)
  } else {
    # Fallback caso seja uma planilha já recortada apenas com as colunas
    raw <- as.data.frame(suppressMessages(readxl::read_excel(caminho)))
    return(raw)
  }
}

dados <- ler_serie_encadeada_seplan(caminho_planilha_seplan)

###############################################################
####  Criar série ts e analises trimestrais  ##################

agro_ts     <- ts(dados$`AGROPECUÁRIA`, start = c(2010, 1), frequency = 4)
ind_ts      <- ts(dados$`INDÚSTRIA`,    start = c(2010, 1), frequency = 4)
serv_ts     <- ts(dados$`SERVIÇOS`,     start = c(2010, 1), frequency = 4)
VA_ts       <- ts(dados$`VA TOTAL`,     start = c(2010, 1), frequency = 4)
impostos_ts <- ts(dados$`IMPOSTOS`,     start = c(2010, 1), frequency = 4)
PIB_ts      <- ts(dados$`PIB TOTAL`,    start = c(2010, 1), frequency = 4)

p_max_nao_sazonal <- 2 # Buscar p entre 0 e 2
d_nao_sazonal     <- 1 # Fixamos a diferenciação não sazonal em 1 (comum para séries com tendência)
q_max_nao_sazonal <- 2 # Buscar q entre 0 e 2

modelo_hibrido_string <- sprintf(
  "(0 %d) %d (0 %d)(0 1 1)",
  p_max_nao_sazonal,
  d_nao_sazonal,
  q_max_nao_sazonal
)

###############################################################
#########     dessazonalizar       ############################

cat("-> Executando dessazonalizacao X-13ARIMA-SEATS (PIB e grandes setores do Tocantins)...\n")

# 1. Modelo AGROPECUÁRIA
modelo_agro <- seas(
  agro_ts,
  transform.function = "auto",
  regression.aictest = c("td", "easter"),
  automdl            = "",
  outlier            = list(types = "all"),
  forecast           = list(maxlead = 6, maxback = 0),
  x11                = list(savelog = "q")
) # ou colocar no lugar de automdl -> automdl = list(), arima.model = "(0 1 1)s"
dados$Agro_dessaz <- as.numeric(final(modelo_agro))

# 2. Modelo INDÚSTRIA
modelo_ind <- seas(
  ind_ts,
  transform.function = "auto",
  regression.aictest = c("td", "easter"),
  automdl            = "",
  outlier            = list(types = "all"),
  forecast           = list(maxlead = 6, maxback = 0),
  x11                = list(savelog = "q")
)
dados$Ind_dessaz <- as.numeric(final(modelo_ind))

# 3. Modelo SERVIÇOS
modelo_serv <- seas(
  serv_ts,
  transform.function = "auto",
  regression.aictest = c("td", "easter"),
  automdl            = "",
  outlier            = list(types = "all"),
  forecast           = list(maxlead = 6, maxback = 0),
  x11                = list(savelog = "q")
)
dados$serv_dessaz <- as.numeric(final(modelo_serv))

# 4. Modelo VA TOTAL
modelo_VA <- seas(
  VA_ts,
  transform.function = "auto",
  regression.aictest = c("td", "easter"),
  automdl            = "",
  outlier            = list(types = "all"),
  forecast           = list(maxlead = 6, maxback = 0),
  x11                = list(savelog = "q")
)
dados$VA_dessaz <- as.numeric(final(modelo_VA))

# 5. Modelo IMPOSTOS SOBRE PRODUTOS
modelo_impostos <- seas(
  impostos_ts,
  transform.function = "auto",
  regression.aictest = c("td", "easter"),
  automdl            = "",
  outlier            = list(types = "all"),
  forecast           = list(maxlead = 6, maxback = 0),
  x11                = list(savelog = "q")
)
dados$Impostos_dessaz <- as.numeric(final(modelo_impostos))

# 6. Modelo PIB TOTAL
modelo_PIB <- seas(
  PIB_ts,
  transform.function = "auto",
  regression.aictest = c("td", "easter"),
  automdl            = "",
  outlier            = list(types = "all"),
  forecast           = list(maxlead = 6, maxback = 0),
  x11                = list(savelog = "q")
)
dados$PIB_dessaz <- as.numeric(final(modelo_PIB))

### Melhorando o modelo da Indústria (Busca em Grid de Especificações ARIMA)
encontrar_melhor_arima <- function(serie_ts,
                                   p_range = 0:2,
                                   d_range = 0:1,
                                   q_range = 0:2,
                                   P_saz_range = 0,
                                   D_saz_fixo = 1,
                                   Q_saz_fixo = 0,
                                   silencioso = TRUE,
                                   ...) {
  # Criar o grid de busca
  search_grid <- expand.grid(p = p_range, d = d_range, q = q_range, P_saz = P_saz_range)

  # Listas para armazenar resultados
  results_list <- list()
  best_aic <- Inf
  best_model <- NULL

  if (!silencioso) {
    cat(sprintf("Iniciando teste de %d combinacoes de modelos...\n", nrow(search_grid)))
  }

  # Loop de teste
  for (i in 1:nrow(search_grid)) {
    params <- search_grid[i, ]
    model_string <- sprintf("(%d %d %d)(%d %d %d)",
                            params$p, params$d, params$q,
                            params$P_saz, D_saz_fixo, Q_saz_fixo)

    if (!silencioso) {
      cat(sprintf("\rTestando %d/%d: ARIMA%s", i, nrow(search_grid), model_string))
    }

    # Tentar ajustar o modelo
    current_model <- try(
      seas(x = serie_ts,
           list = list(arima = list(model = model_string)),
           ...),
      silent = TRUE
    )

    # Se o ajuste funcionou
    if (!inherits(current_model, "try-error")) {
      aic_val <- AIC(current_model)
      bic_val <- BIC(current_model)

      results_list[[i]] <- data.frame(
        Modelo = model_string,
        AIC    = aic_val,
        BIC    = bic_val,
        LogLik = as.numeric(logLik(current_model))
      )

      if (aic_val < best_aic) {
        best_aic <- aic_val
        best_model <- current_model
      }
    } else {
      results_list[[i]] <- data.frame(Modelo = model_string, AIC = NA, BIC = NA, LogLik = NA)
    }
  }

  if (!silencioso) cat("\nBusca finalizada.\n")

  # Preparar a tabela de resultados final
  final_table <- bind_rows(results_list) %>%
    filter(!is.na(AIC)) %>%
    arrange(AIC)

  # Retornar uma lista com a tabela e o melhor modelo
  return(list(tabela = final_table, melhor_modelo = best_model))
}

resultados_busca <- encontrar_melhor_arima(
  serie_ts           = ind_ts,
  silencioso         = !interactive(),
  transform.function = "auto",
  regression.aictest = c("td", "easter"),
  outlier.types      = "all",
  forecast.maxlead   = 6,
  x11.savelog        = "q"
)

# ------------------------------------------------------------------------------
# Diagnósticos Visuais e Testes Estatísticos (Executados apenas em modo interativo
# no RStudio para não abrir janelas gráficas durante a compilação automática)
# ------------------------------------------------------------------------------
if (interactive() && isTRUE(getOption("jornal.plots_x13", default = FALSE))) {
  ##### Plots dos gráficos das médias trimestrais
  for (par_ts in list(
    list(s = agro_ts,     nome = "Agropecuária"),
    list(s = ind_ts,      nome = "Indústria"),
    list(s = serv_ts,     nome = "Serviços"),
    list(s = VA_ts,       nome = "VA Total"),
    list(s = impostos_ts, nome = "Impostos"),
    list(s = PIB_ts,      nome = "PIB Total")
  )) {
    plot(par_ts$s, main = par_ts$nome)
    media_por_trimestre <- tapply(as.numeric(par_ts$s), cycle(par_ts$s), mean)
    plot(1:4, media_por_trimestre, type = "b", xaxt = "n",
         xlab = "Trim", ylab = "média",
         main = paste("Média trimestral ao longo dos anos -", par_ts$nome))
    axis(1, at = 1:4, labels = c("1 trim", "2 trim", "3 trim", "4 trim"))
    grid()
  }

  ##### Diagnóstico completo de cada modelo X-13
  for (par_mod in list(
    list(ts = agro_ts,     mod = modelo_agro,     nome = "Agropecuária"),
    list(ts = ind_ts,      mod = modelo_ind,      nome = "Indústria"),
    list(ts = serv_ts,     mod = modelo_serv,     nome = "Serviços"),
    list(ts = VA_ts,       mod = modelo_VA,       nome = "VA Total"),
    list(ts = impostos_ts, mod = modelo_impostos, nome = "Impostos"),
    list(ts = PIB_ts,      mod = modelo_PIB,      nome = "PIB Total")
  )) {
    print(summary(par_mod$mod))
    print(qs(par_mod$mod))
    plot(par_mod$mod)
    ts.plot(par_mod$ts, final(par_mod$mod), col = c("red", "blue"), lty = 1:2,
            main = paste(par_mod$nome, "- Original vs. Dessazonalizado"), ylab = "Índice")
    legend("topleft", legend = c("Original", "Dessazonalizado"), col = c("red", "blue"), lty = 1:2)
    hist(residuals(par_mod$mod), main = paste("Histograma dos resíduos -", par_mod$nome),
         xlab = "resíduos", ylab = "Frequência", col = "blue", border = "black")
    print(Box.test(residuals(par_mod$mod), lag = 20, type = "Ljung-Box"))
    print(shapiro.test(residuals(par_mod$mod)))
    acf(residuals(par_mod$mod), main = paste("ACF dos Resíduos -", par_mod$nome))
    pacf(residuals(par_mod$mod), main = paste("PACF dos Resíduos -", par_mod$nome))
  }
}

# ------------------------------------------------------------------------------
# Construção das Tabelas Padronizadas para o Jornal e Painel:
#   1. serie_ajuste_sazonal: Número-índice encadeado dessazonalizado (Base 2010 = 100)
#   2. trimestre_contra_anterior: Variação percentual trimestre contra trimestre
#      imediatamente anterior ((I_t / I_{t-1} - 1) * 100)
# ------------------------------------------------------------------------------
converter_periodo_seplan_sidra <- function(periodos) {
  partes <- strsplit(as.character(periodos), "\\.")
  vapply(partes, function(p) {
    if (length(p) < 2) return(NA_character_)
    tri <- match(trimws(p[2]), c("I", "II", "III", "IV"))
    if (is.na(tri)) return(NA_character_)
    sprintf("%s0%d", trimws(p[1]), tri)
  }, character(1))
}

padronizar_rotulo_trim <- function(periodos) {
  partes <- strsplit(as.character(periodos), "\\.")
  vapply(partes, function(p) {
    if (length(p) == 2) {
      tri <- match(trimws(p[2]), c("I", "II", "III", "IV", "1", "2", "3", "4"))
      if (tri > 4) tri <- tri - 4
      if (!is.na(tri)) return(sprintf("T%d %s", tri, trimws(p[1])))
    }
    trimws(as.character(p[1]))
  }, character(1))
}

calcular_var_trimestral_pct <- function(vetor_indice) {
  v <- as.numeric(vetor_indice)
  if (length(v) < 2) return(rep(NA_real_, length(v)))
  c(NA_real_, ((v[-1] / head(v, -1)) - 1) * 100)
}

codigos_sidra <- converter_periodo_seplan_sidra(dados$Periodo)
rotulos_trim  <- padronizar_rotulo_trim(dados$Periodo)

df_serie_ajuste_sazonal <- data.frame(
  Periodo       = rotulos_trim,
  Agropecuaria  = dados$Agro_dessaz,
  Industria     = dados$Ind_dessaz,
  Servicos      = dados$serv_dessaz,
  VA_Total      = dados$VA_dessaz,
  Impostos      = dados$Impostos_dessaz,
  PIB           = dados$PIB_dessaz,
  Trimestre_Cod = codigos_sidra,
  stringsAsFactors = FALSE
)

df_trimestre_contra_anterior <- data.frame(
  Periodo       = rotulos_trim,
  Agropecuaria  = calcular_var_trimestral_pct(df_serie_ajuste_sazonal$Agropecuaria),
  Industria     = calcular_var_trimestral_pct(df_serie_ajuste_sazonal$Industria),
  Servicos      = calcular_var_trimestral_pct(df_serie_ajuste_sazonal$Servicos),
  VA_Total      = calcular_var_trimestral_pct(df_serie_ajuste_sazonal$VA_Total),
  Impostos      = calcular_var_trimestral_pct(df_serie_ajuste_sazonal$Impostos),
  PIB           = calcular_var_trimestral_pct(df_serie_ajuste_sazonal$PIB),
  Trimestre_Cod = codigos_sidra,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Decomposição X-13 (Tendência-Ciclo d12, Fator Sazonal d10 e Irregular d13)
# ------------------------------------------------------------------------------
preparar_matriz_excel <- function(serie_ts) {
  anos_inicio <- floor(start(serie_ts)[1])
  anos_fim    <- floor(end(serie_ts)[1])
  nomes_anos  <- anos_inicio:anos_fim
  n_esperado  <- length(nomes_anos) * 4
  vetor_pad   <- rep(NA_real_, n_esperado)
  vetor_pad[seq_along(serie_ts)] <- as.numeric(serie_ts)
  
  matriz <- matrix(vetor_pad, ncol = 4, byrow = TRUE)
  df     <- as.data.frame(matriz)
  colnames(df) <- c("T1", "T2", "T3", "T4")
  df <- cbind(Ano = nomes_anos, df)
  return(df)
}

tendencia_ciclo_agro <- series(modelo_agro, "d12")
fator_sazonal_agro   <- series(modelo_agro, "d10")
irregular_agro       <- series(modelo_agro, "d13")

tendencia_ciclo_pib  <- series(modelo_PIB, "d12")
fator_sazonal_pib    <- series(modelo_PIB, "d10")
irregular_pib        <- series(modelo_PIB, "d13")

lista_planilhas_dessaz <- list(
  "Serie_Ajuste_Sazonal"      = df_serie_ajuste_sazonal,
  "Trimestre_Contra_Anterior" = df_trimestre_contra_anterior,
  "Dados_Originais_e_Dessaz"  = dados,
  "Tendencia_Ciclo_PIB"       = preparar_matriz_excel(tendencia_ciclo_pib),
  "Fator_Sazonal_PIB"         = preparar_matriz_excel(fator_sazonal_pib),
  "Irregular_PIB"             = preparar_matriz_excel(irregular_pib),
  "Tendencia_Ciclo_Agro"      = preparar_matriz_excel(tendencia_ciclo_agro),
  "Fator_Sazonal_Agro"        = preparar_matriz_excel(fator_sazonal_agro),
  "Irregular_Agro"            = preparar_matriz_excel(irregular_agro)
)

# Salva os resultados em Excel e RDS para consulta da equipe e integração com o painel
writexl::write_xlsx(lista_planilhas_dessaz, "dados/brutos/pib_to_dessazonalizado.xlsx")

resultado_dessaz_pib_to <- list(
  serie_ajuste_sazonal      = df_serie_ajuste_sazonal,
  trimestre_contra_anterior = df_trimestre_contra_anterior,
  dados_completos           = dados,
  decomposicao              = lista_planilhas_dessaz,
  modelos_arima_industria   = resultados_busca$tabela
)

saveRDS(resultado_dessaz_pib_to, "dados/pib_to_dessazonalizado.rds")

cat("-> Dessazonalizacao concluida! Arquivos 'dados/brutos/pib_to_dessazonalizado.xlsx' e 'dados/pib_to_dessazonalizado.rds' gerados.\n")
