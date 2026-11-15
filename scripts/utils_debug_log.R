# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/utils_debug_log.R
# Objetivo:
#   1. Padronizar avisos detalhados de console, rastreamento de erros e tempo de
#      execução em todos os scripts de extração e compilação de boletins.
#   2. Facilitar o debugging imediato (exibindo script, bloco, origem/URL,
#      mensagem original do R, ação de fallback e instrução de correção).
#   3. Fornecer o Motor de Diagnóstico Estatístico Automático ("Análise Robótica")
#      que identifica crescimentos, quedas, valores estáveis e mudanças drásticas
#      antes da análise humana da equipe.
# ==============================================================================

if (!exists(".reg_diagnostico_jornal", envir = .GlobalEnv)) {
  assign(".reg_diagnostico_jornal", list(), envir = .GlobalEnv)
}

limpar_registro_diagnostico <- function() {
  assign(".reg_diagnostico_jornal", list(), envir = .GlobalEnv)
}

registrar_status_bloco <- function(script, bloco, status, detalhe = "", tempo_s = NA_real_) {
  reg <- get(".reg_diagnostico_jornal", envir = .GlobalEnv)
  reg[[length(reg) + 1]] <- data.frame(
    Horario   = format(Sys.time(), "%H:%M:%S"),
    Script    = as.character(script),
    Bloco     = as.character(bloco),
    Status    = as.character(status),
    Tempo_Seg = ifelse(is.na(tempo_s), "-", sprintf("%.2fs", as.numeric(tempo_s))),
    Detalhe   = as.character(detalhe),
    stringsAsFactors = FALSE
  )
  assign(".reg_diagnostico_jornal", reg, envir = .GlobalEnv)
}

log_secao <- function(titulo) {
  cat("\n======================================================================\n")
  cat(sprintf(" [%s] %s\n", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), titulo))
  cat("======================================================================\n")
}

log_info <- function(bloco, msg) {
  cat(sprintf(" -> [INFO | %s] %s\n", bloco, msg))
}

log_ok <- function(bloco, msg, tempo_s = NA_real_, script = "atualizar_dados.R") {
  str_t <- if (!is.na(tempo_s)) sprintf(" (%.2fs)", tempo_s) else ""
  cat(sprintf("   [OK%s | %s] %s\n", str_t, bloco, msg))
  registrar_status_bloco(script, bloco, "OK", msg, tempo_s)
}

log_aviso <- function(bloco, msg, como_corrigir = "Verifique a fonte ou cache local.", script = "atualizar_dados.R") {
  cat(sprintf("   [AVISO | %s] %s | Dica: %s\n", bloco, msg, como_corrigir))
  registrar_status_bloco(script, bloco, "AVISO / CACHE", paste0(msg, " [", como_corrigir, "]"), NA_real_)
}

log_erro_debug <- function(script, bloco, origem, erro_msg, fallback_acao = "Usando cache local", como_corrigir = "Verifique conexão, pacotes ou estrutura da fonte.") {
  cat("\n----------------------------------------------------------------------\n")
  cat(" [ERRO DEBUG DETECTADO - FACILITADOR DE MANUTENÇÃO]\n")
  cat(sprintf("   * Script / Módulo  : %s\n", script))
  cat(sprintf("   * Bloco / Pesquisa : %s\n", bloco))
  cat(sprintf("   * Origem / Arquivo : %s\n", origem))
  cat(sprintf("   * Mensagem do R    : %s\n", erro_msg))
  cat(sprintf("   * Ação de Fallback : %s\n", fallback_acao))
  cat(sprintf("   * Como Corrigir    : %s\n", como_corrigir))
  cat("----------------------------------------------------------------------\n")
  registrar_status_bloco(script, bloco, "ERRO (FALLBACK)", paste0(erro_msg, " -> ", fallback_acao), NA_real_)
}

executar_bloco_seguro <- function(script, bloco, origem, expr, fallback = NULL, como_corrigir = "Verifique a disponibilidade da API/arquivo ou o formato das colunas.") {
  t0 <- proc.time()[["elapsed"]]
  tryCatch({
    res <- force(expr)
    dt <- proc.time()[["elapsed"]] - t0
    if (is.null(res) || (is.data.frame(res) && nrow(res) == 0)) {
      log_aviso(bloco, sprintf("Retorno vazio de [%s]. Acionando fallback.", origem), como_corrigir, script)
      return(fallback)
    }
    info_tam <- if (is.data.frame(res)) {
      sprintf("%d linhas x %d colunas extraídas", nrow(res), ncol(res))
    } else if (is.list(res)) {
      sprintf("Lista com %d elementos processada", length(res))
    } else {
      "Processado com sucesso"
    }
    log_ok(bloco, info_tam, dt, script)
    res
  }, error = function(e) {
    log_erro_debug(
      script        = script,
      bloco         = bloco,
      origem        = origem,
      erro_msg      = conditionMessage(e),
      fallback_acao = if (!is.null(fallback)) "Cache/valor anterior preservado com segurança" else "Retornando NULL",
      como_corrigir = como_corrigir
    )
    fallback
  })
}

imprimir_resumo_diagnostico <- function(titulo = "RESUMO DE SAÚDE E DEBUGGING DA EXECUÇÃO") {
  reg <- get(".reg_diagnostico_jornal", envir = .GlobalEnv)
  if (length(reg) == 0) return(invisible(NULL))
  df_reg <- do.call(rbind, reg)
  cat("\n======================================================================\n")
  cat(sprintf(" %s\n", titulo))
  cat("======================================================================\n")
  n_ok   <- sum(df_reg$Status == "OK")
  n_warn <- sum(grepl("AVISO", df_reg$Status))
  n_err  <- sum(grepl("ERRO", df_reg$Status))
  cat(sprintf(" Total de etapas monitoradas: %d | OK: %d | Avisos/Cache: %d | Erros: %d\n",
              nrow(df_reg), n_ok, n_warn, n_err))
  cat("----------------------------------------------------------------------\n")
  for (i in seq_len(nrow(df_reg))) {
    r <- df_reg[i, ]
    cat(sprintf(" [%s] %-15s | %-28s | %s (%s)\n",
                r$Horario, r$Status, substr(r$Bloco, 1, 28), r$Detalhe, r$Tempo_Seg))
  }
  cat("======================================================================\n\n")
}

# ==============================================================================
# MOTOR DE DIAGNÓSTICO ESTATÍSTICO AUTOMÁTICO ("ANÁLISE ROBÓTICA" PRELIMINAR)
# Classifica variações, detecta mudanças drásticas (> 1.5 desvio-padrão ou > 15%),
# estagnações, quedas e crescimentos para alimentar os boletins antes da equipe.
# ==============================================================================
classificar_variacao_automatica <- function(var_pct, nome_indicador = "Indicador", serie_historica = NULL) {
  v <- suppressWarnings(as.numeric(var_pct))
  if (is.na(v)) {
    return(list(
      status = "SEM DADO COMPARÁVEL",
      intensidade = "neutra",
      frase = sprintf("%s não apresentou base de comparação direta no período.", nome_indicador)
    ))
  }

  # Verifica se é mudança drástica pela distribuição histórica (z-score >= 1.5) ou magnitude >= 15%
  eh_drastico <- abs(v) >= 15
  if (!is.null(serie_historica)) {
    sh <- suppressWarnings(as.numeric(serie_historica))
    sh <- sh[is.finite(sh)]
    if (length(sh) >= 5 && stats::sd(sh) > 0) {
      z <- abs((v - mean(sh)) / stats::sd(sh))
      if (is.finite(z) && z >= 1.65) eh_drastico <- TRUE
    }
  }

  sinal_str <- paste0(if (v > 0) "+" else "", format(round(v, 2), decimal.mark = ",", nsmall = 2), "%")

  if (abs(v) < 0.10) {
    list(
      status = "ESTABILIDADE (VALOR INALTERADO)",
      intensidade = "estavel",
      frase = sprintf("%s manteve-se praticamente inalterado (%s), indicando estabilidade marginal.", nome_indicador, sinal_str)
    )
  } else if (v > 0 && eh_drastico) {
    list(
      status = "EXPANSÃO DRÁSTICA / SALTO POSITIVO",
      intensidade = "alta_forte",
      frase = sprintf("%s registrou aceleração expressiva de %s (mudança drástica acima do padrão recente).", nome_indicador, sinal_str)
    )
  } else if (v >= 5) {
    list(
      status = "CRESCIMENTO FORTE",
      intensidade = "alta",
      frase = sprintf("%s apresentou crescimento robusto de %s no período.", nome_indicador, sinal_str)
    )
  } else if (v > 0) {
    list(
      status = "AVANÇO MODERADO",
      intensidade = "alta_leve",
      frase = sprintf("%s avançou %s em trajetória de expansão gradual.", nome_indicador, sinal_str)
    )
  } else if (v < 0 && eh_drastico) {
    list(
      status = "QUEDA DRÁSTICA / CONTRAÇÃO ACENTUADA",
      intensidade = "queda_forte",
      frase = sprintf("%s sofreu retração acentuada de %s (queda drástica que requer atenção conjuntural).", nome_indicador, sinal_str)
    )
  } else if (v <= -5) {
    list(
      status = "RETRAÇÃO RELEVANTE",
      intensidade = "queda",
      frase = sprintf("%s recuou %s, sinalizando desaceleração relevante no período.", nome_indicador, sinal_str)
    )
  } else {
    list(
      status = "OSCILAÇÃO NEGATIVA LEVE",
      intensidade = "queda_leve",
      frase = sprintf("%s apresentou acomodação negativa de %s.", nome_indicador, sinal_str)
    )
  }
}

gerar_resumo_diagnostico_script <- function(lista_indicadores, otica = "mensal") {
  # lista_indicadores: list of list(nome=..., var_pct=..., valor_fmt=..., serie=...)
  if (length(lista_indicadores) == 0) return(list(latex = "", html = "", itens = list()))
  analises <- lapply(lista_indicadores, function(it) {
    cl <- classificar_variacao_automatica(it$var_pct, it$nome, it$serie)
    val_info <- if (!is.null(it$valor_fmt) && nzchar(it$valor_fmt)) paste0(" [nível: ", it$valor_fmt, "]") else ""
    list(
      nome      = it$nome,
      valor_fmt = it$valor_fmt,
      var_pct   = it$var_pct,
      status    = cl$status,
      frase     = paste0(cl$frase, val_info)
    )
  })
  rotulo_otica <- if (identical(otica, "anual")) {
    "Síntese de Variações e Desempenho (Ótica Anual / 12 Meses)"
  } else if (identical(otica, "conjuntura")) {
    "Radar de Variações, Saltos e Estabilidades Conjunturais"
  } else {
    "Síntese de Variações entre Períodos Subsequentes"
  }
  list(
    titulo = rotulo_otica,
    itens  = analises
  )
}
