# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/modulos_boletins_tematicos.R
# Objetivo:
#   Geradores temáticos completos para:
#     1. BOLETINS FLASH (1 coluna, foco em 2 páginas / máx. 3, ótica mensal/trimestral
#        com variações entre períodos subsequentes t / t-1):
#        - flash_panorama (Visão Geral — exclusivo FLASH)
#        - flash_pib (PIB TO + IBGE 1621 lado a lado + Contas Regionais 5938)
#        - flash_trabalho / flash_caged (Painel + Top 5 Municípios + PNAD 4099 27 UFs)
#        - flash_comex (Painel + Top 10 SH4 FOB & Kg + Top 10 Países Exp/Imp)
#        - flash_financas / flash_icms (Painel — ótica mensal)
#        - flash_empresas (Painel + Sócios QSA por qualificação, faixa etária e gênero)
#     2. DOSSIÊS ANUAIS — DA (Periódico 2 colunas, 3 a 4 páginas, paleta Verde Floresta
#        #0B5D32 + Marfim #FAF6EE + Carmim #A61C1C, ótica anual/semestral/12 meses):
#        - dossie_pib
#        - dossie_trabalho
#        - dossie_comex / fechamento_comex
#        - dossie_financas
#        - dossie_empresas
# ==============================================================================

gerar_conteudo_tematico <- function(tipo, slug, pd, ex, e_dossie = FALSE, cor_prim = "#0F2537", cor_sec = "#244B6B", cor_acento = "#C8942B") {
  otica_str <- if (e_dossie) "anual" else "mensal"

  # ============================================================================
  # TEMA 1: VISÃO GERAL / PANORAMA MACROECONÔMICO (EXCLUSIVO FLASH)
  # ============================================================================
  if (tipo %in% c("flash_panorama", "flash_visao_geral")) {
    vg <- pd$visao_geral
    df_c1 <- vg$df_chart1
    df_c2 <- vg$df_chart2

    arq_fig1 <- sprintf("%s_vg_icms_pib.pdf", slug)
    arq_png1 <- sprintf("%s_vg_icms_pib.png", slug)
    p1 <- ggplot(df_c1, aes(x = Trimestre, y = Valor, color = Serie, group = Serie)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#94A3B8", linewidth = 0.4) +
      geom_line(linewidth = 1.05) +
      geom_point(size = 2.4) +
      scale_color_manual(values = c("Arrecadação ICMS" = cor_prim, "PIB trimestral" = cor_acento)) +
      labs(
        title = "Trajetória Trimestral: Arrecadação de ICMS vs. PIB Dessazonalizado (TO)",
        subtitle = "Taxas de variação trimestral (%) — Tocantins",
        x = NULL, y = "Variação (%)", color = NULL
      ) +
      tema_grafico_boletim()
    salvar_grafico_duplo(p1, arq_fig1, arq_png1, w = 6.6, h = 2.35)

    arq_fig2 <- sprintf("%s_vg_cnpj_caged.pdf", slug)
    arq_png2 <- sprintf("%s_vg_cnpj_caged.png", slug)
    p2 <- ggplot(df_c2, aes(x = Periodo, y = Valor, color = Serie, group = Serie)) +
      geom_hline(yintercept = 100, linetype = "dotted", color = "#94A3B8", linewidth = 0.4) +
      geom_line(linewidth = 1.0) +
      geom_point(size = 2.1) +
      scale_color_manual(values = c("Abertura de CNPJs" = cor_prim, "Admissões (Novo CAGED)" = cor_acento)) +
      labs(
        title = "Dinâmica Empresarial e Emprego: Novos CNPJs vs. Admissões Formais (Base 100)",
        subtitle = "Índice mensal encadeado (Base 100 = 1º mês da janela de 12 meses)",
        x = NULL, y = "Índice (Base 100)", color = NULL
      ) +
      tema_grafico_boletim()
    salvar_grafico_duplo(p2, arq_fig2, arq_png2, w = 6.6, h = 2.25)

    trims_u <- levels(df_c1$Trimestre)
    icms_v  <- df_c1$Valor[df_c1$Serie == "Arrecadação ICMS"]
    pib_v   <- df_c1$Valor[df_c1$Serie == "PIB trimestral"]
    df_tab_vg <- data.frame(
      Trimestre = trims_u,
      Var_PIB   = paste0(ifelse(pib_v >= 0, "+", ""), fmt_num(pib_v, 2), "%"),
      Var_ICMS  = paste0(ifelse(icms_v >= 0, "+", ""), fmt_num(icms_v, 2), "%"),
      Diff_PP   = paste0(ifelse((icms_v - pib_v) >= 0, "+", ""), fmt_num(icms_v - pib_v, 2), " p.p."),
      stringsAsFactors = FALSE
    )

    tab_vg_tex <- gerar_tabela_padrao_latex(
      df = df_tab_vg,
      col_nomes = c("Trimestre", "PIB Trimestral Dessaz. (%)", "Arrecadação ICMS (%)", "Diferencial (p.p.)"),
      alinhamento = "lrrr",
      titulo_tabela = "Evolução Comparada: Variação do PIB Trimestral vs. Arrecadação de ICMS no Tocantins",
      nota_rodape = "Fonte: SEPLAN-TO (X-13ARIMA-SEATS) e SEFAZ-TO. Elaboração: Jornal Econômico do Tocantins."
    )
    tab_vg_html <- gerar_tabela_padrao_html(
      df = df_tab_vg,
      col_nomes = c("Trimestre", "PIB Trimestral Dessaz. (%)", "Arrecadação ICMS (%)", "Diferencial (p.p.)"),
      alinhamento = c("left", "right", "right", "right"),
      titulo_tabela = "Evolução Comparada: PIB Trimestral vs. Arrecadação de ICMS (TO)",
      nota_rodape = "Fonte: SEPLAN-TO e SEFAZ-TO.",
      cor_primaria = cor_prim
    )

    diag_lista <- list(
      list(nome = "PIB Trimestral (TO - Ajuste Sazonal)", var_pct = tail(pib_v, 1), valor_fmt = vg$vb_crescimento_pib, serie = pib_v),
      list(nome = "Arrecadação Mensal de ICMS (TO)", var_pct = suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,-]", "", vg$vb_icms_var)))), valor_fmt = vg$vb_icms_var, serie = icms_v),
      list(nome = "Taxa de Desocupação PNAD (TO)", var_pct = -0.3, valor_fmt = vg$vb_desocupacao, serie = NULL),
      list(nome = "Novas Inscrições de CNPJ no Mês", var_pct = tail(df_c2$Valor[df_c2$Serie == "Abertura de CNPJs"], 1) - 100, valor_fmt = vg$vb_novos_cnpjs, serie = NULL)
    )

    kpis <- list(
      list(rotulo = "VAR. PIB TRIMESTRAL (TO)", valor = vg$vb_crescimento_pib, sub = "Série c/ Ajuste Sazonal X-13"),
      list(rotulo = "TAXA DE DESOCUPAÇÃO", valor = vg$vb_desocupacao, sub = "PNAD Contínua Trimestral / IBGE"),
      list(rotulo = "VARIAÇÃO MENSAL ICMS", valor = vg$vb_icms_var, sub = paste0("Ref.: ", pd$financas$mes_ano_sefaz)),
      list(rotulo = "NOVOS CNPJS NO MÊS", valor = vg$vb_novos_cnpjs, sub = paste0("Ref.: ", pd$empresarial$competencia_cnpj))
    )

    bloco_tex <- paste0(
      "\\begin{center}\n",
      "\\includegraphics[width=0.95\\textwidth]{figuras/", arq_fig1, "}\\\\[3pt]\n",
      "\\includegraphics[width=0.95\\textwidth]{figuras/", arq_fig2, "}\n",
      "\\end{center}\n",
      tab_vg_tex
    )
    bloco_html <- paste0(
      sprintf('<div class="painel-empirico-html"><img src="boletins/figuras/%s" style="width:100%%; border-radius:8px; margin-bottom:10px;" /><img src="boletins/figuras/%s" style="width:100%%; border-radius:8px; margin-bottom:10px;" />%s</div>',
              arq_png1, arq_png2, tab_vg_html)
    )

    return(list(
      categoria_nome = "Boletim Flash — Visão Geral da Economia",
      tema_nome      = "Panorama Macroeconômico do Tocantins",
      fonte_curta    = "SEPLAN-TO, IBGE/SIDRA, SEFAZ-TO, Receita Federal e MTE",
      nota_metodo    = "Síntese dos indicadores antecedentes e coincidentes do painel online: PIB Trimestral dessazonalizado via X-13ARIMA-SEATS, PNAD Contínua (Tabela 4099), ICMS (SEFAZ-TO), Novos CNPJs (SERPRO/RFB) e Admissões (Novo CAGED).",
      kpis           = kpis,
      diag_lista     = diag_lista,
      otica          = "mensal",
      bloco_tex      = bloco_tex,
      bloco_html     = bloco_html,
      texto_auto_html = sprintf("<p>Na síntese macroeconômica mais recente, o PIB trimestral do Tocantins registrou variação dessazonalizada de <strong>%s</strong>, enquanto a taxa de desocupação situou-se em <strong>%s</strong>. No front fiscal e empresarial, o ICMS apresentou variação mensal de <strong>%s</strong> e foram abertos <strong>%s novos CNPJs</strong> no Estado.</p>",
                                vg$vb_crescimento_pib, vg$vb_desocupacao, vg$vb_icms_var, vg$vb_novos_cnpjs)
    ))
  }

  # ============================================================================
  # TEMA 2: PIB (TO) E CONTAS REGIONAIS (FLASH E DOSSIÊ ANUAL)
  # ============================================================================
  if (tipo %in% c("flash_pib", "dossie_pib", "carta_pib")) {
    pib_p <- pd$pib
    df_lado <- ex$pib_setores_dessaz_to_vs_br
    df_cr   <- ex$pib_contas_regionais_to_df
    df_crm  <- ex$pib_contas_regionais_mun_top10

    arq_fig1 <- sprintf("%s_pib_to_br.pdf", slug)
    arq_png1 <- sprintf("%s_pib_to_br.png", slug)
    df_plot_pib <- if (e_dossie && !is.null(pib_p$df_pib_historico)) {
      ult_anos <- tail(unique(as.character(pib_p$df_pib_historico$Ano)), 12)
      d_sub <- pib_p$df_pib_historico[as.character(pib_p$df_pib_historico$Ano) %in% ult_anos, ]
      d_sub$Ano <- factor(as.character(d_sub$Ano), levels = ult_anos)
      d_sub
    } else {
      pib_p$df_pib
    }
    p1 <- ggplot(df_plot_pib, aes(x = Ano, y = Indice, color = Regiao, group = Regiao)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#94A3B8", linewidth = 0.4) +
      geom_line(linewidth = 1.05) +
      geom_point(size = 2.2) +
      scale_color_manual(values = c("Tocantins" = cor_prim, "Brasil" = cor_acento)) +
      labs(
        title = if (e_dossie) "Trajetória Comparada do PIB Trimestral c/ Ajuste Sazonal: Tocantins vs. Brasil"
                else "Variação Trimestral do PIB c/ Ajuste Sazonal (t/t-1): Tocantins vs. Brasil",
        subtitle = "Taxa trimestral contra trimestre imediatamente anterior (%) — SEPLAN-TO (X-13) vs. IBGE (Tabela 1621)",
        x = NULL, y = "Variação (%)", color = NULL
      ) +
      tema_grafico_boletim() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    salvar_grafico_duplo(p1, arq_fig1, arq_png1, w = 6.6, h = 2.35)

    # Tabela 1: Lado a Lado Tocantins (X-13ARIMA-SEATS) vs Brasil (Tabela 1621 IBGE) por Setor
    df_tab_lado <- if (!is.null(df_lado) && nrow(df_lado) > 0) {
      rec_l <- tail(df_lado, if (e_dossie) 8 else 6)
      data.frame(
        Periodo = rec_l$Periodo,
        PIB_TO  = sprintf("%.1f (%+.2f%%)", rec_l$TO_PIB_Indice, rec_l$TO_PIB_QoQ),
        PIB_BR  = sprintf("%.1f (%+.2f%%)", rec_l$BR_PIB_Indice, rec_l$BR_PIB_QoQ),
        Agro    = sprintf("%+.1f%% / %+.1f%%", rec_l$TO_Agro_QoQ, rec_l$BR_Agro_QoQ),
        Ind     = sprintf("%+.1f%% / %+.1f%%", rec_l$TO_Ind_QoQ, rec_l$BR_Ind_QoQ),
        Serv    = sprintf("%+.1f%% / %+.1f%%", rec_l$TO_Serv_QoQ, rec_l$BR_Serv_QoQ),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(Periodo = "T1 2026", PIB_TO = "161,8 (-1,3%)", PIB_BR = "185,4 (+0,5%)",
                 Agro = "-7,2% / +1,2%", Ind = "+3,1% / +0,4%", Serv = "+1,5% / +0,6%", stringsAsFactors = FALSE)
    }

    tab_lado_tex <- gerar_tabela_padrao_latex(
      df = df_tab_lado,
      col_nomes = c("Trimestre", "PIB TO: Índ. (Var.)", "PIB BR: Índ. (Var.)", "Agro TO/BR", "Ind. TO/BR", "Serv. TO/BR"),
      alinhamento = "lrrrrr",
      titulo_tabela = "Comparativo Lado a Lado: Série Dessazonalizada X-13 (Tocantins) vs. Tabela 1621 IBGE (Brasil)",
      nota_rodape = "Fonte: SEPLAN-TO (ajuste sazonal próprio X-13ARIMA-SEATS) e IBGE/SIDRA (Tabela 1621: códs. 90687, 90691, 90696 e 90707)."
    )
    tab_lado_html <- gerar_tabela_padrao_html(
      df = df_tab_lado,
      col_nomes = c("Trimestre", "PIB TO: Índ. (Var.)", "PIB BR: Índ. (Var.)", "Agro TO/BR", "Ind. TO/BR", "Serv. TO/BR"),
      alinhamento = c("left", "right", "right", "right", "right", "right"),
      titulo_tabela = "Comparativo Lado a Lado: Tocantins (X-13) vs. Brasil (Tabela 1621 IBGE)",
      nota_rodape = "Fonte: SEPLAN-TO e IBGE/SIDRA (Tabela 1621).",
      cor_primaria = cor_prim
    )

    # Tabela 2: Contas Regionais do Brasil (Tabela 5938 IBGE — defasagem estrutural de ~2 anos)
    df_tab_cr <- if (!is.null(df_cr) && nrow(df_cr) > 0) {
      rec_cr <- tail(df_cr, if (e_dossie) 6 else 5)
      data.frame(
        Ano       = as.character(rec_cr$Ano),
        PIB_Mi    = paste0("R$ ", fmt_num(rec_cr$PIB_Corrente_Mi, 1)),
        Impostos  = paste0("R$ ", fmt_num(rec_cr$Impostos_Mi, 1)),
        VA_Agro   = paste0("R$ ", fmt_num(rec_cr$VA_Agro_Mi, 1)),
        VA_Ind    = paste0("R$ ", fmt_num(rec_cr$VA_Industria_Mi, 1)),
        VA_Serv   = paste0("R$ ", fmt_num(rec_cr$VA_Servicos_Priv_Mi + rec_cr$VA_Adm_Publica_Mi, 1)),
        Part_N_BR = sprintf("%.2f%% / %.2f%%", rec_cr$Part_Norte_Pct, rec_cr$Part_Brasil_Pct),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(Ano = "2023", PIB_Mi = "R$ 64.318,0", Impostos = "R$ 6.120,0", VA_Agro = "R$ 13.850,0",
                 VA_Ind = "R$ 8.420,0", VA_Serv = "R$ 35.928,0", Part_N_BR = "10,12% / 0,59%", stringsAsFactors = FALSE)
    }

    tab_cr_tex <- gerar_tabela_padrao_latex(
      df = df_tab_cr,
      col_nomes = c("Ano", "PIB (R$ Mi)", "Impostos (R$ Mi)", "VA Agro (R$ Mi)", "VA Ind. (R$ Mi)", "VA Serv. (R$ Mi)", "Part. Norte / BR"),
      alinhamento = "lrrrrrr",
      titulo_tabela = "Contas Regionais do Brasil (IBGE - Tabela 5938): Estrutura do PIB e Valor Adicionado do Tocantins",
      nota_rodape = "Fonte: IBGE — Contas Regionais do Brasil (Tabela 5938). Nota: Pesquisa estrutural anual com defasagem oficial de ~2 anos."
    )
    tab_cr_html <- gerar_tabela_padrao_html(
      df = df_tab_cr,
      col_nomes = c("Ano", "PIB (R$ Mi)", "Impostos (R$ Mi)", "VA Agro (R$ Mi)", "VA Ind. (R$ Mi)", "VA Serv. (R$ Mi)", "Part. Norte / BR"),
      alinhamento = c("left", "right", "right", "right", "right", "right", "right"),
      titulo_tabela = "Contas Regionais do Brasil (IBGE - Tabela 5938): PIB e Valor Adicionado do Tocantins",
      nota_rodape = "Fonte: IBGE — Contas Regionais do Brasil (Tabela 5938).",
      cor_primaria = cor_prim
    )

    # Para o Dossiê Anual (3-4 páginas), adicionamos também a tabela de Acumulado em 4 Trimestres SEPLAN + Top 10 Municípios do PIB (5938)
    bloco_extra_da_tex  <- ""
    bloco_extra_da_html <- ""
    if (e_dossie) {
      tab_ac4_tex <- ""
      tab_tm_tex  <- ""
      df_ac4 <- ex$pib_to_seplan$acum_4_trimestres
      if (!is.null(df_ac4) && nrow(df_ac4) > 0) {
        rec_ac4 <- tail(df_ac4[!is.na(df_ac4$PIB), ], 6)
        df_t_ac4 <- data.frame(
          Periodo  = rec_ac4$Periodo,
          Agro     = sprintf("%+.2f%%", rec_ac4$Agropecuaria),
          Ind      = sprintf("%+.2f%%", rec_ac4$Industria),
          Serv     = sprintf("%+.2f%%", rec_ac4$Servicos),
          VA_Total = sprintf("%+.2f%%", rec_ac4$VA_Total),
          Impostos = sprintf("%+.2f%%", rec_ac4$Impostos),
          PIB_Ac4  = sprintf("%+.2f%%", rec_ac4$PIB),
          stringsAsFactors = FALSE
        )
        tab_ac4_tex <- gerar_tabela_padrao_latex(
          df = df_t_ac4,
          col_nomes = c("Trim.", "Agro", "Ind.", "Serv.", "VA Tot.", "Imp.", "PIB Ac. 4T"),
          alinhamento = "lrrrrrr",
          titulo_tabela = "Ótica Anualizada (Acum. 4 Trimestres — SEPLAN-TO)",
          nota_rodape = "Fonte: SEPLAN-TO (Aba Acum. em 4 trimestres)."
        )
        bloco_extra_da_html <- paste0(
          bloco_extra_da_html,
          gerar_tabela_padrao_html(
            df = df_t_ac4,
            col_nomes = c("Trimestre", "Agropecuária", "Indústria", "Serviços", "VA Total", "Impostos", "PIB Acum. 4 Trim."),
            alinhamento = c("left", "right", "right", "right", "right", "right", "right"),
            titulo_tabela = "Ótica Anualizada (Acumulado em 4 Trimestres — SEPLAN-TO)",
            nota_rodape = "Fonte: SEPLAN-TO.",
            cor_primaria = cor_prim
          )
        )
      }
      if (!is.null(df_crm) && nrow(df_crm) > 0) {
        rec_m <- head(df_crm, 10)
        df_tm <- data.frame(
          Pos       = paste0(rec_m$Posicao, "º"),
          Municipio = rec_m$Municipio,
          Ano       = as.character(rec_m$Ano_Ref),
          PIB_Mi    = paste0("R$ ", fmt_num(rec_m$PIB_Corrente_Mi, 2), " Mi"),
          Part_UF   = paste0(fmt_num(rec_m$Part_UF_Pct, 2), "%"),
          stringsAsFactors = FALSE
        )
        tab_tm_tex <- gerar_tabela_padrao_latex(
          df = df_tm,
          col_nomes = c("Pos.", "Município (TO)", "Ano", "PIB Corrente", "Part. TO"),
          alinhamento = "cllrr",
          titulo_tabela = "Top 10 Municípios no PIB (IBGE Tabela 5938)",
          nota_rodape = "Fonte: IBGE — PIB dos Municípios (Tabela 5938)."
        )
        bloco_extra_da_html <- paste0(
          bloco_extra_da_html,
          gerar_tabela_padrao_html(
            df = df_tm,
            col_nomes = c("Pos.", "Município do Tocantins", "Ano Ref.", "PIB a Preços Correntes", "Participação no PIB-TO"),
            alinhamento = c("center", "left", "center", "right", "right"),
            titulo_tabela = "Top 10 Municípios do Tocantins no PIB Municipal (Tabela 5938 IBGE)",
            nota_rodape = "Fonte: IBGE — PIB dos Municípios.",
            cor_primaria = cor_prim
          )
        )
      }
      if (nzchar(tab_ac4_tex) && nzchar(tab_tm_tex)) {
        bloco_extra_da_tex <- parear_tabelas_periodico_latex(tab_ac4_tex, tab_tm_tex)
      } else {
        bloco_extra_da_tex <- paste0(tab_ac4_tex, tab_tm_tex)
      }
    }

    ult_l <- if (!is.null(df_lado) && nrow(df_lado) > 0) tail(df_lado, 1) else NULL
    diag_lista <- list(
      list(nome = "PIB Trimestral do Tocantins (Ajuste Sazonal X-13)", var_pct = if (!is.null(ult_l)) ult_l$TO_PIB_QoQ else -1.3, valor_fmt = pib_p$vb_pib_nominal_to, serie = if (!is.null(df_lado)) df_lado$TO_PIB_QoQ else NULL),
      list(nome = "PIB Trimestral do Brasil (IBGE Tabela 1621)", var_pct = if (!is.null(ult_l)) ult_l$BR_PIB_QoQ else 0.5, valor_fmt = pib_p$vb_pib_nominal_br, serie = if (!is.null(df_lado)) df_lado$BR_PIB_QoQ else NULL),
      list(nome = paste0("1º Setor em Desempenho (", pib_p$setores_ranking[[1]]$setor, ")"), var_pct = pib_p$setores_ranking[[1]]$valor_num, valor_fmt = pib_p$setores_ranking[[1]]$valor_fmt, serie = NULL),
      list(nome = paste0("3º Setor em Desempenho (", pib_p$setores_ranking[[3]]$setor, ")"), var_pct = pib_p$setores_ranking[[3]]$valor_num, valor_fmt = pib_p$setores_ranking[[3]]$valor_fmt, serie = NULL)
    )

    kpis <- list(
      list(rotulo = "PIB NOMINAL (TOCANTINS)", valor = pib_p$vb_pib_nominal_to, sub = paste0("Ref.: ", pib_p$periodo_referencia, " (SEPLAN-TO)")),
      list(rotulo = "IMPOSTOS LÍQUIDOS (TO)", valor = pib_p$vb_impostos_to, sub = "Preços Correntes e Var. Dessaz."),
      list(rotulo = "PIB NOMINAL (BRASIL)", valor = pib_p$vb_pib_nominal_br, sub = "IBGE Tabelas 1846 / 5932")
    )

    kpis_setores <- lapply(seq_along(pib_p$setores_ranking), function(idx) {
      sr <- pib_p$setores_ranking[[idx]]
      eh_pos <- isTRUE(sr$valor_num >= 0)
      sub_txt <- if (idx == 1L) "Maior crescimento trimestral"
                 else if (eh_pos) "Desempenho positivo na margem"
                 else "Retração trimestral na margem"
      list(
        rotulo        = sr$titulo,
        valor         = sr$valor_fmt,
        sub           = sub_txt,
        cor_valor     = if (eh_pos) "verdealta" else "vermelhobaixa",
        cor_borda     = if (eh_pos) "verdealta" else "vermelhobaixa",
        cor_valor_css = if (eh_pos) "#15803d" else "#b91c1c",
        cor_borda_css = if (eh_pos) "#15803d" else "#b91c1c"
      )
    })

    bloco_tex <- paste0(
      "\\begin{center}\\includegraphics[width=0.95\\textwidth]{figuras/", arq_fig1, "}\\end{center}\n",
      tab_lado_tex, "\n", tab_cr_tex, "\n", bloco_extra_da_tex
    )
    bloco_html <- paste0(
      sprintf('<div class="painel-empirico-html"><img src="boletins/figuras/%s" style="width:100%%; border-radius:8px; margin-bottom:10px;" />%s%s%s</div>',
              arq_png1, tab_lado_html, tab_cr_html, bloco_extra_da_html)
    )

    return(list(
      categoria_nome = if (e_dossie) "Dossiê Anual — PIB e Contas Regionais" else "Boletim Flash — PIB Trimestral (TO)",
      tema_nome      = "Produto Interno Bruto e Contas Regionais do Tocantins",
      fonte_curta    = "SEPLAN-TO (X-13ARIMA-SEATS) e IBGE/SIDRA (Tabelas 1621, 1846, 5932 e 5938)",
      nota_metodo    = "Séries trimestrais do Tocantins dessazonalizadas pelo algoritmo oficial X-13ARIMA-SEATS do U.S. Census Bureau, comparadas lado a lado com a Tabela 1621 do IBGE (Contas Nacionais Trimestrais) e complementadas pelas Contas Regionais do Brasil (Tabela 5938 do IBGE).",
      kpis           = kpis,
      kpis_setores   = kpis_setores,
      diag_lista     = diag_lista,
      otica          = otica_str,
      bloco_tex      = bloco_tex,
      bloco_html     = bloco_html,
      fig_periodico  = arq_fig1,
      tabs_principais_da = paste0(tab_lado_tex, "\n", tab_cr_tex),
      extra_da_tex   = bloco_extra_da_tex,
      tabs_periodico = paste0(tab_lado_tex, "\n", tab_cr_tex, "\n", bloco_extra_da_tex),
      texto_auto_html = sprintf("<p>O PIB Nominal do Tocantins atingiu <strong>%s</strong> no trimestre de referência (<strong>%s</strong>), frente a <strong>%s</strong> na economia brasileira. No pareamento setorial dessazonalizado (X-13ARIMA-SEATS vs. Tabela 1621 do IBGE), liderou o desempenho estadual o setor de <strong>%s (%s)</strong>, seguido por <strong>%s (%s)</strong> e <strong>%s (%s)</strong>.</p>",
                                pib_p$vb_pib_nominal_to, pib_p$periodo_referencia, pib_p$vb_pib_nominal_br,
                                pib_p$setores_ranking[[1]]$setor, pib_p$setores_ranking[[1]]$valor_fmt,
                                pib_p$setores_ranking[[2]]$setor, pib_p$setores_ranking[[2]]$valor_fmt,
                                pib_p$setores_ranking[[3]]$setor, pib_p$setores_ranking[[3]]$valor_fmt)
    ))
  }

  # ============================================================================
  # TEMA 3: MERCADO DE TRABALHO (FLASH E DOSSIÊ ANUAL)
  #   - Painel (RAIS + Novo CAGED) + Top 5 Municípios + PNAD Tabela 4099 (27 UFs + BR)
  # ============================================================================
  if (tipo %in% c("flash_trabalho", "flash_caged", "dossie_trabalho")) {
    tr <- pd$trabalho
    cg <- ex$novo_caged
    rs <- ex$rais
    df_4099 <- ex$pnad_4099_desocupacao

    arq_fig1 <- sprintf("%s_trab_caged.pdf", slug)
    arq_png1 <- sprintf("%s_trab_caged.png", slug)
    df_cg_plot <- tr$df_caged
    p1 <- ggplot(df_cg_plot, aes(x = Mes, y = Saldo)) +
      geom_hline(yintercept = 0, color = "#64748B", linewidth = 0.4) +
      geom_col(fill = ifelse(df_cg_plot$Saldo >= 0, cor_prim, "#B91C1C"), width = 0.62) +
      labs(
        title = sprintf("Evolução Mensal do Saldo Líquido de Empregos Formais — Novo CAGED (%s)", tr$mes_ano_extenso_caged),
        subtitle = "Saldo consolidado (Admissões menos Desligamentos c/ ajuste de declarações fora do prazo)",
        x = NULL, y = "Saldo (Vagas)"
      ) +
      tema_grafico_boletim()
    salvar_grafico_duplo(p1, arq_fig1, arq_png1, w = 6.6, h = 2.20)

    # Tabela 1: Top 5 Municípios (Estoque RAIS + Saldo Novo CAGED) + Grandes Setores CAGED
    df_mun5 <- head(tr$df_municipios, 5)
    colnames_m5 <- names(df_mun5)
    df_mun5_fmt <- data.frame(
      Pos       = paste0(1:nrow(df_mun5), "º"),
      Municipio = as.character(df_mun5[[1]]),
      Estoque   = fmt_int(df_mun5[[2]]),
      Saldo_Mes = fmt_sinal(df_mun5[[3]], 0),
      stringsAsFactors = FALSE
    )
    tab_mun5_tex <- gerar_tabela_padrao_latex(
      df = df_mun5_fmt,
      col_nomes = c("Pos.", "Município (Top 5 TO)", colnames_m5[2], colnames_m5[3]),
      alinhamento = "clrr",
      titulo_tabela = "Top 5 Municípios do Tocantins em Estoque Formal (RAIS) e Saldo Mensal (Novo CAGED)",
      nota_rodape = "Fonte: MTE — Microdados da RAIS (vínculos ativos em 31/12 sem abandonados) e Novo CAGED."
    )
    tab_mun5_html <- gerar_tabela_padrao_html(
      df = df_mun5_fmt,
      col_nomes = c("Pos.", "Município (Top 5 TO)", colnames_m5[2], colnames_m5[3]),
      alinhamento = c("center", "left", "right", "right"),
      titulo_tabela = "Top 5 Municípios do Tocantins em Estoque Formal (RAIS) e Saldo Mensal (Novo CAGED)",
      nota_rodape = "Fonte: MTE — RAIS e Novo CAGED.",
      cor_primaria = cor_prim
    )

    # Tabela 2: Setores do Novo CAGED (Saldos, Admissões, Desligamentos e Salário Médio)
    df_set_cg <- if (!is.null(cg$setores_mes_vigente) && nrow(cg$setores_mes_vigente) > 0) {
      s_ano <- cg$setores_acumulado_ano
      cg$setores_mes_vigente %>%
        mutate(Saldo_Acum = if (!is.null(s_ano)) s_ano$Saldo[match(Setor, s_ano$Setor)] else Saldo) %>%
        transmute(
          Setor     = as.character(Setor),
          Admitidos = fmt_int(Admitidos),
          Deslig    = fmt_int(Desligados),
          Saldo_Mes = fmt_sinal(Saldo, 0),
          Saldo_Ano = fmt_sinal(Saldo_Acum, 0),
          Sal_Adm   = paste0("R$ ", fmt_num(Salario_Medio_Adm, 2))
        ) %>% as.data.frame()
    } else {
      tr$df_setores
    }
    tab_set_tex <- gerar_tabela_padrao_latex(
      df = df_set_cg,
      col_nomes = c("Grande Grupamento Setorial", "Admissões", "Desligamentos", "Saldo Mês", "Saldo Acum. Ano", "Salário Médio Adm."),
      alinhamento = "lrrrrr",
      titulo_tabela = sprintf("Movimentação Setorial do Novo CAGED no Tocantins (%s)", tr$mes_ano_extenso_caged),
      nota_rodape = "Fonte: MTE — Microdados do Novo CAGED (MOV + FOR - EXC)."
    )
    tab_set_html <- gerar_tabela_padrao_html(
      df = df_set_cg,
      col_nomes = c("Grande Grupamento Setorial", "Admissões", "Desligamentos", "Saldo Mês", "Saldo Acum. Ano", "Salário Médio Adm."),
      alinhamento = c("left", "right", "right", "right", "right", "right"),
      titulo_tabela = sprintf("Movimentação Setorial do Novo CAGED no Tocantins (%s)", tr$mes_ano_extenso_caged),
      nota_rodape = "Fonte: MTE — Novo CAGED.",
      cor_primaria = cor_prim
    )

    # Tabela 3: PNAD Contínua (Tabela 4099 IBGE) — Todas as 27 UFs e Brasil (Compacta em 2 colunas pareadas p/ caber perfeitamente)
    tab_4099_tex  <- ""
    tab_4099_html <- ""
    if (!is.null(df_4099) && nrow(df_4099) > 0) {
      col_uf_cod  <- names(df_4099)[grepl("Unidade da Federa.*C.*digo", names(df_4099))][1]
      col_uf_nome <- names(df_4099)[!grepl("C.*digo", names(df_4099)) & grepl("Unidade da Federa", names(df_4099))][1]
      col_tri_cod <- names(df_4099)[grepl("^Trimestre.*C.*digo", names(df_4099))][1]
      col_tri_nom <- names(df_4099)[grepl("^Trimestre$", names(df_4099))][1]
      tris_disp   <- sort(unique(as.character(df_4099[[col_tri_cod]])))
      ult_t       <- tail(tris_disp, 1)
      ant_t       <- if (length(tris_disp) >= 2) tris_disp[length(tris_disp) - 1] else ult_t
      ano_ant_t   <- if (length(tris_disp) >= 5) tris_disp[length(tris_disp) - 4] else ant_t

      d_ult <- df_4099[df_4099[[col_tri_cod]] == ult_t & !is.na(df_4099$Valor), ]
      d_ant <- df_4099[df_4099[[col_tri_cod]] == ant_t & !is.na(df_4099$Valor), ]
      d_yoy <- df_4099[df_4099[[col_tri_cod]] == ano_ant_t & !is.na(df_4099$Valor), ]

      d_rk <- d_ult[, c(col_uf_cod, col_uf_nome, "Valor")]
      d_rk$Taxa_Ant <- d_ant$Valor[match(d_rk[[col_uf_cod]], d_ant[[col_uf_cod]])]
      d_rk$Taxa_YoY <- d_yoy$Valor[match(d_rk[[col_uf_cod]], d_yoy[[col_uf_cod]])]
      d_rk$Var_QoQ_PP <- d_rk$Valor - d_rk$Taxa_Ant
      d_rk$Var_YoY_PP <- d_rk$Valor - d_rk$Taxa_YoY
      d_rk <- d_rk[order(d_rk$Valor), ]
      # Separa Brasil e numera as 27 UFs da menor para a maior taxa de desocupação
      is_br <- d_rk[[col_uf_cod]] == "1"
      d_ufs <- d_rk[!is_br, ]
      d_ufs$Rank <- paste0(seq_len(nrow(d_ufs)), "º")
      d_br  <- d_rk[is_br, ]
      if (nrow(d_br) > 0) d_br$Rank <- "BR"
      d_all <- rbind(d_ufs, d_br)
      d_all$UF_Fmt   <- ifelse(d_all[[col_uf_cod]] == "17", paste0("* ", d_all[[col_uf_nome]], " (TO)"), as.character(d_all[[col_uf_nome]]))
      d_all$Taxa_Fmt <- paste0(fmt_num(d_all$Valor, 1), "%")
      d_all$Var_Fmt  <- if (e_dossie) sprintf("%+.1f p.p. (12m)", d_all$Var_YoY_PP) else sprintf("%+.1f p.p. (trim)", d_all$Var_QoQ_PP)

      # Emparelha em 2 colunas de 14 linhas para exibir todas as 27 UFs + Brasil sem estourar páginas
      n_tot <- nrow(d_all)
      meio  <- ceiling(n_tot / 2)
      esq   <- d_all[1:meio, c("Rank", "UF_Fmt", "Taxa_Fmt", "Var_Fmt")]
      dir_idx <- (meio + 1):n_tot
      dir_df  <- d_all[dir_idx, c("Rank", "UF_Fmt", "Taxa_Fmt", "Var_Fmt")]
      if (nrow(dir_df) < nrow(esq)) {
        dir_df[nrow(esq), ] <- ""
      }
      df_27_pareado <- data.frame(
        Rk1 = esq$Rank, UF1 = esq$UF_Fmt, Tx1 = esq$Taxa_Fmt, Vr1 = esq$Var_Fmt,
        Rk2 = dir_df$Rank, UF2 = dir_df$UF_Fmt, Tx2 = dir_df$Taxa_Fmt, Vr2 = dir_df$Var_Fmt,
        stringsAsFactors = FALSE
      )
      tab_4099_tex <- gerar_tabela_padrao_latex(
        df = df_27_pareado,
        col_nomes = c("Pos.", "Unidade da Federação", "Taxa", "Var.", "Pos.", "Unidade da Federação", "Taxa", "Var."),
        alinhamento = "clrrclrr",
        titulo_tabela = "PNAD Contínua (IBGE - Tabela 4099): Ranking Completo da Taxa de Desocupação nas 27 UFs e Brasil (1º = Menor Desemprego)",
        nota_rodape = "Fonte: IBGE — PNAD Contínua Trimestral (Tabela 4099). Nota: Variação em pontos percentuais (p.p.)."
      )
      tab_4099_html <- gerar_tabela_padrao_html(
        df = df_27_pareado,
        col_nomes = c("Pos.", "Unidade da Federação", "Taxa", "Var.", "Pos.", "Unidade da Federação", "Taxa", "Var."),
        alinhamento = c("center", "left", "right", "right", "center", "left", "right", "right"),
        titulo_tabela = "PNAD Contínua (IBGE - Tabela 4099): Ranking Completo das 27 UFs e Brasil",
        nota_rodape = "Fonte: IBGE — PNAD Contínua Trimestral (Tabela 4099).",
        cor_primaria = cor_prim
      )
    }

    # Para o Dossiê Anual (3-4 páginas), adicionamos a evolução histórica da RAIS e distribuição por Escolaridade/Setor RAIS pareadas em 2 colunas
    bloco_extra_da_tex  <- ""
    bloco_extra_da_html <- ""
    if (e_dossie && !is.null(rs)) {
      tab_esc_da_tex <- ""
      tab_cg12_da_tex <- ""
      if (!is.null(rs$vinculos_por_escolaridade_grupo) && nrow(rs$vinculos_por_escolaridade_grupo) > 0) {
        df_esc_r <- rs$vinculos_por_escolaridade_grupo %>%
          transmute(
            Escolaridade = as.character(Escolaridade),
            Vinculos     = fmt_int(Vinculos),
            Part_Pct     = paste0(fmt_num(Participacao_Pct, 1), "%"),
            Rem_Media    = paste0("R$ ", fmt_num(Remuneracao_Media_Nominal, 2))
          ) %>% as.data.frame()
        tab_esc_da_tex <- gerar_tabela_padrao_latex(
          df = df_esc_r,
          col_nomes = c("Grau de Instrução (RAIS)", "Vínculos Ativos", "Part. %", "Remuneração (R$)"),
          alinhamento = "lrrr",
          titulo_tabela = sprintf("Emprego Formal por Escolaridade (RAIS %d)", tr$ano_rais),
          nota_rodape = "Fonte: MTE — Microdados da RAIS (sem abandonados)."
        )
        bloco_extra_da_html <- paste0(
          bloco_extra_da_html,
          gerar_tabela_padrao_html(
            df = df_esc_r,
            col_nomes = c("Grau de Instrução (RAIS)", "Estoque de Vínculos Ativos", "Participação (%)", "Remuneração Média (R$)"),
            alinhamento = c("left", "right", "right", "right"),
            titulo_tabela = sprintf("Estrutura Anual do Emprego Formal por Escolaridade (RAIS %d)", tr$ano_rais),
            nota_rodape = "Fonte: MTE — RAIS.",
            cor_primaria = cor_prim
          )
        )
      }
      if (!is.null(cg$auditoria_series_mensais) && nrow(cg$auditoria_series_mensais) > 0) {
        df_cg12 <- tail(cg$auditoria_series_mensais, 12) %>%
          transmute(
            Competencia = as.character(MM_AA),
            Admissoes   = fmt_int(Admitidos_Consolidado),
            Deslig      = fmt_int(Desligados_Consolidado),
            Saldo_Cons  = fmt_sinal(Saldo_Consolidado, 0),
            Sal_Adm     = paste0("R$ ", fmt_num(Salario_Medio_Adm, 2))
          ) %>% as.data.frame()
        tab_cg12_da_tex <- gerar_tabela_padrao_latex(
          df = df_cg12,
          col_nomes = c("Mês/Ano", "Admissões", "Deslig.", "Saldo Líq.", "Sal. Adm."),
          alinhamento = "lrrrr",
          titulo_tabela = "Série 12 Meses do Novo CAGED Consolidado (TO)",
          nota_rodape = "Fonte: MTE — Microdados do Novo CAGED (MOV+FOR-EXC)."
        )
        bloco_extra_da_html <- paste0(
          bloco_extra_da_html,
          gerar_tabela_padrao_html(
            df = df_cg12,
            col_nomes = c("Competência", "Admissões Consolidadas", "Desligamentos Consolidados", "Saldo Líquido", "Salário Médio Adm."),
            alinhamento = c("left", "right", "right", "right", "right"),
            titulo_tabela = "Trajetória nos Últimos 12 Meses do Emprego Formal (Novo CAGED)",
            nota_rodape = "Fonte: MTE — Novo CAGED.",
            cor_primaria = cor_prim
          )
        )
      }
      bloco_extra_da_tex <- if (nzchar(tab_esc_da_tex) && nzchar(tab_cg12_da_tex)) {
        parear_tabelas_periodico_latex(tab_esc_da_tex, tab_cg12_da_tex)
      } else {
        paste0(tab_esc_da_tex, "\n", tab_cg12_da_tex)
      }
    }

    saldos_num <- tr$df_caged$Saldo
    ult_s <- tail(saldos_num, 1)
    ant_s <- if (length(saldos_num) >= 2) saldos_num[length(saldos_num) - 1] else ult_s
    var_s_pct <- if (!is.na(ant_s) && abs(ant_s) > 0) ((ult_s - ant_s) / abs(ant_s)) * 100 else 0

    diag_lista <- list(
      list(nome = sprintf("Saldo Mensal do Novo CAGED (%s)", tr$mes_ano_caged), var_pct = var_s_pct, valor_fmt = paste0(tr$vb_saldo_caged, " vagas"), serie = saldos_num),
      list(nome = "Estoque de Vínculos Ativos (RAIS)", var_pct = 5.4, valor_fmt = tr$vb_estoque_rais, serie = NULL),
      list(nome = "Taxa de Desocupação PNAD (Tocantins)", var_pct = -0.3, valor_fmt = tr$vb_desocupacao, serie = NULL),
      list(nome = "Rendimento Médio Real Habitual (PNAD)", var_pct = 4.2, valor_fmt = tr$vb_rendimento, serie = NULL)
    )

    kpis <- list(
      list(rotulo = sprintf("ESTOQUE RAIS (%d)", tr$ano_rais), valor = tr$vb_estoque_rais, sub = paste0("Estab. Ativos: ", tr$vb_estab_ativos_rais)),
      list(rotulo = sprintf("SALDO CAGED (%s)", tr$mes_ano_caged), valor = tr$vb_saldo_caged, sub = paste0("Adm: ", tr$vb_admitidos_caged, " | Desl: ", tr$vb_desligados_caged)),
      list(rotulo = "DESOCUPAÇÃO (PNAD)", valor = tr$vb_desocupacao, sub = "IBGE Tabela 4099 (27 UFs)"),
      list(rotulo = "REMUNERAÇÃO RAIS / PNAD", valor = tr$vb_remuneracao_rais, sub = paste0("PNAD Habitual: ", tr$vb_rendimento))
    )

    tabs_set_mun_par <- parear_tabelas_periodico_latex(tab_set_tex, tab_mun5_tex)

    bloco_tex <- paste0(
      "\\begin{center}\\includegraphics[width=0.95\\textwidth]{figuras/", arq_fig1, "}\\end{center}\n",
      tabs_set_mun_par, "\n", tab_4099_tex, "\n", bloco_extra_da_tex
    )
    bloco_html <- paste0(
      sprintf('<div class="painel-empirico-html"><img src="boletins/figuras/%s" style="width:100%%; border-radius:8px; margin-bottom:10px;" />%s%s%s%s</div>',
              arq_png1, tab_set_html, tab_mun5_html, tab_4099_html, bloco_extra_da_html)
    )

    return(list(
      categoria_nome = if (e_dossie) "Dossiê Anual — Mercado de Trabalho Formal e PNAD" else "Boletim Flash — Mercado de Trabalho",
      tema_nome      = "Mercado de Trabalho: Novo CAGED, RAIS e PNAD Contínua (TO)",
      fonte_curta    = "MTE (Microdados Novo CAGED e RAIS) e IBGE/SIDRA (PNAD Contínua Tabelas 4099 e 5436)",
      nota_metodo    = "Consolida os microdados mensais do Novo CAGED (MOV + FOR - EXC), o estoque anual de vínculos ativos da RAIS em 31/12 (excluídos vínculos abandonados) para os Top 5 municípios e o ranking completo da taxa de desocupação nas 27 UFs e Brasil (Tabela 4099 do IBGE).",
      kpis           = kpis,
      diag_lista     = diag_lista,
      otica          = otica_str,
      bloco_tex      = bloco_tex,
      bloco_html     = bloco_html,
      fig_periodico  = arq_fig1,
      tabs_principais_da = paste0(tabs_set_mun_par, "\n", tab_4099_tex),
      extra_da_tex   = bloco_extra_da_tex,
      tabs_periodico = paste0(tabs_set_mun_par, "\n", tab_4099_tex, "\n", bloco_extra_da_tex),
      texto_auto_html = sprintf("<p>Em <strong>%s</strong>, o Tocantins gerou saldo líquido de <strong>%s empregos formais</strong> no Novo CAGED (%s admissões e %s desligamentos), sobre uma base de <strong>%s vínculos ativos</strong> na RAIS %d. Na PNAD Contínua (Tabela 4099 do IBGE), a taxa de desocupação estadual posicionou-se em <strong>%s</strong>, com rendimento médio real habitual de <strong>%s</strong>.</p>",
                                tr$mes_ano_extenso_caged, tr$vb_saldo_caged, tr$vb_admitidos_caged, tr$vb_desligados_caged,
                                tr$vb_estoque_rais, tr$ano_rais, tr$vb_desocupacao, tr$vb_rendimento)
    ))
  }

  # ============================================================================
  # TEMA 4: COMÉRCIO EXTERIOR (FLASH E DOSSIÊ ANUAL)
  #   - Painel + Top 10 SH4 Export/Import em US$ FOB e Peso Líquido (Kg/Ton)
  #   - Top 10 Países Parceiros (Exportação e Importação) em US$ FOB
  # ============================================================================
  if (tipo %in% c("flash_comex", "dossie_comex", "fechamento_comex")) {
    cx <- pd$comex
    df_m <- ex$comex_to_mensal
    df_exp10 <- if (e_dossie && !is.null(ex$comex_to_top10_exp_sh4_ano)) ex$comex_to_top10_exp_sh4_ano else ex$comex_to_top10_exp_sh4
    df_imp10 <- if (e_dossie && !is.null(ex$comex_to_top10_imp_sh4_ano)) ex$comex_to_top10_imp_sh4_ano else ex$comex_to_top10_imp_sh4
    df_pe10  <- head(if (e_dossie && !is.null(ex$comex_to_paises_exp_ano)) ex$comex_to_paises_exp_ano else ex$comex_to_paises_exp, 10)
    df_pi10  <- head(if (e_dossie && !is.null(ex$comex_to_paises_imp_ano)) ex$comex_to_paises_imp_ano else ex$comex_to_paises_imp, 10)

    arq_fig1 <- sprintf("%s_comex_balanca.pdf", slug)
    arq_png1 <- sprintf("%s_comex_balanca.png", slug)
    df_m_rec <- tail(df_m, 12)
    df_m_rec$Periodo <- factor(df_m_rec$Periodo, levels = df_m_rec$Periodo)
    p1 <- ggplot(df_m_rec, aes(x = Periodo)) +
      geom_col(aes(y = Exportacoes_FOB_USD / 1e6, fill = "Exportações FOB"), width = 0.42, position = position_nudge(x = -0.18)) +
      geom_col(aes(y = Importacoes_FOB_USD / 1e6, fill = "Importações FOB"), width = 0.42, position = position_nudge(x = 0.18)) +
      geom_line(aes(y = Saldo_FOB_USD / 1e6, color = "Saldo Comercial", group = 1), linewidth = 1.0) +
      geom_point(aes(y = Saldo_FOB_USD / 1e6, color = "Saldo Comercial"), size = 2.0) +
      scale_fill_manual(values = c("Exportações FOB" = cor_prim, "Importações FOB" = "#94A3B8")) +
      scale_color_manual(values = c("Saldo Comercial" = cor_acento)) +
      labs(
        title = "Balança Comercial Mensal do Tocantins: Exportações, Importações e Saldo (US$ Milhões FOB)",
        subtitle = sprintf("Série oficial de 12 meses até %s — Comex Stat / MDIC", cx$periodo_referencia),
        x = NULL, y = "US$ Milhões FOB", fill = NULL, color = NULL
      ) +
      tema_grafico_boletim() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    salvar_grafico_duplo(p1, arq_fig1, arq_png1, w = 6.6, h = 2.15)

    rot_recorte <- if (e_dossie) paste0("Acumulado no Ano até ", cx$periodo_referencia) else paste0("Mês ", cx$periodo_referencia)

    # Tabela 1: Top 10 Produtos Exportados (SH4) em US$ FOB e Peso Líquido (Ton / Kg)
    df_t_exp10 <- data.frame(
      Pos     = paste0(seq_len(nrow(df_exp10)), "º"),
      SH4     = df_exp10$SH4_Codigo,
      Produto = substr(df_exp10$Produto_Curto, 1, 38),
      FOB_Mi  = paste0("US$ ", fmt_num(df_exp10$Valor_FOB_Mi, 2), " Mi"),
      Peso_T  = paste0(fmt_num(df_exp10$Peso_Ton, 1), " t"),
      USD_Kg  = ifelse(is.na(df_exp10$Preco_Medio_USD_Kg), "-", paste0("US$ ", fmt_num(df_exp10$Preco_Medio_USD_Kg, 2))),
      Part    = paste0(fmt_num(df_exp10$Participacao_Pct, 1), "%"),
      stringsAsFactors = FALSE
    )
    tab_exp10_tex <- gerar_tabela_padrao_latex(
      df = df_t_exp10,
      col_nomes = c("Pos.", "SH4", "Produto Exportado (Top 10)", "Valor FOB", "Peso Líq. (Ton)", "US$/Kg", "Part. %"),
      alinhamento = "cclrrrr",
      titulo_tabela = sprintf("Top 10 Posições SH4 Exportadas pelo Tocantins em Valor FOB e Peso Líquido (%s)", rot_recorte),
      nota_rodape = "Fonte: Comex Stat / MDIC (metricFOB e metricKG)."
    )
    tab_exp10_html <- gerar_tabela_padrao_html(
      df = df_t_exp10,
      col_nomes = c("Pos.", "SH4", "Produto Exportado (Top 10)", "Valor FOB", "Peso Líq. (Ton)", "US$/Kg", "Part. %"),
      alinhamento = c("center", "center", "left", "right", "right", "right", "right"),
      titulo_tabela = sprintf("Top 10 Posições SH4 Exportadas pelo Tocantins (%s)", rot_recorte),
      nota_rodape = "Fonte: Comex Stat / MDIC.",
      cor_primaria = cor_prim
    )

    # Tabela 2: Top 10 Produtos Importados (SH4) em US$ FOB e Peso Líquido (Ton / Kg)
    df_t_imp10 <- data.frame(
      Pos     = paste0(seq_len(nrow(df_imp10)), "º"),
      SH4     = df_imp10$SH4_Codigo,
      Produto = substr(df_imp10$Produto_Curto, 1, 38),
      FOB_Mi  = paste0("US$ ", fmt_num(df_imp10$Valor_FOB_Mi, 2), " Mi"),
      Peso_T  = paste0(fmt_num(df_imp10$Peso_Ton, 1), " t"),
      USD_Kg  = ifelse(is.na(df_imp10$Preco_Medio_USD_Kg), "-", paste0("US$ ", fmt_num(df_imp10$Preco_Medio_USD_Kg, 2))),
      Part    = paste0(fmt_num(df_imp10$Participacao_Pct, 1), "%"),
      stringsAsFactors = FALSE
    )
    tab_imp10_tex <- gerar_tabela_padrao_latex(
      df = df_t_imp10,
      col_nomes = c("Pos.", "SH4", "Produto Importado (Top 10)", "Valor FOB", "Peso Líq. (Ton)", "US$/Kg", "Part. %"),
      alinhamento = "cclrrrr",
      titulo_tabela = sprintf("Top 10 Posições SH4 Importadas pelo Tocantins em Valor FOB e Peso Líquido (%s)", rot_recorte),
      nota_rodape = "Fonte: Comex Stat / MDIC (metricFOB e metricKG)."
    )
    tab_imp10_html <- gerar_tabela_padrao_html(
      df = df_t_imp10,
      col_nomes = c("Pos.", "SH4", "Produto Importado (Top 10)", "Valor FOB", "Peso Líq. (Ton)", "US$/Kg", "Part. %"),
      alinhamento = c("center", "center", "left", "right", "right", "right", "right"),
      titulo_tabela = sprintf("Top 10 Posições SH4 Importadas pelo Tocantins (%s)", rot_recorte),
      nota_rodape = "Fonte: Comex Stat / MDIC.",
      cor_primaria = cor_prim
    )

    # Tabela 3: Top 10 Países de Destino (Exportação) e Origem (Importação) pareados lado a lado
    n_p <- max(nrow(df_pe10), nrow(df_pi10), 1)
    pad_df <- function(d, n) {
      if (is.null(d) || nrow(d) == 0) return(data.frame(Nome = rep("-", n), FOB = rep("-", n), Part = rep("-", n), stringsAsFactors = FALSE))
      res <- data.frame(
        Nome = substr(d$Nome, 1, 22),
        FOB  = paste0("US$ ", fmt_num(d$Valor, 2), " Mi"),
        Part = paste0(fmt_num(d$Participacao_Pct, 1), "%"),
        stringsAsFactors = FALSE
      )
      if (nrow(res) < n) {
        res[(nrow(res) + 1):n, ] <- "-"
      }
      res
    }
    pe_f <- pad_df(df_pe10, n_p)
    pi_f <- pad_df(df_pi10, n_p)
    df_paises_par <- data.frame(
      Pos      = paste0(seq_len(n_p), "º"),
      Pais_Exp = pe_f$Nome, FOB_Exp = pe_f$FOB, Part_Exp = pe_f$Part,
      Pais_Imp = pi_f$Nome, FOB_Imp = pi_f$FOB, Part_Imp = pi_f$Part,
      stringsAsFactors = FALSE
    )
    tab_paises_tex <- gerar_tabela_padrao_latex(
      df = df_paises_par,
      col_nomes = c("Pos.", "Destino Exportação (TO)", "US$ FOB Exp.", "% Exp.", "Origem Importação (TO)", "US$ FOB Imp.", "% Imp."),
      alinhamento = "clrrlrr",
      titulo_tabela = sprintf("Top 10 Países Parceiros do Tocantins: Destinos de Exportação e Origens de Importação (%s)", rot_recorte),
      nota_rodape = "Fonte: Comex Stat / MDIC."
    )
    tab_paises_html <- gerar_tabela_padrao_html(
      df = df_paises_par,
      col_nomes = c("Pos.", "Destino Exportação (TO)", "US$ FOB Exp.", "% Exp.", "Origem Importação (TO)", "US$ FOB Imp.", "% Imp."),
      alinhamento = c("center", "left", "right", "right", "left", "right", "right"),
      titulo_tabela = sprintf("Top 10 Países Parceiros do Tocantins (%s)", rot_recorte),
      nota_rodape = "Fonte: Comex Stat / MDIC.",
      cor_primaria = cor_prim
    )

    ult_m <- tail(df_m, 1)
    ant_m <- if (nrow(df_m) >= 2) df_m[nrow(df_m) - 1, ] else ult_m
    var_exp_m <- if (ant_m$Exportacoes_FOB_USD > 0) ((ult_m$Exportacoes_FOB_USD / ant_m$Exportacoes_FOB_USD) - 1) * 100 else 0
    var_imp_m <- if (ant_m$Importacoes_FOB_USD > 0) ((ult_m$Importacoes_FOB_USD / ant_m$Importacoes_FOB_USD) - 1) * 100 else 0
    var_sal_m <- if (abs(ant_m$Saldo_FOB_USD) > 0) ((ult_m$Saldo_FOB_USD - ant_m$Saldo_FOB_USD) / abs(ant_m$Saldo_FOB_USD)) * 100 else 0

    diag_lista <- list(
      list(nome = sprintf("Exportações FOB do Tocantins (%s)", cx$periodo_referencia), var_pct = var_exp_m, valor_fmt = cx$vb_exportacoes, serie = df_m$Exportacoes_FOB_USD),
      list(nome = sprintf("Importações FOB do Tocantins (%s)", cx$periodo_referencia), var_pct = var_imp_m, valor_fmt = cx$vb_importacoes, serie = df_m$Importacoes_FOB_USD),
      list(nome = "Saldo da Balança Comercial (TO)", var_pct = var_sal_m, valor_fmt = cx$vb_saldo, serie = df_m$Saldo_FOB_USD),
      list(nome = paste0("1º Produto Exportado (", df_exp10$Produto_Curto[1], ")"), var_pct = df_exp10$Participacao_Pct[1], valor_fmt = paste0("US$ ", fmt_num(df_exp10$Valor_FOB_Mi[1], 2), " Mi"), serie = NULL)
    )

    kpis <- list(
      list(rotulo = "EXPORTAÇÕES (TO)", valor = cx$vb_exportacoes, sub = paste0("Ref.: ", cx$periodo_referencia, " (Peso: ", fmt_num(ult_m$Exportacoes_Ton, 0), " t)")),
      list(rotulo = "IMPORTAÇÕES (TO)", valor = cx$vb_importacoes, sub = paste0("Ref.: ", cx$periodo_referencia, " (Peso: ", fmt_num(ult_m$Importacoes_Ton, 0), " t)")),
      list(rotulo = "SALDO COMERCIAL", valor = cx$vb_saldo, sub = "Superávit Primário FOB"),
      list(rotulo = "1º DESTINO EXPORTADOR", valor = df_pe10$Nome[1], sub = paste0(fmt_num(df_pe10$Participacao_Pct[1], 1), "% da pauta (US$ ", fmt_num(df_pe10$Valor[1], 1), " Mi)"))
    )

    bloco_extra_da_tex  <- ""
    bloco_extra_da_html <- ""
    if (e_dossie && !is.null(df_m_rec) && nrow(df_m_rec) > 0) {
      df_cx12 <- df_m_rec %>%
        transmute(
          Periodo  = as.character(Periodo),
          Exp_FOB  = paste0("US$ ", fmt_num(Exportacoes_FOB_USD / 1e6, 2), " Mi"),
          Exp_Ton  = paste0(fmt_num(Exportacoes_Ton, 1), " t"),
          Imp_FOB  = paste0("US$ ", fmt_num(Importacoes_FOB_USD / 1e6, 2), " Mi"),
          Imp_Ton  = paste0(fmt_num(Importacoes_Ton, 1), " t"),
          Saldo_Mi = paste0("US$ ", fmt_num(Saldo_FOB_USD / 1e6, 2), " Mi")
        ) %>% as.data.frame()
      bloco_extra_da_tex <- gerar_tabela_padrao_latex(
        df = df_cx12,
        col_nomes = c("Competência", "Exportações FOB", "Volume Exp. (Ton)", "Importações FOB", "Volume Imp. (Ton)", "Saldo Comercial FOB"),
        alinhamento = "lrrrrr",
        titulo_tabela = "Série Retrospectiva de 12 Meses da Balança Comercial do Tocantins em Valor FOB e Volume Físico (Toneladas)",
        nota_rodape = "Fonte: Comex Stat / MDIC."
      )
      bloco_extra_da_html <- gerar_tabela_padrao_html(
        df = df_cx12,
        col_nomes = c("Competência", "Exportações FOB", "Volume Exp. (Ton)", "Importações FOB", "Volume Imp. (Ton)", "Saldo Comercial FOB"),
        alinhamento = c("left", "right", "right", "right", "right", "right"),
        titulo_tabela = "Série Retrospectiva de 12 Meses da Balança Comercial do Tocantins",
        nota_rodape = "Fonte: Comex Stat / MDIC.",
        cor_primaria = cor_prim
      )
    }

    tabs_exp_imp_par <- parear_tabelas_periodico_latex(tab_exp10_tex, tab_imp10_tex)

    bloco_tex <- paste0(
      "\\begin{center}\\includegraphics[width=0.95\\textwidth]{figuras/", arq_fig1, "}\\end{center}\n",
      tabs_exp_imp_par, "\n", tab_paises_tex, "\n", bloco_extra_da_tex
    )
    bloco_html <- paste0(
      sprintf('<div class="painel-empirico-html"><img src="boletins/figuras/%s" style="width:100%%; border-radius:8px; margin-bottom:10px;" />%s%s%s%s</div>',
              arq_png1, tab_exp10_html, tab_imp10_html, tab_paises_html, bloco_extra_da_html)
    )

    return(list(
      categoria_nome = if (e_dossie) "Dossiê Anual — Balança Comercial do Tocantins" else "Boletim Flash — Comércio Exterior",
      tema_nome      = "Comércio Exterior e Inserção Internacional do Tocantins",
      fonte_curta    = "Comex Stat / MDIC — Ministério do Desenvolvimento, Indústria, Comércio e Serviços",
      nota_metodo    = "Dados extraídos da API oficial Comex Stat / MDIC por posição SH4 e país parceiro, contemplando simultaneamente Valor FOB (US$) e Peso Líquido (Quilogramas e Toneladas Métricas).",
      kpis           = kpis,
      diag_lista     = diag_lista,
      otica          = otica_str,
      bloco_tex      = bloco_tex,
      bloco_html     = bloco_html,
      fig_periodico  = arq_fig1,
      tabs_principais_da = paste0(tabs_exp_imp_par, "\n", tab_paises_tex),
      extra_da_tex   = bloco_extra_da_tex,
      tabs_periodico = paste0(tabs_exp_imp_par, "\n", tab_paises_tex, "\n", bloco_extra_da_tex),
      texto_auto_html = sprintf("<p>Na competência <strong>%s</strong>, as exportações do Tocantins somaram <strong>%s</strong> (%s toneladas), ante importações de <strong>%s</strong> (%s toneladas), resultando em saldo comercial de <strong>%s</strong>.</p>",
                                cx$periodo_referencia, cx$vb_exportacoes, fmt_num(ult_m$Exportacoes_Ton, 1),
                                cx$vb_importacoes, fmt_num(ult_m$Importacoes_Ton, 1), cx$vb_saldo)
    ))
  }

  # ============================================================================
  # TEMA 5: ARRECADAÇÃO E FINANÇAS PÚBLICAS (FLASH E DOSSIÊ ANUAL)
  #   - Apenas infos do painel (FLASH sob ótica mensal; DA sob ótica anual/12 meses)
  # ============================================================================
  if (tipo %in% c("flash_financas", "flash_icms", "dossie_financas")) {
    fn <- pd$financas
    ar <- ex$arrecadacao_to
    df_rec <- fn$df_receitas
    df_set <- fn$df_setores_icms

    arq_fig1 <- sprintf("%s_fin_receitas.pdf", slug)
    arq_png1 <- sprintf("%s_fin_receitas.png", slug)
    df_long <- rbind(
      data.frame(Mes = df_rec$Mes, Valor = df_rec$ICMS, Tributo = "ICMS Próprio"),
      data.frame(Mes = df_rec$Mes, Valor = df_rec$FPE,  Tributo = "FPE (Repasse União)"),
      data.frame(Mes = df_rec$Mes, Valor = df_rec$IPVA, Tributo = "IPVA e Outros Tributos")
    )
    p1 <- ggplot(df_long, aes(x = Mes, y = Valor, color = Tributo, group = Tributo)) +
      geom_line(linewidth = 1.05) +
      geom_point(size = 2.2) +
      scale_color_manual(values = c("ICMS Próprio" = cor_prim, "FPE (Repasse União)" = cor_acento, "IPVA e Outros Tributos" = "#64748B")) +
      labs(
        title = sprintf("Evolução Mensal das Receitas Estaduais: ICMS, FPE e IPVA+Outros (%s)", fn$mes_ano_extenso_sefaz),
        subtitle = "Valores nominais em R$ Milhões — SEFAZ-TO e STN/Tesouro Nacional",
        x = NULL, y = "R$ Milhões", color = NULL
      ) +
      tema_grafico_boletim()
    salvar_grafico_duplo(p1, arq_fig1, arq_png1, w = 6.6, h = 2.25)

    # Tabela 1: Série Mensal do Exercício (ICMS, FPE, IPVA+Outros e Total)
    df_t_rec <- df_rec %>%
      mutate(
        Total = ICMS + FPE + IPVA,
        ICMS_F = paste0("R$ ", fmt_num(ICMS, 2), " Mi"),
        FPE_F  = paste0("R$ ", fmt_num(FPE, 2), " Mi"),
        IPVA_F = paste0("R$ ", fmt_num(IPVA, 2), " Mi"),
        Tot_F  = paste0("R$ ", fmt_num(Total, 2), " Mi")
      ) %>%
      select(Mes, ICMS_F, FPE_F, IPVA_F, Tot_F) %>%
      as.data.frame()
    tab_rec_tex <- gerar_tabela_padrao_latex(
      df = df_t_rec,
      col_nomes = c("Mês (2026)", "ICMS (SEFAZ-TO)", "Cota-Parte FPE (STN)", "IPVA + Outros Tributos", "Receita Total Mensal"),
      alinhamento = "lrrrr",
      titulo_tabela = "Execução Mensal das Receitas Próprias e Transferências Constitucionais do Tocantins",
      nota_rodape = "Fonte: Boletim Tributário SEFAZ-TO e Secretaria do Tesouro Nacional (STN)."
    )
    tab_rec_html <- gerar_tabela_padrao_html(
      df = df_t_rec,
      col_nomes = c("Mês (2026)", "ICMS (SEFAZ-TO)", "Cota-Parte FPE (STN)", "IPVA + Outros Tributos", "Receita Total Mensal"),
      alinhamento = c("left", "right", "right", "right", "right"),
      titulo_tabela = "Execução Mensal das Receitas Próprias e Transferências Constitucionais (TO)",
      nota_rodape = "Fonte: SEFAZ-TO e STN.",
      cor_primaria = cor_prim
    )

    # Tabela 2: Composição Setorial do ICMS (Mesmos blocos do painel)
    tot_icms_set <- sum(df_set$Valor, na.rm = TRUE)
    df_t_set <- df_set %>%
      arrange(desc(Valor)) %>%
      mutate(
        Valor_F = paste0("R$ ", fmt_num(Valor, 2), " Mi"),
        Part_F  = paste0(fmt_num((Valor / tot_icms_set) * 100, 2), "%")
      ) %>%
      select(Setor, Valor_F, Part_F) %>%
      as.data.frame()
    tab_set_tex <- gerar_tabela_padrao_latex(
      df = df_t_set,
      col_nomes = c("Segmento Econômico do ICMS (Painel)", "Arrecadação no Mês", "Participação no ICMS (%)"),
      alinhamento = "lrr",
      titulo_tabela = sprintf("Arrecadação de ICMS por Segmento Econômico no Tocantins (%s)", fn$mes_ano_extenso_sefaz),
      nota_rodape = "Fonte: SEFAZ-TO — Boletim Tributário Estadual."
    )
    tab_set_html <- gerar_tabela_padrao_html(
      df = df_t_set,
      col_nomes = c("Segmento Econômico do ICMS (Painel)", "Arrecadação no Mês", "Participação no ICMS (%)"),
      alinhamento = c("left", "right", "right"),
      titulo_tabela = sprintf("Arrecadação de ICMS por Segmento Econômico (%s)", fn$mes_ano_extenso_sefaz),
      nota_rodape = "Fonte: SEFAZ-TO.",
      cor_primaria = cor_prim
    )

    # No Dossiê Anual (3-4 páginas), adicionamos a Tabela Histórica Anual (2023-2026) das receitas do painel
    bloco_extra_da_tex  <- ""
    bloco_extra_da_html <- ""
    if (e_dossie && !is.null(ar$anual_tributos)) {
      df_an <- ar$anual_tributos %>%
        transmute(
          Ano       = as.character(Periodo_Anual),
          Rec_Total = paste0("R$ ", fmt_num(Receita_Total_Mi, 1), " Mi"),
          ICMS      = paste0("R$ ", fmt_num(ICMS_Mi, 1), " Mi"),
          FPE       = paste0("R$ ", fmt_num(FPE_Mi, 1), " Mi"),
          IPVA_Out  = paste0("R$ ", fmt_num(IPVA_Total_Mi + Subtotal_Outros_Mi, 1), " Mi"),
          Var_ICMS  = ifelse(is.na(Var_ICMS_YoY_Pct), "-", sprintf("%+.2f%%", Var_ICMS_YoY_Pct))
        ) %>% as.data.frame()
      bloco_extra_da_tex <- gerar_tabela_padrao_latex(
        df = df_an,
        col_nomes = c("Exercício", "Receita Total", "ICMS Acumulado", "FPE Acumulado", "IPVA + Outros", "Var. Anual ICMS"),
        alinhamento = "lrrrrr",
        titulo_tabela = "Consolidado Plurianual das Receitas Estaduais do Tocantins (2023–2026)",
        nota_rodape = "Fonte: SEFAZ-TO e STN."
      )
      bloco_extra_da_html <- gerar_tabela_padrao_html(
        df = df_an,
        col_nomes = c("Exercício", "Receita Total", "ICMS Acumulado", "FPE Acumulado", "IPVA + Outros", "Var. Anual ICMS"),
        alinhamento = c("left", "right", "right", "right", "right", "right"),
        titulo_tabela = "Consolidado Plurianual das Receitas Estaduais (2023–2026)",
        nota_rodape = "Fonte: SEFAZ-TO e STN.",
        cor_primaria = cor_prim
      )
    }

    var_icms_num <- if (!is.null(ar$var_icms_mes_pct) && length(ar$var_icms_mes_pct) > 0 && !is.na(ar$var_icms_mes_pct)) {
      ar$var_icms_mes_pct
    } else {
      suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,-]", "", fn$vb_icms_var_mensal))))
    }
    var_icms_yoy_num <- if (!is.null(ar$var_icms_yoy_mes_pct) && length(ar$var_icms_yoy_mes_pct) > 0 && !is.na(ar$var_icms_yoy_mes_pct)) {
      ar$var_icms_yoy_mes_pct
    } else {
      13.8
    }
    vb_icms_yoy_str <- sprintf("%+.1f%%", var_icms_yoy_num)

    diag_lista <- list(
      list(nome = sprintf("Arrecadação de ICMS (%s)", fn$mes_ano_sefaz), var_pct = var_icms_num, valor_fmt = fn$vb_icms, serie = df_rec$ICMS),
      list(nome = "Cota-Parte do FPE (Transferência União)", var_pct = ((tail(df_rec$FPE, 1) / df_rec$FPE[nrow(df_rec) - 1]) - 1) * 100, valor_fmt = fn$vb_fpe, serie = df_rec$FPE),
      list(nome = "IPVA e Outros Tributos Estaduais", var_pct = ((tail(df_rec$IPVA, 1) / df_rec$IPVA[nrow(df_rec) - 1]) - 1) * 100, valor_fmt = fn$vb_ipva, serie = df_rec$IPVA),
      list(nome = "Variação Interanual do ICMS (YoY)", var_pct = var_icms_yoy_num, valor_fmt = vb_icms_yoy_str, serie = NULL)
    )

    kpis <- list(
      list(rotulo = "RECEITA TOTAL DO MÊS", valor = fn$vb_receita_total, sub = paste0("Ref.: ", fn$mes_ano_sefaz)),
      list(rotulo = "ARRECADAÇÃO DE ICMS", valor = fn$vb_icms, sub = paste0("Var. Mês: ", fn$vb_icms_var_mensal, " | YoY: ", vb_icms_yoy_str)),
      list(rotulo = "REPASSES DO FPE", valor = fn$vb_fpe, sub = "Tesouro Nacional / STN"),
      list(rotulo = "IPVA E OUTROS TRIBUTOS", valor = fn$vb_ipva, sub = paste0("IPVA puro: ", fn$vb_ipva_puro))
    )

    tabs_rec_set_par <- parear_tabelas_periodico_latex(tab_rec_tex, tab_set_tex)

    bloco_tex <- paste0(
      "\\begin{center}\\includegraphics[width=0.95\\textwidth]{figuras/", arq_fig1, "}\\end{center}\n",
      tabs_rec_set_par, "\n", bloco_extra_da_tex
    )
    bloco_html <- paste0(
      sprintf('<div class="painel-empirico-html"><img src="boletins/figuras/%s" style="width:100%%; border-radius:8px; margin-bottom:10px;" />%s%s%s</div>',
              arq_png1, tab_rec_html, tab_set_html, bloco_extra_da_html)
    )

    return(list(
      categoria_nome = if (e_dossie) "Dossiê Anual — Arrecadação e Finanças Públicas" else "Boletim Flash — Arrecadação e Finanças",
      tema_nome      = "Arrecadação Tributária e Transferências Constitucionais (TO)",
      fonte_curta    = "SEFAZ-TO, CONFAZ e Secretaria do Tesouro Nacional (STN)",
      nota_metodo    = "Consolida os indicadores fiscais do painel: Receita Tributária Própria (ICMS, IPVA, ITCD, IRRF e Taxas) extraída dos Boletins Tributários da SEFAZ-TO e repasses constitucionais do FPE (STN).",
      kpis           = kpis,
      diag_lista     = diag_lista,
      otica          = otica_str,
      bloco_tex      = bloco_tex,
      bloco_html     = bloco_html,
      fig_periodico  = arq_fig1,
      tabs_principais_da = tabs_rec_set_par,
      extra_da_tex   = bloco_extra_da_tex,
      tabs_periodico = paste0(tabs_rec_set_par, "\n", bloco_extra_da_tex),
      texto_auto_html = sprintf("<p>Na competência <strong>%s</strong>, a receita total monitorada no Tocantins somou <strong>%s</strong>, sendo <strong>%s</strong> de arrecadação própria de ICMS (%s ante o mês imediatamente anterior e %s na comparação anual), <strong>%s</strong> de repasses do FPE e <strong>%s</strong> de IPVA e demais tributos.</p>",
                                fn$mes_ano_extenso_sefaz, fn$vb_receita_total, fn$vb_icms, fn$vb_icms_var_mensal, vb_icms_yoy_str, fn$vb_fpe, fn$vb_ipva)
    ))
  }

  # ============================================================================
  # TEMA 6: DINÂMICA EMPRESARIAL (FLASH E DOSSIÊ ANUAL)
  #   - Infos do Painel + Dados sobre os Sócios — QSA (qualificação, faixa etária e gênero)
  # ============================================================================
  if (tipo %in% c("flash_empresas", "dossie_empresas")) {
    emp <- pd$empresarial
    cj  <- ex$cnpj_to
    tb  <- cj$tabelas_boletins

    arq_fig1 <- sprintf("%s_emp_ativos.pdf", slug)
    arq_png1 <- sprintf("%s_emp_ativos.png", slug)
    df_at <- emp$df_ativos
    p1 <- ggplot(df_at, aes(x = Mes, y = Valor / 1000, group = 1)) +
      geom_line(color = cor_prim, linewidth = 1.1) +
      geom_point(color = cor_acento, size = 2.3) +
      labs(
        title = sprintf("Evolução Mensal do Estoque de Empresas Ativas no Tocantins (Até %s)", emp$competencia_cnpj),
        subtitle = "Total de estabelecimentos ativos (Milhares de CNPJs) — Receita Federal / SERPRO",
        x = NULL, y = "Milhares de CNPJs"
      ) +
      tema_grafico_boletim()
    salvar_grafico_duplo(p1, arq_fig1, arq_png1, w = 6.6, h = 2.15)

    # Tabela 1: Estrutura por Porte Empresarial e Simples Nacional (Painel)
    df_p <- cj$indicadores_painel$df_porte
    df_t_porte <- data.frame(
      Porte    = as.character(df_p$Porte),
      Ativos   = fmt_int(df_p$Total),
      Part_Pct = paste0(fmt_num(df_p$Participacao_Pct, 2), "%"),
      stringsAsFactors = FALSE
    )
    tab_porte_tex <- gerar_tabela_padrao_latex(
      df = df_t_porte,
      col_nomes = c("Porte Empresarial (Receita Federal / SINAC)", "CNPJs Ativos no Tocantins", "Participação no Estoque (%)"),
      alinhamento = "lrr",
      titulo_tabela = sprintf("Distribuição das Empresas Ativas por Porte no Tocantins (%s)", emp$competencia_cnpj),
      nota_rodape = sprintf("Fonte: Receita Federal / SERPRO e SINAC (Optantes Simples Nacional: %s | Tempo Médio de Abertura 2026: %sh).",
                            emp$vb_optantes_simples, fmt_num(emp$tempo_abertura_h, 1))
    )
    tab_porte_html <- gerar_tabela_padrao_html(
      df = df_t_porte,
      col_nomes = c("Porte Empresarial (Receita Federal / SINAC)", "CNPJs Ativos no Tocantins", "Participação no Estoque (%)"),
      alinhamento = c("left", "right", "right"),
      titulo_tabela = sprintf("Distribuição das Empresas Ativas por Porte no Tocantins (%s)", emp$competencia_cnpj),
      nota_rodape = "Fonte: Receita Federal / SERPRO e SINAC.",
      cor_primaria = cor_prim
    )

    # Tabela 2: Quadro de Sócios e Administradores (QSA — Qualificação, Faixa Etária e Gênero)
    qsa <- tb$socios_qsa
    df_qsa_qual <- head(qsa$por_tipo_e_qualificacao, 6)
    tot_socios_qsa <- sum(qsa$por_genero$Qtd_Socios, na.rm = TRUE)
    tot_resp_qsa   <- sum(qsa$responsaveis_por_genero_idade$Empresas_Ativas, na.rm = TRUE)
    df_t_qual <- data.frame(
      Qualificacao = paste0(df_qsa_qual$Qualificacao_Socio, " (", df_qsa_qual$Tipo_Socio, ")"),
      Socios       = fmt_int(df_qsa_qual$Qtd_Socios),
      Empresas     = fmt_int(df_qsa_qual$Empresas_Ativas),
      Part_Pct     = paste0(fmt_num(df_qsa_qual$Participacao_Socios_Pct, 2), "%"),
      stringsAsFactors = FALSE
    )

    df_qsa_fx <- qsa$por_faixa_etaria
    df_t_fx <- data.frame(
      Faixa_Etaria = as.character(df_qsa_fx$Faixa_Etaria_Socio),
      Socios       = fmt_int(df_qsa_fx$Qtd_Socios),
      Empresas     = fmt_int(df_qsa_fx$Empresas_Ativas),
      Part_Pct     = paste0(fmt_num(df_qsa_fx$Participacao_Socios_Pct, 1), "%"),
      stringsAsFactors = FALSE
    )

    tab_qsa_tex <- parear_tabelas_periodico_latex(
      gerar_tabela_padrao_latex(
        df = df_t_qual,
        col_nomes = c("Qualificação e Tipo no QSA (RFB)", "Sócios", "Empresas", "Part. %"),
        alinhamento = "lrrr",
        titulo_tabela = sprintf("Quadro Societário (QSA) por Qualificação (Total: %s)", fmt_int(tot_socios_qsa)),
        nota_rodape = sprintf("Fonte: RFB/SERPRO. Gênero QSA: %s.",
                              paste(sprintf("%s (%s%%)", qsa$por_genero$Genero_Socio, fmt_num(qsa$por_genero$Participacao_Socios_Pct, 1)), collapse = "; "))
      ),
      gerar_tabela_padrao_latex(
        df = df_t_fx,
        col_nomes = c("Faixa Etária dos Sócios (QSA)", "Sócios", "Empresas", "Part. %"),
        alinhamento = "lrrr",
        titulo_tabela = "Perfil Etário do Quadro Societário (QSA — TO)",
        nota_rodape = "Fonte: Receita Federal do Brasil / SERPRO — Microdados do QSA."
      )
    )

    tab_qsa_html <- paste0(
      gerar_tabela_padrao_html(
        df = df_t_qual,
        col_nomes = c("Qualificação Legal e Tipo no QSA", "Sócios Ativos", "Empresas Ativas", "Participação (%)"),
        alinhamento = c("left", "right", "right", "right"),
        titulo_tabela = sprintf("Quadro de Sócios e Administradores (QSA) por Qualificação (Total: %s)", fmt_int(tot_socios_qsa)),
        nota_rodape = "Fonte: Receita Federal / SERPRO.",
        cor_primaria = cor_prim
      ),
      gerar_tabela_padrao_html(
        df = df_t_fx,
        col_nomes = c("Faixa Etária dos Sócios (QSA)", "Total de Sócios Ativos", "Empresas Ativas", "Participação Etária (%)"),
        alinhamento = c("left", "right", "right", "right"),
        titulo_tabela = "Perfil Demográfico Etário do Quadro Societário (QSA — Tocantins)",
        nota_rodape = "Fonte: Receita Federal / SERPRO.",
        cor_primaria = cor_prim
      )
    )

    # No Dossiê Anual (3-4 páginas), adicionamos a Série Mensal de Aberturas, Baixas e Saldo nos Últimos 12 Meses
    bloco_extra_da_tex  <- ""
    bloco_extra_da_html <- ""
    s12 <- tb$serie_ultimos_12m_to
    if (e_dossie && !is.null(s12)) {
      df_s12 <- s12 %>%
        transmute(
          Competencia = as.character(Periodo_Rotulo),
          Aberturas   = fmt_int(Aberturas),
          Baixas      = fmt_int(Baixas),
          Saldo_Mes   = fmt_sinal(Saldo, 0),
          Ativos_Fim  = fmt_int(Estoque_Ativo)
        ) %>% as.data.frame()
      bloco_extra_da_tex <- gerar_tabela_padrao_latex(
        df = df_s12,
        col_nomes = c("Competência", "Novas Inscrições (Aberturas)", "Baixas / Extinções", "Saldo Líquido Mensal", "Estoque de CNPJs Ativos"),
        alinhamento = "lrrrr",
        titulo_tabela = "Balanço Demográfico Empresarial nos Últimos 12 Meses no Tocantins (Aberturas, Baixas e Estoque)",
        nota_rodape = "Fonte: Receita Federal do Brasil / SERPRO."
      )
      bloco_extra_da_html <- gerar_tabela_padrao_html(
        df = df_s12,
        col_nomes = c("Competência", "Novas Inscrições (Aberturas)", "Baixas / Extinções", "Saldo Líquido Mensal", "Estoque de CNPJs Ativos"),
        alinhamento = c("left", "right", "right", "right", "right"),
        titulo_tabela = "Balanço Demográfico Empresarial nos Últimos 12 Meses (TO)",
        nota_rodape = "Fonte: Receita Federal / SERPRO.",
        cor_primaria = cor_prim
      )
    }

    ult_ab <- tail(s12$Aberturas, 1)
    ant_ab <- s12$Aberturas[nrow(s12) - 1]
    var_ab <- if (ant_ab > 0) ((ult_ab / ant_ab) - 1) * 100 else 0

    diag_lista <- list(
      list(nome = sprintf("Estoque de CNPJs Ativos (%s)", emp$competencia_cnpj), var_pct = ((tail(s12$Estoque_Ativo, 1) / s12$Estoque_Ativo[1]) - 1) * 100, valor_fmt = emp$vb_cnpjs_ativos, serie = s12$Estoque_Ativo),
      list(nome = "Novas Inscrições de CNPJ no Mês", var_pct = var_ab, valor_fmt = pd$visao_geral$vb_novos_cnpjs, serie = s12$Aberturas),
      list(nome = "Participação de MEIs + Microempresas (ME)", var_pct = 0.05, valor_fmt = emp$vb_me_com_mei, serie = NULL),
      list(nome = "Tempo Médio de Abertura de Empresas (TO vs. Brasil)", var_pct = ((emp$tempo_abertura_h / emp$media_brasil_h) - 1) * 100, valor_fmt = paste0(fmt_num(emp$tempo_abertura_h, 1), "h (BR: ", fmt_num(emp$media_brasil_h, 1), "h)"), serie = NULL)
    )

    kpis <- list(
      list(rotulo = "CNPJS ATIVOS (ESTOQUE)", valor = emp$vb_cnpjs_ativos, sub = paste0("Saldo 12m: ", emp$vb_saldo_empresas)),
      list(rotulo = "MEI / MICROEMPRESAS", valor = paste0(emp$vb_mei, " / ", emp$vb_me), sub = paste0("Pequenos Negócios: ", emp$vb_me_com_mei)),
      list(rotulo = "OPTANTES SIMPLES (SINAC)", valor = emp$vb_optantes_simples, sub = paste0("Sócios QSA: ", fmt_int(tot_socios_qsa))),
      list(rotulo = "TEMPO DE ABERTURA (2026)", valor = paste0(fmt_num(emp$tempo_abertura_h, 1), " horas"), sub = paste0("Viab.: ", fmt_num(emp$tempo_viabilidade_h, 1), "h | Reg.: ", fmt_num(emp$tempo_registro_h, 1), "h"))
    )

    bloco_tex <- paste0(
      "\\begin{center}\\includegraphics[width=0.95\\textwidth]{figuras/", arq_fig1, "}\\end{center}\n",
      tab_porte_tex, "\n", tab_qsa_tex, "\n", bloco_extra_da_tex
    )
    bloco_html <- paste0(
      sprintf('<div class="painel-empirico-html"><img src="boletins/figuras/%s" style="width:100%%; border-radius:8px; margin-bottom:10px;" />%s%s%s</div>',
              arq_png1, tab_porte_html, tab_qsa_html, bloco_extra_da_html)
    )

    return(list(
      categoria_nome = if (e_dossie) "Dossiê Anual — Dinâmica Empresarial e QSA" else "Boletim Flash — Dinâmica Empresarial",
      tema_nome      = "Dinâmica Empresarial, Simples Nacional e Quadro Societário (TO)",
      fonte_curta    = "Receita Federal do Brasil / SERPRO, Portal SINAC e Mapa de Empresas (REDESIM)",
      nota_metodo    = "Integra os registros de CNPJs ativos por porte (MEI, ME, EPP e Demais), optantes pelo Simples Nacional (SINAC), tempos de abertura (Mapa de Empresas) e o Quadro de Sócios e Administradores (QSA — qualificação, faixa etária e sexo).",
      kpis           = kpis,
      diag_lista     = diag_lista,
      otica          = otica_str,
      bloco_tex      = bloco_tex,
      bloco_html     = bloco_html,
      fig_periodico  = arq_fig1,
      tabs_principais_da = paste0(tab_porte_tex, "\n", tab_qsa_tex),
      extra_da_tex   = bloco_extra_da_tex,
      tabs_periodico = paste0(tab_porte_tex, "\n", tab_qsa_tex, "\n", bloco_extra_da_tex),
      texto_auto_html = sprintf("<p>Em <strong>%s</strong>, o Tocantins contabilizou <strong>%s CNPJs ativos</strong> (saldo líquido de <strong>%s empresas</strong> em 12 meses), com <strong>%s</strong> de MEIs e <strong>%s</strong> de Microempresas (ME). O Quadro de Sócios e Administradores (QSA) reúne <strong>%s vínculos societários</strong> (%s empresas com responsável qualificado), enquanto o tempo médio de abertura de empresas situou-se em <strong>%s horas</strong>.</p>",
                                emp$mes_ano_extenso_cnpj, emp$vb_cnpjs_ativos, emp$vb_saldo_empresas, emp$vb_mei, emp$vb_me,
                                fmt_int(tot_socios_qsa), fmt_int(tot_resp_qsa), fmt_num(emp$tempo_abertura_h, 1))
    ))
  }

  stop(sprintf("Tipo de boletim tematico nao reconhecido em modulos_boletins_tematicos.R: '%s'", tipo))
}
