# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/compilar_boletins.R
# Objetivo:
#   1. Ler os dados oficiais de 'dados/painel_dados.rds' (e 'dados/agro_to_resumo.rds').
#   2. Ler os arquivos de texto (.txt) redigidos pela equipe em 'boletins/textos_equipe/'.
#   3. Gerar o Diagnóstico Estatístico Automatizado ("Análise Robótica" do Script)
#      identificando crescimentos, quedas, estabilidades e mudanças drásticas.
#   4. Aplicar as 3 Identidades Visuais e Estruturas Editoriais:
#      - Boletim FLASH (1 coluna, Azul-Marinho #0F2537 + Dourado #C8942B, 2 a 3 págs.):
#        6 temas: Visão Geral, PIB, Mercado de Trabalho, Comex, Finanças, Empresas.
#      - Dossiê Anual — DA (Periódico 2 colunas, Verde Floresta #0B5D32 + Marfim #FAF6EE
#        + Carmim #A61C1C, 3 a 4 págs.):
#        5 temas: PIB, Mercado de Trabalho, Comex, Finanças, Empresas.
#      - Carta de Conjuntura — CC (Periódico 2 colunas + tabelas largas, Azul Cobalto
#        #0A2E73 + Esmeralda #1B7A43 + Lima #8CE000 no cabeçalho, 8 Seções na ordem exata).
#   5. Compilar os PDFs via pdflatex e salvar 'dados/catalogo_boletins.rds' p/ news.qmd.
# ==============================================================================

if (!file.exists("index.qmd") && dir.exists("D:/Journal")) {
  setwd("D:/Journal")
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

if (file.exists("scripts/utils_debug_log.R")) {
  source("scripts/utils_debug_log.R")
  limpar_registro_diagnostico()
}

log_secao("INICIANDO COMPILAÇÃO AUTOMATIZADA DE BOLETINS, DOSSIÊS E CARTAS DE CONJUNTURA")

dir.create("boletins/templates", recursive = TRUE, showWarnings = FALSE)
dir.create("boletins/textos_equipe", recursive = TRUE, showWarnings = FALSE)
dir.create("boletins/figuras", recursive = TRUE, showWarnings = FALSE)
dir.create("boletins/gerados_tex", recursive = TRUE, showWarnings = FALSE)
dir.create("boletins/pdfs", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO E SINCRONIZAÇÃO DAS BASES DE DADOS (.RDS)
# ------------------------------------------------------------------------------
args_cmd <- commandArgs(trailingOnly = TRUE)
forcar_sync <- ("--atualizar" %in% args_cmd) || !file.exists("dados/painel_dados.rds")

if (forcar_sync) {
  log_info("Sincronização Geral", "Executando scripts/atualizar_dados.R antes da compilação...")
  source("scripts/atualizar_dados.R", local = TRUE)
}

painel_dados <- readRDS("dados/painel_dados.rds")
ex <- painel_dados$pesquisas_extras

if (is.null(ex$agro_to)) {
  if (file.exists("dados/agro_to_resumo.rds")) {
    ex$agro_to <- readRDS("dados/agro_to_resumo.rds")
  } else if (file.exists("scripts/extrair_agro_to.R")) {
    source("scripts/extrair_agro_to.R", local = TRUE)
    ex$agro_to <- extrair_agro_tocantins(diretorio_raiz = getwd(), forcar_atualizacao = FALSE)
  }
}

# ------------------------------------------------------------------------------
# 2. FUNÇÕES AUXILIARES DE FORMATAÇÃO, ESCAPE LATEX/HTML E COMPONENTES VISUAIS
# ------------------------------------------------------------------------------
fmt_num <- function(x, casas = 1) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "-", format(round(v, casas), big.mark = ".", decimal.mark = ",", nsmall = casas, trim = TRUE))
}

fmt_int <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "-", format(round(v, 0), big.mark = ".", decimal.mark = ",", nsmall = 0, scientific = FALSE, trim = TRUE))
}

fmt_sinal <- function(x, casas = 0) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(v), "-",
    paste0(ifelse(v > 0, "+", ""), format(round(v, casas), big.mark = ".", decimal.mark = ",", nsmall = casas, scientific = FALSE, trim = TRUE))
  )
}

escapar_latex <- function(texto) {
  if (is.null(texto) || length(texto) == 0) return("")
  t <- as.character(texto)
  t <- gsub("\\\\", "\\\\textbackslash{}", t)
  t <- gsub("([%&\\$#_{}])", "\\\\\\1", t, perl = TRUE)
  t <- gsub("~", "\\textasciitilde{}", t, fixed = TRUE)
  t <- gsub("^", "\\textasciicircum{}", t, fixed = TRUE)
  t <- gsub("▲", "$\\blacktriangle$", t, fixed = TRUE)
  t <- gsub("▼", "$\\blacktriangledown$", t, fixed = TRUE)
  t <- gsub("º", "\\textordmasculine{}", t, fixed = TRUE)
  t <- gsub("ª", "\\textordfeminine{}", t, fixed = TRUE)
  t
}

escapar_html <- function(texto) {
  if (is.null(texto) || length(texto) == 0) return("")
  t <- as.character(texto)
  t <- gsub("&", "&amp;", t, fixed = TRUE)
  t <- gsub("<", "&lt;", t, fixed = TRUE)
  t <- gsub(">", "&gt;", t, fixed = TRUE)
  t
}

paragrafos_para_latex <- function(texto_bruto) {
  if (is.null(texto_bruto) || !nzchar(trimws(texto_bruto))) return("")
  blocos <- unlist(strsplit(texto_bruto, "\n\\s*\n"))
  blocos <- trimws(blocos)
  blocos <- blocos[nzchar(blocos)]
  paste(vapply(blocos, function(p) {
    linhas <- gsub("\n", " ", p)
    escapar_latex(linhas)
  }, character(1)), collapse = "\n\n\\medskip\n")
}

paragrafos_para_html <- function(texto_bruto) {
  if (is.null(texto_bruto) || !nzchar(trimws(texto_bruto))) return("")
  blocos <- unlist(strsplit(texto_bruto, "\n\\s*\n"))
  blocos <- trimws(blocos)
  blocos <- blocos[nzchar(blocos)]
  paste(vapply(blocos, function(p) {
    paste0("<p style='text-align:justify; line-height:1.58; margin-bottom:10px;'>", escapar_html(gsub("\n", " ", p)), "</p>")
  }, character(1)), collapse = "\n")
}

# Helper interno para gerar 1 faixa minimalista de indicadores em LaTeX (estilo FT/Economist)
.render_linha_kpis_latex <- function(lista_kpis, eh_ranking_setor = FALSE) {
  n <- length(lista_kpis)
  if (n == 0) return("")
  largura_box <- if (n <= 2) {
    "0.445\\textwidth"
  } else if (n == 3) {
    "0.286\\textwidth"
  } else if (n == 4) {
    "0.206\\textwidth"
  } else {
    "0.156\\textwidth"
  }
  altura_box <- if (eh_ranking_setor) "0.88cm" else "1.14cm"
  caixas <- vapply(lista_kpis, function(k) {
    cor_valor <- if (!is.null(k$cor_valor)) k$cor_valor else "corprimaria"
    cor_borda <- if (!is.null(k$cor_borda)) k$cor_borda else "coracento"
    paste0(
      "\\begin{tikzpicture}[baseline=(box.base)]\n",
      "  \\node[fill=white, draw=cinzaborda, line width=0.35pt,\n",
      "        inner xsep=6pt, inner ysep=4pt, text width=", largura_box, ", minimum height=", altura_box, ", align=left] (box) {%\n",
      "    {\\fontsize{6.0}{7.2}\\selectfont\\bfseries\\color{cinzasub}", escapar_latex(k$rotulo), "}\\par\\vspace{1.2pt}%\n",
      "    {\\fontsize{9.2}{10.8}\\selectfont\\bfseries\\color{", cor_valor, "}", escapar_latex(k$valor), "}\\par\\vspace{1pt}%\n",
      "    {\\fontsize{5.8}{6.8}\\selectfont\\color{cinzasub}", escapar_latex(k$sub), "}%\n",
      "  };\n",
      "  \\fill[", cor_borda, "] (box.north west) rectangle ([yshift=-1.6pt]box.north east);\n",
      "\\end{tikzpicture}"
    )
  }, character(1))
  paste0("\\noindent\\makebox[\\textwidth][s]{%\n", paste(caixas, collapse = "\\hfill%\n"), "%\n}\n")
}

# Gera Faixa de Indicadores-Chave (KPIs) + Faixa Setorial Ordenada em LaTeX
gerar_kpis_latex <- function(kpis, kpis_extra = NULL) {
  out <- .render_linha_kpis_latex(kpis, eh_ranking_setor = FALSE)
  if (!is.null(kpis_extra) && length(kpis_extra) > 0) {
    out <- paste0(
      out,
      "\\par\\vspace{0.03cm}\n",
      "\\noindent{\\fontsize{6.2}{7.4}\\selectfont\\bfseries\\color{cinzasub}DESEMPENHO SETORIAL NO TRIMESTRE (DO MAIOR CRESCIMENTO AO MENOR)}\\par\\vspace{0.02cm}\n",
      .render_linha_kpis_latex(kpis_extra, eh_ranking_setor = TRUE)
    )
  }
  out
}

# Helper interno para gerar 1 faixa minimalista de indicadores em HTML (sem caixas pesadas)
.render_linha_kpis_html <- function(lista_kpis, cor_primaria, cor_fundo, cor_acento) {
  n <- length(lista_kpis)
  if (n == 0) return("")
  cols <- min(max(n, 1L), 5L)
  cards <- vapply(lista_kpis, function(k) {
    c_val <- if (!is.null(k$cor_valor_css)) k$cor_valor_css else cor_primaria
    c_brd <- if (!is.null(k$cor_borda_css)) k$cor_borda_css else cor_acento
    sprintf(
      '<div class="jet-kpi-card" style="background:transparent; border-top:2px solid %s; padding:10px 12px 8px 4px; min-width:0;"><div style="font-size:0.66rem; font-weight:700; color:#64748b; text-transform:uppercase; letter-spacing:0.06em;">%s</div><div style="font-size:1.18rem; font-weight:800; color:%s; margin:4px 0 3px 0; font-family:Newsreader,Georgia,serif; font-variant-numeric:tabular-nums;">%s</div><div style="font-size:0.74rem; color:#475569; line-height:1.3;">%s</div></div>',
      c_brd, escapar_html(k$rotulo), c_val, escapar_html(k$valor), escapar_html(k$sub)
    )
  }, character(1))
  sprintf(
    '<div class="jet-kpi-grid" style="display:grid; grid-template-columns:repeat(%d, minmax(0, 1fr)); gap:14px; margin:14px 0 12px 0; border-bottom:1px solid #cbd5e1; padding-bottom:10px;">%s</div>',
    cols, paste(cards, collapse = "")
  )
}

# Gera Faixa de Indicadores-Chave (KPIs) + Faixa Setorial Ordenada em HTML (para news.qmd)
gerar_kpis_html <- function(kpis, cor_primaria = "#0F2537", cor_fundo = "#FAF8F5", cor_acento = "#C8942B", kpis_extra = NULL) {
  out <- .render_linha_kpis_html(kpis, cor_primaria, cor_fundo, cor_acento)
  if (!is.null(kpis_extra) && length(kpis_extra) > 0) {
    out <- paste0(
      out,
      '<div style="font-size:0.68rem; font-weight:700; color:#64748b; text-transform:uppercase; letter-spacing:0.06em; margin:10px 0 -6px 0;">Desempenho Setorial (do maior crescimento ao menor)</div>',
      .render_linha_kpis_html(kpis_extra, cor_primaria, cor_fundo, cor_acento)
    )
  }
  out
}

# Gera o Bloco de Síntese Editorial dos Indicadores em LaTeX (Sem rótulos robóticos "[STATUS]")
gerar_bloco_diagnostico_script_latex <- function(lista_indicadores, otica = "mensal") {
  res_diag <- gerar_resumo_diagnostico_script(lista_indicadores, otica = otica)
  if (length(res_diag$itens) == 0) return("")
  itens_tex <- vapply(res_diag$itens, function(it) {
    paste0(
      "\\item \\textbf{\\color{corprimaria}", escapar_latex(it$nome), ":} ", escapar_latex(it$frase)
    )
  }, character(1))

  paste0(
    "\\par\\vspace{0.05cm}\n",
    "\\noindent{\\color{cinzaborda}\\hrule height 0.5pt}\\par\\vspace{4pt}\n",
    "{\\fontsize{7.8}{9.4}\\selectfont\\bfseries\\color{corprimaria}\\MakeUppercase{", escapar_latex(res_diag$titulo), "}}\\par\\vspace{2pt}\n",
    "{\\scriptsize\\color{cinzatexto}\n",
    "\\begin{itemize}[leftmargin=10pt, itemsep=1.2pt, topsep=1pt, parsep=0pt]\n",
    paste("  ", itens_tex, collapse = "\n"), "\n",
    "\\end{itemize}\n",
    "}\n",
    "\\noindent{\\color{cinzaborda}\\hrule height 0.5pt}\\par\\vspace{0.05cm}\n"
  )
}

# Gera o Bloco de Síntese Editorial em HTML (para news.qmd, sem caixas/colchetes robóticos)
gerar_bloco_diagnostico_script_html <- function(lista_indicadores, otica = "mensal", cor_primaria = "#0F2537", cor_fundo = "#FAF8F5", cor_acento = "#C8942B") {
  res_diag <- gerar_resumo_diagnostico_script(lista_indicadores, otica = otica)
  if (length(res_diag$itens) == 0) return("")
  lis <- vapply(res_diag$itens, function(it) {
    sprintf(
      '<li style="margin-bottom:5px; line-height:1.48;"><strong style="color:%s;">%s:</strong> %s</li>',
      cor_primaria, escapar_html(it$nome), escapar_html(it$frase)
    )
  }, character(1))

  sprintf(
    '<div style="border-top:1px solid #cbd5e1; border-bottom:1px solid #cbd5e1; padding:12px 2px; margin:14px 0 18px 0; font-size:0.86rem;"><div style="font-weight:700; color:%s; margin-bottom:7px; text-transform:uppercase; font-size:0.72rem; letter-spacing:0.08em;">%s</div><ul style="margin:0; padding-left:18px; color:#334155;">%s</ul></div>',
    cor_primaria, escapar_html(res_diag$titulo), paste(lis, collapse = "")
  )
}

# Gera Tabela Padronizada em LaTeX (Inquebrável via minipage, tipografia uniforme tabularx / resizebox)
gerar_tabela_padrao_latex <- function(df, col_nomes, alinhamento, titulo_tabela, nota_rodape = NULL) {
  if (is.null(df) || nrow(df) == 0) return("")
  cabecalho <- paste(vapply(col_nomes, function(cn) {
    paste0("\\textbf{\\color{white}", escapar_latex(cn), "}")
  }, character(1)), collapse = " & ")

  linhas <- vapply(seq_len(nrow(df)), function(i) {
    vals <- vapply(seq_len(ncol(df)), function(j) {
      escapar_latex(df[i, j])
    }, character(1))
    cor_linha <- if (i %% 2 == 0) "\\rowcolor{corfundo} " else ""
    paste0(cor_linha, paste(vals, collapse = " & "), " \\\\")
  }, character(1))

  nota_tex <- if (!is.null(nota_rodape) && nzchar(nota_rodape)) {
    paste0("\\par\\vspace{1.5pt}{\\fontsize{6.0}{7.2}\\selectfont\\color{cinzasub}\\textit{", escapar_latex(nota_rodape), "}}")
  } else ""

  # Se tiver até 5 colunas e ao menos uma coluna 'l', usa tabularx na largura atual (\linewidth)
  # para manter tamanho de fonte uniforme sem esticar; caso contrário, usa resizebox{\linewidth}{!}.
  chars_al <- strsplit(alinhamento, "")[[1]]
  pos_l <- which(chars_al == "l")
  usar_tabularx <- (ncol(df) <= 5) && (length(pos_l) >= 1)

  if (usar_tabularx) {
    idx_x <- tail(pos_l, 1)
    chars_al[idx_x] <- ">{\\raggedright\\arraybackslash}X"
    alinhamento_x <- paste(chars_al, collapse = "")
    bloco_tab <- paste0(
      "{\\fontsize{7.0}{8.6}\\selectfont\\setlength{\\tabcolsep}{4.5pt}%\n",
      "\\begin{tabularx}{\\linewidth}{", alinhamento_x, "}\n",
      "\\toprule\n",
      "\\rowcolor{corprimaria} ", cabecalho, " \\\\\n",
      "\\midrule\n",
      paste(linhas, collapse = "\n"), "\n",
      "\\bottomrule\n",
      "\\end{tabularx}%\n",
      "}"
    )
  } else {
    bloco_tab <- paste0(
      "{\\scriptsize\\setlength{\\tabcolsep}{4.5pt}%\n",
      "\\noindent\\resizebox{\\linewidth}{!}{%\n",
      "\\begin{tabular}{", alinhamento, "}\n",
      "\\toprule\n",
      "\\rowcolor{corprimaria} ", cabecalho, " \\\\\n",
      "\\midrule\n",
      paste(linhas, collapse = "\n"), "\n",
      "\\bottomrule\n",
      "\\end{tabular}%\n",
      "}%\n",
      "}"
    )
  }

  paste0(
    "\\par\\vspace{0.08cm}\n",
    "\\noindent\\begin{minipage}{\\linewidth}\n",
    "  \\noindent{\\fontsize{7.8}{9.5}\\selectfont\\bfseries\\color{corprimaria}\\rule{0.10cm}{0.20cm}\\hspace{4pt}", escapar_latex(titulo_tabela), "}\\par\\vspace{2pt}\n",
    "  ", bloco_tab, "\n",
    "  ", nota_tex, "\n",
    "\\end{minipage}\n",
    "\\par\\vspace{0.08cm}\n"
  )
}

# Pareia duas tabelas lado a lado em formato Periódico de 2 Colunas (com filete divisório central)
parear_tabelas_periodico_latex <- function(tab_esq, tab_dir) {
  converter_para_resize_col <- function(tx) {
    tx <- gsub("{\\fontsize{7.0}{8.6}\\selectfont\\setlength{\\tabcolsep}{4.5pt}%\n\\begin{tabularx}{\\linewidth}{",
               "{\\scriptsize\\setlength{\\tabcolsep}{4.0pt}%\n\\noindent\\resizebox{\\linewidth}{!}{%\n\\begin{tabular}{", tx, fixed = TRUE)
    tx <- gsub(">{\\raggedright\\arraybackslash}X", "l", tx, fixed = TRUE)
    tx <- gsub("\\end{tabularx}%\n}", "\\end{tabular}%\n}%\n}", tx, fixed = TRUE)
    tx
  }
  t1 <- converter_para_resize_col(tab_esq)
  t2 <- converter_para_resize_col(tab_dir)
  paste0(
    "\\par\\vspace{0.08cm}\n",
    "\\noindent\\begin{minipage}[t]{0.482\\textwidth}\n",
    t1,
    "\\end{minipage}\\hfill\n",
    "{\\color{cinzaborda}\\vrule width 0.4pt}\\hfill\n",
    "\\begin{minipage}[t]{0.482\\textwidth}\n",
    t2,
    "\\end{minipage}\n",
    "\\par\\vspace{0.08cm}\n"
  )
}

# Gera Tabela Padronizada em HTML (para news.qmd)
gerar_tabela_padrao_html <- function(df, col_nomes, alinhamento, titulo_tabela, nota_rodape = NULL, cor_primaria = "#0F2537") {
  if (is.null(df) || nrow(df) == 0) return("")
  ths <- paste(mapply(function(cn, al) {
    sprintf('<th style="background:%s; color:#ffffff; text-align:%s; padding:7px 9px;">%s</th>', cor_primaria, al, escapar_html(cn))
  }, col_nomes, alinhamento), collapse = "")

  trs <- paste(vapply(seq_len(nrow(df)), function(i) {
    tds <- paste(mapply(function(val, al) {
      sprintf('<td style="text-align:%s; padding:6px 9px;">%s</td>', al, escapar_html(val))
    }, as.list(df[i, ]), alinhamento), collapse = "")
    paste0("<tr>", tds, "</tr>")
  }, character(1)), collapse = "\n")

  nota_h <- if (!is.null(nota_rodape) && nzchar(nota_rodape)) {
    sprintf('<div style="font-size:0.75rem; color:#64748b; margin-top:4px;">%s</div>', escapar_html(nota_rodape))
  } else ""

  sprintf(
    '<div style="margin:12px 0;"><div style="font-weight:700; color:%s; font-size:0.90rem; margin-bottom:4px;">%s</div><div class="jet-tabela-scroll" style="overflow-x:auto; -webkit-overflow-scrolling:touch;"><table class="tabela-boletim-html"><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>%s</div>',
    cor_primaria, escapar_html(titulo_tabela), ths, trs, nota_h
  )
}

# Tema de Gráficos ggplot2 dos Boletins (Estilo Editorial FT / The Economist)
tema_grafico_boletim <- function() {
  theme_minimal(base_size = 8.6) +
    theme(
      plot.title         = element_text(face = "bold", size = 9.6, color = "#0F2537", hjust = 0, margin = margin(b = 2)),
      plot.subtitle      = element_text(size = 7.4, color = "#64748B", hjust = 0, margin = margin(b = 7)),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_blank(),
      legend.text        = element_text(size = 7.5, color = "#334155"),
      legend.margin      = margin(b = -2),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#CBD5E1", linewidth = 0.35, linetype = "dotted"),
      axis.line.x        = element_line(color = "#94A3B8", linewidth = 0.4),
      axis.text          = element_text(size = 7.4, color = "#475569"),
      axis.title         = element_text(size = 7.6, face = "bold", color = "#334155"),
      plot.background    = element_rect(fill = "#FFFFFF", color = NA),
      panel.background   = element_rect(fill = "#FFFFFF", color = NA),
      plot.margin        = margin(6, 8, 6, 4)
    )
}

salvar_grafico_duplo <- function(plot_obj, arq_pdf, arq_png, w = 6.6, h = 2.25) {
  ggsave(filename = file.path("boletins/figuras", arq_pdf), plot = plot_obj, width = w, height = h, device = "pdf")
  ggsave(filename = file.path("boletins/figuras", arq_png), plot = plot_obj, width = w, height = h, dpi = 160)
}

# Lê arquivo .txt da equipe em boletins/textos_equipe/
ler_arquivo_equipe <- function(caminho_txt) {
  linhas <- readLines(caminho_txt, encoding = "UTF-8", warn = FALSE)
  if (length(linhas) > 0) {
    linhas[1] <- sub("^\ufeff", "", linhas[1])
  }
  idx_conj <- which(trimws(linhas) == "=== ANALISE_CONJUNTURA ===")[1]
  idx_pers <- which(trimws(linhas) == "=== PERSPECTIVAS ===")[1]

  fim_meta <- if (!is.na(idx_conj)) idx_conj - 1 else length(linhas)
  linhas_meta <- linhas[seq_len(max(0, fim_meta))]

  extrair_campo <- function(chave, padrao = "") {
    pad <- paste0("^", chave, "\\s*:\\s*")
    m <- linhas_meta[grepl(pad, linhas_meta, ignore.case = TRUE)]
    if (length(m) == 0) return(padrao)
    trimws(sub(pad, "", m[1], ignore.case = TRUE))
  }

  analise_conj <- ""
  perspectivas <- ""
  if (!is.na(idx_conj)) {
    fim_c <- if (!is.na(idx_pers) && idx_pers > idx_conj) idx_pers - 1 else length(linhas)
    if (fim_c >= idx_conj + 1) {
      analise_conj <- paste(linhas[(idx_conj + 1):fim_c], collapse = "\n")
    }
  }
  if (!is.na(idx_pers) && idx_pers < length(linhas)) {
    perspectivas <- paste(linhas[(idx_pers + 1):length(linhas)], collapse = "\n")
  }

  list(
    tipo               = tolower(extrair_campo("TIPO", "flash_comex")),
    titulo             = extrair_campo("TITULO", "Boletim Econômico do Tocantins"),
    subtitulo          = extrair_campo("SUBTITULO", "Análise de conjuntura e indicadores oficiais"),
    autor              = extrair_campo("AUTOR", "Lorenzo Costa e Equipe | Jornal Econômico do Tocantins"),
    data               = extrair_campo("DATA", format(Sys.Date(), "%d/%m/%Y")),
    periodo            = extrair_campo("PERIODO", painel_dados$ultima_atualizacao),
    destaque_curto     = extrair_campo("DESTAQUE_CURTO", ""),
    analise_conjuntura = trimws(analise_conj),
    perspectivas       = trimws(perspectivas)
  )
}

preencher_template_tex <- function(caminho_template, substituicoes) {
  tex <- paste(readLines(caminho_template, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (ch in names(substituicoes)) {
    marcador <- paste0("{{", ch, "}}")
    val <- if (is.null(substituicoes[[ch]])) "" else as.character(substituicoes[[ch]])
    tex <- gsub(marcador, val, tex, fixed = TRUE)
  }
  tex
}

# Paleta Editorial Unificada JET (Azul-Marinho #0F2537 + Dourado/Ocre #C8942B sobre papel claro)
obter_identidade_editorial <- function(tipo) {
  tp <- tolower(trimws(tipo))
  if (grepl("^carta", tp)) {
    list(
      familia               = "carta",
      estilo_layout         = "periodico",
      publicar_no_site      = FALSE,
      tipo_upper            = "CARTA DE CONJUNTURA DO TOCANTINS",
      cor_primaria_hex      = "0F2537",
      cor_secundaria_hex    = "244B6B",
      cor_acento_hex        = "C8942B",
      cor_acento_header_hex = "C8942B",
      cor_acento_texto_hex  = "0F2537",
      cor_fundo_hex         = "FAF8F5",
      cor_primaria_css      = "#0F2537",
      cor_secundaria_css    = "#244B6B",
      cor_acento_css        = "#C8942B",
      cor_fundo_css         = "#FAF8F5"
    )
  } else if (grepl("^(dossie|fechamento)", tp)) {
    list(
      familia               = "dossie",
      estilo_layout         = "periodico",
      publicar_no_site      = TRUE,
      tipo_upper            = "DOSSIÊ ANUAL / SEMESTRAL (DA)",
      cor_primaria_hex      = "0F2537",
      cor_secundaria_hex    = "334155",
      cor_acento_hex        = "9A6C11",
      cor_acento_header_hex = "C8942B",
      cor_acento_texto_hex  = "0F2537",
      cor_fundo_hex         = "FAF8F5",
      cor_primaria_css      = "#0F2537",
      cor_secundaria_css    = "#334155",
      cor_acento_css        = "#9A6C11",
      cor_fundo_css         = "#FAF8F5"
    )
  } else {
    list(
      familia               = "flash",
      estilo_layout         = "padrao",
      publicar_no_site      = TRUE,
      tipo_upper            = "BOLETIM FLASH DE CONJUNTURA",
      cor_primaria_hex      = "0F2537",
      cor_secundaria_hex    = "244B6B",
      cor_acento_hex        = "C8942B",
      cor_acento_header_hex = "C8942B",
      cor_acento_texto_hex  = "0F2537",
      cor_fundo_hex         = "F8FAFC",
      cor_primaria_css      = "#0F2537",
      cor_secundaria_css    = "#244B6B",
      cor_acento_css        = "#C8942B",
      cor_fundo_css         = "#F8FAFC"
    )
  }
}

# Carrega os módulos geradores temáticos (FLASH + DA) e da Carta de Conjuntura (8 Seções)
source("scripts/modulos_boletins_tematicos.R")
source("scripts/modulos_carta_conjuntura.R")

# Localiza binário pdflatex
encontrar_pdflatex <- function() {
  bin <- Sys.which("pdflatex")
  if (nzchar(bin)) return(unname(bin))
  candidatos <- c(
    file.path(Sys.getenv("APPDATA"), "TinyTeX/bin/windows/pdflatex.exe"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/MiKTeX/miktex/bin/x64/pdflatex.exe"),
    "C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe"
  )
  for (c in candidatos) {
    if (file.exists(c)) return(c)
  }
  ""
}

bin_pdflatex <- encontrar_pdflatex()

# ------------------------------------------------------------------------------
# 3. PROCESSAMENTO DE TODOS OS ARQUIVOS .TXT EM boletins/textos_equipe/
# ------------------------------------------------------------------------------
arquivos_txt <- sort(list.files("boletins/textos_equipe", pattern = "\\.txt$", full.names = TRUE))
arquivos_txt <- arquivos_txt[!grepl("^_", basename(arquivos_txt))]

catalogo_boletins <- list()

for (idx_arq in seq_along(arquivos_txt)) {
  arq_txt <- arquivos_txt[idx_arq]
  slug    <- tools::file_path_sans_ext(basename(arq_txt))
  t0_b    <- proc.time()[["elapsed"]]

  meta <- ler_arquivo_equipe(arq_txt)
  id_ed <- obter_identidade_editorial(meta$tipo)
  edicao_str <- sprintf("#%02d/%s", idx_arq, format(Sys.Date(), "%Y"))

  log_info(slug, sprintf("Gerando [%s] (%s) — Paleta: #%s / #%s", meta$tipo, id_ed$tipo_upper, id_ed$cor_primaria_hex, id_ed$cor_acento_hex))

  tryCatch({
    if (identical(id_ed$familia, "carta")) {
      # ------------------------------------------------------------------------
      # CARTA DE CONJUNTURA INTEGRADA (8 SEÇÕES NA ORDEM EXATA)
      # ------------------------------------------------------------------------
      dados_cc <- gerar_carta_conjuntura_completa(
        slug       = slug,
        pd         = painel_dados,
        ex         = ex,
        meta_txt   = meta,
        cor_prim   = id_ed$cor_primaria_css,
        cor_sec    = id_ed$cor_secundaria_css,
        cor_acento = id_ed$cor_acento_css
      )

      tex_final <- preencher_template_tex(
        "boletins/templates/template_periodico_to.tex",
        list(
          COR_PRIMARIA_HEX             = id_ed$cor_primaria_hex,
          COR_SECUNDARIA_HEX           = id_ed$cor_secundaria_hex,
          COR_ACENTO_HEX               = id_ed$cor_acento_hex,
          COR_ACENTO_HEADER_HEX        = id_ed$cor_acento_header_hex,
          COR_ACENTO_TEXTO_HEX         = id_ed$cor_acento_texto_hex,
          COR_FUNDO_HEX                = id_ed$cor_fundo_hex,
          TITULO_DOCUMENTO             = escapar_latex(meta$titulo),
          SUBTITULO_DOCUMENTO          = escapar_latex(meta$subtitulo),
          TIPO_PUBLICACAO_UPPER        = escapar_latex(id_ed$tipo_upper),
          EDICAO_NUMERO                = escapar_latex(edicao_str),
          TEMA_NOME                    = escapar_latex(dados_cc$tema_nome),
          PERIODO_REFERENCIA           = escapar_latex(meta$periodo),
          DATA_PUBLICACAO              = escapar_latex(meta$data),
          AUTORIA_EQUIPE               = escapar_latex(meta$autor),
          NOTA_METODOLOGICA            = escapar_latex(dados_cc$nota_metodo),
          CORPO_COMPLETO_PERIODICO_LATEX = dados_cc$corpo_completo_periodico_latex
        )
      )
      caminho_tex <- file.path("boletins/gerados_tex", paste0(slug, ".tex"))
      writeLines(tex_final, caminho_tex, useBytes = TRUE)

    } else {
      # ------------------------------------------------------------------------
      # BOLETIM FLASH (1 COLUNA, 2-3 PÁGS.) OU DOSSIÊ ANUAL — DA (2 COLUNAS, 3-4 PÁGS.)
      # ------------------------------------------------------------------------
      e_dossie <- identical(id_ed$familia, "dossie")
      dados_tm <- gerar_conteudo_tematico(
        tipo       = meta$tipo,
        slug       = slug,
        pd         = painel_dados,
        ex         = ex,
        e_dossie   = e_dossie,
        cor_prim   = id_ed$cor_primaria_css,
        cor_sec    = id_ed$cor_secundaria_css,
        cor_acento = id_ed$cor_acento_css
      )

      kpis_tex  <- gerar_kpis_latex(dados_tm$kpis, dados_tm$kpis_setores)
      kpis_html <- gerar_kpis_html(dados_tm$kpis, id_ed$cor_primaria_css, id_ed$cor_fundo_css, id_ed$cor_acento_css, dados_tm$kpis_setores)
      diag_tex  <- gerar_bloco_diagnostico_script_latex(dados_tm$diag_lista, dados_tm$otica)
      diag_html <- gerar_bloco_diagnostico_script_html(dados_tm$diag_lista, dados_tm$otica, id_ed$cor_primaria_css, id_ed$cor_fundo_css, id_ed$cor_acento_css)

      conj_tex  <- paragrafos_para_latex(meta$analise_conjuntura)
      pers_tex  <- paragrafos_para_latex(meta$perspectivas)
      conj_html <- paragrafos_para_html(meta$analise_conjuntura)
      pers_html <- paragrafos_para_html(meta$perspectivas)

      if (e_dossie) {
        # Layout Periódico 2 Colunas (3 a 4 páginas) para o Dossiê Anual (DA)
        tabs_p1_p2 <- if (!is.null(dados_tm$tabs_principais_da) && nzchar(dados_tm$tabs_principais_da)) {
          dados_tm$tabs_principais_da
        } else {
          dados_tm$tabs_periodico
        }
        tabs_p3_extra <- if (!is.null(dados_tm$extra_da_tex) && nzchar(dados_tm$extra_da_tex)) {
          dados_tm$extra_da_tex
        } else ""

        corpo_da_tex <- paste0(
          kpis_tex, "\n\\vspace{0.03cm}\n",
          diag_tex, "\n\\vspace{0.03cm}\n",
          "\\begin{multicols}{2}\n",
          "\\secaoperiodico{1. Análise Estrutural Anual / Acumulado em 12 Meses}\n",
          "{\\small ", conj_tex, "}\n",
          "\\end{multicols}\n",
          "\\begin{center}\\includegraphics[width=0.84\\textwidth]{figuras/", dados_tm$fig_periodico, "}\\end{center}\n",
          tabs_p1_p2, "\n",
          "\\bannersecaoperiodico{2}{BALANÇO RETROSPECTIVO (12 MESES) E CENÁRIOS PROSPECTIVOS}{Séries Anualizadas, Consolidação Plurianual, Capilaridade Regional e Perspectivas da Equipe Técnica}\n",
          tabs_p3_extra, "\n\\vspace{0.03cm}\n",
          "\\begin{multicols}{2}\n",
          "\\secaoperiodico{2.1. Vetores Estruturais e Perspectivas para o Próximo Ciclo}\n",
          "{\\small ", pers_tex, "}\n\n",
          "\\secaoperiodico{2.2. Transmissão Intersetorial e Capilaridade Municipal}\n",
          "{\\small O acompanhamento sob a ótica acumulada em 12 meses evidencia que a dinâmica econômica do Tocantins apoia-se na complementaridade entre os polos regionais consolidados (Palmas, Araguaína, Gurupi, Porto Nacional, Paraíso do Tocantins e Colinas do Tocantins) e os municípios de expansão agroindustrial e logística ao longo do corredor da BR-153 e da Ferrovia Norte-Sul. A depuração de efeitos sazonais de curto prazo permite identificar ganhos persistentes de escala produtiva, formalização empresarial e densidade salarial.}\n\n",
          "\\secaoperiodico{2.3. Síntese Institucional e Metodológica (Ótica de 12 Meses)}\n",
          "{\\small A leitura conjunta das séries acumuladas em quatro trimestres e das comparações interanuais confirma a solidez dos fundamentos regionais do Tocantins neste eixo temático. O monitoramento contínuo das bases oficiais pelo Jornal Econômico do Tocantins — integrando extração automatizada via APIs públicas, tratamento de microdados e revisão analítica da equipe — permite distinguir oscilações transitórias de mudanças estruturais de tendência, subsidiando decisões de investimento privado e formulação de políticas públicas estaduais e municipais.}\n",
          "\\end{multicols}\n"
        )

        tex_final <- preencher_template_tex(
          "boletins/templates/template_periodico_to.tex",
          list(
            COR_PRIMARIA_HEX               = id_ed$cor_primaria_hex,
            COR_SECUNDARIA_HEX             = id_ed$cor_secundaria_hex,
            COR_ACENTO_HEX                 = id_ed$cor_acento_hex,
            COR_ACENTO_HEADER_HEX          = id_ed$cor_acento_header_hex,
            COR_ACENTO_TEXTO_HEX           = id_ed$cor_acento_texto_hex,
            COR_FUNDO_HEX                  = id_ed$cor_fundo_hex,
            TITULO_DOCUMENTO               = escapar_latex(meta$titulo),
            SUBTITULO_DOCUMENTO            = escapar_latex(meta$subtitulo),
            TIPO_PUBLICACAO_UPPER          = escapar_latex(id_ed$tipo_upper),
            EDICAO_NUMERO                  = escapar_latex(edicao_str),
            TEMA_NOME                      = escapar_latex(dados_tm$tema_nome),
            PERIODO_REFERENCIA             = escapar_latex(meta$periodo),
            DATA_PUBLICACAO                = escapar_latex(meta$data),
            AUTORIA_EQUIPE                 = escapar_latex(meta$autor),
            NOTA_METODOLOGICA              = escapar_latex(dados_tm$nota_metodo),
            CORPO_COMPLETO_PERIODICO_LATEX = corpo_da_tex
          )
        )
      } else {
        # Layout Boletim FLASH Executivo (2 páginas balanceadas)
        bloco_dest_tex <- if (nzchar(pers_tex)) {
          paste0("\n\\subsecaoboletim{Perspectivas e Cenários}\n{\\small ", pers_tex, "}\n")
        } else ""

        tex_final <- preencher_template_tex(
          "boletins/templates/template_boletim_to.tex",
          list(
            COR_PRIMARIA_HEX               = id_ed$cor_primaria_hex,
            COR_SECUNDARIA_HEX             = id_ed$cor_secundaria_hex,
            COR_FUNDO_HEX                  = id_ed$cor_fundo_hex,
            TITULO_DOCUMENTO               = escapar_latex(meta$titulo),
            SUBTITULO_DOCUMENTO            = escapar_latex(meta$subtitulo),
            TIPO_PUBLICACAO_UPPER          = escapar_latex(id_ed$tipo_upper),
            EDICAO_NUMERO                  = escapar_latex(edicao_str),
            TEMA_NOME                      = escapar_latex(dados_tm$tema_nome),
            PERIODO_REFERENCIA             = escapar_latex(meta$periodo),
            DATA_PUBLICACAO                = escapar_latex(meta$data),
            AUTORIA_EQUIPE                 = escapar_latex(meta$autor),
            NOTA_METODOLOGICA              = escapar_latex(dados_tm$nota_metodo),
            BLOCO_KPIS_LATEX               = kpis_tex,
            BLOCO_DIAGNOSTICO_SCRIPT_LATEX = diag_tex,
            TITULO_SECAO_ANALISE           = "1. Análise Conjuntural da Equipe e Cenários",
            TEXTO_ANALISE_LATEX            = conj_tex,
            BLOCO_DESTAQUES_LATEX          = bloco_dest_tex,
            TITULO_SECAO_DADOS             = "2. Panorama Estatístico, Gráfico e Tabelas Executivas do Período",
            BLOCO_TABELA_GRAFICO_LATEX     = dados_tm$bloco_tex
          )
        )
      }

      caminho_tex <- file.path("boletins/gerados_tex", paste0(slug, ".tex"))
      writeLines(tex_final, caminho_tex, useBytes = TRUE)
    }

    # Compilação PDF via pdflatex (2 passagens para resolver LastPage)
    caminho_pdf_final <- file.path("boletins/pdfs", paste0(slug, ".pdf"))
    tem_pdf <- FALSE
    num_pags <- NA_integer_

    if (nzchar(bin_pdflatex)) {
      dir_ant <- getwd()
      setwd("boletins")
      on.exit(setwd(dir_ant), add = TRUE)
      cmd_args <- c("-interaction=nonstopmode", "-halt-on-error", "-output-directory=pdfs", paste0("gerados_tex/", slug, ".tex"))
      out1 <- suppressWarnings(system2(bin_pdflatex, args = cmd_args, stdout = TRUE, stderr = TRUE))
      out2 <- suppressWarnings(system2(bin_pdflatex, args = cmd_args, stdout = TRUE, stderr = TRUE))
      setwd(dir_ant)
      on.exit(NULL)

      if (file.exists(caminho_pdf_final)) {
        tem_pdf <- TRUE
        arq_log <- file.path("boletins/pdfs", paste0(slug, ".log"))
        if (file.exists(arq_log)) {
          linhas_log <- readLines(arq_log, warn = FALSE)
          m_pg <- grep("Output written on .* \\([0-9]+ page", linhas_log, value = TRUE)
          if (length(m_pg) > 0) {
            num_pags <- suppressWarnings(as.integer(sub(".*\\(([0-9]+) page.*", "\\1", tail(m_pg, 1))))
          }
        }
        # Limpa arquivos auxiliares .aux, .log, .out em boletins/pdfs/
        unlink(file.path("boletins/pdfs", paste0(slug, c(".aux", ".log", ".out"))), force = TRUE)
      }
    }

    dt_b <- proc.time()[["elapsed"]] - t0_b
    if (tem_pdf) {
      log_ok(slug, sprintf("PDF compilado com sucesso (%s páginas) -> %s", ifelse(is.na(num_pags), "?", as.character(num_pags)), caminho_pdf_final), dt_b, "compilar_boletins.R")
    } else {
      log_aviso(slug, "Arquivo .tex gerado, mas PDF não compilado (verifique log LaTeX).", "Confira caracteres especiais ou pacotes LaTeX.", "compilar_boletins.R")
    }

    # Adiciona ao catálogo público do site (news.qmd) se for FLASH ou Dossiê Anual (Cartas ficam apenas em PDF p/ assinantes)
    if (isTRUE(id_ed$publicar_no_site)) {
      # Localiza thumbnail PNG gerada para este boletim (para cards e feed estilo G1)
      pngs_slug <- sort(list.files("boletins/figuras", pattern = paste0("^", slug, "_.*\\.png$")))
      thumb_png <- if (length(pngs_slug) > 0) paste0("boletins/figuras/", pngs_slug[1]) else ""

      # Gera 2 destaques curtos em formato bullet jornalístico (estilo G1)
      bullets_feed <- character(0)
      if (!is.null(dados_tm$kpis) && length(dados_tm$kpis) > 0) {
        bullets_feed <- vapply(dados_tm$kpis[seq_len(min(2L, length(dados_tm$kpis)))], function(k) {
          sprintf("%s: %s (%s)", k$rotulo, k$valor, k$sub)
        }, character(1))
      } else if (!is.null(dados_tm$diag_lista) && length(dados_tm$diag_lista) > 0) {
        bullets_feed <- vapply(dados_tm$diag_lista[seq_len(min(2L, length(dados_tm$diag_lista)))], function(d) {
          sprintf("%s: %s", d$nome, d$valor_fmt)
        }, character(1))
      }

      # Mapeia perfil humano do economista responsável pelo post
      autor_lower <- tolower(meta$autor)
      if (grepl("tarc", autor_lower)) {
        autor_id       <- "tarcisio"
        autor_nome     <- "Tarcisio Iago"
        autor_titulo   <- "Economista"
        autor_foto     <- "assets/autores/tarcisio_iago.jpg"
        autor_foto_pos <- "72% 18%"
        autor_bio      <- "Economista no Jornal Econômico do Tocantins (JET). Dedica-se à análise de conjuntura regional, mercado de trabalho formal, comércio exterior e dinâmica empresarial tocantinense."
      } else {
        autor_id       <- "lorenzo"
        autor_nome     <- "Lorenzo Costa"
        autor_titulo   <- "Graduado em Economia"
        autor_foto     <- "assets/autores/lorenzo_costa.jpg"
        autor_foto_pos <- "28% 18%"
        autor_bio      <- "Graduado em Economia pela UFT e editor do Jornal Econômico do Tocantins (JET). Atua na análise macroeconômica, dessazonalização de séries temporais e inteligência de dados econômicos."
      }

      catalogo_boletins[[length(catalogo_boletins) + 1]] <- list(
        slug                    = slug,
        tipo                    = meta$tipo,
        estilo_layout           = id_ed$estilo_layout,
        categoria               = dados_tm$categoria_nome,
        tema_nome               = dados_tm$tema_nome,
        titulo                  = meta$titulo,
        subtitulo               = meta$subtitulo,
        autor                   = meta$autor,
        autor_id                = autor_id,
        autor_nome              = autor_nome,
        autor_titulo            = autor_titulo,
        autor_foto              = autor_foto,
        autor_foto_pos          = autor_foto_pos,
        autor_bio               = autor_bio,
        data                    = meta$data,
        periodo                 = meta$periodo,
        fonte_curta             = dados_tm$fonte_curta,
        destaque_curto          = meta$destaque_curto,
        bullets_feed            = bullets_feed,
        thumb_png               = thumb_png,
        cor_primaria            = id_ed$cor_primaria_css,
        cor_secundaria          = id_ed$cor_secundaria_css,
        cor_acento              = id_ed$cor_acento_css,
        cor_fundo               = id_ed$cor_fundo_css,
        kpis_html               = kpis_html,
        diagnostico_script_html = diag_html,
        texto_auto_html         = dados_tm$texto_auto_html,
        visual_html             = dados_tm$bloco_html,
        conjuntura_html         = conj_html,
        perspectivas_html       = pers_html,
        tem_pdf                 = tem_pdf,
        num_paginas             = num_pags,
        pdf_url                 = paste0("boletins/pdfs/", slug, ".pdf")
      )
    }
  }, error = function(e) {
    log_erro_debug(
      script        = "compilar_boletins.R",
      bloco         = slug,
      origem        = arq_txt,
      erro_msg      = conditionMessage(e),
      fallback_acao = "Pulando este boletim e preservando os demais",
      como_corrigir = "Verifique o campo TIPO no .txt e a disponibilidade dos objetos em dados/painel_dados.rds."
    )
  })
}

# Ordena o catálogo do site (mais recentes primeiro) e salva em dados/catalogo_boletins.rds
catalogo_boletins <- rev(catalogo_boletins)
saveRDS(catalogo_boletins, "dados/catalogo_boletins.rds")

imprimir_resumo_diagnostico("RESUMO DE COMPILAÇÃO DE BOLETINS, DOSSIÊS ANUAIS E CARTAS DE CONJUNTURA")
cat(sprintf("Total de publicações públicas no site (news.qmd): %d | PDFs em 'boletins/pdfs/': %d\n",
            length(catalogo_boletins), length(list.files("boletins/pdfs", pattern = "\\.pdf$"))))
