# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/extrair_novo_caged.R
# Objetivo:
#   1. Consultar automaticamente o repositório oficial de Microdados do Novo CAGED
#      (PDET/MTE - ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/) ou a pasta
#      local 'dados/brutos/caged/'.
#   2. Baixar os arquivos mensais de microdados (.7z) no diretório temporário do
#      SSD D: ('D:/Journal/dados/tmp_microdados'):
#        - CAGEDMOVAAAAMM.7z (Movimentações Dentro do Prazo)
#        - CAGEDFORAAAAMM.7z (Movimentações Fora do Prazo - atualiza competências passadas)
#        - CAGEDEXCAAAAMM.7z (Exclusões de declarações anteriores)
#   3. Descompactar, filtrar imediatamente apenas o Estado do Tocantins (uf == 17)
#      e EXCLUIR IMEDIATAMENTE os arquivos brutos (.7z e .txt) para não ocupar
#      espaço em disco.
#   4. Atualizar a base histórica enxuta do Tocantins ('dados/novo_caged_to_base.rds')
#      reconciliando automaticamente a competência real da movimentação ('competenciamov')
#      com a competência da declaração ('competenciadec') e da exclusão ('competenciaexc').
#   5. Calcular todos os indicadores e tabelas para o Painel do Jornal e para os
#      Boletins, Cartas de Conjuntura e Dossiês Anuais ('dados/novo_caged_to_resumo.rds'):
#        - Admitidos, Desligados e Saldo Geral (Dentro do Prazo, Fora do Prazo e Consolidado)
#        - Saldo por Grande Setor Econômico (Serviços, Construção, Comércio, Agropecuária, Indústria)
#          e pelas 21 Seções da CNAE 2.0
#        - Saldo, Admitidos, Desligados e Salário Médio para os 139 Municípios do Tocantins
#        - Saldo, Admitidos, Desligados e Salário Médio por Faixa Etária (Idade)
#        - Saldo, Admitidos, Desligados e Salário Médio por Sexo
#        - Saldo, Admitidos, Desligados e Salário Médio por Grau de Instrução
#        - Saldo, Admitidos, Desligados e Salário Médio por Tipo de Estabelecimento
#        - Salário Mensal Declarado Médio e Mediano (Admitidos, Desligados e Geral)
# ==============================================================================

# 0. Garante diretório raiz no SSD D: e pastas temporárias fora do disco C:
if (!file.exists("index.qmd") && dir.exists("D:/Journal")) {
  setwd("D:/Journal")
}

dir_tmp_caged <- normalizePath("dados/tmp_microdados", winslash = "/", mustWork = FALSE)
dir.create(dir_tmp_caged, recursive = TRUE, showWarnings = FALSE)
dir.create("dados/brutos/caged", recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMP = dir_tmp_caged, TEMP = dir_tmp_caged, TMPDIR = dir_tmp_caged)

suppressPackageStartupMessages({
  for (pkg in c("data.table", "dplyr", "jsonlite", "readxl")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
  library(data.table)
  library(dplyr)
  library(jsonlite)
})

# ------------------------------------------------------------------------------
# 1. Localiza descompactador 7-Zip (ou pacote 'archive' do R como fallback)
# ------------------------------------------------------------------------------
encontrar_7z <- function() {
  caminhos <- c(
    "C:/Program Files/7-Zip/7z.exe",
    "C:/Program Files (x86)/7-Zip/7z.exe",
    Sys.which("7z"),
    Sys.which("7za")
  )
  caminhos <- caminhos[nzchar(caminhos) & file.exists(caminhos)]
  if (length(caminhos) > 0) return(unname(caminhos[1]))
  return(NULL)
}

descompactar_7z <- function(arquivo_7z, pasta_destino) {
  bin_7z <- encontrar_7z()
  if (!is.null(bin_7z)) {
    cmd <- sprintf('"%s" x "%s" -o"%s" -y', bin_7z, arquivo_7z, pasta_destino)
    status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (status == 0) return(TRUE)
  }
  if (requireNamespace("archive", quietly = TRUE)) {
    tryCatch({
      archive::archive_extract(arquivo_7z, dir = pasta_destino)
      return(TRUE)
    }, error = function(e) FALSE)
  }
  return(FALSE)
}

# ------------------------------------------------------------------------------
# 2. Descoberta automática de competências disponíveis no FTP do MTE
# ------------------------------------------------------------------------------
listar_competencias_ftp_mte <- function(anos = c(2025, 2026, as.integer(format(Sys.Date(), "%Y")))) {
  anos <- sort(unique(anos))
  comps <- integer(0)
  for (ano in anos) {
    url_ano <- sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/", ano)
    saida <- tryCatch(
      system2("curl.exe", args = c("-s", "--max-time", "12", url_ano), stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    if (length(saida) > 0) {
      matches <- regmatches(saida, regexpr("20[0-9]{4}", saida))
      if (length(matches) > 0) {
        comps <- c(comps, as.integer(matches))
      }
    }
  }
  sort(unique(comps))
}

# ------------------------------------------------------------------------------
# 3. Download, Leitura, Filtragem imediata (UF == 17) e Exclusão dos Microdados
# ------------------------------------------------------------------------------
normalizar_nomes_colunas <- function(nomes) {
  n <- iconv(nomes, from = "UTF-8", to = "ASCII//TRANSLIT")
  ind_na <- is.na(n)
  if (any(ind_na)) n[ind_na] <- nomes[ind_na]
  n <- tolower(gsub("[^a-zA-Z0-9_]", "", n))
  return(n)
}

ler_txt_caged_tocantins <- function(caminho_txt, tipo_arq, comp_ref) {
  if (!file.exists(caminho_txt)) return(NULL)
  dt <- tryCatch(
    data.table::fread(caminho_txt, sep = ";", encoding = "UTF-8", showProgress = FALSE),
    error = function(e) NULL
  )
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  
  data.table::setnames(dt, normalizar_nomes_colunas(names(dt)))
  if (!"uf" %in% names(dt)) return(NULL)
  
  # Filtra imediatamente Tocantins (código IBGE 17)
  dt_to <- dt[uf == 17]
  rm(dt)
  gc(verbose = FALSE)
  
  if (nrow(dt_to) == 0) return(NULL)
  
  # Converte salário para numérico caso venha como texto com vírgula
  for (col_sal in intersect(c("salario", "valorsalariofixo"), names(dt_to))) {
    if (is.character(dt_to[[col_sal]])) {
      data.table::set(dt_to, j = col_sal, value = suppressWarnings(as.numeric(gsub(",", ".", dt_to[[col_sal]], fixed = TRUE))))
    } else {
      data.table::set(dt_to, j = col_sal, value = as.numeric(dt_to[[col_sal]]))
    }
  }
  data.table::setalloccol(dt_to)
  
  # Garante colunas padronizadas de competência e indicadores
  if (!"competenciamov" %in% names(dt_to)) dt_to[, competenciamov := as.integer(comp_ref)]
  if (!"competenciadec" %in% names(dt_to)) dt_to[, competenciadec := as.integer(comp_ref)]
  if (!"competenciaexc" %in% names(dt_to)) dt_to[, competenciaexc := NA_integer_]
  if (!"indicadordeexclusao" %in% names(dt_to)) {
    dt_to[, indicadordeexclusao := ifelse(tipo_arq == "EXC", 1L, 0L)]
  }
  if (!"indicadordeforadoprazo" %in% names(dt_to)) {
    dt_to[, indicadordeforadoprazo := ifelse(tipo_arq == "FOR", 1L, 0L)]
  }
  
  dt_to[, tipo_arquivo := tipo_arq]
  dt_to[, competencia_arquivo := as.integer(comp_ref)]
  
  colunas_manter <- c(
    "competenciamov", "competenciadec", "competenciaexc", "competencia_arquivo",
    "tipo_arquivo", "uf", "municipio", "secao", "subclasse", "saldomovimentacao",
    "cbo2002ocupacao", "categoria", "graudeinstrucao", "idade", "horascontratuais",
    "racacor", "sexo", "tipoempregador", "tipoestabelecimento", "tipomovimentacao",
    "tipodedeficiencia", "indtrabintermitente", "indtrabparcial", "salario",
    "tamestabjan", "indicadoraprendiz", "origemdainformacao",
    "indicadordeexclusao", "indicadordeforadoprazo", "unidadesalariocodigo", "valorsalariofixo"
  )
  for (cm in setdiff(colunas_manter, names(dt_to))) {
    dt_to[, (cm) := NA]
  }
  dt_to <- dt_to[, ..colunas_manter]
  return(dt_to)
}

processar_competencia_caged <- function(comp, pasta_tmp = dir_tmp_caged, pasta_local = "dados/brutos/caged") {
  comp_int <- as.integer(comp)
  ano_int  <- comp_int %/% 100
  prefixos <- c(MOV = "CAGEDMOV", FOR = "CAGEDFOR", EXC = "CAGEDEXC")
  lista_dt <- list()
  teve_mov <- FALSE
  
  for (tipo in names(prefixos)) {
    pref <- prefixos[[tipo]]
    nome_base <- sprintf("%s%d", pref, comp_int)
    arq_7z_tmp  <- file.path(pasta_tmp, paste0(nome_base, ".7z"))
    arq_txt_tmp <- file.path(pasta_tmp, paste0(nome_base, ".txt"))
    arq_7z_loc  <- file.path(pasta_local, paste0(nome_base, ".7z"))
    arq_txt_loc <- file.path(pasta_local, paste0(nome_base, ".txt"))
    
    # 1. Verifica se já existe .txt ou .7z local na pasta dados/brutos/caged ou tmp
    if (file.exists(arq_txt_loc)) {
      file.copy(arq_txt_loc, arq_txt_tmp, overwrite = TRUE)
      try(file.remove(arq_txt_loc), silent = TRUE)
    } else if (file.exists(arq_7z_loc)) {
      file.copy(arq_7z_loc, arq_7z_tmp, overwrite = TRUE)
      try(file.remove(arq_7z_loc), silent = TRUE)
    }
    
    # 2. Se não tem o .txt nem o .7z ainda, baixa via FTP oficial do MTE
    if (!file.exists(arq_txt_tmp) && !file.exists(arq_7z_tmp)) {
      url_ftp <- sprintf(
        "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d/%s.7z",
        ano_int, comp_int, nome_base
      )
      status_dl <- system2(
        "curl.exe",
        args = c("-s", "--retry", "2", "--max-time", "180", url_ftp, "-o", arq_7z_tmp),
        stdout = FALSE, stderr = FALSE
      )
      if (status_dl != 0 || !file.exists(arq_7z_tmp) || file.info(arq_7z_tmp)$size < 100) {
        if (file.exists(arq_7z_tmp)) try(file.remove(arq_7z_tmp), silent = TRUE)
      }
    }
    
    # 3. Se temos o .7z mas não o .txt, descompacta no SSD D:
    if (!file.exists(arq_txt_tmp) && file.exists(arq_7z_tmp)) {
      descompactar_7z(arq_7z_tmp, pasta_tmp)
    }
    
    # 4. Lê e filtra imediatamente Tocantins (uf == 17)
    if (file.exists(arq_txt_tmp)) {
      if (tipo == "MOV") teve_mov <- TRUE
      dt_parte <- ler_txt_caged_tocantins(arq_txt_tmp, tipo_arq = tipo, comp_ref = comp_int)
      if (!is.null(dt_parte) && nrow(dt_parte) > 0) {
        lista_dt[[tipo]] <- dt_parte
      }
    }
    
    # 5. EXCLUSÃO IMEDIATA dos microdados brutos (.7z e .txt) para liberar espaço
    if (file.exists(arq_txt_tmp)) try(file.remove(arq_txt_tmp), silent = TRUE)
    if (file.exists(arq_7z_tmp))  try(file.remove(arq_7z_tmp), silent = TRUE)
  }
  
  if (!teve_mov && length(lista_dt) == 0) return(NULL)
  dt_comp <- data.table::rbindlist(lista_dt, use.names = TRUE, fill = TRUE)
  return(dt_comp)
}

# ------------------------------------------------------------------------------
# 4. Mapeamentos Oficiais do MTE (Setores, Escolaridade, Faixa Etária, etc.)
# ------------------------------------------------------------------------------
mapear_grande_setor_caged <- function(secao) {
  s <- toupper(trimws(as.character(secao)))
   dplyr::case_when(
    s == "A" ~ "Agropecuária",
    s %in% c("B", "C", "D", "E") ~ "Indústria",
    s == "F" ~ "Construção",
    s == "G" ~ "Comércio",
    s %in% c("H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U") ~ "Serviços",
    TRUE ~ "Não Identificado"
  )
}

mapear_faixa_etaria_caged <- function(idade) {
  id <- suppressWarnings(as.integer(idade))
  fx <- dplyr::case_when(
    is.na(id) | id <= 0 ~ "Não Identificado",
    id <= 17            ~ "Até 17 anos",
    id >= 18 & id <= 24 ~ "18 a 24 anos",
    id >= 25 & id <= 29 ~ "25 a 29 anos",
    id >= 30 & id <= 39 ~ "30 a 39 anos",
    id >= 40 & id <= 49 ~ "40 a 49 anos",
    id >= 50 & id <= 64 ~ "50 a 64 anos",
    id >= 65            ~ "65 anos ou mais"
  )
  niveis <- c("Até 17 anos", "18 a 24 anos", "25 a 29 anos", "30 a 39 anos",
              "40 a 49 anos", "50 a 64 anos", "65 anos ou mais", "Não Identificado")
  factor(fx, levels = niveis)
}

mapear_sexo_caged <- function(sexo) {
  sx <- suppressWarnings(as.integer(sexo))
  dplyr::case_when(
    sx == 1 ~ "Homem",
    sx == 3 ~ "Mulher",
    TRUE    ~ "Não Identificado"
  )
}

mapear_grau_instrucao_caged <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  desc <- dplyr::case_when(
    c_int == 1  ~ "Analfabeto",
    c_int == 2  ~ "Até 5ª Incompleto",
    c_int == 3  ~ "5ª Completo Fundamental",
    c_int == 4  ~ "6ª a 9ª Fundamental",
    c_int == 5  ~ "Fundamental Completo",
    c_int == 6  ~ "Médio Incompleto",
    c_int == 7  ~ "Médio Completo",
    c_int == 8  ~ "Superior Incompleto",
    c_int == 9  ~ "Superior Completo",
    c_int == 80 ~ "Pós-Graduação Completa",
    c_int == 10 ~ "Mestrado",
    c_int == 11 ~ "Doutorado",
    TRUE        ~ "Não Identificado"
  )
  niveis <- c(
    "Analfabeto", "Até 5ª Incompleto", "5ª Completo Fundamental",
    "6ª a 9ª Fundamental", "Fundamental Completo", "Médio Incompleto",
    "Médio Completo", "Superior Incompleto", "Superior Completo",
    "Pós-Graduação Completa", "Mestrado", "Doutorado", "Não Identificado"
  )
  factor(desc, levels = niveis)
}

mapear_escolaridade_agrupada_caged <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  desc <- dplyr::case_when(
    c_int %in% c(1, 2, 3, 4) ~ "Fundamental Incompleto ou Menos",
    c_int == 5               ~ "Fundamental Completo",
    c_int == 6               ~ "Médio Incompleto",
    c_int == 7               ~ "Médio Completo",
    c_int == 8               ~ "Superior Incompleto",
    c_int %in% c(9, 10, 11, 80) ~ "Superior Completo ou Pós-Graduação",
    TRUE                     ~ "Não Identificado"
  )
  niveis <- c(
    "Fundamental Incompleto ou Menos", "Fundamental Completo",
    "Médio Incompleto", "Médio Completo", "Superior Incompleto",
    "Superior Completo ou Pós-Graduação", "Não Identificado"
  )
  factor(desc, levels = niveis)
}

mapear_tipo_estabelecimento_caged <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  dplyr::case_when(
    c_int == 1 ~ "CNPJ",
    c_int == 3 ~ "CAEPF (Pessoa Física / Produtor Rural)",
    c_int == 4 ~ "CNO (Cadastro Nacional de Obra)",
    c_int == 5 ~ "CEI (CAGED)",
    TRUE       ~ "Não Identificado"
  )
}

# Cálculo de salário médio validado (filtro metodológico contra zeros e erros de digitação > R$ 150 mil)
calc_salario_medio <- function(sal, peso = NULL, min_val = 400, max_val = 150000) {
  ind <- !is.na(sal) & sal >= min_val & sal <= max_val
  if (!any(ind)) return(NA_real_)
  if (is.null(peso)) {
    return(round(mean(sal[ind]), 2))
  }
  p <- peso[ind]
  ind_pos <- p > 0
  if (!any(ind_pos)) return(round(mean(sal[ind]), 2))
  round(sum(sal[ind][ind_pos] * p[ind_pos]) / sum(p[ind_pos]), 2)
}

calc_salario_mediano <- function(sal, min_val = 400, max_val = 150000) {
  ind <- !is.na(sal) & sal >= min_val & sal <= max_val
  if (!any(ind)) return(NA_real_)
  round(stats::median(sal[ind]), 2)
}

formatar_competencia_curta <- function(comp_vec) {
  nomes_m <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  c_int <- as.integer(comp_vec)
  ano <- (c_int %/% 100) %% 100
  mes <- c_int %% 100
  sprintf("%s/%02d", nomes_m[mes], ano)
}

formatar_competencia_mm_aa <- function(comp) {
  c_int <- as.integer(comp)
  sprintf("%02d/%02d", c_int %% 100, (c_int %/% 100) %% 100)
}

formatar_competencia_extenso <- function(comp) {
  nomes_m <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
               "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
  c_int <- as.integer(comp)
  sprintf("%s/%d", nomes_m[c_int %% 100], c_int %/% 100)
}

# ------------------------------------------------------------------------------
# 5. Função Principal de Consolidação e Geração de Tabelas (Painel + Boletins)
# ------------------------------------------------------------------------------
gerar_resumo_novo_caged_to <- function(dt_base) {
  dt <- data.table::copy(dt_base)
  
  # Ponderação contábil oficial MTE:
  # - MOV (Dentro do Prazo) e FOR (Fora do Prazo): somam +1 em Admitidos (quando saldomovimentacao == 1)
  #   ou +1 em Desligados (quando saldomovimentacao == -1), e saldomovimentacao no Saldo.
  # - EXC (Exclusão): estorna (-1) o registro excluído na competência original ('competenciamov').
  dt[, sinal_exc := ifelse(tipo_arquivo == "EXC", -1L, 1L)]
  dt[, adm_efetivo   := as.integer(saldomovimentacao == 1L) * sinal_exc]
  dt[, desl_efetivo  := as.integer(saldomovimentacao == -1L) * sinal_exc]
  dt[, saldo_efetivo := as.integer(saldomovimentacao) * sinal_exc]
  
  # Mapeamentos categóricos
  dt[, Grande_Setor         := mapear_grande_setor_caged(secao)]
  dt[, Faixa_Etaria         := mapear_faixa_etaria_caged(idade)]
  dt[, Sexo_Desc            := mapear_sexo_caged(sexo)]
  dt[, Grau_Instrucao       := mapear_grau_instrucao_caged(graudeinstrucao)]
  dt[, Escolaridade_Grupo   := mapear_escolaridade_agrupada_caged(graudeinstrucao)]
  dt[, Tipo_Estab_Desc      := mapear_tipo_estabelecimento_caged(tipoestabelecimento)]
  
  # Competência vigente = maior competência com arquivo MOV (Dentro do Prazo)
  comps_mov <- sort(unique(dt[tipo_arquivo == "MOV", competencia_arquivo]))
  comp_vigente <- tail(comps_mov, 1)
  ano_vigente  <- comp_vigente %/% 100
  comps_ano_vigente <- comps_mov[comps_mov %/% 100 == ano_vigente]
  ultimas_12_comps  <- tail(comps_mov, 12)
  
  # ----------------------------------------------------------------------------
  # A) AUDITORIA E SÉRIE HISTÓRICA MENSAL (DENTRO DO PRAZO x FORA DO PRAZO x CONSOLIDADO)
  # ----------------------------------------------------------------------------
  serie_mov <- dt[tipo_arquivo == "MOV" & competenciamov %in% comps_mov, .(
    Admitidos_Dentro_Prazo  = sum(adm_efetivo, na.rm = TRUE),
    Desligados_Dentro_Prazo = sum(desl_efetivo, na.rm = TRUE),
    Saldo_Dentro_Prazo      = sum(saldo_efetivo, na.rm = TRUE)
  ), by = .(Competencia = competenciamov)]
  
  serie_for <- dt[tipo_arquivo == "FOR" & competenciamov %in% comps_mov, .(
    Admitidos_Fora_Prazo  = sum(adm_efetivo, na.rm = TRUE),
    Desligados_Fora_Prazo = sum(desl_efetivo, na.rm = TRUE),
    Saldo_Fora_Prazo      = sum(saldo_efetivo, na.rm = TRUE)
  ), by = .(Competencia = competenciamov)]
  
  serie_exc <- dt[tipo_arquivo == "EXC" & competenciamov %in% comps_mov, .(
    Admitidos_Exclusoes  = sum(adm_efetivo, na.rm = TRUE),
    Desligados_Exclusoes = sum(desl_efetivo, na.rm = TRUE),
    Saldo_Exclusoes      = sum(saldo_efetivo, na.rm = TRUE)
  ), by = .(Competencia = competenciamov)]
  
  serie_consolidada <- dt[competenciamov %in% comps_mov, .(
    Admitidos_Consolidado  = sum(adm_efetivo, na.rm = TRUE),
    Desligados_Consolidado = sum(desl_efetivo, na.rm = TRUE),
    Saldo_Consolidado      = sum(saldo_efetivo, na.rm = TRUE),
    Salario_Medio_Adm      = calc_salario_medio(salario[saldomovimentacao == 1 & tipo_arquivo != "EXC"]),
    Salario_Medio_Desl     = calc_salario_medio(salario[saldomovimentacao == -1 & tipo_arquivo != "EXC"]),
    Salario_Medio_Geral    = calc_salario_medio(salario[tipo_arquivo != "EXC"])
  ), by = .(Competencia = competenciamov)]
  
  df_serie_mensal <- data.frame(Competencia = comps_mov) %>%
    left_join(as.data.frame(serie_mov), by = "Competencia") %>%
    left_join(as.data.frame(serie_for), by = "Competencia") %>%
    left_join(as.data.frame(serie_exc), by = "Competencia") %>%
    left_join(as.data.frame(serie_consolidada), by = "Competencia") %>%
    mutate(
      across(c(Admitidos_Dentro_Prazo, Desligados_Dentro_Prazo, Saldo_Dentro_Prazo,
               Admitidos_Fora_Prazo, Desligados_Fora_Prazo, Saldo_Fora_Prazo,
               Admitidos_Exclusoes, Desligados_Exclusoes, Saldo_Exclusoes,
               Admitidos_Consolidado, Desligados_Consolidado, Saldo_Consolidado),
             ~ coalesce(as.integer(.), 0L)),
      Mes_Label = formatar_competencia_curta(Competencia),
      MM_AA     = vapply(Competencia, formatar_competencia_mm_aa, character(1))
    ) %>%
    arrange(Competencia)
  
  # Detalhamento do que veio no arquivo FORA DO PRAZO da última divulgação (comp_vigente)
  df_fora_prazo_ultima_divulgacao <- dt[tipo_arquivo == "FOR" & competencia_arquivo == comp_vigente, .(
    Competencia_Declarada    = comp_vigente,
    Admitidos_Fora_Prazo     = sum(adm_efetivo, na.rm = TRUE),
    Desligados_Fora_Prazo    = sum(desl_efetivo, na.rm = TRUE),
    Saldo_Fora_Prazo         = sum(saldo_efetivo, na.rm = TRUE)
  ), by = .(Competencia_Movimentacao = competenciamov)][order(Competencia_Movimentacao)] %>%
    as.data.frame()
  
  # Data.frame das últimas 12 competências para o gráfico de linhas do Painel
  df_ult12 <- df_serie_mensal %>% filter(Competencia %in% ultimas_12_comps)
  df_caged_painel <- data.frame(
    Mes   = factor(df_ult12$Mes_Label, levels = df_ult12$Mes_Label),
    Saldo = df_ult12$Saldo_Consolidado,
    Serie = "Saldo Mensal",
    stringsAsFactors = FALSE
  )
  
  # ----------------------------------------------------------------------------
  # B) RESUMO GERAL DO MÊS VIGENTE E DO ACUMULADO NO ANO
  # ----------------------------------------------------------------------------
  linha_mes_vig <- df_serie_mensal %>% filter(Competencia == comp_vigente)
  df_ano_vig    <- df_serie_mensal %>% filter(Competencia %in% comps_ano_vigente)
  
  dt_mes_vig <- dt[competenciamov == comp_vigente]
  dt_ano_vig <- dt[competenciamov %in% comps_ano_vigente]
  
  resumo_geral_mes <- list(
    competencia               = comp_vigente,
    mes_ano_curto             = formatar_competencia_mm_aa(comp_vigente),
    mes_ano_label             = formatar_competencia_curta(comp_vigente),
    mes_ano_extenso           = formatar_competencia_extenso(comp_vigente),
    admitidos_dentro_prazo    = linha_mes_vig$Admitidos_Dentro_Prazo,
    desligados_dentro_prazo   = linha_mes_vig$Desligados_Dentro_Prazo,
    saldo_dentro_prazo        = linha_mes_vig$Saldo_Dentro_Prazo,
    admitidos_consolidado     = linha_mes_vig$Admitidos_Consolidado,
    desligados_consolidado    = linha_mes_vig$Desligados_Consolidado,
    saldo_consolidado         = linha_mes_vig$Saldo_Consolidado,
    fora_prazo_declarado_mes  = sum(df_fora_prazo_ultima_divulgacao$Saldo_Fora_Prazo, na.rm = TRUE),
    salario_medio_admitidos   = linha_mes_vig$Salario_Medio_Adm,
    salario_medio_desligados  = linha_mes_vig$Salario_Medio_Desl,
    salario_medio_geral       = linha_mes_vig$Salario_Medio_Geral,
    salario_mediano_admitidos = calc_salario_mediano(dt_mes_vig[saldomovimentacao == 1 & tipo_arquivo != "EXC", salario]),
    salario_mediano_desligados= calc_salario_mediano(dt_mes_vig[saldomovimentacao == -1 & tipo_arquivo != "EXC", salario]),
    salario_mediano_geral     = calc_salario_mediano(dt_mes_vig[tipo_arquivo != "EXC", salario])
  )
  
  resumo_acumulado_ano <- list(
    ano                       = ano_vigente,
    competencias_incluidas    = comps_ano_vigente,
    admitidos_dentro_prazo    = sum(df_ano_vig$Admitidos_Dentro_Prazo, na.rm = TRUE),
    desligados_dentro_prazo   = sum(df_ano_vig$Desligados_Dentro_Prazo, na.rm = TRUE),
    saldo_dentro_prazo        = sum(df_ano_vig$Saldo_Dentro_Prazo, na.rm = TRUE),
    admitidos_for_acumulado   = sum(df_ano_vig$Admitidos_Fora_Prazo, na.rm = TRUE),
    desligados_for_acumulado  = sum(df_ano_vig$Desligados_Fora_Prazo, na.rm = TRUE),
    saldo_for_acumulado       = sum(df_ano_vig$Saldo_Fora_Prazo, na.rm = TRUE),
    admitidos_consolidado     = sum(df_ano_vig$Admitidos_Consolidado, na.rm = TRUE),
    desligados_consolidado    = sum(df_ano_vig$Desligados_Consolidado, na.rm = TRUE),
    saldo_consolidado         = sum(df_ano_vig$Saldo_Consolidado, na.rm = TRUE),
    salario_medio_admitidos   = calc_salario_medio(dt_ano_vig[saldomovimentacao == 1 & tipo_arquivo != "EXC", salario]),
    salario_medio_desligados  = calc_salario_medio(dt_ano_vig[saldomovimentacao == -1 & tipo_arquivo != "EXC", salario]),
    salario_medio_geral       = calc_salario_medio(dt_ano_vig[tipo_arquivo != "EXC", salario])
  )
  
  # Função auxiliar para agregar qualquer dimensão (Setor, Sexo, Idade, Escolaridade, Tipo Estab)
  agregar_dimensao <- function(dt_sub, col_grupo, nome_col_saida) {
    res <- dt_sub[, .(
      Admitidos          = sum(adm_efetivo, na.rm = TRUE),
      Desligados         = sum(desl_efetivo, na.rm = TRUE),
      Saldo              = sum(saldo_efetivo, na.rm = TRUE),
      Idade_Media_Adm    = round(mean(idade[saldomovimentacao == 1 & tipo_arquivo != "EXC" & idade > 0 & idade < 100], na.rm = TRUE), 1),
      Idade_Media_Desl   = round(mean(idade[saldomovimentacao == -1 & tipo_arquivo != "EXC" & idade > 0 & idade < 100], na.rm = TRUE), 1),
      Salario_Medio_Adm  = calc_salario_medio(salario[saldomovimentacao == 1 & tipo_arquivo != "EXC"]),
      Salario_Medio_Desl = calc_salario_medio(salario[saldomovimentacao == -1 & tipo_arquivo != "EXC"]),
      Salario_Medio_Geral= calc_salario_medio(salario[tipo_arquivo != "EXC"])
    ), by = c(col_grupo)]
    data.table::setnames(res, col_grupo, nome_col_saida)
    as.data.frame(res)
  }
  
  # ----------------------------------------------------------------------------
  # C) GRANDES SETORES ECONÔMICOS (Serviços, Construção, Comércio, Agropecuária, Indústria)
  # ----------------------------------------------------------------------------
  ordem_5_setores <- c("Serviços", "Construção", "Comércio", "Agropecuária", "Indústria")
  
  setores_mes <- data.frame(Setor = ordem_5_setores, stringsAsFactors = FALSE) %>%
    left_join(agregar_dimensao(dt_mes_vig, "Grande_Setor", "Setor"), by = "Setor") %>%
    mutate(across(c(Admitidos, Desligados, Saldo), ~ coalesce(as.integer(.), 0L)))
  
  setores_ano <- data.frame(Setor = ordem_5_setores, stringsAsFactors = FALSE) %>%
    left_join(agregar_dimensao(dt_ano_vig, "Grande_Setor", "Setor"), by = "Setor") %>%
    mutate(across(c(Admitidos, Desligados, Saldo), ~ coalesce(as.integer(.), 0L)))
  
  # Série histórica mensal por Grande Setor
  setores_historico_mensal <- dt[competenciamov %in% comps_mov & Grande_Setor %in% ordem_5_setores, .(
    Admitidos          = sum(adm_efetivo, na.rm = TRUE),
    Desligados         = sum(desl_efetivo, na.rm = TRUE),
    Saldo              = sum(saldo_efetivo, na.rm = TRUE),
    Salario_Medio_Adm  = calc_salario_medio(salario[saldomovimentacao == 1 & tipo_arquivo != "EXC"]),
    Salario_Medio_Desl = calc_salario_medio(salario[saldomovimentacao == -1 & tipo_arquivo != "EXC"])
  ), by = .(Competencia = competenciamov, Setor = Grande_Setor)][order(Competencia, Setor)] %>%
    as.data.frame()
  
  # Todas as 21 Seções Econômicas CNAE 2.0 (A a U)
  dic_caged <- if (file.exists("dados/brutos/dicionario_novo_caged.rds")) readRDS("dados/brutos/dicionario_novo_caged.rds") else NULL
  df_secoes_dic <- if (!is.null(dic_caged) && "seção" %in% names(dic_caged)) {
    dic_caged[["seção"]] %>% rename(Secao = 1, Descricao_Secao = 2)
  } else {
    data.frame(Secao = LETTERS[1:21], Descricao_Secao = LETTERS[1:21], stringsAsFactors = FALSE)
  }
  
  secoes_cnae_mes <- agregar_dimensao(dt_mes_vig, "secao", "Secao") %>%
    left_join(df_secoes_dic, by = "Secao") %>%
    mutate(Grande_Setor = mapear_grande_setor_caged(Secao)) %>%
    select(Secao, Descricao_Secao, Grande_Setor, everything()) %>%
    arrange(desc(Saldo))
  
  secoes_cnae_ano <- agregar_dimensao(dt_ano_vig, "secao", "Secao") %>%
    left_join(df_secoes_dic, by = "Secao") %>%
    mutate(Grande_Setor = mapear_grande_setor_caged(Secao)) %>%
    select(Secao, Descricao_Secao, Grande_Setor, everything()) %>%
    arrange(desc(Saldo))
  
  # ----------------------------------------------------------------------------
  # D) RECORTES PARA OS BOLETINS, CARTAS E DOSSIÊS (Idade, Sexo, Instrução, Tipo Estab.)
  # ----------------------------------------------------------------------------
  por_faixa_etaria_mes <- agregar_dimensao(dt_mes_vig, "Faixa_Etaria", "Faixa_Etaria") %>%
    filter(Faixa_Etaria != "Não Identificado" | Saldo != 0) %>%
    arrange(Faixa_Etaria)
  por_faixa_etaria_ano <- agregar_dimensao(dt_ano_vig, "Faixa_Etaria", "Faixa_Etaria") %>%
    filter(Faixa_Etaria != "Não Identificado" | Saldo != 0) %>%
    arrange(Faixa_Etaria)
  
  por_sexo_mes <- agregar_dimensao(dt_mes_vig, "Sexo_Desc", "Sexo") %>%
    filter(Sexo != "Não Identificado" | Saldo != 0) %>%
    arrange(desc(Saldo))
  por_sexo_ano <- agregar_dimensao(dt_ano_vig, "Sexo_Desc", "Sexo") %>%
    filter(Sexo != "Não Identificado" | Saldo != 0) %>%
    arrange(desc(Saldo))
  
  por_grau_instrucao_mes <- agregar_dimensao(dt_mes_vig, "Grau_Instrucao", "Grau_Instrucao") %>%
    filter(Grau_Instrucao != "Não Identificado" | Saldo != 0) %>%
    arrange(Grau_Instrucao)
  por_grau_instrucao_ano <- agregar_dimensao(dt_ano_vig, "Grau_Instrucao", "Grau_Instrucao") %>%
    filter(Grau_Instrucao != "Não Identificado" | Saldo != 0) %>%
    arrange(Grau_Instrucao)
  
  por_escolaridade_grupo_mes <- agregar_dimensao(dt_mes_vig, "Escolaridade_Grupo", "Escolaridade") %>%
    filter(Escolaridade != "Não Identificado" | Saldo != 0) %>%
    arrange(Escolaridade)
  por_escolaridade_grupo_ano <- agregar_dimensao(dt_ano_vig, "Escolaridade_Grupo", "Escolaridade") %>%
    filter(Escolaridade != "Não Identificado" | Saldo != 0) %>%
    arrange(Escolaridade)
  
  por_tipo_estabelecimento_mes <- agregar_dimensao(dt_mes_vig, "Tipo_Estab_Desc", "Tipo_Estabelecimento") %>%
    filter(Tipo_Estabelecimento != "Não Identificado" | Saldo != 0) %>%
    arrange(desc(Saldo))
  por_tipo_estabelecimento_ano <- agregar_dimensao(dt_ano_vig, "Tipo_Estab_Desc", "Tipo_Estabelecimento") %>%
    filter(Tipo_Estabelecimento != "Não Identificado" | Saldo != 0) %>%
    arrange(desc(Saldo))
  
  # ----------------------------------------------------------------------------
  # E) MUNICÍPIOS DO TOCANTINS (Todos os 139 Municípios)
  # ----------------------------------------------------------------------------
  df_mun_ibge <- if (file.exists("dados/brutos/municipios_to_ibge.rds")) {
    readRDS("dados/brutos/municipios_to_ibge.rds")
  } else if (!is.null(dic_caged) && !is.null(dic_caged$municipios_to_ibge)) {
    dic_caged$municipios_to_ibge
  } else {
    data.frame(cod_mun_7 = integer(0), cod_mun_6 = integer(0), Municipio = character(0))
  }
  
  mun_mes <- dt_mes_vig[, .(
    Admitidos_Mes          = sum(adm_efetivo, na.rm = TRUE),
    Desligados_Mes         = sum(desl_efetivo, na.rm = TRUE),
    Saldo_Mes              = sum(saldo_efetivo, na.rm = TRUE),
    Salario_Medio_Adm_Mes  = calc_salario_medio(salario[saldomovimentacao == 1 & tipo_arquivo != "EXC"]),
    Salario_Medio_Desl_Mes = calc_salario_medio(salario[saldomovimentacao == -1 & tipo_arquivo != "EXC"]),
    Salario_Medio_Geral_Mes= calc_salario_medio(salario[tipo_arquivo != "EXC"])
  ), by = .(cod_mun_6 = as.integer(municipio))]
  
  mun_ano <- dt_ano_vig[, .(
    Admitidos_Ano          = sum(adm_efetivo, na.rm = TRUE),
    Desligados_Ano         = sum(desl_efetivo, na.rm = TRUE),
    Saldo_Ano              = sum(saldo_efetivo, na.rm = TRUE),
    Salario_Medio_Adm_Ano  = calc_salario_medio(salario[saldomovimentacao == 1 & tipo_arquivo != "EXC"]),
    Salario_Medio_Desl_Ano = calc_salario_medio(salario[saldomovimentacao == -1 & tipo_arquivo != "EXC"]),
    Salario_Medio_Geral_Ano= calc_salario_medio(salario[tipo_arquivo != "EXC"])
  ), by = .(cod_mun_6 = as.integer(municipio))]
  
  df_municipios_139 <- df_mun_ibge %>%
    left_join(as.data.frame(mun_mes), by = "cod_mun_6") %>%
    left_join(as.data.frame(mun_ano), by = "cod_mun_6") %>%
    mutate(
      across(c(Admitidos_Mes, Desligados_Mes, Saldo_Mes,
               Admitidos_Ano, Desligados_Ano, Saldo_Ano), ~ coalesce(as.integer(.), 0L))
    ) %>%
    arrange(desc(Saldo_Mes), desc(Admitidos_Mes), Municipio)
  
  # Tabela para o Painel do Jornal (com nome das colunas dinâmico para RAIS e Novo CAGED)
  mm_aa_str <- formatar_competencia_mm_aa(comp_vigente)
  nome_col_saldo <- sprintf("Saldo (Novo CAGED) %s", mm_aa_str)
  
  # Integra automaticamente o Estoque Anual da RAIS dos 139 municípios ('dados/rais_to_resumo.rds')
  resumo_rais_local <- if (file.exists("dados/rais_to_resumo.rds")) readRDS("dados/rais_to_resumo.rds") else NULL
  nome_col_estoque  <- if (!is.null(resumo_rais_local) && !is.null(resumo_rais_local$nome_coluna_estoque_mun)) {
    resumo_rais_local$nome_coluna_estoque_mun
  } else {
    "Estoque (RAIS) 2025"
  }
  
  if (!is.null(resumo_rais_local) && !is.null(resumo_rais_local$vinculos_por_municipio)) {
    df_rais_mun <- resumo_rais_local$vinculos_por_municipio %>%
      select(cod_mun_6, Estoque_RAIS = Estoque_Vinculos)
    df_municipios_painel <- df_municipios_139 %>%
      left_join(df_rais_mun, by = "cod_mun_6") %>%
      mutate(Estoque_RAIS = coalesce(as.integer(Estoque_RAIS), 0L)) %>%
      arrange(desc(Estoque_RAIS), desc(Saldo_Mes), Municipio) %>%
      transmute(
        Município    = Municipio,
        Estoque_Temp = Estoque_RAIS,
        Saldo_Temp   = Saldo_Mes
      )
  } else {
    estoque_rais_ref <- c(
      "Palmas"               = 220673L,
      "Araguaína"            = 47010L,
      "Gurupi"               = 20492L,
      "Porto Nacional"       = 14172L,
      "Paraíso do Tocantins" = 12288L,
      "Colinas do Tocantins" = 5707L,
      "Guaraí"               = 3608L,
      "Formoso do Araguaia"  = 3387L,
      "Lagoa da Confusão"    = 3137L,
      "Dianópolis"           = 3122L
    )
    df_municipios_painel <- df_municipios_139 %>%
      mutate(Estoque_RAIS = coalesce(unname(estoque_rais_ref[Municipio]), 0L)) %>%
      arrange(desc(Estoque_RAIS), desc(Saldo_Mes), Municipio) %>%
      transmute(
        Município    = Municipio,
        Estoque_Temp = Estoque_RAIS,
        Saldo_Temp   = Saldo_Mes
      )
  }
  names(df_municipios_painel)[2] <- nome_col_estoque
  names(df_municipios_painel)[3] <- nome_col_saldo
  
  # ValueBox formatado do Saldo Novo CAGED no mês vigente
  s_val <- resumo_geral_mes$saldo_consolidado
  sinal_vb <- if (s_val > 0) "+" else if (s_val < 0) "-" else ""
  vb_saldo_str <- paste0(sinal_vb, format(abs(s_val), big.mark = ".", decimal.mark = ",", trim = TRUE))
  
  resumo_final <- list(
    data_processamento              = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    competencia_vigente             = comp_vigente,
    mes_ano_curto                   = mm_aa_str,
    mes_ano_label                   = formatar_competencia_curta(comp_vigente),
    mes_ano_extenso                 = formatar_competencia_extenso(comp_vigente),
    nome_coluna_saldo_mun           = nome_col_saldo,
    vb_saldo_caged                  = vb_saldo_str,
    resumo_geral_mes                = resumo_geral_mes,
    resumo_acumulado_ano            = resumo_acumulado_ano,
    auditoria_series_mensais        = df_serie_mensal,
    fora_prazo_ultima_divulgacao    = df_fora_prazo_ultima_divulgacao,
    df_caged_painel                 = df_caged_painel,
    df_setores_painel               = setores_mes %>% select(Setor, Saldo),
    df_municipios_painel            = df_municipios_painel,
    setores_mes_vigente             = setores_mes,
    setores_acumulado_ano           = setores_ano,
    setores_historico_mensal        = setores_historico_mensal,
    secoes_cnae_mes_vigente         = secoes_cnae_mes,
    secoes_cnae_acumulado_ano       = secoes_cnae_ano,
    municipios_139_to               = df_municipios_139,
    por_faixa_etaria_mes            = por_faixa_etaria_mes,
    por_faixa_etaria_ano            = por_faixa_etaria_ano,
    por_sexo_mes                    = por_sexo_mes,
    por_sexo_ano                    = por_sexo_ano,
    por_grau_instrucao_mes          = por_grau_instrucao_mes,
    por_grau_instrucao_ano          = por_grau_instrucao_ano,
    por_escolaridade_grupo_mes      = por_escolaridade_grupo_mes,
    por_escolaridade_grupo_ano      = por_escolaridade_grupo_ano,
    por_tipo_estabelecimento_mes    = por_tipo_estabelecimento_mes,
    por_tipo_estabelecimento_ano    = por_tipo_estabelecimento_ano
  )
  
  return(resumo_final)
}

# ------------------------------------------------------------------------------
# 6. Orquestrador de Sincronização (FTP MTE / Pasta Local -> Base TO -> Resumo)
# ------------------------------------------------------------------------------
executar_extracao_novo_caged <- function(competencias_alvo = NULL, ultimos_n_meses = 12, forcar_reprocessamento = FALSE) {
  arq_base_to   <- "dados/novo_caged_to_base.rds"
  arq_resumo_to <- "dados/novo_caged_to_resumo.rds"
  
  dt_base_atual <- if (file.exists(arq_base_to)) {
    data.table::as.data.table(readRDS(arq_base_to))
  } else {
    NULL
  }
  comps_ja_salvas <- if (!is.null(dt_base_atual) && "competencia_arquivo" %in% names(dt_base_atual)) {
    sort(unique(as.integer(dt_base_atual$competencia_arquivo)))
  } else {
    integer(0)
  }
  
  # Verifica se há arquivos avulsos colocados manualmente em dados/brutos/caged ou dados/tmp_microdados
  arqs_locais <- c(
    list.files("dados/brutos/caged", pattern = "^CAGED(MOV|FOR|EXC)20[0-9]{4}\\.(7z|txt)$", ignore.case = TRUE),
    list.files(dir_tmp_caged,        pattern = "^CAGED(MOV|FOR|EXC)20[0-9]{4}\\.(7z|txt)$", ignore.case = TRUE)
  )
  comps_locais <- if (length(arqs_locais) > 0) {
    sort(unique(as.integer(regmatches(arqs_locais, regexpr("20[0-9]{4}", arqs_locais)))))
  } else {
    integer(0)
  }
  
  if (is.null(competencias_alvo)) {
    comps_ftp <- listar_competencias_ftp_mte()
    if (length(comps_ftp) > 0) {
      janela_ftp <- tail(comps_ftp, ultimos_n_meses)
      if (forcar_reprocessamento) {
        competencias_alvo <- sort(unique(c(janela_ftp, comps_locais)))
      } else {
        competencias_alvo <- sort(unique(c(setdiff(janela_ftp, comps_ja_salvas), comps_locais)))
      }
    } else {
      competencias_alvo <- comps_locais
    }
  } else {
    competencias_alvo <- sort(unique(as.integer(competencias_alvo)))
  }
  
  houve_atualizacao_base <- FALSE
  if (length(competencias_alvo) > 0) {
    cat(sprintf("-> [Novo CAGED] Processando %d competencia(s) do MTE para o Tocantins: %s\n",
                length(competencias_alvo), paste(competencias_alvo, collapse = ", ")))
    
    # Processa em lotes de até 4 competências por vez (pré-download paralelo via curl.exe + filtragem UF=17 + exclusão imediata)
    lotes <- split(competencias_alvo, ceiling(seq_along(competencias_alvo) / 4))
    for (lote in lotes) {
      # Pré-download paralelo do lote caso haja mais de 1 arquivo a baixar do FTP
      args_dl <- c("-s", "--parallel", "--parallel-max", "6", "--retry", "2", "--max-time", "240")
      tem_dl_lote <- FALSE
      for (comp in lote) {
        ano_c <- as.integer(comp) %/% 100
        for (pref in c("CAGEDMOV", "CAGEDFOR", "CAGEDEXC")) {
          nome_b <- sprintf("%s%d", pref, as.integer(comp))
          arq_7z_t  <- file.path(dir_tmp_caged, paste0(nome_b, ".7z"))
          arq_txt_t <- file.path(dir_tmp_caged, paste0(nome_b, ".txt"))
          arq_7z_l  <- file.path("dados/brutos/caged", paste0(nome_b, ".7z"))
          arq_txt_l <- file.path("dados/brutos/caged", paste0(nome_b, ".txt"))
          if (!file.exists(arq_7z_t) && !file.exists(arq_txt_t) && !file.exists(arq_7z_l) && !file.exists(arq_txt_l)) {
            url_f <- sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d/%s.7z", ano_c, as.integer(comp), nome_b)
            args_dl <- c(args_dl, url_f, "-o", arq_7z_t)
            tem_dl_lote <- TRUE
          }
        }
      }
      if (tem_dl_lote) {
        cat(sprintf("   * Baixando lote do FTP MTE (%s)...\n", paste(lote, collapse = ", ")))
        try(system2("curl.exe", args = args_dl, stdout = FALSE, stderr = FALSE), silent = TRUE)
      }
      
      novas_partes_lote <- list()
      for (comp in lote) {
        cat(sprintf("   * Lendo/filtrando microdados CAGEDMOV/FOR/EXC %d (UF=17) e excluindo brutos...\n", comp))
        dt_c <- processar_competencia_caged(comp)
        if (!is.null(dt_c) && nrow(dt_c) > 0) {
          novas_partes_lote[[as.character(comp)]] <- dt_c
          houve_atualizacao_base <- TRUE
        }
      }
      if (length(novas_partes_lote) > 0) {
        dt_novas <- data.table::rbindlist(novas_partes_lote, use.names = TRUE, fill = TRUE)
        if (!is.null(dt_base_atual) && nrow(dt_base_atual) > 0) {
          dt_base_atual <- dt_base_atual[!competencia_arquivo %in% as.integer(names(novas_partes_lote))]
          dt_base_atual <- data.table::rbindlist(list(dt_base_atual, dt_novas), use.names = TRUE, fill = TRUE)
        } else {
          dt_base_atual <- dt_novas
        }
        saveRDS(dt_base_atual, arq_base_to, compress = "xz")
      }
    }
    if (houve_atualizacao_base) {
      cat(sprintf("   [OK] Base historica do Tocantins ('%s') atualizada (%d registros de movimentacao).\n",
                  arq_base_to, nrow(dt_base_atual)))
    }
  } else {
    cat(sprintf("-> [Novo CAGED] Base local sincronizada com a ultima divulgacao do MTE (%d).\n",
                if (length(comps_ja_salvas) > 0) max(comps_ja_salvas) else NA_integer_))
  }
  
  # Garante limpeza total da pasta temporária de microdados
  arqs_residuais <- list.files(dir_tmp_caged, pattern = "\\.(7z|txt)$", full.names = TRUE, ignore.case = TRUE)
  if (length(arqs_residuais) > 0) try(file.remove(arqs_residuais), silent = TRUE)
  
  if (!is.null(dt_base_atual) && nrow(dt_base_atual) > 0) {
    if (houve_atualizacao_base || !file.exists(arq_resumo_to) || forcar_reprocessamento) {
      resumo_caged <- gerar_resumo_novo_caged_to(dt_base_atual)
      saveRDS(resumo_caged, arq_resumo_to)
      cat(sprintf("   [OK] Resumo Novo CAGED TO (%s): Adm = %s | Desl = %s | Saldo Consolidado = %s | Salario Medio Adm = R$ %s\n",
                  resumo_caged$mes_ano_curto,
                  format(resumo_caged$resumo_geral_mes$admitidos_consolidado, big.mark = ".", decimal.mark = ","),
                  format(resumo_caged$resumo_geral_mes$desligados_consolidado, big.mark = ".", decimal.mark = ","),
                  resumo_caged$vb_saldo_caged,
                  format(resumo_caged$resumo_geral_mes$salario_medio_admitidos, big.mark = ".", decimal.mark = ",", nsmall = 2)))
      return(invisible(resumo_caged))
    } else {
      return(invisible(readRDS(arq_resumo_to)))
    }
  }
  return(NULL)
}

# ------------------------------------------------------------------------------
# 7. Execução Direta vs. Chamada Modular pelo atualizar_dados.R
# ------------------------------------------------------------------------------
args_cmd <- commandArgs(trailingOnly = TRUE)
comps_cmd <- NULL
if (length(args_cmd) > 0) {
  nums <- regmatches(args_cmd, regexpr("20[0-9]{4}", args_cmd))
  if (length(nums) > 0) comps_cmd <- as.integer(nums)
}

resumo_novo_caged_to <- executar_extracao_novo_caged(competencias_alvo = comps_cmd, ultimos_n_meses = 12)

# Se o usuário rodou 'Rscript scripts/extrair_novo_caged.R' diretamente no terminal
# (no dia em que sair a divulgação do Novo CAGED), deposita automaticamente no Jornal e Boletins!
if (!exists(".chamado_por_atualizar_dados") || !isTRUE(.chamado_por_atualizar_dados)) {
  if (any(grepl("extrair_novo_caged\\.R$", commandArgs(), ignore.case = TRUE)) && ! "--apenas-extrair" %in% args_cmd) {
    cat("-> Depositando dados extraidos do Novo CAGED no Jornal (painel_dados.rds) e Boletins...\n")
    .chamado_por_atualizar_dados <- TRUE
    if (file.exists("scripts/atualizar_dados.R")) {
      source("scripts/atualizar_dados.R", local = FALSE)
    }
    if (file.exists("scripts/compilar_boletins.R")) {
      source("scripts/compilar_boletins.R", local = FALSE)
    }
  }
}
