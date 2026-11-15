# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS
# Script: scripts/extrair_rais.R
# Objetivo:
#   1. Consultar automaticamente o repositório oficial de Microdados da RAIS
#      (PDET/MTE - ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/) ou a pasta
#      local 'dados/brutos/rais/' / 'dados/tmp_microdados/'.
#   2. Baixar temporariamente no SSD D: ('D:/Journal/dados/tmp_microdados')
#      os microdados anuais de Vínculos (RAIS_VINC_PUB_NORTE.7z) e de
#      Estabelecimentos (RAIS_ESTAB_PUB.7z).
#   3. Descompactar, ler e filtrar IMEDIATAMENTE apenas o Estado do Tocantins
#      (UF == 17 / códigos IBGE 170000 a 179999) e EXCLUIR IMEDIATAMENTE os
#      arquivos brutos (.7z, .COMT, .txt) para não ocupar espaço em disco.
#   4. Aplicar o filtro metodológico oficial do Painel Online da RAIS na base
#      de Vínculos:
#        - Vínculos Ativos em 31/12 (ind_vinculo_ativo_31_12_codigo == 1)
#        - Excluindo os Vínculos Abandonados (ind_vinculo_abandonado_codigo == 0)
#   5. Gerar e salvar ('dados/rais_to_resumo.rds') todas as informações para o
#      Painel do Jornal e para os Boletins, Cartas de Conjuntura e Dossiês:
#      A) VÍNCULOS ATIVOS EM 31/12 (Sem Vínculos Abandonados):
#         - Estoque Total Anual no Estado e por Município (todos os 139 municípios)
#         - Remuneração Média (Nominal Anual, Dezembro e em Salários Mínimos)
#           para o Estado e por Município (todos os 139 municípios)
#         - Estoque e Remuneração Média por Faixa Etária
#         - Estoque e Remuneração Média por Raça/Cor
#         - Estoque e Remuneração Média por Gênero/Sexo
#         - Estoque e Remuneração Média por Grau de Instrução (detalhado e agrupado)
#         - Estoque e Remuneração Média pelas 21 Grandes Seções da CNAE 2.0 (A a U)
#           e pelos 5 Grandes Setores Econômicos (Estado e Municípios)
#      B) ESTABELECIMENTOS (Estado e todos os 139 Municípios):
#         - Número Total de Estabelecimentos (Total declarado, Com Atividade no Ano,
#           Com Vínculos Ativos em 31/12, RAIS Negativa e Optantes pelo Simples)
#         - Vínculos e Estabelecimentos por Natureza Jurídica (Macrogrupos CONCLA
#           e detalhamento 4 dígitos, por Município e Estado)
#         - Quantidade de Estabelecimentos (e Vínculos) por Tamanho do
#           Estabelecimento de acordo com empregados ativos em 31/12 (Município e Estado)
#         - Quantidade de Estabelecimentos (e Vínculos) por Tipo de Estabelecimento
#           (CNPJ, CAEPF, CNO/CEI, por Município e Estado)
# ==============================================================================

# 0. Garante diretório raiz no SSD D: e pastas temporárias fora do disco C:
if (!file.exists("index.qmd") && dir.exists("D:/Journal")) {
  setwd("D:/Journal")
}

dir_tmp_rais <- normalizePath("dados/tmp_microdados", winslash = "/", mustWork = FALSE)
dir.create(dir_tmp_rais, recursive = TRUE, showWarnings = FALSE)
dir.create("dados/brutos/rais", recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMP = dir_tmp_rais, TEMP = dir_tmp_rais, TMPDIR = dir_tmp_rais)

suppressPackageStartupMessages({
  for (pkg in c("data.table", "dplyr", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
  library(data.table)
  library(dplyr)
  library(jsonlite)
})

# ------------------------------------------------------------------------------
# 1. Funções Auxiliares de Descompactação (7-Zip) e FTP MTE
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

descobrir_ultimo_ano_rais_ftp <- function(ano_padrao = 2025L) {
  url_rais <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/"
  saida <- tryCatch(
    system2("curl.exe", args = c("-s", "--max-time", "15", url_rais), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (length(saida) > 0) {
    matches <- regmatches(saida, regexpr("(19|20)[0-9]{2}$", trimws(saida)))
    anos <- suppressWarnings(as.integer(matches))
    anos <- anos[!is.na(anos) & anos >= 2010 & anos <= 2035]
    if (length(anos) > 0) return(max(anos))
  }
  return(as.integer(ano_padrao))
}

normalizar_nomes_colunas_rais <- function(nomes) {
  n <- iconv(nomes, from = "Latin1", to = "ASCII//TRANSLIT")
  ind_na <- is.na(n)
  if (any(ind_na)) {
    n2 <- iconv(nomes[ind_na], from = "UTF-8", to = "ASCII//TRANSLIT")
    n[ind_na] <- ifelse(is.na(n2), nomes[ind_na], n2)
  }
  n <- tolower(gsub("^_|_$", "", gsub("[^a-zA-Z0-9]+", "_", n)))
  return(n)
}

detectar_separador_rais <- function(caminho_arq) {
  linha1 <- readLines(caminho_arq, n = 1, warn = FALSE, encoding = "Latin-1")
  if (length(linha1) == 0) return(",")
  n_ponto_virgula <- lengths(regmatches(linha1, gregexpr(";", linha1, fixed = TRUE, useBytes = TRUE)))
  n_virgula       <- lengths(regmatches(linha1, gregexpr(",", linha1, fixed = TRUE, useBytes = TRUE)))
  if (n_ponto_virgula > n_virgula) return(";")
  return(",")
}

converter_num_rais <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(gsub(",", ".", trimws(x), fixed = TRUE))))
  }
  suppressWarnings(as.numeric(x))
}

# ------------------------------------------------------------------------------
# 2. Mapeamentos Oficiais IBGE / CONCLA / MTE-RAIS
# ------------------------------------------------------------------------------
obter_tabela_secoes_cnae <- function() {
  data.frame(
    Secao = LETTERS[1:21],
    Descricao_Secao = c(
      "Agricultura, Pecuária, Produção Florestal, Pesca e Aquicultura",
      "Indústrias Extrativas",
      "Indústrias de Transformação",
      "Eletricidade e Gás",
      "Água, Esgoto, Atividades de Gestão de Resíduos e Descontaminação",
      "Construção",
      "Comércio; Reparação de Veículos Automotores e Motocicletas",
      "Transporte, Armazenagem e Correio",
      "Alojamento e Alimentação",
      "Informação e Comunicação",
      "Atividades Financeiras, de Seguros e Serviços Relacionados",
      "Atividades Imobiliárias",
      "Atividades Profissionais, Científicas e Técnicas",
      "Atividades Administrativas e Serviços Complementares",
      "Administração Pública, Defesa e Seguridade Social",
      "Educação",
      "Saúde Humana e Serviços Sociais",
      "Artes, Cultura, Esporte e Recreação",
      "Outras Atividades de Serviços",
      "Serviços Domésticos",
      "Organismos Internacionais e Outras Instituições Extraterritoriais"
    ),
    stringsAsFactors = FALSE
  )
}

mapear_secao_cnae_por_classe <- function(classe_cnae) {
  cls_str <- sprintf("%05d", suppressWarnings(as.integer(classe_cnae)))
  div <- suppressWarnings(as.integer(substr(cls_str, 1, 2)))
  dplyr::case_when(
    is.na(div)          ~ "Não Identificado",
    div >= 1  & div <= 3  ~ "A",
    div >= 5  & div <= 9  ~ "B",
    div >= 10 & div <= 33 ~ "C",
    div == 35             ~ "D",
    div >= 36 & div <= 39 ~ "E",
    div >= 41 & div <= 43 ~ "F",
    div >= 45 & div <= 47 ~ "G",
    div >= 49 & div <= 53 ~ "H",
    div >= 55 & div <= 56 ~ "I",
    div >= 58 & div <= 63 ~ "J",
    div >= 64 & div <= 66 ~ "K",
    div == 68             ~ "L",
    div >= 69 & div <= 75 ~ "M",
    div >= 77 & div <= 82 ~ "N",
    div == 84             ~ "O",
    div == 85             ~ "P",
    div >= 86 & div <= 88 ~ "Q",
    div >= 90 & div <= 93 ~ "R",
    div >= 94 & div <= 96 ~ "S",
    div == 97             ~ "T",
    div == 99             ~ "U",
    TRUE                  ~ "Não Identificado"
  )
}

mapear_grande_setor_rais <- function(secao) {
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

mapear_faixa_etaria_rais <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  fx <- dplyr::case_when(
    c_int == 1 ~ "10 a 14 anos",
    c_int == 2 ~ "15 a 17 anos",
    c_int == 3 ~ "18 a 24 anos",
    c_int == 4 ~ "25 a 29 anos",
    c_int == 5 ~ "30 a 39 anos",
    c_int == 6 ~ "40 a 49 anos",
    c_int == 7 ~ "50 a 64 anos",
    c_int == 8 ~ "65 anos ou mais",
    TRUE       ~ "Não Identificado"
  )
  niveis <- c("10 a 14 anos", "15 a 17 anos", "18 a 24 anos", "25 a 29 anos",
              "30 a 39 anos", "40 a 49 anos", "50 a 64 anos", "65 anos ou mais",
              "Não Identificado")
  factor(fx, levels = niveis)
}

mapear_raca_cor_rais <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  rc <- dplyr::case_when(
    c_int == 8 ~ "Parda",
    c_int == 2 ~ "Branca",
    c_int == 4 ~ "Preta",
    c_int == 6 ~ "Amarela",
    c_int == 1 ~ "Indígena",
    TRUE       ~ "Não Identificado"
  )
  niveis <- c("Parda", "Branca", "Preta", "Amarela", "Indígena", "Não Identificado")
  factor(rc, levels = niveis)
}

mapear_sexo_rais <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  dplyr::case_when(
    c_int == 1 ~ "Homem",
    c_int == 2 ~ "Mulher",
    TRUE       ~ "Não Identificado"
  )
}

mapear_grau_instrucao_rais <- function(cod) {
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
    c_int == 10 ~ "Mestrado",
    c_int == 11 ~ "Doutorado",
    TRUE        ~ "Não Identificado"
  )
  niveis <- c(
    "Analfabeto", "Até 5ª Incompleto", "5ª Completo Fundamental",
    "6ª a 9ª Fundamental", "Fundamental Completo", "Médio Incompleto",
    "Médio Completo", "Superior Incompleto", "Superior Completo",
    "Mestrado", "Doutorado", "Não Identificado"
  )
  factor(desc, levels = niveis)
}

mapear_escolaridade_grupo_rais <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  desc <- dplyr::case_when(
    c_int %in% c(1, 2, 3, 4) ~ "Fundamental Incompleto ou Menos",
    c_int == 5               ~ "Fundamental Completo",
    c_int == 6               ~ "Médio Incompleto",
    c_int == 7               ~ "Médio Completo",
    c_int == 8               ~ "Superior Incompleto",
    c_int %in% c(9, 10, 11)  ~ "Superior Completo ou Pós-Graduação",
    TRUE                     ~ "Não Identificado"
  )
  niveis <- c(
    "Fundamental Incompleto ou Menos", "Fundamental Completo",
    "Médio Incompleto", "Médio Completo", "Superior Incompleto",
    "Superior Completo ou Pós-Graduação", "Não Identificado"
  )
  factor(desc, levels = niveis)
}

mapear_tamanho_estab_rais <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  desc <- dplyr::case_when(
    c_int == 1  ~ "Zero vínculos",
    c_int == 2  ~ "Até 4 vínculos",
    c_int == 3  ~ "De 5 a 9 vínculos",
    c_int == 4  ~ "De 10 a 19 vínculos",
    c_int == 5  ~ "De 20 a 49 vínculos",
    c_int == 6  ~ "De 50 a 99 vínculos",
    c_int == 7  ~ "De 100 a 249 vínculos",
    c_int == 8  ~ "De 250 a 499 vínculos",
    c_int == 9  ~ "De 500 a 999 vínculos",
    c_int == 10 ~ "1.000 ou mais vínculos",
    TRUE        ~ "Não Identificado"
  )
  niveis <- c(
    "Zero vínculos", "Até 4 vínculos", "De 5 a 9 vínculos",
    "De 10 a 19 vínculos", "De 20 a 49 vínculos", "De 50 a 99 vínculos",
    "De 100 a 249 vínculos", "De 250 a 499 vínculos", "De 500 a 999 vínculos",
    "1.000 ou mais vínculos", "Não Identificado"
  )
  factor(desc, levels = niveis)
}

mapear_tamanho_por_qtd_ativos <- function(qtd) {
  q <- suppressWarnings(as.integer(qtd))
  desc <- dplyr::case_when(
    is.na(q) | q <= 0    ~ "Zero vínculos",
    q >= 1   & q <= 4    ~ "Até 4 vínculos",
    q >= 5   & q <= 9    ~ "De 5 a 9 vínculos",
    q >= 10  & q <= 19   ~ "De 10 a 19 vínculos",
    q >= 20  & q <= 49   ~ "De 20 a 49 vínculos",
    q >= 50  & q <= 99   ~ "De 50 a 99 vínculos",
    q >= 100 & q <= 249  ~ "De 100 a 249 vínculos",
    q >= 250 & q <= 499  ~ "De 250 a 499 vínculos",
    q >= 500 & q <= 999  ~ "De 500 a 999 vínculos",
    q >= 1000            ~ "1.000 ou mais vínculos"
  )
  niveis <- c(
    "Zero vínculos", "Até 4 vínculos", "De 5 a 9 vínculos",
    "De 10 a 19 vínculos", "De 20 a 49 vínculos", "De 50 a 99 vínculos",
    "De 100 a 249 vínculos", "De 250 a 499 vínculos", "De 500 a 999 vínculos",
    "1.000 ou mais vínculos"
  )
  factor(desc, levels = niveis)
}

mapear_tipo_estab_rais <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  desc <- dplyr::case_when(
    c_int == 1 ~ "CNPJ",
    c_int == 5 ~ "CAEPF (Pessoa Física / Produtor Rural)",
    c_int == 6 ~ "CNO (Cadastro Nacional de Obras)",
    c_int == 3 ~ "CEI (Cadastro Específico do INSS)",
    TRUE       ~ "Não Identificado"
  )
  niveis <- c(
    "CNPJ",
    "CAEPF (Pessoa Física / Produtor Rural)",
    "CNO (Cadastro Nacional de Obras)",
    "CEI (Cadastro Específico do INSS)",
    "Não Identificado"
  )
  factor(desc, levels = niveis)
}

mapear_grupo_natureza_juridica <- function(cod) {
  c_str <- trimws(as.character(cod))
  d1 <- substr(c_str, 1, 1)
  desc <- dplyr::case_when(
    d1 == "1" ~ "Administração Pública",
    d1 == "2" ~ "Entidades Empresariais",
    d1 == "3" ~ "Entidades sem Fins Lucrativos",
    d1 == "4" ~ "Pessoas Físicas e Outras Organizações",
    d1 == "5" ~ "Organizações Internacionais e Extraterritoriais",
    TRUE      ~ "Não Identificado / CAEPF"
  )
  niveis <- c(
    "Administração Pública",
    "Entidades Empresariais",
    "Entidades sem Fins Lucrativos",
    "Pessoas Físicas e Outras Organizações",
    "Organizações Internacionais e Extraterritoriais",
    "Não Identificado / CAEPF"
  )
  factor(desc, levels = niveis)
}

mapear_nome_natureza_juridica <- function(cod) {
  c_int <- suppressWarnings(as.integer(cod))
  dplyr::case_when(
    c_int == 1015 ~ "Órgão Público do Poder Executivo Federal",
    c_int == 1023 ~ "Órgão Público do Poder Executivo Estadual ou do DF",
    c_int == 1031 ~ "Órgão Público do Poder Executivo Municipal",
    c_int == 1040 ~ "Órgão Público do Poder Legislativo Federal",
    c_int == 1058 ~ "Órgão Público do Poder Legislativo Estadual ou do DF",
    c_int == 1066 ~ "Órgão Público do Poder Legislativo Municipal",
    c_int == 1074 ~ "Órgão Público do Poder Judiciário Federal",
    c_int == 1082 ~ "Órgão Público do Poder Judiciário Estadual",
    c_int == 1104 ~ "Autarquia Federal",
    c_int == 1112 ~ "Autarquia Estadual ou do Distrito Federal",
    c_int == 1120 ~ "Autarquia Municipal",
    c_int == 1139 ~ "Fundação Pública de Direito Público Federal",
    c_int == 1147 ~ "Fundação Pública de Direito Público Estadual ou do DF",
    c_int == 1155 ~ "Fundação Pública de Direito Público Municipal",
    c_int == 1163 ~ "Órgão Público Autônomo Federal",
    c_int == 1171 ~ "Órgão Público Autônomo Estadual ou do DF",
    c_int == 1180 ~ "Órgão Público Autônomo Municipal",
    c_int == 1210 ~ "Consórcio Público de Direito Público (Associação Pública)",
    c_int == 1244 ~ "Município",
    c_int == 1309 ~ "Fundo Público da Administração Indireta Municipal",
    c_int == 1325 ~ "Fundo Público da Administração Direta Estadual ou do DF",
    c_int == 1333 ~ "Fundo Público da Administração Direta Municipal",
    c_int == 2011 ~ "Empresa Pública",
    c_int == 2038 ~ "Sociedade de Economia Mista",
    c_int == 2046 ~ "Sociedade Anônima Aberta",
    c_int == 2054 ~ "Sociedade Anônima Fechada",
    c_int == 2062 ~ "Sociedade Empresária Limitada (LTDA)",
    c_int == 2070 ~ "Sociedade Empresária em Nome Coletivo",
    c_int == 2127 ~ "Sociedade em Conta de Participação",
    c_int == 2135 ~ "Empresário (Individual)",
    c_int == 2143 ~ "Cooperativa",
    c_int == 2151 ~ "Consórcio de Sociedades",
    c_int == 2178 ~ "Estabelecimento, no Brasil, de Sociedade Estrangeira",
    c_int == 2232 ~ "Sociedade Simples Pura",
    c_int == 2240 ~ "Sociedade Simples Limitada",
    c_int == 2305 ~ "Empresa Individual de Responsabilidade Limitada (EIRELI)",
    c_int == 2321 ~ "Sociedade Unipessoal de Advocacia",
    c_int == 2330 ~ "Cooperativas de Consumo",
    c_int == 2348 ~ "Empresa Simples de Inovação (Inova Simples)",
    c_int == 3034 ~ "Serviço Notarial e Registral (Cartório)",
    c_int == 3069 ~ "Fundação Privada",
    c_int == 3077 ~ "Serviço Social Autônomo",
    c_int == 3085 ~ "Condomínio Edilício",
    c_int == 3131 ~ "Entidade Sindical",
    c_int == 3220 ~ "Organização Religiosa",
    c_int == 3263 ~ "Órgão de Direção Regional de Partido Político",
    c_int == 3271 ~ "Órgão de Direção Local de Partido Político",
    c_int == 3301 ~ "Organização Social (OS)",
    c_int == 3999 ~ "Associação Privada",
    c_int == 4014 ~ "Empresa Individual Imobiliária",
    c_int == 4022 ~ "Segurado Especial",
    c_int == 4065 ~ "Contribuinte Individual (Outros)",
    c_int == 4081 ~ "Contribuinte Individual",
    c_int == 4120 ~ "Produtor Rural (Pessoa Física)",
    c_int == 9999 ~ "Não Identificado / CAEPF / Pessoa Física",
    TRUE          ~ paste0("Natureza Jurídica ", c_int)
  )
}

calc_media_positiva <- function(x, max_val = 250000) {
  ind <- !is.na(x) & x > 0 & x <= max_val
  if (!any(ind)) return(NA_real_)
  round(mean(x[ind]), 2)
}

calc_mediana_positiva <- function(x, max_val = 250000) {
  ind <- !is.na(x) & x > 0 & x <= max_val
  if (!any(ind)) return(NA_real_)
  round(stats::median(x[ind]), 2)
}

# ------------------------------------------------------------------------------
# 3. Download, Descompactação, Leitura e Filtragem Imediata (UF == 17)
# ------------------------------------------------------------------------------
obter_arquivo_microdado_rais <- function(ano, nome_base, pasta_tmp = dir_tmp_rais, pasta_local = "dados/brutos/rais") {
  # Verifica se o arquivo descompactado (.COMT ou .txt) já está em tmp ou na pasta local
  padrao_descomp <- sprintf("^%s\\.(COMT|txt|csv)$", nome_base)
  arqs_tmp_desc  <- list.files(pasta_tmp, pattern = padrao_descomp, full.names = TRUE, ignore.case = TRUE)
  if (length(arqs_tmp_desc) > 0) return(arqs_tmp_desc[1])

  arqs_loc_desc  <- list.files(pasta_local, pattern = padrao_descomp, full.names = TRUE, ignore.case = TRUE)
  if (length(arqs_loc_desc) > 0) {
    dest <- file.path(pasta_tmp, basename(arqs_loc_desc[1]))
    file.copy(arqs_loc_desc[1], dest, overwrite = TRUE)
    try(file.remove(arqs_loc_desc[1]), silent = TRUE)
    return(dest)
  }

  # Verifica se o arquivo .7z está na pasta local ou em tmp
  arq_7z_tmp <- file.path(pasta_tmp, paste0(nome_base, ".7z"))
  arq_7z_loc <- file.path(pasta_local, paste0(nome_base, ".7z"))
  if (!file.exists(arq_7z_tmp) && file.exists(arq_7z_loc)) {
    file.copy(arq_7z_loc, arq_7z_tmp, overwrite = TRUE)
    try(file.remove(arq_7z_loc), silent = TRUE)
  }

  # Se não existe .7z ainda, baixa do FTP oficial do MTE para o SSD D:
  if (!file.exists(arq_7z_tmp)) {
    url_ftp <- sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/%d/%s.7z", as.integer(ano), nome_base)
    cat(sprintf("   * Baixando '%s.7z' do FTP MTE (%s) para '%s'...\n", nome_base, url_ftp, pasta_tmp))
    status_dl <- system2(
      "curl.exe",
      args = c("-s", "--retry", "3", "--max-time", "900", url_ftp, "-o", arq_7z_tmp),
      stdout = FALSE, stderr = FALSE
    )
    if (status_dl != 0 || !file.exists(arq_7z_tmp) || file.info(arq_7z_tmp)$size < 1000) {
      if (file.exists(arq_7z_tmp)) try(file.remove(arq_7z_tmp), silent = TRUE)
      return(NULL)
    }
  }

  # Descompacta no SSD D:
  cat(sprintf("   * Descompactando '%s.7z' em '%s'...\n", nome_base, pasta_tmp))
  ok_desc <- descompactar_7z(arq_7z_tmp, pasta_tmp)
  if (file.exists(arq_7z_tmp)) try(file.remove(arq_7z_tmp), silent = TRUE)
  if (!ok_desc) return(NULL)

  arqs_tmp_desc <- list.files(pasta_tmp, pattern = padrao_descomp, full.names = TRUE, ignore.case = TRUE)
  if (length(arqs_tmp_desc) > 0) return(arqs_tmp_desc[1])
  return(NULL)
}

ler_vinculos_rais_tocantins <- function(ano) {
  arq_vinc <- obter_arquivo_microdado_rais(ano, "RAIS_VINC_PUB_NORTE")
  if (is.null(arq_vinc) || !file.exists(arq_vinc)) return(NULL)

  cat(sprintf("   * Lendo '%s' e filtrando Tocantins (170000 a 179999)...\n", basename(arq_vinc)))
  sep_char <- detectar_separador_rais(arq_vinc)
  dt <- tryCatch(
    data.table::fread(arq_vinc, sep = sep_char, encoding = "Latin-1", showProgress = FALSE),
    error = function(e) NULL
  )
  # Exclui imediatamente o arquivo bruto descompactado (.COMT / .txt)
  try(file.remove(arq_vinc), silent = TRUE)
  arq_7z_res <- file.path(dir_tmp_rais, "RAIS_VINC_PUB_NORTE.7z")
  if (file.exists(arq_7z_res)) try(file.remove(arq_7z_res), silent = TRUE)

  if (is.null(dt) || nrow(dt) == 0) return(NULL)

  data.table::setnames(dt, normalizar_nomes_colunas_rais(names(dt)))

  # Filtra Tocantins pelo código do município (170000 a 179999)
  col_mun <- intersect(c("municipio_codigo", "municipio"), names(dt))[1]
  if (is.na(col_mun)) return(NULL)
  dt[, mun_int := suppressWarnings(as.integer(get(col_mun)))]
  dt_to <- dt[mun_int >= 170000L & mun_int <= 179999L]
  rm(dt)
  gc(verbose = FALSE)

  if (nrow(dt_to) == 0) return(NULL)

  # Padroniza colunas essenciais da base de vínculos
  dt_to[, municipio_codigo := mun_int]
  dt_to[, mun_int := NULL]

  if (!"ind_vinculo_ativo_31_12_codigo" %in% names(dt_to) && "vinculo_ativo_31_12" %in% names(dt_to)) {
    dt_to[, ind_vinculo_ativo_31_12_codigo := as.integer(vinculo_ativo_31_12)]
  }
  if (!"ind_vinculo_abandonado_codigo" %in% names(dt_to)) {
    dt_to[, ind_vinculo_abandonado_codigo := 0L]
  } else {
    dt_to[, ind_vinculo_abandonado_codigo := coalesce(as.integer(ind_vinculo_abandonado_codigo), 0L)]
  }

  for (col_rem in intersect(c("vl_rem_media_nom", "vl_rem_media_sm", "vl_rem_dezembro_nom", "vl_rem_dezembro_sm"), names(dt_to))) {
    data.table::set(dt_to, j = col_rem, value = converter_num_rais(dt_to[[col_rem]]))
  }

  colunas_vinc_manter <- intersect(c(
    "municipio_codigo", "municipio_trab_codigo", "cnae_2_0_classe_codigo",
    "cnae_2_0_subclasse_codigo", "cbo_2002_ocupacao_codigo",
    "ind_vinculo_ativo_31_12_codigo", "ind_vinculo_abandonado_codigo",
    "faixa_etaria_codigo", "idade", "raca_cor_codigo", "sexo_codigo",
    "escolaridade_apos_2005_codigo", "natureza_juridica_codigo",
    "tamanho_estabelecimento_codigo", "tipo_estabelecimento_codigo",
    "tipo_vinculo_codigo", "ind_estabelecimento_participante_simples_codigo",
    "vl_rem_media_nom", "vl_rem_media_sm", "vl_rem_dezembro_nom", "vl_rem_dezembro_sm",
    "faixa_rem_media_sm_codigo", "faixa_rem_dez_sm_codigo",
    "tempo_emprego", "qtd_hora_contr", "ind_portador_defic_codigo",
    "ind_trabalho_intermitente_codigo", "ind_trabalho_parcial_codigo"
  ), names(dt_to))

  dt_to <- dt_to[, ..colunas_vinc_manter]
  dt_to[, ano_rais := as.integer(ano)]
  return(dt_to)
}

ler_estabelecimentos_rais_tocantins <- function(ano) {
  arq_estab <- obter_arquivo_microdado_rais(ano, "RAIS_ESTAB_PUB")
  if (is.null(arq_estab) || !file.exists(arq_estab)) return(NULL)

  cat(sprintf("   * Lendo '%s' e filtrando Tocantins (UF == 17)...\n", basename(arq_estab)))
  sep_char <- detectar_separador_rais(arq_estab)
  dt <- tryCatch(
    data.table::fread(arq_estab, sep = sep_char, encoding = "Latin-1", showProgress = FALSE),
    error = function(e) NULL
  )
  # Exclui imediatamente o arquivo bruto descompactado (.COMT / .txt)
  try(file.remove(arq_estab), silent = TRUE)
  arq_7z_res <- file.path(dir_tmp_rais, "RAIS_ESTAB_PUB.7z")
  if (file.exists(arq_7z_res)) try(file.remove(arq_7z_res), silent = TRUE)

  if (is.null(dt) || nrow(dt) == 0) return(NULL)

  data.table::setnames(dt, normalizar_nomes_colunas_rais(names(dt)))

  # Filtra Tocantins por UF == 17 ou código de município 170000:179999
  if ("uf_codigo" %in% names(dt)) {
    dt_to <- dt[suppressWarnings(as.integer(uf_codigo)) == 17L]
  } else {
    col_mun <- intersect(c("municipio_codigo", "municipio"), names(dt))[1]
    dt[, mun_int := suppressWarnings(as.integer(get(col_mun)))]
    dt_to <- dt[mun_int >= 170000L & mun_int <= 179999L]
  }
  rm(dt)
  gc(verbose = FALSE)

  if (nrow(dt_to) == 0) return(NULL)

  colunas_estab_manter <- intersect(c(
    "uf_codigo", "municipio_codigo", "cnae_2_0_classe_codigo", "cnae_2_0_subclasse_codigo",
    "qtd_vinculos_ativos", "qtd_vinculos_clt", "qtd_vinculos_estatutarios",
    "ind_atividade_ano_codigo", "ind_rais_negativa_codigo",
    "ind_estab_participante_simples_codigo", "ind_estab_participante_pat_codigo",
    "natureza_juridica_codigo", "tamanho_estabelecimento_codigo",
    "tipo_estabelecimento_codigo", "ibge_subsetor_codigo", "cep_estab"
  ), names(dt_to))

  dt_to <- dt_to[, ..colunas_estab_manter]
  dt_to[, ano_rais := as.integer(ano)]
  return(dt_to)
}

# ------------------------------------------------------------------------------
# 4. Consolidação Completa da RAIS Tocantins (Vínculos + Estabelecimentos)
# ------------------------------------------------------------------------------
gerar_resumo_rais_to <- function(dt_vinc_to, dt_estab_to, ano_ref) {
  dt_v <- data.table::copy(dt_vinc_to)
  dt_e <- data.table::copy(dt_estab_to)

  # Carrega lista oficial dos 139 municípios do Tocantins (IBGE)
  df_mun_ibge <- if (file.exists("dados/brutos/municipios_to_ibge.rds")) {
    readRDS("dados/brutos/municipios_to_ibge.rds")
  } else {
    data.frame(
      cod_mun_7 = sort(unique(dt_v$municipio_codigo)),
      cod_mun_6 = sort(unique(dt_v$municipio_codigo)),
      Municipio = as.character(sort(unique(dt_v$municipio_codigo))),
      stringsAsFactors = FALSE
    )
  }

  # Auditoria metodológica de Vínculos no Tocantins:
  # - Total de registros de vínculos no ano
  # - Desligados/inativos em 31/12 (ind_vinculo_ativo_31_12_codigo == 0)
  # - Ativos em 31/12 mas ABANDONADOS (ind_vinculo_ativo_31_12_codigo == 1 & ind_vinculo_abandonado_codigo == 1)
  # - Ativos VÁLIDOS em 31/12 (ind_vinculo_ativo_31_12_codigo == 1 & ind_vinculo_abandonado_codigo == 0)
  auditoria_vinculos <- list(
    ano_rais                        = as.integer(ano_ref),
    total_registros_ano             = nrow(dt_v),
    vinculos_inativos_31_12         = sum(dt_v$ind_vinculo_ativo_31_12_codigo == 0L, na.rm = TRUE),
    vinculos_abandonados_excluidos  = sum(dt_v$ind_vinculo_ativo_31_12_codigo == 1L & dt_v$ind_vinculo_abandonado_codigo == 1L, na.rm = TRUE),
    estoque_ativos_validos_31_12    = sum(dt_v$ind_vinculo_ativo_31_12_codigo == 1L & dt_v$ind_vinculo_abandonado_codigo == 0L, na.rm = TRUE),
    estoque_ativos_base_estab       = sum(dt_e$qtd_vinculos_ativos, na.rm = TRUE)
  )

  # FILTRO METODOLÓGICO OFICIAL (Painel RAIS / MTE):
  # Mantém exclusivamente vínculos ativos em 31/12 e NÃO abandonados
  dt_ativos <- dt_v[ind_vinculo_ativo_31_12_codigo == 1L & ind_vinculo_abandonado_codigo == 0L]

  # Classificações categóricas na base de Vínculos Ativos
  dt_ativos[, Secao_CNAE          := mapear_secao_cnae_por_classe(cnae_2_0_classe_codigo)]
  dt_ativos[, Grande_Setor        := mapear_grande_setor_rais(Secao_CNAE)]
  dt_ativos[, Faixa_Etaria        := mapear_faixa_etaria_rais(faixa_etaria_codigo)]
  dt_ativos[, Raca_Cor            := mapear_raca_cor_rais(raca_cor_codigo)]
  dt_ativos[, Sexo                := mapear_sexo_rais(sexo_codigo)]
  dt_ativos[, Grau_Instrucao      := mapear_grau_instrucao_rais(escolaridade_apos_2005_codigo)]
  dt_ativos[, Escolaridade_Grupo  := mapear_escolaridade_grupo_rais(escolaridade_apos_2005_codigo)]
  dt_ativos[, Grupo_Natureza_Jur  := mapear_grupo_natureza_juridica(natureza_juridica_codigo)]
  dt_ativos[, Nome_Natureza_Jur   := mapear_nome_natureza_juridica(natureza_juridica_codigo)]
  dt_ativos[, Tamanho_Estab       := mapear_tamanho_estab_rais(tamanho_estabelecimento_codigo)]
  dt_ativos[, Tipo_Estab          := mapear_tipo_estab_rais(tipo_estabelecimento_codigo)]

  estoque_total_to <- nrow(dt_ativos)

  # ----------------------------------------------------------------------------
  # A) RESUMO ESTADUAL DE VÍNCULOS E REMUNERAÇÃO MÉDIA NO TOCANTINS
  # ----------------------------------------------------------------------------
  resumo_vinculos_to <- list(
    ano_rais                      = as.integer(ano_ref),
    estoque_total_ativos          = estoque_total_to,
    vinculos_abandonados_excluidos= auditoria_vinculos$vinculos_abandonados_excluidos,
    remuneracao_media_nominal     = calc_media_positiva(dt_ativos$vl_rem_media_nom),
    remuneracao_media_dezembro    = calc_media_positiva(dt_ativos$vl_rem_dezembro_nom),
    remuneracao_media_sm          = calc_media_positiva(dt_ativos$vl_rem_media_sm, max_val = 500),
    remuneracao_mediana_nominal   = calc_mediana_positiva(dt_ativos$vl_rem_media_nom),
    remuneracao_mediana_dezembro  = calc_mediana_positiva(dt_ativos$vl_rem_dezembro_nom),
    idade_media                   = round(mean(dt_ativos$idade[dt_ativos$idade > 0 & dt_ativos$idade < 100], na.rm = TRUE), 1)
  )

  # Função auxiliar padronizada para agregar dimensões de Vínculos Ativos
  agregar_vinculos_dim <- function(dt_sub, col_grupo, nome_col_saida) {
    res <- dt_sub[, .(
      Vinculos                   = .N,
      Participacao_Pct           = round(.N * 100 / max(1L, estoque_total_to), 2),
      Remuneracao_Media_Nominal  = calc_media_positiva(vl_rem_media_nom),
      Remuneracao_Media_Dezembro = calc_media_positiva(vl_rem_dezembro_nom),
      Remuneracao_Media_SM       = calc_media_positiva(vl_rem_media_sm, max_val = 500),
      Remuneracao_Mediana_Nominal= calc_mediana_positiva(vl_rem_media_nom),
      Idade_Media                = round(mean(idade[idade > 0 & idade < 100], na.rm = TRUE), 1)
    ), by = c(col_grupo)]
    data.table::setnames(res, col_grupo, nome_col_saida)
    as.data.frame(res)
  }

  # 1. Vínculos e Remuneração Média por Município (Todos os 139 Municípios do TO)
  vinc_mun_raw <- dt_ativos[, .(
    Estoque_Vinculos           = .N,
    Remuneracao_Media_Nominal  = calc_media_positiva(vl_rem_media_nom),
    Remuneracao_Media_Dezembro = calc_media_positiva(vl_rem_dezembro_nom),
    Remuneracao_Media_SM       = calc_media_positiva(vl_rem_media_sm, max_val = 500),
    Remuneracao_Mediana_Nominal= calc_mediana_positiva(vl_rem_media_nom),
    Idade_Media                = round(mean(idade[idade > 0 & idade < 100], na.rm = TRUE), 1)
  ), by = .(cod_mun_6 = as.integer(municipio_codigo))]

  # Adiciona também desagregação CLT vs Estatutários da base de Estabelecimentos por Município
  estab_vinc_mun <- dt_e[, .(
    Vinculos_CLT_Estab         = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios_Estab= sum(qtd_vinculos_estatutarios, na.rm = TRUE)
  ), by = .(cod_mun_6 = as.integer(municipio_codigo))]

  vinculos_por_municipio <- df_mun_ibge %>%
    left_join(as.data.frame(vinc_mun_raw), by = "cod_mun_6") %>%
    left_join(as.data.frame(estab_vinc_mun), by = "cod_mun_6") %>%
    mutate(
      Estoque_Vinculos            = coalesce(as.integer(Estoque_Vinculos), 0L),
      Vinculos_CLT_Estab          = coalesce(as.integer(Vinculos_CLT_Estab), 0L),
      Vinculos_Estatutarios_Estab = coalesce(as.integer(Vinculos_Estatutarios_Estab), 0L),
      Participacao_Pct            = round(Estoque_Vinculos * 100 / max(1L, estoque_total_to), 2)
    ) %>%
    select(
      cod_mun_7, cod_mun_6, Municipio,
      Estoque_Vinculos, Participacao_Pct,
      Vinculos_CLT_Estab, Vinculos_Estatutarios_Estab,
      Remuneracao_Media_Nominal, Remuneracao_Media_Dezembro,
      Remuneracao_Media_SM, Remuneracao_Mediana_Nominal, Idade_Media
    ) %>%
    arrange(desc(Estoque_Vinculos), Municipio)

  # 2. Vínculos por Faixa Etária
  vinculos_por_faixa_etaria <- agregar_vinculos_dim(dt_ativos, "Faixa_Etaria", "Faixa_Etaria") %>%
    filter(Faixa_Etaria != "Não Identificado" | Vinculos > 0) %>%
    arrange(Faixa_Etaria)

  # 3. Vínculos por Raça/Cor
  vinculos_por_raca_cor <- agregar_vinculos_dim(dt_ativos, "Raca_Cor", "Raca_Cor") %>%
    filter(Raca_Cor != "Não Identificado" | Vinculos > 0) %>%
    arrange(desc(Vinculos))

  # 4. Vínculos por Gênero/Sexo
  vinculos_por_sexo <- agregar_vinculos_dim(dt_ativos, "Sexo", "Sexo") %>%
    filter(Sexo != "Não Identificado" | Vinculos > 0) %>%
    arrange(desc(Vinculos))

  # 5. Vínculos por Grau de Instrução (Detalhado e Agrupado)
  vinculos_por_grau_instrucao <- agregar_vinculos_dim(dt_ativos, "Grau_Instrucao", "Grau_Instrucao") %>%
    filter(Grau_Instrucao != "Não Identificado" | Vinculos > 0) %>%
    arrange(Grau_Instrucao)

  vinculos_por_escolaridade_grupo <- agregar_vinculos_dim(dt_ativos, "Escolaridade_Grupo", "Escolaridade") %>%
    filter(Escolaridade != "Não Identificado" | Vinculos > 0) %>%
    arrange(Escolaridade)

  # 6. Vínculos pelas 21 Grandes Seções da CNAE 2.0 (A a U) e pelos 5 Grandes Setores
  df_secoes_cnae <- obter_tabela_secoes_cnae()
  vinculos_por_secao_cnae <- df_secoes_cnae %>%
    left_join(agregar_vinculos_dim(dt_ativos, "Secao_CNAE", "Secao"), by = "Secao") %>%
    mutate(
      Grande_Setor     = mapear_grande_setor_rais(Secao),
      Vinculos         = coalesce(as.integer(Vinculos), 0L),
      Participacao_Pct = coalesce(Participacao_Pct, 0)
    ) %>%
    select(Secao, Descricao_Secao, Grande_Setor, everything()) %>%
    arrange(desc(Vinculos))

  ordem_5_setores <- c("Serviços", "Comércio", "Indústria", "Agropecuária", "Construção")
  vinculos_por_grande_setor <- data.frame(Grande_Setor = ordem_5_setores, stringsAsFactors = FALSE) %>%
    left_join(agregar_vinculos_dim(dt_ativos, "Grande_Setor", "Grande_Setor"), by = "Grande_Setor") %>%
    mutate(
      Vinculos         = coalesce(as.integer(Vinculos), 0L),
      Participacao_Pct = coalesce(Participacao_Pct, 0)
    ) %>%
    arrange(desc(Vinculos))

  # Vínculos por Grande Setor e por Seção CNAE em cada Município
  vinculos_setor_mun_raw <- dt_ativos[, .(
    Vinculos                   = .N,
    Remuneracao_Media_Nominal  = calc_media_positiva(vl_rem_media_nom),
    Remuneracao_Media_Dezembro = calc_media_positiva(vl_rem_dezembro_nom)
  ), by = .(cod_mun_6 = as.integer(municipio_codigo), Grande_Setor)]

  vinculos_grande_setor_por_municipio <- df_mun_ibge %>%
    select(cod_mun_7, cod_mun_6, Municipio) %>%
    inner_join(as.data.frame(vinculos_setor_mun_raw), by = "cod_mun_6") %>%
    arrange(Municipio, desc(Vinculos))

  vinculos_secao_mun_raw <- dt_ativos[, .(
    Vinculos                   = .N,
    Remuneracao_Media_Nominal  = calc_media_positiva(vl_rem_media_nom),
    Remuneracao_Media_Dezembro = calc_media_positiva(vl_rem_dezembro_nom)
  ), by = .(cod_mun_6 = as.integer(municipio_codigo), Secao = Secao_CNAE)]

  vinculos_secao_cnae_por_municipio <- df_mun_ibge %>%
    select(cod_mun_7, cod_mun_6, Municipio) %>%
    inner_join(as.data.frame(vinculos_secao_mun_raw), by = "cod_mun_6") %>%
    left_join(df_secoes_cnae, by = "Secao") %>%
    arrange(Municipio, desc(Vinculos))

  # ----------------------------------------------------------------------------
  # B) BASE DE ESTABELECIMENTOS (ESTADO E TODOS OS 139 MUNICÍPIOS)
  # ----------------------------------------------------------------------------
  dt_e[, Secao_CNAE                 := mapear_secao_cnae_por_classe(cnae_2_0_classe_codigo)]
  dt_e[, Grande_Setor               := mapear_grande_setor_rais(Secao_CNAE)]
  dt_e[, Grupo_Natureza_Juridica    := mapear_grupo_natureza_juridica(natureza_juridica_codigo)]
  dt_e[, Nome_Natureza_Juridica     := mapear_nome_natureza_juridica(natureza_juridica_codigo)]
  dt_e[, Tamanho_Estabelecimento    := mapear_tamanho_estab_rais(tamanho_estabelecimento_codigo)]
  dt_e[, Tamanho_Ativos_31_12       := mapear_tamanho_por_qtd_ativos(qtd_vinculos_ativos)]
  dt_e[, Tipo_Estabelecimento       := mapear_tipo_estab_rais(tipo_estabelecimento_codigo)]

  total_estab_to        <- nrow(dt_e)
  total_estab_ativos_to <- sum(dt_e$qtd_vinculos_ativos > 0, na.rm = TRUE)
  total_vinc_estab_to   <- sum(dt_e$qtd_vinculos_ativos, na.rm = TRUE)

  # 1. Resumo Geral de Estabelecimentos no Estado e por Município (139 municípios)
  resumo_estab_to <- list(
    ano_rais                            = as.integer(ano_ref),
    total_estabelecimentos              = total_estab_to,
    estabelecimentos_com_atividade_ano  = sum(dt_e$ind_atividade_ano_codigo == 1L, na.rm = TRUE),
    estabelecimentos_com_vinculos_ativos= total_estab_ativos_to,
    estabelecimentos_sem_vinculos_ativos= sum(dt_e$qtd_vinculos_ativos == 0L, na.rm = TRUE),
    estabelecimentos_rais_negativa      = sum(dt_e$ind_rais_negativa_codigo == 1L, na.rm = TRUE),
    estabelecimentos_optantes_simples   = sum(dt_e$ind_estab_participante_simples_codigo == 1L, na.rm = TRUE),
    estabelecimentos_participantes_pat  = sum(dt_e$ind_estab_participante_pat_codigo == 1L, na.rm = TRUE),
    vinculos_ativos_total               = total_vinc_estab_to,
    vinculos_clt_total                  = sum(dt_e$qtd_vinculos_clt, na.rm = TRUE),
    vinculos_estatutarios_total         = sum(dt_e$qtd_vinculos_estatutarios, na.rm = TRUE)
  )

  estab_mun_raw <- dt_e[, .(
    Total_Estabelecimentos              = .N,
    Estabelecimentos_Com_Atividade_Ano  = sum(ind_atividade_ano_codigo == 1L, na.rm = TRUE),
    Estabelecimentos_Com_Vinculos_Ativos= sum(qtd_vinculos_ativos > 0L, na.rm = TRUE),
    Estabelecimentos_RAIS_Negativa      = sum(ind_rais_negativa_codigo == 1L, na.rm = TRUE),
    Estabelecimentos_Optantes_Simples   = sum(ind_estab_participante_simples_codigo == 1L, na.rm = TRUE),
    Vinculos_Ativos                     = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                        = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios               = sum(qtd_vinculos_estatutarios, na.rm = TRUE)
  ), by = .(cod_mun_6 = as.integer(municipio_codigo))]

  estab_por_municipio <- df_mun_ibge %>%
    left_join(as.data.frame(estab_mun_raw), by = "cod_mun_6") %>%
    mutate(
      across(c(Total_Estabelecimentos, Estabelecimentos_Com_Atividade_Ano,
               Estabelecimentos_Com_Vinculos_Ativos, Estabelecimentos_RAIS_Negativa,
               Estabelecimentos_Optantes_Simples, Vinculos_Ativos,
               Vinculos_CLT, Vinculos_Estatutarios), ~ coalesce(as.integer(.), 0L)),
      Participacao_Estab_Ativos_Pct = round(Estabelecimentos_Com_Vinculos_Ativos * 100 / max(1L, total_estab_ativos_to), 2),
      Participacao_Vinculos_Pct     = round(Vinculos_Ativos * 100 / max(1L, total_vinc_estab_to), 2)
    ) %>%
    arrange(desc(Estabelecimentos_Com_Vinculos_Ativos), desc(Total_Estabelecimentos), Municipio)

  # 2. Vínculos e Estabelecimentos por Natureza Jurídica (Estado e Municípios)
  # Remuneração média por Natureza Jurídica vinda da base de vínculos ativos
  rem_nat_grupo <- dt_ativos[, .(
    Remuneracao_Media_Nominal  = calc_media_positiva(vl_rem_media_nom),
    Remuneracao_Media_Dezembro = calc_media_positiva(vl_rem_dezembro_nom)
  ), by = .(Grupo_Natureza_Juridica = Grupo_Natureza_Jur)]

  estab_por_natureza_grupo_to <- dt_e[, .(
    Total_Estabelecimentos              = .N,
    Estabelecimentos_Com_Vinculos_Ativos= sum(qtd_vinculos_ativos > 0L, na.rm = TRUE),
    Vinculos_Ativos                     = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                        = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios               = sum(qtd_vinculos_estatutarios, na.rm = TRUE),
    Participacao_Vinculos_Pct           = round(sum(qtd_vinculos_ativos, na.rm = TRUE) * 100 / max(1L, total_vinc_estab_to), 2)
  ), by = Grupo_Natureza_Juridica] %>%
    as.data.frame() %>%
    left_join(as.data.frame(rem_nat_grupo), by = "Grupo_Natureza_Juridica") %>%
    arrange(desc(Vinculos_Ativos))

  rem_nat_det <- dt_ativos[, .(
    Remuneracao_Media_Nominal  = calc_media_positiva(vl_rem_media_nom),
    Remuneracao_Media_Dezembro = calc_media_positiva(vl_rem_dezembro_nom)
  ), by = .(Codigo_Natureza_Juridica = as.integer(natureza_juridica_codigo))]

  estab_por_natureza_detalhada_to <- dt_e[, .(
    Total_Estabelecimentos              = .N,
    Estabelecimentos_Com_Vinculos_Ativos= sum(qtd_vinculos_ativos > 0L, na.rm = TRUE),
    Vinculos_Ativos                     = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                        = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios               = sum(qtd_vinculos_estatutarios, na.rm = TRUE),
    Participacao_Vinculos_Pct           = round(sum(qtd_vinculos_ativos, na.rm = TRUE) * 100 / max(1L, total_vinc_estab_to), 2)
  ), by = .(
    Codigo_Natureza_Juridica = as.integer(natureza_juridica_codigo),
    Nome_Natureza_Juridica,
    Grupo_Natureza_Juridica
  )] %>%
    as.data.frame() %>%
    left_join(as.data.frame(rem_nat_det), by = "Codigo_Natureza_Juridica") %>%
    arrange(desc(Vinculos_Ativos))

  estab_nat_grupo_mun_raw <- dt_e[, .(
    Total_Estabelecimentos              = .N,
    Estabelecimentos_Com_Vinculos_Ativos= sum(qtd_vinculos_ativos > 0L, na.rm = TRUE),
    Vinculos_Ativos                     = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                        = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios               = sum(qtd_vinculos_estatutarios, na.rm = TRUE)
  ), by = .(cod_mun_6 = as.integer(municipio_codigo), Grupo_Natureza_Juridica)]

  estab_por_natureza_grupo_mun <- df_mun_ibge %>%
    select(cod_mun_7, cod_mun_6, Municipio) %>%
    inner_join(as.data.frame(estab_nat_grupo_mun_raw), by = "cod_mun_6") %>%
    arrange(Municipio, desc(Vinculos_Ativos))

  estab_nat_det_mun_raw <- dt_e[, .(
    Total_Estabelecimentos              = .N,
    Estabelecimentos_Com_Vinculos_Ativos= sum(qtd_vinculos_ativos > 0L, na.rm = TRUE),
    Vinculos_Ativos                     = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                        = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios               = sum(qtd_vinculos_estatutarios, na.rm = TRUE)
  ), by = .(
    cod_mun_6 = as.integer(municipio_codigo),
    Codigo_Natureza_Juridica = as.integer(natureza_juridica_codigo),
    Nome_Natureza_Juridica,
    Grupo_Natureza_Juridica
  )]

  estab_por_natureza_detalhada_mun <- df_mun_ibge %>%
    select(cod_mun_7, cod_mun_6, Municipio) %>%
    inner_join(as.data.frame(estab_nat_det_mun_raw), by = "cod_mun_6") %>%
    arrange(Municipio, desc(Vinculos_Ativos))

  # 3. Quantidade de Estabelecimentos (e Vínculos) por Tamanho do Estabelecimento
  #    de acordo com empregados ativos em 31/12 (Estado e Municípios)
  #    Nota: Calcula tanto pela variável original 'Tamanho Estabelecimento' quanto
  #    pela reclassificação exata via 'Qtd Vínculos Ativos' (que já desconta os
  #    vínculos abandonados em 31/12).
  tam_ativos_3112_to <- dt_e[, .(
    Quantidade_Estabelecimentos_Ativos_3112 = .N,
    Vinculos_Ativos                         = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                            = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios                   = sum(qtd_vinculos_estatutarios, na.rm = TRUE)
  ), by = .(Tamanho_Estabelecimento = Tamanho_Ativos_31_12)]

  tam_orig_to <- dt_e[, .(
    Quantidade_Estabelecimentos_Layout = .N
  ), by = .(Tamanho_Estabelecimento)]

  rem_tam_to <- dt_ativos[, .(
    Remuneracao_Media_Nominal  = calc_media_positiva(vl_rem_media_nom),
    Remuneracao_Media_Dezembro = calc_media_positiva(vl_rem_dezembro_nom)
  ), by = .(Tamanho_Estabelecimento = Tamanho_Estab)]

  niveis_tam <- levels(dt_e$Tamanho_Ativos_31_12)
  estab_por_tamanho_to <- data.frame(
    Tamanho_Estabelecimento = factor(niveis_tam, levels = niveis_tam),
    stringsAsFactors = FALSE
  ) %>%
    left_join(as.data.frame(tam_ativos_3112_to), by = "Tamanho_Estabelecimento") %>%
    left_join(as.data.frame(tam_orig_to), by = "Tamanho_Estabelecimento") %>%
    left_join(as.data.frame(rem_tam_to), by = "Tamanho_Estabelecimento") %>%
    mutate(
      across(c(Quantidade_Estabelecimentos_Ativos_3112, Quantidade_Estabelecimentos_Layout,
               Vinculos_Ativos, Vinculos_CLT, Vinculos_Estatutarios), ~ coalesce(as.integer(.), 0L)),
      Participacao_Estab_Pct    = round(Quantidade_Estabelecimentos_Ativos_3112 * 100 / max(1L, total_estab_to), 2),
      Participacao_Vinculos_Pct = round(Vinculos_Ativos * 100 / max(1L, total_vinc_estab_to), 2)
    ) %>%
    arrange(Tamanho_Estabelecimento)

  estab_tam_mun_raw <- dt_e[, .(
    Quantidade_Estabelecimentos_Ativos_3112 = .N,
    Vinculos_Ativos                         = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                            = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios                   = sum(qtd_vinculos_estatutarios, na.rm = TRUE)
  ), by = .(cod_mun_6 = as.integer(municipio_codigo), Tamanho_Estabelecimento = Tamanho_Ativos_31_12)]

  estab_por_tamanho_mun <- df_mun_ibge %>%
    select(cod_mun_7, cod_mun_6, Municipio) %>%
    inner_join(as.data.frame(estab_tam_mun_raw), by = "cod_mun_6") %>%
    arrange(Municipio, Tamanho_Estabelecimento)

  # 4. Quantidade de Estabelecimentos (e Vínculos) por Tipo de Estabelecimento
  #    (CNPJ, CAEPF, CNO/CEI) no Estado e por Município
  rem_tipo_to <- dt_ativos[, .(
    Remuneracao_Media_Nominal  = calc_media_positiva(vl_rem_media_nom),
    Remuneracao_Media_Dezembro = calc_media_positiva(vl_rem_dezembro_nom)
  ), by = .(Tipo_Estabelecimento = Tipo_Estab)]

  estab_por_tipo_to <- dt_e[, .(
    Total_Estabelecimentos              = .N,
    Estabelecimentos_Com_Atividade_Ano  = sum(ind_atividade_ano_codigo == 1L, na.rm = TRUE),
    Estabelecimentos_Com_Vinculos_Ativos= sum(qtd_vinculos_ativos > 0L, na.rm = TRUE),
    Vinculos_Ativos                     = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                        = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios               = sum(qtd_vinculos_estatutarios, na.rm = TRUE),
    Participacao_Estab_Pct              = round(.N * 100 / max(1L, total_estab_to), 2),
    Participacao_Vinculos_Pct           = round(sum(qtd_vinculos_ativos, na.rm = TRUE) * 100 / max(1L, total_vinc_estab_to), 2)
  ), by = Tipo_Estabelecimento] %>%
    as.data.frame() %>%
    left_join(as.data.frame(rem_tipo_to), by = "Tipo_Estabelecimento") %>%
    arrange(desc(Vinculos_Ativos))

  estab_tipo_mun_raw <- dt_e[, .(
    Total_Estabelecimentos              = .N,
    Estabelecimentos_Com_Atividade_Ano  = sum(ind_atividade_ano_codigo == 1L, na.rm = TRUE),
    Estabelecimentos_Com_Vinculos_Ativos= sum(qtd_vinculos_ativos > 0L, na.rm = TRUE),
    Vinculos_Ativos                     = sum(qtd_vinculos_ativos, na.rm = TRUE),
    Vinculos_CLT                        = sum(qtd_vinculos_clt, na.rm = TRUE),
    Vinculos_Estatutarios               = sum(qtd_vinculos_estatutarios, na.rm = TRUE)
  ), by = .(cod_mun_6 = as.integer(municipio_codigo), Tipo_Estabelecimento)]

  estab_por_tipo_mun <- df_mun_ibge %>%
    select(cod_mun_7, cod_mun_6, Municipio) %>%
    inner_join(as.data.frame(estab_tipo_mun_raw), by = "cod_mun_6") %>%
    arrange(Municipio, desc(Vinculos_Ativos))

  # 5. Estabelecimentos por Seção da CNAE 2.0 e Grande Setor (Estado e Municípios)
  estab_por_secao_cnae_to <- df_secoes_cnae %>%
    left_join(
      as.data.frame(dt_e[, .(
        Total_Estabelecimentos              = .N,
        Estabelecimentos_Com_Vinculos_Ativos= sum(qtd_vinculos_ativos > 0L, na.rm = TRUE),
        Vinculos_Ativos                     = sum(qtd_vinculos_ativos, na.rm = TRUE)
      ), by = .(Secao = Secao_CNAE)]),
      by = "Secao"
    ) %>%
    mutate(
      Grande_Setor = mapear_grande_setor_rais(Secao),
      across(c(Total_Estabelecimentos, Estabelecimentos_Com_Vinculos_Ativos, Vinculos_Ativos),
             ~ coalesce(as.integer(.), 0L))
    ) %>%
    select(Secao, Descricao_Secao, Grande_Setor, everything()) %>%
    arrange(desc(Estabelecimentos_Com_Vinculos_Ativos))

  nome_col_estoque <- sprintf("Estoque (RAIS) %d", as.integer(ano_ref))
  vb_estoque_str   <- format(estoque_total_to, big.mark = ".", decimal.mark = ",", trim = TRUE)
  vb_estab_str     <- format(total_estab_ativos_to, big.mark = ".", decimal.mark = ",", trim = TRUE)
  vb_rem_media_str <- paste0("R$ ", format(resumo_vinculos_to$remuneracao_media_nominal, big.mark = ".", decimal.mark = ",", nsmall = 2, trim = TRUE))

  resumo_rais <- list(
    data_processamento                  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    ano_rais                            = as.integer(ano_ref),
    nome_coluna_estoque_mun             = nome_col_estoque,
    vb_estoque_rais_to                  = vb_estoque_str,
    vb_estabelecimentos_ativos_to       = vb_estab_str,
    vb_remuneracao_media_to             = vb_rem_media_str,
    auditoria_metodologica              = auditoria_vinculos,
    resumo_vinculos_to                  = resumo_vinculos_to,
    vinculos_por_municipio              = vinculos_por_municipio,
    vinculos_por_faixa_etaria           = vinculos_por_faixa_etaria,
    vinculos_por_raca_cor               = vinculos_por_raca_cor,
    vinculos_por_sexo                   = vinculos_por_sexo,
    vinculos_por_grau_instrucao         = vinculos_por_grau_instrucao,
    vinculos_por_escolaridade_grupo     = vinculos_por_escolaridade_grupo,
    vinculos_por_secao_cnae             = vinculos_por_secao_cnae,
    vinculos_por_grande_setor           = vinculos_por_grande_setor,
    vinculos_grande_setor_por_municipio = vinculos_grande_setor_por_municipio,
    vinculos_secao_cnae_por_municipio   = vinculos_secao_cnae_por_municipio,
    resumo_estab_to                     = resumo_estab_to,
    estab_por_municipio                 = estab_por_municipio,
    estab_por_natureza_grupo_to         = estab_por_natureza_grupo_to,
    estab_por_natureza_detalhada_to     = estab_por_natureza_detalhada_to,
    estab_por_natureza_grupo_mun        = estab_por_natureza_grupo_mun,
    estab_por_natureza_detalhada_mun    = estab_por_natureza_detalhada_mun,
    estab_por_tamanho_to                = estab_por_tamanho_to,
    estab_por_tamanho_mun               = estab_por_tamanho_mun,
    estab_por_tipo_to                   = estab_por_tipo_to,
    estab_por_tipo_mun                  = estab_por_tipo_mun,
    estab_por_secao_cnae_to             = estab_por_secao_cnae_to
  )

  return(resumo_rais)
}

# ------------------------------------------------------------------------------
# 5. Orquestrador Principal da RAIS (Download -> Filtro UF=17 -> Limpeza -> Resumo)
# ------------------------------------------------------------------------------
executar_extracao_rais <- function(ano_alvo = NULL, forcar_reprocessamento = FALSE) {
  arq_base_rais   <- "dados/rais_to_base.rds"
  arq_resumo_rais <- "dados/rais_to_resumo.rds"

  # Verifica se há arquivos brutos locais colocados manualmente em tmp ou dados/brutos/rais
  arqs_brutos_locais <- c(
    list.files(dir_tmp_rais,        pattern = "^RAIS_(VINC_PUB_NORTE|ESTAB_PUB)\\.(7z|COMT|txt)$", ignore.case = TRUE),
    list.files("dados/brutos/rais", pattern = "^RAIS_(VINC_PUB_NORTE|ESTAB_PUB)\\.(7z|COMT|txt)$", ignore.case = TRUE)
  )
  tem_brutos_locais <- length(arqs_brutos_locais) > 0

  base_salva <- if (file.exists(arq_base_rais)) readRDS(arq_base_rais) else NULL
  ano_salvo  <- if (!is.null(base_salva) && !is.null(base_salva$ano_rais)) as.integer(base_salva$ano_rais) else NA_integer_

  if (is.null(ano_alvo)) {
    if (!is.na(ano_salvo) && !tem_brutos_locais && !forcar_reprocessamento) {
      # Consulta rápida ao FTP apenas se desejar verificar novo ano; mantém ano_salvo como fallback seguro
      ano_ftp <- descobrir_ultimo_ano_rais_ftp(ano_padrao = ano_salvo)
      ano_alvo <- max(ano_salvo, ano_ftp, na.rm = TRUE)
    } else {
      ano_alvo <- descobrir_ultimo_ano_rais_ftp(ano_padrao = 2025L)
    }
  } else {
    ano_alvo <- as.integer(ano_alvo[1])
  }

  precisa_ler_microdados <- is.na(ano_salvo) || (ano_alvo > ano_salvo) || tem_brutos_locais || forcar_reprocessamento

  if (precisa_ler_microdados) {
    cat(sprintf("-> [RAIS %d] Processando microdados de Vinculos e Estabelecimentos do MTE para o Tocantins...\n", ano_alvo))
    dt_vinc_to  <- ler_vinculos_rais_tocantins(ano_alvo)
    dt_estab_to <- ler_estabelecimentos_rais_tocantins(ano_alvo)

    if (!is.null(dt_vinc_to) && !is.null(dt_estab_to)) {
      base_salva <- list(
        ano_rais            = as.integer(ano_alvo),
        data_extracao       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        vinculos_to         = dt_vinc_to,
        estabelecimentos_to = dt_estab_to
      )
      saveRDS(base_salva, arq_base_rais, compress = "xz")
      cat(sprintf("   [OK] Base RAIS %d Tocantins salva em '%s' (%d vinculos no ano | %d estabelecimentos).\n",
                  ano_alvo, arq_base_rais, nrow(dt_vinc_to), nrow(dt_estab_to)))
    }
  } else {
    cat(sprintf("-> [RAIS %d] Base local sincronizada com a ultima divulgacao anual do MTE.\n", ano_salvo))
  }

  # Limpeza rigorosa de quaisquer arquivos pesados residuais (.7z, .COMT, .txt, .xls) em tmp_microdados
  arqs_lixo <- list.files(dir_tmp_rais, pattern = "\\.(7z|COMT|txt|csv|xls|xlsx)$", full.names = TRUE, ignore.case = TRUE)
  if (length(arqs_lixo) > 0) {
    try(file.remove(arqs_lixo), silent = TRUE)
  }

  if (!is.null(base_salva) && !is.null(base_salva$vinculos_to) && !is.null(base_salva$estabelecimentos_to)) {
    if (precisa_ler_microdados || !file.exists(arq_resumo_rais) || forcar_reprocessamento) {
      resumo_rais <- gerar_resumo_rais_to(
        dt_vinc_to  = base_salva$vinculos_to,
        dt_estab_to = base_salva$estabelecimentos_to,
        ano_ref     = base_salva$ano_rais
      )
      saveRDS(resumo_rais, arq_resumo_rais)
      cat(sprintf(
        "   [OK] Resumo RAIS %d TO: Estoque Ativo (s/ abandonados) = %s | Estab. Totais = %s (c/ vinculos = %s) | Rem. Media = %s\n",
        resumo_rais$ano_rais,
        resumo_rais$vb_estoque_rais_to,
        format(resumo_rais$resumo_estab_to$total_estabelecimentos, big.mark = ".", decimal.mark = ","),
        resumo_rais$vb_estabelecimentos_ativos_to,
        resumo_rais$vb_remuneracao_media_to
      ))
      return(invisible(resumo_rais))
    } else {
      return(invisible(readRDS(arq_resumo_rais)))
    }
  }

  return(NULL)
}

# ------------------------------------------------------------------------------
# 6. Execução Direta vs. Chamada Modular pelo atualizar_dados.R
# ------------------------------------------------------------------------------
args_cmd_rais <- commandArgs(trailingOnly = TRUE)
ano_cmd <- NULL
if (length(args_cmd_rais) > 0) {
  nums_ano <- regmatches(args_cmd_rais, regexpr("^(19|20)[0-9]{2}$", args_cmd_rais))
  if (length(nums_ano) > 0) ano_cmd <- as.integer(nums_ano[1])
}
forcar_cmd <- "--forcar" %in% args_cmd_rais

resumo_rais_to <- executar_extracao_rais(ano_alvo = ano_cmd, forcar_reprocessamento = forcar_cmd)

# Se o usuário rodou 'Rscript scripts/extrair_rais.R' diretamente no terminal
# (no dia em que sair a divulgação anual da RAIS), deposita automaticamente no Jornal e Boletins!
if (!exists(".chamado_por_atualizar_dados") || !isTRUE(.chamado_por_atualizar_dados)) {
  if (any(grepl("extrair_rais\\.R$", commandArgs(), ignore.case = TRUE)) && ! "--apenas-extrair" %in% args_cmd_rais) {
    cat("-> Depositando dados extraidos da RAIS no Jornal (painel_dados.rds) e Boletins...\n")
    .chamado_por_atualizar_dados <- TRUE
    if (file.exists("scripts/atualizar_dados.R")) {
      source("scripts/atualizar_dados.R", local = FALSE)
    }
    if (file.exists("scripts/compilar_boletins.R")) {
      source("scripts/compilar_boletins.R", local = FALSE)
    }
  }
}
