# ==============================================================================
# JORNAL ECONÔMICO DO TOCANTINS (JET)
# Script: scripts/atualizar_mercados_json.R
# Objetivo:
#   Coletar cotações reais e séries históricas (90 dias) de câmbio, bolsa,
#   criptoativos, commodities agrícolas (CBOT/CME) e juros/inflação (BCB SGS)
#   para alimentar a aba "Câmbio e Mercados" em dados/mercados_tempo_real.json.
# ==============================================================================

if (!file.exists("index.qmd") && dir.exists("D:/Journal")) {
  setwd("D:/Journal")
}

suppressPackageStartupMessages({
  library(jsonlite)
  library(curl)
})

dir.create("dados", recursive = TRUE, showWarnings = FALSE)

fetch_json_safe <- function(url, timeout_sec = 10) {
  tryCatch({
    h <- curl::new_handle(
      followlocation = TRUE,
      timeout = timeout_sec,
      useragent = "Mozilla/5.0 (compatible; JET-Observatorio/1.0)"
    )
    res <- curl::curl_fetch_memory(url, handle = h)
    if (res$status_code >= 200 && res$status_code < 300) {
      txt <- rawToChar(res$content)
      Encoding(txt) <- "UTF-8"
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

num_safe <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  v <- suppressWarnings(as.numeric(x[[1]]))
  if (length(v) == 0) NA_real_ else v
}

extrair_yahoo_chart <- function(symbol, range_str = "3mo") {
  sym_enc <- utils::URLencode(symbol, reserved = TRUE)
  url <- sprintf("https://query1.finance.yahoo.com/v8/finance/chart/%s?interval=1d&range=%s", sym_enc, range_str)
  j <- fetch_json_safe(url, timeout_sec = 10)
  if (is.null(j) || is.null(j$chart$result) || length(j$chart$result) == 0) return(NULL)

  res <- j$chart$result[[1]]
  meta <- res$meta
  ts_list <- unlist(res$timestamp)
  closes_raw <- res$indicators$quote[[1]]$close

  dates_iso <- character(0)
  vals <- numeric(0)
  if (!is.null(ts_list) && !is.null(closes_raw)) {
    n <- min(length(ts_list), length(closes_raw))
    for (i in seq_len(n)) {
      v <- num_safe(closes_raw[[i]])
      t_val <- num_safe(ts_list[[i]])
      if (!is.na(v) && is.finite(v) && !is.na(t_val)) {
        d_str <- format(as.POSIXct(t_val, origin = "1970-01-01", tz = "America/Sao_Paulo"), "%Y-%m-%d")
        dates_iso <- c(dates_iso, d_str)
        vals <- c(vals, v)
      }
    }
  }

  # Remove datas duplicadas mantendo a mais recente do dia
  if (length(dates_iso) > 0) {
    keep <- !duplicated(dates_iso, fromLast = TRUE)
    dates_iso <- dates_iso[keep]
    vals <- vals[keep]
  }

  price <- num_safe(meta$regularMarketPrice)
  if (is.na(price) && length(vals) > 0) price <- tail(vals, 1)

  prev <- num_safe(meta$previousClose)
  if (is.na(prev) || !is.finite(prev)) {
    if (length(vals) >= 2) {
      prev <- vals[length(vals) - 1]
    } else {
      prev <- num_safe(meta$chartPreviousClose)
    }
  }

  pct <- if (!is.na(price) && !is.na(prev) && prev > 0) ((price - prev) / prev) * 100 else 0
  high <- num_safe(meta$regularMarketDayHigh)
  low  <- num_safe(meta$regularMarketDayLow)
  if (is.na(high)) high <- price
  if (is.na(low))  low  <- price

  list(
    symbol   = symbol,
    currency = if (!is.null(meta$currency)) meta$currency else "",
    price    = price,
    prev     = prev,
    pct      = round(pct, 3),
    high     = high,
    low      = low,
    dates    = dates_iso,
    values   = vals
  )
}

cat("[INFO] Coletando cotações em tempo real (AwesomeAPI, Yahoo Finance, CoinGecko, BCB)...\n")

# 1. Câmbio & Bitcoin (AwesomeAPI)
aw_last <- fetch_json_safe("https://economia.awesomeapi.com.br/last/USD-BRL,USD-BRLT,EUR-BRL,BTC-BRL")

parse_aw_quote <- function(obj, fallback_yahoo = NULL) {
  if (!is.null(obj) && !is.null(obj$bid)) {
    list(
      bid  = as.numeric(obj$bid),
      high = as.numeric(obj$high),
      low  = as.numeric(obj$low),
      pct  = as.numeric(obj$pctChange)
    )
  } else if (!is.null(fallback_yahoo) && !is.na(fallback_yahoo$price)) {
    list(
      bid  = fallback_yahoo$price,
      high = fallback_yahoo$high,
      low  = fallback_yahoo$low,
      pct  = fallback_yahoo$pct
    )
  } else {
    NULL
  }
}

yh_usd  <- extrair_yahoo_chart("BRL=X", "3mo")
yh_eur  <- extrair_yahoo_chart("EURBRL=X", "3mo")
yh_ibov <- extrair_yahoo_chart("^BVSP", "3mo")
yh_soja <- extrair_yahoo_chart("ZS=F", "5d")
yh_milho <- extrair_yahoo_chart("ZC=F", "5d")
yh_boi  <- extrair_yahoo_chart("LE=F", "5d")

q_usd  <- parse_aw_quote(aw_last$USDBRL, yh_usd)
q_usdt <- parse_aw_quote(aw_last$USDBRLT, q_usd)
q_eur  <- parse_aw_quote(aw_last$EURBRL, yh_eur)
q_btc  <- parse_aw_quote(aw_last$BTCBRL, NULL)

# Fallback CoinGecko para BTC caso AwesomeAPI BTC falhe
if (is.null(q_btc)) {
  cg_now <- fetch_json_safe("https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&vs_currencies=brl&include_24hr_change=true")
  if (!is.null(cg_now$bitcoin$brl)) {
    p_btc <- as.numeric(cg_now$bitcoin$brl)
    q_btc <- list(
      bid  = p_btc,
      high = p_btc,
      low  = p_btc,
      pct  = round(as.numeric(cg_now$bitcoin$brl_24h_change), 2)
    )
  }
}

# 2. Conversão real de Commodities (CBOT / CME -> Unidades Regionais TO)
converter_commodity_usd <- function(q_yh, fator) {
  if (is.null(q_yh) || is.na(q_yh$price)) return(NULL)
  mult <- if (toupper(q_yh$currency) == "USX") 0.01 else 1.0
  list(
    usd_unit   = round(q_yh$price * mult * fator, 2),
    pct        = q_yh$pct,
    futuro_raw = q_yh$price,
    symbol     = q_yh$symbol
  )
}

# Soja: bushels (27.2155 kg) -> saca 60 kg (60 / 27.2155 = 2.20462262)
c_soja  <- converter_commodity_usd(yh_soja, 60 / 27.2155)
# Milho: bushels (25.4012 kg) -> saca 60 kg (60 / 25.4012 = 2.362099)
c_milho <- converter_commodity_usd(yh_milho, 60 / 25.4012)
# Boi Gordo CME (Live Cattle): libra (0.45359237 kg) -> 15 kg (15 / 0.45359237 = 33.06934)
c_boi   <- converter_commodity_usd(yh_boi, 15 / 0.45359237)

# 3. Séries Históricas 90 dias (USD, EUR, IBOV, BTC)
aw_usd_hist <- fetch_json_safe("https://economia.awesomeapi.com.br/json/daily/USD-BRL/90")
aw_eur_hist <- fetch_json_safe("https://economia.awesomeapi.com.br/json/daily/EUR-BRL/90")

normalizar_aw_hist <- function(lst) {
  if (is.null(lst) || length(lst) == 0) return(list(dates = character(0), values = numeric(0)))
  ts <- vapply(lst, function(x) suppressWarnings(as.numeric(x$timestamp)), numeric(1))
  vs <- vapply(lst, function(x) suppressWarnings(as.numeric(x$bid)), numeric(1))
  ord <- order(ts)
  ts <- ts[ord]
  vs <- vs[ord]
  ds <- format(as.POSIXct(ts, origin = "1970-01-01", tz = "America/Sao_Paulo"), "%Y-%m-%d")
  ok <- !is.na(vs) & is.finite(vs) & !is.na(ds)
  ds <- ds[ok]; vs <- vs[ok]
  keep <- !duplicated(ds, fromLast = TRUE)
  list(dates = ds[keep], values = round(vs[keep], 4))
}

hist_usd <- normalizar_aw_hist(aw_usd_hist)
if (length(hist_usd$dates) < 5 && !is.null(yh_usd)) {
  hist_usd <- list(dates = yh_usd$dates, values = round(yh_usd$values, 4))
}
hist_eur <- normalizar_aw_hist(aw_eur_hist)
if (length(hist_eur$dates) < 5 && !is.null(yh_eur)) {
  hist_eur <- list(dates = yh_eur$dates, values = round(yh_eur$values, 4))
}
hist_ibov <- if (!is.null(yh_ibov) && length(yh_ibov$dates) > 0) {
  list(dates = yh_ibov$dates, values = round(yh_ibov$values, 2))
} else {
  list(dates = character(0), values = numeric(0))
}

# BTC 90d via CoinGecko
cg_hist <- fetch_json_safe("https://api.coingecko.com/api/v3/coins/bitcoin/market_chart?vs_currency=brl&days=90&interval=daily")
hist_btc <- list(dates = character(0), values = numeric(0))
if (!is.null(cg_hist$prices) && length(cg_hist$prices) > 0) {
  d_btc <- vapply(cg_hist$prices, function(pt) {
    format(as.POSIXct(as.numeric(pt[[1]]) / 1000, origin = "1970-01-01", tz = "America/Sao_Paulo"), "%Y-%m-%d")
  }, character(1))
  v_btc <- vapply(cg_hist$prices, function(pt) round(as.numeric(pt[[2]]), 0), numeric(1))
  keep <- !duplicated(d_btc, fromLast = TRUE)
  hist_btc <- list(dates = d_btc[keep], values = v_btc[keep])
}

# 4. Macro Brasil Ao Vivo via API oficial do Banco Central (SGS)
bcb_selic <- fetch_json_safe("https://api.bcb.gov.br/dados/serie/bcdata.sgs.432/dados/ultimos/1?formato=json")
bcb_ipca  <- fetch_json_safe("https://api.bcb.gov.br/dados/serie/bcdata.sgs.13522/dados/ultimos/1?formato=json")

selic_fmt <- NULL
ipca_fmt  <- NULL
if (!is.null(bcb_selic) && length(bcb_selic) >= 1 && !is.null(bcb_selic[[1]]$valor)) {
  v <- suppressWarnings(as.numeric(bcb_selic[[1]]$valor))
  if (!is.na(v)) selic_fmt <- paste0(format(round(v, 2), decimal.mark = ",", nsmall = 2), "% a.a.")
}
if (!is.null(bcb_ipca) && length(bcb_ipca) >= 1 && !is.null(bcb_ipca[[1]]$valor)) {
  v <- suppressWarnings(as.numeric(bcb_ipca[[1]]$valor))
  if (!is.na(v)) ipca_fmt <- paste0(format(round(v, 2), decimal.mark = ",", nsmall = 2), "%")
}

# Preserva snapshot anterior caso alguma API tenha falhado momentaneamente
prev_snap <- if (file.exists("dados/mercados_tempo_real.json")) {
  tryCatch(jsonlite::read_json("dados/mercados_tempo_real.json", simplifyVector = FALSE), error = function(e) NULL)
} else NULL

fallback_or <- function(val, path) {
  if (!is.null(val)) return(val)
  if (!is.null(prev_snap)) {
    cur <- prev_snap
    for (p in path) {
      if (is.list(cur) && !is.null(cur[[p]])) cur <- cur[[p]] else return(NULL)
    }
    return(cur)
  }
  NULL
}

saida <- list(
  atualizado_iso = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "America/Sao_Paulo"),
  atualizado_brt = format(Sys.time(), "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo"),
  cotacoes = list(
    usd  = fallback_or(q_usd,  c("cotacoes", "usd")),
    usdt = fallback_or(q_usdt, c("cotacoes", "usdt")),
    eur  = fallback_or(q_eur,  c("cotacoes", "eur")),
    btc  = fallback_or(q_btc,  c("cotacoes", "btc")),
    ibov = fallback_or(
      if (!is.null(yh_ibov)) list(pontos = round(yh_ibov$price, 2), variacao = yh_ibov$pct, high = round(yh_ibov$high, 2), low = round(yh_ibov$low, 2)) else NULL,
      c("cotacoes", "ibov")
    )
  ),
  commodities = list(
    soja  = fallback_or(c_soja,  c("commodities", "soja")),
    boi   = fallback_or(c_boi,   c("commodities", "boi")),
    milho = fallback_or(c_milho, c("commodities", "milho"))
  ),
  macro = list(
    selic_meta = fallback_or(selic_fmt, c("macro", "selic_meta")),
    ipca_12m   = fallback_or(ipca_fmt,  c("macro", "ipca_12m"))
  ),
  series_90d = list(
    usd  = if (length(hist_usd$dates)  > 0) hist_usd  else fallback_or(NULL, c("series_90d", "usd")),
    eur  = if (length(hist_eur$dates)  > 0) hist_eur  else fallback_or(NULL, c("series_90d", "eur")),
    ibov = if (length(hist_ibov$dates) > 0) hist_ibov else fallback_or(NULL, c("series_90d", "ibov")),
    btc  = if (length(hist_btc$dates)  > 0) hist_btc  else fallback_or(NULL, c("series_90d", "btc"))
  )
)

jsonlite::write_json(saida, "dados/mercados_tempo_real.json", auto_unbox = TRUE, digits = 6, pretty = FALSE)
cat(sprintf("[OK] dados/mercados_tempo_real.json salvo! USD=%.4f | IBOV=%.0f (%.2f%%) | Soja=US$%.2f/sc | Boi=US$%.2f/@ | Milho=US$%.2f/sc | Selic=%s\n",
            saida$cotacoes$usd$bid, saida$cotacoes$ibov$pontos, saida$cotacoes$ibov$variacao,
            saida$commodities$soja$usd_unit, saida$commodities$boi$usd_unit, saida$commodities$milho$usd_unit,
            ifelse(is.null(saida$macro$selic_meta), "--", saida$macro$selic_meta)))
