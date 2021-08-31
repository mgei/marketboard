library(shiny)
library(shinydashboard)
library(DT)
library(shiny.router)

modify_stop_propagation <- function(x) {
  x$children[[1]]$attribs$onclick = "event.stopPropagation()"
  x
}

library(tidyverse)
library(jsonlite)
library(lubridate)

if (!dir.exists("cache")) dir.create("cache")

get_fmp <- function(x = "earning_calendar", apikey, version = "v3",
                    cache = T) {
  file <- paste0("cache/", x, ".RDS")
  
  if (file.exists(file) & cache) {
    return(readRDS(file))
  }
  
  t0 <- Sys.time()
  url <- paste0("https://fmpcloud.io/api/", version, "/", x, "?apikey=", apikey)
  raw <- read_json(url, simplifyVector = T) %>% 
    as_tibble()
  
  if (x %in% c("earning_calendar", "ipo_calendar", "stock_split_calendar",
               "stock_dividend_calendar")) {
    df <- raw %>% 
      mutate(date = as.Date(date))
  } else if (x %in% c("economic_calendar", "rss_feed")) {
    df <- raw %>% 
      mutate(date = ymd_hms(date))
  } else if (x %in% c("stock_news")) {
    df <- raw %>% 
      mutate(publishedDate = ymd_hms(publishedDate))
  } else if (x %in% c("insider-trading-rss-feed")) {
    df <- raw %>% 
      mutate(fillingDate = ymd_hms(fillingDate))
  } else {
    df <- raw
  }
  
  out <- list(t0 = t0,
              url = url,
              raw = raw,
              df = df)
  saveRDS(out, file)
  return(out)
}

fmp_search <- function(search, apikey) {
  url <- paste0("https://fmpcloud.io/api/v3/search?query=",
                search,
                "&limit=20&apikey=", apikey)
  
  raw <- read_json(url, simplifyVector = T) %>% 
    as_tibble()
  
  return(raw)
}

apikey_handler <- function(x = NULL, hide = F) {
  file <- "cache/apikey.RDS"
  
  if (is.null(x)) {
    if (file.exists(file)) {
      x <- readRDS(file)
    } else {
      x <- NULL
    }
    if (hide) {
      x <- apikey_hider(x)
    }
    
    return(x)
  } else {
    saveRDS(x, file)
  }
}

apikey_hider <- function(x = NULL, hide_but = 4) {
  if (is.null(x)) return("")
  
  len <- str_length(x)
  lst <- str_sub(x, start = len-hide_but+1)
  out <- paste0(strrep("*", len-hide_but), lst)
  return(out)
}

dt_layout <- function(dt) {
  NULL
}

