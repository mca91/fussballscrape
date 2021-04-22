library(rvest)
library(dplyr)
library(svMisc)
library(tidyverse)
rm(list = ls())

# CSS selectors
name <- "h1" # full name
goals <- "tfoot .zentriert:nth-child(7)" # goals in 1BL
time_played <- "tfoot .zentriert~ .zentriert+ .rechts" # minutes played in 1BL
height <- ".dataDaten:nth-child(2) p:nth-child(1) .dataValue" # height

url1 <- "https://www.transfermarkt.de/michael-zorc/leistungsdatendetails/spieler/"
url2 <- "/plus/0?saison=&verein=&liga=&wettbewerb=L1&pos=&trainer_id="

get_data <- function(id) {
  
  full_url <- paste0(url1, id, url2)
  
  page <- read_html(full_url) 
  
  p_name <- page %>% html_nodes(name) %>% html_text() %>% ifelse(is_empty(.), NA, .)
  p_height <- page %>% html_nodes(height) %>% html_text() %>% ifelse(is_empty(.), NA, .)
  p_goals <- page %>% html_nodes(goals) %>% html_text() %>% ifelse(is_empty(.), NA, .)
  p_time <- page %>% html_nodes(time_played) %>% html_text() %>% ifelse(is_empty(.), NA, .)
  
  data <- c(p_name, p_height, p_goals, p_time) %>%
    t() %>%
    as_tibble() %>%
    mutate_all(as.character) %>%
    mutate(ID = id)
  
  names(data) <- c("Name", "Height", "Goals", "Minutes", "ID")    
  
  return(data)
}

# thresholds where current status is saved to RDS file
thresholds <- seq(500, 5e5, 500)

# initialise
df <- get_data(20000)

for (id in 20001:3e4) {
  
  cat("Getting data for player ID", id, "...", "\n")
  
  new <- get_data(id)
  
  df <- df %>% add_case(new)
  
  Sys.sleep(3.6)
  
  if(id %in% thresholds) {
    saveRDS(df, file = paste0("TM_data", "_", id, ".RDS"))
  }
  
}

