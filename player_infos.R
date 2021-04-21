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


data <- page %>% 
  html_nodes(selectors) %>%
  html_text() %>%
  t() %>%
  as_tibble() %>%
  mutate(ID = id)

names(data) <- c("Name", "Height", "Goals", "Minutes", "ID")     
    
df <- tibble()

df %>% tibble::add_case(data)


for(page in 1:20){
  die_wertvollsten_spieler <- read_html(
    x = paste("https://www.transfermarkt.de/spieler-statistik/wertvollstespieler/marktwertetop?page=",
              page, sep = ""))
  # Name, ID und Link zum Profil
  spieler_name <- die_wertvollsten_spieler %>% 
    html_nodes("#yw1 .inline-table .hauptlink a")
  href <- spieler_name %>% html_attr("href")
  names <- spieler_name %>% html_text()
  ids <- spieler_name %>% html_attr("id")
  
  # Aktueller Marktwert
  spieler_wert <- die_wertvollsten_spieler %>% html_nodes("#yw1 b") %>%
    html_text() %>% 
    stringr::str_remove("Mio. €") %>% 
    stringr::str_replace(",", ".") %>%
    as.numeric()
  
  if(page == 1){
    players <- data.frame(name = names, 
                          url = 
                            href,
                          id = ids, 
                          "market_value (Mio.€)" = spieler_wert)
  } else {
    players <- rbind(players, 
                     data.frame(name = names, 
                                url = href, 
                                id = ids, 
                                "market_value (Mio.€)" = spieler_wert))
  }
  progress(page, max.value = 20)
}

# Get all variables from player profile box and save players profiles

vars <- list()
players_list <- list()

for(i in 1:500){
  players_list[[i]] <- read_html(
    x = paste("https://www.transfermarkt.us",
              players$url[i], sep = ""))
  data <- players_list[[i]] %>% html_nodes(".auflistung td , .auflistung th") %>% 
    html_text(trim = T)
  vars[[i]] <- data[seq(from = 1, to = length(data), by = 2)]
  progress(i, max.value = 500)
}

vars <- vars %>% unlist()
df <- as.data.frame(matrix(ncol=length(unique(vars)),nrow=nrow(players)))
colnames(df) <- unique(vars)
players_new <- cbind(players, df)

# Add variables from other places
vars <- c("current_club", "ligue", "ligue_level", "other_pos.","curr_int", "caps", "goals", "reached_max_val.", "max_val.")
df <- as.data.frame(matrix(ncol=length(unique(vars)),nrow=nrow(players)))
colnames(df) <- unique(vars)
players_final <- cbind(players_new, df)


for(i in 1:nrow(players)){
  player <- players_list[[i]]
  
  # Current Club
  players_final$current_club[i] <- player %>% html_nodes(".hauptpunkt") %>% html_text()
  
  # Liga
  ligue <- player %>% html_nodes(".mediumpunkt a") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n")
  
  if(length(ligue) ==1){players_final$ligue[i] <- ligue
  }else{
    players_final$ligue[i] <- NA
  }
  
  # Ligahoehe
  ligueh <- player %>% html_nodes(".dataValue:nth-child(6)") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n")
  
  if(length(ligueh) ==1){players_final$ligue_level[i] <- ligueh
  }else{
    players_final$ligue_level[i] <- NA
  }
  
  
  # Other positions
  op <- player %>% html_nodes(".nebenpositionen") %>% 
    html_text(trim = T) %>% stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_remove_all("\\(.*\\)") %>%
    stringr::str_remove_all("Other position: ")
  if(length(op) ==1){players_final$other_pos.[i] <- op
  }else{
    players_final$other_pos.[i] <- NA
  }
  # Current International
  ci <- player %>% html_nodes(".forMobile") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n") %>%
    stringr::str_remove_all("Current international:")
  if(length(ci) ==1){players_final$curr_int[i] <- ci
  }else{
    players_final$curr_int[i] <- NA
  }
  
  # Caps/Goals
  caps_goals <- player %>% html_nodes(".hide-for-small .dataValue a") %>% 
    html_text() %>% as.numeric()
  
  # Caps
  players_final$caps[i] <- caps_goals[1]
  
  # Goals
  players_final$goals[i] <- caps_goals[2] 
  
  # Reached Highest Market Value
  hmv <- player %>% html_nodes(".zeile-unten span") %>% 
    html_text(trim = T)
  if(length(hmv[2]) ==1){players_final$reached_max_val.[i] <- hmv[2]
  }else{
    players_final$reached_max_val.[i] <- NA
  }
  
  # Highest Market Value
  maxval <- player %>% html_nodes(".zeile-unten .right-td") %>% 
    html_text(trim = T) %>% readr::parse_number()
  
  if(length(maxval) ==1){players_final$max_val.[i] <- maxval
  }else{
    players_final$max_val.[i] <- NA
  }
  
  # Data from Players Profile
  pp_data <- player %>% html_nodes(".auflistung th , .auflistung td") %>% 
    html_text(trim = T)
  
  players_final[i,pp_data[seq(1,length(pp_data),2)]] <- pp_data[seq(2,length(pp_data),2)]
  
  progress(i, max.value = nrow(players_final))
}

players <- players_final

save(file = "soccer_data.rda", players)
