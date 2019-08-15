library(rvest)

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
  svMisc::progress(page, max.value = 20, progress.bar = T)
}

for(i in 1:nrow(players)){
  player <- read_html(
    x = paste("https://www.transfermarkt.us",
              players$url[i], sep = ""))
  # Verein
  player %>% html_nodes(".hauptpunkt") %>% html_text()
  # Ligahoehe
  player %>% html_nodes(".dataValue:nth-child(6)") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n")
  
  
  
  
  
  
  
  
  
  
}

