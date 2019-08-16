library(rvest)
library(dplyr)

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
  
  # Name in home country
  player %>% html_nodes(".auflistung tr:nth-child(1) td") %>% html_text()
  
  # Current Club
  player %>% html_nodes(".hauptpunkt") %>% html_text()
  
  # Liga
  player %>% html_nodes(".mediumpunkt a") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n")
  
  # Ligahoehe
  player %>% html_nodes(".dataValue:nth-child(6)") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n")
  
  # Joined
  player %>% html_nodes("tr:nth-child(11) td") %>% 
    html_text(trim = T)
  
  # Vertrag bis
  player %>% html_nodes(".dataValue:nth-child(12)") %>% 
    html_text()
  
  # Date of Birth
  player %>% html_nodes(".auflistung tr:nth-child(2) a") %>% 
    html_text()
  
  # Age
  player %>% html_nodes(".auflistung tr:nth-child(4) td") %>% 
    html_text() %>% as.numeric()
  
  # Place of Birth
  player %>% html_nodes(".spielerdaten span") %>% 
    html_text(trim = T)
  
  # Citizenship
  player %>% html_nodes(".auflistung tr:nth-child(6) td") %>% 
    html_text(trim = T)
  
  # Height
  player %>% html_nodes(".dataDaten:nth-child(2) p:nth-child(1) .dataValue") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n")

  # Position
  player %>% html_nodes(".hauptposition-left") %>% 
    html_text(trim = T) %>% stringr::str_remove_all("Main position") %>%
    stringr::str_remove_all(":") %>%
    stringr::str_remove_all(" ")
  
  # Other positions
  player %>% html_nodes(".nebenpositionen") %>% 
    html_text(trim = T) %>% stringr::str_remove_all("Other position") %>%
    stringr::str_remove_all(":") %>%
    stringr::str_remove_all("\\(.*\\)") %>%
    stringr::str_remove_all(" ") %>% stringr::str_remove_all("\\n")
  
  
  # Agent
  player %>% html_nodes(".show-for-small+ p a") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n")
  
  # Current International
  player %>% html_nodes(".dataValue .tooltipstered") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n")

  # Current International
  player %>% html_nodes(".forMobile") %>% 
    html_text() %>% stringr::str_remove_all("\\t") %>% 
    stringr::str_remove_all("\\n") %>%
    str_remove_all("Current international:")
  
  # Caps/Goals
  caps_goals <- player %>% html_nodes(".hide-for-small .dataValue a") %>% 
    html_text() %>% as.numeric()
  
  # Caps
  caps_goals[1]
  
  # Goals
  caps_goals[2]
  
  # Foot:
  player %>% html_nodes(".auflistung tr:nth-child(8) td") %>% 
    html_text()
  
  #
  
  

  
  
  
}
  
  
  
  
  
  
  
  
  
  
}

