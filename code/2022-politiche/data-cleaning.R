###--------------------------------------------------------------------------###
####                             POLITICHE 2022                             ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

###------------------###
####    RISULTATI   ####
###------------------###

#### senato_plurinominale_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2022-senato-plurinominale&q=&rows=10000&sort=-sezione&facet=quartiere&facet=zona&facet=sezione&facet=nome_lista")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
senato_plurinominale_voti = x[["records"]][["fields"]]

senato_voti = senato_plurinominale_voti %>%
  as_tibble() %>%
  select(id_sezione = sezione,
         id_lista = numero_liste,
         nome_lista,
         voti_validi) %>%
  arrange(id_sezione, id_lista)

saveRDS(senato_voti, "data/2022-politiche/senato_voti.rds")

#### senato_uninominale_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2022-senato-uni&q=&rows=10000&facet=quartiere&facet=zona&facet=sezione&facet=candidato")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
senato_uninominale_voti = x[["records"]][["fields"]]

senato_uninominale_voti = senato_uninominale_voti %>%
  as_tibble() %>%
  select(id_sezione = sezione,
         nome_candidato = candidato,
         voti_validi) %>%
  arrange(id_sezione, nome_candidato)

saveRDS(senato_uninominale_voti, "data/2022-politiche/senato_uninominale_voti.rds")

#### senato_affluenza ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2022-affluenza-votanti-senato-delle-repubblica&q=&rows=10000&sort=-sezione&facet=quartiere&facet=zona&facet=sezione")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
senato_affluenza = x[["records"]][["fields"]]

senato_affluenza = senato_affluenza %>%
  select(id_sezione = sezione, iscritti = elettori, totale_votanti = votanti)

saveRDS(senato_affluenza, "data/2022-politiche/senato_affluenza.rds")

#### camera_plurinominale_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2022-camera-dei-deputati-plurinominale&q=&rows=10000&sort=-sezione&facet=quartiere&facet=zona&facet=sezione&facet=nome_lista")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera_plurinominale_voti = x[["records"]][["fields"]]

camera_voti = camera_plurinominale_voti %>%
  as_tibble() %>%
  select(id_sezione = sezione,
         id_lista = numero_liste,
         nome_lista,
         voti_validi) %>%
  arrange(id_sezione, id_lista)

saveRDS(camera_voti, "data/2022-politiche/camera_voti.rds")

#### camera_uninominale_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2022-camera-dei-deputati-uni&q=&rows=10000&facet=quartiere&facet=zona&facet=sezione&facet=candidato")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera_uninominale_voti = x[["records"]][["fields"]]

camera_uninominale_voti = camera_uninominale_voti %>%
  as_tibble() %>%
  select(id_sezione = sezione,
         nome_candidato = candidato,
         voti_validi) %>%
  arrange(id_sezione, nome_candidato)

saveRDS(camera_uninominale_voti, "data/2022-politiche/camera_uninominale_voti.rds")

#### camera_affluenza ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2022-affluenza-votanti-camera-dei-deputati&q=&rows=10000&sort=-sezione&facet=quartiere&facet=zona&facet=sezione")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera_affluenza = x[["records"]][["fields"]]

camera_affluenza = camera_affluenza %>%
  select(id_sezione = sezione, iscritti = elettori, totale_votanti = votanti)

saveRDS(camera_affluenza, "data/2022-politiche/camera_affluenza.rds")
