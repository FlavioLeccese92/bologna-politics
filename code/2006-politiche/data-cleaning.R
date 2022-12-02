###--------------------------------------------------------------------------###
####                             POLITICHE 2006                             ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

###------------------###
####    RISULTATI   ####
###------------------###

#### senato_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2006-voti-liste-senato-della-repubblica&q=&rows=10000&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
senato = x[["records"]][["fields"]]

senato_voti = senato %>%
  as_tibble() %>%
  select(id_sezione = sezione,nome_lista, voti_validi) %>%
  arrange(id_sezione, nome_lista)

saveRDS(senato, "data/2006-politiche/senato_voti.rds")

#### senato_affluenza ####
#### Mancano elettori per sezione nel 2006 -> usare dati 2008 e riproporzionare con numero iscritti 2006 = 289736 ####
res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-affluenza-votanti-senato-della-repubblica&q=&rows=10000")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
iscritti_2008 = x[["records"]][["fields"]]
iscritti_2008 = iscritti_2008 %>%
  mutate(iscritti = elettori_maschi + elettori_femmine) %>%
  mutate(iscritti = round(iscritti*289736/sum(.$iscritti, na.rm = TRUE), 0)) %>%
  select(id_sezione = sezione, iscritti)

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2006-affluenza-votanti-senato-della-repubblica&q=&rows=10000")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
senato_affluenza = x[["records"]][["fields"]]

senato_affluenza = senato_affluenza %>% rename(id_sezione = sezione) %>%
  left_join(iscritti_2008, by = "id_sezione") %>%
  mutate(totale_votanti = voti_validi + schede_nulle, voti_nulli, voti_contestati) %>%
  select(id_sezione, iscritti, totale_votanti, totale_voti_validi = voti_validi)

saveRDS(senato_affluenza, "data/2006-politiche/senato_affluenza.rds")

#### camera_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2006-voti-liste-camera-dei-deputati&q=&rows=10000&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera = x[["records"]][["fields"]]

camera_voti = camera %>%
  as_tibble() %>%
  select(id_sezione = sezione, nome_lista, voti_validi) %>%
  arrange(id_sezione, nome_lista)

saveRDS(camera_voti, "data/2006-politiche/camera_voti.rds")

#### camera_affluenza ####
#### Mancano elettori per sezione nel 2006 -> usare dati 2008 e riproporzionare con numero iscritti 2006 = 305577 ####
res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-affluenza-votanti-senato-della-repubblica&q=&rows=10000")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
iscritti_2008 = x[["records"]][["fields"]]
iscritti_2008 = iscritti_2008 %>%
  mutate(iscritti = elettori_maschi + elettori_femmine) %>%
  mutate(iscritti = round(iscritti*305577/sum(.$iscritti, na.rm = TRUE), 0)) %>%
  select(id_sezione = sezione, iscritti)

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2006-affluenza-votanti-camera-dei-deputati&q=&rows=10000&sort=-sezione")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
camera_affluenza = x[["records"]][["fields"]]

camera_affluenza = camera_affluenza  %>% rename(id_sezione = sezione) %>%
  left_join(iscritti_2008, by = "id_sezione") %>%
  mutate(totale_votanti = voti_validi + schede_nulle, voti_nulli, voti_contestati) %>%
  select(id_sezione, iscritti, totale_votanti, totale_voti_validi = voti_validi)

saveRDS(camera_affluenza, "data/2006-politiche/camera_affluenza.rds")
