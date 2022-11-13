###--------------------------------------------------------------------------###
####                             COMUNALI 2009                              ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)


###------------------###
####    RISULTATI   ####
###------------------###

#### consiglio_comunale_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2009-voti-alle-liste-per-il-consiglio-comunale&q=&rows=10000&facet=descrizione_quartiere&facet=descrizione_zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
consiglio_comunale = x[["records"]][["fields"]]

consiglio_comunale_voti = consiglio_comunale %>%
  as_tibble() %>%
  select(sezione, zona, descrizione_zona, quartiere, descrizione_quartiere,
         nulle, cont,
         everything()) %>%
  pivot_longer(cols = names(.)[8:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(sezione_elettorale = sezione,
         nome_lista,
         voti_validi) %>%
  arrange(sezione_elettorale, nome_lista)

saveRDS(consiglio_comunale_voti, "data/2009-comunali/consiglio_comunale_voti.rds")

#### consiglio_comunale_affluenza ####
#### Mancano elettori per sezione nel 2009 -> usare dati 2008 e riproporzionare con numero iscritti 2009 = 305086 ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-affluenza-votanti-senato-della-repubblica&q=&rows=10000")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
iscritti_2008 = x[["records"]][["fields"]]
iscritti_2008 = iscritti_2008 %>%
  mutate(iscritti = elettori_maschi + elettori_femmine) %>%
  mutate(iscritti = round(iscritti*305086/sum(.$iscritti, na.rm = TRUE), 0)) %>%
  select(sezione, iscritti)

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2009-affluenza-consiglio&q=&rows=10000&facet=descrizione_quartiere&facet=descrizione_zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
consiglio_comunale_affluenza = x[["records"]][["fields"]]

consiglio_comunale_affluenza = consiglio_comunale_affluenza %>%
  left_join(iscritti_2008, by = "sezione") %>%
  mutate(totale_votanti = voti_validi + schede_bianche + schede_nulle + voti_contestati) %>%
  select(sezione_elettorale = sezione, iscritti, totale_votanti, totale_voti_validi = voti_validi)

saveRDS(consiglio_comunale_affluenza, "data/2009-comunali/consiglio_comunale_affluenza.rds")

#### sindaco_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2009-risultati-votazioni-candidati-sindaci&q=&rows=10000&sort=-sezione&facet=descrizione_quartiere&facet=descrizione_zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
sindaco = x[["records"]][["fields"]]

sindaco_voti = sindaco  %>%
  as_tibble() %>%
  select(sezione, zona, descrizione_zona, quartiere, descrizione_quartiere, cont, nulli,
         everything()) %>%
  pivot_longer(cols = names(.)[8:ncol(.)], names_to = "nome_candidato", values_to = "voti_validi") %>%
  select(sezione_elettorale = sezione,
         nome_candidato,
         voti_validi) %>%
  arrange(sezione_elettorale, nome_candidato)

saveRDS(sindaco_voti, "data/2009-comunali/sindaco_voti.rds")

#### sindaco_affluenza ####
#### Mancano elettori per sezione nel 2009 -> usare dati 2008 e riproporzionare con numero iscritti 2009 = 305086 ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-affluenza-votanti-senato-della-repubblica&q=&rows=10000")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
iscritti_2008 = x[["records"]][["fields"]]
iscritti_2008 = iscritti_2008 %>%
  mutate(iscritti = elettori_maschi + elettori_femmine) %>%
  mutate(iscritti = round(iscritti*305086/sum(.$iscritti, na.rm = TRUE), 0)) %>%
  select(sezione, iscritti)

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2009-affluenza&q=&rows=10000&facet=descrizione_quartiere&facet=descrizione_zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
sindaco_affluenza = x[["records"]][["fields"]]

sindaco_affluenza = sindaco_affluenza %>%
  left_join(iscritti_2008, by = "sezione") %>%
  mutate(totale_votanti = voti_validi + schede_bianche + schede_nulle + voti_contestati) %>%
  select(sezione_elettorale = sezione, iscritti, totale_votanti, totale_voti_validi = voti_validi)

saveRDS(sindaco_affluenza, "data/2009-comunali/sindaco_affluenza.rds")
