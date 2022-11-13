###--------------------------------------------------------------------------###
####                             COMUNALI 2011                              ####
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

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2011-consiglio&q=&rows=10000&sort=-sezione&facet=descrizione_quartiere&facet=descrizione_zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
consiglio_comunale = x[["records"]][["fields"]]

consiglio_comunale_voti = consiglio_comunale %>%
  as_tibble() %>%
  select(sezione, zona, descrizione_zona, qua, descrizione_quartiere,
         everything()) %>%
  pivot_longer(cols = names(.)[6:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(sezione_elettorale = sezione,
         nome_lista,
         voti_validi) %>%
  arrange(sezione_elettorale, nome_lista)

saveRDS(consiglio_comunale_voti, "data/2011-comunali/consiglio_comunale_voti.rds")

#### consiglio_comunale_affluenza ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2011-affluenza&q=&rows=10000&sort=-sezione&facet=quartiere&facet=zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
consiglio_comunale_affluenza = x[["records"]][["fields"]]

consiglio_comunale_affluenza = consiglio_comunale_affluenza %>%
  mutate(totale_votanti = voti_validi + schede_bianche + schede_nulle + voti_contestati) %>%
  select(sezione_elettorale = sezione, iscritti = elettori, totale_votanti, totale_voti_validi = voti_validi)

saveRDS(consiglio_comunale_affluenza, "data/2011-comunali/consiglio_comunale_affluenza.rds")

#### sindaco_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-amministrative-2011-risultati-votazioni-candidati-sindaci&q=&rows=10000&sort=-sezione&facet=descrizione_quartiere&facet=descrizione_zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
sindaco = x[["records"]][["fields"]]

sindaco_voti = sindaco  %>%
  as_tibble() %>%
  select(sezione, zona, descrizione_zona, quartiere, descrizione_quartiere, c,
         everything()) %>%
  pivot_longer(cols = names(.)[7:ncol(.)], names_to = "nome_candidato", values_to = "voti_validi") %>%
  select(sezione_elettorale = sezione,
         nome_candidato,
         voti_validi) %>%
  arrange(sezione_elettorale, nome_candidato)

saveRDS(sindaco_voti, "data/2011-comunali/sindaco_voti.rds")
