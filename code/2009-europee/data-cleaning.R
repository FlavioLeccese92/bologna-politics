###--------------------------------------------------------------------------###
####                              EUROPEE 2009                              ####
###                             data-cleaning.R                              ###
###--------------------------------------------------------------------------###

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

###------------------###
####    RISULTATI   ####
###------------------###

#### parlamento_voti ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-europee-2009-preferenze-alle-liste&q=&rows=10000&sort=-sezione")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
parlamento = x[["records"]][["fields"]]

parlamento_voti = parlamento %>%
  as_tibble()%>%
  filter(!is.na(sezione)) %>%
  select(sezione_elettorale = sezione,
         nulli, contestati, zona, quartiere,
         everything()) %>%
  pivot_longer(cols = names(.)[6:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(sezione_elettorale,
         nome_lista,
         voti_validi) %>%
  # mutate(nome_lista = nome_lista %>% gsub("_[^_]*$", "", .)) %>%
  arrange(sezione_elettorale, nome_lista)

saveRDS(parlamento_voti, "data/2009-europee/parlamento_voti.rds")

#### parlamento_affluenza ####
#### Mancano elettori per sezione nel 2009 -> usare dati 2008 e riproporzionare con numero iscritti 2009 = 301153 ####

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-politiche-2008-affluenza-votanti-senato-della-repubblica&q=&rows=10000")
x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
iscritti_2008 = x[["records"]][["fields"]]
iscritti_2008 = iscritti_2008 %>%
  mutate(iscritti = elettori_maschi + elettori_femmine) %>%
  mutate(iscritti = round(iscritti*301153/sum(.$iscritti, na.rm = TRUE), 0)) %>%
  select(sezione, iscritti)

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-europee-2009-dati-affluenza&q=&rows=10000&sort=-sezione&facet=descrizione_quartiere&facet=descrizione_zona")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
parlamento_affluenza = x[["records"]][["fields"]]

parlamento_affluenza = parlamento_affluenza %>%
  filter(!is.na(sezione)) %>%
  left_join(iscritti_2008, by = "sezione") %>%
  mutate(totale_votanti = voti_validi + schede_bianche + voti_nulli + voti_contestati) %>%
  select(sezione_elettorale = sezione, iscritti, totale_votanti, totale_voti_validi = voti_validi)

saveRDS(parlamento_affluenza, "data/2009-europee/parlamento_affluenza.rds")
