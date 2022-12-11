###--------------------------------------------------------------------------###
####                              EUROPEE 2014                              ####
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

res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-europee-2014-preferenze-alle-liste&q=&rows=10000&sort=-sezione")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
parlamento = x[["records"]][["fields"]]

parlamento_voti = parlamento %>%
  as_tibble() %>%
  select(id_sezione = sezione,
         totale_votanti,  totale_voti_validi, totale_voti_non_validi,
         totale_schede_nulle, totale_schede_bianche, totale_schede_contestate,
         schede_bianche_859, schede_nulle_2857, contestate_non_assegnate_39, zona, quar,
         everything()) %>%
  pivot_longer(cols = names(.)[13:ncol(.)], names_to = "nome_lista", values_to = "voti_validi") %>%
  select(id_sezione,
         nome_lista,
         voti_validi) %>%
  mutate(nome_lista = nome_lista %>% gsub("_[^_]*$", "", .)) %>%
  arrange(id_sezione, nome_lista)

saveRDS(parlamento_voti, "data/2014-europee/parlamento_voti.rds")

#### parlamento_affluenza ####
res = GET("https://opendata.comune.bologna.it/api/records/1.0/search/?dataset=elezioni-europee-2014-dati-affluenza&q=&rows=10000&sort=-nr_sez")

x = jsonlite::fromJSON(httr::content(res, 'text', encoding = "UTF-8"))
parlamento_affluenza = x[["records"]][["fields"]]

parlamento_affluenza = parlamento_affluenza %>%
  select(id_sezione = nr_sez, iscritti = iscr_tot, totale_votanti = vot_tot) %>%
  left_join(parlamento %>% select(id_sezione = sezione, totale_voti_validi), by = "id_sezione")

saveRDS(parlamento_affluenza, "data/2014-europee/parlamento_affluenza.rds")
