###--------------------------------------------------------------------------###
####                            concile-names.R                             ####
###--------------------------------------------------------------------------###

library(dplyr)
library(stringr)
library(tidyr)
library(rvest)
library(tools)

tree_table = tibble(
  anno = c("2022", "2022", "2021", "2020", "2019", "2018", "2018", "2018", "2016",
           "2014", "2013", "2013", "2011", "2009", "2009",
           "2008", "2008", "2006", "2006", "2004"),
  data = c("2022-09-25","2022-09-25", "2021-10-03", "2020-01-26", "2019-05-26", "2018-03-04", "2018-03-04", "2018-03-04", "2016-06-05",
           "2014-05-25", "2013-02-24", "2013-02-24", "2011-05-15", "2009-06-07", "2009-06-07", "2008-04-13",
           "2008-04-13", "2006-04-09", "2006-04-09", "2004-06-12"),
  votazione = c("politiche", "politiche", "comunali", "regionali", "europee", "politiche", "politiche", "politiche", "comunali",
                "europee", "politiche", "politiche", "comunali", "europee", "comunali",
                "politiche", "politiche", "politiche", "politiche", "comunali"),
  organo = c("camera", "senato", "consiglio_comunale", "consiglio_regionale", "parlamento", "camera06", "camera07", "senato", "consiglio_comunale",
             "parlamento", "camera", "senato", "consiglio_comunale", "parlamento", "consiglio_comunale",
             "camera", "senato", "camera", "senato", "consiglio_comunale"),
  url = c("https://dait.interno.gov.it/elezioni/trasparenza/elezioni-politiche-2022",
          "https://dait.interno.gov.it/elezioni/trasparenza/elezioni-politiche-2022",
          "https://elezionistorico.interno.gov.it/index.php?tpel=G&dtel=03/10/2021&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=8&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=R&dtel=26/01/2020&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&lev3=13&levsut3=3&levsut4=4&ne1=8&ne2=13&ne3=13&es0=S&es1=S&es2=S&es3=S&es4=N&ms=S&ne4=130060&lev4=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=E&dtel=26/05/2019&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=2&levsut1=1&lev2=8&levsut2=2&lev3=13&levsut3=3&levsut4=4&ne1=2&ne2=8&ne3=13&es0=S&es1=S&es2=S&es3=S&es4=N&ms=S&ne4=130060&lev4=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=C&dtel=04/03/2018&tpa=I&tpe=L&lev0=0&levsut0=0&lev1=11&levsut1=1&lev2=3&levsut2=2&levsut3=3&ne1=11&ne2=113&es0=S&es1=S&es2=S&es3=S&ms=S&ne3=1133&lev3=3",
          "https://elezionistorico.interno.gov.it/index.php?tpel=C&dtel=04/03/2018&tpa=I&tpe=L&lev0=0&levsut0=0&lev1=11&levsut1=1&lev2=3&levsut2=2&levsut3=3&ne1=11&ne2=113&es0=S&es1=S&es2=S&es3=S&ms=S&ne3=1134&lev3=4",
          "https://elezionistorico.interno.gov.it/index.php?tpel=S&dtel=04/03/2018&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=1&levsut2=2&lev3=4&levsut3=3&levsut4=4&ne1=8&ne2=81&ne3=814&es0=S&es1=S&es2=S&es3=S&es4=N&ms=S&ne4=140130060&lev4=130060",
          "https://elezionistorico.interno.gov.it/index.php?tpel=G&dtel=05/06/2016&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=8&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=E&dtel=25/05/2014&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=2&levsut1=1&lev2=8&levsut2=2&lev3=13&levsut3=3&levsut4=4&ne1=2&ne2=8&ne3=13&es0=S&es1=S&es2=S&es3=S&es4=N&ms=S&ne4=130060&lev4=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=C&dtel=24/02/2013&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=11&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=11&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=S&dtel=24/02/2013&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=8&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=G&dtel=15/05/2011&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=8&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=E&dtel=07/06/2009&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=2&levsut1=1&lev2=8&levsut2=2&lev3=13&levsut3=3&levsut4=4&ne1=2&ne2=8&ne3=13&es0=S&es1=S&es2=S&es3=S&es4=N&ms=S&ne4=130060&lev4=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=G&dtel=07/06/2009&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=8&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=C&dtel=13/04/2008&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=11&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=11&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=S&dtel=13/04/2008&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=8&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=C&dtel=09/04/2006&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=11&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=11&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=S&dtel=09/04/2006&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=8&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60",
          "https://elezionistorico.interno.gov.it/index.php?tpel=G&dtel=12/06/2004&tpa=I&tpe=C&lev0=0&levsut0=0&lev1=8&levsut1=1&lev2=13&levsut2=2&levsut3=3&ne1=8&ne2=13&es0=S&es1=S&es2=S&es3=N&ms=S&ne3=130060&lev3=60"),
  scrape_proc_contrassegni = c(3, 3, 2, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2),
  scrape_proc_risultati = c(3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1)
)  %>%
  mutate(key = paste0(anno, "-", votazione), .before = 1)%>%
  mutate(prefix_eligendo = paste0("./data/", key, "/eligendo_", organo),
         prefix = paste0("./data/", key, "/", organo),
         temp_html =  paste0(prefix_eligendo, ".html"),
         temp_files = paste0(prefix_eligendo, "_files"),
         temp_rds = paste0(prefix, "_voti.rds")) %>%
  select(-prefix_eligendo, -prefix)

contrassegni = NULL
nome_lista_conciliazione = NULL
eligendo_risultati = NULL
open_data_risultati = NULL

for(i in seq_len(nrow(tree_table))){
  url = tree_table$temp_html[i]

  files = list.files(tree_table$temp_files[i])
  file.remove(paste0(tree_table$temp_files[i], "/", files[!grepl(".png|.PNG", files)])) %>%
    suppressWarnings()

  if(tree_table$scrape_proc_risultati[i] == 3){
    temp_eligendo_risultati =  url %>%
      read_html() %>%
      html_nodes("#superTable") %>%
      html_table() %>%
      .[[1]] %>% .[c(3,4)] %>%
      rename(nome_lista_eligendo = 1, voti_validi = 2) %>%
      mutate(temp = ifelse(is.na(voti_validi), 1, 0) %>% cumsum(),
             voti_validi = voti_validi*1000,
             voti_validi = voti_validi %>% ifelse(.%%1000==0, ./1000, .)) %>%
      filter(!is.na(voti_validi), !(nome_lista_eligendo %in% c("Uninominale", "Proporzionale"))) %>%
      group_by(temp) %>%
      mutate(nome_coalizione_eligendo = first(nome_lista_eligendo) %>%
               gsub("ELETTO", "", .) %>% trimws(), .before = 1) %>%
      slice(-1) %>% ungroup() %>%
      select(-temp)
  }else{
    if(tree_table$scrape_proc_risultati[i] == 1){columns = c(1,2,6)
    }else if(tree_table$scrape_proc_risultati[i] == 2){columns = c(1,2,3)}

    temp_eligendo_risultati = url %>%
      read_html() %>%
      html_nodes(".table-responsive") %>%
      html_table() %>% .[[1]] %>%.[columns] %>%
      rename(nome_coalizione_eligendo = 1, nome_lista_eligendo = 2, voti_validi = 3) %>%
      mutate(nome_coalizione_eligendo = nome_coalizione_eligendo %>% ifelse(nchar(.) == 0, NA, .),
             voti_validi = voti_validi*1000,
             voti_validi = voti_validi %>% ifelse(.%%1000==0, ./1000, .)) %>%
      fill(nome_coalizione_eligendo) %>%
      mutate(nome_coalizione_eligendo = nome_coalizione_eligendo %>% ifelse(is.na(.), "temp", .)) %>%
      filter(!grepl("totale|totali", tolower(nome_lista_eligendo)),
             !grepl("totale|totali", tolower(nome_coalizione_eligendo)),
             nome_lista_eligendo != nome_coalizione_eligendo) %>%
      mutate(nome_coalizione_eligendo = nome_coalizione_eligendo %>% ifelse(. == "temp", nome_lista_eligendo, .))
  }

  temp_open_data_risultati = readRDS(tree_table$temp_rds[i]) %>%
    group_by(nome_lista) %>% summarise(voti_validi = sum(voti_validi, na.rm = TRUE), .groups = "drop")

  temp_nome_lista_conciliazione =
    left_join(
      temp_open_data_risultati %>%
        arrange(desc(voti_validi), nome_lista) %>%
        {if (tree_table$anno[i] == "2013" && tree_table$organo[i] == "senato"){bind_cols(., row_id = c(1:9, 11, 13, 10, 12, 14:18))}
         else mutate(., row_id = row_number())} %>%
        select(nome_lista_opendata = nome_lista, row_id),
      temp_eligendo_risultati %>% arrange(desc(voti_validi), nome_lista_eligendo) %>%
        mutate(row_id = row_number()) %>%
        select(nome_lista_eligendo, nome_coalizione_eligendo, row_id),
      by = "row_id"
    ) %>% select(-row_id)

  open_data_risultati = temp_open_data_risultati %>%
  mutate(key = tree_table$key[i], organo = tree_table$organo[i], .before = 1) %>%
  bind_rows(open_data_risultati, .)

  eligendo_risultati = temp_eligendo_risultati %>%
    mutate(key = tree_table$key[i], organo = tree_table$organo[i], .before = 1) %>%
    bind_rows(eligendo_risultati, .)

  nome_lista_conciliazione = temp_nome_lista_conciliazione %>%
    mutate(key = tree_table$key[i], organo = tree_table$organo[i], .before = 1) %>%
    bind_rows(nome_lista_conciliazione, .)

  if(tree_table$scrape_proc_contrassegni[i] == 1){
     temp_contrassegni = tibble(
       nome_lista_eligendo = url %>%
         read_html() %>%
         html_nodes(".simbolo_lista > a") %>%
         html_attr("title"),
       image = url %>%
         read_html() %>%
         html_nodes(".simbolo_lista > a > img") %>%
         html_attr("src") %>%
         gsub("\\./", "", .)
       )

    file.remove(paste0(tree_table$temp_files[i], "/e-logo-white.png"))

  }else if(tree_table$scrape_proc_contrassegni[i] == 2){
     temp_contrassegni = tibble(
       nome_lista_eligendo = url %>%
         read_html() %>%
         html_nodes(".simbolo_lista > img") %>%
         html_attr("title"),
       image = url %>%
         read_html() %>%
         html_nodes(".simbolo_lista > img") %>%
         html_attr("src") %>%
         gsub("\\./", "", .)
       )

    file.remove(paste0(tree_table$temp_files[i], "/e-logo-white.png"))

  }else{
     temp_contrassegni = tibble(
       nome_lista_eligendo = url %>%
         read_html() %>%
         html_nodes("#superTable > tbody > tr> .dt-center > img") %>%
         html_attr("title"),
       image = url %>%
         read_html() %>%
         html_nodes("#superTable > tbody > tr> .dt-center > img") %>%
         html_attr("src") %>%
         gsub("\\./", "", .)
     )
  }

  contrassegni = temp_contrassegni %>%
    mutate(key = tree_table$key[i], organo = tree_table$organo[i], .before = 1) %>%
    bind_rows(contrassegni, .)
}

saveRDS(tree_table, "data/general-porpuse/tree_table.rds")
saveRDS(contrassegni, "data/general-porpuse/contrassegni.rds")
saveRDS(nome_lista_conciliazione, "data/general-porpuse/nome_lista_conciliazione.rds")
saveRDS(eligendo_risultati, "data/general-porpuse/eligendo_risultati.rds")
saveRDS(open_data_risultati, "data/general-porpuse/open_data_risultati.rds")

# bkp #

#liste_conciliate =
#  bind_rows(
#    readRDS("data/2022-politiche/senato_plurinominale_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2022-politiche/camera_plurinominale_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2021-comunali/consiglio_comunale_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2020-regionali/consiglio_regionale_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2019-europee/parlamento_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2018-politiche/senato_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2018-politiche/camera_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2016-comunali/consiglio_comunale_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2014-europee/parlamento_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2013-politiche/senato_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2013-politiche/camera_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2011-comunali/consiglio_comunale_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2009-europee/parlamento_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2009-comunali/consiglio_comunale_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2008-politiche/senato_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2008-politiche/camera_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2006-politiche/senato_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2006-politiche/camera_liste_voti.rds") %>% distinct(nome_lista),
#    readRDS("data/2004-comunali/consiglio_comunale_voti.rds") %>% distinct(nome_lista)
#    ) #%>%
# mutate(nome_lista_cleaned = nome_lista %>%
#          gsub("_", " ", .) %>%
#          gsub("\\.", "", .) %>%
#          str_to_title() %>%
#          gsub(" D ", " d'", .) %>%
#          gsub(" L | L'", " l'", .) %>%
#          gsub("^L ", "L'", .) %>%
#          gsub("\\+|Piu ", "Più ", .) %>%
#          gsub("Dell", "dell", .) %>%
#          gsub(" E ", " e ", .) %>%
#          gsub(" Di ", " di ", .) %>%
#          gsub(" Per ", " per ", .) %>%
#          gsub(" Con ", " con ", .) %>%
#          gsub(" Fa ", " fa ", .) %>%
#          gsub(" Al ", " al ", .) %>%
#          gsub(" Si ", " si ", .),
#        nome_lista_cleaned = case_when(grepl("Lega", nome_lista_cleaned) ~ "Lega Nord",
#                                       grepl("ratelli", nome_lista_cleaned) ~ "Fratelli d'Italia",
#                                       grepl("Forza Italia", nome_lista_cleaned) ~ "Forza Italia",
#                                       grepl("P d'L", nome_lista_cleaned) ~ "Popolo delle Liberta",
#                                       grepl("Stelle|M5s", nome_lista_cleaned) ~ "Movimento 5 Stelle",
#                                       grepl("Partito Democratico|^P D$", nome_lista_cleaned) ~ "Partito Democratico",
#                                       grepl("U d'C", nome_lista_cleaned) ~ "Udc",
#                                       grepl("S e L", nome_lista_cleaned) ~ "Sel",
#                                       grepl("P R I", nome_lista_cleaned) ~ "Pri",
#                                       grepl("Msfiamma", nome_lista_cleaned) ~ "Fiamma",
#                                       TRUE ~ nome_lista_cleaned)) %>%
# distinct(nome_lista, nome_lista_cleaned)
