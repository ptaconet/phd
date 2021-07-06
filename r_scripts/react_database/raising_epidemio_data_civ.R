## raising epidemio data from CIV

library(tidyverse)
library(DBI)
### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 

### open the tables
epidemio_ci <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% dplyr::select(-fid) %>% filter(codepays == "CI", especeanoph == "An.gambiae s.l.")

an_gam_sl <- epidemio_ci %>%
  group_by(idpointdecapture,nummission,codevillage,pointdecapture,postedecapture,idpostedecapture,heuredecapture,genre,especeanoph) %>%
  summarise(n_gamsl_tot=n())

an_gam_sl_echant <- epidemio_ci %>%
  filter(!is.na(pcr_espece)) %>%
  group_by(idpointdecapture,nummission,codevillage,pointdecapture,postedecapture,idpostedecapture,heuredecapture,genre,especeanoph) %>%
  summarise(n_gamsl_echant=n())

an_gam_ss <-  epidemio_ci %>%
  filter(pcr_espece=="An.gambiae_ss") %>%
  group_by(idpointdecapture,nummission,codevillage,pointdecapture,postedecapture,idpostedecapture,heuredecapture,genre,especeanoph) %>%
  summarise(n_gamss_echant=n())

an_col <-  epidemio_ci %>%
  filter(pcr_espece=="An.coluzzii") %>%
  group_by(idpointdecapture,nummission,codevillage,pointdecapture,postedecapture,idpostedecapture,heuredecapture,genre,especeanoph) %>%
  summarise(n_col_echant=n())

# signification des 3 dernières colonnes dans la table suivante (an_gam) : 
# n_gam_tot_sl = nb total de gambiae sl capturés
# n_gam_sl_echant = nb échantillonés à l'espèce
# n_gam_ss = nb de gambiae ss identifiés
# n_col = nb de coluzzii identifiés
an_gam <- an_gam_sl %>%
  left_join(an_gam_sl_echant) %>%
  left_join(an_gam_ss) %>%
  left_join(an_col)
