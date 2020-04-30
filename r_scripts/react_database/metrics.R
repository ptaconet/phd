##### script to compute various entomological and epidemiological indicators from the REACT database
##### indicators definitions are extracted from Tusting et al., 2014   https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4847140/

library(DBI)
library(RSQLite)
require(tidyverse)

#path_to_gpkg_database <- "data/react_db/react_db.gpkg"
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_gpkg_database)

# --- ### --- ###--- ###--- ###--- ###--- ###
######## Entomological indicators ##########
# --- ### --- ###--- ###--- ###--- ###--- ###


######## Ma	: Human biting rate (Number of bites by vector mosquitoes received per human per night) ########

# # get the table of CSH points (2 rows by CSH point - 1 for interior and 1 for exterior)&
idpointdecaptures <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>%
   dplyr::select(idpointdecapture) 

 csh_exterior <- data.frame(idpointdecapture = idpointdecaptures, postedecapture = "e")
 csh_interior <- data.frame(idpointdecapture = idpointdecaptures, postedecapture = "i")
 idpostesdecaptures <- rbind(csh_exterior,csh_interior) %>%
    mutate(idpostedecapture = paste0(idpointdecapture,as.character(postedecapture))) %>%
    dplyr::select(idpostedecapture)



# i) get the table of mosquitoes identifications, ii) summarize to get the number of bites by postedecapture, iii) join the table csh_all to also get all the points where there were no mosquitoes bites
ma <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
  group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece,heuredecapture) %>%
  summarise(val = n()) %>%
  #right_join(csh_all) %>%
  #mutate(val = ifelse(is.na(val),0,val)) %>%
  mutate(var = "ma") %>%
  as_tibble()

# ma_anopheles <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
#   filter(genre=="Anopheles") %>%
#   group_by(idpointdecapture,postedecapture,especeanoph) %>%
#   summarise(val = n()) %>%
#   right_join(csh_all) %>%
#   mutate(val = ifelse(is.na(val),0,val)) %>%
#   mutate(var = "ma_anopheles") %>%
#   mutate(genre = "Anopheles") %>%
#   as_tibble()
# 
# ma_wout_species <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
#   group_by(idpointdecapture,postedecapture) %>%
#   summarise(val = n()) %>%
#   right_join(csh_all) %>%
#   mutate(val = ifelse(is.na(val),0,val)) %>%
#   mutate(var = "ma_wout_species") %>%
#   as_tibble()

# str(ma)
# str(ma_wout_species)

######## EIR : Entomological inoculation rate (Number of infectious bites by Anopheles per person per night)  ########
# filter data where pcr was done
# EIR_pcr_done <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
#   filter(!is.na(pcr_pf)) %>%
#   group_by(idpointdecapture,postedecapture,pcr_pf,especeanoph,pcr_espece) %>%
#   summarise(val = n()) %>%
#   as_tibble()

# # At least 1 infectious bite in the {idpointdecapture,postedecapture} (ie pcr was done and at least one infectious mosquito was found)
# EIR_infectiousbites <- EIR_pcr_done %>%
#   filter(pcr_pf==1) %>%
#   dplyr::select(idpointdecapture,postedecapture,val,especeanoph,pcr_espece)

# # 0 infectious bite in the {idpointdecapture,postedecapture} (ie pcr was done and 0 infectious mosquito was found)
# EIR_noninfectionbites <- EIR_pcr_done %>%
#   filter(pcr_pf==0) %>%
#   left_join(EIR_infectiousbites, by = c("idpointdecapture", "postedecapture","especeanoph","pcr_espece")) %>%
#   filter(is.na(val.y)) %>%
#   dplyr::select(idpointdecapture, postedecapture, val.x,especeanoph,pcr_espece) %>%
#   rename(val = val.x) %>%
#   mutate(val = 0)

# # bind the two
# eir <- rbind(EIR_infectiousbites, EIR_noninfectionbites) %>%
#   mutate(var = "eir", genre = "Anopheles") %>%
#   dplyr::select(idpointdecapture,var,postedecapture,especeanoph,pcr_espece,genre,val) %>%
#   as_tibble()

eir <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>%
   filter(pcr_pf==1) %>%
   group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece,heuredecapture) %>%
   summarise(val = n()) %>%
   mutate(var = "eir") %>%
   as_tibble()

# ######## SR : Sporozoite rate (Proportion of mosquitoes with sporozoites in their salivary glands) (anopheles only) <=> proportion of mosquitoes whose pcr_pf is positive ########
# 
# # number of mosquitoes tested negative at plasmodium PCR
# mosq_negative <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
#   filter(pcr_pf==0) %>% # keep rows for mosquitoes that have had pcr and that were tested negative to pf
#   group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece) %>%
#   summarise(mosq_negative = n()) %>%
#   as_tibble()
# 
# mosq_positive <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
#   filter(pcr_pf==1) %>% # keep rows for mosquitoes that have had pcr and that were positive to pf
#   group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece) %>%
#   summarise(mosq_positive = n()) %>%
#   as_tibble()
# 
# sr <- full_join(mosq_negative,mosq_positive) %>%
#   mutate(mosq_positive = ifelse(is.na(mosq_positive),0,mosq_positive)) %>%
#   mutate(mosq_negative = ifelse(is.na(mosq_negative),0,mosq_negative)) %>%
#   mutate(val = mosq_positive/(mosq_negative + mosq_positive)*100) %>%
#   mutate(var = "sr") %>%
#   dplyr::select(idpointdecapture,var,postedecapture,especeanoph,pcr_espece,val,genre) %>%
#   as_tibble()
#   
#  str(sr)

 ######## Physiological resistance (Proportion of mosquitoes with gene resistance) (anopheles only) ########
 ## fréquence allélique : voir def ici : http://campus.cerimes.fr/genetique-medicale/enseignement/genetique_2/site/html/cours.pdf  page 6 et application pour gène kdr ici : http://www.beep.ird.fr/collect/upb/index/assoc/IDR-2007-NAM-DIS/IDR-2007-NAM-DIS.pdf au tableau page 22
 
 # fkdrw <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
 #    filter(!is.na(kdrw)) %>% # keep rows for mosquitoes that have had pcr for kdrw identification
 #    group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece,kdrw) %>%
 #    summarise(val = n()) %>%
 #    as_tibble() %>%
 #    pivot_wider(names_from = kdrw, values_from = val, values_fill = list(val = 0)) %>%
 #    mutate(var="fkdrw") %>%
 #    mutate(val = (2*RR + RS) / (2*(RR + RS + SS))) %>%
 #    dplyr::select(-c(RR,RS,SS,IC))
 
 kdrw <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
     filter(!is.na(kdrw)) %>% # keep rows for mosquitoes that have had pcr for kdrw identification
     group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece,kdrw,heuredecapture) %>%
     summarise(val = n()) %>%
     as_tibble() %>%
     rename(var=kdrw) %>%
    filter(var!="IC") %>%
    mutate(var=case_when(var=="RS" ~ "kdrw_RS",
                         var=="RR" ~ "kdrw_RR",
                         var=="SS" ~ "kdrw_SS"))
    
 kdre <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
    filter(!is.na(kdre)) %>% # keep rows for mosquitoes that have had pcr for kdre identification
    group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece,kdre,heuredecapture) %>%
    summarise(val = n()) %>%
    as_tibble() %>%
    rename(var=kdre) %>%
    filter(var!="IC") %>%
    mutate(var=case_when(var=="RS" ~ "kdre_RS",
                         var=="RR" ~ "kdre_RR",
                         var=="SS" ~ "kdre_SS"))
 
 ace1 <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
    filter(!is.na(ace1)) %>% # keep rows for mosquitoes that have had pcr for ace1 identification
    group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece,ace1,heuredecapture) %>%
    summarise(val = n()) %>%
    as_tibble() %>%
    rename(var=ace1) %>%
    filter(var!="IC") %>%
    mutate(var=case_when(var=="RS" ~ "ace1_RS",
                         var=="RR" ~ "ace1_RR",
                         var=="SS" ~ "ace1_SS"))

########  Behaviourial resistance ########
 
 # bre <-  dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
 #    filter(as.numeric(heuredecapture) >= 13 & as.numeric(heuredecapture) <= 19) %>%
 #    group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece,heuredecapture) %>%
 #    summarise(val = n()) %>%
 #    mutate(var = "bre") %>%
 #    as_tibble()
 #    
 # brl <-  dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
 #    filter(as.numeric(heuredecapture) >= 6  & as.numeric(heuredecapture) <= 13) %>%
 #    group_by(idpointdecapture,postedecapture,genre,especeanoph,pcr_espece,heuredecapture) %>%
 #    summarise(val = n()) %>%
 #    mutate(var = "brl") %>%
 #    as_tibble()
 # 
 
######## metrics_epidemio_qpcr_stats : indicators regarding the anopheles analysed with (q)pcr ######

######## n	: (Number of anopheles caught) ########
 an_n <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
   filter(genre == "Anopheles") %>%
   group_by(idpointdecapture,postedecapture,especeanoph,pcr_espece,genre,heuredecapture) %>%
   summarise(val = n()) %>%
   mutate(var = "an_n") %>%
   as_tibble()

######## pcr_sp_n	: (Number of anopheles successfully analysed with PCR for species identification) ########
 an_pcr_sp <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
   filter(!is.na(pcr_espece), genre == "Anopheles") %>%
   group_by(idpointdecapture,postedecapture,especeanoph,pcr_espece,genre,heuredecapture) %>%
   summarise(val = n()) %>%
   mutate(var = "an_pcr_sp") %>%
   as_tibble()

# ######## pcr_sp_r	: (Proportion of anopheles successfully analysed with PCR for species identification among the total number of anopheles caught) ########
# pcr_sp_r <- n %>% 
#    left_join(pcr_sp_n, by = c("idpointdecapture", "postedecapture","especeanoph","pcr_espece","genre")) %>%
#    mutate(val.y = ifelse(is.na(val.y),0,val.y)) %>%
#    mutate(val = round(val.y / val.x * 100)) %>%
#    mutate(var = "pcr_sp_r") %>%
#    dplyr::select(idpointdecapture, postedecapture,especeanoph,pcr_espece,genre, var, val)

######## pcr_pf_n	: (Number of anopheles successfully analysed with PCR for plasmodium falciparum sporozoites presence or absence) ########
 an_pcr_pf <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
   filter(!is.na(pcr_pf), genre == "Anopheles") %>%
   group_by(idpointdecapture,postedecapture,especeanoph,pcr_espece,genre,heuredecapture) %>%
   summarise(val = n()) %>%
   mutate(var = "an_pcr_pf") %>%
   as_tibble()

# ######## pcr_pf_r	: (Proportion of anopheles analysed with PCR for plasmodium falciparum sporozoites presence or absence among the total number of anopheles caught) ########
# pcr_pf_r <- n %>% 
#    left_join(pcr_pf_n, by = c("idpointdecapture", "postedecapture","especeanoph","pcr_espece","genre")) %>%
#    mutate(val.y = ifelse(is.na(val.y),0,val.y)) %>%
#    mutate(val = round(val.y / val.x * 100)) %>%
#    mutate(var = "pcr_pf_r") %>%
#    dplyr::select(idpointdecapture, postedecapture,especeanoph,pcr_espece,genre, var, val)

######## pcr_kdrw	: (Number of anopheles successfully analysed with PCR for kdrw mutation identification) ########
 an_pcr_kdrw <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
   filter(!is.na(kdrw), genre == "Anopheles") %>%
   group_by(idpointdecapture,postedecapture,especeanoph,pcr_espece,genre,heuredecapture) %>%
   summarise(val = n()) %>%
   mutate(var = "an_pcr_kdrw") %>%
   as_tibble()

# ######## pcr_kdrw_r	: (Proportion of anopheles analysed with PCR for kdrw mutation identification among the total number of anopheles caught) ########
# pcr_kdrw_r <- n %>% 
#    left_join(pcr_kdrw_n, by = c("idpointdecapture", "postedecapture","especeanoph","pcr_espece","genre")) %>%
#    mutate(val.y = ifelse(is.na(val.y),0,val.y)) %>%
#    mutate(val = round(val.y / val.x * 100)) %>%
#    mutate(var = "pcr_kdrw_r") %>%
#    dplyr::select(idpointdecapture, postedecapture,especeanoph,pcr_espece,genre, var, val)

######## pcr_kdre_n	: (Number of anopheles successfully analysed with PCR for kdre mutation) ########
 an_pcr_kdre <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
   filter(!is.na(kdre), genre == "Anopheles") %>%
   group_by(idpointdecapture,postedecapture,especeanoph,pcr_espece,genre,heuredecapture) %>%
   summarise(val = n()) %>%
   mutate(var = "an_pcr_kdre") %>%
   as_tibble()

# ######## pcr_kdre_r	: (Proportion of anopheles analysed with PCR for kdre mutation identification among the total number of anopheles caught) ########
# pcr_kdre_r <- n %>% 
#    left_join(pcr_kdre_n, by = c("idpointdecapture", "postedecapture","especeanoph","pcr_espece","genre")) %>%
#    mutate(val.y = ifelse(is.na(val.y),0,val.y)) %>%
#    mutate(val = round(val.y / val.x * 100)) %>%
#    mutate(var = "pcr_kdre_r") %>%
#    dplyr::select(idpointdecapture, postedecapture,especeanoph,pcr_espece,genre, var, val)

######## pcr_ace1_n	: (Number of anopheles successfully analysed with PCR for ace1 mutation) ########
 an_pcr_ace1 <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
   filter(!is.na(ace1), genre == "Anopheles") %>%
   group_by(idpointdecapture,postedecapture,especeanoph,pcr_espece,genre,heuredecapture) %>%
   summarise(val = n()) %>%
   mutate(var = "an_pcr_ace1") %>%
   as_tibble()

# ######## pcr_ace1_r	: (Proportion of anopheles analysed with PCR for ace1 mutation identification among the total number of anopheles caught) ########
# pcr_ace1_r <- n %>% 
#    left_join(pcr_ace1_n, by = c("idpointdecapture", "postedecapture","especeanoph","pcr_espece","genre")) %>%
#    mutate(val.y = ifelse(is.na(val.y),0,val.y)) %>%
#    mutate(val = round(val.y / val.x * 100)) %>%
#    mutate(var = "pcr_ace1_r") %>%
#    dplyr::select(idpointdecapture, postedecapture,especeanoph,pcr_espece,genre, var, val)

######## overall entomo metrics ########

trmetrics_entomo <- rbind(ma,eir,kdrw,kdre,ace1,an_n,an_pcr_sp,an_pcr_pf,an_pcr_kdrw,an_pcr_kdre,an_pcr_ace1) %>%
    rename(complex = especeanoph, species = pcr_espece) %>%
    mutate(idpostedecapture=paste0(idpointdecapture,postedecapture)) %>%
    dplyr::select( idpointdecapture ,postedecapture ,idpostedecapture,genre   ,  complex   ,  species ,      heuredecapture  , val ,var   )
   #pivot_wider(names_from = var, values_from = val, values_fill = list(val = 0))

 
 # trmetrics_entomo2 <- trmetrics_entomo %>%
 #    mutate(genre = ifelse(genre=="Anopheles","an","oth")) %>%
 #    mutate(complex = case_when(complex=="An.funestus" ~ "funestus",
 #                               complex=="An.gambiae s.l." ~ "gambiae",
 #                               !(complex %in% c("funestus","gambiae")) ~ "oth")) %>%
 #    mutate(species = case_when(species=="An.funestus_ss" ~ "funestus_ss",
 #                               species=="An.coluzzii" ~ "coluzzii",
 #                               species=="An.gambiae_ss" ~ "gambiae_ss",
 #                               !(species %in% c("funestus_ss","coluzzii","gambiae_ss")) ~ "oth"))
 # 
 # trmetrics_entomo3 <- trmetrics_entomo2 %>%
 #    group_by(idpointdecapture)
 #    
 # 
 # 
pres_ma_an <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles") %>% 
   group_by(idpointdecapture) %>%
   summarise(ma_an=sum(val)) %>%
   mutate(pres_an=TRUE) %>%
   right_join(idpointdecaptures) %>%
   mutate(ma_an = ifelse(is.na(ma_an),0,ma_an)) %>%
   mutate(pres_an = ifelse(is.na(pres_an),FALSE,pres_an))
   
pres_ma_oth<- trmetrics_entomo %>%
   filter(var=="ma",genre!="Anopheles") %>% 
   group_by(idpointdecapture) %>%
   summarise(ma_oth=sum(val)) %>%
   mutate(pres_oth=TRUE) %>%
   right_join(idpointdecaptures) %>%
   mutate(ma_oth = ifelse(is.na(ma_oth),0,ma_oth)) %>%
   mutate(pres_oth = ifelse(is.na(pres_oth),FALSE,pres_oth))

eir_an <-  trmetrics_entomo %>%
   filter(var=="eir",genre=="Anopheles") %>% 
   group_by(idpointdecapture) %>%
   summarise(eir_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(eir_an = ifelse(is.na(eir_an),0,eir_an))

er_an <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles") %>% 
   group_by(idpointdecapture,postedecapture) %>%
   summarise(ma=sum(val)) %>%
   pivot_wider(names_from = postedecapture, values_from = ma , values_fill = list(ma = 0)) %>%
   mutate(er_an = round(e/(e+i)*100)) %>%
   #mutate(er_an = e) %>%
   dplyr::select(-c(e,i)) %>%
   right_join(idpointdecaptures) %>%
   mutate(er_an = ifelse(is.na(er_an),0,er_an))

er_oth <- trmetrics_entomo %>%
   filter(var=="ma",genre!="Anopheles") %>% 
   group_by(idpointdecapture,postedecapture) %>%
   summarise(ma=sum(val)) %>%
   pivot_wider(names_from = postedecapture, values_from = ma , values_fill = list(ma = 0)) %>%
   mutate(er_oth = round(e/(e+i)*100)) %>%
   #mutate(er_oth = e) %>%
   dplyr::select(-c(e,i)) %>%
   right_join(idpointdecaptures) %>%
   mutate(er_oth = ifelse(is.na(er_oth),0,er_oth))
   
pcr_sp_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_sp") %>% 
   group_by(idpointdecapture) %>%
   summarise(pcr_sp_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(pcr_sp_an = ifelse(is.na(pcr_sp_an),0,pcr_sp_an))

pcr_pf_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_pf") %>% 
   group_by(idpointdecapture) %>%
   summarise(pcr_pf_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(pcr_pf_an = ifelse(is.na(pcr_pf_an),0,pcr_pf_an))

pcr_kdrw_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_kdrw") %>% 
   group_by(idpointdecapture) %>%
   summarise(pcr_kdrw_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(pcr_kdrw_an = ifelse(is.na(pcr_kdrw_an),0,pcr_kdrw_an))

pcr_kdre_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_kdre") %>% 
   group_by(idpointdecapture) %>%
   summarise(pcr_kdre_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(pcr_kdre_an = ifelse(is.na(pcr_kdre_an),0,pcr_kdre_an))

pcr_ace1_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_ace1") %>% 
   group_by(idpointdecapture) %>%
   summarise(pcr_ace1_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(pcr_ace1_an = ifelse(is.na(pcr_ace1_an),0,pcr_ace1_an))



pres_ma_by_complex <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles",complex %in% c("An.funestus","An.gambiae s.l.")) %>% 
   mutate(complex = gsub("An.funestus","ma_funestus",complex)) %>%
   mutate(complex = gsub("An.gambiae s.l.","ma_gambiae",complex)) %>%
   group_by(idpointdecapture,complex) %>%
   summarise(ma=sum(val)) %>%
   pivot_wider(names_from = complex , values_from = ma, values_fill = list(ma = 0)) %>%
   right_join(idpointdecaptures) %>%
   mutate(ma_funestus = ifelse(is.na(ma_funestus),0,ma_funestus)) %>%
   mutate(ma_gambiae = ifelse(is.na(ma_gambiae),0,ma_gambiae)) %>%
   mutate(pres_funestus =  ifelse(ma_funestus==0,FALSE,TRUE)) %>%
   mutate(pres_gambiae =  ifelse(ma_gambiae==0,FALSE,TRUE))
   
pres_ma_by_species <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles",species %in% c("An.funestus_ss","An.coluzzii","An.gambiae_ss")) %>% 
   mutate(species = gsub("An.funestus_ss","ma_funestus_ss",species)) %>%
   mutate(species = gsub("An.coluzzii","ma_coluzzi",species)) %>%
   mutate(species = gsub("An.gambiae_ss","ma_gambiae_ss",species)) %>%
   group_by(idpointdecapture,species) %>%
   summarise(ma=sum(val)) %>%
   pivot_wider(names_from = species , values_from = ma, values_fill = list(ma = 0)) %>%
   right_join(idpointdecaptures) %>%
   mutate(ma_funestus_ss = ifelse(is.na(ma_funestus_ss),0,ma_funestus_ss)) %>%
   mutate(ma_coluzzi = ifelse(is.na(ma_coluzzi),0,ma_coluzzi)) %>%
   mutate(ma_gambiae_ss = ifelse(is.na(ma_gambiae_ss),0,ma_gambiae_ss)) %>%
   mutate(pres_funestus_ss =  ifelse(ma_funestus_ss==0,FALSE,TRUE)) %>%
   mutate(pres_gambiae_ss =  ifelse(ma_gambiae_ss==0,FALSE,TRUE)) %>%
   mutate(pres_coluzzi =  ifelse(ma_coluzzi==0,FALSE,TRUE))

er_by_complex <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles",complex %in% c("An.funestus","An.gambiae s.l.")) %>% 
   group_by(idpointdecapture,postedecapture,complex) %>%
   summarise(ma=sum(val)) %>%
   mutate(complex = gsub("An.funestus","er_funestus",complex)) %>%
   mutate(complex = gsub("An.gambiae s.l.","er_gambiae",complex)) %>%
   pivot_wider(names_from = c(postedecapture,complex), values_from = ma , values_fill = list(ma = 0)) %>%
   mutate(er_funestus = round(e_er_funestus/(e_er_funestus+i_er_funestus)*100)) %>%
   mutate(er_gambiae = round(e_er_gambiae/(e_er_gambiae+i_er_gambiae)*100)) %>%
   #mutate(er_funestus = e_er_funestus) %>%
   #mutate(er_gambiae = e_er_gambiae) %>%
   dplyr::select(-c(e_er_funestus,e_er_gambiae,i_er_funestus,i_er_gambiae)) %>%
   right_join(idpointdecaptures) %>%
   mutate(er_funestus = ifelse(is.na(er_funestus),0,er_funestus)) %>%
   mutate(er_gambiae = ifelse(is.na(er_gambiae),0,er_gambiae)) 

   
er_by_species <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles",species %in% c("An.funestus_ss","An.coluzzii","An.gambiae_ss")) %>% 
   group_by(idpointdecapture,postedecapture,species) %>%
   summarise(ma=sum(val)) %>%
   mutate(species = gsub("An.funestus_ss","er_funestus_ss",species)) %>%
   mutate(species = gsub("An.coluzzii","er_coluzzi",species)) %>%
   mutate(species = gsub("An.gambiae_ss","er_gambiae_ss",species)) %>%
   pivot_wider(names_from = c(postedecapture,species), values_from = ma , values_fill = list(ma = 0)) %>%
   mutate(er_funestus_ss = round(e_er_funestus_ss/(e_er_funestus_ss+i_er_funestus_ss)*100)) %>%
   mutate(er_gambiae_ss = round(e_er_gambiae_ss/(e_er_gambiae_ss+i_er_gambiae_ss)*100)) %>%
   mutate(er_coluzzi = round(e_er_coluzzi/(e_er_coluzzi+i_er_coluzzi)*100)) %>%
   #mutate(er_funestus_ss = e_er_funestus_ss) %>%
   #mutate(er_gambiae_ss = e_er_gambiae_ss) %>%
   #mutate(er_coluzzi = e_er_coluzzi) %>%
   dplyr::select(-c(e_er_coluzzi,e_er_funestus_ss,i_er_funestus_ss,i_er_coluzzi,i_er_gambiae_ss,e_er_gambiae_ss)) %>%
   right_join(idpointdecaptures) %>%
   mutate(er_funestus_ss = ifelse(is.na(er_funestus_ss),0,er_funestus_ss)) %>%
   mutate(er_gambiae_ss = ifelse(is.na(er_gambiae_ss),0,er_gambiae_ss)) %>%
   mutate(er_coluzzi = ifelse(is.na(er_coluzzi),0,er_coluzzi))

kdre_rs_an <- trmetrics_entomo %>%
   filter(var=="kdre_RS") %>%
   group_by(idpointdecapture) %>%
   summarise(kdre_rs_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(kdre_rs_an = ifelse(is.na(kdre_rs_an),0,kdre_rs_an))
kdre_rr_an <- trmetrics_entomo %>%
   filter(var=="kdre_RR") %>%
   group_by(idpointdecapture) %>%
   summarise(kdre_rr_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(kdre_rr_an = ifelse(is.na(kdre_rr_an),0,kdre_rr_an))
kdre_ss_an <- trmetrics_entomo %>%
   filter(var=="kdre_SS") %>%
   group_by(idpointdecapture) %>%
   summarise(kdre_ss_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(kdre_ss_an = ifelse(is.na(kdre_ss_an),0,kdre_ss_an))
kdrw_rs_an <- trmetrics_entomo %>%
   filter(var=="kdrw_RS") %>%
   group_by(idpointdecapture) %>%
   summarise(kdrw_rs_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(kdrw_rs_an = ifelse(is.na(kdrw_rs_an),0,kdrw_rs_an))
kdrw_rr_an <- trmetrics_entomo %>%
   filter(var=="kdrw_RR") %>%
   group_by(idpointdecapture) %>%
   summarise(kdrw_rr_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(kdrw_rr_an = ifelse(is.na(kdrw_rr_an),0,kdrw_rr_an))
kdrw_ss_an <- trmetrics_entomo %>%
   filter(var=="kdrw_SS") %>%
   group_by(idpointdecapture) %>%
   summarise(kdrw_ss_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(kdrw_ss_an = ifelse(is.na(kdrw_ss_an),0,kdrw_ss_an))
ace1_rs_an <- trmetrics_entomo %>%
   filter(var=="ace1_RS") %>%
   group_by(idpointdecapture) %>%
   summarise(ace1_rs_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(ace1_rs_an = ifelse(is.na(ace1_rs_an),0,ace1_rs_an))
ace1_rr_an <- trmetrics_entomo %>%
   filter(var=="ace1_RR") %>%
   group_by(idpointdecapture) %>%
   summarise(ace1_rr_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(ace1_rr_an = ifelse(is.na(ace1_rr_an),0,ace1_rr_an))
ace1_ss_an <- trmetrics_entomo %>%
   filter(var=="ace1_SS") %>%
   group_by(idpointdecapture) %>%
   summarise(ace1_ss_an=sum(val)) %>%
   right_join(idpointdecaptures) %>%
   mutate(ace1_ss_an = ifelse(is.na(ace1_ss_an),0,ace1_ss_an))

    


trmetrics_entomo_pointdecapture <- pres_ma_an %>%
   left_join(pres_ma_oth) %>%
   left_join(eir_an) %>%
   left_join(er_an) %>%
   left_join(er_oth) %>%
   left_join(pcr_sp_an) %>%
   left_join(pcr_pf_an) %>%
   left_join(pcr_kdrw_an) %>%
   left_join(pcr_kdre_an) %>%
   left_join(pcr_ace1_an) %>%
   left_join(pres_ma_by_complex) %>%
   left_join(pres_ma_by_species) %>%
   left_join(er_by_complex) %>%
   left_join(er_by_species) %>%
   left_join(kdre_rs_an) %>%
   left_join(kdre_rr_an) %>%
   left_join(kdre_ss_an) %>%
   left_join(kdrw_rs_an) %>%
   left_join(kdrw_rr_an) %>%
   left_join(kdrw_ss_an) %>%
   left_join(ace1_rs_an) %>%
   left_join(ace1_rr_an) %>%
   left_join(ace1_ss_an)


trmetrics_entomo_pointdecapture <- trmetrics_entomo_pointdecapture %>%
   mutate(prop_pcr_sp_an=round(pcr_sp_an/ma_an*100)) %>%
   mutate(prop_pcr_pf_an=round(pcr_pf_an/ma_an*100)) %>%
   mutate(prop_pcr_kdrw_an=round(pcr_kdrw_an/ma_an*100)) %>%
   mutate(prop_pcr_kdre_an=round(pcr_kdre_an/ma_an*100)) %>%
   mutate(prop_pcr_ace1_an=round(pcr_ace1_an/ma_an*100))
   
trmetrics_entomo_pointdecapture <- trmetrics_entomo_pointdecapture %>%
   dplyr::select(idpointdecapture,
                 pres_an,pres_oth,
                 ma_an,
                 ma_oth,
                 er_an,
                 er_oth,
                 pres_funestus,
                 pres_gambiae,
                 ma_funestus,
                 ma_gambiae,
                 er_funestus,
                 er_gambiae,
                 pcr_sp_an,
                 prop_pcr_sp_an,
                 pres_funestus_ss,
                 pres_gambiae_ss,
                 pres_coluzzi,
                 ma_funestus_ss,
                 ma_gambiae_ss,
                 ma_coluzzi,
                 er_funestus_ss,
                 er_gambiae_ss,
                 er_coluzzi,
                 pcr_pf_an,
                 prop_pcr_pf_an,
                 eir_an,
                 pcr_kdrw_an,
                 prop_pcr_kdrw_an,
                 kdrw_rs_an,
                 kdrw_rr_an,
                 kdrw_ss_an,
                 pcr_kdre_an,
                 prop_pcr_kdre_an,
                 kdre_rs_an,
                 kdre_rr_an,
                 kdre_ss_an,
                 pcr_ace1_an,
                 prop_pcr_ace1_an,
                 ace1_rs_an,
                 ace1_rr_an,
                 ace1_ss_an
   )


pres_ma_an <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles") %>% 
   group_by(idpostedecapture) %>%
   summarise(ma_an=sum(val)) %>%
   mutate(pres_an=TRUE) %>%
   right_join(idpostesdecaptures) %>%
   mutate(ma_an = ifelse(is.na(ma_an),0,ma_an)) %>%
   mutate(pres_an = ifelse(is.na(pres_an),FALSE,pres_an))

pres_ma_oth<- trmetrics_entomo %>%
   filter(var=="ma",genre!="Anopheles") %>% 
   group_by(idpostedecapture) %>%
   summarise(ma_oth=sum(val)) %>%
   mutate(pres_oth=TRUE) %>%
   right_join(idpostesdecaptures) %>%
   mutate(ma_oth = ifelse(is.na(ma_oth),0,ma_oth)) %>%
   mutate(pres_oth = ifelse(is.na(pres_oth),FALSE,pres_oth))

eir_an <-  trmetrics_entomo %>%
   filter(var=="eir",genre=="Anopheles") %>% 
   group_by(idpostedecapture) %>%
   summarise(eir_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(eir_an = ifelse(is.na(eir_an),0,eir_an))

pcr_sp_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_sp") %>% 
   group_by(idpostedecapture) %>%
   summarise(pcr_sp_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(pcr_sp_an = ifelse(is.na(pcr_sp_an),0,pcr_sp_an))

pcr_pf_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_pf") %>% 
   group_by(idpostedecapture) %>%
   summarise(pcr_pf_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(pcr_pf_an = ifelse(is.na(pcr_pf_an),0,pcr_pf_an))

pcr_kdrw_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_kdrw") %>% 
   group_by(idpostedecapture) %>%
   summarise(pcr_kdrw_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(pcr_kdrw_an = ifelse(is.na(pcr_kdrw_an),0,pcr_kdrw_an))

pcr_kdre_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_kdre") %>% 
   group_by(idpostedecapture) %>%
   summarise(pcr_kdre_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(pcr_kdre_an = ifelse(is.na(pcr_kdre_an),0,pcr_kdre_an))

pcr_ace1_an <- trmetrics_entomo %>%
   filter(var=="an_pcr_ace1") %>% 
   group_by(idpostedecapture) %>%
   summarise(pcr_ace1_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(pcr_ace1_an = ifelse(is.na(pcr_ace1_an),0,pcr_ace1_an))



pres_ma_by_complex <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles",complex %in% c("An.funestus","An.gambiae s.l.")) %>% 
   mutate(complex = gsub("An.funestus","ma_funestus",complex)) %>%
   mutate(complex = gsub("An.gambiae s.l.","ma_gambiae",complex)) %>%
   group_by(idpostedecapture,complex) %>%
   summarise(ma=sum(val)) %>%
   pivot_wider(names_from = complex , values_from = ma, values_fill = list(ma = 0)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(ma_funestus = ifelse(is.na(ma_funestus),0,ma_funestus)) %>%
   mutate(ma_gambiae = ifelse(is.na(ma_gambiae),0,ma_gambiae)) %>%
   mutate(pres_funestus =  ifelse(ma_funestus==0,FALSE,TRUE)) %>%
   mutate(pres_gambiae =  ifelse(ma_gambiae==0,FALSE,TRUE))

pres_ma_by_species <- trmetrics_entomo %>%
   filter(var=="ma",genre=="Anopheles",species %in% c("An.funestus_ss","An.coluzzii","An.gambiae_ss")) %>% 
   mutate(species = gsub("An.funestus_ss","ma_funestus_ss",species)) %>%
   mutate(species = gsub("An.coluzzii","ma_coluzzi",species)) %>%
   mutate(species = gsub("An.gambiae_ss","ma_gambiae_ss",species)) %>%
   group_by(idpostedecapture,species) %>%
   summarise(ma=sum(val)) %>%
   pivot_wider(names_from = species , values_from = ma, values_fill = list(ma = 0)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(ma_funestus_ss = ifelse(is.na(ma_funestus_ss),0,ma_funestus_ss)) %>%
   mutate(ma_coluzzi = ifelse(is.na(ma_coluzzi),0,ma_coluzzi)) %>%
   mutate(ma_gambiae_ss = ifelse(is.na(ma_gambiae_ss),0,ma_gambiae_ss)) %>%
   mutate(pres_funestus_ss =  ifelse(ma_funestus_ss==0,FALSE,TRUE)) %>%
   mutate(pres_gambiae_ss =  ifelse(ma_gambiae_ss==0,FALSE,TRUE)) %>%
   mutate(pres_coluzzi =  ifelse(ma_coluzzi==0,FALSE,TRUE))

kdre_rs_an <- trmetrics_entomo %>%
   filter(var=="kdre_RS") %>%
   group_by(idpostedecapture) %>%
   summarise(kdre_rs_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(kdre_rs_an = ifelse(is.na(kdre_rs_an),0,kdre_rs_an))
kdre_rr_an <- trmetrics_entomo %>%
   filter(var=="kdre_RR") %>%
   group_by(idpostedecapture) %>%
   summarise(kdre_rr_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(kdre_rr_an = ifelse(is.na(kdre_rr_an),0,kdre_rr_an))
kdre_ss_an <- trmetrics_entomo %>%
   filter(var=="kdre_SS") %>%
   group_by(idpostedecapture) %>%
   summarise(kdre_ss_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(kdre_ss_an = ifelse(is.na(kdre_ss_an),0,kdre_ss_an))
kdrw_rs_an <- trmetrics_entomo %>%
   filter(var=="kdrw_RS") %>%
   group_by(idpostedecapture) %>%
   summarise(kdrw_rs_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(kdrw_rs_an = ifelse(is.na(kdrw_rs_an),0,kdrw_rs_an))
kdrw_rr_an <- trmetrics_entomo %>%
   filter(var=="kdrw_RR") %>%
   group_by(idpostedecapture) %>%
   summarise(kdrw_rr_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(kdrw_rr_an = ifelse(is.na(kdrw_rr_an),0,kdrw_rr_an))
kdrw_ss_an <- trmetrics_entomo %>%
   filter(var=="kdrw_SS") %>%
   group_by(idpostedecapture) %>%
   summarise(kdrw_ss_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(kdrw_ss_an = ifelse(is.na(kdrw_ss_an),0,kdrw_ss_an))
ace1_rs_an <- trmetrics_entomo %>%
   filter(var=="ace1_RS") %>%
   group_by(idpostedecapture) %>%
   summarise(ace1_rs_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(ace1_rs_an = ifelse(is.na(ace1_rs_an),0,ace1_rs_an))
ace1_rr_an <- trmetrics_entomo %>%
   filter(var=="ace1_RR") %>%
   group_by(idpostedecapture) %>%
   summarise(ace1_rr_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(ace1_rr_an = ifelse(is.na(ace1_rr_an),0,ace1_rr_an))
ace1_ss_an <- trmetrics_entomo %>%
   filter(var=="ace1_SS") %>%
   group_by(idpostedecapture) %>%
   summarise(ace1_ss_an=sum(val)) %>%
   right_join(idpostesdecaptures) %>%
   mutate(ace1_ss_an = ifelse(is.na(ace1_ss_an),0,ace1_ss_an))




trmetrics_entomo_postedecapture <- pres_ma_an %>%
   left_join(pres_ma_oth) %>%
   left_join(eir_an) %>%
   left_join(pcr_sp_an) %>%
   left_join(pcr_pf_an) %>%
   left_join(pcr_kdrw_an) %>%
   left_join(pcr_kdre_an) %>%
   left_join(pcr_ace1_an) %>%
   left_join(pres_ma_by_complex) %>%
   left_join(pres_ma_by_species) %>%
   left_join(kdre_rs_an) %>%
   left_join(kdre_rr_an) %>%
   left_join(kdre_ss_an) %>%
   left_join(kdrw_rs_an) %>%
   left_join(kdrw_rr_an) %>%
   left_join(kdrw_ss_an) %>%
   left_join(ace1_rs_an) %>%
   left_join(ace1_rr_an) %>%
   left_join(ace1_ss_an)


trmetrics_entomo_postedecapture <- trmetrics_entomo_postedecapture %>%
   mutate(prop_pcr_sp_an=round(pcr_sp_an/ma_an*100)) %>%
   mutate(prop_pcr_pf_an=round(pcr_pf_an/ma_an*100)) %>%
   mutate(prop_pcr_kdrw_an=round(pcr_kdrw_an/ma_an*100)) %>%
   mutate(prop_pcr_kdre_an=round(pcr_kdre_an/ma_an*100)) %>%
   mutate(prop_pcr_ace1_an=round(pcr_ace1_an/ma_an*100)) %>%
   mutate(idpointdecapture = substr(idpostedecapture,1,nchar(idpostedecapture)-1))

trmetrics_entomo_postedecapture <- trmetrics_entomo_postedecapture %>%
   dplyr::select(idpostedecapture,
                 idpointdecapture,
                 pres_an,pres_oth,
                 ma_an,
                 ma_oth,
                 pres_funestus,
                 pres_gambiae,
                 ma_funestus,
                 ma_gambiae,
                 pcr_sp_an,
                 prop_pcr_sp_an,
                 pres_funestus_ss,
                 pres_gambiae_ss,
                 pres_coluzzi,
                 ma_funestus_ss,
                 ma_gambiae_ss,
                 ma_coluzzi,
                 pcr_pf_an,
                 prop_pcr_pf_an,
                 eir_an,
                 pcr_kdrw_an,
                 prop_pcr_kdrw_an,
                 kdrw_rs_an,
                 kdrw_rr_an,
                 kdrw_ss_an,
                 pcr_kdre_an,
                 prop_pcr_kdre_an,
                 kdre_rs_an,
                 kdre_rr_an,
                 kdre_ss_an,
                 pcr_ace1_an,
                 prop_pcr_ace1_an,
                 ace1_rs_an,
                 ace1_rr_an,
                 ace1_ss_an
   )






# link info on villages, intervention dates and intervention types
 recensement_villages_l1 <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>%
    filter(!is.na(intervention)) %>%
    dplyr::select("codevillage","codepays","intervention","date_debut_interv","date_fin_interv")
 
 entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>%
    dplyr::select("idpointdecapture","codevillage","date_capture","nummission")
 
 
 trmetrics_entomo <- trmetrics_entomo %>%
    left_join(entomo_csh_metadata_l1) %>%
    left_join(recensement_villages_l1) %>%
    mutate(phase_interv = ifelse(as.Date(date_capture) < as.Date(date_debut_interv),"pre-intervention","post-intervention")) %>%
    mutate(phase_interv = ifelse((is.na(phase_interv) & codepays=="BF" & as.Date(date_capture) < as.Date("2017-08-17")),"pre-intervention",phase_interv)) %>%
    mutate(phase_interv = ifelse((is.na(phase_interv) & codepays=="CI" & as.Date(date_capture) < as.Date("2017-09-01")),"pre-intervention",phase_interv)) %>%
    mutate(phase_interv = ifelse((is.na(phase_interv) & codepays=="BF" & as.Date(date_capture) > as.Date("2017-08-17")),"post-intervention",phase_interv)) %>%
    mutate(phase_interv = ifelse((is.na(phase_interv) & codepays=="CI" & as.Date(date_capture) > as.Date("2017-09-01")),"post-intervention",phase_interv))
 
 
 # --- ### --- ###--- ###--- ###--- ###--- ###
 ######## Epidemiological indicators ##########
 # --- ### --- ###--- ###--- ###--- ###--- ###
 
 ######## spn	: parasite number (Number of people who are infected with parasites)  ########
 
 epidemio_active_l1 <- dbReadTable(react_gpkg, 'epidemio_active_l1') %>% 
   mutate(case = ifelse(is.na(case),0,case)) %>%
   mutate(case2 = ifelse(is.na(case2),0,case2)) %>%
   mutate(fever = ifelse(is.na(fever),0,fever)) %>%
   mutate(fever2 = ifelse(is.na(fever2),0,fever2)) %>%
   mutate(case = ifelse(case==0,FALSE,TRUE)) %>%
   mutate(case2 = ifelse(case2==0,FALSE,TRUE)) %>%
   mutate(fever = ifelse(fever==0,FALSE,TRUE)) %>%
   mutate(fever2 = ifelse(fever2==0,FALSE,TRUE))
   
 spn_bf <- epidemio_active_l1 %>%
   filter(codepays=="BF") %>%
   group_by(idenquete,datenquete,codenquete,codevillage,codepays,age_c,case) %>%
   summarise(val = n())
 spn_ci <- epidemio_active_l1 %>%
   filter(codepays=="CI") %>%
   group_by(idenquete,datenquete,codenquete,codevillage,codepays,age_c,case2) %>%
   summarise(val = n()) %>%
   rename(case=case2)
   
 spn <- rbind(spn_bf,spn_ci) %>%
   mutate(var="spn") %>%
   as_tibble() %>%
   filter(!is.na(age_c)) %>%
    mutate(age_c = case_when(age_c=="0-5" ~ "0_5",
                             age_c=="5-10" ~ "5_10",
                             age_c==">10" ~ "sup_10"))
 
 # when case==TRUE : val indicates the number of people that were tested positive for malaria for the stratum
 # when case==FALSE : val indicates the number of people that were tested negative for malaria for the stratum
 
 ######## tot_pop : total pop in the village
 
 tot_pop <- dbReadTable(react_gpkg, 'recensement_individus_l0') %>% 
    #mutate(sexe = ifelse(sexeindividu %in% c("masculin","M"), "M", "F")) %>%
    mutate(age_c = case_when(ageindividu <= 5 ~ "0_5",
                             ageindividu > 5 &  ageindividu <= 10 ~ "5_10",
                             ageindividu > 10 & ageindividu <= 21 & codepays =="CI" ~ "sup_10",
                             ageindividu > 10 & ageindividu <= 17 & codepays =="BF" ~ "sup_10"
                             )) %>%
    group_by(codevillage,codepays,age_c) %>%
    summarise(tot_pop = n()) %>%
    filter(!is.na(age_c)) %>%
    pivot_wider(names_from = age_c, values_from = tot_pop, names_prefix = "total_" )
 
 tot_tested <- spn %>%
    group_by(idenquete ,datenquete ,codenquete ,codevillage ,codepays , age_c) %>%
    summarise(val=sum(val)) %>%
    pivot_wider(names_from = age_c, values_from = val, names_prefix = "tested_")
 
 spn2 <- spn %>%
    pivot_wider(names_from = c(age_c,case), values_from = val, values_fill = list(val = 0)) %>%
    left_join(tot_tested) %>%
    left_join(tot_pop) %>%
    mutate(prop_tested_sup_10 = tested_sup_10 / total_sup_10 * 100) %>% # proportion of people tested with age >10 among all the people with age >10 in the village
    mutate(prop_tested_0_5 = tested_0_5 / total_0_5 * 100) %>%
    mutate(prop_tested_5_10 = tested_5_10 / total_5_10 * 100) %>%
    mutate(spr_sup_10 = sup_10_TRUE / tested_sup_10 * 100) %>% # proportion of people tested with age >10 among all the people with age >10 in the village
    mutate(spr_0_5 = `0_5_TRUE` / `tested_0_5` * 100) %>%
    mutate(spr_5_10 = `5_10_TRUE` / `tested_5_10` * 100) %>%
    mutate(tot_FALSE = sup_10_FALSE + `5_10_FALSE` + `0_5_FALSE`) %>%
    mutate(tot_TRUE = sup_10_TRUE + `5_10_TRUE` + `0_5_TRUE`) %>% 
    mutate(tested_tot = tot_FALSE + tot_TRUE) %>%
    mutate(total_tot = total_0_5 + total_5_10 + total_sup_10) %>%
    mutate(prop_tested_tot = tested_tot / total_tot * 100) %>%
    mutate(spr_tot = tot_TRUE / tested_tot * 100) 
 
 ######## pfpn	: Total number of febrile malaria cases as a proportion of all febrile cases  ########
 
 a <- epidemio_active_l1 %>%
   distinct(idenquete,datenquete,codenquete,codevillage,codepays,sexe,age_c) %>%
   mutate(case=TRUE)

 b <- a %>%
   mutate(case=FALSE)
 
 ab <- rbind(a,b)
 
 pfpn_bf <- epidemio_active_l1 %>%
   filter(codepays=="BF",fever==TRUE) %>%
   mutate(fever_case = ifelse(case==TRUE, TRUE, FALSE)) %>%
   group_by(idenquete,datenquete,codenquete,codevillage,codepays,sexe,age_c,fever_case) %>%
   summarise(val = n()) %>%
   rename(case=fever_case)
 
 pfpn_ci <- epidemio_active_l1 %>%
   filter(codepays=="CI",fever2==TRUE) %>%
   mutate(fever_case = ifelse(case2==TRUE, TRUE, FALSE))  %>%
   group_by(idenquete,datenquete,codenquete,codevillage,codepays,sexe,age_c,fever_case) %>%
   summarise(val = n()) %>%
   rename(case=fever_case)
 
 pfpn <- rbind(pfpn_bf,pfpn_ci) %>%
   right_join(ab) %>%
   mutate(val=ifelse(is.na(val),0,val)) %>%
   mutate(var="pfpn") %>%
   as_tibble()
 
 # when case==TRUE : val indicates the number of people that had fever and had malaria for the stratum
 # when case==FALSE : val indicates the number of people that had fever but did not had malaria for the stratum
 
 
 ######## in	: Incidence  ########
 
 inc <- spn %>%
   dplyr::select(-c(idenquete,datenquete)) %>%
   pivot_wider(names_from = codenquete, values_from = val, values_fill = list(val = 0))
 
 
 
 
######## overall epidemio metrics ########
 
 trmetrics_epidemio <- rbind(spn,pfpn) %>%
   pivot_wider(names_from = var, values_from = val, values_fill = list(val = 0))
 
 # link info on villages, intervention dates and intervention types
 recensement_villages_l1 <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>%
   filter(!is.na(intervention)) %>%
   dplyr::select("codevillage","codepays","intervention","date_debut_interv","date_fin_interv")
 
 trmetrics_epidemio <- trmetrics_epidemio %>%
   left_join(recensement_villages_l1) %>%
    mutate(nummission = as.numeric(nummission)) %>%
   mutate(phase_interv = ifelse(as.Date(datenquete) < as.Date(date_debut_interv),"pre-intervention","post-intervention")) %>%
   mutate(phase_interv = ifelse((is.na(phase_interv) & codepays=="BF" & as.Date(datenquete) < as.Date("2017-08-17")),"pre-intervention",phase_interv)) %>%
   mutate(phase_interv = ifelse((is.na(phase_interv) & codepays=="CI" & as.Date(datenquete) < as.Date("2017-09-01")),"pre-intervention",phase_interv)) %>%
   mutate(phase_interv = ifelse((is.na(phase_interv) & codepays=="BF" & as.Date(datenquete) > as.Date("2017-08-17")),"post-intervention",phase_interv)) %>%
   mutate(phase_interv = ifelse((is.na(phase_interv) & codepays=="CI" & as.Date(datenquete) > as.Date("2017-09-01")),"post-intervention",phase_interv))
 
# --- ### --- ###--- ###--- ###--- ###--- ###
######## Human behaviour indicators ##########
# --- ### --- ###--- ###--- ###--- ###--- ###
 
 
 
 