require(dplyr)
require(stringr)
require(RSQLite)
require(lubridate)

# rapatrier la table biomol de la base d'amal
query<-"SELECT * FROM biomol"
df_biomol_db<-dbGetQuery(amal_db, query)

# rapatrier la table capturedeterm de la base react
query<-"SELECT * FROM capturedeterm"
df_capturedeterm<-dbGetQuery(amal_db, query)
query<-"SELECT * FROM capturedeterm_ci_niv1"
df_capturedeterm_ci_niv1<-dbGetQuery(amal_db, query)
# on aligne les noms de colonne
df_capturedeterm_ci_niv1$identifiant<-NA
df_capturedeterm_ci_niv1$baro_id<-NA
df_capturedeterm_ci_niv1$row_id_pk<-NULL
# on aligne les formats de date
df_capturedeterm$date<-as.character(as.Date(df_capturedeterm$date))
df_capturedeterm_ci_niv1$date<-as.character(as.Date(df_capturedeterm_ci_niv1$date,format = "%d/%m/%Y"))
colnames(df_capturedeterm)[which(colnames(df_capturedeterm)=="idmoustique_pk")]="idmoustique"
df_capturedeterm<-rbind(df_capturedeterm,df_capturedeterm_ci_niv1)
df_capturedeterm$postedecapture<-gsub("int","i",df_capturedeterm$postedecapture)
df_capturedeterm$postedecapture<-gsub("ext","e",df_capturedeterm$postedecapture)
df_capturedeterm$idpostedecapture<-paste0(df_capturedeterm$enquete,df_capturedeterm$codevillage_fk,df_capturedeterm$pointdecapture,df_capturedeterm$postedecapture)
colnames(df_capturedeterm)<-gsub("_fk","",colnames(df_capturedeterm))
colnames(df_capturedeterm)<-gsub("_pk","",colnames(df_capturedeterm))

df_capturedeterm_db<-df_capturedeterm

colnames(df_biomol_db)[which(colnames(df_biomol_db)=="idmoustique_pk_fk")]<-"idmoustique"

df_mosquitoes<-full_join(df_capturedeterm_db,df_biomol_db,by="idmoustique")

# Correction pour enquete==8 & codevillage=="KON" & date.x=="2018-04-01" -> KON devient KOG
df_mosquitoes_correct <- df_mosquitoes %>%
  filter(enquete==8 & codevillage=="KON" & date.x=="2018-04-01") %>%
  mutate(codevillage="KOG") %>%
  mutate(idmoustique=gsub("KON","KOG",codevillage)) %>%
  mutate(identifiant=gsub("KON","KOG",identifiant)) %>%
  mutate(baro_id=gsub("KON","KOG",baro_id)) %>%
  mutate(idpostedecapture=gsub("KON","KOG",idpostedecapture))

df_mosquitoes <- df_mosquitoes %>%
  filter(!(enquete==8 & codevillage=="KON" & date.x=="2018-04-01")) %>%
  rbind(df_mosquitoes_correct)

# rapatrier la village pour lier les pays
query<-"SELECT * FROM recensement_villages_l1"
df_countries<-dbGetQuery(react_gpkg, query) %>%
  dplyr::select(codevillage,codepays)

df_mosquitoes <- df_mosquitoes %>%
  left_join(df_countries)

# On ajoute les colonnes manquantes et supprime les colonnes inutiles
df_mosquitoes <- df_mosquitoes %>%
  mutate(idpointdecapture=substr(idpostedecapture,1,5)) %>%
  mutate(pointdecapture=substr(idpostedecapture,5,5)) %>%
  dplyr::select(idmoustique,idpointdecapture,enquete,codevillage,codepays,pointdecapture,postedecapture,idpostedecapture,heuredecapture,genre,especeanoph,etatabdomen,parturite.x,pcr_espece,pcr_pf,pcr_rs,kdrw,kdre,ace1) %>%
  rename(parturite=parturite.x) %>%
  rename(nummission=enquete)

# Les noms de village sont OK
#en post intervention : NAV devient NAM
#en post intervention : KOL devient BLA
#tout le temps : KOU (CI) -> KON
#tout le temps : NAV (CI) -> NAA

# on fait les modifs pour aligner les référentiels
# genre
df_mosquitoes$genre <- df_mosquitoes$genre %>% str_replace_all(c("Culex"="Culex_sp",
                                                               "Mansonia"="Mansonia_sp",
                                                               "Aedes"="Aedes_sp",
                                                               "Phlebotomus"="Phlebotomus_sp"))
# especeanoph
# sort(unique(df_mosquitoes$especeanoph))
df_mosquitoes$especeanoph <- df_mosquitoes$especeanoph %>% str_replace_all(c("Culex_sp"="Culex_sp",
                                                                           "Mansonia_sp"="Mansonia_sp",
                                                                           "Aedes_sp"="Aedes_sp",
                                                                           "An.funestus"="An.funestus",
                                                                           "An.coustani"="An.coustani",
                                                                           "An.pharoensis"="An.pharoensis",
                                                                           "An.nili"="An.nili",
                                                                           "UNIFORMIS"="Mans.uniformis",
                                                                           "AFRICANA"="Mans.africana",
                                                                           "AEGYPTI"="Ae.aegypti",
                                                                           "AFRICANUS"="Ae.africanus",
                                                                           "FOWLERI"="Ae.fowleri",
                                                                           "FURCIFER"="Ae.furcifer",
                                                                           "CINEREUS"="Cx.cinereus",
                                                                           "VITTATUS"="Ae.vittatus",
                                                                           "SP"="sp",
                                                                           "gambiaeS.1."="An.gambiae s.l.",
                                                                           "An.gambiae s.l."="An.gambiae s.l.",
                                                                           "Ae.aegypti FORMOSUS"="Ae.aegypti",
                                                                           "DECENS"="Cx.decens",
                                                                           "QUINQUEFASCIATUS"="Cx.quinq",
                                                                           "UNIAe.vittatus"="UNIVITTATUS"))

df_mosquitoes$especeanoph <-gsub("funestus","An.funestus",df_mosquitoes$especeanoph,fixed = TRUE)
df_mosquitoes$especeanoph <-gsub("coustani","An.coustani",df_mosquitoes$especeanoph,fixed = TRUE)
df_mosquitoes$especeanoph <-gsub("flavicosta","An.flavicosta",df_mosquitoes$especeanoph,fixed = TRUE)
df_mosquitoes$especeanoph <-gsub("nili","An.nili",df_mosquitoes$especeanoph,fixed = TRUE)
df_mosquitoes$especeanoph <-gsub("pharoensis","An.pharoensis",df_mosquitoes$especeanoph,fixed = TRUE)
df_mosquitoes$especeanoph <-gsub("rufipes","An.rufipes",df_mosquitoes$especeanoph,fixed = TRUE)
df_mosquitoes$especeanoph <-gsub("squamosus","An.squamosus",df_mosquitoes$especeanoph,fixed = TRUE)
df_mosquitoes$especeanoph <-gsub("An.An.","An.",df_mosquitoes$especeanoph,fixed = TRUE)

df_mosquitoes$pcr_espece <-gsub("An","An.",df_mosquitoes$pcr_espece,fixed = TRUE)

df_mosquitoes$especeanoph[which(df_mosquitoes$especeanoph %in% c("","p"))]=NA
# etatabdomen
df_mosquitoes$etatabdomen[which(df_mosquitoes$etatabdomen %in% c("","Choisir"))]=NA
# parturite
df_mosquitoes$parturite <- df_mosquitoes$parturite %>% str_replace_all(c("p"="P"))
df_mosquitoes$parturite[which(df_mosquitoes$parturite %in% c("","nonfait"))]=NA
# pcr_espece
df_mosquitoes$pcr_espece[which(df_mosquitoes$pcr_espece %in% c("","na","Neant","Negatif","perdu","Perdu","Anleesoni","Anruvilorum","Anruvilorum_like"))]=NA
# pcr_pf  (plasmodium)
# rien à faire
# kdrw
df_mosquitoes$kdrw <- df_mosquitoes$kdrw %>% str_replace_all(c("Rs"="RS"))
df_mosquitoes$kdrw[which(df_mosquitoes$kdrw %in% c("","na","Neant","Negatif","perdu","Perdu"))]=NA
# kdre
df_mosquitoes$kdre[which(df_mosquitoes$kdre %in% c(""))]=NA
# ace1
df_mosquitoes$ace1 <- df_mosquitoes$ace1 %>% str_replace_all(c("Rs"="RS"))
df_mosquitoes$ace1[which(df_mosquitoes$ace1 %in% c("","na","Neant","Negatif","perdu","Perdu"))]=NA
# pcr_rs  (repas de sang)
df_mosquitoes$pcr_rs <- df_mosquitoes$pcr_rs %>% str_replace_all(c("BŒUF"="bœuf",
                                                                 "Homme"="homme",
                                                                 "HOMME"="homme"))
df_mosquitoes$pcr_rs[which(df_mosquitoes$pcr_rs %in% c(""))]=NA

# heuredecapture
df_mosquitoes$heuredecapture<-as.numeric(df_mosquitoes$heuredecapture)


## data from Pooda (bras complémentaire IVM)
data_pooda <- read_excel("data/react_db/miscellaneous_data/CSH_data_final_pooda.xlsx")

data_pooda <- data_pooda %>%
  rename(idmoustique = Idmoustique_bis)
colnames(data_pooda) = tolower(colnames(data_pooda))
data_pooda <- data_pooda %>%
  rename(especeanoph = espèceanophèle) %>%
  dplyr::select(-c(datedétermination,codetechnicien,nummoustique,idlot,datecréation,odre,traitement))

data_pooda$idpointdecapture <- paste0(data_pooda$nummission,data_pooda$idpointdecapture)
data_pooda$idpostedecapture <- paste0(data_pooda$nummission,data_pooda$idpostedecapture)
data_pooda$heuredecapture <- gsub("Choisir",NA,data_pooda$heuredecapture)
data_pooda[data_pooda=="NA"]<-NA

data_pooda[data_pooda=="NA"]<-NA
data_pooda$kdre[which(data_pooda$kdre=="R")]="RR"
data_pooda$kdrw[which(data_pooda$kdrw=="R")]="RR"
data_pooda$pcr_pf[which(data_pooda$pcr_pf=="N")]=NA
data_pooda$postedecapture[which(data_pooda$postedecapture=="ext")]="e"
data_pooda$postedecapture[which(data_pooda$postedecapture=="int")]="i"
data_pooda$parturite[which(data_pooda$parturite=="p")]="P"
data_pooda$parturite[which(data_pooda$parturite=="ND")]=NA
data_pooda$etatabdomen[which(data_pooda$etatabdomen=="Semi-gravide")]="SemiGravide"
data_pooda$etatabdomen[which(data_pooda$etatabdomen=="Choisir")]=NA
data_pooda$etatabdomen[which(data_pooda$etatabdomen=="ajeun")]="Ajeun"
data_pooda$etatabdomen[which(data_pooda$etatabdomen=="AJeun")]="Ajeun"
data_pooda$especeanoph[which(data_pooda$especeanoph=="gambiaeS.1")]="An.gambiae s.l."
data_pooda$especeanoph[which(data_pooda$especeanoph=="funestus")]="An.funestus"
data_pooda$especeanoph[which(data_pooda$especeanoph=="Choisir")]=NA
data_pooda$especeanoph[which(data_pooda$especeanoph=="coustani")]="An.coustani"
data_pooda$especeanoph[which(data_pooda$especeanoph=="nili")]="An.nili"
data_pooda$especeanoph[which(data_pooda$especeanoph=="flavicosta")]="An.flavicosta"
data_pooda$pcr_espece[which(data_pooda$pcr_espece=="Angambiae_ss")]="An.gambiae_ss"
data_pooda$pcr_espece[which(data_pooda$pcr_espece=="Anarabiensis")]="An.arabiensis"
data_pooda$pcr_espece[which(data_pooda$pcr_espece=="Afunestus")]="An.funestus_ss"
data_pooda$pcr_espece[which(data_pooda$pcr_espece=="Ancoluzzii")]="An.coluzzii"
data_pooda$pcr_espece[which(data_pooda$pcr_espece=="Anrivulorum")]="An.ruvilorum"
data_pooda$pcr_espece[which(data_pooda$pcr_espece=="Anrivulorum_Like")]="An.ruvilorum_like"
data_pooda$pcr_espece[which(data_pooda$pcr_espece=="Aleesoni")]="An.leesoni"
data_pooda$heuredecapture<-as.numeric(data_pooda$heuredecapture)


df_mosquitoes <- rbind(df_mosquitoes,data_pooda)
df_mosquitoes$especeanoph[which(df_mosquitoes$especeanoph=="gambiaeS.1")]="An.gambiae s.l."

df_mosquitoes$pcr_pf <- as.numeric(df_mosquitoes$pcr_pf)

df_mosquitoes <- df_mosquitoes %>%
  mutate(pcr_espece = ifelse(especeanoph=="An.funestus","An.funestus_ss",pcr_espece))

## import ent_hlcmetadata to check if all villages / points de captures are ok
#ent_hlcmetadata<-st_read(path_to_gpkg_database,"ent_hlcmetadata") %>% as_tibble
#df_mosquitoes_check<-df_mosquitoes %>% mutate(codevillage=substr(idpointdecapture,2,4)) %>% mutate(nummission=as.numeric(substr(idpointdecapture,1,1)))

