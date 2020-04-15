
menages <- st_read(react_gpkg,"recensement_menages_l0")

menages_pays <- unique(menages[c("codevillage", "codepays")]) %>% st_drop_geometry() %>% unique()

villages_from_menage <- menages %>%
  group_by(codevillage) %>%
  summarise(X=median(X),Y=median(Y),population=sum(nbrehabitant))

# intervention et nom du village
query<-"SELECT * FROM village"
villages<-dbGetQuery(amal_db, query)
villages$nomvillage[which(villages$codevillage_pk=="BLA")]<-"Blawara"
villages$nomvillage[which(villages$codevillage_pk=="NAM")]<-"Namasselikaha"
colnames(villages)<-gsub("_fk","",colnames(villages))
colnames(villages)<-gsub("_pk","",colnames(villages))
village_interv<-read.csv("data/react_db/miscellaneous_data/villages.csv",sep=";")
village_interv<-village_interv[,c("country","codevillage","Interv")]
colnames(village_interv)<-c("country","codevillage","intervention")
village_interv$codevillage<-as.character(village_interv$codevillage)
village_interv$codevillage[which(village_interv$codevillage=="KOU" & village_interv$country=="CIV")]<-"KON"
village_interv$codevillage[which(village_interv$codevillage=="NAV" & village_interv$country=="CIV")]="NAA"
village_interv$country<-NULL
villages<-left_join(villages,village_interv)
villages$intervention[which(villages$codevillage %in% c("NAA","KOL","NON","NOK"))]<-"Ctrle"
villages <- villages %>% dplyr::select(nomvillage,codevillage,csps_csu,intervention) %>% filter(nomvillage!="nouveau_village") %>% filter(codevillage!="NAN")


villages <- villages_from_menage %>%
  full_join(villages) %>%
  full_join(menages_pays) %>%
  dplyr::select(codevillage,codepays,nomvillage,population,intervention,csps_csu,X,Y) %>%
  st_drop_geometry()


villages_pooda <- read_excel("data/react_db/miscellaneous_data/villages_REACT.xlsx") %>%
  mutate(Code_village=gsub("KOL","KLK",Code_village)) %>%
  rename(nomvillage = Nom, codevillage = Code_village)

villages <- villages %>%
  left_join(villages_pooda, by = "codevillage") %>%
  mutate(nomvillage.x = ifelse(is.na(nomvillage.x),nomvillage.y,nomvillage.x)) %>%
  rename(nomvillage = nomvillage.x) %>%
  dplyr::select(-c(nomvillage.y,Departement,Coord_X,Coord_Y,Latitude,longitude,Population))

## dates des interventions
#CIV :
#distribution des MILDA : 26 mai au 6 juin 2017
#IEC : 1 septembre 2017 au 31 aout 2018
#PID : 4 au 7 octobre 2017
#Larvicides :
#tous les lundis PEN et LOK du 20 novembre 2017 au 9 avril 2018;
#tous les mardis LAT et KAT du 21 novembre 2017 au 10 avril 2018;
#tous les mercerdis NOW et PES du 22 novembre 2017 au 11 avril 2018;
#tous les jeudis NAL du 16 novembre 2017 au 12 avril 2018;
#tous les vendredis NAM du 17 novembre 2017 au 13 avril 2018.

#BF :
#IVM : /home/ptaconet/react/datasets/miscellaneous_data/Traitement_IVM_Bougouriba_2017.xlsx


villages <- villages %>%
  mutate(date_debut_interv=NA) %>%
  mutate(date_fin_interv=NA) %>%
  mutate(date_debut_interv=case_when(codevillage %in% c("PEN","LOK") ~ "2017-11-20",
                                     codevillage %in% c("LAT","KAT") ~ "2017-11-21",
                                     codevillage %in% c("NOW","PES") ~ "2017-11-22",
                                     codevillage %in% c("NAL") ~ "2017-11-16",
                                     codevillage %in% c("NAM") ~ "2017-11-17",
                                     codepays=="CI" & intervention=="IEC" ~ "2017-09-01",
                                     codepays=="CI" & intervention=="IRS" ~ "2017-10-04",
                                     codepays=="BF" & intervention=="IRS" ~ "2017-09-19",
                                     codepays=="BF" & intervention=="IEC" ~ "2017-10-01",
                                     codevillage %in% c("DIA") ~ "2017-08-19",
                                     codevillage %in% c("KOU") ~ "2017-08-17",
                                     codevillage %in% c("KPE") ~ "2017-08-18",
                                     codevillage %in% c("LOB") ~ "2017-08-18",
                                     codevillage %in% c("NBR") ~ "2017-08-17",
                                     codevillage %in% c("SID") ~ "2017-08-18",
                                     codevillage %in% c("SKI") ~ "2017-08-18",
                                     codevillage %in% c("YBE") ~ "2017-08-19"
  )) %>%
  mutate(date_fin_interv=case_when(codevillage %in% c("PEN","LOK") ~ "2018-04-09",
                                     codevillage %in% c("LAT","KAT") ~ "2018-04-10",
                                     codevillage %in% c("NOW","PES") ~ "2018-04-11",
                                     codevillage %in% c("NAL") ~ "2018-04-12",
                                     codevillage %in% c("NAM") ~ "2018-04-13",
                                     codepays=="CI" & intervention=="IEC" ~ "2018-08-31",
                                     codepays=="CI" & intervention=="IRS" ~ "2017-10-07",
                                     codepays=="BF" & intervention=="IRS" ~ "2017-09-30",
                                     codepays=="BF" & intervention=="IEC" ~ "2018-08-01",
                                     codevillage %in% c("DIA") ~ "2017-11-28",
                                     codevillage %in% c("KOU") ~ "2017-11-29",
                                     codevillage %in% c("KPE") ~ "2017-11-29",
                                     codevillage %in% c("LOB") ~ "2017-11-27",
                                     codevillage %in% c("NBR") ~ "2017-11-28",
                                     codevillage %in% c("SID") ~ "2017-11-27",
                                     codevillage %in% c("SKI") ~ "2017-11-26",
                                     codevillage %in% c("YBE") ~ "2017-11-26"
  ))
