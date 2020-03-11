interv_ivm_traites_l0<-read_excel("data/react_db/miscellaneous_data/Traitement_IVM_Bougouriba_2017.xlsx",sheet = "Traitements_IVM_Total")

colnames(interv_ivm_traites_l0)<-c("nomvillage","rang","date","bovins","ovins","caprins")
interv_ivm_traites_l0$nomvillage <- interv_ivm_traites_l0$nomvillage %>% str_replace_all(c("Kpedia"="Kpédia","Yelbellela"="Yelbéléla","Diagnon"="Diagno"))

villages<-st_read(path_to_gpkg_database,"recensement_villages_l1") %>% dplyr::select(codevillage,codepays,nomvillage) %>% st_drop_geometry()

interv_ivm_traites_l0 <- interv_ivm_traites_l0 %>%
  arrange(date) %>%
  left_join(villages) %>%
  dplyr::select(codevillage,rang,date,bovins,ovins,caprins) %>%
  mutate(date=as.character(date))

interv_ivm_recenses_l0 <- read_excel("data/react_db/miscellaneous_data/Traitement_IVM_Bougouriba_2017.xlsx",sheet = "Efectifs_recensés")
colnames(interv_ivm_recenses_l0)<-c("nomvillage","bovins","ovins","caprins")
interv_ivm_recenses_l0$nomvillage <- interv_ivm_recenses_l0$nomvillage %>% str_replace_all(c("Kpedia"="Kpédia","Yelbellela"="Yelbéléla","Diagnon"="Diagno"))

interv_ivm_recenses_l0 <- interv_ivm_recenses_l0 %>%
  left_join(villages) %>%
  dplyr::select(codevillage,bovins,ovins,caprins) %>%
  filter(!is.na(codevillage))

