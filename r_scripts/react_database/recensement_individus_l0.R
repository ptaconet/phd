#require(RSQLite)
#require(dplyr)
#amal_db <- dbConnect(RSQLite::SQLite(),path_to_amal_database)

query<-"SELECT * FROM individu"
individus<-dbGetQuery(amal_db, query)
# individus$codeindividu_pk[which(individus$codemenage_fk %in% codemenage_menages_nam)]<-gsub("NAA","NAM",individus$codeindividu_pk[which(individus$codemenage_fk %in% codemenage_menages_nam)])
# individus$codemenage_fk[which(individus$codemenage_fk %in% codemenage_menages_nam)]<-gsub("NAA","NAM",individus$codemenage_fk[which(individus$codemenage_fk %in% codemenage_menages_nam)])
# individus$codeindividu_pk[which(individus$codemenage_fk %in% codemenage_menages_bla)]<-gsub("KOL","BLA",individus$codeindividu_pk[which(individus$codemenage_fk %in% codemenage_menages_bla)])
# individus$codemenage_fk[which(individus$codemenage_fk %in% codemenage_menages_bla)]<-gsub("KOL","BLA",individus$codemenage_fk[which(individus$codemenage_fk %in% codemenage_menages_bla)])

individus$codevillage <- substr(individus$codemenage,1,3)
colnames(individus)<-gsub("_fk","",colnames(individus))
colnames(individus)<-gsub("_pk","",colnames(individus))
individus <- individus %>% dplyr::select(repondant,codevillage,codemenage,codeindividu,nomindividu,prenomindividu,sexeindividu,ageindividu,niveducation,profession,autreprofession,lienchefm,autrelienchefm,dormirssmoust,piecetmoust)




query<-"SELECT * FROM village"
villages<-dbGetQuery(amal_db, query)
colnames(villages)<-gsub("_fk","",colnames(villages))
colnames(villages)<-gsub("_pk","",colnames(villages))

ind_nam_bla <- read.csv("data/react_db/miscellaneous_data/IndividuCI_niv1_NAM_BLA.txt",sep="\t") %>%
  mutate(codevillage = substr(codemenage,1,3))

# CI :
individus_CI <- individus %>%
  mutate(autreniveducation=NA) %>%
  bind_rows(ind_nam_bla) %>%
  left_join(villages) %>%
  filter(codepays=="CI") %>%
  dplyr::select(repondant,codevillage,codemenage,codeindividu,nomindividu,prenomindividu,sexeindividu,ageindividu,niveducation,autreniveducation,profession,autreprofession,lienchefm,autrelienchefm,dormirssmoust,piecetmoust) %>%
  mutate(codepays="CI")

# BF : 
individus_BF <- read_excel("data/react_db/miscellaneous_data/IndividusBF_ok.xlsx") %>%
  dplyr::select(repondant,codemenage,codeindividu,nomindividu,prenomindividu,sexeindividu,ageindividu,niveducation,profession,autreprofession,lienchefm,autrelienchefm) %>%
  mutate(codevillage=substr(codemenage,1,3)) %>%
  mutate(dormirssmoust=NA) %>%
  mutate(piecetmoust=NA) %>%
  mutate(autreniveducation=NA) %>%
  mutate(codepays="BF") %>%
  mutate(codemenage=gsub("KOL","KLK",codemenage)) %>%
  mutate(codeindividu=gsub("KOL","KLK",codeindividu)) %>%
  mutate(codevillage=gsub("KOL","KLK",codevillage))
  

individus <- rbind(individus_CI,individus_BF)

  