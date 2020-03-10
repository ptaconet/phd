library(readxl)
library(tidyverse)
library(lubridate)
library(sf)

## BF
path_to_humanbehavior_excel<-"miscellaneous_data/DonnéesREACT/BF/entomo/comportement-humain/All_Data_HumanBehavior_BF.xlsx"

hbm_sheets<-c("HBm1","HBm2","HBm3")
hbmbehavior_sheets<-c("HBm1Behavior","HBm2Behavior","HBm3Behavior")


hbm_df<-NULL
hbmbehavior_df<-NULL
for (i in 1:length(hbm_sheets)){

  hbm_df_th_hbm<-read_excel(path_to_humanbehavior_excel,sheet=hbm_sheets[i])
  hbmbehavior_df_th_hbmbehavior<-read_excel(path_to_humanbehavior_excel,sheet=hbmbehavior_sheets[i])

  if(i < 3){
    hbm_df_th_hbm$metadatemarkedascomplete<-ymd_hms(hbm_df_th_hbm$metadatemarkedascomplete)
    hbmbehavior_df_th_hbmbehavior$rcpdateenquete<-ymd(hbmbehavior_df_th_hbmbehavior$rcpdateenquete)
    hbmbehavior_df_th_hbmbehavior$hintmaison<-substr(as.character(hbmbehavior_df_th_hbmbehavior$hintmaison),12,20)
    hbmbehavior_df_th_hbmbehavior$hcoucher<-substr(as.character(hbmbehavior_df_th_hbmbehavior$hcoucher),12,20)
    hbmbehavior_df_th_hbmbehavior$hlever<-substr(as.character(hbmbehavior_df_th_hbmbehavior$hlever),12,20)
    hbmbehavior_df_th_hbmbehavior$hsortiemaison<-substr(as.character(hbmbehavior_df_th_hbmbehavior$hsortiemaison),12,20)
  } else if(i==3){
    hbm_df_th_hbm$metadatemarkedascomplete<-mdy_hms(hbm_df_th_hbm$metadatemarkedascomplete)
    hbm_df_th_hbm$dateenquete<-mdy(hbm_df_th_hbm$dateenquete)
    hbm_df_th_hbm$metasubmissiondate<-mdy_hms(hbm_df_th_hbm$metasubmissiondate)
    hbmbehavior_df_th_hbmbehavior$rcpdateenquete<-ymd(hbmbehavior_df_th_hbmbehavior$rcpdateenquete)
  }

  hbm_df<-rbind(hbm_df,hbm_df_th_hbm)
  hbmbehavior_df<-rbind(hbmbehavior_df,hbmbehavior_df_th_hbmbehavior)

}

hbm_df$dateenquete<-as.character(hbm_df$dateenquete)
hbmbehavior_df$rcpdateenquete<-as.character(hbmbehavior_df$rcpdateenquete)

df_humanbehavior_bf<-left_join(hbmbehavior_df,hbm_df,by = c("rcpcodemenage" = "codemenage","rcpdateenquete"="dateenquete")) %>%
  mutate(codepays="BF") %>%
    dplyr::select(rcpdateenquete,codevillage,codepays,numordremenage,rcpcodemenage,infosmenagecoordgpsLatitude,infosmenagecoordgpsLongitude,nomprenomrepdt,nbrenfant1,nbrenfant2,nbradulte,sexe,age,rcptrcheage,hintmaison,hcoucher,dormirssmoust,hlever,hsortiemaison) %>%
  rename("dateenquete"="rcpdateenquete","codemenage"="rcpcodemenage","latitude"="infosmenagecoordgpsLatitude","longitude"="infosmenagecoordgpsLongitude") %>%
  unique()

df_humanbehavior_bf <- df_humanbehavior_bf %>%
  mutate(dateenquete=replace(dateenquete, dateenquete=="2013-09-23", "2017-09-23")) %>%
  mutate(dateenquete=as.Date(dateenquete)) %>%
  mutate(periode="preinterv") %>%
  mutate(periode=replace(periode, dateenquete>"2018-01-01", "postinterv")) %>%
  mutate(saison="seche") %>%
  mutate(saison=replace(saison, dateenquete>"2017-09-01" & dateenquete<"2017-10-01", "pluies"))


## Check des horaires

#df_humanbehavior_bf$hintmaison_datetime<-ymd_hms(paste(as.Date(df_humanbehavior_bf$dateenquete)-1,df_humanbehavior_bf$hintmaison))
#df_humanbehavior_bf$hcoucher_datetime<-ymd_hms(paste(as.Date(df_humanbehavior_bf$dateenquete)-1,df_humanbehavior_bf$hcoucher))
#df_humanbehavior_bf$hlever_datetime<-ymd_hms(paste(as.Date(df_humanbehavior_bf$dateenquete),df_humanbehavior_bf$hlever))
#df_humanbehavior_bf$hsortiemaison_datetime<-ymd_hms(paste(as.Date(df_humanbehavior_bf$dateenquete),df_humanbehavior_bf$hsortiemaison))

#df_humanbehavior_bf <- df_humanbehavior_bf %>%
#  mutate(diff_time_soir=difftime(hcoucher_datetime,hintmaison_datetime,units="mins")) %>%
#  mutate(diff_time_matin=difftime(hsortiemaison_datetime,hlever_datetime,units="mins")) %>%
#  mutate(diff_time_soir=replace(diff_time_soir, diff_time_soir< -1000, NA))


### CIV
path_to_humanbehavior_excel<-"/home/ptaconet/react/datasets/miscellaneous_data/DonnéesREACT/CI/entomo/comportement-humain/new/All_Data_HumanBehavior_orig.xlsx"

hbm_sheets<-c("HBm1","HBm2")
hbmbehavior_sheets<-c("HBm1Behavior","HBm2Behavior")


hbm_df<-NULL
hbmbehavior_df<-NULL
for (i in 1:length(hbm_sheets)){

  hbm_df_th_hbm<-read_excel(path_to_humanbehavior_excel,sheet=hbm_sheets[i])
  hbmbehavior_df_th_hbmbehavior<-read_excel(path_to_humanbehavior_excel,sheet=hbmbehavior_sheets[i])

  hbm_df<-rbind(hbm_df,hbm_df_th_hbm)
  hbmbehavior_df<-rbind(hbmbehavior_df,hbmbehavior_df_th_hbmbehavior)

}

hbm_df$dateenquete<-as.character(mdy(hbm_df$dateenquete))

hbm_df <- hbm_df %>%
  mutate(codemenage=replace(codemenage, codemenage=="MOR016" & dateenquete=="2017-10-17" & nomprenomrepdt=="YEO_FA", "MOR099")) %>%
 mutate(codemenage=replace(codemenage, codemenage=="PEN012" & dateenquete=="2017-10-10" & nomprenomrepdt=="Silue dagafolo", "PEN099"))%>%
mutate(codemenage=replace(codemenage, codemenage=="BAP010" & dateenquete=="2018-03-22" & nomprenomrepdt=="YEO_KOLOTCHOLOMA", "BAP099"))%>%
mutate(codemenage=replace(codemenage, codemenage=="YAY021" & dateenquete=="2017-10-19" & nomprenomrepdt=="Sidibe Assana", "YAY099"))%>%
mutate(codemenage=replace(codemenage, codemenage=="PES004" & dateenquete=="2017-10-13" & nomprenomrepdt=="Silue Souleymane", "PES099"))%>%
mutate(codemenage=replace(codemenage, codemenage=="LAT009" & dateenquete=="2018-03-13" & nomprenomrepdt=="YEO_KAKI", "LAT099"))%>%
mutate(codemenage=replace(codemenage, codemenage=="FEL015" & dateenquete=="2017-10-17" & nomprenomrepdt=="Yeo doro", "FEL099"))

hbmbehavior_df <- hbmbehavior_df %>%
  mutate(rcpcodemenage=replace(rcpcodemenage, rcpcodemenage=="MOR016" & rcpdateenquete=="2017-10-17" & rcpnomprenomrepdt=="YEO_FA", "MOR099")) %>%
   mutate(rcpcodemenage=replace(rcpcodemenage, rcpcodemenage=="PEN012" & rcpdateenquete=="2017-10-10" & rcpnomprenomrepdt=="Silue dagafolo", "PEN099"))%>%
mutate(rcpcodemenage=replace(rcpcodemenage, rcpcodemenage=="BAP010" & rcpdateenquete=="2018-03-22" & rcpnomprenomrepdt=="YEO_KOLOTCHOLOMA", "BAP099"))%>%
mutate(rcpcodemenage=replace(rcpcodemenage, rcpcodemenage=="YAY021" & rcpdateenquete=="2017-10-19" & rcpnomprenomrepdt=="Sidibe Assana", "YAY099"))%>%
mutate(rcpcodemenage=replace(rcpcodemenage, rcpcodemenage=="PES004" & rcpdateenquete=="2017-10-13" & rcpnomprenomrepdt=="Silue Souleymane", "PES099"))%>%
mutate(rcpcodemenage=replace(rcpcodemenage, rcpcodemenage=="LAT009" & rcpdateenquete=="2018-03-13" & rcpnomprenomrepdt=="YEO_KAKI", "LAT099"))%>%
mutate(rcpcodemenage=replace(rcpcodemenage, rcpcodemenage=="FEL015" & rcpdateenquete=="2017-10-17" & rcpnomprenomrepdt=="Yeo doro", "FEL099"))

df_humanbehavior_ci<-left_join(hbmbehavior_df,hbm_df,by = c("rcpcodemenage" = "codemenage","rcpdateenquete"="dateenquete")) %>%
  mutate(codepays="CI") %>%
  dplyr::select(rcpdateenquete,codevillage,codepays,numordremenage,rcpcodemenage,infosmenagecoordgpsLatitude,infosmenagecoordgpsLongitude,nomprenomrepdt,nbrenfant1,nbrenfant2,nbradulte,sexe,age,rcptrcheage,hintmaison,hcoucher,dormirssmoust,hlever,hsortiemaison) %>%
  rename("dateenquete"="rcpdateenquete","codemenage"="rcpcodemenage","latitude"="infosmenagecoordgpsLatitude","longitude"="infosmenagecoordgpsLongitude") %>%
  unique()

df_humanbehavior_ci <- df_humanbehavior_ci %>%
  mutate(dateenquete=as.Date(dateenquete)) %>%
  mutate(periode="preinterv") %>%
  mutate(periode=replace(periode, dateenquete>"2017-09-30", "postinterv")) %>%
  mutate(saison="seche") %>%
  mutate(saison=replace(saison, dateenquete<"2018-01-01", "pluies"))


df_humanbehavior<-rbind(df_humanbehavior_bf,df_humanbehavior_ci) %>%
  arrange(codepays,dateenquete,codemenage) %>%
  mutate(dateenquete=as.character(dateenquete))

df_humanbehavior<-df_humanbehavior %>%
      mutate(codevillage=replace(codevillage, codepays=="CI" & codevillage=="NAV", "NAA")) %>%
  mutate(codevillage=replace(codevillage, codepays=="CI" & codevillage=="KOU", "KON")) %>%
  mutate(codevillage=replace(codevillage, codepays=="CI" & codevillage=="NAA" & latitude<9, "NAM")) %>%
  mutate(codevillage=replace(codevillage, codepays=="CI" & codevillage=="KOL" & latitude<9, "BLA")) %>%

  mutate(codevillage=replace(codevillage, codepays=="CI" & codevillage=="NAN", "NAK")) %>%
  mutate(codevillage=replace(codevillage, codemenage %in% c("KOU004","KOU012","KOU015") & dateenquete=="2018-02-10", "LOB")) %>%

  mutate(codemenage=paste0(codevillage,sprintf("%03d",as.numeric(numordremenage))))


## import villages to check if no error in villages names

#villages_humanbehavior<-df_humanbehavior %>% distinct(codevillage,codepays) %>% mutate(humanbehabior=T)
#villages_from_db<-st_read("/home/ptaconet/react/datasets/react_db.gpkg","rst_villages") %>% as_tibble %>% distinct(codevillage,codepays)  %>% mutate(officialvillages=T)
#View(full_join(villages_humanbehavior,villages_from_db))


