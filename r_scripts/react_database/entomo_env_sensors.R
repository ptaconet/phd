  ### Capteurs environnementaux
  require(readxl)
  require(dplyr)
  require(stringr)
  require(sf)
  require(lubridate)
  require(data.table)

  #path_to_gpkg_database<-"react_db.gpkg"

  ####################################################################
  ############################## BF   ################################
  ####################################################################

  path_to_folder<-"data/react_db/miscellaneous_data/Donnees_Environn_BF_ok/donnees_traitees"

  path_to_missions_folders<-file.path(path_to_folder,c("enqte1_donnees_brutes","enqte2_baro_hobo_hygro","enqte3barohobohygro","enqte4barohobohygro","enqte5barohobohygro","enqte6barohobohygro","enqte7barohobohygro"))

  df_villages<-read_sf(path_to_gpkg_database,"recensement_villages_l1") %>% as_tibble() %>% dplyr::select(nomvillage,codevillage) %>% rename("Village"="nomvillage","village"="codevillage")
  hlc_dates<-read_sf(path_to_gpkg_database,"entomo_csh_metadata_l1") %>% as_tibble %>% mutate(nummission=as.numeric(nummission)) %>% filter(codepays=="BF",nummission<=8) %>% dplyr::select(codevillage,date_capture,nummission)  %>% unique()

  ### Baro
  # on traite la mission 1
  df_baro_enq_1<-NULL
  path_to_baro_sacA<-file.path(path_to_missions_folders[1],"M1_Data environementaux","Sac A","Sac A_Nom des villages_Mision 01 Barometre.xlsx")
  path_to_baro_sacB<-file.path(path_to_missions_folders[1],"M1_Data environementaux","Sac B","Sac B_Mission01_Barometre et nom des villages.xlsx")
  path_to_baro_sacC<-file.path(path_to_missions_folders[1],"M1_Data environementaux","Sac C","Sac C_Mission 01_Barometre-code des villages.xlsx")

  nomvillage_sacA<-c("Dontelo","Gongombiro","Perbalembiro","Diagnon","Niamba","Ouidiaro","Palembera","Yelbelela","Bohero")
  nomvillage_sacB<-c("Kalpalo","Lobignonao","Dangbara","kpedia","Moule","Niombripo","Sousoubro","Tiarkiro","Tiordiero")
  nomvillage_sacC<-c("Doumbourou","Sidmoukar","Kouloh","Nioumbouna","Nipodja","Nouvielgane","Sarambour","Sinkiro","Yelela")

  fun_open_village<-function(path_to_baro_xslx,nomvillage){
    df<-read_excel(path_to_baro_xslx,sheet=nomvillage) %>%
      mutate(path=file.path(path_to_baro_xslx,nomvillage)) %>%
      mutate(Village=nomvillage) %>%
      mutate(nummission=1)
    return(df)
  }

  for(i in 1:length(nomvillage_sacA)){
    #cat(paste0(nomvillage_sacA[i],"\n"))
    df<-fun_open_village(path_to_baro_sacA,nomvillage_sacA[i])
    df$sac<-"A"
    df_baro_enq_1<-rbind(df_baro_enq_1,df)
  }
  for(i in 1:length(nomvillage_sacB)){
    #cat(paste0(nomvillage_sacB[i],"\n"))
    df<-fun_open_village(path_to_baro_sacB,nomvillage_sacB[i])
    df$sac<-"B"
    df_baro_enq_1<-rbind(df_baro_enq_1,df)
  }
  for(i in 1:length(nomvillage_sacC)){
    #cat(paste0(nomvillage_sacC[i],"\n"))
    df<-fun_open_village(path_to_baro_sacC,nomvillage_sacC[i])
    df$sac<-"C"
    df_baro_enq_1<-rbind(df_baro_enq_1,df)
  }

  df_baro_enq_1$Village<-df_baro_enq_1$Village %>%
    str_replace_all(c("Dontelo"="Dontélo","Diagnon"="Diagno","Niamba"="Niaba","Yelbelela"="Yelbéléla","Bohero"="Bohéro","Kalpalo"="Kpalbalo","kpedia"="Kpédia","Moule"="Moulé","Sousoubro"="Soussoubro","Tiordiero"="TIordiero","Nioumbouna"="Niombouna","Sarambour"="Sarambour Pokouro","Yelela"="Yellela"))

  df_baro_enq_1$Date<-as.Date(df_baro_enq_1$Date)
  df_baro_enq_1$Time<-format(df_baro_enq_1$Time, "%H:%M:%S")
  df_baro_enq_1$Ch1_Value<-gsub(",",".",df_baro_enq_1$Ch1_Value) %>% as.numeric()
  df_baro_enq_1$Ch2_Value<-gsub(",",".",df_baro_enq_1$Ch2_Value) %>% as.numeric()
  df_baro_enq_1$Ch3_Value<-gsub(",",".",df_baro_enq_1$Ch3_Value) %>% as.numeric()

  df_baro_enq_1<-df_baro_enq_1 %>%
    left_join(df_villages) %>%
    dplyr::select(-Village)

  enq1_sac_villages<-df_baro_enq_1 %>%
    distinct(sac,village) %>%
    rename(codevillage=village)

  df_baro_enq_1$sac<-NULL

  # cas spécial décalage horaire Niamba NIA (Heure vrai	09h57 ; Heure capteur	18h08). On retire donc 8h11 minutes
  df_baro_enq_1$Time[which(df_baro_enq_1$village=="NIA")]<-format(as_datetime(paste0("1900-01-01 ",df_baro_enq_1$Time[which(df_baro_enq_1$village=="NIA")]))-hours(8)-minutes(11), "%H:%M:%S")
  df_baro_enq_1$Date[which(df_baro_enq_1$village=="NIA")]<-rep(as.Date("2017-01-18"),length(df_baro_enq_1$Date[which(df_baro_enq_1$village=="NIA")]))
  df_baro_enq_1$Date[which(df_baro_enq_1$village=="NIA" & df_baro_enq_1$Position>=49)]<-rep(as.Date("2017-01-19"),57)

  # on traite les missions 2,3
  df_baro_enq_2_3<-NULL
  for (i in c(2,3)){
    #cat(path_to_missions_folders[i],"\n")

    path_to_metadata_baro<-file.path(path_to_missions_folders[i],"table_baro.xlsx")
    metadata_baro<-read_excel(path_to_metadata_baro, sheet = "Feuil1")

    for (j in 1:nrow(metadata_baro)){
      #cat("   ",metadata_baro$fichier_baro[j],"\n")

      if(metadata_baro$fichier_baro[j]=="4ZbaroLOB.txt"){
        metadata_baro$fichier_baro[j]="4ZLOB.txt"
      }
      if(!(metadata_baro$fichier_baro[j]=="4ZbaroNAV.txt")){ # no data for 4NAV
        path_to_df_baro<-file.path(path_to_missions_folders[i],paste0(metadata_baro$dossier[j],"/Sac ",metadata_baro$sac[j],"/",metadata_baro$fichier_baro[j]))
        #path_to_df_baro<-gsub("6Z","6",path_to_df_baro)
        #path_to_df_baro<-gsub("7Z","7",path_to_df_baro)

        if(metadata_baro$fichier_baro[j]!="3ZbaroPAL.txt"){
        # deal with eventual minus signs
        tx  <- readLines(path_to_df_baro)
        tx2  <- gsub(pattern = "- ", replace = "-", x = tx)
        tx2  <- gsub(pattern = "DEGREE C", replace = "DEGREEC", x = tx2)

        writeLines(tx2, con=path_to_df_baro)

        df_baro_th_baro<-read.table(path_to_df_baro,stringsAsFactors = F,header=T)

        df_baro_th_baro$path<-metadata_baro$fichier_baro[j]
        df_baro_th_baro$village<-metadata_baro$village[j]
        df_baro_th_baro$nummission<-metadata_baro$enqte[j]
        df_baro_enq_2_3<-rbind(df_baro_enq_2_3,df_baro_th_baro)
        }
      }
    }
  }

  df_baro_enq_2_3 <- df_baro_enq_2_3 %>%
    mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>%
    mutate(Ch2_unit="DEGREE C")

  # on traite les missions 4, 5, 6, 7
  df_baro_enq_4_5_6_7<-NULL
  for (i in c(4,5,6,7)){

    #cat(path_to_missions_folders[i],"\n")

    if(i==4){
      path_to_df_baro<-file.path(path_to_missions_folders[i],"doc_baro","Sac Z","fichiersExcel","Data Barometre et heures de captures.xls")
    } else if(i==5){
      path_to_df_baro<-file.path(path_to_missions_folders[i],"doc_baro","Sac Z","baro.xlsx")  # baro.xlsx est une copie de Barometres_Heure_des_captures_M5.xlsx
    } else if(i==6){
      path_to_df_baro<-file.path(path_to_missions_folders[i],"doc_baro","Sac Z","data_baro.xlsx")
    } else if(i==7){
      path_to_df_baro<-file.path(path_to_missions_folders[i],"doc_baro","Sac Z","Baro_enqte7.xlsx")
    }

    df_baro_th_baro<-read_excel(path_to_df_baro)

    if(i==7){
      #df_baro_th_baro$Date<-as.Date("1900-01-01")+df_baro_th_baro$Date

      df_baro_th_baro <- df_baro_th_baro %>%
        mutate(Time=df_baro_th_baro$Time*24) %>%
        mutate(Time=strftime(as.POSIXct(Time * 60 * 60, origin = Sys.Date(), tz = "GMT"), format = "%H:%M:%S")) %>%
        mutate(Time=as_datetime(paste(Date,Time))) %>%
        mutate(Date=as_datetime(Date))

        df_baro_th_baro$Date[which(df_baro_th_baro$Village=="Yellela")]<-df_baro_th_baro$Date[which(df_baro_th_baro$Village=="Yellela")]+years(1)

    }
    df_baro_th_baro$Observations<-NULL
    df_baro_th_baro$nummission<-i
    df_baro_th_baro$path<-path_to_df_baro
    df_baro_enq_4_5_6_7<-rbind(df_baro_enq_4_5_6_7,df_baro_th_baro)
  }


  df_baro_enq_4_5_6_7$Date<-as.Date(df_baro_enq_4_5_6_7$Date)
  df_baro_enq_4_5_6_7$Time<-format(df_baro_enq_4_5_6_7$Time, "%H:%M:%S")
  df_baro_enq_4_5_6_7$Ch1_Value<-gsub(",",".",df_baro_enq_4_5_6_7$Ch1_Value) %>% as.numeric()
  df_baro_enq_4_5_6_7$Ch2_Value<-gsub(",",".",df_baro_enq_4_5_6_7$Ch2_Value) %>% as.numeric()
  df_baro_enq_4_5_6_7$Ch3_Value<-gsub(",",".",df_baro_enq_4_5_6_7$Ch3_Value) %>% as.numeric()
  df_baro_enq_4_5_6_7$Village<-df_baro_enq_4_5_6_7$Village %>%
    str_replace_all(c("Nioubouna"="Niombouna","Dontelo"="Dontélo","Diagnon"="Diagno","Soussobrou"="Soussoubro","Sarambour"="Sarambour Pokouro","Tiordiero"="TIordiero","Yelbelella"="Yelbéléla","Niobouna"="Niombouna","Yelbellela"="Yelbéléla","Nouvielgame"="Nouvielgane","Kplabalo"="Kpalbalo"))
  df_baro_enq_4_5_6_7<-df_baro_enq_4_5_6_7 %>%
    left_join(df_villages) %>%
    dplyr::select(-Village)



  #### bind
  df_baro<-rbind(df_baro_enq_1,df_baro_enq_2_3,df_baro_enq_4_5_6_7) %>%
    dplyr::select(Position,Date,Time,Ch1_Value,Ch2_Value,Ch3_Value,village,nummission) %>%
    rename(position=Position,date=Date,time=Time,humidity=Ch1_Value,temperature=Ch2_Value,pressure=Ch3_Value,codevillage=village) %>%
    mutate(date_time=lubridate::ymd_hms(paste(date,time)))

  ### checks-validation

  df_baro <- df_baro %>%
    filter(!(codevillage=="NBO" & nummission==3 & date_time<"2017-05-24 16:00:00")) %>% # on corrige NBO mission 3
    filter(!(codevillage=="OUI" & nummission==4 & date_time<"2017-11-10 16:00:00")) %>% # on corrige OUI mission 4
    filter(!(codevillage=="NAV" & nummission==3 & date_time<"2017-05-18 16:00:00")) %>% # on corrige NAV mission 3
    filter(!(codevillage=="KOU" & nummission==5 & date_time<"2017-12-12 16:00:00")) %>% # on corrige KOU mission 5
    filter(!(codevillage=="KOU" & nummission==7 & date_time>"2018-03-25 10:00:00")) %>% # on corrige KOU mission 7
    filter(!(codevillage=="DIA" & nummission==1 & date_time<"2017-01-26 16:00:00")) %>% # on corrige DIA mission 1
    filter(!(codevillage=="YLE" & nummission==7 & date_time<"2018-03-23 16:00:00")) %>% # on corrige YLE mission 7
    filter(!(codevillage=="DON" & nummission==4 & date_time<"2017-11-04 16:00:00")) # on corrige DON mission 4

  df_baro$date[which(df_baro$codevillage=="KOU" & df_baro$nummission==3 & df_baro$position>1)]<-df_baro$date[which(df_baro$codevillage=="KOU" & df_baro$nummission==3 & df_baro$position>1)]+years(1)
  df_baro$date[which(df_baro$codevillage=="OUI" & df_baro$nummission==7)]<-df_baro$date[which(df_baro$codevillage=="OUI" & df_baro$nummission==7)]+years(1)
  df_baro$date[which(df_baro$codevillage=="NBR" & df_baro$nummission==7)]<-df_baro$date[which(df_baro$codevillage=="NBR" & df_baro$nummission==7)]+years(1)

  ##dates
  #df_baro<-df_baro %>%
  #  full_join(hlc_dates) %>%
  #  mutate(date_capture=as.Date(date_capture)) %>%
  #  mutate(diff_date=date-date_capture)

  df_baro<-df_baro %>%
    dplyr::select(position,date_time,codevillage,nummission,humidity,temperature,pressure)

  # missing :
  # NAV	mission 4 -> normal
  # PAL	mission 3 -> ? disparu
  # BOH	mission 4 -> normal
  # DAN	mission 7 -> ? disparu

  #df_baro_stats_village_mission<-df_baro %>%
  #  group_by(codevillage,nummission) %>%
  #  summarise(num_obs=n(),min_date=min(date),max_date=max(date),min_time=min(date_time),max_time=max(date_time))  %>%
  #  mutate(diff_days=max_date-min_date)




  ### Hobo-hygro
  df_hobo<-NULL
  df_hygro<-NULL
  # on traite les missions 2 à 7
  for (i in c(2,3,4,5,6,7)){

    #cat(path_to_missions_folders[i],"\n")

    path_to_metadata_hygro<-file.path(path_to_missions_folders[i],"table_hobo_hygro.xlsx")
    metadata_hygro<-read_excel(path_to_metadata_hygro, sheet = "Feuil1")

    files_available_hobo_hygro<-list.files(file.path(path_to_missions_folders[i],"doc_hobo_hygro"),recursive = T,full.names = T)

    if (i==3){
      files_available_hobo_hygro<-list.files(file.path(path_to_missions_folders[i],"doc_hobohygro"),recursive = T,full.names = T)
    }

    for(j in 1:nrow(metadata_hygro)){

      path_to_df<-files_available_hobo_hygro[which(grepl(metadata_hygro$nomfichier[j],files_available_hobo_hygro))]
      #cat(paste0("  ",metadata_hygro$capteur[j],"\n"))

      path_to_df<-rev(path_to_df)

      if (i==2){
        if(j<=48 | j>=51){
          path_to_df<-path_to_df[1]
        } else {
          path_to_df<-path_to_df[2]
        }
      }

      if(!(metadata_hygro$nomfichier[j] %in% c("2hoboY7_4e.txt","2hygroY7_4e.txt","5hoboA2_1e.txt","5hoboC5_3i.txt","5hoboD7_4i.txt","5hoboD8_4e.txt"))){  # files missing

       if(grepl("hobo",metadata_hygro$capteur[j])){
         if(i %in% c(2,3)){
           ncol=4
         } else if(i %in% c(4,5,6,7)){
           ncol=5
         }
          df_hobo_th_hobo<-fread(path_to_df,skip = 1, select = c(1:ncol),sep = '\t')
          df_hobo_th_hobo<-as.data.frame(df_hobo_th_hobo)
          df_hobo_th_hobo$X<-NULL
          df_hobo_th_hobo$nummission=i
          df_hobo_th_hobo$pointdecapture=metadata_hygro$point[j]
          df_hobo_th_hobo$postedecapture=metadata_hygro$poste[j]
          df_hobo_th_hobo$sac=metadata_hygro$sac[j]

          colnames(df_hobo_th_hobo)<-tolower(colnames(df_hobo_th_hobo))

          if(i %in% c(2,3)){
            df_hobo_th_hobo$date_heure<-lubridate::mdy_hms(df_hobo_th_hobo$date_heure)
          } else if(i %in% c(4,5,6,7)){
            df_hobo_th_hobo$heure<-gsub(",000","",df_hobo_th_hobo$heure)
            df_hobo_th_hobo$date_heure<-lubridate::dmy_hms(paste(df_hobo_th_hobo$date,df_hobo_th_hobo$heure))
            df_hobo_th_hobo$date<-df_hobo_th_hobo$heure<-NULL
          }

          if(i %in% c(2,3)){
            df_hobo_th_hobo$luminosite<-gsub(",","",df_hobo_th_hobo$luminosite)
          }

          df_hobo<-rbind(df_hobo,df_hobo_th_hobo)
       } else { #if (grepl("hygro",metadata_hygro$capteur[j])){
         df_hygro_th_hygro<-fread(path_to_df,skip = 3, select = c(1:5),header=T,col.names=c("date","heure","temperature","humidity","pointderosee"))
         df_hygro_th_hygro<-as.data.frame(df_hygro_th_hygro)
         df_hygro_th_hygro$nummission=i
         df_hygro_th_hygro$pointdecapture=metadata_hygro$point[j]
         df_hygro_th_hygro$postedecapture=metadata_hygro$poste[j]
         df_hygro_th_hygro$sac=metadata_hygro$sac[j]
         df_hygro_th_hygro$date_heure<-lubridate::dmy_hms(paste(df_hygro_th_hygro$date,df_hygro_th_hygro$heure))
         df_hygro_th_hygro$date<-df_hygro_th_hygro$heure<-NULL

         df_hygro<-rbind(df_hygro,df_hygro_th_hygro)
       }

      }
    }

  }

  # add Kouloh mission 3
  df_hygro_koulohmission3_files<-list.files("data/react_db/miscellaneous_data/Donnees_Environn_BF_ok/donnees_traitees/enqte3barohobohygro/doc_hobohygro/Kouloh",pattern = "hygro",full.names = T)

  for (i in 1:length(df_hygro_koulohmission3_files)){

    df_hygro_th_hygro<-fread(df_hygro_koulohmission3_files[i],skip = 3, select = c(1:5),header=T,col.names=c("date","heure","temperature","humidity","pointderosee"))
    df_hygro_th_hygro<-as.data.frame(df_hygro_th_hygro)
    df_hygro_th_hygro$nummission=3
    df_hygro_th_hygro$pointdecapture=substr(df_hygro_koulohmission3_files[i],nchar(df_hygro_koulohmission3_files[i])-5,nchar(df_hygro_koulohmission3_files[i])-5)
    df_hygro_th_hygro$postedecapture=substr(df_hygro_koulohmission3_files[i],nchar(df_hygro_koulohmission3_files[i])-4,nchar(df_hygro_koulohmission3_files[i])-4)
    df_hygro_th_hygro$sac="D"
    df_hygro_th_hygro$date_heure<-lubridate::dmy_hms(paste(df_hygro_th_hygro$date,df_hygro_th_hygro$heure))
    df_hygro_th_hygro$date<-df_hygro_th_hygro$heure<-NULL

    df_hygro<-rbind(df_hygro,df_hygro_th_hygro)

  }

  df_hobo_koulohmission3_files<-list.files("data/react_db/miscellaneous_data/Donnees_Environn_BF_ok/donnees_traitees/enqte3barohobohygro/doc_hobohygro/Kouloh",pattern = "hobo",full.names = T)

  for (i in 1:length(df_hobo_koulohmission3_files)){

    df_hobo_th_hobo<-fread(df_hobo_koulohmission3_files[i],skip = 1, select = c(1:4),sep = '\t')
    df_hobo_th_hobo<-as.data.frame(df_hobo_th_hobo)
    df_hobo_th_hobo$X<-NULL
    df_hobo_th_hobo$nummission=3
    df_hobo_th_hobo$pointdecapture=substr(df_hobo_koulohmission3_files[i],nchar(df_hobo_koulohmission3_files[i])-5,nchar(df_hobo_koulohmission3_files[i])-5)
    df_hobo_th_hobo$postedecapture=substr(df_hobo_koulohmission3_files[i],nchar(df_hobo_koulohmission3_files[i])-4,nchar(df_hobo_koulohmission3_files[i])-4)
    df_hobo_th_hobo$sac="D"
    df_hobo_th_hobo$date_heure<-lubridate::mdy_hms(df_hobo_th_hobo$date_heure)
    colnames(df_hobo_th_hobo)<-tolower(colnames(df_hobo_th_hobo))

    df_hobo<-rbind(df_hobo,df_hobo_th_hobo)


  }

  # add mission 1
  df_hobo$idappdemes<-NA
  df_hygro$idappdemes<-NA

  df_hygro_mission1_files<-list.files("data/react_db/miscellaneous_data/Donnees_Environn_BF_ok/donnees_traitees/enqte1_donnees_brutes/M1_Data environementaux",pattern = "_courbe.csv",full.names = T,recursive = T)
  df_hobo_mission1_files<-list.files("data/react_db/miscellaneous_data/Donnees_Environn_BF_ok/donnees_traitees/enqte1_donnees_brutes/M1_Data environementaux",pattern = "Hobo_",full.names = T,recursive = T)

  ent_supervcaptureraw<-read_sf(path_to_gpkg_database,"entomo_csh_ctrlequalite_l0") %>% as_tibble() %>% filter(codepays=="BF" & nummission==1) %>%
    group_by(idpostedecapture,idappdemes) %>%
    summarise(number_obs=n()) %>%
    slice(which.max(number_obs)) %>%
    mutate(idappdemes=as.numeric(idappdemes)) %>%
    mutate(pointdecapture=as.numeric(substr(idpostedecapture,5,5))) %>%
    mutate(postedecapture=substr(idpostedecapture,6,6)) %>%
    mutate(codevillage=substr(idpostedecapture,2,4)) %>%
    dplyr::select(codevillage,pointdecapture,postedecapture,idappdemes)  %>%
    as.data.frame() %>%
    dplyr::select(-idpostedecapture)


  for(i in 1:length(df_hygro_mission1_files)){
    df_hygro_th_hygro<-fread(df_hygro_mission1_files[i],skip = 3, select = c(1:5),header=T,col.names=c("date","heure","temperature","humidity","pointderosee"))
    df_hygro_th_hygro<-as.data.frame(df_hygro_th_hygro)
    df_hygro_th_hygro$nummission=1
    df_hygro_th_hygro$pointdecapture=NA
    df_hygro_th_hygro$postedecapture=NA
    df_hygro_th_hygro$sac=substr(df_hygro_mission1_files[i],nchar(df_hygro_mission1_files[i])-27,nchar(df_hygro_mission1_files[i])-27)
    df_hygro_th_hygro$idappdemes<-as.numeric(substr(df_hygro_mission1_files[i],nchar(df_hygro_mission1_files[i])-12,nchar(df_hygro_mission1_files[i])-11))
    df_hygro_th_hygro$date_heure<-lubridate::dmy_hms(paste(df_hygro_th_hygro$date,df_hygro_th_hygro$heure))
    df_hygro_th_hygro$date<-df_hygro_th_hygro$heure<-NULL

    df_hygro<-rbind(df_hygro,df_hygro_th_hygro)
  }


  for(i in 1:length(df_hobo_mission1_files)){
    df_hobo_th_hobo<-read_excel(df_hobo_mission1_files[i],skip =2, col_names=c("position","date_heure","temperature","luminosite"))
    df_hobo_th_hobo$nummission<-1
    df_hobo_th_hobo$sac<-substr(df_hobo_mission1_files[i],nchar(df_hobo_mission1_files[i])-17,nchar(df_hobo_mission1_files[i])-17)
    df_hobo_th_hobo$idappdemes<-as.numeric(substr(df_hobo_mission1_files[i],nchar(df_hobo_mission1_files[i])-5,nchar(df_hobo_mission1_files[i])-4))
    df_hobo_th_hobo$date_heure<-lubridate::mdy_hms(df_hobo_th_hobo$date_heure)
    df_hobo_th_hobo$pointdecapture<-NA
    df_hobo_th_hobo$postedecapture<-NA
    df_hobo<-rbind(df_hobo,df_hobo_th_hobo)
  }


  df_hygro$temperature<-as.numeric(gsub(",",".",df_hygro$temperature))
  df_hygro$humidity<-as.numeric(gsub(",",".",df_hygro$humidity))
  df_hygro$pointderosee<-as.numeric(gsub(",",".",df_hygro$pointderosee))

  df_hobo$temperature<-as.numeric(gsub(",",".",df_hobo$temperature))
  df_hobo$luminosite<-as.numeric(gsub(",",".",df_hobo$luminosite))


  # On doit maintenant relier le nom des villages

  ent_hlcmetadataraw<-read_sf(path_to_gpkg_database,"entomo_csh_metadata_l0") %>% filter(codepays=="BF",n_mission<=8) %>% dplyr::select(codevillage,n_mission,date_de_captures,n_sac,date_heure_debut,date_heure_fin)
  colnames(ent_hlcmetadataraw)=c("codevillage","nummission","date","sac","date_heure_debut","date_heure_fin")
  ent_hlcmetadataraw$date_heure_debut<-ymd_hms(ent_hlcmetadataraw$date_heure_debut)
  ent_hlcmetadataraw$date_heure_fin<-ymd_hms(ent_hlcmetadataraw$date_heure_fin)
  # fill sac mission 1
  ent_hlcmetadataraw_mission_1<-ent_hlcmetadataraw %>%
    filter(nummission==1) %>%
    left_join(enq1_sac_villages,by="codevillage") %>%
    mutate(sac=sac.y) %>%
    dplyr::select(-sac.y,-sac.x)

  ent_hlcmetadataraw <- ent_hlcmetadataraw %>%
    filter(nummission!=1) %>%
    rbind(ent_hlcmetadataraw_mission_1)

  # correction pour certains sacs
  ent_hlcmetadataraw<-ent_hlcmetadataraw %>%
    mutate(sac=replace(sac, codevillage=="DAN" & nummission==2, "DAN"))

  df_hobo <- df_hobo %>%
    mutate(heure=as.numeric(strftime(date_heure, format="%H"))) %>%
    mutate(date=if_else(heure>14,as.Date(date_heure),as.Date(date_heure)-1)) %>%
    mutate(date=as.character(date))

  df_hobo2<- df_hobo %>%
    full_join(ent_hlcmetadataraw)

  # on regarde s'il manque des villages
  missing<-df_hobo2 %>%
    filter(is.na(date_heure)) %>%
    dplyr::select(date,codevillage) %>%
    left_join(ent_hlcmetadataraw)

  # check times and number of observations
  df_hobo3 <-df_hobo %>%
    left_join(ent_hlcmetadataraw) %>%
    filter(date_heure>=date_heure_debut & date_heure<=date_heure_fin) %>%
    group_by(codevillage,nummission,pointdecapture,postedecapture) %>%
    summarise(compte=n(),min_date=min(date_heure),max_date=max(date_heure))

  # on ajoute les pointdecapture et postedecapture pour la mission 1

  df_hobo_mission_1<-df_hobo %>%
    filter(nummission==1) %>%
    dplyr::select(-pointdecapture) %>%
    dplyr::select(-postedecapture) %>%
    left_join(ent_hlcmetadataraw) %>%
    left_join(ent_supervcaptureraw,by=c("codevillage","idappdemes")) %>%
    dplyr::select(position,date_heure,temperature,luminosite,nummission,pointdecapture,postedecapture,sac,idappdemes,heure,date) %>%
    mutate(pointdecapture=as.character(pointdecapture)) %>%
    filter(!(is.na(postedecapture)))

  # On est bien !! le dataset final :
  df_hobo <-df_hobo %>%
    filter(nummission!=1) %>%
    rbind(df_hobo_mission_1) %>%
    left_join(ent_hlcmetadataraw) %>%
    filter(date_heure>=date_heure_debut & date_heure<=date_heure_fin) %>% # on ne garde que les heures pendant lesquelles il y a des captures
    mutate(idpointdecapture=paste0(nummission,codevillage,pointdecapture,postedecapture)) %>%
    dplyr::select(idpointdecapture,position,date_heure,temperature,luminosite) %>%
    rename(date_time=date_heure)

  # View(df_hobo %>% group_by(idpointdecapture) %>% summarise(compte=n(),min_date=min(date_heure),max_date=max(date_heure)))

  ## pareil pour hygro

  df_hygro <- df_hygro %>%
    mutate(heure=as.numeric(strftime(date_heure, format="%H"))) %>%
    mutate(date=if_else(heure>14,as.Date(date_heure),as.Date(date_heure)-1)) %>%
    mutate(date=as.character(date))


  df_hygro2<- df_hygro %>%
    full_join(ent_hlcmetadataraw)

  missing<-df_hygro2 %>%
    filter(is.na(date_heure)) %>%
    dplyr::select(date,codevillage) %>%
    left_join(ent_hlcmetadataraw)

  # check times and number of observations
  df_hygro3 <-df_hygro %>%
    left_join(ent_hlcmetadataraw) %>%
    filter(date_heure>=date_heure_debut & date_heure<=date_heure_fin) %>%
    group_by(codevillage,nummission,pointdecapture,postedecapture) %>%
    summarise(compte=n(),min_date=min(date_heure),max_date=max(date_heure))


  # on ajoute les pointdecapture et postedecapture pour la mission 1

  df_hygro_mission_1<-df_hygro %>%
    filter(nummission==1) %>%
    dplyr::select(-pointdecapture) %>%
    dplyr::select(-postedecapture) %>%
    left_join(ent_hlcmetadataraw) %>%
    left_join(ent_supervcaptureraw,by=c("codevillage","idappdemes")) %>%
    dplyr::select(date_heure,temperature,humidity,pointderosee,nummission,pointdecapture,postedecapture,sac,idappdemes,heure,date) %>%
    mutate(pointdecapture=as.character(pointdecapture)) %>%
    filter(!(is.na(postedecapture)))

  # On est bien !! le dataset final :
  df_hygro <-df_hygro %>%
    filter(nummission!=1) %>%
    rbind(df_hygro_mission_1) %>%
    left_join(ent_hlcmetadataraw) %>%
    filter(date_heure>=date_heure_debut & date_heure<=date_heure_fin) %>% # on ne garde que les heures pendant lesquelles il y a des captures
    mutate(idpointdecapture=paste0(nummission,codevillage,pointdecapture,postedecapture)) %>%
    dplyr::select(idpointdecapture,date_heure,temperature,humidity,pointderosee) %>%
    rename(date_time=date_heure)

  # View(df_hygro %>% group_by(idpointdecapture) %>% summarise(compte=n(),min_date=min(date_heure),max_date=max(date_heure)))

  df_hygro$date_time<-as.character(df_hygro$date_time)
  df_hobo$date_time<-as.character(df_hobo$date_time)
  df_baro$date_time<-as.character(df_baro$date_time)

# df_hobo, df_hygro, df_baro
  ####################################################################
  ############################## CIV  ################################
  ####################################################################
