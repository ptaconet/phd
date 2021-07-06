library(tidyverse) 
library(lubridate)
library(readxl)

# load household data and match date field type
behav_men_BF_1 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_BF.xlsx", sheet = "HBm1", na=c("","NA")) %>%
	mutate(dateenquete = dateenquete %>% as.character()) %>% distinct()
behav_men_BF_2 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_BF.xlsx", sheet = "HBm2", na=c("","NA")) %>%
	mutate(dateenquete = dateenquete %>% as.character()) %>% distinct()
behav_men_BF_3 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_BF.xlsx", sheet = "HBm3", na=c("","NA")) %>%
	mutate(dateenquete = dateenquete %>% mdy() %>% as.character()) %>% distinct()

# load individual data, join to household data (gps data), delete most of duplicates, match hour data type
behav_ind_BF_1 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_BF.xlsx", sheet = "HBm1Behavior", na=c("","NA"),col_types = "text") %>%
	mutate(rcpdateenquete = as_date(as.numeric(rcpdateenquete), origin = "1899-12-30") %>% as.character()) %>%
	left_join(behav_men_BF_1[,c(7:9,11,14)],by = c("rcpcodemenage"="codemenage","rcpdateenquete"="dateenquete")) %>%
	distinct() %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), as.numeric ) %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), map_dbl, prod,24,3600 ) %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), hms::as_hms ) %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), as.character) %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), str_sub,1,8 ) 

behav_ind_BF_2 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_BF.xlsx", sheet = "HBm2Behavior", na=c("","NA"),col_types = "text") %>%
	mutate(rcpdateenquete = as_date(as.numeric(rcpdateenquete), origin = "1899-12-30") %>% as.character()) %>%
	left_join(behav_men_BF_2[,c(7:9,11,14)],by = c("rcpcodemenage"="codemenage","rcpdateenquete"="dateenquete")) %>%
	filter(!(is.na(infosmenagecoordgpsLatitude))) %>%
	distinct() %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), as.numeric ) %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), map_dbl, prod,24,3600 ) %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), hms::as_hms ) %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), as.character) %>%
	mutate_at(c("hintmaison","hcoucher","hlever","hsortiemaison"), str_sub,1,8 ) 

behav_ind_BF_3 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_BF.xlsx", sheet = "HBm3Behavior", na=c("","NA"),col_types = "text") %>%
	left_join(behav_men_BF_3[,c(7:9,11,14)],by = c("rcpcodemenage"="codemenage","rcpdateenquete"="dateenquete")) %>%
	distinct() 

# create dataframe for BF data
behav_ind_BF <- bind_rows(behav_ind_BF_1, behav_ind_BF_2, behav_ind_BF_3) %>%
	dplyr::select(-c(parentuid, ageclass1:ageclass3)) %>%
	mutate(survey = ifelse(ymd(rcpdateenquete) %within% interval(ymd("2017-02-21"),ymd("2017-04-29")), 1, NA)) %>%
	mutate(survey = ifelse(ymd(rcpdateenquete) %within% interval(ymd("2017-09-16"),ymd("2017-09-23")), 2, survey)) %>%
	mutate(survey = ifelse(ymd(rcpdateenquete) %within% interval(ymd("2018-01-30"),ymd("2018-02-14")), 3, survey)) %>%
  mutate(codepays="BF") %>%
  mutate(rcpdateenquete=as.Date(rcpdateenquete)) %>%
  mutate(periode="preinterv") %>%
  mutate(periode=replace(periode, rcpdateenquete>"2018-01-01", "postinterv")) %>%
  mutate(saison="seche") %>%
  mutate(saison=replace(saison, rcpdateenquete>"2017-09-01" & rcpdateenquete<"2017-10-01", "pluies"))



# behav_ind_BF %>% mutate(week = isoweek(ymd(rcpdateenquete)), year = isoyear(ymd(rcpdateenquete))) %>%
# 	group_by(year,week) %>%
# 	summarise(n=n(),min=min(ymd(rcpdateenquete)),max=max(ymd(rcpdateenquete)))
	
# same for CI data
behav_men_CI_1 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_CI.xlsx", sheet = "HBm1", na=c("","NA")) %>%
	mutate(dateenquete = dateenquete %>% mdy() %>% as.character()) %>% distinct()

behav_ind_CI_1 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_CI.xlsx", sheet = "HBm1Behavior", na=c("","NA")) %>% distinct() %>%
	left_join(behav_men_CI_1[,c(7:9,11,14)],by = c("rcpcodemenage"="codemenage","rcpdateenquete"="dateenquete")) %>%
	distinct(across(!c(parentuid,infosmenagecoordgpsLatitude,infosmenagecoordgpsLongitude,infosmenagecoordgpsAltitude)),.keep_all = TRUE)

behav_men_CI_2 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_CI.xlsx", sheet = "HBm2", na=c("","NA")) %>%
	mutate(dateenquete = dateenquete %>% mdy() %>% as.character()) %>% distinct()

behav_ind_CI_2 <- read_excel("data/react_db/miscellaneous_data/All_Data_HumanBehavior_CI.xlsx", sheet = "HBm2Behavior", na=c("","NA")) %>% distinct() %>%
	left_join(behav_men_CI_2[,c(7:9,11,14)],by = c("rcpcodemenage"="codemenage","rcpdateenquete"="dateenquete")) %>%
	distinct(across(!c(parentuid,infosmenagecoordgpsLatitude,infosmenagecoordgpsLongitude,infosmenagecoordgpsAltitude)),.keep_all = TRUE)

behav_ind_CI <- bind_rows(behav_ind_CI_1, behav_ind_CI_2) %>%
	dplyr::select(-c(parentuid,ageclass1:ageclass3)) %>%
	mutate(survey = ifelse(ymd(rcpdateenquete) %within% interval(ymd("2017-09-29"),ymd("2017-11-05")), 2, 3)) %>%
  mutate(codepays="CI") %>%
  mutate(rcpdateenquete=as.Date(rcpdateenquete)) %>%
  mutate(periode="preinterv") %>%
  mutate(periode=replace(periode, rcpdateenquete>"2017-09-30", "postinterv")) %>%
  mutate(saison="seche") %>%
  mutate(saison=replace(saison, rcpdateenquete<"2018-01-01", "pluies")) %>%
  mutate(rcpcodevillage=replace(rcpcodevillage, rcpcodevillage=="NAV", "NAA")) %>%
  mutate(rcpcodevillage=replace(rcpcodevillage, rcpcodevillage=="KOU", "KON")) %>%
  mutate(rcpcodevillage=replace(rcpcodevillage, rcpcodevillage=="NAA" & infosmenagecoordgpsLatitude<9, "NAM")) %>%
  mutate(rcpcodevillage=replace(rcpcodevillage, rcpcodevillage=="KOL" & infosmenagecoordgpsLatitude<9, "BLA")) %>%
  mutate(rcpcodevillage=replace(rcpcodevillage, rcpcodevillage=="NAN", "NAK")) %>%
  mutate(rcpcodemenage = paste0(rcpcodevillage, substr(rcpcodemenage,4,6)))

# behav_ind_CI %>% mutate(week = isoweek(ymd(rcpdateenquete)), year = isoyear(ymd(rcpdateenquete))) %>%
# 	group_by(year,week) %>%
# 	summarise(n=n(),min=min(ymd(rcpdateenquete)),max=max(ymd(rcpdateenquete)))


# merge BF and CI data			
df_humanbehavior <- bind_rows(behav_ind_BF,behav_ind_CI)		

colnames(df_humanbehavior) <- gsub("rcp","",colnames(df_humanbehavior))
df_humanbehavior$infosmenagecoordgpsAltitude <- NULL
df_humanbehavior$latitude <- df_humanbehavior$infosmenagecoordgpsLatitude
df_humanbehavior$longitude <- df_humanbehavior$infosmenagecoordgpsLongitude
df_humanbehavior$infosmenagecoordgpsLatitude <- df_humanbehavior$infosmenagecoordgpsLongitude <- NULL

df_humanbehavior$latitude[which(is.na(df_humanbehavior$latitude) & df_humanbehavior$codevillage=="OUI")] = 10.7911
df_humanbehavior$longitude[which(is.na(df_humanbehavior$longitude) & df_humanbehavior$codevillage=="OUI")] = -3.4026
  
df_humanbehavior$latitude[which(is.na(df_humanbehavior$latitude) & df_humanbehavior$codevillage=="NIP")] =   10.9892
df_humanbehavior$longitude[which(is.na(df_humanbehavior$longitude) & df_humanbehavior$codevillage=="NIP")] = -3.3848
  
df_humanbehavior$latitude[which(is.na(df_humanbehavior$latitude) & df_humanbehavior$codevillage=="KPA")] = 10.8153272
df_humanbehavior$longitude[which(is.na(df_humanbehavior$longitude) & df_humanbehavior$codevillage=="KPA")] = -3.44626716
  
df_humanbehavior$latitude[which(is.na(df_humanbehavior$latitude) & df_humanbehavior$codevillage=="TDI")] = 10.69310416
df_humanbehavior$longitude[which(is.na(df_humanbehavior$longitude) & df_humanbehavior$codevillage=="TDI")] = -3.202630125