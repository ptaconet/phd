# load libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(stringdist)
library(stringi)

### load data -----
# detection active
BF_act_df <- read_delim("miscellaneous_data/Data_epidemio/BF_enquete_active.csv", delim=",") %>% as.data.frame()
CI_act_df <- read_excel("miscellaneous_data/Data_epidemio/CI_enquete_active.xlsx", na=c("","NA"), col_types = "text") %>% as.data.frame()
date_act_CI_df <- read_delim("miscellaneous_data/Data_epidemio/date_enq_act_CI.csv", delim="\t") %>% as.data.frame()
load("miscellaneous_data/Data_epidemio/col_to_remove_BF.RData")
load("miscellaneous_data/Data_epidemio/col_to_remove_CI.RData")
#detection passive
BF_pas_df <- read_excel("miscellaneous_data/Data_epidemio/BF_suivi_passif.xlsx", col_types = "text")
CI_pas_df <- read_excel("miscellaneous_data/Data_epidemio/CI_suivi_passif.xlsx", col_types = "text")

# goutte épaisse
BF_GE_df <-  read_tsv("miscellaneous_data/Data_epidemio/GE_BF_post.csv") %>% as.data.frame()
CI_GE_df <-  read_tsv("miscellaneous_data/Data_epidemio/GE_CI_post.csv") %>% as.data.frame()

#interventions
Intervention_df <- read.delim("miscellaneous_data/Data_epidemio/Intervention.txt")

# household data
household <-  read_tsv("miscellaneous_data/Data_epidemio/household.csv") %>% as.data.frame()
household <- household %>% mutate_at(c(4,5,18:30,78), as.factor)

# calculate population size
vil_pop <- household %>% group_by(country, codevillage) %>% summarise(pop = sum(nbrehabitant))


### Work on active surveys data ----
BF_act <- BF_act_df
# supprimer/modifier lignes aberantes
BF_act[BF_act$codevillage=="KOL",] <- gsub("KOL","KOU", BF_act[BF_act$codevillage=="KOL",])
BF_act <- BF_act[!(BF_act$codevillage %in% c("NON")),]
# modify column types
BF_act$codemenage <- str_sub(BF_act$codeindividu,1,6) # because some codeinidvidu may have been modified...
BF_act$codevillage <- str_sub(BF_act$codeindividu,1,3)  # because some codeinidvidu may have been modified...
BF_act$codeindividu_GE <- paste0(BF_act$codeindividu_GE,"_",BF_act$codenquete)
BF_act$datenquete <- BF_act$datenquete %>% dmy()
BF_act$age <- BF_act$age %>% as.numeric()

# create date_BF_act
date_act_BF <- BF_act %>%
	mutate(datenquete = as.character(datenquete)) %>%
	group_by(codevillage, codenquete, datenquete) %>%
	summarise(n=n()) %>%
	ungroup() %>%
	group_by(codevillage, codenquete) %>%
	filter(n == max(n)) %>%
	summarise(datenquete = first(datenquete)) %>%
	ungroup() %>%
	mutate(datenquete = ymd(datenquete))

date_act_BF <- date_act_BF[!(date_act_BF$codevillage %in% c("NON","KOL","(Mi")),]

# create a new field date (from BF_act$datenquet) and update BF_act$datenquete with date_act_BF

BF_act <- BF_act %>% mutate(date = datenquete)
BF_act <- left_join(BF_act, date_act_BF, by=c("codevillage","codenquete" )) %>%
	mutate(datenquete.x = datenquete.y) %>%
	rename(datenquete = datenquete.x) %>%
	dplyr::select(-c("datenquete.y")) %>%
	mutate(country = "BF") %>%
  mutate(datenquete=as.character(datenquete))


#dplyr::select usefull column
BF_act_tight <- BF_act[, !(colnames(BF_act) %in% col_to_remove_BF), drop = FALSE]




# dplyr::select usefull column and create new ones (for correspondance with BF_act)
CI_act <- CI_act_df

CI_act <- CI_act %>% mutate(codenqte = as.numeric(codenqte) - 10)

## update codevillage in CI
# a parti de l'enquete 4, le code NAv correspond au nouveau village NAM (Namasselikaha)
# a parti de l'enquete 4, le code KOL correspond au nouveau village BLA (Blahouara)
# le code de KOU est modifié en KON pour éviter les homonymies avec le BF
# le code de NAV (enquete 1 à 3) est modifié en NAA pour éviter les homonymies avec le BF
CI_act$codeindividu <- with(CI_act, if_else(str_detect(codeindividu, "NAV") & codenqte > 3, str_replace(codeindividu, "NAV", "NAM"), codeindividu))
CI_act$codeindividu <- with(CI_act, if_else(str_detect(codeindividu, "KOL") & codenqte > 3, str_replace(codeindividu, "KOL", "BLA"), codeindividu))
CI_act$codeindividu <- with(CI_act, if_else(str_detect(codeindividu, "KOU") , str_replace(codeindividu, "KOU", "KON"), codeindividu))
CI_act$codeindividu <- with(CI_act, if_else(str_detect(codeindividu, "NAV") & codenqte <= 3, str_replace(codeindividu, "NAV", "NAA"), codeindividu))

# convertir les deux colonnes d'âge (mois et année) en une seule
CI_act <- CI_act %>% mutate(age = as.numeric(CI_act$ageannee))
CI_act$age[is.na(CI_act$age)] <- as.numeric(CI_act$agemois[is.na(CI_act$age)]) / 12
CI_act$agemois <- CI_act$ageannee <- NULL # delete agemois and ageanne

#
CI_act <- CI_act %>% mutate(ind_enq = paste0(CI_act$codeindividu,"_",CI_act$codenqte))
CI_act <- CI_act %>% mutate(codevillage = str_sub(CI_act$codeindividu,1,3))
CI_act$codemenage <- str_sub(CI_act$codeindividu,1,6)
CI_act$codeindividu_GE <- CI_act$ind_enq

#supprimer 2 ligne abérantes
CI_act <- CI_act[!(CI_act$codevillage %in% c("LPL","M")),]


# harmonize column names
#match(names(BF_act),names(CI_act))
names(CI_act)[which(names(CI_act)=="codenqte")] <- "codenquete"
names(CI_act)[which(names(CI_act)=="csps_csu")] <- "csps"


# create a new field date (from CI_act$datenquet) and update CI_act$datenquete with date_act_BF
CI_act <- CI_act %>% mutate(date = as_date(NA))

CI_act$date[str_which(CI_act$datenquete,"[:digit:]{5}")] <- CI_act$datenquete[str_which(CI_act$datenquete,"[:digit:]{5}")] %>%
	as.numeric() %>%
	as_date(origin = ymd("1900-01-01"))

CI_act$date[-(str_which(CI_act$datenquete,"[:digit:]{5}"))] <- CI_act$datenquete[-(str_which(CI_act$datenquete,"[:digit:]{5}"))] %>%
	mdy()

# ubdate datenquete values with data from date_act_CI
date_act_CI <- date_act_CI_df[!duplicated(date_act_CI_df[,c(1:2)]),] # remove duplicated date (two date for one village in one survey)
## update codevillage in CI
# a parti de l'enquete 4, le code NAv correspond au nouveau village NAM (Namasselikaha)
# a parti de l'enquete 4, le code KOL correspond au nouveau village BLA (Blahouara)
# le code de KOU est modifié en KON pour éviter les homonymies avec le BF
# le code de NAV (enquete 1 à 3) est modifié en NAA pour éviter les homonymies avec le BF
date_act_CI$codevillage <- with(date_act_CI, if_else(str_detect(codevillage, "NAV") & codenquete > 3, str_replace(codevillage, "NAV", "NAM"), codevillage))
date_act_CI$codevillage <- with(date_act_CI, if_else(str_detect(codevillage, "KOL") & codenquete > 3, str_replace(codevillage, "KOL", "BLA"), codevillage))
date_act_CI$codevillage <- with(date_act_CI, if_else(str_detect(codevillage, "KOU") , str_replace(codevillage, "KOU", "KON"), codevillage))
date_act_CI$codevillage <- with(date_act_CI, if_else(str_detect(codevillage, "NAV") & codenquete <= 3, str_replace(codevillage, "NAV", "NAA"), codevillage))

CI_act <- left_join(CI_act, date_act_CI[,c(1:3)], by=c("codevillage","codenquete" )) %>%
	mutate(datenquete.x = datenquete.y) %>%
	rename(datenquete = datenquete.x) %>%
	dplyr::select(-c("datenquete.y")) %>%
	mutate(country = "CI") %>%
  mutate(datenquete=as.character(datenquete))

#dplyr::select usefull column
CI_act_tight <- CI_act[, !(colnames(CI_act) %in% col_to_remove_CI), drop = FALSE]


# merge BF and CI data
CI_act_tight <- CI_act_tight %>% mutate(codenquete = as.character(codenquete))
act <- bind_rows(BF_act_tight, CI_act_tight) %>% arrange(codeindividu,codenquete) # reorder data to be used by geeglm

# modify column data types and clean
act <- act %>% mutate_at(c(3:7,11,13:22, 24:26), as.factor)
act$resultatdr[act$resultatdr == "TestnonFait"] <- NA
act$resultatdr <- droplevels(act$resultatdr)
act <- act %>% mutate_at(c(9,10,12), as.numeric)

# create a column age_cat with age as categories
act$age_c <- act$age %>% cut( breaks=c(-1, 5, 10, +Inf),
															labels=c("0-5", "5-10", ">10")) %>% as.factor()

# create columne case ((hbrile48h = OUI | tempaxillaire > 37) & resultatdr = Positif, WHO def. )
# in BF, all children were tested with TDR
# in CIV, only clinical cases (but not all with fever) were tested with TDR
temp <- 37   # according to the definition of a malria case (TDR + fever > 37.5°), but temp axillaire is 0.5° lower than body temp
act <- act %>% mutate(fever = ifelse(hfebrile48hprecedtes == "OUI" | tempaxillaire > temp, 1, 0)) # fever or history of fever
act <- act %>% mutate(fever2 = ifelse(tempaxillaire > temp, 1, 0)) # fever at time of observation
act <- act %>% mutate(case = ifelse(fever == 1 & resultatdr == "Positif", 1, 0)) # valid for BF & CIV (but with numerous NA in CIV), to be used for cases analysis in BF
act <- act %>% mutate(case2 = ifelse(tdr == "OUI" & resultatdr == "Positif", 1, 0)) # probable case for CIV, prevalence for BF, to be used for cases analysis in CIV



### Work on intervention data ----
Intervention <- Intervention_df
# add new villages to Intervention table (NAM et BLA)
# le code de KOU est modifié en KON pour éviter les homonymies avec le BF
# le code de NAV est modifié en NAA pour éviter les homonymies avec le BF
Intervention$codevillage <- fct_expand(Intervention$codevillage,"BLA","NAM","NAA","KON")

Intervention<-rbind(Intervention, c("BLA","CIV","IRS"))
Intervention<-rbind(Intervention, c("NAM","CIV","Larvicide"))
Intervention[Intervention$codevillage == "KOU" & Intervention$country == "CIV",1] <- "KON"
Intervention[Intervention$codevillage == "NAV" & Intervention$country == "CIV",1] <- "NAA"
# merge with pop data
Intervention <- left_join(Intervention, vil_pop, by="codevillage")


### work on passive detection data ----
CI_pas <- CI_pas_df
BF_pas <- BF_pas_df


CI_pas$village <- toupper(CI_pas$village) # mettre en majuscule
CI_pas$codevillage <- str_sub(CI_pas$village,-3,-1) %>% as.factor()
#levels(CI_pas$codevillage)

# changer code_village (homonymie BF-CI)
CI_pas$codevillage <- fct_expand(CI_pas$codevillage,"NAA","KON")
CI_pas[CI_pas$codevillage == "KOU",3] <- "KON"
CI_pas[CI_pas$codevillage == "NAV",3] <- "NAA"

# date de consulattion (plusieurs format à harmoniser)
#CI_pas <- CI_pas %>% mutate(date = as_date(NA))

CI_pas$datenquete <- as.character(as.Date(as.numeric(CI_pas$datenquete),origin = "1899-12-30"))

CI_pas$datecsltation[-(str_which(CI_pas$datecsltation,"[:digit:]{5}"))] <- CI_pas$datecsltation[-(str_which(CI_pas$datecsltation,"[:digit:]{5}"))] %>%
  mdy() %>%
  as.character()

CI_pas$datecsltation[str_which(CI_pas$datecsltation,"[:digit:]{5}")] <- CI_pas$datecsltation[str_which(CI_pas$datecsltation,"[:digit:]{5}")] %>%
	as.numeric() %>%
	as_date(origin = ymd("1899-12-30"))  %>%
  as.character()


# csps à harmoniser
#levels(as.factor(toupper(CI_pas$csu)))
CI_pas$csu <- toupper(CI_pas$csu)
CI_pas$csu <- gsub("CSUDE","",CI_pas$csu)
CI_pas$csu <- gsub("CSRDE","",CI_pas$csu)
CI_pas$csu <- gsub("DRDE","",CI_pas$csu)
CI_pas$csu <- gsub("_[[:upper:]]{3}","",CI_pas$csu)

CI_pas$csu <- as.factor(CI_pas$csu)
CI_pas$csu <- CI_pas$csu %>% fct_recode("BALLEKAHA" = "BALLÉKAHA")
CI_pas <- CI_pas[!(CI_pas$csu == "SILUEIFOU"),] # suppression ligne
CI_pas$csu <- CI_pas$csu %>% fct_drop()

#levels(CI_pas$csu) # check levels
#match(names(BF_pas), names(CI_pas))
names(CI_pas)[1] <- names(BF_pas)[1]
names(CI_pas)[2]  <- names(BF_pas)[2]
names(CI_pas)[17]  <- names(BF_pas)[17]
names(CI_pas)[18]  <- names(BF_pas)[18]
names(CI_pas)[43]  <- names(BF_pas)[43]
names(CI_pas)[44]  <- names(BF_pas)[44]

# date de consulattion (plusieurs format à harmoniser)
#BF_pas <- BF_pas %>% mutate(date = as_date(NA))

BF_pas$datenquete[-(str_which(BF_pas$datenquete,"[:digit:]{5}"))] <- BF_pas$datenquete[-(str_which(BF_pas$datenquete,"[:digit:]{5}"))] %>%
  mdy() %>%
  as.character()

BF_pas$datenquete[str_which(BF_pas$datenquete,"[:digit:]{5}")] <- BF_pas$datenquete[str_which(BF_pas$datenquete,"[:digit:]{5}")] %>%
  as.numeric() %>%
  as_date(origin = ymd("1899-12-30"))  %>%
  as.character()

BF_pas$datecsltation[-(str_which(BF_pas$datecsltation,"[:digit:]{5}"))] <- BF_pas$datecsltation[-(str_which(BF_pas$datecsltation,"[:digit:]{5}"))] %>%
  mdy() %>%
  as.character()

BF_pas$datecsltation[str_which(BF_pas$datecsltation,"[:digit:]{5}")] <- BF_pas$datecsltation[str_which(BF_pas$datecsltation,"[:digit:]{5}")] %>%
  as.numeric() %>%
  as_date(origin = ymd("1899-12-30"))  %>%
  as.character()


# bind BF & CI rows
BF_pas$country <- "BF"
CI_pas$country <- "CI"
pas <- bind_rows(BF_pas, CI_pas)
pas <- pas %>% mutate(palu = ifelse(palusimple == "OUI" | palugrave == "OUI", TRUE, FALSE))



###### Work on GE data ----
BF_GE <- BF_GE_df
CI_GE <- CI_GE_df

# harmonise names
match(names(BF_GE), names(CI_GE))
match(names(CI_GE), names(BF_GE))
names(BF_GE)[5] <- names(CI_GE)[5]
names(BF_GE)[2] <- names(CI_GE)[2] <- "codenquete"

# remplacer NA par zero quand utile
BF_GE[, 9:20][is.na(BF_GE[, 9:20])] <- 0

# recalculate dp (and code NA )
BF_GE <- BF_GE %>% mutate(dp_pf = tropho_pf*8000 / nbredeleucocytes)
BF_GE <- BF_GE %>% mutate(dp_pm = tropho_pm*8000 / nbredeleucocytes)
BF_GE <- BF_GE %>% mutate(dp_po = tropho_po*8000 / nbredeleucocytes)

# update columns (gameto_pX are number in BF and binomila in CI)
# create new column for gameto number in BF
BF_GE$gameto_pf_n <- BF_GE$gameto_pf
BF_GE$gameto_pm_n <- BF_GE$gameto_pm
BF_GE$gameto_po_n <- BF_GE$gameto_po

# and convert to binomial data
BF_GE$gameto_pf[BF_GE$gameto_pf > 0 ] <- 1
BF_GE$gameto_pm[BF_GE$gameto_pm > 0 ] <- 1
BF_GE$gameto_po[BF_GE$gameto_po > 0 ] <- 1
BF_GE <- BF_GE %>% mutate_at(.vars=c(10,14,18), .funs=as.logical)



# créer code village et menage
BF_GE$codemenage <- str_sub(BF_GE$codeindividu,1,6)
BF_GE$codevillage <- str_sub(BF_GE$codeindividu,1,3)

#table(BF_GE$codevillage, BF_GE$codenquete)

# créer code village (va etre modifié ulterieurement)
CI_GE$codevillage <- str_sub(CI_GE$codeindividu,1,3)

## update codevillage in CI
# a parti de l'enquete 4, le code NAv correspond au nouveau village NAM (Namasselikaha)
# a parti de l'enquete 4, le code KOL correspond au nouveau village BLA (Blahouara)
# le code de KOU est modifié en KON pour éviter les homonymies avec le BF
# le code de NAV est modifié en NAA pour éviter les homonymies avec le BF

CI_GE$codeindividu <- with(CI_GE, if_else(str_detect(codeindividu, "NAA") & codenquete > 3, str_replace(codeindividu, "NAA", "NAM"), codeindividu))
CI_GE$codeindividu <- with(CI_GE, if_else(str_detect(codeindividu, "KOL") & codenquete > 3, str_replace(codeindividu, "KOL", "BLA"), codeindividu))

# updater code village et créer menage
CI_GE$codemenage <- str_sub(CI_GE$codeindividu,1,6)
CI_GE$codevillage <- str_sub(CI_GE$codeindividu,1,3)
table(CI_GE$codevillage, CI_GE$codenquete)


##### stats sur les table act pour extraire age median et sexe des individus
BF_act$sexe <- as.factor(BF_act$sexe)
CI_act$sexe <- as.factor(CI_act$sexe)
BF_act$age <- as.numeric(BF_act$age)
CI_act$age <- as.numeric(CI_act$age)

BF_ind_act <- BF_act %>% group_by(codeindividu) %>% summarise(age_med = median(age), sex = median(as.numeric(sexe)))
CI_ind_act <- CI_act %>% group_by(codeindividu) %>% summarise(age_med = median(age), sex = median(as.numeric(sexe)))
BF_ind_act$sex_med <- BF_ind_act$sex - 1 # recode to 0 = F and 1 = M (for modelling purpose)
CI_ind_act$sex_med <- CI_ind_act$sex - 1 # recode to 0 = F and 1 = M (for modelling purpose)
BF_ind_act$sex <- CI_ind_act$sex <- NULL

### lier la table GE aux données d'âge et de sexe (CI):
CI_GE_stats <- left_join(CI_GE, CI_ind_act, by = "codeindividu")
BF_GE_stats <- left_join(BF_GE, BF_ind_act, by = "codeindividu")

### binding BF & CI tables
BF_GE_stats$country <- "BF"
CI_GE_stats$country <- "CI"
GE_stats <- bind_rows(BF_GE_stats, CI_GE_stats)

# create a column prev (prevalence for falciparum, binomial) based on parasite density
GE_stats <- GE_stats %>% mutate(prev = tropho_pf > 1) %>%
	arrange(codeindividu,codenquete)
#GE_stats$codenquete <- as.factor(GE_stats$codenquete)
#GE_stats$codeindividu <- as.factor(GE_stats$codeindividu)
GE_stats$datelecture<-as.character(GE_stats$datelecture)


act$date <- BF_act$date <- CI_act$date <- NULL

### Save output files ----
#write.csv(act, file="act_tight.csv", row.names = FALSE)
#write.csv(BF_act, file="BF_act.csv", row.names = FALSE)
#write.csv(CI_act, file="CI_act.csv", row.names = FALSE)
#write.csv(pas, file="pas.csv", row.names = FALSE)
#write.csv(GE_stats, file="GE_stats.csv", row.names = FALSE)
