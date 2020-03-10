# load libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(stringdist)
library(stringi)

# load household data
household_BF <- read_excel("miscellaneous_data/DonnéesREACT/BF/recensement/Menagebf_niv1.xlsx", na=c("","NA"), col_types = "text") %>% as.data.frame()
household_CI <- read_excel("miscellaneous_data/DonnéesREACT/CI/recensement/MénageCI_niv1.xlsx", na=c("","NA"), col_types = "text") %>% as.data.frame()

## update codevillage in CI
# le code de KOU est modifié en KON pour éviter les homonymies avec le BF
# le code de NAV (enquete 1 à 3) est modifié en NAA pour éviter les homonymies avec le BF
household_CI$codevillage <- with(household_CI, if_else(str_detect(codevillage, "KOU") , str_replace(codevillage, "KOU", "KON"), codevillage))
household_CI$codevillage <- with(household_CI, if_else(str_detect(codevillage, "NAV") , str_replace(codevillage, "NAV", "NAA"), codevillage))
household_CI$codemenage <- with(household_CI, if_else(str_detect(codemenage, "KOU") , str_replace(codemenage, "KOU", "KON"), codemenage))
household_CI$codemenage <- with(household_CI, if_else(str_detect(codemenage, "NAV") , str_replace(codemenage, "NAV", "NAA"), codemenage))
## update codevillage in BF
# le code de KOL est modifié en KLK pour éviter les homonymies avec le CI
household_BF$codevillage <- with(household_BF, if_else(str_detect(codevillage, "KOL") , str_replace(codevillage, "KOL", "KLK"), codevillage))
household_BF$codemenage <- with(household_BF, if_else(str_detect(codemenage, "KOL") , str_replace(codemenage, "KOL", "KLK"), codemenage))

# change column names for correspondance between BF and CI
names(household_CI) <- gsub("Part12Part2","",names(household_CI))

# bind data
household_BF$codepays <- "BF"
household_CI$codepays <- "CI"
household <- bind_rows(household_BF,household_CI)

# change data types
household <- household %>% mutate_at(c(7:10,14:17,31:36,38:43,45:50), as.numeric)
household <- household %>% mutate_at(c(4,5,18:30,78), as.factor)
household$dateenq <- household$dateenq %>% as.numeric() %>% as_date(origin = ymd("1900-01-01"))
household <- household %>% filter(consentement=="OUI") # only 4 rows are NON
menages <- household
