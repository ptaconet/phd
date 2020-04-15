entomo_gites_larvaires_ci_l0 <- readxl::read_excel("data/react_db/miscellaneous_data/DonneesGitesLarvaires_raw_ 20200227.xlsx")
entomo_gites_larvaires_ci_l0$datenquete <- as.character(as.Date(entomo_gites_larvaires_ci_l0$datenquete))
entomo_gites_larvaires_ci_l0$heurenquete <- strftime(entomo_gites_larvaires_ci_l0$heurenquete, format="%H:%M:%S")
colnames(entomo_gites_larvaires_ci_l0) <- tolower(colnames(entomo_gites_larvaires_ci_l0))