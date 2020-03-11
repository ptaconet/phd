larvicide_ci_ctrlequalite <- read_excel("data/react_db/miscellaneous_data/Controle qualité Bti.xlsx", col_types = "text")
colnames(larvicide_ci_ctrlequalite) <- c("date_ttmt","date_prlvt","codevillage","numero_echantillon","n_total","n_morts_24h","mortalite_24h","n_morts_48h","mortalite_48h")
larvicide_ci_ctrlequalite$mortalite_24h <- larvicide_ci_ctrlequalite$mortalite_48h <- NULL

larvicide_ci_ctrlequalite$date_ttmt[-(str_which(larvicide_ci_ctrlequalite$date_ttmt,"[:digit:]{5}"))] <- c("2018-02-31","2018-02-31","2018-02-31")

larvicide_ci_ctrlequalite$date_ttmt[str_which(larvicide_ci_ctrlequalite$date_ttmt,"[:digit:]{5}")] <- larvicide_ci_ctrlequalite$date_ttmt[str_which(larvicide_ci_ctrlequalite$date_ttmt,"[:digit:]{5}")] %>%
  as.numeric() %>%
  as_date(origin = ymd("1899-12-30"))  %>%
  as.character()

larvicide_ci_ctrlequalite$date_prlvt[-(str_which(larvicide_ci_ctrlequalite$date_prlvt,"[:digit:]{5}"))] <- "2018-10-20"

larvicide_ci_ctrlequalite$date_prlvt[str_which(larvicide_ci_ctrlequalite$date_prlvt,"[:digit:]{5}")] <- larvicide_ci_ctrlequalite$date_prlvt[str_which(larvicide_ci_ctrlequalite$date_prlvt,"[:digit:]{5}")] %>%
  as.numeric() %>%
  as_date(origin = ymd("1899-12-30"))  %>%
  as.character()
