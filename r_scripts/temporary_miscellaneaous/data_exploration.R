require(RSQLite)
require(tidyverse)
require(purrr)

# compte de vecteurs capturés / habitation
path_to_db<-"/home/ptaconet/react/datasets/react_db.gpkg"

con <- dbConnect(RSQLite::SQLite(),path_to_db)

df_mosquitoes<-dbReadTable(con,"mosquitoes")

df_catchpoints<-dbReadTable(con,"hlc_dates_loc_times")

nb_mosq_per_catchpoint<-df_mosquitoes %>%
  group_by(idpointdecapture) %>%
  summarise(n_mosq=n()) %>%
  full_join(df_catchpoints,by="idpointdecapture") %>%
  mutate(n_mosq=replace_na(n_mosq,0))

# group by country and make 2 lists (1 per country)
list_mosq_by_country=nb_mosq_per_catchpoint %>%
  group_by(codepays) %>%
  nest() %>%
  pull()

# create function to visualize histograms
fun_barplot_ggplot<-function(df){
  p<-ggplot(df,aes(x=n_mosq)) +
    geom_histogram(color="black", fill="white",binwidth=1)
  return(p)
}

# generate 2 histograms of counts of mosquitoes / setllement (1 for each country)
hist_nmosq_by_country<-nested_mosq_by_country %>%
  map(~fun_barplot_ggplot(.))

