library(glmmTMB)
library(GGally)
library(tidyverse)
library(ggplot2)

####################################
######### glmm pour l'abondance ######### 
####################################

## chargement des données
df_abundance <- read.csv("/home/ptaconet/abundance_bf_funestus.csv")

## séléction des variables réponses uniquement
var_model <- setdiff(colnames(df_abundance),c("nummission","codevillage","pointdecapture","date_capture","X","Y"))

## ggpairs pour voir distributions statistiques
pm1 <- ggpairs(df_abundance[,var_model], columns = c(1,2:5), upper = "blank") + theme_bw()
pm1
pm2 <- ggpairs(df_abundance[,var_model], columns = c(1,6:10), upper = "blank") + theme_bw()
pm2


## question : faut-il transformer nos variables, eg centrer-réduire ?? faut-il discrétiser la variable réponse en quartiles ?

## GLMM fitting
# distribution négative binomiale, on utilise un GLMM avec une fonction de lien nbinom2(link = "log")
# de plus, on a des mesures répétées dans l'espace et dans le temps. on néglige l'autocorrelation spatiale car les mesures sont relativement éloignées dans le temps, mais on prend en compte l'autocorrélation temporelle en ajoutant un effet aléatoire imbriqué (1|codevillage/pointdecapture)

# ci dessous : un GLMM avec (1|codevillage/pointdecapture) en effet mixte, en intégrant uniquement 1 variable uniquement (rainfall)
glm3 <- glmmTMB(resp_var ~ RFD1_F_2000_8_12 + (1|codevillage/pointdecapture), data = df_abundance, family = nbinom2(link = "log"))
summary(glm3)


#############################################
######### glmm for presence / absence #########
#############################################

## chargement des données
df_presence <- read.csv("/home/ptaconet/presence_bf_funestus.csv" )

var_model <- setdiff(colnames(df_presence),c("nummission","codevillage","pointdecapture","date_capture","X","Y"))

## ggpairs pour voir distributions statistiques


## GLMM fitting
# question : la grande différence entre le nb d'observations pour chacune des 2 classes pose-t-elle un pb ?
glm4 <- glmmTMB(resp_var ~ RFD1_F_2000_21_22 + (1|codevillage/pointdecapture), data = df_presence, family = binomial(link = "logit"))
summary(glm4)




