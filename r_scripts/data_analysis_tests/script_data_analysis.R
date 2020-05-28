library(tidyverse)
library(ncf)
library(glmmTMB)
library(performance)
library(GGally)
library(googlesheets4)
library(correlation)
library(glmertree)

## source functions
source("functions_script_data_analysis.R")

setwd("/home/ptaconet/Bureau/data_analysis")

## get datasets
df_resp <- readRDS("resp_var.rds")  # response variable (ma_gambiae_ss OR ma_funestus_ss OR ma_coluzzi) + metadata (codevillage, date, point de capture etc.)
expl_spatiotemporal_list <- readRDS("expl_var_spatiotemporal.rds")  # list of time related explanatory variables. each element of the list is an explanatory variable (rainfall, soil moisture, etc.) extracted up to 120 days prior to the night of catch
expl_landcover <- readRDS("expl_var_landcover.rds")  # landcover related expl. var
expl_nightcatch <- readRDS("expl_var_nightcatch.rds")    # expl. var for the night of catch (derived from sat. data) (rainfall, wind, etc)
expl_nightcatch_2 <- readRDS("expl_var_nightcatch_2.rds")  # expl. var for the night of catch (derived from micro climate in-situ sensors)
expl_spatial <- readRDS("expl_var_spatial.rds")   # spatial-only expl. var (altitude, etc.)
expl_static <- readRDS("expl_var_static.rds")  # non temporal and non-spatial expl. var (implementation of LAV, etc.)
googlesheets4::sheets_deauth()
expl_metadata <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_explication", col_types="c")


### Some EDA for the response variable (resp. var is ma_gambiae_ss, i.e. number of bites by Anopheles gambiae ss)

## frequency of bites
table(df_resp$ma_gambiae_ss)
hist(df_resp$ma_gambiae_ss) 
# -> data is highly right skewed (negative binomial), with many zeros. high probability of zero inflation.
## boxplot of the number of bites for each mission :
boxplot(ma_gambiae_ss~nummission, df_resp)
# -> within some missions we see high variance (e.g. 11, 12, 13), other less (e.g. 15, 5, 6) . Let's do the same plot with only positive bites : 
boxplot(ma_gambiae_ss~nummission, df_resp %>% filter(ma_gambiae_ss > 0))

## is there spatial autocorrelation in the resp. variable ? (caution : takes long to run)
correlog_resp <- ncf::spline.correlog(x = df_resp$X,
                                 y = df_resp$Y,
                                 z = df_resp$ma_gambiae_ss)
plot(correlog_resp)
# -> yes, there is spatial autocorrelation since at short distances (< 4 km) the line is above 0.

## is there zero inflation ? 
# let's compare a ZIP and a ZINB GLMM model with one of the explanatory variables (here: water accumulation on a 2km wide buffer)
df <- df_resp %>% left_join(expl_spatial) %>% mutate(WAC_cr = scale(WAC))
glm1 <- glmmTMB(ma_gambiae_ss ~ WAC_cr + (1|codevillage/pointdecapture), data = df, family = nbinom2(link = "log")) # fit the model 
glm3 <- glmmTMB(ma_gambiae_ss ~ WAC_cr + (1|codevillage/pointdecapture), data = df, family = nbinom2(link = "log"), ziformula = ~.)
# compare models with and without zero-inflation
anova(glm1, glm3) # AIC of glm3 is lower, p-value is < 0.05, i.e. glm3 is better, there is zero-inflation !


## create a column with presence-absence of bites of ma_gambiae_ss
df_resp <- df_resp %>% mutate(ma_gambiae_ss_bin = if_else(ma_gambiae_ss==0, FALSE, TRUE))


## Les principales difficultés concernant nos données :
# i) elles sont auto-correlées spatialement
# ii) les observations ne sont pas indépendantes, i.e. les données sont hiérarchisées (codevillage/pointdecapture)
# iii) elles sont surdispersées (distribution asymétrique binomiale négative)
# iv) elles sont zero-inflated
# v) le nombre de comptes nuls (absence de piqure) est largement supérieur au nombre de comptes positifs (on est dans un cas d' "imbalanced data")
# vi) il y a un nombre important de variables explicatives pour chaque variable à expliquer
# vii) les intéractions entre les variables réponses et les variables explicatives ont de fortes chances d'être complexes, non-linéaires, non-monotones, etc.
# viii) il y a un nombre important de variables à expliquer

## Comment gérer chacune de ces problématiques ? 
# pour l'autocorellation spatiale et la non - indépendance des observations (points i) et ii) ) : 
  # - avec des modèles paramétriques : introduction d'effets aléatoires (GLMM, GAMM, etc.)
  # - avec des modèles algorithmiques : introduction d'effets aléatoires (Mixed-effects random forest, Generalized Linear Mixed Model Trees, etc.)
  # - avec des modèles algorithmiques : introduction de variables spatiales ("Random Forest for spatial data") type coordonnées géographiques, distances aux points de capture, distance à des coordonnées fixes de la zone d'étude, etc.
# pour la surdispersion, le caractère zéro-inflated (point iii) et iv) ): 
  # - avec des modèles paramétriques : modèle de haie, modèle ZINB
  # - avec des modèles algorithmiques : en théorie, les modèles algorithmiques (non paramétriques) sont insensibles à la distribution à priori des données. En pratique, il est possible (recommandé) d'utiliser le principe du modèle de haie (1 modèle pour les comptes nuls vs positifs, un modèle pour les comptes positifs uniquement)
# pour le déséquilibre entre les comptes nuls et les comptes positifs (point v) :
  # - avec des modèles paramétriques : géré naturellement dans les modèles de haie
  # - avec des modèles algorithmiques : rééchantilloner la classe minoritaire avec des méthode de rééchantillonage type SMOTE
# pour le nombre important de variables explicatives (point vi) ) :
  # - filter les variables à insérer dans le modèle multivarié, par séléction supervisée en analyse univariée au préalable
# pour la complexité des intéractions et la non-linéarité ou non-monotonie des relations :
  # - utiliser un modèle algorithmique (type random forest, gradient boosting trees, etc.)
# pour le nombre important de variables à expliquer (point viii) ) :
  # - utiliser des méthodes aussi rapides que possibles

# On voit donc qu'on a de nombreuses contraintes dans ce travail de modélisation. Le challenge est de définir une méthode de modélisation qui soit un compromis permettant de prendre en compte toutes ces contraintes, tout en étant scientifiquement rigoureuse.

## ici nous proposons la méthode suivante : 
# - modéliser séparement la présence/absence et les comptes positifs de piqures (1 modèle présence/absence et 1 modèle abondance), comme dans un modèle de haie. Si possible traiter les deux modèles comme deux processus bien séparés (notamment en terme de séléction supervisée des variables à inclure dans le modèle multivarié, car l'hyopthèse est que les processus biologiques conduisant à la présence des vecteurs ne sont pas nécéssairement les mêmes que ceux conduisant à leur abondance)
# - utiliser dans les deux cas des modèles algorithmiques pour la modélisation multivariée (random forest en l'occurence) : modèle de classification pour la présence/absence et modèle de régression pour l'abondance (-> à voir si ça marche). Ces modèles sont reconnus pour gérer efficacement un nombre important de variables explicatives + les relations non-linéaires, non-monotones, etc.
# - pour le modèle de présence / absence, rééchantilloner les données (pour rééquilibrer les classes) en augmentant le nombre d'observations de la classe minoritaire avec la méthode de rééchantillonage SMOTE
# - dans les deux cas, effectuer une pré-séléction semi-supervisée des variables à inclure dans le modèle multivarié final afin de limiter le nombre de variables explicatives.

###############################
## Challenge n°1 : définir une méthode permettant de filtrer, pour chacun des deux modèles (présence/absence et abondance), les variables spatiotemporelles et les variables d'occupation du sol à intégrer dans les modèles multivariés (plusieurs dizaines de milliers de variables potentielles si on ne filtre pas) 
## Challenge n°2 : utiliser, en analyse multivariée, un type de modèle (random forest) qui normalement requiert l'indépendance des observations (i.e. ne gère pas l'autocorrelation spatiale, l'aspect hiérarchique des observations, etc). De nombreux travaux récents proposent cependant des méthodes pour tenter de prendre en considération ces éléments dans ces modèles (mixed effects random forest, Random Forest for spatial data, Generalized Linear Mixed Model Trees, etc.) 
###############################

## L'interpétation des modèles "black box" type random forest ne devrait pas être un problème : de nombreux outils d'intérprétation de ces modèles existent et sont bien documentés : variables d'importance, partial dependence, plots, etc. Voir : https://christophm.github.io/interpretable-ml-book/pdp.html


###########################
####### univariate analysis 
###########################

## The main objectives of the univariate analysis are to : 
# i) explore univariate relationship between resp. and expl. variables ,
# ii) filter the features to include in the multivariate model

####### ####### ####### 
####### for time related explanatory variables
####### ####### ####### 

## Pour les données spatio-temporelles, on a collecté, pour chaque nuit de capture, les données allant jusqu'à 120 jours avant la nuit de capture pour chaque poste de capture.
## Pour séléctionner la variable à insérer dans le modèle multivarié, on génère des cross-correlation maps (CCM). L'idée des CCM est de considérer différents pas de temps pour la variable explicative temporelle. On conservera pour les modèles multivariés la variable présentant la corrélation la plus importante avec la variable réponse. 
## problème 1) : comment évaluer la corrélation entre la variable à expliquer et la variable explicative laggée ? En théorie il faut utiliser une méthode qui prend en compte l'auto-corrélation spatiale, la surdispersion, les éventuelles non-linéarités et non-monotonies, etc. 
## problème 2) : pour chaque variable explicative, nous pouvons avoir jusqu'à environ 7500 tests à effectuer (i.e. pour les variables à résolution temporelle journalière, receuillies jusqu'à 120 jours avant la date de capture). Il faut donc une méthode relativement rapide

### ici nous proposons les méthodes suivantes pour générer les CCMs :
## pour les comptes positifs, de générer 2 CCM pour chaque variable explicative, basées sur 2 méthodes différentes : 
# - méthode 1 : coefficient de corrélation de Spearman avec un effet aléatoire au niveau du village (voir la fonction correlation::correlation()). 
# pros : takes into account non-linearity + gives the direction of the relationship (positive or negative) + we set-up a random effects on villages using the argument multilevel = TRUE in the function correlation::correlation()
# cons : does not take into account non-monotony
# - méthode 2 : coefficient de corrélation de distance (voir la fonction correlation::correlation())
# pros : takes into account non-linearity + non-monotony (see https://en.wikipedia.org/wiki/Distance_correlation)
# cons : does not gives the direction of the relationship + impossibility to insert random effects (as for Spearman) as the function did not work...
## nous conserverons pour la suite des analyses les variables laggées présentant les coefficients de corrélation les plus important (en valeur absolue) selon chacune des deux méthodes.
# une 3è méthode est détaillée en dessous (basée sur les Generalized linear mixed-effects model trees). Cette méthode est intéressante mais longue à tourner...

env_spatiotemporal2 <- expl_spatiotemporal_list %>%
  enframe(name = "var", value = "predictive_df") %>%
  mutate(buffer = 2000, codepays = "BF") %>%
  mutate(fun_summarize_ccm = ifelse(var %in% c("RFD1_F","RFD1_L","RFD7_F","RFD7_L"), "sum", "mean")) 

####### generate the CCMs

## for positive counts : CCM using spearman correlation with random effect on village (caution: takes 30 min to run...)
env_spatiotemporal2 <- env_spatiotemporal2 %>%
  mutate(ccm_corrmat_sup0_spearman = map(predictive_df, ~fun_ccm_corrmat_sup0(., df_resp, "ma_gambiae_ss", "spearman"))) %>% # function "fun_ccm_corrmat_sup0" creates the CCM by using the spearman correlation coefficient 
  mutate(ccm_plot_sup0_spearman = pmap(list(ccm_corrmat_sup0_spearman, var, buffer, codepays), ~fun_ccm_plot(..1,..2,..3,..4))) %>% # function "fun_ccm_plot" plots the CCM
  mutate(ccm_maxcorr_vcor_spearman = map_dbl(ccm_corrmat_sup0_spearman, function(x) ifelse( !all(is.na(x$correlation)), x$correlation[which.max(abs(x$abs_corr))], NA))) %>% # get max correlation value (wether positive or negative) for the CCM
  mutate(ccm_maxcorr_lag1_spearman = map_dbl(ccm_corrmat_sup0_spearman, function(x) ifelse( !all(is.na(x$correlation)), x$time_lag_1[which.max(abs(x$abs_corr))], NA))) %>% # get time lag 1 for max correlation value
  mutate(ccm_maxcorr_lag2_spearman = map_dbl(ccm_corrmat_sup0_spearman, function(x) ifelse( !all(is.na(x$correlation)), x$time_lag_2[which.max(abs(x$abs_corr))], NA))) %>% # get time lag 2 for max correlation value
  mutate(var_df_spearman = paste(var,buffer,ccm_maxcorr_lag2_spearman,ccm_maxcorr_lag1_spearman,sep="_")) # get name of the variable for the max correlation value

## for positive counts : CCM using distance correlation (around 15 min to run)
env_spatiotemporal2 <- env_spatiotemporal2 %>%
  mutate(ccm_corrmat_sup0_distance = map(predictive_df, ~fun_ccm_corrmat_sup0(., df_resp, "ma_gambiae_ss", "distance"))) %>% # function "fun_ccm_corrmat_sup0" creates the CCM by using the spearman correlation coefficient 
  mutate(ccm_plot_sup0_distance = pmap(list(ccm_corrmat_sup0_distance, var, buffer, codepays), ~fun_ccm_plot(..1,..2,..3,..4))) %>% # function "fun_ccm_plot" plots the CCM
  mutate(ccm_maxcorr_vcor_distance = map_dbl(ccm_corrmat_sup0_distance, function(x) ifelse( !all(is.na(x$correlation)), x$correlation[which.max(abs(x$abs_corr))], NA))) %>% # get max correlation value (wether positive or negative) for the CCM
  mutate(ccm_maxcorr_lag1_distance = map_dbl(ccm_corrmat_sup0_distance, function(x) ifelse( !all(is.na(x$correlation)), x$time_lag_1[which.max(abs(x$abs_corr))], NA))) %>% # get time lag 1 for max correlation value
  mutate(ccm_maxcorr_lag2_distance = map_dbl(ccm_corrmat_sup0_distance, function(x) ifelse( !all(is.na(x$correlation)), x$time_lag_2[which.max(abs(x$abs_corr))], NA))) %>% # get time lag 2 for max correlation value
  mutate(var_df_distance = paste(var,buffer,ccm_maxcorr_lag2_distance,ccm_maxcorr_lag1_distance,sep="_")) # get name of the variable for the max correlation value

## Une autre option pourrait être d'utiliser un Generalized linear mixed-effects model trees (GLMM trees or glmertrees) (voir package glmertree, vignette("glmertree") et article https://link.springer.com/article/10.3758/s13428-017-0971-x) . 
# Un glmertree combine les avantages des méthodes basées sur les arbres de décision (détéction de relation non-linéaires, détéction des intéractions complexes etc.) et les GLMM (introduction d'effets aléatoires).
# cette méthode présente aussi l'avantage de gérer à la fois les données binaires (présence/absence) et les données de comptages.
# exemple : on applique un glmertree pour ma_gambiae_ss et la variable explicative RFD1_F_2000_0_46, en spécifiant les effets aléatoires (1|codevillage/pointdecapture) et fixes (ici on a choisi RFD1_F_2000_0_46) :

df_count <- env_spatiotemporal2$predictive_df[[3]] %>% left_join(df_resp) %>% filter(ma_gambiae_ss > 0)
glmt <- glmertree(ma_gambiae_ss ~ 1 | (1|codevillage/pointdecapture) | RFD1_F_2000_0_46, data = df_count, family=poisson(link = "log")) # only poisson is available for count data...
plot(glmt)  # on a 3 graphiques : l'arbre de décision (plot 1) et les deux niveaux d'effets aléatoires (plot 2 et 3) (pointdecapture:codevillage et codevillage)
# on prédit les nouvelles valeurs (il faudrait évidemment utiliser un jeu de données indépendant mais ici pour l'exemple on prédit sur le même jeu de données)
df_count$pred_glmtree <- predict(glmt)
cor(df_count$ma_gambiae_ss,df_count$pred_glmtree)  # 0.92

# glmertree pour la présence / absence de ma_gambiae_ss, même variable explicative
df_pres <- env_spatiotemporal2$predictive_df[[3]] %>% left_join(df_resp) %>% mutate(ma_gambiae_ss_bin=as.factor(ma_gambiae_ss_bin))
glmt_pres <- glmertree(ma_gambiae_ss_bin ~ 1 | (1|codevillage/pointdecapture) | RFD1_F_2000_0_46, data = df_pres, family="binomial")
plot(glmt_pres)
df_pres$pred_glmtree <- predict(glmt_pres) # ici on prédit non pas la classe mais la probabilité d'appartenir à TRUE (présence) ou FALSE (absence)
boxplot(df_pres$pred_glmtree~df_pres$ma_gambiae_ss_bin) # globalement les prédictions sont bonnes

# petit bémol : ces modèles sont relativement long à tourner et il semble compliqué de les faire tourner sur toutes les combinaisons temporelles des CCM...
# autre question : comment évaluer la performance d'un tel modèle (ie la valeur à représenter dans la CCM) ? Cela pourrait être par ex. le coeff de correlation de pearson entre les valeurs réelles et les valeurs prédites. 
# si on ne retient pas cette méthode (car trop longue..) il pourrait néanmoins être intéressant de la tester pour le modèle multivarié final


# plot the CCM for both Spearman and distance correlation :
# here we choose to plot the CCM for the rainfall (var=="RFD1_F"). 
# possible_vars = unique(env_spatiotemporal2$var)
env_spatiotemporal2$ccm_plot_sup0_spearman[[which(env_spatiotemporal2$var=="RFD1_F")]]
env_spatiotemporal2$ccm_plot_sup0_distance[[which(env_spatiotemporal2$var=="RFD1_F")]]

# red framed dot is the highest correlation coeff (either positive or negative). black framed dots are the top 3% highest correlation coeffs. grey dots are when the p-value of the spearman coefficient is not significant (>0.05)
# so here for example here the highest coefficient is 0.57 at a time lag r(42,17) for spearman correlation, meaning that : the sum of the rainfall between 17 days and 42 days before the HLC misssion is best correlated (using Spearman correlation coefficient) with the positive counts of An. gambiae ss in BF.
# for the distance correlation, it's 0.57 at time lag r(18,16)
# so for the the multivariate analysis we would keep both variables for the rainfall

############ pour la présence / absence, comment faire pour séléctionner les variables à retenir ? on ne peut pas utiliser de coeff de correlation car la réponse est binomiale. 
# on pourrait utiliser les glmertree mais le temps de calcul au regard des différentes combinaisons de lags temporels semble prohibitif. 


####### ####### ####### 
###### for landcover variables
####### ####### ####### 

### Nous avons 2500 variables de land cover. Il nous faut donc ici aussi pré-séléctionner les variables à inclure dans le modèle multivarié. 

expl_landcover_pos <- df_resp %>% left_join(expl_landcover) %>% filter(ma_gambiae_ss > 0) %>%  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
expl_landcover_pres <- df_resp %>% left_join(expl_landcover) %>%  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))


####### ####### ####### 
###### for covariates covering the night of the catch 
####### ####### ####### 
# Ici il n'y a que 8 variables. Nous pouvons donc les conserver toutes pour le modèle multivarié. Le code suivant est à but principalement exploratoire.
expl_nightcatch_pos <- df_resp %>% left_join(expl_nightcatch) %>% left_join(expl_nightcatch_2) %>% dplyr::select(ma_gambiae_ss,RFH,WDR,WSP,LMN,NMT,NML,NMH,NDP) %>% filter(ma_gambiae_ss > 0)
expl_nightcatch_pres <- df_resp %>% left_join(expl_nightcatch) %>% left_join(expl_nightcatch_2) %>% dplyr::select(ma_gambiae_ss_bin,RFH,WDR,WSP,LMN,NMT,NML,NMH,NDP) 

# presence / absence (variables names can be retrieved in the dataframe expl_metadata)
ggpairs(expl_nightcatch_pres, upper = list(continuous = wrap("cor", method = "spearman")))
# positive counts
ggpairs(expl_nightcatch_pos, upper = list(continuous = wrap("cor", method = "spearman")))


####### ####### ####### 
###### for spatial-only data
####### ####### ####### 
# Ici il n'y a que 10 variables. Nous pouvons donc les conserver toutes pour le modèle multivarié. Le code suivant est à but principalement exploratoire.
expl_spatial_pos <- df_resp %>% left_join(expl_spatial) %>% dplyr::select(ma_gambiae_ss,TEL,TSL,TAS,WAC,TCI,TWI,WAD,WLS,WAL,HYS) %>% filter(ma_gambiae_ss>0)
expl_spatial_pres <-  df_resp %>% left_join(expl_spatial) %>% dplyr::select(ma_gambiae_ss_bin,TEL,TSL,TAS,WAC,TCI,TWI,WAD,WLS,WAL,HYS)

# presence / absence (variables names can be retrieved in the dataframe expl_metadata)
ggpairs(expl_spatial_pres, upper = list(continuous = wrap("cor", method = "spearman")))
# positive counts
ggpairs(expl_spatial_pos, upper = list(continuous = wrap("cor", method = "spearman")))


####### ####### ####### 
###### for static variables
####### ####### ####### 
# Ici il n'y a que 5 variables. Nous pouvons donc les conserver toutes pour le modèle multivarié. Le code suivant est à but principalement exploratoire.
expl_static_pos <- df_resp %>% left_join(expl_static) %>% dplyr::select(ma_gambiae_ss,WMD,BDE,VCP,VCM,VCT) %>% filter(ma_gambiae_ss>0)
expl_static_pres <-  df_resp %>% left_join(expl_static) %>% dplyr::select(ma_gambiae_ss_bin,WMD,BDE,VCP,VCM,VCT)

# presence / absence (variables names can be retrieved in the dataframe expl_metadata)
ggpairs(expl_static_pres, upper = list(continuous = wrap("cor", method = "spearman")))
# positive counts
ggpairs(expl_static_pos, upper = list(continuous = wrap("cor", method = "spearman")))


###########################
####### exploring multicollinearity among the response variables
###########################
# TODO



###########################
####### Multivariate modeling
###########################
# TODO


###########################
####### Model interpretation
###########################
# TODO