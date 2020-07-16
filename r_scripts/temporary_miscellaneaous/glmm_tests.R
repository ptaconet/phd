library(glmmTMB)
library(buildmer)
library(GGally)
library(tidyverse)
library(ggplot2)
library(car)
library(DHARMa)
####################################
######### glmm pour l'abondance ######### 
####################################

## chargement des données
df_abundance <- read.csv("/home/ptaconet/abundance_bf_funestus.csv")

### univariate selection

# make a dataframe with the variables names to be used in the model, will also receive the results
dep_var <- colnames(df_abundance)[1] # dependent variable (to explain)
ind_vars <- colnames(df_abundance)[5:ncol(df_abundance)] # independent variables (explanatory)
ind_vars <- setdiff(ind_vars,c("X_32630","Y_32630","int_ext","VCP"))
results <- expand_grid(dep_var,ind_vars)

# function that return the p-value of the variable
p_value <- function(dep_var, ind_var, data){
  formule <- as.formula(paste(dep_var," ~ ", ind_var, "+ (1|codevillage/pointdecapture)")) # write the formula for the model
  res <- data %>%
    glmmTMB::glmmTMB(formule, data = ., family = nbinom2(link = "log")) %>% # fit the model
    Anova() %>% # test the significance of the variable
    nth(3) # get the p-value
  return(res)
}

# fill with the results (i.e the p_value from the model test )
results <- results %>% 
  mutate(pvalue = map_dbl(ind_vars, ~p_value(dep_var, ., df_abundance))) %>% 
  filter(pvalue < 0.05)

var_to_keep <- c(results$ind_vars,"int_ext","VCM")

df_abundance <- df_abundance[,c("resp_var","codevillage","pointdecapture",var_to_keep)]

## ggpairs pour voir distributions statistiques
# pm1 <- ggpairs(df_abundance[,var_model], columns = c(1,2:5), upper = "blank") + theme_bw()
# pm1
# pm2 <- ggpairs(df_abundance[,var_model], columns = c(1,6:10), upper = "blank") + theme_bw()
# pm2


## question : faut-il transformer nos variables, eg centrer-réduire ?? -> réponse Angélique : pas la peine car on est en negatif binomial

## faut-il discrétiser la variable réponse en quartiles ? -> 

## GLMM fitting
# distribution négative binomiale, on utilise un GLMM avec une fonction de lien nbinom2(link = "log")
# de plus, on a des mesures répétées dans l'espace et dans le temps. on néglige l'autocorrelation spatiale car les mesures sont relativement éloignées dans le temps, mais on prend en compte l'autocorrélation temporelle en ajoutant un effet aléatoire imbriqué (1|codevillage/pointdecapture)

# ci dessous : un GLMM avec (1|codevillage/pointdecapture) en effet mixte, en intégrant uniquement 1 variable uniquement (rainfall)


form <- as.formula(paste("resp_var ~ ",paste(var_to_keep, collapse = "+"), "+ (1|codevillage/pointdecapture)"))

mod <- buildglmmTMB(form, data = df_abundance, family = nbinom2(link = "log"))

summary(mod)


summary(glm3)
Anova(glm3)# quelle covar est signif 

emmeans(glm3, "RFD1_F_2000_8_12")# a ameliorer 

# validation des résidus du modèle. (package DHARMa). 
res <- simulateResiduals(glm3)
plot(res)# je suis dubitative ... il me semblent bon ces résidus, mais la ligne est décallé. 

resc <- residuals(glm3)
plot(resc)

resb <- simulateResiduals(glm3)
plot(resb)


# pour savoir si une variable ou une intéraction est significative : on fait Anova(glm3) et on regarde la p-value
# on peut aussi utiliser emmeans 



#############################################
######### glmm for presence / absence #########
#############################################

## chargement des données
df_presence <- read.csv("/home/ptaconet/presence_bf_funestus.csv")


# make a dataframe with the variables names to be used in the model, will also receive the results
dep_var <- colnames(df_presence)[1] # dependent variable (to explain)
ind_vars <- colnames(df_presence)[5:ncol(df_presence)] # independent variables (explanatory)
ind_vars <- setdiff(ind_vars,c("X_32630","Y_32630","int_ext","VCP","VCM"))
results <- expand_grid(dep_var,ind_vars)

# function that return the p-value of the variable
p_value <- function(dep_var, ind_var, data){
  formule <- as.formula(paste(dep_var," ~ ", ind_var, "+ (1|codevillage/pointdecapture)")) # write the formula for the model
  res <- data %>%
      glmmTMB::glmmTMB(formule, data = ., family = binomial(link = "logit")) %>% # fit the model
    Anova() %>% # test the significance of the variable
    nth(3) # get the p-value
  return(res)
}

# fill with the results (i.e the p_value from the model test )
results <- results %>% 
  mutate(pvalue = map_dbl(ind_vars, ~p_value(dep_var, ., df_presence))) %>% 
  filter(pvalue < 0.05)

var_to_keep <- c(results$ind_vars,"int_ext","VCM")

df_presence <- df_presence[,c("resp_var","codevillage","pointdecapture",var_to_keep)]

## ggpairs pour voir distributions statistiques
# pm1 <- ggpairs(df_presence[,var_model], columns = c(1,2:5), upper = "blank") + theme_bw()
# pm1

form <- as.formula(paste("resp_var ~ ",paste(var_to_keep, collapse = "+"), "+ (1|codevillage/pointdecapture)"))

mod <- buildglmmTMB(form, data = df_presence, family = binomial(link = "logit"))

summary(mod)
res <- simulateResiduals(mod)
plot(res)




