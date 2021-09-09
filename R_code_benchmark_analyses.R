# The necessary .sav files for the analyses can be requested from stefan.heber@meduniwien.ac.at

# > sessionInfo()
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] DALEX_2.3.0         pROC_1.18.0         randomForest_4.6-14 stringr_1.4.0       haven_2.4.3        
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.1.1  remotes_2.2.0     purrr_0.3.4       lattice_0.20-41   colorspace_2.0-2  vctrs_0.3.8       generics_0.1.0   
# [8] testthat_3.0.0    usethis_1.6.3     yaml_2.2.1        utf8_1.2.2        rlang_0.4.11      pkgbuild_1.1.0    pillar_1.6.2     
# [15] glue_1.4.2        withr_2.4.2       DBI_1.1.0         sessioninfo_1.1.1 lifecycle_1.0.0   plyr_1.8.6        munsell_0.5.0    
# [22] gtable_0.3.0      devtools_2.3.2    memoise_1.1.0     labeling_0.4.2    forcats_0.5.1     callr_3.7.0       tzdb_0.1.2       
# [29] ps_1.4.0          fansi_0.5.0       Rcpp_1.0.7        xtable_1.8-4      readr_2.0.1       scales_1.1.1      desc_1.2.0       
# [36] pkgload_1.1.0     ingredients_2.2.0 farver_2.1.0      fs_1.5.0          ggplot2_3.3.5     hms_1.1.0         digest_0.6.27    
# [43] stringi_1.7.4     processx_3.5.2    dplyr_1.0.7       grid_4.0.3        rprojroot_2.0.2   cli_3.0.1         tools_4.0.3      
# [50] magrittr_2.0.1    tibble_3.1.4      crayon_1.4.1      pkgconfig_2.0.3   ellipsis_0.3.2    prettyunits_1.1.1 assertthat_0.2.1 
# [57] rstudioapi_0.13   R6_2.5.1          compiler_4.0.3 


#########################################################################################################################
### 1) CV

library(haven)
library(stringr)
library(randomForest)
library(pROC)

# Validation superset of variables including all ten blood-related predictors
vnames <- c("PLT_intercept2", "PLT_slope",
            "CRP_intercept2", "CRP_slope",
            "Creatinine_intercept2", "Creatinine_slope",
            "Lymphozyten_intercept2", "Lymphozyten_slope",      
            "LDH_intercept2", "LDH_slope", 
            "Age", "Fever_on_admission", "Death_yn")                
  
set.seed(42)
aucs  <- NULL
sizes <- NULL
for(i in 1:10){
  #i <- 1
  train       <- read_sav(paste0("Cross_sample_",i,".sav"))
  test        <- read_sav(paste0("Not_in__cross_sample_",i,".sav"))
  names(test) <- sapply(names(test), function(z) ifelse(str_sub(z, -4,-1) == "_est", str_sub(z,1,-5), z))
  
  train <- train[,names(train) %in% vnames]
  test  <- test[,names(test) %in% vnames]
  
  train <- as.data.frame(train)
  test <- as.data.frame(test)
  
  train$Fever_on_admission <- as.factor(train$Fever_on_admission)
  test$Fever_on_admission <- as.factor(test$Fever_on_admission)
  
  train$Death_yn <- as.factor(ifelse(train$Death_yn == 1, "yes", "no"))
  test$Death_yn <- as.factor(ifelse(test$Death_yn == 1, "yes", "no"))
  
  train <- train[complete.cases(train),]
  test  <- test[complete.cases(test),]
  
  rfmod  <- randomForest(Death_yn ~ ., train)
  predictions <- predict(rfmod, test, type = "prob")[,2]
  aucs <- c(aucs, auc(test$Death_yn, predictions, levels = c("no","yes"), direction = "<"))
  sizes <- rbind(sizes, c(nrow(train), nrow(test)))
}


# Validation on final selected variable subset
vnames <- c("Age",  "LDH_slope", "PLT_slope", "CRP_slope", "Fever_on_admission",
            "LDH_intercept2", "CRP_intercept2",  "Creatinine_intercept2", "Death_yn")

aucs2  <- NULL
sizes2 <- NULL
for(i in 1:10){
  train       <- read_sav(paste0("Cross_sample_",i,".sav"))
  test        <- read_sav(paste0("Not_in__cross_sample_",i,".sav"))
  names(test) <- sapply(names(test), function(z) ifelse(str_sub(z, -4,-1) == "_est", str_sub(z,1,-5), z))
  
  train <- train[,names(train) %in% vnames]
  test  <- test[,names(test) %in% vnames]
  
  train <- as.data.frame(train)
  test <- as.data.frame(test)
  
  train$Fever_on_admission <- as.factor(train$Fever_on_admission)
  test$Fever_on_admission <- as.factor(test$Fever_on_admission)
  
  train$Death_yn <- as.factor(ifelse(train$Death_yn == 1, "yes", "no"))
  test$Death_yn <- as.factor(ifelse(test$Death_yn == 1, "yes", "no"))
  
  train <- train[complete.cases(train),]
  test  <- test[complete.cases(test),]
  
  rfmod  <- randomForest(Death_yn ~ ., train)
  predictions <- predict(rfmod, test, type = "prob")[,2]
  aucs2 <- c(aucs2, auc(test$Death_yn, predictions, levels = c("no","yes"), direction = "<"))
  sizes2 <- rbind(sizes2, c(nrow(train), nrow(test)))
}


mean(aucs)
mean(aucs2)



#########################################################################################################################
## Final model

require(haven)
require(randomForest)
require(pROC)

x  <- read_sav("All_slopes_KFJ.sav")

vnames <- c("Age",  "LDH_slope", "PLT_slope", "CRP_slope", "Fever_on_admission",
                    "LDH_intercept2", "CRP_intercept2",  "Creatinine_intercept2", "Death")
x  <- x[,names(x) %in% vnames]
x  <- x[complete.cases(x),]

x$Fever_on_admission <- as.factor(x$Fever_on_admission)
x$Death <- as.factor(ifelse(x$Death == 1, "yes", "no"))


baseline               <- glm(Death ~ ., data = x, family = "binomial")
baseline$coefficients  <- 0.7974 * baseline$coefficients

set.seed(42)
rfmod  <- randomForest(Death ~ ., x)


#########################################################################################################################
## 2) Validation on B1.1.7

test  <- read_sav("B117_Death_and Prob_death_with_predictors.sav")

vnames <- c("Age",  "LDH_slope", "PLT_slope", "CRP_slope", "Fever_on_admission",
            "LDH_intercept2", "CRP_intercept2",  "Creatinine_intercept2", "Death")
test  <- test[,names(test) %in% vnames]
test  <- test[complete.cases(test),]

test$Fever_on_admission <- as.factor(test$Fever_on_admission)
test$Death <- as.factor(ifelse(test$Death == 1, "yes", "no"))

predictions <- predict(rfmod, test, type = "prob")[,2]
auc(test$Death, predictions, levels = c("no","yes"), direction = "<")

ROC <- roc(test$Death, predictions, levels = c("no","yes"), direction = "<")
set.seed(42)
ci.auc(ROC)



#########################################################################################################################
## 3) XAI

library(DALEX)

# create explainers
bl_exp <- explain(baseline, data = x[,-9], y = as.integer(x$Death), label = "baseline", colorize = TRUE)
rf_exp <- explain(rfmod, data = x[,-9], y = as.integer(x$Death), label = "forest", colorize = TRUE)


selected_variables <- names(x)[-9]
bl_ale  <- model_profile(bl_exp, variables = selected_variables, type = "accumulated")$agr_profiles
rf_ale  <- model_profile(rf_exp, variables = selected_variables, type = "accumulated")$agr_profiles
plot(bl_ale, rf_ale)

summary(x)
par(mfrow= c(1,2), cex = 0.7, lwd = 2)
plot(density(x$Creatinine_intercept2), main = "Creatinine_intercept2")
plot(density(x$LDH_slope), main = "LDH_slope")

