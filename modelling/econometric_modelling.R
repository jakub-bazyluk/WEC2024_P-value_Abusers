# importing libraries
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)
library(GGally)
library(MASS)
library(fitdistrplus)
library(nortest)
library(mgcv)
library(betareg)
library(tidyr)
library(glmnet)
library(ppcor)
# miexed effects stuff
library(nlme)
library(lme4)
library(glmmTMB)
library(spatialreg)
library(geosphere)
library(spdep)
library(car)
library(effects)


## Wczytywanie -----------------------------------------------------------------
df = read.csv("processed_df_modelling.csv", row.names = 1)

logit_inverse <- function(x){return(1 / (1 + exp(-x)))}
logit <- function(x){return(log(x/(1-x)))}
df['y_original'] = logit_inverse(df[, 'y'])
  
attach(df)
print(dim(df))

## EDA -------------------------------------------------------------------------
# par(mfrow = c(1, 1))
# hist(df[, 'percent_vaccinated_01_r'], breaks = 30)
# 
# par(mfrow = c(1, 1))
# plot(percent_over_60, y)
# 
# par(mfrow = c(1, 1))
# plot(healthcare_advices_ratio_total, percent_vaccinated_01_r)
# 
# par(mfrow = c(1, 1))
# plot(sqrt(min_dist), y)
# 
# par(mfrow = c(1, 1))
# plot(population_total_f_ratio_total, y)
# 
# df['revenues_per_capita_PIT_log'] = log(df['revenues_per_capita_PIT'])

## Modele ----------------------------------------------------------------------
### MVP - FINAL MODEL ----------------------------------------------------------

mvp = lm(y ~
      # factor(partitions) +
      # factor(type_of_municipality) +
      
      sqrt(SLD_percent) +
      PO_percent +
      frekwencja_wyborcza +
      PSL_percent +
      Konfederacja_percent +
      
      revenues_per_capita_PIT_log +
      percent_over_60 +
        
      sqrt(min_dist) +
      no_loc_in10km +
      
      education_share_higher +
      poly(cars_per_1000_persons, 2) +

      healthcare_advices_ratio_total

    , data = df)

par(mfrow = c(2, 2))
plot(Effect(c("PO_percent"), mvp, partial.residuals = TRUE))
plot(Effect(c("cars_per_1000_persons"), mvp, partial.residuals = TRUE))
plot(Effect(c("revenues_per_capita_PIT_log"), mvp, partial.residuals = TRUE))
plot(Effect(c("education_share_higher"), mvp, partial.residuals = TRUE))

summary(mvp)
par(mfrow = c(2, 2))
plot(mvp)
vif(mvp)

### linowy ---------------------------------------------------------------------
m1 = lm(y ~
          factor(partitions) +
          factor(type_of_municipality) +
          
          # PO_percent + 
          sqrt(SLD_percent) +
          PO_pe rcent:frekwencja_wyborcza +
          frekwencja_wyborcza +
          PSL_percent +
          
          log(revenues_per_capita_PIT) +
          poly(percent_over_60, 2) +
          percent_under_18 +
          
          min_dist +
          healthcare_advices_ratio_total +
          
          population_total_f_ratio_total +
          population_density_log +
          poly(forests_area_ratio_area_km2, 2) +
          
          poly(education_share_higher, 2) +
          sqrt(education_share_vocational)
        
        ,data = df)

summary(m1)
par(mfrow = c(2, 2))
plot(m1)


### Model z efektami losowymi --------------------------------------------------
mrandom1 = lmer(y ~ 
                factor(partitions) +
                factor(type_of_municipality) +
                
                # PO_percent + 
                sqrt(SLD_percent) +
                PO_percent:frekwencja_wyborcza +
                frekwencja_wyborcza +
                PSL_percent +
                
                log(revenues_per_capita_PIT) +
                poly(percent_over_60, 2) +
                
                # min_dist +
                healthcare_advices_ratio_total +
                
                population_total_f_ratio_total +
                population_density_log +
                poly(forests_area_ratio_area_km2, 2) +
                
                poly(education_share_higher, 2) +
                sqrt(education_share_vocational) +
                  
                (1|factor(county_code)), REML = T)

summary(mrandom1)

### step -----------------------------------------------------------------------

m0 = lm(y ~ ., data = df)
summary(m0)
par(mfrow = c(2,2))
plot(m0)

n = dim(df)[1]
m_setp = step(m0, direction =  'backward', k = log(n),  trace = F)

summary(m_setp)
par(mfrow = c(2,2))
plot(m_setp)
vif(m_setp)

### GAM ------------------------------------------------------------------------
mgam1 = gam(y ~
              factor(partitions) +
              factor(type_of_municipality) +
              
              PO_percent + 
              sqrt(SLD_percent) +
              PO_percent:frekwencja_wyborcza +
              frekwencja_wyborcza +
              PSL_percent +
              
              log(revenues_per_capita_PIT) +
              poly(percent_over_60, 2) +
              percent_under_18 +
              
              healthcare_advices_ratio_total +
              
              population_total_f_ratio_total +
              population_density_log +
              poly(forests_area_ratio_area_km2, 2) +
              
              poly(education_share_higher, 2) +
              
              motorcycles_per_1000_persons
            ,data = df, gamma = 1)

summary(mgam1)
gam.check(mgam1)
par(mfrow = c(1, 1))
vif(mgam1)
plot.gam(mgam1, residuals = T, all.terms = T)

model_plot = mgam1
vis.gam(model_plot, view=c("Longitude","Latitude"),
        theta = 120, plot.type = "persp")