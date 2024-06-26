library(dplyr)
library(ape)
library(sf)
library(readr)
library(geosphere)
library(spdep)

# Read the data
df <- read_csv("data_own/data_municipalities.csv")
data <- st_read("data_own/map_municipalities.shp")

sf_multipolygon <- st_sfc(data$geometry)
data.nb <- spdep::poly2nb(sf_multipolygon)
data.list <- spdep::nb2listw(data.nb,zero.policy = TRUE)
spdep::moran.test(df$percent_vaccinated, data.list, zero.policy = TRUE)

lisaRslt <- spdep::localmoran(df$percent_vaccinated, data.list, 
                              zero.policy = TRUE, na.action = na.omit)

significanceLevel <- 0.05; # 95% confidence
meanVal <- mean(df$percent_vaccinated);

lisaRslt %<>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
  dplyr::mutate(coType = dplyr::case_when(
    `Pr(z > 0)` > 0.05 ~ "Insignificant",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & df$percent_vaccinated >= meanVal ~ "High cluster",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & df$percent_vaccinated < meanVal ~ "Low cluster",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & df$percent_vaccinated >= meanVal ~ "High outlier",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & df$percent_vaccinated < meanVal ~ "Low outlier"
  ))

# Now add this coType to original sf data
data$coType <- lisaRslt$coType %>% tidyr::replace_na("Insignificant")
library(ggplot2)
ggplot(data) +
  geom_sf(aes(fill=coType),color = 'lightgrey') +
  scale_fill_manual(values = c('red','brown','gray','cyan','blue'), name='Vaccination percentage') +
  labs(title = "Vaccination rate in Polish municipalities")

data <- read.csv('processed_df_modelling.csv')

mod <- lm(y ~
            factor(partitions) +
            factor(type_of_municipality) +
            SLD_percent + PO_percent +
            frekwencja_wyborcza + PSL_percent +
            Konfederacja_percent +
            log(revenues_per_capita_PIT) +
            percent_over_60 + percent_under_18 +
            education_share_higher + cars_per_1000_persons +
            forests_area_ratio_area_km2,
          data = data)  
summary(mod)

## Adding neighbours columns
data["education_share_higher_neighbours"] <- df_spatial["education_share_higher"]
data["forests_area_ratio_area_km2_neighbours"] <- df_spatial["forests_area_ratio_area_km2"]
data["frekwencja_wyborcza_neighbours"] <- df_spatial["frekwencja_wyborcza"]
# Deleted factor(partitions)
serrRslt <- spatialreg::errorsarlm(y ~  factor(type_of_municipality) + SLD_percent + PO_percent + frekwencja_wyborcza + PSL_percent +
                                     log(revenues_per_capita_PIT) + percent_over_60 + education_share_higher + cars_per_1000_persons + 
                                     forests_area_ratio_area_km2 + education_share_higher_neighbours + forests_area_ratio_area_km2_neighbours,
                                   data = data,
                                   listw = data.list,
                                   zero.policy = TRUE)

summary(serrRslt)
AIC(mod)
AIC(serrRslt)

W <- listw2mat(data.list)
W %*% df$population_total
matr <- as.matrix(data[,-c(1,2)])

df_spatial <- as.data.frame(W %*% matr)

final_mod <- spatialreg::sacsarlm(y ~  factor(type_of_municipality) + SLD_percent + PO_percent + frekwencja_wyborcza + PSL_percent +
                                    log(revenues_per_capita_PIT) + percent_over_60 + education_share_higher + cars_per_1000_persons + 
                                    forests_area_ratio_area_km2 + education_share_higher_neighbours + forests_area_ratio_area_km2_neighbours,
                                  data = data,
                                  listw = data.list,
                                  zero.policy = TRUE)

summary(final_mod)

mod_lin <- lm(y ~  factor(type_of_municipality) + SLD_percent + PO_percent + frekwencja_wyborcza + PSL_percent +
  log(revenues_per_capita_PIT) + percent_over_60 + education_share_higher + cars_per_1000_persons + 
  forests_area_ratio_area_km2 + education_share_higher_neighbours + forests_area_ratio_area_km2_neighbours, data=data)

data.list %>%
  spdep::moran.test(mod_lin$residuals, ., zero.policy = TRUE)
### Nieistotne zmienne przestrzenne w modelu przestrzennym

sp_notimp <- spatialreg::errorsarlm(y ~ factor(type_of_municipality) + 
                                      factor(partitions) + PO_percent + sqrt(SLD_percent) + 
                                PO_percent:frekwencja_wyborcza + frekwencja_wyborcza + PSL_percent + 
                                log(revenues_per_capita_PIT) + poly(percent_over_60, 2)  + 
                                healthcare_advices_ratio_total  + forests_area_ratio_area_km2_neighbours + 
                                education_share_higher_neighbours + cars_per_1000_persons + 
                                forests_area_ratio_area_km2,
                              data = data,
                              listw = data.list,
                              zero.policy = TRUE)
summary(sp_notimp)

### Skomplikowany model ###
sp1 <- spatialreg::errorsarlm(y ~ PO_percent + sqrt(SLD_percent) + 
                                PO_percent:frekwencja_wyborcza + frekwencja_wyborcza + PSL_percent + 
                                log(revenues_per_capita_PIT) + poly(percent_over_60, 2)  + 
                                healthcare_advices_ratio_total  + forests_area_ratio_area_km2_neighbours + 
                                education_share_higher_neighbours + cars_per_1_persons + persons_running_business_per_1_person +
                                entities_registered_per_1_person + min_dist +
                                forests_area_ratio_area_km2,
                              data = data,
                              listw = data.list,
                              zero.policy = TRUE)
summary(sp1)

sp2 <- spatialreg::sacsarlm(y ~ PO_percent + sqrt(SLD_percent) + 
                              PO_percent:frekwencja_wyborcza + frekwencja_wyborcza + PSL_percent + 
                              log(revenues_per_capita_PIT) + poly(percent_over_60, 2)  + 
                              healthcare_advices_ratio_total  + forests_area_ratio_area_km2_neighbours + 
                              education_share_higher_neighbours + cars_per_1_persons + persons_running_business_per_1_person +
                              entities_registered_per_1_person + min_dist +
                              forests_area_ratio_area_km2,
                            data = data,
                            listw = data.list,
                            zero.policy = TRUE)
summary(sp2)

# Plot the results
ggplot(mean_res_by_county, aes(x = , y = mean_res)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "County Code", y = "Mean of res", title = "Mean of res by County Code")