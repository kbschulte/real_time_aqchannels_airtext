## Real-time air quality channels - Data prep reproducible code#
## 16/05/2021 ##
## Kayla Schulte ##
## Leverhulme Centre for Demographic Science - University of Oxford ##

#Load packages

packages <- c("readxl", "dplyr", "lubridate", "ggplot2")

lapply(packages, require, character.only = TRUE)

##Load data
#Airtext data

load("airtext_data.rda")

#London Population data

load("/demographic_data.rda")

#London postcode data

London_postcodes <- read.csv("/london postcodes.csv", header = T)

#clean data

airtext_users_borough <- rename(borough = ...1, airtext_users_borough)
airtext_users_borough <- airtext_users_borough[-c(35), ]

airtext_users_postcode_all <- airtext_users_postcode %>%
  group_by(outercode) %>% 
  summarise(num = n(),
            total = sum(count)) #is this a randomly distributed sample?

London_postcodes$Postcode <- as.character(as.factor(London_postcodes$Postcode))
London_postcodes$Postcode <- str_replace_all(London_postcodes$Postcode, fixed(" "), "")

London_postcodes_grouped <- London_postcodes %>%
  group_by(Postcode.district) 

airtext_users_postcode_all <- rename(Postcode.district = outercode, airtext_users_postcode_all)
airtext_users_postcode_metadata <- merge(airtext_users_postcode_all, London_postcodes, by = "Postcode.district")

#Generate dataframe  outer postcode (district) level

airtext_GLA2016_no2_postcode <- merge(GLA_no2_bypostcode_FINAL, airtext_users_postcode_all, by = "Postcode.district", all.x = T)
airtext_GLA2016_no2_postcode_test <- distinct(airtext_GLA2016_no2_postcode, Postcode.district, mean_NO2, total) 

airtext_GLA2016_no2_postcode[is.na(airtext_GLA2016_no2_postcode)] <- 0

airtext_no2_postcode_dem <- left_join(airtext_GLA2016_no2_postcode, postcode_district_pop, by = "Postcode.district")
airtext_no2_postcode_dem <- subset(airtext_no2_postcode_dem, select = c(Postcode.district, usual_res, total, mean_NO2))

airtext_no2_postcode_dem[is.na(airtext_no2_postcode_dem)] <- 0

airtext_no2_postcode_dem <- na.omit(airtext_no2_postcode_dem) 

airtext_users_postcode_metadata2 <- airtext_users_postcode_metadata[!duplicated(airtext_users_postcode_metadata$Postcode.district),]
airtext_no2_postcode_dem2 <- right_join(airtext_no2_postcode_dem, airtext_users_postcode_metadata2, by = "Postcode.district")

airtext_no2_postcode_dem2[is.na(airtext_no2_postcode_dem2)] <- 0
airtext_no2_postcode_dem2 <- na.omit(airtext_no2_postcode_dem2) 
airtext_no2_postcode_dem2 <- rename(total = total.x, airtext_no2_postcode_dem2)
airtext_no2_postcode_dem2 <- rename(IMD = Index.of.Multiple.Deprivation, airtext_no2_postcode_dem2)
airtext_no2_postcode_dem2 <- subset(airtext_no2_postcode_dem2, select = c(Average.Income, IMD, Altitude, total, usual_res, mean_NO2, Postcode.district))

nrow(airtext_no2_postcode_dem2)
