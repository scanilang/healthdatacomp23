
library(tidycensus)
library(readxl)
library(dplyr)

all_data <- read.csv("/Users/zuofuhuang/Desktop/healthdatacomp23/Data/tidycensus_data.csv")


# income <- get_acs(
#   geography = "zip code tabulation area", 
#   variables = "B19013_001",
#   year = 2019,
#   geometry = TRUE
# )
# 
# Minnesota <- income %>% 
#   dplyr::mutate(zip = str_replace(NAME, "ZCTA5 ", ""),
#                 zip = as.numeric(zip))  %>% 
#   dplyr::filter(zip >= 55001 & zip <= 56763)
# 
# v17 <- load_variables(2019, "acs1", cache = TRUE)
# v19 <- load_variables(2021, "acs5", cache = TRUE)
# 
# write.csv(v19, "vars.csv")


mod1 <- lm(data = all_data, Actual.Rate ~ as.factor(Measurement.Year) + home_ownership_rate +
             educ_attainment_rate + private_vehicle_rate + insurance_coverage_rate + 
             covid_rate + public_fqhc_ind + as.factor(code) + `Adults.TCOC` + idk_rate + 
             public_transit_to_work_rate + aggregate_travel_time_rate + survival::cluster(ZIP))
summary(mod1)
summary(step(mod1))


# --------------------------------------------

all_data$code <- as.factor(all_data$code)

levels(all_data$code) <- c("B","M","M","S","S","S")


mod2 <- lm(data = all_data, Actual.Rate ~ as.factor(Measurement.Year) + home_ownership_rate +
             educ_attainment_rate + private_vehicle_rate + insurance_coverage_rate + 
             covid_rate + public_fqhc_ind + as.factor(code) + `Adults.TCOC` + idk_rate + 
             public_transit_to_work_rate + aggregate_travel_time_rate + survival::cluster(ZIP))
summary(mod2)
summary(step(mod2))

# --------------------------------------------


lambda_grid <- 10^seq(-3, 2, length = 1000)

# Perform LASSO
lasso_model <- train(Actual.Rate ~ as.factor(Measurement.Year) + home_ownership_rate +
                       educ_attainment_rate + private_vehicle_rate + insurance_coverage_rate + 
                       covid_rate + public_fqhc_ind + as.factor(code) + `Adults.TCOC` + idk_rate + 
                       public_transit_to_work_rate + aggregate_travel_time_rate + survival::cluster(ZIP),
                     data = all_data,
                     method = "glmnet",
                     trControl = trainControl(method = "cv", number = 10, selectionFunction = "oneSE"),
                     tuneGrid = data.frame(alpha = 1, lambda = lambda_grid),
                     metric = "MAE",
                     na.action = na.omit
)

lasso_model$bestTune
coef(lasso_model$finalModel, lasso_model$bestTune$lambda)


# --------------------------------------------


mod3 <- lm(data = all_data, Actual.Rate ~ home_ownership_rate +
             private_vehicle_rate + 
             covid_rate + public_fqhc_ind + as.factor(code) + `Adults.TCOC` + survival::cluster(ZIP))
summary(mod3)
summary(step(mod3))



