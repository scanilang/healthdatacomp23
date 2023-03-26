
library(tidycensus)
library(readxl)
library(dplyr)

utilization_data <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx") %>% filter(`Measurement Year` == 2021) %>% select(`Medical Group Name`, `Adults TCOC`)
quality_data_adj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 1)
# quality_data_unadj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 2)

quality_data_adj <- quality_data_adj %>% 
  mutate(`Clinic City` = case_when(`Clinic City`  == c("Saint Paul", "St Paul") ~ "St. Paul",
                                   `Clinic City`  == "Inver Grove Hts" ~ "Inver Grove Heights", 
                                   `Clinic City`  == "Mankato, MN" ~ "Mankato",
                                   `Clinic City`  == "Mt. Iron" ~ "Mountain Iron",
                                   `Clinic City`  == "St Cloud" ~ "St. Cloud",
                                   `Clinic City`  == "Saint Louis Park" ~ "St. Louis Park",
                                   `Clinic City`  == "Rpbbinsdale" ~ "Robbinsdale",
                                   TRUE ~ `Clinic City`),
         `Clinic Name` = ifelse(`Clinic Name` == 'Centracare Clinic - SCMG St. Cloud Medical Group â€“ South', "Centracare Clinic - SCMG St. Cloud Medical Group South", str_trim(`Clinic Name`)),
         `Medical Group Name` = case_when(str_detect(`Clinic Name`, "EH East") == TRUE ~ "Essentia Health - East Region", # in 2019 it is East not East Region
                                          str_detect(`Clinic Name`, "EH West") == TRUE ~ "Essentia Health - West",
                                          `Clinic Name` == "Mayo Clinic Health System Fairmont" ~ "Mayo Clinic Health System - Fairmont",
                                          `Clinic Name` == "Mayo Clinic Health System- Franciscan Healthcare in Caledonia" ~ "Mayo Clinic Health System - Franciscan Healthcare in La Cr",
                                          `Clinic Name` == "Mayo Clinic Health System Lake City" ~ "Mayo Clinic Health System - Lake City",
                                          `Clinic Name` == "Mayo Clinic - Northwest" ~ "Mayo Clinic Health System - Northwest Wisconsin Region",
                                          `Clinic Name` == "Mayo Clinic Health System Red Wing" ~ "Mayo Clinic Health System - Red Wing",
                                          `Clinic Name` == "Mayo Family Clinic Southeast" ~ "Mayo Clinic Health System - Southeast Minnesota Region",
                                          `Clinic Name` == "Mayo Clinic Health System St. James" ~ "Mayo Clinic Health System - St. James",
                                          `Clinic Name` == "Mayo Clinic Health System Owatonna" ~ "Mayo Clinic Health System-Owatonna ",
                                          TRUE ~ `Medical Group Name`))

data_phq9_6month <- quality_data_adj %>% 
  dplyr::filter(`Measure Name` == "PHQ-9 Follow-up Rate at 6 Months Adult" & `Clinic Name` != "TOTAL") %>%
  dplyr::select(-c(`Statewide Average`, `Expected Rate`, `Ratio`, `Healthscore Rating`)) %>%
  mutate(ZIP = as.factor(`Clinic Zip Code`))


population <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B01003_001",
  year = 2021
)

# response_rate <- get_acs(
#   geography = "zip code tabulation area", 
#   variables = "B98022_001",
#   year = 2021
# )

# owner occupied housing units
home_ownership <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B25106_002",
  year = 2021,
  survey = "acs5"
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(home_ownership_rate = estimate.x/estimate.y) %>%
  select(GEOID, home_ownership_rate)

educ_attainment <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B15003_022",
  year = 2021) %>%
  left_join(get_acs(
    geography = "zip code tabulation area", 
    variables = "B15003_023",
    year = 2021), by = "GEOID") %>%
  left_join(get_acs(
    geography = "zip code tabulation area", 
    variables = "B15003_024",
    year = 2021), by = "GEOID") %>%
  left_join(get_acs(
    geography = "zip code tabulation area", 
    variables = "B15003_025",
    year = 2021), by = "GEOID") %>% 
  mutate(bachelors_up = estimate.x + estimate.y + estimate.x.x + estimate.y.y) %>%
  left_join(population, by = "GEOID") %>%
  mutate(educ_attainment_rate = bachelors_up/estimate) %>%
  select(GEOID, educ_attainment_rate)

private_vehicle <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B99082_002",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(private_vehicle_rate = estimate.x/estimate.y) %>%
  select(GEOID, private_vehicle_rate)

insurance_coverage <- get_acs(
  geography = "zip code tabulation area", 
  variables = "C27012_003",
  year = 2021) %>%
  left_join(get_acs(
    geography = "zip code tabulation area", 
    variables = "C27012_010",
    year = 2021), by = "GEOID") %>%
  left_join(get_acs(
    geography = "zip code tabulation area", 
    variables = "C27012_017",
    year = 2021), by = "GEOID") %>% 
  mutate(covered = estimate.x + estimate.y + estimate) %>%
  left_join(population, by = "GEOID") %>%
  mutate(insurance_coverage_rate = covered/estimate.y.y) %>%
  select(GEOID, insurance_coverage_rate)

workplace <- get_acs(
  geography = "zip code tabulation area", 
  variables = "C24070_053",
  year = 2021) %>%
  left_join(get_acs(
    geography = "zip code tabulation area", 
    variables = "C24070_054",
    year = 2021), by = "GEOID") %>%
  left_join(get_acs(
    geography = "zip code tabulation area", 
    variables = "C24070_056",
    year = 2021), by = "GEOID") %>%
  mutate(public_workplace = estimate.x + estimate.y + estimate) %>%
  left_join(population, by = "GEOID") %>%
  mutate(workplace_rate = public_workplace/estimate.y.y) %>%
  select(GEOID, workplace_rate)

public_transit_to_work <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B08301_010",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(public_transit_to_work_rate = estimate.x/estimate.y) %>%
  select(GEOID, public_transit_to_work_rate)

aggregate_travel_time <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B08013_001",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(aggregate_travel_time_rate = estimate.x/estimate.y) %>%
  select(GEOID, aggregate_travel_time_rate)


covid <- read.csv("Data/covid_zip.csv") %>% 
  select(ZIP, Cases) %>%
  mutate(ZIP = as.factor(ZIP)) %>%
  left_join(population, by = c("ZIP" = "GEOID")) %>%
  mutate(covid_rate = Cases/estimate) %>%
  select(ZIP, covid_rate)


# classification <- read.csv("Data/clinics.csv") %>%
#   select(-c(X, X.1, X.2)) %>%
#   mutate(low.cost.option = ifelse(is.na(low.cost.option), 0, low.cost.option),
#          FQHC = ifelse(FQHC == "FQHC", 1, 0),
#          type = ifelse(type == "church", "other", type),
#          Clinic.Name = ifelse(str_detect(Clinic.Name, "Centracare Clinic - SCMG St. Cloud Medical Group"), "Centracare Clinic - SCMG St. Cloud Medical Group South", str_trim(Clinic.Name)))

classification <- read.csv("Data/clinics.csv") %>%
  select(-c(X, X.1, X.2)) %>%
  mutate(public_fqhc_ind = ifelse(type == "public" | FQHC == "FQHC", 1, 0),
         Clinic.Name = ifelse(str_detect(Clinic.Name, "Centracare Clinic - SCMG St. Cloud Medical Group"), "Centracare Clinic - SCMG St. Cloud Medical Group South", str_trim(Clinic.Name))) %>%
  select(Clinic.Name, public_fqhc_ind)


rural_classification <- read_xlsx("Z/NCHSURCodes2013.xlsx") %>%
  filter(`State Abr.` == "MN") %>%
  rename(County = `County name`) %>%
  rename(code = `2013 code`) %>% 
  mutate(County = gsub(" County", "", County),
         County = case_when(County == "St. Louis" ~ "Saint Louis", 
                            County == "Lac qui Parle" ~ "Lac Qui Parle",
                            County == "McLeod" ~ "Mcleod",
                            TRUE ~ County)) %>%
  select(County, code)

zip_data <- read.csv("Data/zip_crosswalk.csv") %>% select(Zip.Code, County)


census_data <- home_ownership %>%
  left_join(educ_attainment, by = "GEOID") %>%
  left_join(private_vehicle, by = "GEOID") %>%
  left_join(insurance_coverage, by = "GEOID") %>%
  left_join(workplace, by = "GEOID") %>%
  left_join(public_transit_to_work, by = "GEOID") %>%
  left_join(aggregate_travel_time, by = "GEOID") %>%
  left_join(covid, by = c("GEOID" = "ZIP"))

all_data <- left_join(data_phq9_6month, census_data, by = c("ZIP" = "GEOID")) %>%
  mutate(covid_rate = case_when(`Measurement Year` == 2019 ~ 0,
                                TRUE ~ covid_rate)) %>%
  left_join(classification, by = c("Clinic Name" = "Clinic.Name")) %>%
  left_join(zip_data, by = c(`Clinic Zip Code` = "Zip.Code")) %>%
  left_join(rural_classification, by = "County") %>%
  left_join(utilization_data, by = "Medical Group Name")


write.csv(all_data, "Data/tidycensus_data.csv")


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


mod1 <- lm(data = all_data, `Actual Rate` ~ as.factor(`Measurement Year`) + home_ownership_rate +
             educ_attainment_rate + private_vehicle_rate + insurance_coverage_rate + 
             covid_rate + public_fqhc_ind + as.factor(code) + `Adults TCOC` + workplace_rate + 
             public_transit_to_work_rate + aggregate_travel_time_rate)
summary(mod1)
summary(step(mod1))


# --------------------------------------------

all_data$code <- as.factor(all_data$code)

levels(all_data$code) <- c("B","B","M","M","S","S")


mod2 <- lm(data = all_data, `Actual Rate` ~ as.factor(`Measurement Year`) + home_ownership_rate +
             educ_attainment_rate + insurance_coverage_rate + 
             covid_rate + public_fqhc_ind + private_vehicle_rate * as.factor(code) + `Adults TCOC` + workplace_rate + 
             public_transit_to_work_rate + aggregate_travel_time_rate)
summary(mod2)
summary(step(mod2))

# --------------------------------------------


lambda_grid <- 10^seq(-3, 2, length = 1000)

# Perform LASSO
lasso_model <- train(`Actual Rate` ~ as.factor(`Measurement Year`) + home_ownership_rate +
                       educ_attainment_rate + private_vehicle_rate + insurance_coverage_rate + 
                       covid_rate + public_fqhc_ind + as.factor(code) + `Adults TCOC` + workplace_rate + 
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


mod3 <- lm(data = all_data, `Actual Rate` ~ home_ownership_rate +
             private_vehicle_rate + public_fqhc_ind + as.factor(code) + `Adults TCOC`)
summary(mod3)
summary(step(mod3))



