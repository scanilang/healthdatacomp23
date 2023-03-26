
library(tidycensus)
library(readxl)
library(dplyr)

# utilization_data <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx")
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
                                   TRUE ~ `Clinic City`)) 

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
  variables = "B15003_001",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(educ_attainment_rate = estimate.x/estimate.y) %>%
  select(GEOID, educ_attainment_rate)

private_vehicle <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B99082_001",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(private_vehicle_rate = estimate.x/estimate.y) %>%
  select(GEOID, private_vehicle_rate)

public_insurance <- get_acs(
  geography = "zip code tabulation area", 
  variables = "C27014_001",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(public_insurance_rate = estimate.x/estimate.y) %>%
  select(GEOID, public_insurance_rate)

private_insurance <- get_acs(
  geography = "zip code tabulation area", 
  variables = "C27013_001",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(private_insurance_rate = estimate.x/estimate.y) %>%
  select(GEOID, private_insurance_rate)


covid <- read.csv("Data/covid_zip.csv") %>% 
  select(ZIP, Cases) %>%
  mutate(ZIP = as.factor(ZIP)) %>%
  left_join(population, by = c("ZIP" = "GEOID")) %>%
  mutate(covid_rate = Cases/estimate) %>%
  select(ZIP, covid_rate)


census_data <- home_ownership %>%
  left_join(educ_attainment, by = "GEOID") %>%
  left_join(private_vehicle, by = "GEOID") %>%
  left_join(public_insurance, by = "GEOID") %>%
  left_join(private_insurance, by = "GEOID") %>%
  left_join(covid, by = c("GEOID" = "ZIP"))

all_data <- left_join(data_phq9_6month, census_data, by = c("ZIP" = "GEOID")) %>%
  mutate(covid_rate = case_when(`Measurement Year` == 2019 ~ 0,
            TRUE ~ covid_rate))

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
             educ_attainment_rate + private_vehicle_rate + public_insurance_rate + private_insurance_rate + 
             covid_rate)
summary(mod1)

