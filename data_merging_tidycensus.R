
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
  dplyr::select(-c(`Statewide Average`, `Expected Rate`, `Ratio`, `Healthscore Rating`))


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
  geometry = TRUE,
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

employer_based_insurance <- get_acs(
  geography = "zip code tabulation area", 
  variables = "C27004_001",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(employer_based_insurance_rate = estimate.x/estimate.y) %>%
  select(GEOID, employer_based_insurance_rate)

medicare_coverage <- get_acs(
  geography = "zip code tabulation area", 
  variables = "C27006_001",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(medicare_coverage_rate = estimate.x/estimate.y) %>%
  select(GEOID, medicare_coverage_rate)

medicaid_coverage <- get_acs(
  geography = "zip code tabulation area", 
  variables = "C27007_001",
  year = 2021
) %>%
  left_join(population, by = "GEOID") %>%
  mutate(medicaid_coverage_rate = estimate.x/estimate.y) %>%
  select(GEOID, medicaid_coverage_rate)

health_insurance_coverage <- get_acs(
    geography = "zip code tabulation area", 
    variables = "C27016_001",
    year = 2021
  ) %>%
  left_join(population, by = "GEOID") %>%
  mutate(insurance_coverage_rate = estimate.x/estimate.y) %>%
  select(GEOID, insurance_coverage_rate)

covid <- read.csv("Data/covid_zip.csv") %>% 
  select(ZIP, Cases) %>%
  mutate(ZIP = as.factor(ZIP)) %>%
  left_join(population, by = c("ZIP" = "GEOID"))



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


