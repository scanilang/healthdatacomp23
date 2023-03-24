
library(readxl)
library(dplyr)

utilization_data <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx")
quality_data_adj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 1)
quality_data_unadj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 2)
zip_data <- read.csv("Data/zip_crosswalk.csv")
food_data <- read_excel("Data/FoodAccessResearchAtlasData2019.xlsx", sheet = 3) # this is messy because it's by census tract
police_data <- read.csv("Data/department_score.csv")
social_cap_data <- read.csv("social_index_mn.csv")
covid_data <- read.csv("Data/time_series_covid19_confirmed_US.csv")
aqi_data <- read.csv("Data/annual_aqi_by_county_2021.csv")
educ_attain_data <- read.csv("Data/educational_attainment.csv") # need to figure out what variable/s we want to use and then clean this up
transit_data <- read.csv("Data/alltransit_data_places_27.csv")

police_data$department <- str_trim(police_data$department)
colnames(police_data)[3] <- "police_score"
covid_data <- covid_data %>% 
  filter(Province_State == "Minnesota") %>% 
  select(!starts_with("X") | ends_with(".21"))
covid_data$total_covid_count21 <- covid_data %>%
  select(starts_with("X")) %>%
  rowSums()
covid_data <- covid_data %>%
  select(!starts_with("X"))
transit_data$name <- str_sub(transit_data$name, 2, -2)

data_phq9 <- quality_data_adj %>% dplyr::filter(str_detect(`Measure Name`, "PHQ-9") & `Clinic Name` != "TOTAL")


combined_data <- data_phq9 %>%
  left_join(utilization_data, by = c("Medical Group Name", "Measurement Year")) %>%
  left_join(zip_data, by = c('Clinic Zip Code' = 'Zip.Code')) %>%
  left_join(police_data, by = c('Clinic City' = 'department')) %>%
  left_join(social_cap_data, by = c('FIPS.County.Code' = 'county.fips')) %>%
  left_join(covid_data, by = c('County' = 'Admin2')) %>%
  left_join(aqi_data, by = "County") %>%
  left_join(transit_data, by = c('County' = 'name')) %>%
  select("Measurement Year", "Measure Name", "Statewide Average", "Medical Group Name", "Clinic Name", 
         "Clinic City", "Clinic Zip Code", "Denominator", "Actual Rate", "Expected Rate", "Healthscore Rating",
         "All Patients TCOC", "Adults TCOC", "Cost Index", "Price Index", "Resource Use Index", "County",                            
         "Metropolitan.Micropolitan.Statistical.Area", "FIPS.County.Code", "police_score", "sk2014",
         "total_covid_count21", "Days.with.AQI", "Good.Days", "Unhealthy.Days", "Max.AQI", "Median.AQI",
         "population", "households", "alltransit_performance_score"  )

numeric_data <- combined_data %>% 
  dplyr::select(where(is.numeric))

cormat <- round(cor(numeric_data, use = "complete.obs"), 2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()




