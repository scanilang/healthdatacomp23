
library(readxl)
library(dplyr)

# loading data
utilization_data <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx")
quality_data_adj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 1)
quality_data_unadj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 2)
zip_data <- read.csv("Data/zip_crosswalk.csv")
# food_data <- read_excel("Data/FoodAccessResearchAtlasData2019.xlsx", sheet = 3) # this is messy because it's by census tract
police_data <- read.csv("Data/department_score.csv")
social_cap_data <- read.csv("social_index_mn.csv")
covid_data <- read.csv("Data/time_series_covid19_confirmed_US.csv")
aqi_data <- read.csv("Data/annual_aqi_by_county_2021.csv") %>% filter(State == "Minnesota")
educ_attain_data <- read.csv("Data/educational_attainment.csv", header = FALSE)
transit_data <- read.csv("Data/walkscore.csv") %>% filter(STATEFP == 27)
home_data <- read.csv("Data/homeownership_modified.csv")


# cleaning data
quality_data_adj <- quality_data_adj %>% 
  mutate(`Clinic City` = case_when(`Clinic City`  == c("Saint Paul", "St Paul") ~ "St. Paul",
                                   `Clinic City`  == "Inver Grove Hts" ~ "Inver Grove Heights", 
                                   `Clinic City`  == "Mankato, MN" ~ "Mankato",
                                   `Clinic City`  == "Mt. Iron" ~ "Mountain Iron",
                                   `Clinic City`  == "St Cloud" ~ "St. Cloud",
                                   `Clinic City`  == "Saint Louis Park" ~ "St. Louis Park",
                                   `Clinic City`  == "Rpbbinsdale" ~ "Robbinsdale",
                                 TRUE ~ `Clinic City`))


police_data$department <- str_trim(police_data$department)
police_data$department[c(3, 13, 16, 27, 45, 49, 68, 80, 103, 42, 156, 213, 179, 137)] <- 
  c("Preston", "Shoreview", "Chanhassen", "North Oaks", "Vadnais Heights", "East Bethel", 
    "New Brighton", "Little Canada", "Minnetrista", "Andover", 
    "Inver Grove Heights", "Lauderdale", "Lexington", "Mankato")
colnames(police_data)[3] <- "police_score"
covid_data <- covid_data %>% 
  filter(Province_State == "Minnesota") %>% 
  select(c(1:11, `X12.31.21`)) %>%
  rename(covid_count21 = X12.31.21)
educ_attain_data <- as.data.frame(t(educ_attain_data[c(1,17), -1])) %>% 
  dplyr::filter(str_detect(`1`, "County, Minnesota!!Percent!!Estimate")) %>%
  mutate(county = word(`1`, 1, sep = " County"),
         educ_pct = as.numeric(word(`17`, 1, sep = "%")))
transit_data <- transit_data %>% group_by(COUNTYFP) %>% summarize(mean_walk_score = mean(NatWalkInd))
home_data <- home_data %>% 
  filter(row_number() %% 7 == 1 | row_number() %% 7 == 2) %>% 
  filter(row_number() != 175 & row_number() != 176)
home_data <- cbind(home_data %>% select(1) %>% filter(row_number() %% 2 == 1), 
                   home_data %>% select(2) %>% filter(row_number() %% 2 == 0)) %>%
  rename(county = X, homeownershiprate = X2017.2021) %>%
  mutate(homeownershiprate = as.numeric(word(homeownershiprate, 1, sep = "%")))

data_phq9 <- quality_data_adj %>% dplyr::filter(str_detect(`Measure Name`, "PHQ-9") & `Clinic Name` != "TOTAL")


# combining data
combined_data <- data_phq9 %>%
  left_join(utilization_data, by = c("Medical Group Name", "Measurement Year")) %>%
  left_join(zip_data, by = c('Clinic Zip Code' = 'Zip.Code')) %>%
  left_join(police_data, by = c('Clinic City' = 'department')) %>%
  left_join(social_cap_data, by = c('FIPS.County.Code' = 'county.fips')) %>%
  left_join(covid_data, by = c('County' = 'Admin2')) %>%
  left_join(aqi_data, by = "County") %>%
  left_join(transit_data, by = c('FIPS.County.Code' = 'COUNTYFP')) %>%
  left_join(educ_attain_data, by = c('County' = 'county')) %>%
  left_join(home_data, by = c('County' = 'county')) %>%
  select("Measurement Year", "Measure Name", "Statewide Average", "Medical Group Name", "Clinic Name", 
         "Clinic City", "Clinic Zip Code", "Denominator", "Actual Rate", "Expected Rate", "Ratio", "Healthscore Rating",
         "County", "Metropolitan.Micropolitan.Statistical.Area", "FIPS.County.Code", "police_score", "sk2014",
         "covid_count21", "Days.with.AQI", "Good.Days", "Unhealthy.Days", "Max.AQI", "Median.AQI",
         #"population", "households", "alltransit_performance_score", 
         "educ_pct", "mean_walk_score", "homeownershiprate")

write.csv(combined_data, "Data/combined_data.csv")

# looking at correlation matrix, suuuuper rough because only looks at complete cases
numeric_data <- combined_data %>% 
  dplyr::select(where(is.numeric))

cormat <- round(cor(numeric_data, use = "complete.obs"), 2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


# very basic regression
summary(lm(`Actual Rate` ~ `Adults TCOC` + 
             police_score + sk2014 + total_covid_count21 + 
             Median.AQI, data = combined_data))






#########################################


