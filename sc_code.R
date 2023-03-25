library(dplyr)
library(openxlsx)
library(stringr)

# raw data
cost_util_data <- read.xlsx("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx", sheet = 1)
quality_data <- read.xlsx("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx")
crosswalk <- read.csv("Data/zip_crosswalk.csv")

############################################
# data clean
############################################
# restrict to year 2021 and clinics with phq 
# change medical group to match cost data
data_clean <- quality_data %>% 
  #filter(Measurement.Year == '2021') %>% 
  filter(Measure.Name %in% unique(quality_data$Measure.Name)[6:17]) %>% 
  mutate(Medical.Group.Name = case_when(str_detect(Clinic.Name, "EH East") == TRUE ~ "Essentia Health - East Region", # in 2019 it is East not East Region
                                        str_detect(Clinic.Name, "EH West") == TRUE ~ "Essentia Health - West",
                                        Clinic.Name == "Mayo Clinic Health System Fairmont" ~ "Mayo Clinic Health System - Fairmont",
                                        Clinic.Name == "Mayo Clinic Health System- Franciscan Healthcare in Caledonia" ~ "Mayo Clinic Health System - Franciscan Healthcare in La Cr",
                                        Clinic.Name == "Mayo Clinic Health System Lake City" ~ "Mayo Clinic Health System - Lake City",
                                        Clinic.Name == "Mayo Clinic - Northwest" ~ "Mayo Clinic Health System - Northwest Wisconsin Region",
                                        Clinic.Name == "Mayo Clinic Health System Red Wing" ~ "Mayo Clinic Health System - Red Wing",
                                        Clinic.Name == "Mayo Family Clinic Southeast" ~ "Mayo Clinic Health System - Southeast Minnesota Region",
                                        Clinic.Name == "Mayo Clinic Health System St. James" ~ "Mayo Clinic Health System - St. James",
                                        Clinic.Name == "Mayo Clinic Health System Owatonna" ~ "Mayo Clinic Health System-Owatonna ",
                                        TRUE ~ Medical.Group.Name  ),
         subject = if_else(str_detect(Measure.Name, "Adol"), "Adol", "Adult"),
         time = if_else(str_detect(Measure.Name, "12"), 12, 6),
         group = case_when(subject == "Adult" & time == 12 ~ "Adults 12 Month",
                           subject == "Adult" & time == 6 ~ "Adults 6 Month",
                           subject == "Adol" & time == 12 ~ "Adolescents 12 Month",
                           subject == "Adol" & time == 6 ~ "Adolescents 6 Month")) %>% 
  left_join(crosswalk, by = c("Clinic.Zip.Code" = "Zip.Code"))

############################################
# plots
############################################
library(ggplot2)

## Histograms
data_clean %>% 
  filter(Clinic.Name == "TOTAL") %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate") %>% 
  group_by(group) %>% 
  mutate(avg = mean(Actual.Rate)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Actual.Rate)) +
  geom_histogram(aes(y= ..density..))+
  geom_density(alpha=.2, fill="#FF6666") + 
  theme(axis.title.x = element_blank()) +
  geom_vline(aes(xintercept=avg), color="black", linetype="dashed", size=1) +
  ggtitle("Distribution of Actual PHQ-9 Follow-up Rate by Medical Group (2021) ")+
  facet_wrap(~group)

data_clean %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  filter(Measurement.Year == 2019) %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate") %>% 
  group_by(group) %>% 
  mutate(avg = mean(Actual.Rate)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Actual.Rate)) +
  geom_histogram(aes(y= ..density..))+
  geom_density(alpha=.2, fill="#FF6666") + 
  theme(axis.title.x = element_blank()) +
  geom_vline(aes(xintercept=avg), color="black", linetype="dashed", size=1) +
  ggtitle("Distribution of Actual PHQ-9 Follow-up Rate by Clinic")+
  facet_wrap(~group)


data_clean %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  #filter(Measurement.Year == 2021) %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate") %>% 
  group_by(group) %>% 
  mutate(avg = mean(Actual.Rate)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Actual.Rate)) +
  geom_histogram(aes(y= ..density..))+
  geom_density(alpha=.2, fill="#FF6666") + 
  theme(axis.title.x = element_blank()) +
  geom_vline(aes(xintercept=avg), color="blue1", linetype="dashed", size=1, alpha = .6) +
  ggtitle("Clinic Distribution of PHQ-9 Follow-up Actual Rate")+
  facet_wrap(~group)


data_clean %>% 
  filter(Clinic.Name == "TOTAL") %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate") %>% 
  group_by(group) %>% 
  mutate(avg = mean(Ratio)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Ratio)) +
  geom_histogram(aes(y= ..density..))+
  geom_density(alpha=.2, fill="#FF6666") + 
  theme(axis.title.x = element_blank()) +
  geom_vline(aes(xintercept=avg), color="black", linetype="dashed", size=1) +
  ggtitle("Distribution of Ratio PHQ-9 Follow-up Rate by Medical Group (2021) ")+
  facet_wrap(~group)

data_clean %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate") %>% 
  group_by(group) %>% 
  mutate(avg = mean(Ratio)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Ratio)) +
  geom_histogram(aes(y= ..density..))+
  geom_density(alpha=.2, fill="#FF6666") + 
  theme(axis.title.x = element_blank()) +
  geom_vline(aes(xintercept=avg), color="blue1", linetype="dashed", size=1, alpha = .6) +
  ggtitle("Clinic Distribution of PHQ-9 Follow-up Rate Ratio")+
  xlab("Follow-up Ratio") +
  facet_wrap(~group)

#### Box Plots
data_clean %>% 
  filter(Clinic.Name == "TOTAL") %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  ggplot(aes(x = Actual.Rate, y = Measure.Name, fill = Measure.Name)) +
  geom_boxplot() + coord_flip() +
  theme(axis.title.x = element_blank(),
        legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  ggtitle("Depression Measures by Medical Group (2021) ")+
  facet_wrap(~group)

data_clean %>% 
  filter(Clinic.Name == "TOTAL") %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  ggplot(aes(x = Ratio, y = Measure.Name, fill = Measure.Name)) +
  geom_boxplot() + coord_flip() +
  theme(axis.title.x = element_blank(),
        legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  ggtitle("Depression Measures by Medical Group (2021) ")+
  facet_wrap(~group)

data_clean %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  rename(`Acutal Rate` = Actual.Rate) %>% 
  ggplot(aes(x = `Acutal Rate`, y = Measure.Name, fill = Measure.Name)) +
  geom_boxplot() + coord_flip() +
  theme(axis.title.x = element_blank(),
        legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  ggtitle("Clinic Depression Measures")+
  facet_wrap(~group)

data_clean %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  ggplot(aes(x = Ratio, y = Measure.Name, fill = Measure.Name)) +
  geom_boxplot() + coord_flip() +
  theme(axis.title.x = element_blank(),
        legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  ggtitle("Clinic Depression Measures")+
  facet_wrap(~group)

############################################
# Heat Map
############################################
library(maps)

library(tidycensus)
options(tigris_use_cache = TRUE)

# by zip code 
follow_up_by_zip <- data_clean %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult") %>% 
  group_by(Clinic.Zip.Code, Measure.Name) %>%  
  summarize(avg_ratio = mean(Ratio),
            avg_actual = mean(Actual.Rate),
            sum  = sum(Denominator),
            sd = sd(Ratio))

income <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B19013_001",
  year = 2019,
  geometry = TRUE
)

Minnesota <- income %>% 
  mutate(zip = str_replace(NAME, "ZCTA5 ", ""),
         zip = as.numeric(zip))  %>% 
  filter(zip >= 55001 & zip <= 56763) %>%  # keep minnesota data only
  left_join(follow_up_by_zip, by = c("zip" = "Clinic.Zip.Code")) %>% 
  rename(`Actual Rate Average` = avg_actual)

library(mapview)

mapview(mn_income, zcol = "avg_ratio")

mapview(Minnesota, zcol = "Actual Rate Average")

mapview(mn_income, zcol = "sum")


############################################
# Association with outcomes
############################################

# follow up ratio vs outcome ratios
totals <- data_clean %>%  
  filter(Clinic.Name == "TOTAL", Measure.Name == "PHQ-9 Follow-up Rate at 12 Months Adult") %>% 
  select(Measurement.Year, Medical.Group.Name, Denominator, Actual.Rate, Expected.Rate, Ratio) 

location_main <- data_clean %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 12 Months Adult") %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  group_by(Medical.Group.Name, County) %>% 
  summarize(n = n())  %>% 
  arrange(Medical.Group.Name, desc(n)) %>% 
  ungroup() %>% 
  distinct(Medical.Group.Name, .keep_all = T) %>% 
  left_join(cost_util_data %>% filter(Measurement.Year == 2021)) %>% 
  left_join(totals)

# those with higher follow up ratio have lower ER ratio, lower inpatient.admissions ratio
location_main %>% 
  ggplot(aes(x = Ratio, y = ER.Visits.Ratio)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Ratio vs ER Visits Ratio") +
  xlab("PHQ-9 Follow up Rate Ratio") +
  ylab("ER Visits Ratio")

location_main %>% 
  ggplot(aes(x = Ratio, y = Inpatient.Admission.Ratio)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Low Follow up Rates Associated with Higher Inpatient Admission")+
  xlab("PHQ-9 Follow up Rate Ratio") +
  ylab("Inpatient Admission Ratio")

location_main %>% 
  #filter(Primary.Care.Visits.Ratio < 1.2) %>% 
  ggplot(aes(x = Ratio, y = Primary.Care.Visits.Ratio)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Ratio vs Primary Care Ratio")

mod1 <- lm(Ratio ~ ER.Visits.Ratio + Inpatient.Admission.Ratio, 
           data = location_main )
summary(mod1)

mod1 <- lm(Ratio ~ Inpatient.Admission.Ratio, 
           data = location_main)
summary(mod1)

# county performance
follow_up_county <- data_clean %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult") %>% 
  filter(Clinic.Name != "TOTAL") %>%
  group_by(County, Measurement.Year) %>% 
  summarize(avg_actual = mean(Actual.Rate),
            avg_ratio = mean(Ratio))

outcome_county <- location_main %>% 
  group_by(County) %>%  
  summarize(avg_inpatient = mean(Inpatient.Admission.Ratio),
            avg_ER = mean(ER.Visits.Ratio))

country_perf <- left_join(follow_up_county, outcome_county)

country_perf %>% 
  filter(avg_ratio > 0.4) %>% 
  ggplot(aes(x = avg_ratio, y = avg_inpatient)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Ratio vs Inpatient Admission Ratio (County)")

country_perf %>% 
  filter(avg_ratio > 0.4) %>% 
  ggplot(aes(x = avg_ratio, y = avg_ER)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Ratio vs Inpatient Admission Ratio (County)")

mod2 <- lm(avg_ratio ~ avg_ER + avg_inpatient, 
           data = country_perf  %>%  filter(avg_ratio > 0.4) )
summary(mod2)

# city performance

location_main_city <- data_clean %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult") %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  group_by(Medical.Group.Name, City) %>% 
  summarize(n = n())  %>% 
  arrange(Medical.Group.Name, desc(n)) %>% 
  ungroup() %>% 
  distinct(Medical.Group.Name, .keep_all = T) %>% 
  left_join(cost_util_data %>% filter(Measurement.Year == 2021)) %>% 
  left_join(totals)

follow_up_city <- data_clean %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult") %>% 
  filter(Clinic.Name != "TOTAL") %>%
  group_by(City, Measurement.Year) %>% 
  summarize(avg_actual = mean(Actual.Rate),
            avg_ratio = mean(Ratio),
            avg_denom = mean(Denominator))

outcome_city <- location_main_city %>% 
  group_by(City) %>%  
  summarize(avg_inpatient = mean(Inpatient.Admission.Ratio),
            avg_ER = mean(ER.Visits.Ratio),
            sum_inpatient = sum(Inpatient.Admissions.Actual),
            sum_ER = sum(ER.Visits.Actual),
            avg_inpat_actual = mean(Inpatient.Admissions.Actual))

city_perf <- left_join(follow_up_city, outcome_city)

city_perf %>% 
  ggplot(aes(x = avg_actual, y = avg_inpatient)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Actual Ratio vs Inpatient Admission Ratio (City)")

city_perf %>% 
  ggplot(aes(x = avg_denom, y = avg_inpat_actual)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Actual Ratio vs Inpatient Admission Ratio (City)")

city_perf %>% 
  ggplot(aes(x = avg_ratio, y = avg_ER)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Ratio vs Inpatient Admission Ratio (City)")

city_perf %>% 
  filter(sum_denom < 500) %>% 
  ggplot(aes(x = sum_denom, y = sum_inpatient)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Ratio vs Inpatient Admission Ratio (City)")

city_perf %>% 
  #filter(sum_denom < 500, sum_ER < 250) %>% 
  ggplot(aes(x = avg_ratio, y = sum_ER)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() + 
  ggtitle("Ratio vs Inpatient Admission Ratio (City)")

mod3 <- lm(avg_ratio ~ avg_ER + avg_inpatient, 
           data = city_perf)
summary(mod3)

