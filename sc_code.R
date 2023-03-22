library(dplyr)
library(openxlsx)
library(stringr)

# raw data
cost_util_data <- read.xlsx("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx", sheet = 1)
quality_data <- read.xlsx("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx")

############################################
# data clean
############################################
# restrict to year 2021 and clinics with phq 
# change medical group to match cost data
data_clean <- quality_data %>% 
  filter(Measurement.Year == '2021') %>% 
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
                           subject == "Adol" & time == 6 ~ "Adolescents 6 Month"))

# police data
police_score <- read.xlsx("Data/scorecard.xlsx") %>% 
  mutate(score = str_split_i(POLICE.DEPARTMENT, "\\* ", 2),
         score2 = str_split_i(POLICE.DEPARTMENT, "\\d+. ", 2)) %>% 
  mutate(city = if_else(is.na(score), score2, score),
         score = str_extract(city, "\\d+"),
         department = str_sub(city,end = -4)) %>% 
  select(department, score)

write.csv(police_score, "Data/department_score.csv")

############################################
# compare follow up with cost
############################################
# by zip code 
follow_up_by_zip <- data_clean %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult") %>% 
  group_by(Clinic.Zip.Code, Measure.Name) %>%  
  summarize(avg_ratio = mean(Ratio),
            avg_actual = mean(Actual.Rate),
            sum  = sum(Denominator))

# by institution
follow_up_by_group <- data_clean %>% 
  filter(Clinic.Name == "TOTAL") %>% 
  group_by(Medical.Group.Name, Measure.Name) %>%  
  summarize(avg = mean(Ratio)) %>% 
  left_join(cost_util_data %>% filter(Measurement.Year == 2021), by = c("Medical.Group.Name")) %>% 
  tidyr::pivot_wider(names_from = "Measure.Name", values_from = "avg")


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
  ggtitle("Distribution of Actual PHQ-9 Follow-up Rate by Clinic (2021) ")+
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
  geom_vline(aes(xintercept=avg), color="black", linetype="dashed", size=1) +
  ggtitle("Distribution of Ratio PHQ-9 Follow-up Rate by Clinic (2021) ")+
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
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  ggplot(aes(x = Actual.Rate, y = Measure.Name, fill = Measure.Name)) +
  geom_boxplot() + coord_flip() +
  theme(axis.title.x = element_blank(),
        legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  ggtitle("Depression Measures by Clinic (2021) ")+
  facet_wrap(~group)

data_clean %>% 
  filter(Clinic.Name != "TOTAL") %>% 
  mutate(Measure.Name = case_when(str_detect(Measure.Name, "Depression Remission") ~ "Depression Remission Rate",
                                  str_detect(Measure.Name, "Depression Response") ~ "Depression Response Rate",
                                  str_detect(Measure.Name, "PHQ-9 Follow-up Rate") ~ "PHQ-9 Follow-up Rate",
                                  TRUE ~ Measure.Name)) %>% 
  ggplot(aes(x = Ratio, y = Measure.Name, fill = Measure.Name)) +
  geom_boxplot() + coord_flip() +
  theme(axis.title.x = element_blank(),
        legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  ggtitle("Depression Measures by Clinic (2021) ")+
  facet_wrap(~group)

############################################
# Heat Map
############################################
library(maps)

library(tidycensus)
options(tigris_use_cache = TRUE)

income <- get_acs(
  geography = "zip code tabulation area", 
  variables = "B19013_001",
  year = 2019,
  geometry = TRUE
)

mn_income <- income %>% 
  mutate(zip = str_replace(NAME, "ZCTA5 ", ""),
         zip = as.numeric(zip))  %>% 
  filter(zip >= 55001 & zip <= 56763) %>%  # keep minnesota data only
  left_join(follow_up_by_zip, by = c("zip" = "Clinic.Zip.Code"))

library(mapview)

mapview(mn_income, zcol = "avg_ratio")

mapview(mn_income, zcol = "avg_actual")

mapview(mn_income, zcol = "sum")


############################################
# Association with outcomes
############################################

# idea: zip codes with low follow up rates 
# associated with hospitals in the same zip with high ER and inpatient admission ratio
follow_up_by_group %>% 
  filter(Adults.TCOC < 1200) %>% # remove outlier
  ggplot(aes(x = `PHQ-9 Follow-up Rate at 12 Months Adult`, y = `Adults.TCOC`)) +
  geom_point()
  
follow_up_by_group %>% 
  ggplot(aes(x = `PHQ-9 Follow-up Rate at 12 Months Adult`, y = `Inpatient.Admission.Ratio`)) +
  geom_point()

follow_up_by_group %>% 
  ggplot(aes(x = `PHQ-9 Follow-up Rate at 12 Months Adult`, y = `ER.Visits.Ratio`)) +
  geom_point()

follow_up_by_zip %>% 
  ggplot(aes(x = `PHQ-9 Follow-up Rate at 12 Months Adult`, y = `ER.Visits.Ratio`)) +
  geom_point()
