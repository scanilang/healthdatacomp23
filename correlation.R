library(dplyr)
library(estimatr)
library(ggeffects)
library(ggplot2)

combinedata <- read.csv("Data/tidycensus_data.csv") %>% 
  mutate(Measurement.Year = as.factor(Measurement.Year))
cost_util_data <- read.xlsx("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx", sheet = 1) %>% 
  mutate(Measurement.Year = as.factor(Measurement.Year))

quality_data <- read.xlsx("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx") %>% 
  mutate(Measurement.Year = as.factor(Measurement.Year))

combinedata <- combinedata %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult") %>% 
  mutate(Actual.Rate = as.numeric(Actual.Rate)) %>% 
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
                                        TRUE ~ Medical.Group.Name  )) %>% 
  left_join(cost_util_data, by = c("Measurement.Year", "Medical.Group.Name")) %>% 
  left_join(quality_data %>% filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult") %>% 
              select(Measurement.Year, Medical.Group.Name, Clinic.Name, Ratio)) %>% 
  rename(educ_social_health = idk_rate)
  
medgroup <- quality_data %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult",
         Clinic.Name == "TOTAL") %>% 
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
                                        TRUE ~ Medical.Group.Name  )) %>% 
  left_join(cost_util_data ) 

######## Community Characteristics
vars = c('home_ownership_rate',"covid_rate", "educ_attainment_rate", 
         "insurance_coverage_rate", "public_fqhc_ind", "private_vehicle_rate", "code",
          "aggregate_travel_time_rate", "public_transit_to_work_rate",
         "educ_social_health")
combinedata <- combinedata %>% 
  filter(Clinic.Zip.Code != 55905)
table = NULL

for(i in vars){
  
  if(i != "police_score"){
    print(i)
    
  mod = lm_robust(scale(combinedata$Actual.Rate) ~ scale(combinedata[[paste(i)]]) + combinedata$Clinic.Zip.Code,
                   clusters = combinedata$Clinic.Zip.Code)
  } else{
    mod = lm_robust(scale(combinedata$Actual.Rate) ~ scale(combinedata[[paste(i)]]),
                    clusters = combinedata$Clinic.City)
  }
  
  temp <- data.frame(characteristic = paste(i),
                     correlation = mod$coefficients[2],
                     lowerci = mod$conf.low[2],
                     upperci = mod$conf.high[2])
  table <- rbind(table, temp) 
}

table$characteristic = c("Home Ownership", "Covid-19", "Educational Attainment", 
                         "Insurance Coverage", "Government or FQHC", "Private Vehicle", "Rural or Urban",
                         "Travel Time to Work", "Public Transit to Work",
                         "Educational Services,\nHealth Care, Social Assistance\n Non-Profit Employees")

table_ordered <- table %>%
  mutate(characteristic = factor(characteristic, levels = c("Educational Attainment", 
                                                            "Educational Services,\nHealth Care, Social Assistance\n Non-Profit Employees",
                                                            "Insurance Coverage", "Covid-19", "Government or FQHC", "Home Ownership",
                                                            "Rural or Urban", "Public Transit to Work", "Private Vehicle", "Travel Time to Work"))) %>%
  arrange(characteristic)
  
  
p <- ggplot(table_ordered, aes(x = characteristic, y = correlation, ymin = lowerci, ymax = upperci)) +
  geom_linerange(color = "#7a0019", size = 1.5) +
  geom_point(color = "#7a0019", size = 4) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylim(-1, 1) +
  coord_flip() +
  ylab("Correlation") +
  # labs(caption = "Note: This figure shows the correlation of various community characteristics with PHQ-9 6 month adult follow-up rate\n The horizontal bars show the 95% confidence interval based on standard errors clustered by zip code.")+ 
  theme(plot.caption = element_text(hjust = 0)) + 
  theme_minimal(base_size = 16) +
  xlab("")

ggsave("social_corr.png", p, bg = "white", width = 10, height = 8)



cor.test(combinedata$Actual.Rate, combinedata$educ_attainment_rate)
test = lm(combinedata$Actual.Rate ~ combinedata$educ_attainment_rate)
summary(test)


####### Institution
vars = c("Adults.TCOC.x", "ER.Visits.Ratio", "Inpatient.Admission.Ratio", "Resource.Use.Index",
         "Primary.Care.Visits.Ratio", "Pharmacy.Use.Ratio")
table = NULL

for(i in vars){
  print(i)
  
  mod = lm_robust(scale(combinedata$Ratio) ~ scale(combinedata[[paste(i)]]) + combinedata$Clinic.Zip.Code,
                  cluster = combinedata$Clinic.Zip.Code)
  
  temp <- data.frame(characteristic = paste(i),
                     correlation = mod$coefficients[2],
                     lowerci = mod$conf.low[2],
                     upperci = mod$conf.high[2])
  table <- rbind(table, temp) 
}

table$characteristic = c("Adults TCOC",
                         "ER Visits Ratio",
                         "Inpatient Admission Ratio",
                         "Resource Use Index",
                         "Primary Care Visits Ratio",
                         "Pharmacy Use Ratio")

table_ordered <- table %>%
  mutate(characteristic = factor(characteristic, levels = c("Adults TCOC", "ER Visits Ratio", "Inpatient Admission Ratio",
                                                            "Pharmacy Use Ratio", "Primary Care Visits Ratio", "Resource Use Index"))) %>%
  arrange(characteristic)


p <- ggplot(table_ordered, aes(x = characteristic, y = correlation, ymin = lowerci, ymax = upperci)) +
  geom_linerange(color = "#7a0019", size = 1.5) +
  geom_point(color = "#7a0019", size = 4) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylim(-1, 1) +
  coord_flip() +
  ylab("Correlation") +
  # labs(caption = "Note: This figure shows the correlation of various community characteristics with PHQ-9 6 month adult follow-up rate\n The horizontal bars show the 95% confidence interval based on standard errors clustered by zip code.")+ 
  theme(plot.caption = element_text(hjust = 0)) + 
  theme_minimal(base_size = 16) +
  xlab("")

ggsave("medgroup_corr.png", p, bg = "white", width = 10, height = 8)



test = lm(scale(Actual.Rate) ~ scale(Adults.TCOC), data = combinedata)
summary(test)
