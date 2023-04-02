library(dplyr)
library(estimatr)
library(ggeffects)
library(ggplot2)

combinedata <- read.csv("Data/combined.data.csv", skip = 1)
cost_util_data <- read.xlsx("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx", sheet = 1) %>% 
  mutate(Measurement.Year = as.factor(Measurement.Year))

quality_data <- read.xlsx("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx") %>% 
  mutate(Measurement.Year = as.factor(Measurement.Year))

combinedata <- combinedata %>% 
  filter(Measure.Name == "PHQ-9 Follow-up Rate at 6 Months Adult") %>% 
  mutate(Actual.Rate = as.numeric(Actual.Rate),
         educ_pct = as.numeric(educ_pct),
         covid_count21 =  as.numeric(educ_pct),
         mean_walk_score = as.numeric(mean_walk_score),
         police_score = as.numeric(police_score)) %>% 
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
                                        TRUE ~ Medical.Group.Name  )) 
  
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
vars = c('homeownershiprate',"covid_count21", "educ_pct", "mean_walk_score", "police_score")
table = NULL

for(i in vars){
  
  if(i != "police_score"){
    print(i)
    
  mod = lm_robust(scale(combinedata$Actual.Rate) ~ scale(combinedata[[paste(i)]]),
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

table$characteristic = c("Home Ownership Rate", "Covid Cases", "Education Attainment", 
                         "Mean Walk Score", "Police Score")


ggplot(table, aes(x=characteristic, y=correlation, ymin=lowerci, ymax=upperci))+
  geom_pointrange()+
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 0, linetype=2)+
  coord_flip()+
 # ggtitle("Correlates of PHQ-9 Follow Up Rates") +
  ylab("Correlation") +
  labs(caption = "Note: This figure shows the correlation of various community characteristics with PHQ-9 follow-up 
rate. The horizontal bars show the 95% confidence interval based on standard errors clustered 
by zip code.")+ 
  theme(plot.caption = element_text(hjust = 0),
        axis.title.y  = element_blank())

####### Institution
vars = c("Adults.TCOC", "ER.Visits.Ratio", "Inpatient.Admission.Ratio", "Resource.Use.Index",
         "Primary.Care.Visits.Ratio", "Pharmacy.Use.Ratio")
table = NULL

for(i in vars){
  print(i)
  
  mod = lm_robust(scale(combinedata$Ratio) ~ scale(combinedata[[paste(i)]]))
  
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

ggplot(table, aes(x=characteristic, y=correlation, ymin=lowerci, ymax=upperci))+
  geom_pointrange()+
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 0, linetype=2)+
  coord_flip()+
  ylab("Correlation") +
  labs(caption = "Note: This figure shows the correlation of various Medical Group Characteritics with 
PHQ-9 follow-up ratio.")+ 
  theme(plot.caption = element_text(hjust = 0),
        axis.title.y  = element_blank())

