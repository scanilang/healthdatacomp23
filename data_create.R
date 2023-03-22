library(dplyr)
library(openxlsx)
library(stringr)

# create crosswalk
# micro/metro/csa
metro_county <- read.xlsx("Data/list1_2020.xlsx", startRow = 3) %>% 
  filter(FIPS.State.Code == 27) %>% 
  mutate(County = str_sub(`County/County.Equivalent`,end= -8))

# zip/city/county
zip_city_county <- read.xlsx("Data/zip_city_county.xlsx") 

crosswalk <- zip_city_county %>% 
  left_join(metro_county)

write.csv(crosswalk, "Data/zip_crosswalk.csv")

# police data
police_score <- read.xlsx("Data/scorecard.xlsx") %>% 
  mutate(score = str_split_i(POLICE.DEPARTMENT, "\\* ", 2),
         score2 = str_split_i(POLICE.DEPARTMENT, "\\d+. ", 2)) %>% 
  mutate(city = if_else(is.na(score), score2, score),
         score = str_extract(city, "\\d+"),
         department = str_sub(city,end = -4)) %>% 
  select(department, score)

write.csv(police_score, "Data/department_score.csv")
