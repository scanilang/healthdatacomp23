library(dplyr)
library(openxlsx)
library(stringr)

# create crosswalk
zip_county <- read.xlsx("Data/ZIP_COUNTY_122021.xlsx") %>% 
  filter(usps_zip_pref_state == "MN") %>% 
  select(zip, county) %>% 
  mutate(county_fip = str_sub(county,start= 3),
         state_fip = str_sub(county, end = -4))

metro_county <- read.xlsx("Data/list1_2020.xlsx", startRow = 3) %>% 
  filter(FIPS.State.Code == 27)

crosswalk <- zip_county %>% 
  left_join(metro_county, by = c("county_fip" = "FIPS.County.Code",
                                 "state_fip" = "FIPS.State.Code" ))

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
