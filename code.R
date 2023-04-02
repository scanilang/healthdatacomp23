all_clinics <- quality %>% 
  filter(!is.na(`Clinic City`)) %>%
  group_by(`Measure Name`, `Clinic Name`) %>%
  filter(n() == 2)


all_clinics_mutated <- all_clinics %>%
  group_by(`Measure Name`, `Clinic Name`) %>%
  mutate(change = diff(Denominator),
         change_prop = diff(Denominator)/Denominator)

all_clinics_mutated <- all_clinics_mutated[order(all_clinics_mutated$`Clinic Name`, all_clinics_mutated$`Measure Name`),]
all_clinics_noreplication <- all_clinics_mutated[seq(1,10466,2),]


mean(all_clinics_noreplication$change)
sd(all_clinics_noreplication$change)
mean(all_clinics_noreplication$change_prop)
sd(all_clinics_noreplication$change_prop)




big_clinics <- all_clinics_mutated %>%
  filter(grepl("Mayo",`Medical Group Name`) + grepl("Park Nicollet",`Medical Group Name`) + grepl("United",`Medical Group Name`) + grepl("Fairview",`Medical Group Name`) + grepl("Essentia",`Medical Group Name`) +grepl("St. Luke's",`Medical Group Name`) + grepl("HealthPartners",`Medical Group Name`) > 0)

big_clinics_mutated <- big_clinics %>%
  group_by(`Measure Name`, `Clinic Name`) %>%
  mutate(la = diff(Denominator),
         prop = diff(Denominator)/Denominator)




big_clinics_mutated <- big_clinics_mutated[order(big_clinics_mutated$`Clinic Name`, big_clinics_mutated$`Measure Name`),]
big_clinic_noreplication <- big_clinics_mutated[seq(1,3898,2),]

mean(big_clinic_noreplication$change)
sd(big_clinic_noreplication$change)
mean(big_clinic_noreplication$change_prop)
sd(big_clinic_noreplication$change_prop)