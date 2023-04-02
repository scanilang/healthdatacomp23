
library(readxl)
library(stringr)

utilization_data <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx")
quality_data_adj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 1)
quality_data_unadj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 2)

unique(quality_data_adj$`Medical Group Name`)
sum(unique(utilization_data$`Medical Group Name`) %in% unique(quality_data_adj$`Medical Group Name`))

util_groups <- unique(utilization_data$`Medical Group Name`)
qual_groups <- unique(quality_data_adj$`Medical Group Name`)

util_groups[which(!util_groups %in% qual_groups)]

unique(quality_data_adj$`Measure Name`[str_detect(quality_data_adj$`Measure Name`, "PHQ-9")])

index <- which(str_detect(quality_data_adj$`Measure Name`, "PHQ-9"))
t <- quality_data_adj[index,]
unique(t$`Medical Group Name`)




unique(quality_data_unadj$`Measure Name`[str_detect(quality_data_unadj$`Measure Name`, "PHQ-9")])

index <- which(str_detect(quality_data_unadj$`Measure Name`, "PHQ-9"))
s <- quality_data_unadj[index,]
unique(s$`Medical Group Name`)

t$`Clinic Name`[which(!(unique(t$`Clinic Name`) %in% unique(s$`Clinic Name`)))]



sum(unique(t$`Medical Group Name`) %in% unique(utilization_data$`Medical Group Name`))


unique(t$`Medical Group Name`)[!((unique(t$`Medical Group Name`) %in% unique(s$`Medical Group Name`)))]

