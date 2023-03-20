
library(readxl)

utilization_data <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Cost & Utilization Data Set_3.2.2023.xlsx")
quality_data_adj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 1)
quality_data_unadj <- read_excel("Data/BACH Competition Data Set_3.2.2023/BACH Competition_Quality Measures Data Set_3.2.2023.xlsx", sheet = 2)

unique(quality_data_adj$`Medical Group Name`)
sum(unique(utilization_data$`Medical Group Name`) %in% unique(quality_data_adj$`Medical Group Name`))

util_groups <- unique(utilization_data$`Medical Group Name`)
qual_groups <- unique(quality_data_adj$`Medical Group Name`)

util_groups[which(!util_groups %in% qual_groups)]