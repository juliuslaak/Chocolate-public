# Merge two data files and save to new file

rm(list=ls())

saas.data <- read.csv("initial_saas_data.csv", sep=";")
saas.user <- read.csv("saas-user_excel.csv", sep=";")

#### Data manipulation, cleaning

initial.data <- merge(saas.data, saas.user, by.x="user_id", by.y="USER.ID", all.x=TRUE)
# Throw out some columns
initial.data <- subset(initial.data, select = -c(
  USER.TAGS.Tag.Name,
  test_completion_status,
  days_elapsed,
  MARKETING.ATTRIBUTION.Tag.Channel.Level.1,
  MARKETING.ATTRIBUTION.Tag.Channel.Level.2,
  MARKETING.ATTRIBUTION.Channel.Level.3,
  USER.PROFILE.Business.Name))

data <- initial.data
save(data, file="saas-merged-data.RData")
