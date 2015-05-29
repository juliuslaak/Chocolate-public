# Clean data and replace names
rm(list=ls())

load("saas-merged-data.RData")

# Change count-transfer-before/after and
# count-invites-before/after to binary values
data$count_transfers_prior[data$count_transfers_prior>0] <- 1
data$count_transfers_after[data$count_transfers_after>0] <- 1
data$count_invites_prior[data$count_invites_prior>0] <- 1
data$count_invites_after[data$count_invites_after>0] <- 1

# Change transfer value-before/after to 
# 0-500 -> 500
# 500-3000 -> 3000
# 3000+ -> 10000
data$value_transfers_prior <- as.numeric(gsub(",", ".", as.vector(data$value_transfers_prior)))
data$value_transfers_prior[data$value_transfers_prior <= 500 & data$value_transfers_prior !=0] <- 500
data$value_transfers_prior[data$value_transfers_prior > 500 & data$value_transfers_prior <= 3000] <- 3000
data$value_transfers_prior[data$value_transfers_prior > 3000] <- 10000

data$value_transfers_after <- as.numeric(gsub(",", ".", as.vector(data$value_transfers_after)))
data$value_transfers_after[data$value_transfers_after <= 500 & data$value_transfers_after !=0] <- 500
data$value_transfers_after[data$value_transfers_after > 500 & data$value_transfers_after <= 3000] <- 3000
data$value_transfers_after[data$value_transfers_after > 3000] <- 10000

data$user_set <- as.matrix(data$user_set)
data$user_set[which(data$user_set!="saas-experiment-3")] <- as.character("free transfer")
data$user_set[which(data$user_set=="saas-experiment-3")] <- as.character("chocolate")

data$NPS.Cs.Review.Score[data$NPS.Cs.Review.Score <=6] <- 6
data$NPS.Cs.Review.Score[data$NPS.Cs.Review.Score >= 7 & data$NPS.Cs.Review.Score <= 8] <- 8
data$NPS.Cs.Review.Score[data$NPS.Cs.Review.Score > 8] <- 10

data$NPS.Net.Promoter.Score[data$NPS.Net.Promoter.Score <=6] <- 6
data$NPS.Net.Promoter.Score[data$NPS.Net.Promoter.Score >= 7 & data$NPS.Net.Promoter.Score <= 8] <- 8
data$NPS.Net.Promoter.Score[data$NPS.Net.Promoter.Score > 8] <- 10

data$NPS.User.Experience.Score[data$NPS.User.Experience.Score <=6] <- 6
data$NPS.User.Experience.Score[data$NPS.User.Experience.Score >= 7 & data$NPS.User.Experience.Score <= 8] <- 8
data$NPS.User.Experience.Score[data$NPS.User.Experience.Score > 8] <- 10

data$FIRST.CURRENCY.Source <- as.matrix(data$FIRST.CURRENCY.Source)
data$FIRST.CURRENCY.Source[which(data$FIRST.CURRENCY.Source == "")] <- NA

data$FIRST.CURRENCY.Target <- as.matrix(data$FIRST.CURRENCY.Target)
data$FIRST.CURRENCY.Target[which(data$FIRST.CURRENCY.Target == "")] <- NA

data$sent_date <- as.matrix(data$sent_date)
data$sent_date <- substr(data$sent_date,1,2)
data$sent_date <- as.numeric(data$sent_date)
data$sent_date[data$sent_date < 8] <- 1
data$sent_date[data$sent_date >= 8 & data$sent_date < 15 ] <- 2
data$sent_date[data$sent_date >= 15 & data$sent_date < 22 ] <- 3
data$sent_date[data$sent_date >= 22 ] <- 4
colnames(data)[4] <- "sent_week_of_month"

# Make everything a factor for apriori
data[] <- lapply(data, factor)

data <- data[, c(2,4,7,8,9,10,12,14,18,19,20,21,25,29,30,35)]

save(data, file="saas-cleaned-data.RData")




