##############
# Data Mining project
# May 2015
# Laak, Jerkku
##############

rm(list=ls())
if(!require(arules)) {install.packages("arules"); require(arules)}
if(!require(arulesViz)) {install.packages("arulesViz"); require(arulesViz)}

load("saas-cleaned-data.RData")

# FIND OVERALL RULES
countInvitesTransferCountClassification <-
  apriori( data, parameter = list(minlen=2, maxlen=5, supp=0.1, conf=0.6),
           appearance = list(
             rhs=c(
               "count_invites_classification=2a. 50+% reduction",
               "count_invites_classification=2c. 50+% increase",
               "count_invites_classification=2b. Similar (+/- 50%) ",
               "count_invites_classification=3. Started",
               "count_invites_classification=0. No activity",
               "count_invites_classification=1. Stopped",
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=3. Started",
               "transfer_value_classification=0. No activity",
               "transfer_value_classification=1. Stopped",
               "transfer_count_classification=2a. 50+% reduction",
               "transfer_count_classification=2c. 50+% increase",
               "transfer_count_classification=2b. Similar (+/- 50%) ",
               "transfer_count_classification=3. Started",
               "transfer_count_classification=0. No activity",
               "transfer_count_classification=1. Stopped"
             ), default="lhs") )

# SUBSET FOR RHS, LHS
subset.countInvitesTransferCountClassification <-
  subset(countInvitesTransferCountClassification, 
         lhs %pin% "user_set=" &
         !lhs %pin% "transfer_count_classification=" & 
           !lhs %pin%"transfer_value_classification=" & 
           !lhs %pin%"count_invites_classification=" )

# SORT BY INTERESTINGNESS MEASURE
count.sorted <- sort(subset.countInvitesTransferCountClassification, na.last=NA, by="confidence", decreasing=TRUE)

# Inspect first 10 or so
inspect(head(count.sorted, 50))

# Include sex
countInvitesTransferCountClassification <- 
  apriori( data, parameter = list(minlen=2, maxlen=3, supp=0.001, conf=0.006),
           appearance = list(
             lhs=c("user_set=chocolate","user_set=free transfer", "Sex=M", "Sex=F"),
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=3. Started",
               "transfer_value_classification=0. No activity",
               "transfer_value_classification=1. Stopped"
             ), default="none") )

# SORT BY INTERESTINGNESS MEASURE
count.sorted <- sort(countInvitesTransferCountClassification, na.last=NA, by="confidence", decreasing=TRUE)

# Inspect first 10 or so
inspect(head(count.sorted, 50))

# INITIAL COMPARISSON
# CHOCOLATE IS BETTER
1  {user_set=free transfer} => {transfer_value_classification=1. Stopped}             0.23471400 0.42348754 1.0175743
2  {user_set=chocolate}     => {transfer_value_classification=1. Stopped}             0.18145957 0.40707965 0.9781487

3  {user_set=free transfer} => {transfer_value_classification=0. No activity}         0.21301775 0.38434164 1.0310117
4  {user_set=chocolate}     => {transfer_value_classification=0. No activity}         0.15976331 0.35840708 0.9614412

5  {user_set=chocolate}     => {transfer_value_classification=3. Started}             0.03944773 0.08849558 1.2819216
9  {user_set=free transfer} => {transfer_value_classification=3. Started}             0.02958580 0.05338078 0.7732588

7  {user_set=chocolate}     => {transfer_value_classification=2c. 50+% increase}      0.02761341 0.06194690 1.2562832
10 {user_set=free transfer} => {transfer_value_classification=2c. 50+% increase}      0.02169625 0.03914591 0.7938790

# CHOCOLATE IS WORSE
6  {user_set=free transfer} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.04339250 0.07829181 1.1341129
8  {user_set=chocolate}     => {transfer_value_classification=2b. Similar (+/- 50%) } 0.02564103 0.05752212 0.8332491

11 {user_set=chocolate}     => {transfer_value_classification=2a. 50+% reduction}     0.01183432 0.02654867 1.1216814
12 {user_set=free transfer} => {transfer_value_classification=2a. 50+% reduction}     0.01183432 0.02135231 0.9021352


# Including SEX (length 3)
lhs                         rhs                                                        support confidence      lift
1  {user_set=chocolate,                                                                                               
    Sex=F}                  => {transfer_value_classification=1. Stopped}             0.055226824 0.50909091 1.2232658
11 {user_set=chocolate,                                                                                               
    Sex=M}                  => {transfer_value_classification=1. Stopped}             0.110453649 0.38095238 0.9153690

3  {user_set=free transfer,                                                                                           
    Sex=F}                  => {transfer_value_classification=1. Stopped}             0.067061144 0.46575342 1.1191326
6  {user_set=free transfer,                                                                                           
    Sex=M}                  => {transfer_value_classification=1. Stopped}             0.149901381 0.41758242 1.0033852


38 {user_set=chocolate,                                                                                               
    Sex=M}                  => {transfer_value_classification=2a. 50+% reduction}     0.007889546 0.02721088 1.1496599
46 {user_set=chocolate,                                                                                               
    Sex=F}                  => {transfer_value_classification=2a. 50+% reduction}     0.001972387 0.01818182 0.7681818

36 {user_set=free transfer,                                                                                           
    Sex=F}                  => {transfer_value_classification=2a. 50+% reduction}     0.003944773 0.02739726 1.1575342
44 {user_set=free transfer,                                                                                           
    Sex=M}                  => {transfer_value_classification=2a. 50+% reduction}     0.007889546 0.02197802 0.9285714

17 {user_set=chocolate,                                                                                               
    Sex=M}                  => {transfer_value_classification=3. Started}             0.027613412 0.09523810 1.3795918
31 {user_set=chocolate,                                                                                               
    Sex=F}                  => {transfer_value_classification=3. Started}             0.005917160 0.05454545 0.7901299

21 {user_set=chocolate,                                                                                               
    Sex=M}                  => {transfer_value_classification=2c. 50+% increase}      0.023668639 0.08163265 1.6555102
47 {user_set=chocolate,                                                                                               
    Sex=F}                  => {transfer_value_classification=2c. 50+% increase}      0.001972387 0.01818182 0.3687273



# Reduction separately once again: the support is too low!

38 {user_set=chocolate,                                                                                               
    Sex=M}                  => {transfer_value_classification=2a. 50+% reduction}     0.007889546 0.02721088 1.1496599
44 {user_set=free transfer,                                                                                           
    Sex=M}                  => {transfer_value_classification=2a. 50+% reduction}     0.007889546 0.02197802 0.9285714

36 {user_set=free transfer,                                                                                           
    Sex=F}                  => {transfer_value_classification=2a. 50+% reduction}     0.003944773 0.02739726 1.1575342
46 {user_set=chocolate,                                                                                               
    Sex=F}                  => {transfer_value_classification=2a. 50+% reduction}     0.001972387 0.01818182 0.7681818

#### Amount of money ####
rules <- 
  apriori( data, parameter = list(minlen=2, maxlen=3, supp=0.01, conf=0.05),
           appearance = list(
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction", 
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ", 
               "transfer_value_classification=3. Started", 
               "transfer_value_classification=0. No activity",
               "transfer_value_classification=1. Stopped"
             ), default="lhs") )

subset.rules <-
  subset(rules, 
         lhs %pin% "user_set=" &
           lhs %pin% "value_transfers_prior=" &
           !lhs %pin% "transfer_count_classification=" & 
           !lhs %pin%"transfer_value_classification=" & 
           !lhs %pin%"count_invites_classification=" &
           !rhs %pin% "=0. No activity")

# SORT BY INTERESTINGNESS MEASURE
count.sorted <- sort(subset.rules, na.last=NA, by="confidence", decreasing=TRUE)

# Inspect first 10 or so
inspect(head(count.sorted, 50))

# STOPPED
lhs                              rhs                                                        support confidence      lift
1  {user_set=free transfer,                                                                                              
    value_transfers_prior=10000} => {transfer_value_classification=1. Stopped}             0.04142012  0.8750000 2.102488
6  {user_set=chocolate,                                                                                                  
    value_transfers_prior=10000} => {transfer_value_classification=1. Stopped}             0.04733728  0.6857143 1.647664

2  {user_set=chocolate,                                                                                                  
    value_transfers_prior=3000}  => {transfer_value_classification=1. Stopped}             0.09072978  0.7666667 1.842180
5  {user_set=free transfer,                                                                                              
    value_transfers_prior=3000}  => {transfer_value_classification=1. Stopped}             0.10453649  0.7066667 1.698009

3  {user_set=free transfer,                                                                                              
    value_transfers_prior=500}   => {transfer_value_classification=1. Stopped}             0.08875740  0.7627119 1.832677
4  {user_set=chocolate,                                                                                                  
    value_transfers_prior=500}   => {transfer_value_classification=1. Stopped}             0.04339250  0.7333333 1.762085

1 {user_set=chocolate,                                                                                  
   value_transfers_prior=0} => {transfer_value_classification=3. Started} 0.03944773  0.1980198 2.868458
2 {user_set=free transfer,                                                                              
   value_transfers_prior=0} => {transfer_value_classification=3. Started} 0.02958580  0.1219512 1.766551

#### Sex + amount of money ####
rules <- 
  apriori( data, parameter = list(minlen=4, maxlen=5, supp=0.05, conf=0.2),
           appearance = list(
             lhs=c("user_set=chocolate","user_set=free transfer", "Sex=M", "Sex=F"),
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction", 
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ", 
               "transfer_value_classification=3. Started", 
               "transfer_value_classification=0. No activity",
               "transfer_value_classification=1. Stopped"
             ), default="lhs") )

# SUBSET FOR RHS, LHS
subset.rules <-
  subset(rules, 
         lhs %pin% "user_set=" &
           lhs %pin% "Sex=" &
           !lhs %pin% "transfer_count_classification=" & 
           !lhs %pin%"transfer_value_classification=" & 
           !lhs %pin%"count_invites_classification=" &
           !rhs %pin% "=0. No activity")

# SORT BY INTERESTINGNESS MEASURE
count.sorted <- sort(subset.rules, na.last=NA, by="confidence", decreasing=TRUE)

# Value + sex
2  {user_set=chocolate,                                                                                      
    value_transfers_prior=3000,                                                                              
    Sex=F}                       => {transfer_value_classification=1. Stopped} 0.02958580  0.9375000 2.252666
6  {user_set=free transfer,                                                                                  
    value_transfers_prior=3000,                                                                              
    Sex=F}                       => {transfer_value_classification=1. Stopped} 0.02761341  0.7777778 1.868878

9  {user_set=chocolate,                                                                                      
    value_transfers_prior=3000,                                                                              
    Sex=M}                       => {transfer_value_classification=1. Stopped} 0.05128205  0.7027027 1.688485
10 {user_set=free transfer,                                                                                  
    value_transfers_prior=3000,                                                                              
    Sex=M}                       => {transfer_value_classification=1. Stopped} 0.06903353  0.7000000 1.681991

4  {user_set=chocolate,                                                                                      
    value_transfers_prior=500,                                                                               
    Sex=F}                       => {transfer_value_classification=1. Stopped} 0.01577909  0.8000000 1.922275
5  {user_set=free transfer,                                                                                  
    value_transfers_prior=500,                                                                               
    Sex=F}                       => {transfer_value_classification=1. Stopped} 0.02761341  0.7777778 1.868878

7  {user_set=free transfer,                                                                                  
    value_transfers_prior=500,                                                                               
    Sex=M}                       => {transfer_value_classification=1. Stopped} 0.05522682  0.7567568 1.818368
11 {user_set=chocolate,                                                                                      
    value_transfers_prior=500,                                                                               
    Sex=M}                       => {transfer_value_classification=1. Stopped} 0.02564103  0.6842105 1.644051


rules <- 
  apriori( data, parameter = list(minlen=4, maxlen=4, supp=0.01, conf=0.2),
           appearance = list(
             lhs=c("user_set=chocolate","user_set=free transfer", "Sex=M", "Sex=F"),
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction", 
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ", 
               "transfer_value_classification=3. Started", 
               "transfer_value_classification=0. No activity",
               "transfer_value_classification=1. Stopped"
             ), default="lhs") )

# include value_transfer_prior
subset.rules <- subset(rules, 
         lhs %pin% "value_transfers_prior=" &
           lhs %pin% "user_set=" &
           lhs %pin% "Sex=" &
           !lhs %pin% "transfer_count_classification=" & 
           !lhs %pin%"transfer_value_classification=" & 
           !lhs %pin%"count_invites_classification=" &
           !rhs %pin% "=0. No activity")

# SORT BY INTERESTINGNESS MEASURE
count.sorted <- sort(subset.rules, na.last=NA, by="confidence", decreasing=TRUE)

# Inspect first 10 or so
inspect(head(count.sorted, 50))



# This looks if there are any rules between transfer counts before complaint and the transfer classification after complaint
countInvitesTransferCountClassification <-
  apriori(data, parameter = list(minlen=2, supp=0.005, conf=0.01),
   appearance = list(
     lhs=c("count_invites_prior=0", "count_invites_prior=1"),
     rhs=c(
       "transfer_count_classification=2a. 50+% reduction",
       "transfer_count_classification=2b. Similar (+/- 50%) ",
       "transfer_count_classification=3. Started",
       "transfer_count_classification=0. No activity",
       "transfer_count_classification=1. Stopped"
       ), default="none")
)

inspect(sort(countInvitesTransferCountClassification, by="confidence"))

# We can see that wether or not you had any transfers prior to complaint, you probably stopped.
# There is a 20% confidence that the transfer number stayed the same or 15% confidence that it started(from 0).
# There is a 5% conficence that transfer count increases while 1% that it decreases
# transfer_count_classification

# This looks if there are any rules between inviting someone and the transfer classification after complaint
countInvitesTransferValueClassification <- apriori(data, parameter = list(minlen=2, supp=0.005, conf=0.01),
                 appearance = list(
                   lhs=c("count_invites_prior=0", "count_invites_prior=1"),
                   rhs=c(
                     "transfer_value_classification=2a. 50+% reduction",
                     "transfer_value_classification=2b. Similar (+/- 50%) ",
                     "transfer_value_classification=3. Started",
                     "transfer_value_classification=0. No activity",
                     "transfer_value_classification=1. Stopped"
                   ), default="none"))

inspect(sort(countInvitesTransferValueClassification, by="confidence"))

# We can see that after complaints, the invites almost certainly stop. Only 5% confidence that they start


# In conclusion we can say that invites dont affect the amount of money somebody sends
countInvitesCountClassification <- apriori(data, parameter = list(minlen=2, supp=0.005, conf=0.01),
                                                   appearance = list(
                                                     lhs=c("count_invites_prior=0", "count_invites_prior=1"),
                                                     rhs=c(
                                                       "count_invites_classification=2a. 50+% reduction",
                                                       "count_invites_classification=2b. Similar (+/- 50%) ",
                                                       "count_invites_classification=3. Started",
                                                       "count_invites_classification=0. No activity",
                                                       "count_invites_classification=1. Stopped"
                                                     ), default="none"))

inspect(sort(countInvitesCountClassification, by="confidence"))
# We can see that after complaints, the invites almost certainly stop. Only 5% confidence that they start


# Rules with all classifications
rules_no_negative_low_confidence <-
  apriori( data, parameter = list(minlen=3, maxlen=5, supp=0.1, conf=0.2),
           appearance = list(
             rhs=c(
               "count_invites_classification=2a. 50+% reduction",
               "count_invites_classification=2c. 50+% increase",
               "count_invites_classification=2b. Similar (+/- 50%) ",
               "count_invites_classification=3. Started",
               "count_invites_classification=0. No activity",
               "count_invites_classification=1. Stopped",
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=3. Started",
               "transfer_value_classification=0. No activity",
               "transfer_value_classification=1. Stopped",
               "transfer_count_classification=2a. 50+% reduction",
               "transfer_count_classification=2c. 50+% increase",
               "transfer_count_classification=2b. Similar (+/- 50%) ",
               "transfer_count_classification=3. Started",
               "transfer_count_classification=0. No activity",
               "transfer_count_classification=1. Stopped"
             ), default="lhs") )

inspect(sort(rules_no_negative_low_confidence, by="confidence"))

# SUBSET FOR RHS, LHS
subset.rules_no_negative_low_confidence <-
  subset(rules_no_negative_low_confidence,
         lhs %pin% "user_set=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=" )

length(subset.rules_no_negative_low_confidence)
inspect(sort(subset.rules_no_negative_low_confidence, by="confidence")[1:200])
inspect(sort(subset.rules_no_negative_low_confidence, by="confidence")[200:399])

# Boring results, mostly no activity or stopped

# Rules with no stopped or no activity classifications
rules_no_negative <-
  apriori( data, parameter = list(minlen=3, maxlen=5, supp=0.1, conf=0.6),
           appearance = list(
             rhs=c(
               "count_invites_classification=2a. 50+% reduction",
               "count_invites_classification=2c. 50+% increase",
               "count_invites_classification=2b. Similar (+/- 50%) ",
               "count_invites_classification=3. Started",
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=3. Started",
               "transfer_count_classification=2a. 50+% reduction",
               "transfer_count_classification=2c. 50+% increase",
               "transfer_count_classification=2b. Similar (+/- 50%) ",
               "transfer_count_classification=3. Started"
             ), default="lhs") )

# SUBSET FOR RHS, LHS
subset.rules_no_negative <-
  subset(rules_no_negative,
         lhs %pin% "user_set=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=" )

inspect(sort(subset.rules_no_negative, by="confidence"))


# Rules with no stopped or no activity classifications, very low support and confidence
rules_no_negative_low_confidence <-
  apriori( data, parameter = list(minlen=3, maxlen=5, supp=0.01, conf=0.01),
           appearance = list(
             rhs=c(
               "count_invites_classification=2a. 50+% reduction",
               "count_invites_classification=2c. 50+% increase",
               "count_invites_classification=2b. Similar (+/- 50%) ",
               "count_invites_classification=3. Started",
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=3. Started",
               "transfer_count_classification=2a. 50+% reduction",
               "transfer_count_classification=2c. 50+% increase",
               "transfer_count_classification=2b. Similar (+/- 50%) ",
               "transfer_count_classification=3. Started"
             ), default="lhs") )

# SUBSET FOR RHS, LHS
subset.rules_no_negative_low_confidence <-
  subset(rules_no_negative_low_confidence,
         lhs %pin% "user_set=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=" )

inspect(head(sort(subset.rules_no_negative_low_confidence, by="confidence"), 200))

# Lets take a deeper look at value transfer prior and age tiers
subset.rules_value_transfer_age_tiers <-
  subset(rules_no_negative_low_confidence,
         lhs %pin% "user_set=" &
           lhs %pin% "value_transfers_prior=" &
           lhs %pin% "USER.PROFILE.Age.Tiers=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=" )

inspect(head(sort(subset.rules_value_transfer_age_tiers, by="confidence"), 200))


# Lets take a deeper look at only the age tiers and user set
rules_small_len <-
  apriori( data, parameter = list(minlen=2, maxlen=3, supp=0.01, conf=0.01),
           appearance = list(
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=1. Stopped",
               "transfer_value_classification=3. Started"
             ), default="lhs") )

subset.rules_age_tiers <-
  subset(rules_small_len,
         lhs %pin% "user_set=" &
           lhs %pin% "USER.PROFILE.Age.Tiers=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_age_tiers, by="confidence"), 200))

# Lets take a deeper look at only the prior transfer value and user set

subset.rules_value_transfers_prior <-
  subset(rules_small_len,
         lhs %pin% "user_set=" &
           lhs %pin% "value_transfers_prior=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_value_transfers_prior, by="confidence"), 200))

# Lets take a deeper look at the week of the month. Maybe people are happier after payday

subset.rules_sent_week_of_month <-
  subset(rules_small_len,
         lhs %pin% "user_set=" &
           lhs %pin% "sent_week_of_month=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_sent_week_of_month, by="confidence"), 200))

# Lets take a look at the country name

subset.country_name <-
  subset(rules_small_len,
         lhs %pin% "user_set=" &
           lhs %pin% "ADDRESS.COUNTRY.Name=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.country_name, by="confidence"), 200))

# All results give united kingdom, so nothing interesting here

subset.first_currency <-
  subset(rules_small_len,
         lhs %pin% "user_set=" &
           lhs %pin% "FIRST.CURRENCY.Target=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.first_currency, by="confidence"), 200))
# Nothing interesting

subset.first_currency_target <-
  subset(rules_small_len,
         lhs %pin% "user_set=" &
           lhs %pin% "FIRST.CURRENCY.Target=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.first_currency_target, by="confidence"), 200))
# Cant really make out anything interesting from it


# Lets include stopped and try to find some good association rules with regards to initial value transfer
rules_value_transfer <-
  apriori( data, parameter = list(minlen=3, maxlen=3, supp=0.001, conf=0.001),
           appearance = list(
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=1. Stopped",
               "transfer_value_classification=3. Started"
             ), default="lhs") )

subset.rules_value_transfer10000 <-
  subset(rules_value_transfer,
         lhs %pin% "user_set=" &
         lhs %pin% "value_transfers_prior=10000" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_value_transfer10000, by="confidence"), 200))

subset.rules_value_transfer3000 <-
  subset(rules_value_transfer,
         lhs %pin% "user_set=" &
           lhs %pin% "value_transfers_prior=3000" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_value_transfer3000, by="confidence"), 200))

subset.rules_value_transfer500 <-
  subset(rules_value_transfer,
         lhs %pin% "user_set=" &
           lhs %pin% "value_transfers_prior=500" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_value_transfer500, by="confidence"), 200))

subset.rules_value_transfer0 <-
  subset(rules_value_transfer,
         lhs %pin% "user_set=" &
           lhs %pin% "value_transfers_prior=0" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_value_transfer0, by="confidence"), 200))

# Lets find what makes transfer value similar better (assume age)
rules_transfer_value_similar_age <-
  apriori( data, parameter = list(minlen=3, maxlen=3, supp=0.001, conf=0.02),
           appearance = list(
             rhs=c(
               "transfer_value_classification=2b. Similar (+/- 50%) "
             ), default="lhs") )

subset.rules_transfer_value_similar_age <-
  subset(rules_transfer_value_similar_age,
         lhs %pin% "user_set=" &
          lhs %pin% "USER.PROFILE.Age.Tiers=4. 36-45" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_transfer_value_similar_age, by="confidence"), 200))

length(which(data$USER.PROFILE.Age.Tiers == '4. 36-45' & data$user_set == 'free transfer'))
length(which(data$USER.PROFILE.Age.Tiers == '4. 36-45' & data$user_set == 'chocolate'))

# Lets find all the rules
rules_transfer_value_36_45 <-
  apriori( data, parameter = list(minlen=3, maxlen=3, supp=0.0001, conf=0.002),
           appearance = list(
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=1. Stopped",
               "transfer_value_classification=3. Started"
             ), default="lhs") )

subset.rules_transfer_value_36_45 <-
  subset(rules_transfer_value_36_45,
         lhs %pin% "user_set=" &
           lhs %pin% "USER.PROFILE.Age.Tiers=4. 36-45" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_transfer_value_36_45, by="confidence"), 200))

# Lets add more rules
rules_transfer_value_36_45_sex <-
  apriori( data, parameter = list(minlen=4, maxlen=4, supp=0.0001, conf=0.002),
           appearance = list(
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=1. Stopped",
               "transfer_value_classification=3. Started"
             ), default="lhs") )

subset.rules_transfer_value_36_45_sex <-
  subset(rules_transfer_value_36_45_sex,
         lhs %pin% "user_set=" &
           lhs %pin% "USER.PROFILE.Age.Tiers=4. 36-45" &
           lhs %pin% "Sex=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_transfer_value_36_45_sex, by="confidence"), 200))


rules_transfer_value_sex <-
  apriori( data, parameter = list(minlen=2, maxlen=3, supp=0.01, conf=0.01),
           appearance = list(
             rhs=c(
               "transfer_value_classification=2a. 50+% reduction",
               "transfer_value_classification=2c. 50+% increase",
               "transfer_value_classification=2b. Similar (+/- 50%) ",
               "transfer_value_classification=1. Stopped",
               "transfer_value_classification=3. Started"
             ), default="lhs") )

subset.rules_transfer_value_sex <-
  subset(rules_transfer_value_sex,
         lhs %pin% "user_set=" &
           lhs %pin% "Sex=" &
           !lhs %pin% "transfer_count_classification=" &
           !lhs %pin%"transfer_value_classification=" &
           !lhs %pin%"count_invites_classification=")

inspect(head(sort(subset.rules_transfer_value_sex, by="confidence"), 200))

?length
nrow(data[which(data$user_set=='chocolate' & data$Sex=='F' & data$transfer_value_classification=='1. Stopped'),])
nrow(data[which(data$user_set=='chocolate' & data$Sex=='M' & data$transfer_value_classification=='1. Stopped'),])

m <- matrix(c(0.509, 0.465, 0.38, 0.417), nrow=2, ncol=2)
males_females_compensation = data.frame(m)
names(males_females_compensation) <- c("Females", "Males")
colnames(males_females_compensation) <- c("Chocolate", "Free transfer")
counts <- table(males_females_compensation$Females, males_females_compensation$Males)
barplot(counts, beside=T)

nrow(data[which(data$user_set=='chocolate' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100

stuff <- c(
  nrow(data[which(data$user_set=='chocolate' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$transfer_value_classification=='2a. 50+% reduction'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$transfer_value_classification=='3. Started'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$transfer_value_classification=='2b. Similar (+/- 50%) '),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$transfer_value_classification=='2c. 50+% increase'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$transfer_value_classification=='2a. 50+% reduction'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$transfer_value_classification=='3. Started'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$transfer_value_classification=='2b. Similar (+/- 50%) '),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$transfer_value_classification=='2c. 50+% increase'),]) / nrow(data) * 100
)

stuff <- c(
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='2. 18-25' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='2. 18-25' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='3. 26-35' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='3. 26-35' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='4. 36-45' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='4. 36-45' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='5. 46-60' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='5. 46-60' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='6. 61+' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='6. 61+' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data) * 100
)

stuff2 <- c(
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='2. 18-25' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='2. 18-25'),]) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='2. 18-25' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='2. 18-25'),]) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='3. 26-35' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='3. 26-35'),]) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='3. 26-35' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='3. 26-35'),]) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='4. 36-45' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='4. 36-45'),]) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='4. 36-45' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='4. 36-45'),]) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='5. 46-60' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='5. 46-60'),]) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='5. 46-60' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='5. 46-60'),]) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$USER.PROFILE.Age.Tiers=='6. 61+' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='6. 61+' ),]) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$USER.PROFILE.Age.Tiers=='6. 61+' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$USER.PROFILE.Age.Tiers=='6. 61+' ),]) * 100
)


c(
  nrow(data[which(data$user_set=='chocolate' & data$value_transfers_prior=='500' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$user_set=='chocolate' & data$value_transfers_prior=='500'),]) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$value_transfers_prior=='500' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$user_set=='free transfer' & data$value_transfers_prior=='500'),]) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$value_transfers_prior=='3000' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$user_set=='chocolate' & data$value_transfers_prior=='3000'),]) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$value_transfers_prior=='3000' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$user_set=='free transfer' & data$value_transfers_prior=='3000'),]) * 100,
  nrow(data[which(data$user_set=='chocolate' & data$value_transfers_prior=='10000' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$user_set=='chocolate' & data$value_transfers_prior=='10000'),]) * 100,
  nrow(data[which(data$user_set=='free transfer' & data$value_transfers_prior=='10000' & data$transfer_value_classification=='1. Stopped'),]) / nrow(data[which(data$user_set=='free transfer' & data$value_transfers_prior=='10000'),]) * 100
)

?aggregate
?barplot
####### INTERESTING RULES
# In conclusion we dont have enough data to show the following interesting classifications with decent confidence and support:
# increase, reduction, similar or started

# AGE TIERS
# If we lower our confidence and support to absyssal levels, we get those 2 big groups, which can be summed up by those 2:
lhs                                  rhs                                                       support confidence     lift
1  {user_set=chocolate,
    value_transfers_prior=10000,
    USER.PROFILE.Age.Tiers=5. 46-60} => {transfer_count_classification=2b. Similar (+/- 50%) } 0.01183432  0.5454545 4.851675
5  {user_set=free transfer,
    value_transfers_prior=3000,
    USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.01380671  0.4117647 5.964706
# We can see that we simply dont have enough data. The data seems to have similar free transfers with ages 36-45 and chocolate
# with ages 46-60. Lets remove the noise and only look at the age tiers:

lhs                                  rhs                                                       support confidence      lift
1 {user_set=chocolate,
   USER.PROFILE.Age.Tiers=5. 46-60} => {transfer_count_classification=2b. Similar (+/- 50%) } 0.02169625 0.22000000 1.9568421
2 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.01775148 0.15517241 2.2477833
3 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_count_classification=2b. Similar (+/- 50%) } 0.01775148 0.15517241 1.3802178
5 {user_set=chocolate,
   USER.PROFILE.Age.Tiers=5. 46-60} => {transfer_value_classification=2c. 50+% increase}      0.01183432 0.12000000 2.4336000
# The results confirm our previous results - the data happens to have a lot of chocolate transfers with age 46-60 and
# free transfers with age 36-45. Looking at the tier 26-35:

4 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=3. 26-35} => {transfer_count_classification=2b. Similar (+/- 50%) } 0.01972387 0.12658228 1.1259161
6 {user_set=chocolate,
   USER.PROFILE.Age.Tiers=3. 26-35} => {transfer_count_classification=2b. Similar (+/- 50%) } 0.01183432 0.10344828 0.9201452
7 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=3. 26-35} => {count_invites_classification=3. Started}              0.01380671 0.08860759 2.1392405
8 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=3. 26-35} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.01183432 0.07594937 1.1001808
# We can see that free transfer seems to have better effect at keeping transfer count similar

1 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.017751479 0.15517241 2.2477833
2 {user_set=chocolate,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.003944773 0.03703704 0.5365079
# This is where the difference comes in for the chocolate performance in transfer value classification when the classification is 2b - similar. It seems that
# in ages 36-45 the value classification 2b is strongly in favor of free transfer
# There are 54 cases with age 36-45 where chocolate was sent and 58 cases with free transfer. Lets dig deeper.

1 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=1. Stopped}             0.059171598 0.51724138 1.2428501
2 {user_set=chocolate,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=1. Stopped}             0.047337278 0.44444444 1.0679305
3 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.017751479 0.15517241 2.2477833
4 {user_set=chocolate,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=3. Started}             0.009861933 0.09259259 1.3412698
5 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=3. Started}             0.005917160 0.05172414 0.7492611
6 {user_set=chocolate,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2c. 50+% increase}      0.003944773 0.03703704 0.7511111
7 {user_set=chocolate,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.003944773 0.03703704 0.5365079
8 {user_set=free transfer,
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2c. 50+% increase}      0.001972387 0.01724138 0.3496552

# JOHHAIDIII chocolate wins
# Lets add sex
lhs                                  rhs                                                        support confidence      lift
1  {user_set=free transfer,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=F}                           => {transfer_value_classification=1. Stopped}             0.023668639 0.66666667 1.6018957
4  {user_set=chocolate,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=F}                           => {transfer_value_classification=1. Stopped}             0.015779093 0.42105263 1.0117236
3  {user_set=free transfer,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=M}                           => {transfer_value_classification=1. Stopped}             0.035502959 0.45000000 1.0812796
2  {user_set=chocolate,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=M}                           => {transfer_value_classification=1. Stopped}             0.031558185 0.45714286 1.0984428
6  {user_set=chocolate,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=M}                           => {transfer_value_classification=3. Started}             0.009861933 0.14285714 2.0693878
8  {user_set=free transfer,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=M}                           => {transfer_value_classification=3. Started}             0.005917160 0.07500000 1.0864286
5  {user_set=free transfer,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=M}                           => {transfer_value_classification=2b. Similar (+/- 50%) } 0.015779093 0.20000000 2.8971429
7  {user_set=chocolate,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=F}                           => {transfer_value_classification=2b. Similar (+/- 50%) } 0.003944773 0.10526316 1.5248120
10 {user_set=free transfer,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=F}                           => {transfer_value_classification=2b. Similar (+/- 50%) } 0.001972387 0.05555556 0.8047619
9  {user_set=chocolate,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=M}                           => {transfer_value_classification=2c. 50+% increase}      0.003944773 0.05714286 1.1588571
11 {user_set=free transfer,
    USER.PROFILE.Age.Tiers=4. 36-45,
    Sex=M}                           => {transfer_value_classification=2c. 50+% increase}      0.001972387 0.02500000 0.5070000

# SENT WEEK OF THE MONTH
# Lets take a look at the sent week of the month
lhs                         rhs                                                       support confidence      lift
3  {user_set=free transfer,                                                                                          
    sent_week_of_month=1}   => {transfer_value_classification=1. Stopped}             0.06508876 0.49253731 1.1834901
5  {user_set=chocolate,                                                                                              
    sent_week_of_month=1}   => {transfer_value_classification=1. Stopped}             0.04339250 0.38596491 0.9274133
9  {user_set=chocolate,                                                                                              
    sent_week_of_month=1}   => {transfer_value_classification=3. Started}             0.01775148 0.15789474 2.2872180
11 {user_set=free transfer,                                                                                          
    sent_week_of_month=1}   => {transfer_value_classification=2b. Similar (+/- 50%) } 0.01380671 0.10447761 1.5134328

1  {user_set=free transfer,                                                                                          
    sent_week_of_month=2}   => {transfer_value_classification=1. Stopped}             0.06508876 0.53225806 1.2789329
7  {user_set=chocolate,                                                                                              
    sent_week_of_month=2}   => {transfer_value_classification=1. Stopped}             0.02958580 0.35714286 0.8581584

2  {user_set=chocolate,                                                                                              
    sent_week_of_month=3}   => {transfer_value_classification=1. Stopped}             0.04930966 0.51020408 1.2259406
8  {user_set=free transfer,                                                                                          
    sent_week_of_month=3}   => {transfer_value_classification=1. Stopped}             0.03944773 0.28169014 0.6768574
10 {user_set=free transfer,                                                                                          
    sent_week_of_month=3}   => {transfer_value_classification=3. Started}             0.01577909 0.11267606 1.6321932
12 {user_set=free transfer,                                                                                          
    sent_week_of_month=3}   => {transfer_value_classification=2b. Similar (+/- 50%) } 0.01380671 0.09859155 1.4281690

13 {user_set=chocolate,                                                                                              
    sent_week_of_month=4}   => {transfer_value_classification=3. Started}             0.01183432 0.07692308 1.1142857
4  {user_set=free transfer,                                                                                          
    sent_week_of_month=4}   => {transfer_value_classification=1. Stopped}             0.06508876 0.40740741 0.9789363
6  {user_set=chocolate,                                                                                              
    sent_week_of_month=4}   => {transfer_value_classification=1. Stopped}             0.05917160 0.38461538 0.9241706


# VALUE TRANSFER PRIOR
# Taking a look at value transfer prior
lhs                              rhs                                                       support confidence     lift
2  {user_set=chocolate,
    value_transfers_prior=10000} => {transfer_count_classification=2b. Similar (+/- 50%) } 0.01577909 0.22857143 2.033083

1  {user_set=free transfer,
    value_transfers_prior=3000}  => {transfer_count_classification=2b. Similar (+/- 50%) } 0.03747535 0.25333333 2.253333
4  {user_set=chocolate,
    value_transfers_prior=3000}  => {transfer_count_classification=2b. Similar (+/- 50%) } 0.02366864 0.20000000 1.778947

8  {user_set=free transfer,
    value_transfers_prior=500}   => {transfer_count_classification=2b. Similar (+/- 50%) } 0.01972387 0.16949153 1.507583
3  {user_set=chocolate,
    value_transfers_prior=500}   => {transfer_count_classification=2b. Similar (+/- 50%) } 0.01183432 0.20000000 1.778947

7  {user_set=free transfer,
    value_transfers_prior=3000}  => {transfer_value_classification=2b. Similar (+/- 50%) } 0.02564103 0.17333333 2.510857
9  {user_set=chocolate,
    value_transfers_prior=3000}  => {transfer_value_classification=2b. Similar (+/- 50%) } 0.01577909 0.13333333 1.931429

5  {user_set=chocolate,
    value_transfers_prior=0}     => {transfer_value_classification=3. Started}             0.03944773 0.19801980 2.868458
10 {user_set=free transfer,
    value_transfers_prior=0}     => {transfer_value_classification=3. Started}             0.02958580 0.12195122 1.766551

# The support is again super low, confidence is 10-20%, but it seems that free transfer is better always, except when your
# prior transfer value was 0

# 10000
1 {user_set=free transfer,
   value_transfers_prior=10000} => {transfer_value_classification=1. Stopped}             0.041420118 0.87500000 2.102488
2 {user_set=chocolate,
   value_transfers_prior=10000} => {transfer_value_classification=1. Stopped}             0.047337278 0.68571429 1.647664
3 {user_set=chocolate,
   value_transfers_prior=10000} => {transfer_value_classification=2c. 50+% increase}      0.009861933 0.14285714 2.897143
4 {user_set=chocolate,
   value_transfers_prior=10000} => {transfer_value_classification=2a. 50+% reduction}     0.005917160 0.08571429 3.621429
7 {user_set=free transfer,
   value_transfers_prior=10000} => {transfer_value_classification=2a. 50+% reduction}     0.001972387 0.04166667 1.760417
5 {user_set=chocolate,
   value_transfers_prior=10000} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.005917160 0.08571429 1.241633
6 {user_set=free transfer,
   value_transfers_prior=10000} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.003944773 0.08333333 1.207143

# 3000
1 {user_set=chocolate,
   value_transfers_prior=3000} => {transfer_value_classification=1. Stopped}             0.090729783 0.76666667 1.842180
2 {user_set=free transfer,
   value_transfers_prior=3000} => {transfer_value_classification=1. Stopped}             0.104536489 0.70666667 1.698009
3 {user_set=free transfer,
   value_transfers_prior=3000} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.025641026 0.17333333 2.510857
4 {user_set=chocolate,
   value_transfers_prior=3000} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.015779093 0.13333333 1.931429
5 {user_set=free transfer,
   value_transfers_prior=3000} => {transfer_value_classification=2a. 50+% reduction}     0.009861933 0.06666667 2.816667
6 {user_set=chocolate,
   value_transfers_prior=3000} => {transfer_value_classification=2c. 50+% increase}      0.007889546 0.06666667 1.352000
7 {user_set=free transfer,
   value_transfers_prior=3000} => {transfer_value_classification=2c. 50+% increase}      0.007889546 0.05333333 1.081600
8 {user_set=chocolate,
   value_transfers_prior=3000} => {transfer_value_classification=2a. 50+% reduction}     0.003944773 0.03333333 1.408333

# 500
lhs                            rhs                                                        support confidence      lift
1 {user_set=free transfer,
   value_transfers_prior=500} => {transfer_value_classification=1. Stopped}             0.088757396 0.76271186 1.8326773
2 {user_set=chocolate,
   value_transfers_prior=500} => {transfer_value_classification=1. Stopped}             0.043392505 0.73333333 1.7620853
3 {user_set=chocolate,
   value_transfers_prior=500} => {transfer_value_classification=2c. 50+% increase}      0.009861933 0.16666667 3.3800000
4 {user_set=free transfer,
   value_transfers_prior=500} => {transfer_value_classification=2c. 50+% increase}      0.013806706 0.11864407 2.4061017
5 {user_set=free transfer,
   value_transfers_prior=500} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.013806706 0.11864407 1.7186441
6 {user_set=chocolate,
   value_transfers_prior=500} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.003944773 0.06666667 0.9657143
7 {user_set=chocolate,
   value_transfers_prior=500} => {transfer_value_classification=2a. 50+% reduction}     0.001972387 0.03333333 1.4083333

# 0
lhs                          rhs                                           support confidence     lift
1 {user_set=chocolate,
   value_transfers_prior=0} => {transfer_value_classification=3. Started} 0.03944773  0.1980198 2.868458
2 {user_set=free transfer,
   value_transfers_prior=0} => {transfer_value_classification=3. Started} 0.02958580  0.1219512 1.766551


# With all transfer amounts prior to complaint, chocolate has better odds

1 {user_set=free transfer,                                                                                                    
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.017751479 0.15517241 2.2477833
2 {user_set=chocolate,                                                                                                        
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.003944773 0.03703704 0.5365079
# This is where the difference comes in. 
# There are 54 cases with age 36-45 where chocolate was sent and 58 cases with free transfer. Lets dig deeper.

1 {user_set=free transfer,                                                                                                    
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=1. Stopped}             0.059171598 0.51724138 1.2428501
2 {user_set=chocolate,                                                                                                        
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=1. Stopped}             0.047337278 0.44444444 1.0679305
3 {user_set=free transfer,                                                                                                    
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.017751479 0.15517241 2.2477833
4 {user_set=chocolate,                                                                                                        
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=3. Started}             0.009861933 0.09259259 1.3412698
5 {user_set=free transfer,                                                                                                    
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=3. Started}             0.005917160 0.05172414 0.7492611
6 {user_set=chocolate,                                                                                                        
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2c. 50+% increase}      0.003944773 0.03703704 0.7511111
7 {user_set=chocolate,                                                                                                        
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2b. Similar (+/- 50%) } 0.003944773 0.03703704 0.5365079
8 {user_set=free transfer,                                                                                                    
   USER.PROFILE.Age.Tiers=4. 36-45} => {transfer_value_classification=2c. 50+% increase}      0.001972387 0.01724138 0.3496552

# JOHHAIDIII chocolate wins
# Lets add sex
lhs                                  rhs                                                        support confidence      lift
1  {user_set=free transfer,                                                                                                    
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=F}                           => {transfer_value_classification=1. Stopped}             0.023668639 0.66666667 1.6018957
4  {user_set=chocolate,                                                                                                        
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=F}                           => {transfer_value_classification=1. Stopped}             0.015779093 0.42105263 1.0117236
3  {user_set=free transfer,                                                                                                    
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=M}                           => {transfer_value_classification=1. Stopped}             0.035502959 0.45000000 1.0812796 
2  {user_set=chocolate,                                                                                                        
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=M}                           => {transfer_value_classification=1. Stopped}             0.031558185 0.45714286 1.0984428
6  {user_set=chocolate,                                                                                                        
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=M}                           => {transfer_value_classification=3. Started}             0.009861933 0.14285714 2.0693878
8  {user_set=free transfer,                                                                                                    
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=M}                           => {transfer_value_classification=3. Started}             0.005917160 0.07500000 1.0864286
5  {user_set=free transfer,                                                                                                    
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=M}                           => {transfer_value_classification=2b. Similar (+/- 50%) } 0.015779093 0.20000000 2.8971429
7  {user_set=chocolate,                                                                                                        
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=F}                           => {transfer_value_classification=2b. Similar (+/- 50%) } 0.003944773 0.10526316 1.5248120
10 {user_set=free transfer,                                                                                                    
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=F}                           => {transfer_value_classification=2b. Similar (+/- 50%) } 0.001972387 0.05555556 0.8047619
9  {user_set=chocolate,                                                                                                        
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=M}                           => {transfer_value_classification=2c. 50+% increase}      0.003944773 0.05714286 1.1588571
11 {user_set=free transfer,                                                                                                    
    USER.PROFILE.Age.Tiers=4. 36-45,                                                                                           
    Sex=M}                           => {transfer_value_classification=2c. 50+% increase}      0.001972387 0.02500000 0.5070000

