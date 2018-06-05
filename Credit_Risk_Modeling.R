# Load Libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(ggthemes)
library(curl)
library(RCurl)
library(DT)
library(stringr)
library(rpart)
library(rpart.plot)

# Get Data
loan_df <- read.csv("https://raw.githubusercontent.com/veeranalytics/Credit-Risk-Model/master/loan_data.csv")
# back_df <- loan_df

# Credit Scorecard Scoring Table
score_df <- read.csv("https://raw.githubusercontent.com/veeranalytics/Credit-Risk-Model/master/Credit_Scoring.csv")
print(score_df)

# Get a list of existing customers
df <- read.csv("https://raw.githubusercontent.com/veeranalytics/Credit-Risk-Model/master/Customers.csv")

# Logistic Regression 
log_model <- readRDS(url("https://github.com/veeranalytics/Credit-Risk-Model/blob/master/LogModel.rds?raw=true"))

# Decision Tree
tree_model_mod <- readRDS(url("https://github.com/veeranalytics/Credit-Risk-Model/blob/master/LogModel.rds"))

# Take a glimpse of data
str(loan_df)

# Convert Numerical Factor Variables to numbers
loan_df$annual_inc <- as.numeric(gsub('[$,]', '', loan_df$annual_inc))
loan_df$loan_amnt <- as.numeric(gsub('[$,]', '', loan_df$loan_amnt))
loan_df$int_rate <- as.numeric(gsub('[%]', '', loan_df$int_rate))

# Take a glimpse of data
str(loan_df)

# Look at missing values
sapply(loan_df, function(x) sum(is.na(x)))

# Missing Value Imputation using median
loan_df$emp_length[which(is.na(loan_df$emp_length))] <- median(loan_df$emp_length, na.rm = TRUE)

# create dataset-- of current loan customers for credit risk modeling
loan_current <- loan_df %>%
  filter(loan_status %in% c("Current"))

# create dataset-- of current loan customers at warning for credit risk modeling
loan_warning <- loan_df %>%
  filter(loan_status %in% c("In Grace Period","Late (16-30 days)","Late (31-120 days)"))

# Create a copy of loan dataset for modeling and remove loan_status are
# "Current" and Warnings ("In Grace Period","Late (16-30 days)","Late (31-120 days)") 
# And ID variables
loan_data <- loan_df %>%
  filter(!(loan_status %in% c("Current","In Grace Period","Late (16-30 days)","Late (31-120 days)")))

# Create 02 categories for loan status
loan_data$loan_status2 <- rep(NA, length(loan_data$loan_status))
loan_data$loan_status2[which(loan_data$loan_status == "Charged Off")] <- "Yes"
loan_data$loan_status2[which(loan_data$loan_status == "Default")] <- "Yes"
loan_data$loan_status2[which(loan_data$loan_status == "Fully Paid")] <- "No"
loan_data$loan_status <- NULL
colnames(loan_data)[14] <- c("loan_status")
loan_data$loan_status <- as.factor(loan_data$loan_status)

# Removing loan_id and member id columns-- as will not be useful for the modeling process
model_data <- loan_data %>%
  select(-loan_id, -member_id)

# Look at the loan_status 
n_default <- summary(model_data$loan_status)[2]
model_data$index <- as.numeric(row.names(model_data))

# The data is unbalanced-- Performing Undersampling
# Create sample dataset
not_default <- model_data %>%
  filter(loan_status == "No") %>%
  sample_n(n_default)

is_default <- model_data %>%
  filter(loan_status == "Yes")

full_sample <- rbind(not_default, is_default) %>%
  arrange(index)

full_sample$index <- NULL
str(full_sample)

#Remove result column and categorical columns
default_numeric <- full_sample %>%
  select(-home_ownership, -term, -marital, -job, -loan_status)

# Find any highly correlated predictors and remove
# Highly correlated predictors create instability in the model so one of the two is removed.
high_cor_cols <- findCorrelation(cor(default_numeric), cutoff = .1, verbose = TRUE, 
                                 names = TRUE, exact = TRUE)
high_cor_cols # no predictors are highly co-related, so will not drop any variable


# Calculate WoE and Information Value
library(Information)
library(gridExtra)

full_sample["loan_status_num"] <- ifelse(as.character(full_sample$loan_status) == "Yes", 1, 0)

IV <- create_infotables(data=full_sample[,-12],
                        y="loan_status_num", bins = 5)
print(head(IV$Summary), row.names=FALSE)

print(IV$Tables$term, row.names=FALSE)
print(IV$Tables$int_rate, row.names=FALSE)
print(IV$Tables$cibil_score, row.names=FALSE)
print(IV$Tables$inq_last_6mths, row.names=FALSE)
print(IV$Tables$annual_inc, row.names=FALSE)
print(IV$Tables$loan_amnt, row.names=FALSE)
print(IV$Tables$age, row.names=FALSE)

full_sample$loan_status_num <- NULL

# Pre-processing the full dataset for modelling
# preproc_model <- preProcess(full_sample[, -12], 
                            #method = c("center", "scale", "nzv"))

# fraud_preproc <- predict(preproc_model, newdata = full_sample[, -12])

# Bind the results to the pre-processed data
# fraud_pp_w_result <- cbind(loan_status = full_sample$loan_status, fraud_preproc)

# Split sample into train and test sets for Random Forest
in_train <- createDataPartition(y = full_sample$loan_status, p = .75, 
                                list = FALSE) 
train <- full_sample[in_train, ] 
test <- full_sample[-in_train, ]

# write.csv(test,"C:/Users/veer/Desktop/Projects/Credit_Risk_Modeling/Final/test.csv")

# Logistic Regression
# Build the logistic regression model
log_model <- glm(loan_status ~ term + int_rate + cibil_score + inq_last_6mths + annual_inc +
                   loan_amnt + loan_amnt, family = "binomial", data = train)
summary(log_model)

# saveRDS(log_model, "C:/Users/veer/Desktop/Projects/Credit_Risk_Modeling/Final/LogModel.rds")


# Applying Decision tree as getting very bad prediction accuracy for logistric regression

# Using Decision Tree model
set.seed(123)
tree_model <- rpart(loan_status ~ ., method = "class",
                          data =  train, control = rpart.control(cp = 0.001))
# Create an index for of the row with the minimum xerror
plotcp(tree_model)
index <- which.min(tree_model$cptable[ , "xerror"])
cp <- tree_model$cptable[index, "CP"]
#cp


# Using Decision Tree model
tree_model_mod <- rpart(loan_status ~ ., method = "class",
                    data =  train, control = rpart.control(minsplit = 180, minbucket = 60, cp = cp))

# saveRDS(tree_model_mod, "C:/Users/veer/Desktop/Projects/Credit_Risk_Modeling/Final/TreeModel.rds")

# pred <- predict(tree_model_mod, newdata = test,  type = "class")

# Plot the tree
prp(tree_model_mod, extra=6, uniform=T, branch=1, yesno=T, box.palette=c("green", "pink"))

# Make predictions for the probability of default using the pruned tree and the test set.
prob_default_DT <- predict(tree_model_mod, newdata = loan_data)[ ,2]
prob_defaut_log <- predict(log_model, newdata = loan_data, type = "response")


# Obtain the cutoff for acceptance rate 80%
#acceptance_rate = 0.80
#cutoff_prior = quantile(prob_default, acceptance_rate)  

# Obtain the binary predictions.
# cut_off_pred <- ifelse(prob_default > cutoff_prior, "Yes", "No")

#cm <- table(test$loan_status, cut_off_pred)
#cm
#(cm[1,1]+cm[2,2])/sum(cm)

# Obtain the actual default status for the accepted loans
# accepted_status <- test$loan_status[cut_off_pred == "No"]
# accepted <- table(accepted_status)

# Acceptance Rate; Bad Rate and Cut-off Rate
# Obtain the bad rate for the accepted loans
# bad_rate <- as.numeric(accepted["Yes"])/sum(as.numeric(accepted["Yes"]), as.numeric(accepted["No"]))
# bad_rate
# cutoff_prior
# acceptance_rate
loan_data$loan_status <- ifelse(loan_data$loan_status=="Yes",1,0)

# Strategy Function to show Acceptance Rate, Cutoff and Bad Rate
strategy_func <- function(prob_of_def){
  cutoff = rep(NA, 20)
  bad_rate = rep(NA, 20)
  accept_rate = seq(1,0.05,by=-0.05)
  for (i in 1:20){
    cutoff[i]=quantile(prob_of_def,accept_rate[i])
    pred_i=ifelse(prob_of_def> cutoff[i], 1, 0)
    pred_as_good=loan_data$loan_status[pred_i==0]
    # bad_rate = as.numeric(pred_as_good["Yes"])/(as.numeric(pred_as_good["Yes"]) + as.numeric(pred_as_good["No"]))}
    bad_rate[i]=sum(pred_as_good)/length(pred_as_good)}
  table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4))
  return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff))
}

# Logistic Regression: Obtain the strategy tables for both prediction-vectors
strategy_tree = strategy_func(prob_defaut_log)
strategy_df <- as.data.frame(strategy_tree$table)

# Draw plots
ggplot(strategy_df, aes(x=accept_rate, y= cutoff)) + 
  geom_point(stat='identity', col="red", size=6)  + 
  labs(title="Strategy Charts", 
       subtitle="Acceptance Rate vs. Cut-off")

ggplot(strategy_df, aes(x=accept_rate, y= bad_rate)) + 
  geom_point(stat='identity', col="blue", size=6)  + 
  labs(title="Strategy Charts", 
       subtitle="Acceptance Rate vs. bad_rate")

ggplot(strategy_df, aes(x=cutoff, bad_rate)) + 
  geom_point(stat='identity', col="dark green", size=6)  + 
  labs(title="Strategy Charts", 
       subtitle="Cut-off vs. Bad Rate")

df <- test[2:2,]


creditScore <- function(df) {
  score = 0
  
  # Scoring for term
  if (df$term == " 36 months") {
    score = score + 200
  }
  else {
    score = score + 100
  }
  
  # Scoring for Interest Rate
  if (df$int_rate >= 0 && df$int_rate < 8.6) {
    score = score + 180
  }
  else if (df$int_rate >= 8.6 && df$int_rate < 11.15) {
    score = score + 160
  }
  else if (df$int_rate >= 11.15 && df$int_rate < 13.36) {
    score = score + 140
  }
  else if (df$int_rate >= 13.36 && df$int_rate < 15.21) {
    score = score + 120
  }
  else {
    score = score + 100
  }
  
  # Scoring for Cibil Score
  if (df$cibil_score < 675) {
    score = score + 100
  }
  else if (df$cibil_score >= 675 && df$cibil_score < 695) {
    score = score + 110
  }
  else if (df$cibil_score >= 695 && df$cibil_score < 715) {
    score = score + 120
  }
  else if (df$cibil_score >= 715 && df$cibil_score < 740) {
    score = score + 140
  }
  else {
    score = score + 160
  }
  
  # Scoring for inq_last_6mths
  if (df$inq_last_6mths == 0) {
    score = score + 130
  }
  else if (df$inq_last_6mths >= 1 && df$inq_last_6mths < 2) {
    score = score + 110
  }
  else {
    score = score + 100
  }
  
  # Scoring for annual_inc
  if (df$annual_inc >= 0 && df$annual_inc < 34820) {
    score = score + 100
  }
  else if (df$annual_inc >= 34820 && df$annual_inc < 47545) {
    score = score + 105
  }
  else if (df$annual_inc >= 47545 && df$annual_inc < 62401) {
    score = score + 110
  }
  else if (df$annual_inc >= 62401 && df$annual_inc < 84997) {
    score = score + 115
  }
  else {
    score = score + 120
  }
  
  # Scoring for loan_amnt
  if (df$loan_amnt >= 0 && df$loan_amnt < 4701) {
    score = score + 110
  }
  else if (df$loan_amnt >= 4701 && df$loan_amnt < 7101) {
    score = score + 106
  }
  else if (df$loan_amnt >= 7101 && df$loan_amnt < 15601) {
    score = score + 104
  }
  else if (df$loan_amnt >= 15601 && df$loan_amnt < 16001) {
    score = score + 102
  }
  else {
    score = score + 100
  }
  
  return(score)
}
  
creditScore(df)
df$cibil_score

prob_defaut_log_df <- predict(log_model, newdata = df, type = "response")

library(plotly)
library(ggplot2)

gg.gauge <- function(pos,breaks=c(0,30,70,100)) {
  
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}
gg.gauge(as.integer(prob_defaut_log_df*100),breaks=c(0,30,70,100))


  
  
  
  













