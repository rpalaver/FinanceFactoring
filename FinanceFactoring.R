# setup: required packages
library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(dplyr)
library(gridExtra)
library(lubridate)
library(gplots)
library(imputeTS)

#################################
##  data loading and cleaning  ##
#################################
# Factoring data from IBM:
# https://raw.githubusercontent.com/rpalaver/FinanceFactoring/master/WA_Fn-UseC_-Accounts-Receivable.csv

# load csv-file
df <- read.csv("https://raw.githubusercontent.com/rpalaver/FinanceFactoring/master/WA_Fn-UseC_-Accounts-Receivable.csv", stringsAsFactors = FALSE)

# convert invoice date from character to format date
df$PaperlessDate <- as.Date(df$PaperlessDate, "%m/%d/%Y")
df$InvoiceDate <- as.Date(df$InvoiceDate, "%m/%d/%Y")
df$DueDate <- as.Date(df$DueDate, "%m/%d/%Y")
df$SettledDate <- as.Date(df$SettledDate, "%m/%d/%Y")

# convert some variables to factors
df$countryCode <- as.factor(df$countryCode)
df$customerID <- as.factor(df$customerID)
df$Disputed <- as.factor(df$Disputed)
df$PaperlessBill <- as.factor(df$PaperlessBill)

# show first part of origin daabase
df %>% select(invoiceNumber, customerID, countryCode, InvoiceAmount, InvoiceDate, DueDate, PaperlessBill) %>% slice(1:10) %>% knitr::kable()

# show second part of origin daabase
df %>% select(invoiceNumber, Disputed, SettledDate, DaysToSettle, DaysLate, PaperlessDate) %>% slice(1:10) %>% knitr::kable()

# add numeric ID for customer
# custID <- df %>% group_by(customerID) %>% summarize() %>% mutate(custID = seq(1:100))
# df <- left_join(df, custID, by = "customerID")
# rm(custID)

# Validation set will be 10% of df
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = df$DaysLate, times = 1, p = 0.2, list = FALSE)
train_set <- df[-test_index,]
temp <- df[test_index,]

# Make sure customerID in validation set are also in df1 set
test_set <- temp %>% 
  semi_join(train_set, by = "customerID")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(df, test_index, temp, removed)

########################
##  data analysis  ##
########################

# add variable for late payment yes/no
train_set <- train_set %>% mutate(Late = ifelse(DaysLate > 0, 1,0))
test_set  <- test_set  %>% mutate(Late = ifelse(DaysLate > 0, 1,0))

# proportion of overall late settlements
cat(mean(train_set$Late))

# show number of different DayLate
train_set %>% group_by(DaysLate) %>% summarize(number=n()) %>% filter(DaysLate > 0) %>%
ggplot(aes(DaysLate, number), xlab = "days too late") + geom_point() +
  xlab("days too late") +
  ylab("Number of invoices")

# plot distribution of invoice ammounts
qplot(train_set$InvoiceAmount, bins = 30, color = I("black"), xlab = "Invoice amount", ylab = "Number of invoices")

# plot distribution of days to settle
qplot(train_set$DaysToSettle, bins = 30, color = I("black"), 
      xlab = "Days to settle", ylab = "Number of invoices") +
  geom_vline(xintercept = 30, show.legend = "due date", linetype = "longdash", color = "Red", size = 1.5) +
  annotate("text", x = 30, y = 50, label = "payment target = 30", angle = 90, color = "White", vjust = 1.5)

# show percentage in time and late splited regarding PaperlessBill
train_set %>% mutate(payment = ifelse(Late == 1,"to late", "in time")) %>%
  ggplot(aes(payment, group = PaperlessBill)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_fill_manual("Late", values = c("seagreen3", "tomato2")) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  facet_grid(~PaperlessBill) +
  labs(y = "Percentage", title = "Paperless yes or no") +
  theme(legend.position = "none")


# show percentage in time and late splitted regarding Disputed
train_set %>% mutate(payment = ifelse(Late == 1,"to late", "in time")) %>%
  ggplot(aes(payment, group = Disputed)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_fill_manual("Late", values = c("seagreen3", "tomato2")) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  facet_grid(~Disputed) +
  labs(y = "Percentage", title = "Disputed yes or no") +
  theme(legend.position = "none")
  
# show percentage in time and late splitted regarding country
train_set %>% mutate(payment = ifelse(Late == 1,"to late", "in time")) %>%
  ggplot(aes(payment, group = countryCode)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_fill_manual("Late", values = c("seagreen3", "tomato2")) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  facet_grid(~countryCode) +
  labs(y = "Percentage", title = "Country situation") +
  theme(legend.position = "none")

# show percentage in time and late splitted regarding quarterly invoice date
train_set %>% mutate(payment = ifelse(Late == 1,"1", "0")) %>%
  mutate(quarter = quarter(InvoiceDate, with_year = FALSE)) %>%
  ggplot(aes(payment, group = quarter)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_fill_manual("Late", values = c("seagreen3", "tomato2")) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  facet_grid(~quarter) +
  labs(y = "Percentage", title = "Invoice quarter situation") +
  theme(legend.position = "none")


######################
##  data wrangling  ##
######################

# add variable for quarter
train_set <- train_set %>% mutate(quarter = quarter(InvoiceDate, with_year = FALSE))
test_set <- test_set %>% mutate(quarter = quarter(InvoiceDate, with_year = FALSE))

# create a table grouped by customer to compute variables for mean of DaysToSettle, Late und DaysLate
custTable <- train_set %>% group_by(customerID) %>% summarize(mu_DaysToSettle = mean(DaysToSettle),
                                                              mu_Late = mean(Late),
                                                              mu_DaysLate = mean(DaysLate))

# stratify DaysToSettle
custTable <- custTable %>% mutate(strat_DaysToSettle = round(mu_DaysToSettle/5)*5)

# add values to train and test set
train_set <- left_join(train_set, custTable, by = "customerID")
test_set <- left_join(test_set, custTable, by = "customerID")
rm(custTable)

# stratify InvoiceAmount
train_set <- train_set %>% mutate(Amount_bin = ifelse(InvoiceAmount < 20,"small", ifelse(InvoiceAmount > 100, "high", "medium")))
test_set <- test_set %>% mutate(Amount_bin = ifelse(InvoiceAmount < 20,"small", ifelse(InvoiceAmount > 100, "high", "medium")))


# create dataset from train set to plot correlation matrix
corr_set <- train_set %>%
  mutate(countryCode = as.numeric(countryCode), 
  PaperlessBill = as.numeric(PaperlessBill),
  Disputed = as.numeric(Disputed),
  Amount_bin = as.numeric(factor(Amount_bin))) %>%
  select(countryCode, PaperlessBill, Disputed, Amount_bin, strat_DaysToSettle, mu_Late, quarter)

# compute correlations
res <- cor(corr_set)

# show correlation matrix
heatmap.2(res
          ,cellnote=round(res,2)
          ,notecol = "black"
          ,cexRow = 0.5
          ,cexCol = 0.5
          ,scale = "column"
          ,col = RColorBrewer::brewer.pal(11, "Spectral")
          ,main = "Correlation matrix"
          ,Colv = NA
          ,Rowv = NA
          ,key = FALSE)

# remove res
rm(res, corr_set)

##########################################################
##  Train models to predict if a payment will be late  ##
##########################################################

# prepare datasets for modeling and select predictors
test <- test_set %>%  select(countryCode, PaperlessBill, Disputed, Amount_bin, strat_DaysToSettle, mu_Late, quarter)
train <- train_set %>%  select(Late, countryCode, PaperlessBill, Disputed, Amount_bin, strat_DaysToSettle,mu_Late,
                               quarter)
train <- train %>% mutate(Late = as.factor(Late))

# train different models
models <- c("lda", "glm", "rpart","svmLinear", "knn", "gamLoess")
set.seed(1, sample.kind = "Rounding")
fits <- lapply(models, function(model){ 
  print(model)
  train(Late ~ ., method = model, data = train)
})

# predictions of different models
pred <- sapply(fits, function(object) 
  predict(object, newdata = test))
colnames(pred) <- models

# create ensemble
ensemble <- pred == 1
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, 1, 0)
pred_new <- cbind.data.frame(pred, ensemble_preds)

# compute accuracy of different models
all_models <- c(models, "ensemble")
n <- seq(1:length(all_models))

# compute accuracy, sensitivity, specificity
accuracy <- sapply(n, function(object)
  mean(pred_new[,object] == test_set$Late))

sensitivity <- sapply(n, function(object)
  confusionMatrix(factor(pred_new[,object]), reference = factor(test_set$Late))$byClass["Sensitivity"])

specificity <- sapply(n,function(object)
  confusionMatrix(factor(pred_new[,object]), reference = factor(test_set$Late))$byClass["Specificity"])

data.frame(Model = all_models, Accuracy = accuracy, Sensitivity = sensitivity,
           Specificity = specificity) %>% knitr::kable()

# compute prevalence
confusionMatrix(factor(pred_new[,1]), reference = factor(test_set$Late))$byClass["Prevalence"]
rm(pred, pred_new, n, all_models, ensemble_preds, ensemble, accuracy, sensitivity, specificity, models)

# extract important variables from different models
glm <- varImp(fits[[2]])$importance
glm <- rownames_to_column(glm)
rpart <- varImp(fits[[3]])$importance
rpart <- rownames_to_column(rpart)
gam <- varImp(fits[[6]])$importance
gam <- rownames_to_column(gam)

# create table to compare different importance
variables <- data.frame(rowname = glm[,1])
variables <- left_join(variables, glm, by = "rowname")
variables <- left_join(variables, rpart, by = "rowname")
variables <- left_join(variables, gam, by = "rowname")
colnames(variables) <- c("variables","glm", "rpart", "gamLoess")
options(digits = 2)
variables %>% knitr::kable()

rm(fits,variables, glm, rpart, gam)

##########################################################
##  Train models to predict if a payment will be late  ##
##########################################################

# prepare datasets for modeling and select predictors
test <- test_set %>%  select(countryCode, customerID, InvoiceAmount, PaperlessBill, Disputed, Amount_bin, 
                             mu_DaysToSettle, mu_DaysLate, strat_DaysToSettle, mu_Late, quarter)
train <- train_set %>%  select(DaysToSettle, countryCode, customerID, InvoiceAmount, PaperlessBill, Disputed,
                               Amount_bin, mu_DaysToSettle, mu_DaysLate, strat_DaysToSettle, mu_Late, quarter)

# create RMSE function
RMSE <- function(true_DaysToSettle, predicted_DaysToSettle){
  sqrt(mean((true_DaysToSettle - predicted_DaysToSettle)^2))
}

# train different models
models <- c("lm", "rpart","svmLinear", "knn", "gamLoess")
set.seed(1, sample.kind = "Rounding")
fits <- lapply(models, function(model){ 
  print(model)
  train(DaysToSettle ~ ., method = model, data = train)
})

# predictions of different models
pred <- sapply(fits, function(object) 
  predict(object, newdata = test))

# add columns for mean of DaysToSettle and payment target for comparison reason
target <- rep(c(30), times = nrow(test_set))
mean_DaysToSettle <- rep(c(mean(train_set$DaysToSettle)), times = nrow(test_set))
comparisons <- c(models, "target", "mean")
pred_new <- cbind.data.frame(pred, target, mean_DaysToSettle)
colnames(pred_new) <- comparisons

# compute RMSE for all col's
n <- seq(1:length(comparisons))
rmse <- sapply(n, function(num){
  RMSE(test_set$DaysToSettle, pred_new[,num])
})

data.frame(comparisons, rmse)[order(rmse),] %>% knitr::kable()
rm(fits, comparisons, rmse, target, mean_DaysToSettle, models)

# create table with payment dates for different estimates comparing to true payment day
# round predicted days
pred_new <- round(pred_new)

# convert days to dates and add true settledDate
pred_date <- cbind.data.frame(true = as.numeric(test_set$SettledDate), as.numeric(test_set$InvoiceDate) + pred_new)

# convert dates to periods
n <- seq(1:ncol(pred_date))
periods <- sapply(n, function(num)
  format(as.Date(pred_date[,num], origin="1970-01-01"), "%Y-%m"))
colnames(periods) <- names(pred_date)

# add amount to table
periods <- cbind.data.frame(Amount = test_set$InvoiceAmount,periods)

# create timeline
timeline <- gather(periods, key = "Amount") %>% group_by(value) %>% summarize()
colnames(timeline) <- "period"

# create datasets for 4 estimations
true_pay <- periods[,1:2] %>% group_by(true) %>% summarize(x = sum(Amount))
colnames(true_pay) <- c("period", "true_payment")
lm_pay <- periods[,c(1,3)] %>% group_by(lm) %>% summarize(x = sum(Amount))
colnames(lm_pay) <- c("period", "lm_payment")
target_pay <- periods[,c(1,8)] %>% group_by(target) %>% summarize(x = sum(Amount))
colnames(target_pay) <- c("period", "target_payment")
mean_pay <- periods[,c(1,9)] %>% group_by(mean) %>% summarize(x = sum(Amount))
colnames(mean_pay) <- c("period", "mean_payment")

# create table by left_join
cash_Table <- left_join(timeline, true_pay, by = "period")
cash_Table <- left_join(cash_Table, lm_pay, by = "period")
cash_Table <- left_join(cash_Table, target_pay, by = "period")
cash_Table <- left_join(cash_Table, mean_pay, by = "period")

# set 0 for na's
cash_Table[is.na(cash_Table)] <- 0

# compute differences 
cash_Table <- cash_Table %>% mutate(diff_lm = (lm_payment - true_payment),
                                   diff_target = (target_payment - true_payment),
                                   diff_mean = (mean_payment - true_payment))
# show table
cash_Table %>% knitr::kable()

# compare min and max differences
max <- apply(X = cash_Table[,6:8], MARGIN = 2, FUN = max, na.rm = TRUE)
min <- apply(X = cash_Table[,6:8], MARGIN = 2, FUN = min, na.rm = TRUE)
overview <- rbind.data.frame(max, min)
colnames(overview) <- c("diff true to lm", "diff true to target", "diff true to mean")
rownames(overview) <- c("highest positive difference in one month", "highest negative difference in one month")

# show overview
overview %>% knitr::kable()

# show ratio of differences based on average monthly cash income
options(digits = 4)
deviation <- overview/mean(cash_Table$true_payment)*100 
deviation %>% knitr::kable()

rm(pred_new, pred_date, n, periods, timeline, true_pay, lm_pay, target_pay, mean_pay, cash_Table, 
   pred, max, min, overview, deviation)
