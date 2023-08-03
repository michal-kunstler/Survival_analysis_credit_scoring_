
##############################################################################################
#                                                                                            #
#                                   DEFINITION OF DEFAULT                                    #
#                                                                                            #     
##############################################################################################

# Loading libraries
library(readr)
library(dplyr)
library(data.table)
library(zoo)
library(lubridate)
library(rpart)
library(rpart.plot)


# Define reference date
ref_date = '032016'

# Set directory
dir <- "/Users/mkunstler/Desktop/Magisterka/data/"

# Loading data
data <- readRDS(paste0(dir, "dataset_", ref_date, "_perf_window.rds"))

f90_table <- data %>%
  filter(DLQ_STATUS >= 3 & DLQ_STATUS < 999) %>%
  group_by(LOAN_ID) %>%
  summarize(F90_DTE = min(ACT_PERIOD)) %>%
  left_join(data, by = c("LOAN_ID" = "LOAN_ID", "F90_DTE" = "ACT_PERIOD")) %>%
  select(LOAN_ID, F90_DTE)

data <- data %>%
  left_join(f90_table, by = 'LOAN_ID')

rm(f90_table)
colnames(data)

##### Past due amount and DPD counting
# The amount past due shall be the sum of all amounts past due, including all fees, interest and principal. For the relative 
# threshold, this amount should be divided by the total on-balance exposure. In case the principal is not repaid or refinanced 
# when an interest-only loan expires, DPD counting should start from that date even if the obligor continues to pay interest.
# 
# There are also specific requirements for when DPD counting may be stopped — when the credit arrangement specifically allows 
# the obligor to change the schedule, when there are legal grounds for suspended repayment, in case of formal legal disputes 
# over the repayment or when the obligor changes due to a merger or similar event.

# Default definition
data <- data %>% 
  mutate(# flaga default na moment obserwacji
         default_flg=if_else(DLQ_STATUS >= 3 & DLQ_STATUS < 999, 1, 0),
         # flaga default na poziomie klienta
         DEFAULT=if_else(!is.na(F90_DTE), 1, 0)) 

# How many goods and bads
data %>% group_by(DEFAULT) %>% distinct(LOAN_ID) %>% count
# DEFAULT     n
# 1       0   9628
# 2       1   369

# Default flag based of Zero_bal_code variable
table(data$Zero_Bal_Code)
data <- data %>% filter(Zero_Bal_Code %in% c('', '01', '09', '15'))

data$Zero_Bal_Code[data$Zero_Bal_Code == ''] <- NA

library(tidyr)
data <- data %>%
  dplyr::group_by(LOAN_ID) %>%
  tidyr::fill(Zero_Bal_Code, .direction = "up") %>% 
  dplyr::ungroup()

data_1 <- data %>% 
  mutate(DEFAULT_zbc = if_else(Zero_Bal_Code=='01', 0, if_else(Zero_Bal_Code %in% c('09','15'), 1, -1)))

data_1 %>% group_by(DEFAULT_zbc) %>% distinct(LOAN_ID) %>% count
#             DEFAULT_zbc       n
# 1           0                 88968
# 2           1                 292
# 3          NA                 541900

# Differences between DEFAULT and DEFAULT_zbc flags
# test <- data_1 %>% filter(DEFAULT != DEFAULT_zbc & !is.na(DEFAULT_zbc)) %>% distinct(LOAN_ID)
# test1 <- data_1  %>% filter(LOAN_ID %in% test$LOAN_ID) %>% select(LOAN_ID, ACT_PERIOD, DEFAULT, DLQ_STATUS, DEFAULT_zbc, Zero_Bal_Code, F90_DTE, F90_UPB, ORIG_UPB)

data_1 %>% group_by(DEFAULT, DEFAULT_zbc) %>% summarise(n=n_distinct(LOAN_ID))
# DEFAULT DEFAULT_zbc      n
# 1       0           0  87889
# 2       0           1      4``
# 3       0          NA 528392
# 4       1           0   1079
# 5       1           1    288
# 6       1          NA  13508  

data_2 <- data_1 %>% filter(DEFAULT == DEFAULT_zbc | is.na(DEFAULT_zbc))

data_2 %>% group_by(DEFAULT, DEFAULT_zbc) %>% summarise(n=n_distinct(LOAN_ID))
# DEFAULT DEFAULT_zbc      n
# 1       0           0  87889
# 2       0          NA 528392
# 3       1           1    288
# 4       1          NA  13508

colnames(data_2)
rm(data, data_1)

# Joining default flag to historical data
data_history <- readRDS(paste0(dir, "dataset_", ref_date, "_history.rds"))

def_flag <- data_2 %>% distinct(LOAN_ID, DEFAULT) 
dim(def_flag) # 9862 unique loans

data_history <- left_join(data_history, def_flag, by=c("LOAN_ID"="LOAN_ID")) %>% filter(!is.na(DEFAULT))
colnames(data_history)

saveRDS(data_2, paste0(dir, "dataset_", ref_date, "_perf_window_1.rds"))
saveRDS(data_history, paste0(dir, "dataset_", ref_date, "_data_history_1.rds"))


# TUTAJ JEST MOJA MAGIA
data_surv <- data_2

data_max_act_period <-  data_2 %>%
  group_by(LOAN_ID) %>% 
  summarise(ACT_PERIOD_MAX = max(ACT_PERIOD))
  
data_surv <- left_join(data_surv, data_max_act_period, by=c("LOAN_ID"="LOAN_ID"))

data_surv$time_to_event <- ifelse(replace_na(data_surv$F90_DTE > 0, 0), interval(data_surv$ACT_PERIOD, data_surv$F90_DTE) %/% months(1), interval(data_surv$ACT_PERIOD, data_surv$ACT_PERIOD_MAX) %/% months(1))

data_surv$censored <- ifelse(data_surv$DEFAULT == 1, 0, 1)

data_surv <- data_surv %>% filter(ACT_PERIOD <= F90_DTE | is.na(F90_DTE))

colnames
# TUTAJ SIĘ KOŃCZY MOJA MAGIA
# podejście 1: bierzemy pierwszy możliwy moment, ignorujemy zmienne behawioralne
# podejście 2: dodajemy zmienne behawioralne, samplujemy obserwacje <- wole to, ma sens nadal jeżeli samplujemy obserwacje z performingu

# następne kroki -> 
# 1. przejrzeć zmienne na samplu perf i usunąć niepotrzebne
  # robię to na podstawie zmiennych usuwanych w zbiorze data_history
data_surv2 <- data_surv[,!(colnames(data_surv) %in% setdiff(colnames(data_surv), colnames(data_history))[1:23])]

for (i in 1:length(colnames(data_surv2))){
  if (nrow(unique(data_surv2[,i])) <= 1){
    print(colnames(data_surv2)[i])
  }
}
# no variables with le 1 different values are left
# 2. znaleźć słowniki do zmiennych (TYPU FACTOR ALBO TEKSTOWYCH) które były grupowane ALBO pogrupować je drzewem decyzyjnym
  # grupowanie będzie do 3 grup (arbitralna liczba grup), gdzie każda zmienna będzie zaklasyfikowana według mocy predykcyjnej na zmienną default
# 
# # Extract names of character and factor variables into a vector
# grouping_vars <- names(data_surv2)[sapply(data_surv2, is.character)][-1]
# #after trying, the only variables that can be grouped are MSA and ZIP
# grouping_vars <- c("MSA", "ZIP")
# target <- "censored"
# 
# # Define the number of leaves for the decision tree
# num_leaves <- 3
# 
# # Build decision tree model for each variable in V1
# for (var in grouping_vars) {
#   print(var)
#   # Create formula with current variable as predictor
#   fmla <- as.formula(paste(target, "~", var))
#   
#   # Fit decision tree model
#   model <- rpart(fmla, data = data_surv2, cp = -1, control = rpart.control(maxnode = 3, maxdepth = 2))
#   
#   rpart.plot(model)
#   # Get leaf node predictions
#   leaf_preds <- predict(model, type = "vector")
#   
#   print(unique(leaf_preds))
#   
#   # Get leaf node assignments for each observation
#   # leaf_assignments <- apply(leaf_preds, 1, function(x) which.max(x))
#   
#   # Add leaf node assignments as a new column to the original dataset
#   # data_surv2[paste0(var, "_gr")] <- as.factor(leaf_assignments)
# }

# 3. dograć historię, dać flagi performing i historia

data_history$hist_flg <- 1
data_surv2$hist_flg <- 0

data_full <- rbind(data_history, data_surv2, fill=TRUE)

# 4. przeliczyć drivery behawioralne według kolejnych kodów

# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

data_full$ACT_PERIOD <- floor_date(data_full$ACT_PERIOD, 'month')
data_full$DLQ_STATUS <- as.numeric(as.character(data_full$DLQ_STATUS))

library(slider)
# DPD/DOD
start_time <- Sys.time()
data_1 <- data_full %>%
  group_by(LOAN_ID) %>%
  arrange(LOAN_ID, ACT_PERIOD) %>%
  mutate(
    # Max deliquency status in last 3, 6, 12 months
    max_deliq_6m = slider::slide_index_dbl(DLQ_STATUS, as.Date(ACT_PERIOD), max, .before = months(5)),
    max_deliq_12m = slider::slide_index_dbl(DLQ_STATUS, as.Date(ACT_PERIOD), max, .before = months(11)),
    # Stosunek max stat z ostatnich 3/6, 6/12
    max_deliq_6_12 = coalesce(max_deliq_6m/max_deliq_12m, 0),

    if_deliq = if_else(DLQ_STATUS>=1, 1, 0),

    # Ile miesiecy ze statusem opóźienia w ostatnich 3/6/12
    deliq_stat_6m = slider::slide_index_dbl(if_deliq, as.Date(ACT_PERIOD), sum, .before = months(5)),
    deliq_stat_12m = slider::slide_index_dbl(if_deliq, as.Date(ACT_PERIOD), sum, .before = months(11)),
    
    deliq_stat_avg_3_12 = coalesce(slider::slide_index_dbl(if_deliq, as.Date(ACT_PERIOD), mean, .before = months(2))/slider::slide_index_dbl(if_deliq, ACT_PERIOD, mean, .before = months(11)), 0),
    deliq_stat_avg_6_12 = coalesce(slider::slide_index_dbl(if_deliq, as.Date(ACT_PERIOD), mean, .before = months(5))/slider::slide_index_dbl(if_deliq, ACT_PERIOD, mean, .before = months(11)), 0)
  ) %>% 
  ungroup()

end_time <- Sys.time()
end_time - start_time

### Create first base table with a copy of acquisition fields plus AQSN_DTE field and recodes of MI_TYPE and OCLTV
data_1 <- data_1 %>%
  mutate(
    MI_TYPE = case_when(
      MI_TYPE == '1' ~ 'BPMI', #MI_TYPE is recoded to be more descriptive
      MI_TYPE == '2' ~ 'LPMI',
      MI_TYPE == '3' ~ 'IPMI',
      TRUE ~ 'None'
    ),
    OCLTV = if_else(is.na(OCLTV), OLTV, OCLTV) #If OCLTV is missing, we replace it with OLTV
  )
data_1$MI_TYPE <- as.factor(data_1$MI_TYPE)
colnames(data_1)

#DescTools::Desc(data_1)

delete <- c("SERVICER", "SELLER", "STATE", "MSA", "ZIP", "ORIG_DATE", "FIRST_PAY", 
            "REM_MONTHS", "MATR_DT", "RELOCATION_MORTGAGE_INDICATOR", "SERV_IND", "DEFAULT_zbc")

data_2 <- data_1[ , !(names(data_1) %in% delete)]

library(caret)

rm(data_1, data_full, data_history, data_surv, data_surv2)

data_2$LOAN_ID <- as.numeric(data_2$LOAN_ID)

data_2$CHANNEL <- as.factor(as.character(data_2$CHANNEL))
data_2$NUM_BO <- as.factor(as.character(data_2$NUM_BO))
data_2$FIRST_FLAG <- as.factor(as.character(data_2$FIRST_FLAG))
data_2$PURPOSE <- as.factor(as.character(data_2$PURPOSE))
data_2$PROP <- as.factor(as.character(data_2$PROP))
data_2$OCC_STAT <- as.factor(as.character(data_2$OCC_STAT))
data_2$HIGH_BALANCE_LOAN_INDICATOR <- as.factor(as.character(data_2$HIGH_BALANCE_LOAN_INDICATOR))
data_2$NO_UNITS <- as.factor(as.character(data_2$NO_UNITS))
data_2$MI_TYPE <- as.factor(as.character(data_2$MI_TYPE))


dmy <- dummyVars(" ~ .", data = data_2)
data_3 <- data.frame(predict(dmy, newdata = data_2))

data_3$LOAN_ID <- as.character(data_3$LOAN_ID)

# 6. ogarnąć missingi w numerycznych zmiennych przez medianę

# impute missing values with mean in numeric columns of df, except "example"
num_cols <- data_3[, sapply(data_3, is.numeric) & !grepl("^(F90_DTE|default_flg|ACT_PERIOD_MAX|time_to_event|censored)$", names(data_3))]
for (col in names(num_cols)) {
  if (is.numeric(num_cols[[col]])) {
    num_cols[[col]][is.na(num_cols[[col]])] <- median(num_cols[[col]], na.rm = TRUE)
  }
}

data_3[, names(num_cols)] <- num_cols
rm(num_cols)

# 9. można modelować i robić XAI DO 15.04
data_samples <- data_3 %>% filter(hist_flg == 0 & time_to_event != 0) %>% 
                group_by(across(c(-ACT_PERIOD, -CURRENT_UPB, -LOAN_AGE, -ADJ_REM_MONTHS, -DLQ_STATUS, -time_to_event))) %>% 
                summarise(ACT_PERIOD = min(ACT_PERIOD), tte_min = min(time_to_event), time_to_event = max(time_to_event))

data_to_join <- data_3 %>%  select(LOAN_ID, ACT_PERIOD, CURRENT_UPB, LOAN_AGE, ADJ_REM_MONTHS, DLQ_STATUS)

data_sampled <- data_samples %>% left_join(data_to_join, by = c('LOAN_ID', "ACT_PERIOD"))

library(survminer)

data_sampled$status <- if_else(data_sampled$censored == 1, 1, 2)

data_final <- data_sampled %>%ungroup() %>% 
              filter(hist_flg == 0) %>% 
              select(-tte_min, -censored,  -hist_flg, -ACT_PERIOD_MAX, -F90_DTE, -DLQ_STATUS)


default_summary_table <- data_2 %>% filter(default_flg %in% c(0,1)) %>% group_by(LOAN_ID) %>% summarise(default = max(default_flg))

#number of loans 9862
nrow(default_summary_table)

#number of defaults 235
sum(default_summary_table$default)

#number of non-defaults 9627
nrow(default_summary_table) - sum(default_summary_table$default)

#share of defaults 0.02382884
sum(default_summary_table$default)/nrow(default_summary_table)

#share of non defaults 0.9761712
(nrow(default_summary_table) - sum(default_summary_table$default))/nrow(default_summary_table)



#distribution of survival time of defaulted facilities
default_surv_table <- data_2 %>% filter(default_flg %in% c(0,1)) %>% 
  group_by(LOAN_ID) %>% 
  summarise(survived = max(time_to_event), default = max(default_flg)) %>% 
  filter(default == 1) 

survobject <- Surv(time = default_surv_table$survived, event = default_surv_table$default)
fit_for_plot <- survfit(survobject ~ 1, data = default_surv_table)
#plot for data description
ggsurvplot(fit_for_plot, data = default_surv_table)

# zrobić train testsplit
#make this reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(data_final), replace=TRUE, prob=c(0.7,0.3))
train  <- data_final[sample, ]
test   <- data_final[!sample, ]

# SAVING THE FINAL DATAFRAME
write_csv(train, "train_fin.csv")
write_csv(test, "test_fin.csv")

library(survivalmodels)

features <- c('CHANNEL.B', 'CHANNEL.C', 'CHANNEL.R', 'NUM_BO.1', 'NUM_BO.2',
              'NUM_BO.3', 'NUM_BO.4', 'FIRST_FLAG.N', 'FIRST_FLAG.Y', 'PURPOSE.C',
              'PURPOSE.P', 'PURPOSE.R', 'PROP.CO', 'PROP.CP', 'PROP.MH', 'PROP.PU',
              'PROP.SF', 'OCC_STAT.I', 'OCC_STAT.P', 'OCC_STAT.S',
              'HIGH_BALANCE_LOAN_INDICATOR.N', 'HIGH_BALANCE_LOAN_INDICATOR.Y',
              'ORIG_RATE', 'CURR_RATE', 'ORIG_UPB', 'ORIG_TERM', 'OLTV', 'OCLTV',
              'DTI', 'CSCORE_B', 'CSCORE_C', 'MI_PCT', 'NO_UNITS.1', 'NO_UNITS.2',
              'NO_UNITS.3', 'NO_UNITS.4', 'MI_TYPE.BPMI', 'MI_TYPE.LPMI',
              'MI_TYPE.None', 'max_deliq_6m',
              'max_deliq_12m', 'max_deliq_6_12', 'if_deliq', 'deliq_stat_6m',
              'deliq_stat_12m', 'deliq_stat_avg_3_12', 'deliq_stat_avg_6_12',
              'ACT_PERIOD', 'CURRENT_UPB', 'LOAN_AGE',
              'ADJ_REM_MONTHS')

targets <- c("time_to_event", "status")

train_NN <- train %>% dplyr::select(features, targets) %>% mutate(event = if_else(status == 2, TRUE, FALSE)) %>% select(-status)
test_NN <- test %>% dplyr::select(features, targets) %>% mutate(event = if_else(status == 2, TRUE, FALSE)) %>% select(-status)

# Load necessary libraries
library(survival)
library(ranger)
library(mlr)
library(survex)

# Load your data
# Assume df is your data frame with survival data
# and 'time', 'status' are your time-to-event and event indicator columns

# Define the control function for training
ctrl <- trainControl(method = "cv", number = 5)

# Create a task for mlr
surv_task <- makeSurvTask(id = "surv", data = train_NN, target = c("time_to_event", "event"))

# Create a learner for the ranger method
lrn <- makeLearner("surv.ranger", predict.type = "response")

# Define the parameter set for tuning
ps <- makeParamSet(
  makeDiscreteParam("splitrule", values = c("extratrees", "logrank")),
  makeIntegerParam("mtry", lower = 2, upper = sqrt(ncol(train_NN))),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)

# Define the control function for tuning
ctrl <- makeTuneControlRandom(maxit = 100L)

set.seed(1)
# Perform hyperparameter tuning
res <- tuneParams(lrn, task = surv_task, resampling = cv5, par.set = ps, control = ctrl)

# Print the best tuning parameters
print(res$x)


# Define your parameters
params <- list(
  splitrule = "logrank",
  mtry = 5,
  min.node.size = 1
)

set.seed(1)
# Fit the Random Survival Forest model
model <- ranger(
  formula = Surv(time_to_event, event) ~ .,
  data = train_NN,
  importance = 'permutation',
  splitrule = params$splitrule,
  mtry = params$mtry,
  min.node.size = params$min.node.size
)

# Predict on the test set
preds <- predict(model, newdata = test_NN)

# Create explanation for the model using the 'survex' package
test_NN$LOAN_ID <- paste0(test$LOAN_ID, test$ACT_PERIOD)

explanation <- explain(model, data = test_NN, y = Surv(test_NN$time_to_event, test_NN$event))


m_parts <- model_parts(explanation)
plot(m_parts)

m_profile <- model_profile(explanation)
plot(m_profile)

m_perf <- model_performance(explanation)
plot(m_perf)

train_NN <- train_NN %>% select(-ACT_PERIOD)
test_NN <- test_NN %>% select(-ACT_PERIOD)

# Normalize the input data
mean <- apply(train_NN[, -c(51,52)], 2, mean)
std <- apply(train_NN[, -c(51,52)], 2, sd)

train_regular <- train_NN
test_regular <- test_NN

train_NN[, -c(51,52)] <- scale(train_NN[, -c(51,52)], center = mean, scale = std)
test_NN[, -c(51,52)] <- scale(test_NN[, -c(51,52)], center = mean, scale = std)

library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3tuning)

#standardized
task_train_NN = TaskSurv$new("PD_train", backend = train_NN, time = "time_to_event", event = "event")
task_test_NN = TaskSurv$new("PD_test", backend = test_NN, time = "time_to_event", event = "event")

#non-standardized
task_train = TaskSurv$new("PD_train", backend = train_regular, time = "time_to_event", event = "event")
task_test = TaskSurv$new("PD_test", backend = test_regular, time = "time_to_event", event = "event")


# COXPH
# Create the learner
learner_coxph = lrn("surv.coxph")

# Train the learner
learner_coxph$train(task_train)

# Make predictions on the testing task
prediction_coxph = learner_coxph$predict(task_test)
prediction_trian_coxph = learner_coxph$predict(task_train)

# Calculate the C-index
cindex_coxph = prediction_coxph$score(msr("surv.cindex"))
cindex_trian_coxph = prediction_trian_coxph$score(msr("surv.cindex"))
# logloss_coxph = prediction_coxph$score(msr("surv.logloss"))



# deephit
param_set_deephit <- ParamSet$new(params = list(
  ParamDbl$new("dropout", lower = 0.1, upper = 0.5),
  ParamInt$new("num_nodes", lower = 16, upper = 64),
  ParamDbl$new("learning_rate", lower = 0.001, upper = 0.01)
))
# Create the learner
learner_deephit = lrn("surv.deephit")
learner_deephit$param_set$values = list(optimizer = "adam")

# Train the learner
at_deephit <- AutoTuner$new(learner_deephit, resampling = rsmp("cv", folds = 3), measure = msr("surv.cindex"), 
                        search_space = param_set_deephit, terminator = trm("evals", n_evals = 20), tuner = tnr("random_search"))
at_deephit$train(task_train_NN)

best_params_deephit <- at_deephit$tuning_result
# Make predictions on the testing task
prediction_deephit = at_deephit$predict(task_test_NN)
prediction_train_deephit = at_deephit$predict(task_train_NN)

# Calculate the C-index
cindex_deephit = prediction_deephit$score(msr("surv.cindex"))
cindex_train_deephit = prediction_train_deephit$score(msr("surv.cindex"))


# gbm

# Define a parameter set for Gradient Boosting Machine
param_set_gbm <- ParamSet$new(params = list(
  ParamInt$new("n.trees", lower = 100, upper = 500),
  ParamInt$new("interaction.depth", lower = 1, upper = 10),
  ParamDbl$new("shrinkage", lower = 0.01, upper = 0.3),
  ParamInt$new("n.minobsinnode", lower = 10, upper = 30)
))

# Create the learner
learner_gbm = lrn("surv.gbm")



at_gbm <- AutoTuner$new(learner_gbm, resampling = rsmp("cv", folds = 5), measure = msr("surv.cindex"), 
                        search_space = param_set_gbm, terminator = trm("evals", n_evals = 20), tuner = tnr("random_search"))
at_gbm$train(task_train)

best_params_gbm <- at_gbm$tuning_result

# Make predictions on the testing task
prediction_gbm = at_gbm$predict(task_test)
prediction_train_gbm = at_gbm$predict(task_train)
# Calculate the C-index
cindex_gbm = prediction_gbm$score(msr("surv.cindex"))
cindex_train_gbm = prediction_train_gbm$score(msr("surv.cindex"))




library(CoxBoost)
# Define a parameter set for Cox Boost
param_set_coxboost <- ParamSet$new(params = list(
  ParamInt$new("stepno", lower = 50, upper = 500),
  ParamDbl$new("penalty", lower = 100, upper = 2000)
))

# Create the learner
learner_coxboost = lrn("surv.coxboost")



at_coxboost <- AutoTuner$new(learner_coxboost, resampling = rsmp("cv", folds = 5), measure = msr("surv.cindex"), 
                        search_space = param_set_coxboost, terminator = trm("evals", n_evals = 5), tuner = tnr("random_search"))
at_coxboost$train(task_train)

best_params_coxboost <- at_coxboost$tuning_result

# Make predictions on the testing task
prediction_coxboost = at_coxboost$predict(task_test)
prediction_train_coxboost = at_coxboost$predict(task_train)
# Calculate the C-index
cindex_coxboost = prediction_coxboost$score(msr("surv.cindex"))
cindex_train_coxboost = prediction_train_coxboost$score(msr("surv.cindex"))






# deepsurv

# Define a parameter set for DeepSurv
param_set_deepsurv <- ParamSet$new(params = list(
  ParamDbl$new("dropout", lower = 0.1, upper = 0.5),
  ParamInt$new("num_nodes", lower = 16, upper = 64),
  ParamDbl$new("learning_rate", lower = 0.001, upper = 0.01)
))
# Create the learner
learner_deepsurv = lrn("surv.deepsurv")
learner_deepsurv$param_set$values = list(optimizer = "adam")

# Train the learner
at_deepsurv <- AutoTuner$new(learner_deepsurv, resampling = rsmp("cv", folds = 3), measure = msr("surv.cindex"), 
                            search_space = param_set_deepsurv, terminator = trm("evals", n_evals = 20), tuner = tnr("random_search"))
at_deepsurv$train(task_train_NN)

best_params_deepsurv <- at_deepsurv$tuning_result
# Make predictions on the testing task
prediction_deepsurv = at_deepsurv$predict(task_test_NN)
prediction_train_deepsurv = at_deepsurv$predict(task_train_NN)
# Calculate the C-index
cindex_deepsurv = prediction_deepsurv$score(msr("surv.cindex"))
cindex_train_deepsurv = prediction_train_deepsurv$score(msr("surv.cindex"))



# xgboost
param_set_xgboost <- ParamSet$new(params = list(
  ParamInt$new("nrounds", lower = 100, upper = 500),
  ParamDbl$new("eta", lower = 0.01, upper = 0.3),
  ParamDbl$new("gamma", lower = 0, upper = 1),
  ParamInt$new("max_depth", lower = 3, upper = 10),
  ParamDbl$new("min_child_weight", lower = 1, upper = 5),
  ParamDbl$new("subsample", lower = 0.5, upper = 1),
  ParamDbl$new("colsample_bytree", lower = 0.5, upper = 1)
))
# Create the learner
learner_xgboost = lrn("surv.xgboost")

# Train the learner on the training task
at_xgboost  <- AutoTuner$new(learner_xgboost, resampling = rsmp("cv", folds = 5), measure = msr("surv.cindex"), 
                        search_space = param_set_xgboost, terminator = trm("evals", n_evals = 30), tuner = tnr("random_search"))
at_xgboost$train(task_train)

best_params_xgboost <- at_xgboost $tuning_result
# Make predictions on the testing task
prediction_train_xgboost = at_xgboost$predict(task_test)
prediction_train_xgboost = at_xgboost$predict(task_train)
# Calculate the C-index
cindex_xgboost = prediction_xgboost$score(msr("surv.cindex"))
cindex_train_xgboost = prediction_train_xgboost$score(msr("surv.cindex"))


# randomforest
param_set_randomforest <- ParamSet$new(params = list(
  ParamInt$new("mtry", lower = 2, upper = ceiling(sqrt(ncol(train_NN)))),
  ParamInt$new("min.node.size", lower = 1, upper = 10)
))
# Create the learner
learner_randomforest = lrn("surv.ranger")

# Train the learner on the training task
at_randomforest  <- AutoTuner$new(learner_randomforest, resampling = rsmp("cv", folds = 3), measure = msr("surv.cindex"), 
                             search_space = param_set_randomforest, terminator = trm("evals", n_evals = 10), tuner = tnr("random_search"))
at_randomforest$train(task_train)

best_params_randomforest <- at_randomforest$tuning_result
# Make predictions on the testing task
prediction_randomforest = at_randomforest$predict(task_test)
prediction_train_randomforest = at_randomforest$predict(task_train)
# Calculate the C-index
cindex_randomforest = prediction_randomforest$score(msr("surv.cindex"))
cindex_train_randomforest = prediction_train_randomforest$score(msr("surv.cindex"))




# XAI with survex, 3 model architectures compared - neural network, decision tree ensamble and COXPH based

# Create explanation for the model using the 'survex' package
library(survex)

#DeepSurv explainer
explanation_deepsurv <- explain(at_deepsurv$learner, data = train_NN[, -c(51,52)], y = Surv(train_NN$time_to_event, train_NN$event), label = "DeepSurv")

m_parts_deepsurv <- model_parts(explanation_deepsurv)
plot(m_parts_deepsurv) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


m_profile_deepsurv <- model_profile(explanation_deepsurv, variables = c("OLTV", "CSCORE_B"))
plot(m_profile_deepsurv)

m_profile_deepsurv2 <- model_profile(explanation_deepsurv, variables = c("ORIG_TERM", "LOAN_AGE"))
plot(m_profile_deepsurv2)


#Random Forest explainer
explanation_randomforest <- explain(at_randomforest$learner, data = train_regular[, -c(51,52)], y = Surv(train_regular$time_to_event, train_regular$event), label = "Random Forest")

m_parts_randomforest <- model_parts(explanation_randomforest)
plot(m_parts_randomforest) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

m_profile_randomforest <- model_profile(explanation_randomforest, variables = c("OLTV", "CSCORE_B"))
plot(m_profile_randomforest)

m_profile_randomforest2 <- model_profile(explanation_randomforest, variables = c("ORIG_TERM", "LOAN_AGE"))
plot(m_profile_randomforest2)

#Cox PH explainer
explanation_coxph <- explain(learner_coxph, data = train_regular[, -c(51,52)], y = Surv(train_regular$time_to_event, train_regular$event), label = "Cox Proportional Hazards")

m_parts_coxph <- model_parts(explanation_coxph)
plot(m_parts_coxph) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


m_profile_coxph <- model_profile(explanation_coxph, variables = c("OLTV", "CSCORE_B"))
plot(m_profile_coxph)

m_profile_coxph2 <- model_profile(explanation_coxph, variables = c("ORIG_TERM", "LOAN_AGE"))
plot(m_profile_coxph2)



# Clear R environment
# rm(list = ls())
# gc()data_defs