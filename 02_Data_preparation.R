##############################################################################################
#                                                                                            #
#               DATA PREPARATION - modelling dataset at reference date 09.2009               #
#                                                                                            #     
##############################################################################################
# Loading libraries
library(readr)
library(dplyr)
library(data.table)
library(zoo)
library(lubridate)
ref_date <- "032016"
dir <- "/Users/mkunstler/Desktop/Magisterka/data/"
# Loading data
dataset <- readRDS(paste0(dir, "dataset_", ref_date, ".rds"))
# Columns names and types
column_names <- c("POOL_ID", "LOAN_ID", "ACT_PERIOD", "CHANNEL", "SELLER", "SERVICER",
                  "MASTER_SERVICER", "ORIG_RATE", "CURR_RATE", "ORIG_UPB", "ISSUANCE_UPB",
                  "CURRENT_UPB", "ORIG_TERM", "ORIG_DATE", "FIRST_PAY", "LOAN_AGE",
                  "REM_MONTHS", "ADJ_REM_MONTHS", "MATR_DT", "OLTV", "OCLTV",
                  "NUM_BO", "DTI", "CSCORE_B", "CSCORE_C", "FIRST_FLAG", "PURPOSE",
                  "PROP", "NO_UNITS", "OCC_STAT", "STATE", "MSA", "ZIP", "MI_PCT",
                  "PRODUCT", "PPMT_FLG", "IO", "FIRST_PAY_IO", "MNTHS_TO_AMTZ_IO",
                  "DLQ_STATUS", "PMT_HISTORY", "MOD_FLAG", "MI_CANCEL_FLAG", "Zero_Bal_Code",
                  "ZB_DTE", "LAST_UPB", "RPRCH_DTE", "CURR_SCHD_PRNCPL", "TOT_SCHD_PRNCPL",
                  "UNSCHD_PRNCPL_CURR", "LAST_PAID_INSTALLMENT_DATE", "FORECLOSURE_DATE",
                  "DISPOSITION_DATE", "FORECLOSURE_COSTS", "PROPERTY_PRESERVATION_AND_REPAIR_COSTS",
                  "ASSET_RECOVERY_COSTS", "MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS",
                  "ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY", "NET_SALES_PROCEEDS",
                  "CREDIT_ENHANCEMENT_PROCEEDS", "REPURCHASES_MAKE_WHOLE_PROCEEDS",
                  "OTHER_FORECLOSURE_PROCEEDS", "NON_INTEREST_BEARING_UPB", "PRINCIPAL_FORGIVENESS_AMOUNT",
                  "ORIGINAL_LIST_START_DATE", "ORIGINAL_LIST_PRICE", "CURRENT_LIST_START_DATE",
                  "CURRENT_LIST_PRICE", "ISSUE_SCOREB", "ISSUE_SCOREC", "CURR_SCOREB",
                  "CURR_SCOREC", "MI_TYPE", "SERV_IND", "CURRENT_PERIOD_MODIFICATION_LOSS_AMOUNT",
                  "CUMULATIVE_MODIFICATION_LOSS_AMOUNT", "CURRENT_PERIOD_CREDIT_EVENT_NET_GAIN_OR_LOSS",
                  "CUMULATIVE_CREDIT_EVENT_NET_GAIN_OR_LOSS", "HOMEREADY_PROGRAM_INDICATOR",
                  "FORECLOSURE_PRINCIPAL_WRITE_OFF_AMOUNT", "RELOCATION_MORTGAGE_INDICATOR",
                  "ZERO_BALANCE_CODE_CHANGE_DATE", "LOAN_HOLDBACK_INDICATOR", "LOAN_HOLDBACK_EFFECTIVE_DATE",
                  "DELINQUENT_ACCRUED_INTEREST", "PROPERTY_INSPECTION_WAIVER_INDICATOR",
                  "HIGH_BALANCE_LOAN_INDICATOR", "ARM_5_YR_INDICATOR", "ARM_PRODUCT_TYPE",
                  "MONTHS_UNTIL_FIRST_PAYMENT_RESET", "MONTHS_BETWEEN_SUBSEQUENT_PAYMENT_RESET",
                  "INTEREST_RATE_CHANGE_DATE", "PAYMENT_CHANGE_DATE", "ARM_INDEX",
                  "ARM_CAP_STRUCTURE", "INITIAL_INTEREST_RATE_CAP", "PERIODIC_INTEREST_RATE_CAP",
                  "LIFETIME_INTEREST_RATE_CAP", "MARGIN", "BALLOON_INDICATOR",
                  "PLAN_NUMBER", "FORBEARANCE_INDICATOR", "HIGH_LOAN_TO_VALUE_HLTV_REFINANCE_OPTION_INDICATOR",
                  "DEAL_NAME", "RE_PROCS_FLAG", "ADR_TYPE", "ADR_COUNT", "ADR_UPB", "QRT")
colnames(dataset) <- column_names


length(unique(dataset$LOAN_ID))
table(dataset$LOAN_AGE)
# Unique loan ids per quarter
# summary <- dataset %>% group_by(QRT) %>% distinct(LOAN_ID) %>% count
# write.csv(summary, "/run/media/ewelinka/3T/ewelina/DJL/3Q2020/summary_qtr_", ref_date, ".csv")

# Missing values handling
# https://loanperformancedata.fanniemae.com/lppub-docs/FNMA_SF_Loan_Performance_FAQs.pdf?_ga=2.129986820.905087143.1611573870-387794647.1609837568
# str. 9
miss_hand <- c(
  # Expenses
  "ASSET_RECOVERY_COSTS", 'ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY', 'FORECLOSURE_COSTS','MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS',
  'PROPERTY_PRESERVATION_AND_REPAIR_COSTS',
  # Proceeds
  'CREDIT_ENHANCEMENT_PROCEEDS','NET_SALES_PROCEEDS','OTHER_FORECLOSURE_PROCEEDS','REPURCHASES_MAKE_WHOLE_PROCEEDS',
  # Other
  "RPRCH_DTE"
)

for (i in miss_hand){
  print(i)
  dataset[[i]] <- ifelse(is.na(dataset[[i]]) | (!is.na(dataset[[i]]) & dataset[[i]]==''), 0, dataset[[i]])
}

# Date format
dates = c('ORIG_DATE','FIRST_PAY','MATR_DT','FIRST_PAY_IO','ZB_DTE','RPRCH_DTE', 'LAST_PAID_INSTALLMENT_DATE',
          'FORECLOSURE_DATE','DISPOSITION_DATE','ORIGINAL_LIST_START_DATE','CURRENT_LIST_START_DATE','ZERO_BALANCE_CODE_CHANGE_DATE',
          'LOAN_HOLDBACK_EFFECTIVE_DATE','INTEREST_RATE_CHANGE_DATE','PAYMENT_CHANGE_DATE')
for (i in dates){
  dataset[[i]] <- ifelse(is.na(dataset[[i]]) | dataset[[i]]=='', dataset[[i]], paste(substr(dataset[[i]], 3, 6), substr(dataset[[i]], 1, 2), '01', sep = "-"))
  dataset[[i]] <- as.Date(dataset[[i]], format="%Y-%m-%d")
}

# Acquisition date
acquisition_year <- substr(dataset$QRT, 1, 4)
acquisition_qtr <- substr(dataset$QRT, 5, 7)
acquisition_month <- ifelse(acquisition_qtr == 'Q1', '03', 
                            ifelse(acquisition_qtr == 'Q2', '06', 
                                   ifelse(acquisition_qtr == 'Q3', '09', '12')))

acquisition_date <- paste(acquisition_year, acquisition_month, '01', sep = "-")
dataset$AQSN_DTE <- acquisition_date

# ACT_PERIOD - from first day of the month to last day of the month
dataset$ACT_PERIOD <- ceiling_date(dataset$ACT_PERIOD, "month") - days(1)

rm(acquisition_year, acquisition_qtr, acquisition_month)
gc()

####################################################
#                                                  #
#           12-months loan history                 #  
#                                                  #
####################################################

data_history <- dataset %>% 
  filter(ACT_PERIOD <= ceiling_date(as.Date(paste(substr(ref_date, 3, 6), substr(ref_date, 1, 2), '01', sep = "-"), format="%Y-%m-%d"), "month") - days(1))

dim(data_history)
# 129961    110
gc()

# Delete variables with only NA values
na <- sapply(data_history, function(x) all(is.na(x)))
to_delete <- names(na)[na==T]
to_delete
na <- names(na)[na==F]
data_history <- subset(data_history, select=na)
dim(data_history)
# 129961     65

# choosing numerical variables
nums <- sapply(data_history, is.numeric)
data_n <- subset(data_history, select=nums)

# calculating unique values
unique <- sapply(data_n, function(x) length(unique(na.omit(x))))
unique0 <- names(unique)[unique==0]
unique1 <- names(unique)[unique==1]
unique2 <- names(unique)[unique>1 & unique<=5]
unique3 <- names(unique)[unique>5]

# numerical to factor
num_as_fact <- subset(data_history, select=unique2)
num_as_fact <- data.frame(lapply(num_as_fact, function(x) factor(x, ordered = T))) # 3 variables

# numerical
data_n <- subset(data_history, select=unique3) # 14 variables

# Dates
dates <- sapply(data_history, is.Date)
data_dates <- subset(data_history, select=dates) # 4 variables

unique_oth <- sapply(data_dates, function(x) length(unique(na.omit(x))))
unique_oth0 <- names(unique_oth)[unique_oth==0] # empty
unique_oth1 <- names(unique_oth)[unique_oth==1] # empty

# Character variables
chars <- sapply(data_history, is.character)
data_chr <- subset(data_history, select=chars)
dim(data_chr)

# Identifying and removing the "empty" columns
data_chr <- subset(data_chr, select=!sapply(data_chr, function(x) all(x == "")))
dim(data_chr)

# calculating unique values
unique <- sapply(data_chr, function(x) length(unique(na.omit(x))))
unique0 <- names(unique)[unique==0]
unique1 <- names(unique)[unique==1]
unique2 <- names(unique)[unique>1 & unique<=5]
unique3 <- names(unique)[unique>5]

# character to factor
char_as_fact <- subset(data_history, select=unique2)
char_as_fact <- data.frame(lapply(char_as_fact, function(x) factor(x, ordered = T))) # 10 variables

data_chr <- subset(data_history, select=unique3)

# joining
colnames(data_n)
colnames(num_as_fact)
colnames(data_dates)
colnames(data_chr)
colnames(char_as_fact)

data_history <- cbind(data_chr, data_dates, char_as_fact, data_n, num_as_fact)
rm(data_chr, data_dates, char_as_fact, data_n, num_as_fact)

dim(data_history)
# 8205080      36
saveRDS(data_history, paste0(dir, "dataset_", ref_date, "_history.rds"))

rm(data_history)
gc()

####################################################
#                                                  #
#          12-months performance window            #  
#                                                  #
####################################################

data_perf_window <- dataset %>% 
  filter(ACT_PERIOD > ceiling_date(as.Date(paste(substr(ref_date, 3, 6), substr(ref_date, 1, 2), '01', sep = "-"), format="%Y-%m-%d"), "month") - days(1))

dim(data_perf_window)
# 7053915     110

# Delete variables with only NA values
na <- sapply(data_perf_window, function(x) all(is.na(x)))
to_delete <- names(na)[na==T]
to_delete
na <- names(na)[na==F]
data_perf_window <- subset(data_perf_window, select=na)
dim(data_perf_window)

# choosing numerical variables
nums <- sapply(data_perf_window, is.numeric)
data_n <- subset(data_perf_window, select=nums)

# calculating unique values
unique <- sapply(data_n, function(x) length(unique(na.omit(x))))
unique0 <- names(unique)[unique==0]
unique1 <- names(unique)[unique==1]
unique2 <- names(unique)[unique>1 & unique<=5]
unique3 <- names(unique)[unique>5]

# numerical to factor
num_as_fact <- subset(data_perf_window, select=unique2)
num_as_fact <- data.frame(lapply(num_as_fact, function(x) factor(x, ordered = T))) # 3 variables

# numerical
data_n <- subset(data_perf_window, select=unique3) # 26 variables

# Dates
dates <- sapply(data_perf_window, is.Date)
data_dates <- subset(data_perf_window, select=dates) # 8 variables

unique_oth <- sapply(data_dates, function(x) length(unique(na.omit(x))))
unique_oth0 <- names(unique_oth)[unique_oth==0] # empty
unique_oth1 <- names(unique_oth)[unique_oth==1] # empty

# Character variables
chars <- sapply(data_perf_window, is.character)
data_chr <- subset(data_perf_window, select=chars)
dim(data_chr)

# Identifying and removing the "empty" columns
data_chr <- subset(data_chr, select = !sapply(data_chr, function(x) all(x == "")))
dim(data_chr)

# calculating unique values
unique <- sapply(data_chr, function(x) length(unique(na.omit(x))))
unique0 <- names(unique)[unique==0]
unique1 <- names(unique)[unique==1]
unique2 <- names(unique)[unique>1 & unique<=5]
unique3 <- names(unique)[unique>5]

# character to factor
char_as_fact <- subset(data_perf_window, select=unique2)
char_as_fact <- data.frame(lapply(char_as_fact, function(x) factor(x, ordered = T))) # 12 variables

data_chr <- subset(data_perf_window, select=unique3)

# joining
colnames(data_n)
colnames(num_as_fact)
colnames(data_dates)
colnames(data_chr)
colnames(char_as_fact)

data_perf_window <- cbind(data_chr, data_dates, char_as_fact, data_n, num_as_fact)
dim(data_perf_window)
# 471677      59

saveRDS(data_perf_window, paste0(dir, "dataset_", ref_date, "_perf_window.rds"))

# Clear R environment
rm(list = ls())
gc()
