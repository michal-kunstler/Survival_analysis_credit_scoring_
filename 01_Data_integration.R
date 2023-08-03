
##############################################################################################
#                                                                                            #
#               DATA INTEGRATION - modeling dataset at reference date 09.2009               #
#                                                                                            #     
##############################################################################################

df <- fread("Original_files/2017Q1.csv")
colnames(df) <- column_names

new_period <- filter(df, df$LOAN_AGE == 1)
length(unique(df$LOAN_ID))
unique(df$DLQ_STATUS)
table(df$LOAN_AGE)


data <- data %>% mutate(ACT_PERIOD = paste(substr(ACT_PERIOD, 3, 6), substr(ACT_PERIOD, 1, 2), '01', sep = "-"),
                        ACT_PERIOD = as.Date(ACT_PERIOD, format="%Y-%m-%d"),
                        AQSN_DTE = gsub("[_][\\s\\S]*$", "", files_split[i], perl=T),
                        DLQ_STATUS = if_else(DLQ_STATUS == 'XX', '999', DLQ_STATUS),
                        DLQ_STATUS = as.integer(DLQ_STATUS))
# test <- filter()
# library(usethis) 
# usethis::edit_r_environ()
# Loading libraries
library(readr)
library(dplyr)
library(data.table)
library(zoo)
library(lubridate)

gc()

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
                  "DEAL_NAME", "RE_PROCS_FLAG", "ADR_TYPE", "ADR_COUNT", "ADR_UPB")
column_classes <- c("character", "character", "character", "character", "character", "character",
                    "character", "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "character", "character", "numeric", "numeric",
                    "numeric", "character", "numeric", "numeric", "character", "numeric",
                    "numeric", "numeric", "character", "character", "character",
                    "numeric", "character", "character", "character", "character",
                    "numeric", "character", "character", "character", "character",
                    "numeric", "character", "character", "character", "character",
                    "character", "character", "numeric", "character", "numeric",
                    "numeric", "numeric", "character", "character", "character",
                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "numeric", "numeric", "numeric", "character",
                    "numeric", "character", "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "character", "numeric", "numeric", "numeric",
                    "numeric", "character", "numeric", "character", "numeric", "character",
                    "numeric", "numeric", "character", "character", "numeric", "numeric",
                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "numeric", "numeric", "numeric", "character",
                    "character", "character", "character", "character",
                    "character", "numeric", "numeric")


qrt <- function(x){
  month <- as.numeric(format(start_date, format = "%m"))[1]
  if (month <= 3) {
    quarter <- paste(format(start_date, format = "%Y")[1], "Q1", sep="")
  } else if (month > 3 & month <= 6) {
    quarter <- paste( format(start_date, format = "%Y")[1], "Q2", sep="")            
  } else if (month > 6 & month <= 9) {
    quarter <- paste( format(start_date, format = "%Y")[1], "Q3", sep="")
  } else if (month > 9) {
    quarter <- paste( format(start_date, format = "%Y")[1], "Q4", sep="")
  }
  quarter
}

# Set directory
# In the directory there are tree folders required in order to run the code without any changes: 
# 1. Original_files (should contain csv files for each quarter of interest)
# 2. Split_files (will be populated with split files of the original files)
# 3. Results (will be populated with data obtained after applying algorithm of selecting loan ids 
#    having continuous history 12 months before and after reference date)
dir = paste0('/Users/mkunstler/Desktop/Magisterka/data/')

# Listing original files (and also paths) in a Directory/Folder
files_orig <- list.files(paste0(dir, "Original_files/"), pattern = "(\\d{4}[a-zA-Z]\\d{1}).csv")
paths_orig <- list.files(paste0(dir, "Original_files"), pattern = "(\\d{4}[a-zA-Z]\\d{1}).csv", full.names = TRUE)

# Splitting files into N txt files including 2 mln obserations each (it is done by row so the last file might be less heavy)
# Solution adopted on Fedora; Windows example: https://stackoverflow.com/questions/20602869/batch-file-to-split-csv-file
setwd("/Users/mkunstler/Desktop/Magisterka/data/Split_files/")
 for (i in 1:length(paths_orig)) {
    x <- paste0('split -l 2000000 ', paths_orig[i], " ", gsub("[.][\\s\\S]*$", "", files_orig[i], perl=T), '_new')
    system(x)
 }

# Setting seed
set.seed(1234)

# Splitting original data might result in a situation that we will split the history of some loan ids and treat them as not 
# proper for the final dataset (no continuity in data for 25 subsequent months) while in fact these are proper loans.
# We could modify the solution by sourcing two datasets that are subsequent and try to rescue the last loan id. However it would result
# in longer execution of the code. What is more, we ultimately sample only about 1000 loan ids per quarter so we decide not to rescue 
# for those loans because we could later not even include them in the analysis due to sampling.

# Define reference date
ref_date = c(#'032015', '062015','092015','122015',
             '032016' #, '062016','092016','122016',
             # '032017', '062017','092017','122017',
             # '032018', '062018', '092018','122018',
             #'032019', '062019','092019','122019',
             #'032020', '062020','092020','122020',
             #'032021', '062021','092021','122021'
             )

# Listing split files (and also paths) in a Directory/Folder
files_split <- list.files(paste0(dir, "Split_files"), pattern = "(\\d{4}[a-zA-Z]\\d{1})[_]")
paths_split <- list.files(paste0(dir, "Split_files"), pattern = "(\\d{4}[a-zA-Z]\\d{1})[_]", full.names = TRUE)

for (date in 1:length(ref_date)){
  ref_date_d = as.Date(paste(substr(ref_date[date], 3, 6), substr(ref_date[date], 1, 2), '01', sep = "-"), format="%Y-%m-%d")
  start_date = as.Date(paste(substr(ref_date[date], 3, 6), substr(ref_date[date], 1, 2), '01', sep = "-"), format="%Y-%m-%d") %m-% months(12)

  # Setting length of the second loop
  dlg = sum(gsub("[_][\\s\\S]*$", "", files_split, perl=T) <= qrt(start_date))
  if( dlg == 0 ){ next }
  # Obtaining table with number of documents created after split out of each quarter 
  patterns <- gsub("[_][\\s\\S]*$", "", files_split, perl=T)
  df <- table(patterns)
  
  for(i in 1:dlg) {
    # Loading data
    data <- fread(paths_split[i], sep="|", col.names = column_names, colClasses=column_classes, nThread=4)
    # Formatting ACT_PERIOD as date
    data <- data %>% mutate(ACT_PERIOD = paste(substr(ACT_PERIOD, 3, 6), substr(ACT_PERIOD, 1, 2), '01', sep = "-"),
                            ACT_PERIOD = as.Date(ACT_PERIOD, format="%Y-%m-%d"),
                            AQSN_DTE = gsub("[_][\\s\\S]*$", "", files_split[i], perl=T),
                            DLQ_STATUS = if_else(DLQ_STATUS == 'XX', '999', DLQ_STATUS),
                            DLQ_STATUS = as.integer(DLQ_STATUS))
    # Limiting data to period of interest
    data <- with(data, data[(ACT_PERIOD >= start_date),])
    # Limiting data to loans that at the reference date are not 90 days delinquent
    good_loans <- data %>% filter(ACT_PERIOD == ref_date_d & (DLQ_STATUS<3 | is.na(DLQ_STATUS))) %>% distinct(LOAN_ID)
    data <- data[data$LOAN_ID %in% good_loans$LOAN_ID,]
    rm(good_loans)
    # Assuring continuity in ACT_PERIOD - 12 months before reference date
    full_before <- data %>% filter(ACT_PERIOD <= ref_date_d) %>% count(LOAN_ID) %>% filter(n==13)
    
    if (dim(full_before)[1] == 0) { next }
    
    data <- data[data$LOAN_ID %in% full_before$LOAN_ID,]
    rm(full_before)
    
    # sampling loan ids
    n = 10000/df[[gsub("[_][\\s\\S]*$", "", files_split[i], perl=T)]]
    if(as.numeric(data %>% distinct(LOAN_ID) %>% count)>=n) {n=n} else {n=as.numeric(data %>% distinct(LOAN_ID) %>% count)}
    a <- sample_n(as.data.frame(data$LOAN_ID) %>% distinct(), n, replace = FALSE)
    data <- data[data$LOAN_ID %in% a$`data$LOAN_ID`==TRUE,]
    # saving to RDS
    saveRDS(data, paste0(dir, "Results/", files_split[i], "_", ref_date[date] ,".rds"))
    rm(data, a)
    gc()
  }
  
  # Joining files
  files_results <- list.files(paste0(dir, "Results"), pattern = "(\\d{4}[a-zA-Z]\\d{1})[_]")
  paths_results <- list.files(paste0(dir, "Results"), pattern = "(\\d{4}[a-zA-Z]\\d{1})[_]", full.names = TRUE)
  
  paths_results_upd = paths_results[gsub(".*[_]([^.]+)[.].*", "\\1", paths_results, perl=T) == ref_date[date]]
  
  all_results = list()
  for (j in 1:length(paths_results_upd)){
    data <- readRDS(paths_results_upd[j])
    data <- as.data.table(data)
    all_results[[j]] <- data
    rm(data)
    gc()
  }

    all_results <- lapply(all_results, setNames, c(column_names, "QRT"))
    DT <- rbindlist(all_results)
    rm(all_results)
    saveRDS(DT, paste0(dir, "dataset_", ref_date[date], ".rds"))
    rm(DT, files_results, paths_results, paths_results_upd)
    gc()
}
