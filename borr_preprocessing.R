borr_preprocessing <- function(df,df_circ_pre) {
  library(dplyr)
  df_borr_tran <- df
  
  df_borr_tran <- df_borr_tran %>% select(-one_of('AccNum', 'TransactionID', 'FirstName', 'Salutation', 'RctNumber', 'RctDate', 
                                                  'BarCodeNum',  'LibCode', 'MobileNumber', 'CircCode', 'ReaderDays', 'SessionID', 
                                                  'Remark', 'TransactionOperator','TranType'))
  
  df_borr_tran$TranDateTime <- str_sub(str_trim(df_borr_tran$TranDateTime),end = -6)
  df_borr_tran$DueDate <- str_sub(str_trim(df_borr_tran$DueDate),end = -6)
  df_borr_tran$EntryDateTime <- str_sub(str_trim(df_borr_tran$EntryDateTime),end = -6)
  
  df_borr_tran$EntryDateTime <- ifelse(df_borr_tran$EntryDateTime == "", NA, df_borr_tran$EntryDateTime)
  df_borr_tran$DueDate <- ifelse(df_borr_tran$DueDate == "", NA, df_borr_tran$DueDate)
  df_borr_tran$TranDateTime <- ifelse(df_borr_tran$TranDateTime == "", NA, df_borr_tran$TranDateTime)
  
  df_circ_pre <- df_circ_pre %>% select(-one_of('BorrowerID','TranDateTime', 'TranType','AccNum','CatRefNum','EntryDateTime','FineAmount',
                                                'DelayDays','TotFineAmt','DueDate','RtnDate','BorrEndMemDate'))
  
  df_circ_pre <- df_circ_pre %>% select(-one_of('BorrMemEndDate'))
  
  df_samp_borr <- df_borr_tran
  
  df_samp_borr$Branch <- NA
  df_samp_borr$CategoryID <- NA
  
  branch_map <- list()
  
  branch_map[df_circ_pre$PatronName] <- df_circ_pre$BorrDepartmentName
  
  keys_to_replace <- df_samp_borr$LastName %in% names(branch_map)
  
  df_samp_borr$Branch[keys_to_replace] <- unlist(branch_map[df_samp_borr$LastName[keys_to_replace]])
  
  category_map <- list()
  
  category_map[df_circ_pre$PatronName] <- df_circ_pre$CategoryID
  
  keys_to_replace <- df_samp_borr$LastName %in% names(category_map)
  
  df_samp_borr$CategoryID[keys_to_replace] <- unlist(category_map[df_samp_borr$LastName[keys_to_replace]])
  
  df_samp_borr <- df_samp_borr[!(is.na(df_samp_borr$Branch) & is.na(df_samp_borr$CategoryID)), ]
  
  sapply(df_samp_borr, function(x) sum(is.na(x)))
  
  return(df_samp_borr)
}