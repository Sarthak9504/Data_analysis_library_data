

circ_preprocessing <- function(df) {
  library(stringr)
  library(dplyr)
  df_circ <- df

  df_circ$BorrDepartmentName <- ifelse(df_circ$BorrDepartmentName == "", NA, df_circ$BorrDepartmentName)
  
  df_category <- read.csv("C:/Users/sarth/OneDrive/Documents/Library Data/Category.csv")
  
  category_map <- list()
  
  category_map[df_category$CategoryID] <- df_category$CategoryName
  
  keys_to_replace <- df_circ$CategoryID %in% names(category_map)
  
  df_circ$CategoryID[keys_to_replace] <- unlist(category_map[df_circ$CategoryID[keys_to_replace]])
  
  df_trantype <- read.csv("C:/Users/sarth/OneDrive/Documents/Library Data/TranType.csv")
  
  tran_map <- list()
  
  tran_map[df_trantype$TranTypeCD] <- df_trantype$TranTypeDesc
  
  keys_to_replace <- df_circ$TranType %in% names(tran_map)
  
  df_circ$TranType[keys_to_replace] <- unlist(tran_map[df_circ$TranType[keys_to_replace]])
  
  df_circ <- df_circ %>% select(-one_of('ErrorTranCD', 'ReaderDays', 'ChangeData', 'GnrlCounter',
                                        'EntryDate', 'EntryTime','TranTime',
                                        'TranDate','TransactionID','RctNumber', 
                                        'RctDate', 'TransactionOperator', 'DomainName', 'MachineName', 
                                        'BarCodeNum', 'LibCode','TransferNumber','DepartmentName', 'TransferedToLib',
                                        'Remark','SessionID','WorkPlace','OrganizationName')) 
  
  branch_map <- list()
  
  df_map <- read.csv("C:/Users/sarth/OneDrive/Documents/Library Data/bak.csv")
  
  branch_map[df_map$Unique] <- df_map$Branch
  
  keys_to_replace <- df_circ$BorrDepartmentName %in% names(branch_map)
  
  df_circ$BorrDepartmentName[keys_to_replace] <- unlist(branch_map[df_circ$BorrDepartmentName[keys_to_replace]])
  
  df_circ$EntryDateTime <- str_sub(str_trim(df_circ$EntryDateTime), end = -6)
  df_circ$DueDate <- str_sub(str_trim(df_circ$DueDate), end = -6)
  df_circ$RtnDate <- str_sub(str_trim(df_circ$RtnDate), end = -6)
  df_circ$BorrMemEndDate <- str_sub(str_trim(df_circ$BorrMemEndDate), end = -6)
  df_circ$TranDateTime <- str_sub(str_trim(df_circ$TranDateTime), end = -6)
  
  df_circ$EntryDateTime <- ifelse(df_circ$EntryDateTime == "", NA, df_circ$EntryDateTime)
  df_circ$DueDate <- ifelse(df_circ$DueDate == "", NA, df_circ$DueDate)
  df_circ$RtnDate <- ifelse(df_circ$RtnDate == "", NA, df_circ$RtnDate)
  df_circ$BorrMemEndDate <- ifelse(df_circ$BorrMemEndDate == "", NA, df_circ$BorrMemEndDate)
  df_circ$TranDateTime <- ifelse(df_circ$TranDateTime == "", NA, df_circ$TranDateTime)
  
  rm(df_map)
  rm(df_trantype)
  rm(tran_map)
  rm(branch_map)
  rm(keys_to_replace)
  rm(category_map)
  rm(df_category)
  
  return(df_circ)
}