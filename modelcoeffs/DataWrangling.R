# Load necessary libraries
library(dplyr)
library(purrr)
library(readxl)
library(readr)

# Specify the folder containing the CSV files

folder_path = "C:/Users/HLCSH/OneDrive/Documents/MSc 2023-2024/MSc Dissertation/MScDissertationCodeBase/modelcoeffs"

# Get a list of all CSV files in the folder

csv_files = list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames

data_list = map(csv_files, read.csv)

lapply(data_list, function(df) df[1, "Outcome"])

#data_list = data_list[-c(13:16)]

# Separate out reflected immune outcomes

# Define the column name to search within
column_name = "Reflected"

data_listR = Filter(function(df) df[1, column_name] == "TRUE", data_list)
lapply(data_listR, function(df) df[1, "Outcome"])

data_listNotR = data_list[-which(data_list %in% data_listR)]
lapply(data_listNotR, function(df) df[1, "Outcome"])

# Add new columns - estimate/se, exponentiated coefficient, bounds

data_listNotR = lapply(data_listNotR, function(df) {
  
  df$EstOverSE = df$Estimate/df$Std..Error
  df$ExpEstimate = exp(df$Estimate)
  df$ExpLower = exp(df$LowerCI)
  df$ExpUpper = exp(df$UpperCI)
  
  return(df)
})

filenames = lapply(seq_along(data_listNotR), function(i) {
  
  f = data_listNotR[[i]]$Outcome[1]
  
  return(f)
})

filenames = lapply(filenames, function(f) paste0(f, ".csv"))
filenames = unlist(filenames)
filenames = gsub("/", "", filenames)


write_excel_csv(data_listNotR[[1]], file = filenames[1])
write_excel_csv(data_listNotR[[2]], file = filenames[2])
write_excel_csv(data_listNotR[[3]], file = filenames[3])
write_excel_csv(data_listNotR[[4]], file = filenames[4])
write_excel_csv(data_listNotR[[5]], file = filenames[5])
write_excel_csv(data_listNotR[[6]], file = filenames[6])
write_excel_csv(data_listNotR[[7]], file = filenames[7])
write_excel_csv(data_listNotR[[8]], file = filenames[8])
write_excel_csv(data_listNotR[[9]], file = filenames[9])
write_excel_csv(data_listNotR[[10]], file = filenames[10])
write_excel_csv(data_listNotR[[11]], file = filenames[11])
write_excel_csv(data_listNotR[[12]], file = filenames[12])
write_excel_csv(data_listNotR[[13]], file = filenames[13])
write_excel_csv(data_listNotR[[14]], file = filenames[14])
write_excel_csv(data_listNotR[[15]], file = filenames[15])
write_excel_csv(data_listNotR[[16]], file = filenames[16])
write_excel_csv(data_listNotR[[17]], file = filenames[17])
write_excel_csv(data_listNotR[[18]], file = filenames[18])
write_excel_csv(data_listNotR[[19]], file = filenames[19])
write_excel_csv(data_listNotR[[20]], file = filenames[20])
write_excel_csv(data_listNotR[[21]], file = filenames[21])
write_excel_csv(data_listNotR[[22]], file = filenames[22])
write_excel_csv(data_listNotR[[23]], file = filenames[23])
write_excel_csv(data_listNotR[[24]], file = filenames[24])
write_excel_csv(data_listNotR[[25]], file = filenames[25])
write_excel_csv(data_listNotR[[26]], file = filenames[26])
write_excel_csv(data_listNotR[[27]], file = filenames[27])
write_excel_csv(data_listNotR[[28]], file = filenames[28])
write_excel_csv(data_listNotR[[29]], file = filenames[29])
write_excel_csv(data_listNotR[[30]], file = filenames[30])
write_excel_csv(data_listNotR[[31]], file = filenames[31])
write_excel_csv(data_listNotR[[32]], file = filenames[32])
write_excel_csv(data_listNotR[[33]], file = filenames[33])
write_excel_csv(data_listNotR[[34]], file = filenames[34])
write_excel_csv(data_listNotR[[35]], file = filenames[35])
write_excel_csv(data_listNotR[[36]], file = filenames[36])
write_excel_csv(data_listNotR[[37]], file = filenames[37])
write_excel_csv(data_listNotR[[38]], file = filenames[38])
write_excel_csv(data_listNotR[[39]], file = filenames[39])
write_excel_csv(data_listNotR[[40]], file = filenames[40])
write_excel_csv(data_listNotR[[41]], file = filenames[41])
write_excel_csv(data_listNotR[[42]], file = filenames[42])
write_excel_csv(data_listNotR[[43]], file = filenames[43])
write_excel_csv(data_listNotR[[44]], file = filenames[44])
write_excel_csv(data_listNotR[[45]], file = filenames[45])
write_excel_csv(data_listNotR[[46]], file = filenames[46])
write_excel_csv(data_listNotR[[47]], file = filenames[47])

# Reflect. Add new columns - estimate/se, exponentiated coefficient, bounds

data_listR = lapply(data_listR, function(df) {
  
  df$Estimate = -1*df$Estimate
  df$EstOverSE = df$Estimate/df$Std..Error
  
  df$LowerCI = -1*df$LowerCI
  df$UpperCI = -1*df$UpperCI
  
  df$ExpEstimate = exp(df$Estimate)
  
  df$ExpLower = pmin(exp(df$LowerCI), exp(df$UpperCI)) 
  df$ExpUpper = pmax(exp(df$LowerCI), exp(df$UpperCI))
  
  return(df)
})

filenames = lapply(seq_along(data_listR), function(i) {
  
  f = data_listR[[i]]$Outcome[1]
  
  return(f)
})

filenames = lapply(filenames, function(f) paste0(f, ".csv"))
filenames = unlist(filenames)
filenames = gsub("/", "", filenames)


write_excel_csv(data_listR[[1]], file = filenames[1])
write_excel_csv(data_listR[[2]], file = filenames[2])
write_excel_csv(data_listR[[3]], file = filenames[3])
write_excel_csv(data_listR[[4]], file = filenames[4])
write_excel_csv(data_listR[[5]], file = filenames[5])

# Combine all modified data frames into one

combined_data = bind_rows(c(data_listNotR, data_listR))

# Drop unneccessary columns for analysis

combined_data = combined_data[, c("Outcome", "Arm", "Group", "Coefficient", "Estimate", "Std..Error", "Pr...z..")]
combined_data$EstOverSE = combined_data$Estimate/combined_data$Std..Error

# Extract data

write.csv(combined_data, "combined_data.csv")
