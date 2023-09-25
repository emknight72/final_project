library(readr)
library(utils)
library(tidyverse)
library(readxl)
setwd("/Users/emilyknight/Documents/final_project")

#### load in e-cigarette ASV data from zip file ####
zip_file <- "../final_project/RSV_ecig/RSV_ecig.zip"
unzip(zip_file, list = TRUE) # lists all the CSVs in the zip file

# Get a list of all CSV files in the extracted directory
csv_files <- list.files("../final_project/RSV_ecig", pattern = "*.csv", full.names = TRUE)

# Extract the files from the zip archive
unzip(zip_file, exdir = "../final_project/RSV_ecig")

# Get a list of all CSV files in the extracted directory
csv_files <- list.files("../final_project/RSV_ecig", pattern = "*.csv", full.names = TRUE)

# Create an empty list to store the data frames
data_frames <- list()

# Read each CSV file and store it in the list
for (file in csv_files) {
  df_name <- gsub(".csv$", "", basename(file))  # Extract the file name without extension
  data_frames[[df_name]] <- read.csv(file)  # Save the data frame with the extracted file name
}

# for all, delete first row
for (df_name in names(data_frames)) {
  data_frames[[df_name]] <- data_frames[[df_name]] %>% slice(1:n())
}

# for all, make the first row the col headers
for (df_name in names(data_frames)) {
  data_frames[[df_name]] <- data_frames[[df_name]] %>%
    setNames(.[1, ]) %>%
    dplyr::slice(-1)
}


# rename the second column to be the country name followed by _RSV
for (df_name in names(data_frames)) {
  new_col_name <- paste0(df_name, "_RSV")
  data_frames[[df_name]] <- data_frames[[df_name]] %>%
    rename(!!new_col_name := colnames(.)[2])
}

# make the row index a col instead
#for (df_name in names(data_frames)) {
  #data_frames[[df_name]] <- data_frames[[df_name]] %>%
   # rownames_to_column(var = "Time")
  #}

# rename the first column to be Time
for (df_name in names(data_frames)) {
  new_col_name <- paste0("Time")
  data_frames[[df_name]] <- data_frames[[df_name]] %>%
    rename(!!new_col_name := colnames(.)[1])
}

# merge all together
ecig_RSV_full <- data_frames[[1]]
for (i in 2:length(data_frames)) {
  ecig_RSV_full <- merge(ecig_RSV_full, data_frames[[i]], all = TRUE)
}

# make time col posix
ecig_RSV_full$Time <- as.POSIXct(paste0(ecig_RSV_full$Time, "-01"), format = "%Y-%m-%d")

# save data
write.csv(ecig_RSV_full, "/Users/emilyknight/Documents/final_project/RSV_data/ecig_RSV_full.csv", row.names = FALSE)





#### load in e-vaporizor ASV data from zip file ####
zip_file <- "../final_project/RSV_vap/RSV_vap.zip"
unzip(zip_file, list = TRUE) # lists all the CSVs in the zip file

# Get a list of all CSV files in the extracted directory
csv_files <- list.files("../final_project/RSV_vap", pattern = "*.csv", full.names = TRUE)

# Extract the files from the zip archive
unzip(zip_file, exdir = "../final_project/RSV_vap")

# Get a list of all CSV files in the extracted directory
csv_files <- list.files("../final_project/RSV_vap", pattern = "*.csv", full.names = TRUE)

# Create an empty list to store the data frames
data_frames <- list()

# Read each CSV file and store it in the list
for (file in csv_files) {
  df_name <- gsub(".csv$", "", basename(file))  # Extract the file name without extension
  data_frames[[df_name]] <- read.csv(file)  # Save the data frame with the extracted file name
}

# for all, delete first row
for (df_name in names(data_frames)) {
  data_frames[[df_name]] <- data_frames[[df_name]] %>% slice(1:n())
}

# for all, make the first row the col headers
for (df_name in names(data_frames)) {
  data_frames[[df_name]] <- data_frames[[df_name]] %>%
    setNames(.[1, ]) %>%
    slice(-1)
}

# rename the second column to be the country name followed by _RSV
for (df_name in names(data_frames)) {
  new_col_name <- paste0(df_name, "_RSV")
  data_frames[[df_name]] <- data_frames[[df_name]] %>%
    rename(!!new_col_name := colnames(.)[2])
}

# make the row index a col instead
#for (df_name in names(data_frames)) {
 # data_frames[[df_name]] <- data_frames[[df_name]] %>%
  #  rownames_to_column(var = "Time")
#}

# rename the first column to be Time
for (df_name in names(data_frames)) {
  new_col_name <- paste0("Time")
  data_frames[[df_name]] <- data_frames[[df_name]] %>%
    rename(!!new_col_name := colnames(.)[1])
}


# merge all together
vap_RSV_full <- data_frames[[1]]
for (i in 2:length(data_frames)) {
  vap_RSV_full <- merge(vap_RSV_full, data_frames[[i]], all = TRUE)
}

# make time col posix
vap_RSV_full$Time <- as.POSIXct(paste0(vap_RSV_full$Time, "-01"), format = "%Y-%m-%d")

# save data
write.csv(vap_RSV_full, "/Users/emilyknight/Documents/final_project/RSV_data/vap_RSV_full.csv", row.names = FALSE)




