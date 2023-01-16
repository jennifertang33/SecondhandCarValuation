#################################################################
########## Script for Data Cleaning and Model Training ##########
#################################################################

library(randomForest)

# import all csv 
all_csv <- list.files('car_data')

# create empty data frame
data1 <- data.frame()

# read all csv, create brand column, concatenate all csv together
for ( i in all_csv) {
  df <- read.csv(paste('car_data',i,sep='/'))
  brandname <- strsplit(i, split=".csv")
  df$brand <- brandname %>% unlist()
  data1 <- rbind(data1, df)
}

# check missing values
colSums(is.na(data1))

# consider data after year 2000
data2 <- data1[(data1$year>=2000),]

# remove non-sense data (e.g. engineSize=0)
data3 <- data2[(data2$engineSize!=0),]

# create detect outlier function
detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25)
  Quantile3 <- quantile(x, probs=.75)
  IQR = Quantile3-Quantile1
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

# create remove outlier function
remove_outlier <- function(dataframe, columns=names(dataframe)) {
  for (col in columns) {
    result <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  return (result)
}

# apply remove outlier function to continuous features only
data4 <- remove_outlier(data3, c("mileage", "engineSize", "tax", "mpg"))

# export csv for data visualization
#write.csv(data4, "cleaned_data.csv", row.names=FALSE)

# Modeling Training
rf_start_time <- Sys.time()
rf_model <- randomForest(price ~ ., 
                         data=data4, 
                         mtry=3,
                         importance=TRUE,
                         na.action=na.omit)
rf_end_time <- Sys.time()

# Time used for training
rf_end_time - rf_start_time  #remarks: it took me ~40 mins for training

# Print Regression Model
print(rf_model)

# Save the Model
#saveRDS(rf_model, "rf_model.rds")





