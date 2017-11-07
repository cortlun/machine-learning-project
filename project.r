setwd("C:/Users/43365/Documents/school/Fall2017/machineLearning/project")

library(glmnet)

zscore <- function(column){
    return((column - mean(column))/sd(column))
} # Create user defined zscores function to ease zscore calculation

#Cleans a numeric column by removing the comma then filling in blank values with the median. 
transform_column_to_numeric <- function(column){
    new <- as.numeric(gsub(",", "", column))
    med <- median(new[which(is.numeric(new))])
	new[is.na(new)] <- med
	new[!is.numeric(new)] <- med
	new[is.nan(new)] <- med
	new[is.infinite(new)] <- med
    return (new)
}

zscores <- function(dataframe) {
    new_df <- dataframe
    for (columnName in names(new_df)){
        new_df[, columnName] <- zscore(transform_column_to_numeric(new_df[, columnName]))
    }
    return(new_df)
}

data <- read.csv("data.csv", header = TRUE)
y <- data$PCT_OBESE_ADULTS13
data$PCT_OBESE_ADULTS13 <- NULL
data$PCT_OBESE_ADULTS08 <- NULL
labels <- data.frame(data$FIPS, data$State, data$County)
data$FIPS <- NULL
data$State <- NULL
data$County <- NULL
x <- data
x_z <- zscores(x)
lasso <- glmnet(as.matrix(x_z), y, alpha=1) # Create a lasso plot for the data