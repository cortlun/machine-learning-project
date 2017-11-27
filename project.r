setwd("C:/Users/43365/Documents/school/Fall2017/machineLearning/project")

library("glmnet")
library("plotmo")
library("DMwR")
library("e1071")
library("ROCR")
library("caret")
library("nnet")

zscore <- function(column){
    if (is.factor(column)) {
        return(as.factor(column))
    } 
    else {
        return((column - mean(column))/sd(column))
    }
} # Create user defined zscores function to ease zscore calculation

#Cleans a numeric column by removing the comma.  By default it will perform imputation using the column median, good for categorical variables. 
transform_column_to_numeric <- function(column){
    new <- as.numeric(gsub(",", "", column))
    med <- median(new, na.rm=TRUE)
	new[which(is.na(new))] <- med
	new[which(!is.numeric(new))] <- med
	new[which(is.nan(new))] <- med
	new[which(is.infinite(new))] <- med
    if (is.factor(column)) {
        return(as.factor(new))
    } 
    else {
        return (new)
    }
}

zscores <- function(dataframe) {
    new_df <- dataframe
    for (columnName in names(new_df)){
        new_df[, columnName] <- zscore(transform_column_to_numeric(new_df[, columnName]))
    }
    return(new_df)
}


######## Standardize dataset, establish y var and x vars
data <- read.csv("data.csv", header = TRUE)
y <- data$PCT_OBESE_ADULTS13


####### Find attributes closest to 2013.  Take only attributes expressed as a percentage or per capita figure(where applicable).  Remove codependent attributes (percent diabetes).
data$X2010.Census.Population <- NULL
data$Population.Estimate.2011 <- NULL
data$Population.Estimate.2012 <- NULL
data$Population.Estimate.2014 <- NULL
data$Population.Estimate.2015 <- NULL
data$Population.Estimate.2016 <- NULL
data$LACCESS_POP10 <- NULL
data$LACCESS_POP15 <- NULL
data$PCH_LACCESS_POP_10_15 <- NULL
data$PCT_LACCESS_POP10 <- NULL
data$LACCESS_LOWI10 <- NULL
data$LACCESS_LOWI15 <- NULL
data$PCH_LACCESS_LOWI_10_15 <- NULL
data$PCT_LACCESS_LOWI10 <- NULL
data$LACCESS_HHNV10 <- NULL
data$LACCESS_HHNV15 <- NULL
data$PCH_LACCESS_HHNV_10_15 <- NULL
data$PCT_LACCESS_HHNV10 <- NULL
data$LACCESS_SNAP15 <- NULL
data$LACCESS_CHILD10 <- NULL
data$LACCESS_CHILD15 <- NULL
data$LACCESS_CHILD_10_15 <- NULL
data$PCT_LACCESS_CHILD10 <- NULL
data$LACCESS_SENIORS10 <- NULL
data$LACCESS_SENIORS15 <- NULL
data$PCH_LACCESS_SENIORS_10_15 <- NULL
data$PCT_LACCESS_SENIORS10 <- NULL
data$LACCESS_WHITE15 <- NULL
data$LACCESS_BLACK15 <- NULL
data$LACCESS_HISP15 <- NULL
data$LACCESS_NHASIAN15 <- NULL
data$LACCESS_NHNA15 <- NULL
data$LACCESS_NHPI15 <- NULL
data$LACCESS_MULTIR15 <- NULL
data$FFR09 <- NULL
data$FFR14 <- NULL
data$FFRPTH09 <- NULL
data$PCH_FFRPTH_09_14 <- NULL
data$FSR09 <- NULL
data$FSR14 <- NULL
data$PCH_FSR_09_14 <- NULL
data$FSRPTH09 <- NULL
data$PCH_FSRPTH_09_14 <- NULL
data$PC_FFRSALES07 <- NULL
data$PC_FSRSALES07 <- NULL
data$GROC09 <- NULL
data$PCH_GROC_09_14 <- NULL
data$GROCPTH09 <- NULL
data$PCH_GROCPTH_09_14 <- NULL
data$SUPERC09 <- NULL
data$PCH_SUPERC_09_14 <- NULL
data$SUPERCPTH09 <- NULL
data$PCH_SUPERCPTH_09_14 <- NULL
data$CONVS09 <- NULL
data$PCH_CONVS_09_14 <- NULL
data$CONVSPTH09 <- NULL
data$PCH_CONVSPTH_09_14 <- NULL
data$SPECS09 <- NULL
data$PCH_SPECS_09_14 <- NULL
data$SPECSPTH09 <- NULL
data$PCH_SPECSPTH_09_14 <- NULL
data$SNAPS16 <- NULL
data$PCH_SNAPS_12_16 <- NULL
data$SNAPSPTH16 <- NULL
data$PCH_SNAPSPTH_12_16 <- NULL
data$WICS08 <- NULL
data$PCH_WICS_08_12 <- NULL
data$WICSPTH08 <- NULL
data$PCH_WICSPTH_08_12 <- NULL
data$REDEMP_SNAPS16 <- NULL
data$PCH_REDEMP_SNAPS_12_16 <- NULL
data$PCT_SNAP16 <- NULL
data$PCH_SNAP_12_16 <- NULL
data$PC_SNAPBEN10 <- NULL
data$PCH_PC_SNAPBEN_10_15 <- NULL
data$SNAP_PART_RATE08 <- NULL
data$SNAP_OAPP09 <- NULL
data$SNAP_CAP09 <- NULL
data$SNAP_BBCE09 <- NULL
data$SNAP_REPORTSIMPLE09 <- NULL
data$PCT_NSLP09 <- NULL
data$PCH_NSLP_09_15 <- NULL
data$PCT_FREE_LUNCH09 <- NULL
data$PCT_REDUCED_LUNCH09 <- NULL
data$PCT_SBP09 <- NULL
data$PCH_SBP_09_15 <- NULL
data$PCT_SFSP09 <- NULL
data$PCH_SFSP_09_15 <- NULL
data$PC_WIC_REDEMP08 <- NULL
data$PCH_PC_WIC_REDEMP_08_12 <- NULL
data$REDEMP_WICS08 <- NULL
data$PCH_REDEMP_WICS_08_12 <- NULL
data$PCT_WIC09 <- NULL
data$PCH_WIC_09_15 <- NULL
data$PCT_CACFP09 <- NULL
data$PCH_CACFP_09_15 <- NULL
data$FOODINSEC_10_12 <- NULL
data$FOODINSEC_13_15 <- NULL
data$CH_FOODINSEC_12_15 <- NULL
data$VLFOODSEC_10_12 <- NULL
data$VLFOODSEC_13_15 <- NULL
data$CH_VLFOODSEC_12_15 <- NULL
data$FOODINSEC_CHILD_01_07 <- NULL
data$FOODINSEC_CHILD_03_11 <- NULL
data$MILK_PRICE10 <- NULL
data$SODA_PRICE10 <- NULL
data$MILK_SODA_PRICE10 <- NULL
data$DIRSALES_FARMS07 <- NULL
data$DIRSALES_FARMS12 <- NULL
data$PCH_DIRSALES_FARMS_07_12 <- NULL
data$PCT_LOCLFARM07 <- NULL
data$PCT_LOCLSALE07 <- NULL
data$DIRSALES07 <- NULL
data$DIRSALES12 <- NULL
data$PCH_DIRSALES_07_12 <- NULL
data$PC_DIRSALES07 <- NULL
data$PCH_PC_DIRSALES_07_12 <- NULL
data$FMRKT09 <- NULL
data$PCH_FMRKT_09_16 <- NULL
data$FMRKTPTH09 <- NULL
data$PCH_FMRKTPTH_09_16 <- NULL
data$FMRKT_SNAP16 <- NULL
data$FMRKT_WIC16 <- NULL
data$FMRKT_WICCASH16 <- NULL
data$FMRKT_SFMNP16 <- NULL
data$FMRKT_CREDIT16 <- NULL
data$FMRKT_FRVEG16 <- NULL
data$FMRKT_ANMLPROD16 <- NULL
data$FMRKT_BAKED16 <- NULL
data$FMRKT_OTHERFOOD16 <- NULL
data$VEG_FARMS07 <- NULL
data$PCH_VEG_FARMS_07_12 <- NULL
data$VEG_ACRES07 <- NULL
data$PCH_VEG_ACRES_07_12 <- NULL
data$VEG_ACRESPTH07 <- NULL
data$PCH_VEG_ACRESPTH_07_12 <- NULL
data$FRESHVEG_FARMS07 <- NULL
data$PCH_FRESHVEG_FARMS_07_12 <- NULL
data$FRESHVEG_ACRES07 <- NULL
data$PCH_FRESHVEG_ACRES_07_12 <- NULL
data$FRESHVEG_ACRESPTH07 <- NULL
data$PCH_FRESHVEG_ACRESPTH_07_12 <- NULL
data$ORCHARD_FARMS07 <- NULL
data$PCH_ORCHARD_FARMS_07_12 <- NULL
data$ORCHARD_ACRES07 <- NULL
data$PCH_ORCHARD_ACRES_07_12 <- NULL
data$ORCHARD_ACRESPTH07 <- NULL
data$PCH_ORCHARD_ACRESPTH_07_12 <- NULL
data$BERRY_FARMS07 <- NULL
data$PCH_BERRY_FARMS_07_12 <- NULL
data$BERRY_ACRES07 <- NULL
data$PCH_BERRY_ACRES_07_12 <- NULL
data$BERRY_ACRESPTH07 <- NULL
data$PCH_BERRY_ACRESPTH_07_12 <- NULL
data$SLHOUSE07 <- NULL
data$PCH_SLHOUSE_07_12 <- NULL
data$GHVEG_FARMS07 <- NULL
data$PCH_GHVEG_FARMS_07_12 <- NULL
data$GHVEG_SQFT07 <- NULL
data$PCH_GHVEG_SQFT_07_12 <- NULL
data$GHVEG_SQFTPTH07 <- NULL
data$PCH_GHVEG_SQFTPTH_07_12 <- NULL
data$CSA07 <- NULL
data$PCH_CSA_07_12 <- NULL
data$AGRITRSM_OPS07 <- NULL
data$PCH_AGRITRSM_OPS_07_12 <- NULL
data$AGRITRSM_RCT07 <- NULL
data$PCH_AGRITRSM_RCT_07_12 <- NULL
data$FARM_TO_SCHOOL09 <- NULL
data$PCT_DIABETES_ADULTS08 <- NULL
data$PCT_DIABETES_ADULTS13 <- NULL
data$RECFAC09 <- NULL
data$PCH_RECFAC_09_14 <- NULL
data$RECFACPTH09 <- NULL
data$PCH_RECFACPTH_09_14 <- NULL
data$PCT_NHWHITE10 <- NULL
data$PCT_NHBLACK10 <- NULL
data$PCT_HISP10 <- NULL
data$PCT_NHASIAN10 <- NULL
data$PCT_NHNA10 <- NULL
data$PCT_NHPI10 <- NULL
data$PCT_65OLDER10 <- NULL
data$PCT_18YOUNGER10 <- NULL
data$PERPOV10 <- NULL
data$PERCHLDPOV10 <- NULL
data$POPLOSS10 <- NULL
data$SNAP_REPORTSIMPLE16 <- 
data$PCH_FFR_09_14 <- NULL
data$GROC14 <- NULL
data$SUPERC14 <- NULL
data$CONVS14 <- NULL
data$SPECS14 <- NULL
data$SNAPS12 <- NULL
data$WICS12 <- NULL
data$FMRKT16 <- NULL
data$VEG_FARMS12 <- NULL
data$VEG_ACRES12 <- NULL
data$FRESHVEG_ACRES12 <- NULL
data$ORCHARD_FARMS12 <- NULL
data$ORCHARD_ACRES12 <- NULL
data$BERRY_FARMS12 <- NULL
data$BERRY_ACRES12 <- NULL
data$GHVEG_SQFT12 <- NULL
data$RECFAC14 <- NULL

data$PCT_OBESE_ADULTS13 <- NULL
data$PCT_OBESE_ADULTS08 <- NULL

labels <- data.frame(data$FIPS, data$State, data$County)
data$FIPS <- NULL
data$State <- NULL
data$County <- NULL


# Factors
data$SNAP_CAP16 <- as.factor(data$SNAP_CAP16)
data$SNAP_BBCE16 <- as.factor(data$SNAP_BBCE16)
data$FARM_TO_SCHOOL13 <- as.factor(transform_column_to_numeric(data$FARM_TO_SCHOOL13))
data$Population.Estimate.2013 <- as.numeric(gsub(",", "", data$Population.Estimate.2013))

x <- data


# Perform imputation using column median
#x_z <- zscores(x)

# Perform imputation using nearest neighbors method (based on euclidian distance, uses square root of the number of observations for k)
x_knn = knnImputation(x, k=round(sqrt(nrow(x))))
x_z = zscores(x_knn)
x_z$SNAP_CAP16 <- as.numeric(x_z$SNAP_CAP16)
x_z$SNAP_BBCE16 <- as.numeric(x_z$SNAP_BBCE16)
x_z$FARM_TO_SCHOOL13 <- as.numeric(x_z$FARM_TO_SCHOOL13)

write.csv(x, file = "predictorData.csv")

write.csv(x_z, file = "predictorsNormalized.csv")


####### Linear model to check for outliers and identify significant predictors (using coefficients/p-values for significance and cook's distance for outliers)
x_z <- x_z
x_z$SNAP_CAP16 <- as.numeric(x_z$SNAP_CAP16)
x_z$SNAP_BBCE16 <- as.numeric(x_z$SNAP_BBCE16)
x_z$FARM_TO_SCHOOL13 <- as.numeric(x_z$FARM_TO_SCHOOL13)
linear_model <- lm(y ~ as.matrix(x_z))
summary(linear_model)
layout(matrix(c(1,2,3,4), 2, 2))
plot(linear_model)
cooks_distance <- cooks.distance(linear_model)
layout(matrix(c(1), 1, 1))
plot(cooks_distance, pch="*", cex=2, main="Cook's Distance")
abline(h = 4*mean(cooks_distance, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooks_distance)+1, y=cooks_distance, labels=ifelse(cooks_distance>4*mean(cooks_distance, na.rm=T),names(cooks_distance),""), col="red", pos=4) #labels

####### Remove outliers based on Cook's Distance
x_z <- x_z[-c(298,1013,1226),]
y <- y[-c(298,1013,1226)]

####### Lasso to determine significant predictors
layout(matrix(c(1), 1, 1))
lasso <- glmnet(as.matrix(x_z), y, alpha=1) # Create a lasso plot for the data
plot_glmnet(lasso)
cvmodel <- cv.glmnet(as.matrix(x_z), y, alpha=1, nfold=10) # Run CV model using 10-fold cross validation
coef(lasso, s=cvmodel$lambda.1se)
plot(cvmodel)
#Lambda chosen: 0.0.07562909
#Percent variability explained: 56%

####### Remove insignificant predictors
x_z$PCT_LACCESS_POP15 <- NULL
x_z$PCT_LACCESS_LOWI15 <- NULL
x_z$PCT_LACCESS_SNAP15 <- NULL
x_z$PCT_LACCESS_CHILD15 <- NULL
x_z$PCT_LACCESS_WHITE15 <- NULL
x_z$PCT_LACCESS_NHNA15 <- NULL
x_z$PCT_LACCESS_MULTIR15 <- NULL
x_z$SPECSPTH14 <- NULL
x_z$WICSPTH12 <- NULL
x_z$REDEMP_SNAPS12 <- NULL
x_z$PCT_REDUCED_LUNCH14 <- NULL
x_z$PCT_SBP15 <- NULL
x_z$REDEMP_WICS12 <- NULL
x_z$FDPIR12 <- NULL
x_z$SODATAX_VENDM14 <- NULL
x_z$CHIPSTAX_STORES14 <- NULL
x_z$FOOD_TAX14 <- NULL
x_z$PCT_LOCLSALE12 <- NULL
x_z$FMRKTPTH16 <- NULL
x_z$PCT_FMRKT_WIC16 <- NULL
x_z$PCT_FMRKT_WICCASH16 <- NULL
x_z$PCT_FMRKT_FRVEG16 <- NULL
x_z$PCT_FMRKT_BAKED16 <- NULL
x_z$PCT_FMRKT_OTHERFOOD16 <- NULL
x_z$VEG_ACRESPTH12 <- NULL
x_z$FRESHVEG_FARMS12 <- NULL
x_z$FRESHVEG_ACRESPTH12 <- NULL
x_z$BERRY_ACRESPTH12 <- NULL
x_z$SLHOUSE12 <- NULL
x_z$GHVEG_FARMS12 <- NULL
x_z$GHVEG_SQFTPTH12 <- NULL
x_z$FARM_TO_SCHOOL13 <- NULL
x_z$PCT_HSPA15 <- NULL
x_z$POVRATE15 <- NULL
x_z$CHILDPOVRATE15 <- NULL
x_z$METRO13 <- NULL

####### Set y-pred to predict whether a county is in the third quartile of obesity:
y_pred <- ifelse(y >= quantile(y,.75), 1, 0)
set.seed(320489239)
size <- floor(0.667 * nrow(x_z))
train_ind <- sample(seq_len(nrow(x_z)), size = size)
x_train <- x_z[train_ind,]
x_test <- x_z[-train_ind,]
y_train <- y_pred[train_ind]
y_test <- y_pred[-train_ind]

####### SVM model - tune
#cv <- tune(svm, x_train, as.factor(y_train), cross=10, probability=TRUE, kernel="radial") #Perform 10-fold cross validation on the data set
svm_model <- cv$best.model #Get the model selected by tune
summary (svm_model) #Summary information for the SVM model
summary(cv) #Summary information for the cross validation
svm_val <- predict(svm_model, x_test) #Using the model, predict cell dna on validation data for fold eight
svm_predicted <- as.numeric(levels(svm_val))[svm_val] #Convert predicted values to a numeric vector
svm_confusionMatrix <- confusionMatrix(svm_predicted, y_test, mode="prec_recall", positive="1") #Check precision, accuracy, and recall for the eighth fold
svm_pred <- prediction(svm_predicted, y_test) #Build a prediction object using predicted classification values and actual class
svm_perf <- performance(svm_pred, measure="tpr", x.measure="fpr") #Build a performance object measuring true positive rate and false positive rate
plot(svm_perf) #Plot the ROC curve

####### Neural net
nnet_data <- x_train
nnet_data$y <- y_train
nn <- nnet(as.factor(y) ~ ., data=nnet_data, size=3)
summary (nn) #Summary information for the SVM model
predicted <- predict(nn, x_test, type="class")
confusionMatrix(as.factor(predicted), y_test, mode="prec_recall", positive="1") #Check precision, accuracy, and recall for the eighth fold
pred <- prediction(as.numeric(predicted), y_test) #Build a prediction object using predicted classification values and actual class
perf <- performance(pred, measure="tpr", x.measure="fpr") #Build a performance object measuring true positive rate and false positive rate
plot(perf) #Plot the ROC curve
