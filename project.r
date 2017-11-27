setwd("C:/Users/43365/Documents/school/Fall2017/machineLearning/project")

library(glmnet)
library(plotmo)
library(DMwR)

zscore <- function(column){
    return((column - mean(column))/sd(column))
} # Create user defined zscores function to ease zscore calculation

#Cleans a numeric column by removing the comma.  By default it will perform imputation using the column median. 
transform_column_to_numeric <- function(column){
    new <- as.numeric(gsub(",", "", column))
    med <- median(new, na.rm=TRUE)
	new[which(is.na(new))] <- med
	new[which(!is.numeric(new))] <- med
	new[which(is.nan(new))] <- med
	new[which(is.infinite(new))] <- med
    return (new)
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
y_pred <- ifelse(y >= quantile(y,.75), 1, 0)


####### Find attributes closest to 2013.  Take only attributes expressed as a percentage (where applicable).  Remove codependent attributes (percent diabetes).
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
data$SNAP_REPORTSIMPLE16 <- NULL


data$PCT_OBESE_ADULTS13 <- NULL
data$PCT_OBESE_ADULTS08 <- NULL

labels <- data.frame(data$FIPS, data$State, data$County)
data$FIPS <- NULL
data$State <- NULL
data$County <- NULL

x <- data

# Perform imputation using column median
#x_z <- zscores(x)

# Perform imputation using nearest neighbors method (based on euclidian distance, uses square root of the number of observations for k)
x_knn = knnImputation(x, k=round(sqrt(nrow(x))))
x_z = zscores(x_knn)

#write.csv(x_z, file = "independentVarsNormalized.csv")


####### Linear model to check for outliers and identify significant predictors (using coefficients/p-values for significance and cook's distance for outliers)
linear_model <- lm(y ~ as.matrix(x_z))
summary(linear_model)
layout(matrix(c(1,2,3,4), 2, 2))
plot(linear_model)
cooks_distance <- cooks.distance(linear_model)
layout(matrix(c(1), 1, 1))
plot(cooks_distance, pch="*", cex=2, main="Cook's Distance")
abline(h = 4*mean(cooks_distance, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooks_distance)+1, y=cooks_distance, labels=ifelse(cooks_distance>4*mean(cooks_distance, na.rm=T),names(cooks_distance),""), col="red", pos=4) #labels

#Remove outliers based on Cook's Distance
x_z[c(298, 1226, 2507),] <- NULL

####### Lasso to determine significant predictors
layout(matrix(c(1), 1, 1))
lasso <- glmnet(as.matrix(x_z_knn), y, alpha=1) # Create a lasso plot for the data
plot_glmnet(lasso)
