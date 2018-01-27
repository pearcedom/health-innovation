library(caret)
library(magrittr)

read_data <- function(data_path="../output/sim_dfr.csv") {
    data <- read.csv(data_path)
    # ignore first column as Dom doesn't know about row.names=FALSE
    data[, 1] <- NULL
    return(data)
}


make_area_encoder <- function() {
    dataframe <- read_data()
    formula <- sprintf("~ area")
    encoder <- caret::dummyVars(formula, data=dataframe, fullRank=TRUE)
    return(encoder)
}


encoder <- make_area_encoder()


encode_area_col <- function(dataframe, area_col="area") {
    # one-hot encode area columns
    formula <- sprintf("~ %s", area_col)
    encoder <- caret::dummyVars(formula, data=dataframe, fullRank=TRUE)
    # return area columns
    data.frame(predict(encoder, dataframe))
}


replace_area_columns <- function(dataframe, new_cols) {
    data["area"] <- NULL
    cbind(dataframe, new_cols)
}


encode_area <- function(dataframe, area_col="area") {
    new_area_cols <- encode_area_col(dataframe, area_col)
    dataframe[, "area"] <- NULL
    cbind(dataframe, new_area_cols)
}


activity_to_num <- function(dataframe, activity_col="activity") {
    # convert activity to numeric
    #re-order factor levels
    activity <- dataframe[, activity_col]
    # wtf
    activity <- factor(activity, levels(activity)[c(3, 1, 2)])
    dataframe[, activity_col] <- as.numeric(activity)
    return(dataframe)
}


binarise_gender <- function(data, gender_col="gender") {
    data[, gender_col] <- as.integer(data[, gender_col])
    return(data)
}


binarise_bools <- function(data, bool_cols=NULL) {
    # convert boolean columns to integers
    if (is.null(bool_cols)) {
        # find boolean_cols
        bool_cols <- which(sapply(data, is.logical))
    }
    data[, bool_cols] <- apply(data[, bool_cols], 2, as.integer)
    return(data)
}


z_score <- function(x) {
    return((x - mean(x)) / sd(x))
}


scale_features <- function(dataframe, feature_cols=NULL) {
    #standardise features to mean of zero and unit variance
    if (is.null(feature_cols)) {
        feature_cols <- colnames(dataframe)
    }
    dataframe[, feature_cols] <- apply(dataframe[, feature_cols], 2, z_score)
    return(dataframe)
}


transform_data <- function(data) {
    # remove is_user_sample column from dataset
    # as don't want to transform that
    sample_col <- data$is_user_sample
    data$is_user_sampe <- NULL
    # transform data
    data %>%
    encode_area() %>%
    binarise_gender() %>%
    binarise_bools() %>%
    activity_to_num() %>%
    scale_features() -> transformed_data
    # add is_user_sample column back in after transformations
    transformed_data$is_user_sample <- sample_col
    return(transformed_data)
}


pca <- function(data, feature_cols=NULL) {
    # calculate principal components
    # returns dataframe of only principal component columns
    if (is.null(feature_cols)) {
        # all columns are feature_cols
        feature_cols <- colnames(data)
    }
    pca_dat <- prcomp(data[, feature_cols])$x
    new_colnames <- sapply(1:length(feature_cols), function(x) {
                           sprintf("PC%d", x)})
    colnames(pca_dat) <- new_colnames
    return(pca_dat)
}

