# distance from input to all samples

library(magrittr)

# import utils
source("utils.R", local = TRUE)


example_sample <- function() {
    dat <- read.csv("./Data/sim_dfr.csv")
    dat[, 1] <- NULL
    row_sample <- dat[sample(1:nrow(dat), 1), ]
    row_sample$age <- sample(20:60, 1)
    row_sample$bmi <- sample(20:30, 1)
    return(row_sample)
}


add_sample_to_existing <- function(sample, existing) {
    # add sample (user input) into existing data
    # user input should be a single-rowed dataframe
    # this needs to be done before scaling
    # though need to keep track of what is the user inputted
    # sample and the existing data
    sample$is_user_sample <- TRUE
    existing$is_user_sample <- FALSE
    combined_data <- rbind(existing, sample)
    transform_data(combined_data)
}


euclid_dist <- function(x, y){
    # Euclidean distance between two vectors
    as.numeric(dist(rbind(as.numeric(x), as.numeric(y))))
}


dist_from_user <- function(combined_data) {
    user <- combined_data[combined_data$is_user_sample == TRUE, ]
    user$is_user_sample <- NULL
    user_id <- combined_data$is_user_sample
    combined_data$is_user_sample <- NULL
    apply(combined_data, 1, function(x) euclid_dist(x, user))
}



main <- function(user_sample) {
    existing <- read_data()
    combined <- add_sample_to_existing(user_sample, existing)
    dists <- dist_from_user(combined)
    existing$is_user_sample <- FALSE
    user_sample$is_user_sample <- TRUE
    unscaled_combined <- rbind(existing, user_sample)
    unscaled_combined$dist_from_user <- dists
    # sort by dist
    unscaled_combined <- unscaled_combined[order(unscaled_combined$dist), ]
    return(unscaled_combined)
}

