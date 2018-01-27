
predict_cancer <- function(age, gender) {
    # return probability of cancer for a given age and gender
    ages   <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    risk_f <- c(0, 0, 0.01, 0.02, 0.03, 0.04, 0.065, 0.08, 0.1, 0.11, 0.15)
    risk_m <- c(0, 0, 0, 0.02, 0.02, 0.05, 0.08, 0.11, 0.18, 0.23, 0.35)
    data   <- data.frame("age" = ages, "risk_f" = risk_f, "risk_m" = risk_m)
    if (startsWith(tolower(gender),  "m")) {
        data <- data[, c("age", "risk_m")]
    } else {
        dat <- data[, c("age", "risk_f")]
    }
    colnames(data)[2] <- "risk"
    model <- loess(risk ~ age, data=data)
    prob <- predict(model, age)
    sample(c(TRUE, FALSE), 1, prob = c(prob, 1 - prob))

}


predict_diabetes <- function(age, gender) {
    # create data
    ages <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    risks <- c(0.25, 1.18, 2.06, 3.53, 9.5, 19.09, 27, 24.55, 13.4, 12.2, 10)
    data <- data.frame("age" = ages, "risk" = risks)
    # returns probability of diabetes for a given age and gender
    if (startsWith(tolower(gender),  "f")) {
        # women 10% less likely at all ages compared to men
        data$risk <- data$risk * 0.9
    }
    colnames(data)[2] <- "risk"
    model <- loess(risk ~ age, data=data)
    prob <- predict(model, age)
    sample(c(TRUE, FALSE), 1, prob = c(prob, 100 - prob))
}


predict_stroke <- function(age, gender) {
    # given an age, return the probability of having a stroke
    ages <-  c(0, 10, 20, 40, 50, 75, 80, 100)
    risk <- c(0.001, 0.01, 0.01, 0.02, 0.03, 0.05, 0.1, 0.1)
    data <- data.frame("age" = ages, "risk" = risk)
    if (startsWith(tolower(gender), "f")) {
        # women 4% higher incidence of stroke
        data$risk <- data$risk * 1.04
    }
    model <- loess(risk ~ age, data=data)
    prob <- predict(model, age)
    sample(c(TRUE, FALSE), 1, prob = c(prob, 1 - prob))
}


predict_bmi <- function(age, gender) {
    # Generate typical BMI for a given age and gender
    BMI_SD <- 1.8
    ages <- c(5, 7, 8, 12, 13, 18, 20, 40, 60, 80, 100)
    bmi_f <- c(15, 16.5, 18.3, 21.3, 22.9, 24.1, 27.1, 28.6, 28, 27.2, 27.2)  
    bmi_m <- c(15, 16.5, 17.9, 21.3, 21.9, 26.1, 26.5, 27.5, 29.5, 26.4, 26.2)
    data <- data.frame("age" = ages, "bmi_m" = bmi_m, "bmi_f" = bmi_m)
    if (startsWith(tolower(gender), "m")) {
        data <- data[, c("age", "bmi_m")]
    } else {
        data <- data[, c("age", "bmi_f")]
    }
    colnames(data)[2] <- "bmi"
    model <- loess(bmi ~ age, data=data)
    predicted <- predict(model, age)
    rnorm(1, mean=predicted, sd=BMI_SD)
}

