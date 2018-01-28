library(shiny)
library(ggplot2)
library(ggthemes)
library(cowplot); theme_set(theme_gray())
library(wesanderson)
cpop <- read.csv("./Data/council-population.csv")
sim_dfr <- read.csv("./Data/sim_dfr.csv")

#trump_sample <- data.frame(X = NA, age = 71, gender = "MALE", area = "Aberdeen City", 
#                          activity = "low", binge = FALSE, stroke = FALSE, 
#                          diabetes = FALSE, cancer = FALSE, bmi = 30)
#user_sample <- trump_sample
#user_sample$is_user_sample <- TRUE

#trump_neighbours <- do.call(rbind, lapply(1:49, function(x) trump_sample))
#trump_neighbours$stroke <- sample(c(TRUE, FALSE), 49, replace = TRUE, prob = c(0.6, 0.4))
#trump_neighbours$diabetes <- sample(c(TRUE, FALSE), 49, replace = TRUE, prob = c(0.6, 0.4))
#trump_neighbours$cancer <- sample(c(TRUE, FALSE), 49, replace = TRUE, prob = c(0.6, 0.4))
#sim_dfr <- rbind(sim_dfr, trump_neighbours)

timeTo <- function(age, disease){
    if(disease){
        age - sample(1:age, 1)
    } else {
        NA
    }
}

likelihoodOf <- function(disease, sim_sub, user_sample){
    overall_risk <- sum(sim_sub[[disease]]) / nrow(sim_sub) * 100 
    t_disease <- paste0("t_", disease)
    year_risk <- sapply(c(5, 10), function(years){
                            plus_year <- user_sample$age + years
                            sum(sim_sub[[t_disease]] <= plus_year, 
                                na.rm = TRUE) / nrow(sim_sub) * 100 
                        })
    c(overall_risk, year_risk)
}


ui = fluidPage(theme = "flatly.css",
               titlePanel("How do I compare?"),
               sidebarLayout(
                             sidebarPanel(
                                          helpText("Welcome to How do you compare!"),
                                          helpText("Fill in your details and health history to the 
                                                   best of your ability."),
                                          textInput(inputId = "age", 
                                                    label = "Age", 
                                                    value = "",
                                                    placeholder = "e.g. 45"),
                                          checkboxGroupInput(inputId = "sex", 
                                                             label = "Sex", 
                                                             choices = c("MALE", "FEMALE")),

                                          selectInput(inputId = "area",
                                                      label = "Place of Birth",
                                                      choice = cpop$local_council_area ),
                                          sliderInput(inputId = "bmi",
                                                      label = "BMI", 
                                                      min = 19,
                                                      max = 35,
                                                      value = 28),
                                          checkboxGroupInput(inputId = "activity", 
                                                             label = "How active are you?", 
                                                             choices = c(Low = "low", 
                                                                         Medium = "mid", 
                                                                         High = "high")),

                                          checkboxGroupInput(inputId = "binge", 
                                                             label = "Have you binge drunk in the 
                                                             last 7 days?", 
                                                             choices = c(TRUE, FALSE)),
                                          actionButton("go", "Compare me!")
                                          ),

                              mainPanel(
                                        tabsetPanel(
                                                    tabPanel("How do you compare?", 
                                                             plotOutput("p_overall")),
                                                    tabPanel("Diabetes", 
                                                             plotOutput("p_diabetes")),
                                                    tabPanel("Stroke", 
                                                             plotOutput("p_stroke")),
                                                    tabPanel("Cancer", 
                                                             plotOutput("p_cancer"))
                                                    )
                                        )
                              )
               )

server = function(input, output){
    pal <- wes_palette("Moonrise3")
    pal2 <- wes_palette("Zissou") 
    source("./Data/predictor.R", local = TRUE)
    source("./Data/utils.R", local = TRUE)

    observeEvent(input$go, {
                     user_sample <- data.frame(X = NA, age = as.numeric(input$age), 
                                               gender = as.character(input$sex), 
                                               area = as.character(input$area), 
                                               activity = as.character(input$activity), 
                                               binge = as.logical(input$binge), 
                                               stroke = FALSE, 
                                               diabetes = FALSE, 
                                               cancer = FALSE, 
                                               bmi = as.numeric(input$bmi))
                     sim_dfr <- main(user_sample)
                     sim_dfr$t_stroke <- sapply(1:nrow(sim_dfr), function(x){
                                                    timeTo(sim_dfr$age[x],
                                                           sim_dfr$stroke[x])
                                               })
                     sim_dfr$t_diabetes <- sapply(1:nrow(sim_dfr), function(x){
                                                    timeTo(sim_dfr$age[x],
                                                           sim_dfr$diabetes[x])
                                               })
                     sim_dfr$t_cancer <- sapply(1:nrow(sim_dfr), function(x){
                                                    timeTo(sim_dfr$age[x],
                                                           sim_dfr$cancer[x])
                                               })

                     sim_sub <- sim_dfr[1:(nrow(sim_dfr)/20),]
                     #sim_sub$stroke <- c(FALSE, 
                     #                    sample(c(TRUE, FALSE), 
                     #                           49, 
                     #                           replace = TRUE, 
                     #                           prob = c(0.8, 0.2)))
                     #sim_sub$diabetes <- c(FALSE, 
                     #                      sample(c(TRUE, FALSE), 
                     #                             49, 
                     #                             replace = TRUE, 
                     #                             prob = c(0.7, 0.3)))
                     #sim_sub$cancer <- c(FALSE, 
                     #                    sample(c(TRUE, FALSE), 
                     #                           49, 
                     #                           replace = TRUE, 
                     #                           prob = c(0.8, 0.2)))

                     output$p_overall = renderPlot({
                         p1 <- ggplot(sim_dfr, aes(x = age)) + 
                             geom_histogram(fill = pal[1], alpha = 0.7) + 
                             geom_vline(xintercept = sim_dfr$age[sim_dfr$is_user_sample], 
                                        linetype = 3) + 
                             labs(title = "How old are you?", 
                                  x = "Age", 
                                  y = "Number of People") + 
                             theme_pander()
                         p2 <- ggplot(sim_dfr, aes(x = bmi)) + 
                             geom_histogram(fill = pal[2], alpha = 0.7) + 
                             geom_vline(xintercept = sim_dfr$bmi[sim_dfr$is_user_sample], 
                                        linetype = 3) + 
                             labs(title = "What's your BMI?", 
                                  x = "BMI", 
                                  y = "Number of People") + 
                             theme_pander()
                         p3 <- ggplot(sim_dfr, aes(x = activity, fill = activity)) + 
                             geom_bar(stat = 'count') + 
                             labs(title = "How active are others?", 
                                  x = "Reported Activity", 
                                  y = "Number of People") + 
                             scale_fill_manual(values = pal) + 
                             theme_pander() + 
                             theme(legend.position = 'none')
                         p4 <- ggplot(sim_dfr, aes(x = binge, fill = binge)) + 
                             geom_bar(stat = 'count') + 
                             labs(title = "How much do others drink?", 
                                  x = "Reported Binge Drinking", 
                                  y = "Number of People") + 
                             scale_fill_manual(values = pal) + 
                             theme_pander() + 
                             theme(legend.position = 'none')
                         plot_grid(p1, p2, NULL, NULL, p3, p4, nrow = 3, scale = 0.9,
                                   rel_heights = c(1, 0.2, 1))
                     })


                     output$p_diabetes = renderPlot({
                         #likelihood_vec <- c(40, 12, 24)
                         likelihood_vec <- likelihoodOf("diabetes", sim_sub, user_sample)
                         likelihood_dfr <- data.frame(age = factor(c("Overall", 5, 10),
                                                            levels = c(5, 10, "Overall")),
                                               l = likelihood_vec)
                         ggplot(likelihood_dfr, aes(x = age, y = l, fill = age)) + 
                             geom_bar(stat = 'identity') + 
                             ylim(0, 100) + 
                             labs(title = "Five-year, Ten-year and Lifetime Diabetes Risk",
                                  x = "Years",
                                  y = "% Risk") + 
                             scale_fill_manual(values = pal2) + 
                             theme_pander() +
                             theme(legend.position = 'none')
                     })

                     output$p_stroke = renderPlot({
                         #likelihood_vec <- c(50, 17, 40)
                         likelihood_vec <- likelihoodOf("stroke", sim_sub, user_sample)
                         likelihood_dfr <- data.frame(age = factor(c("Overall", 5, 10),
                                                            levels = c(5, 10, "Overall")),
                                               l = likelihood_vec)
                         ggplot(likelihood_dfr, aes(x = age, y = l, fill = age)) + 
                             geom_bar(stat = 'identity') + 
                             ylim(0, 100) + 
                             labs(title = "Five-year, Ten-year and Lifetime Stroke Risk",
                                  x = "Years",
                                  y = "% Risk") + 
                             scale_fill_manual(values = pal2) + 
                             theme_pander() +
                             theme(legend.position = 'none')
                     })

                     output$p_cancer = renderPlot({
                         #likelihood_vec <- c(16, 8, 15)
                         likelihood_vec <- likelihoodOf("cancer", sim_sub, user_sample)
                         likelihood_dfr <- data.frame(age = factor(c("Overall", 5, 10),
                                                            levels = c(5, 10, "Overall")),
                                               l = likelihood_vec)
                         ggplot(likelihood_dfr, aes(x = age, y = l, fill = age)) + 
                             geom_bar(stat = 'identity') + 
                             ylim(0, 100) + 
                             labs(title = "Five-year, Ten-year and Lifetime Cancer Risk",
                                  x = "Years",
                                  y = "% Risk") + 
                             scale_fill_manual(values = pal2) + 
                             theme_pander() +
                             theme(legend.position = 'none')
                     })
               })
}

shinyApp(ui, server)
