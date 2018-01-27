library(shiny)
library(ggplot2)
library(ggthemes)
library(cowplot); theme_set(theme_gray())
library(wesanderson)
cpop <- read.csv("./Data/council-population.csv")
sim_dfr <- read.csv("./Data/sim_dfr.csv")


ui = fluidPage(theme = "flatly.css",
               titlePanel("How do you compare?"),
               sidebarLayout(
                             sidebarPanel(
                                          helpText("Welcome to How do you compare"),
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
                                                    tabPanel("Obesity & Diabetes", 
                                                             plotOutput("testplot")),
                                                    tabPanel("Mental Health", 
                                                             plotOutput("testplot2")),
                                                    tabPanel("Cancer", 
                                                             tableOutput("testtable"))
                                                    )
                                        )
                              )
               )

server = function(input, output){
    sim_dfr$is_user_sample <- rep(c(TRUE, FALSE), c(1, 999))
    sim_sub <- sim_dfr[1:50,]
    pal <- wes_palette("Moonrise3")



    observeEvent(input$go, {
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

                     output$testplot = renderPlot({
                         qplot(1)
                     })

                     output$testplot2 = renderPlot({
                         qplot(1:6)
                     })

                     output$testtable = renderTable({
                         iris
                     })
               })
}

shinyApp(ui, server)
