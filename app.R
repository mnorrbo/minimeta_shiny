source("global.R")

#define UI for shiny app
ui <- fluidPage(
  
  theme = "styles.css",
  
  # Application title
  headerPanel(title = "Hack your own internal meta-analysis!",
              windowTitle = "InternalMeta"),
  
  # Sidebar with a slider input for sample size and for effect size
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Sample size per group (same for both groups):",
                  min = 10,
                  max = 200,
                  value = 100),
      sliderInput("d",
                  "True Effect Size:",
                  min = 0,
                  max = 1,
                  value = 0.3),
      actionButton("start", "Start Fresh"),
      actionButton("newstudy", "Run another study!"),
      textInput("hackstudy", "Which studies would you like to put in the file-drawer? (Separate studies with spaces)", placeholder = "e.g. '1 2 3'"),
      actionButton("remove", "Remove studies"),
      actionButton("meta", "Conduct meta-analysis")
    ),
    # defining where plots and results are on page
    mainPanel(
      plotOutput("plot"),
      tableOutput("results"),
      textOutput("metadescribe")
      
      
    )
  )
)
# Define server logic
server <- function(input, output) {
  #pressing the "Start Fresh" button
  observeEvent({input$start | input$d}, {
    isolate(sim_func(input$n, input$d))
    output$results <- renderTable({
      isolate(sim_func(input$n, input$d))
      results <<- tibble(ES= study.es, P = study.p, n = study.n, var = study.var)  %>% 
        mutate(Study = 1:nrow(.))
      results %>%
        select(Study, P, n) %>%
        mutate(P = ifelse(P > 0.001,
                          ifelse(P < 0.05, paste0(round(P, 3), "*"), round(P, 3)),
                          "< 0.001*" ),
               n = as.character(n*2),
               Study = as.character(Study)) %>%
        rename("P-Value" = P, "N" = n)
    }, digits = 3)
    output$plot <- renderPlot({
      visualise(data = results)
    })
    output$metadescribe <- renderText({paste("")})
  })
  #pressing the "Add a new study" button
  observeEvent(input$newstudy, {
    isolate(sim_func(input$n, input$d))
    output$results <- renderTable({
      resultdat()}, digits = 3)
    output$plot <- renderPlot({
      visualise(data = results)
    })
    output$metadescribe <- renderText({paste("")})
  })
  #Pressing the "Meta-analyse!" button
  observeEvent(input$meta, {
    minimeta <- rma(results$ES, results$var, method = "HE")
    output$metadescribe <- renderText({paste0("
                       You included ", nrow(results), " studies in your internal meta-analysis, which gave your report an overall p-value of ", ifelse(minimeta$pval > 0.001, round(minimeta$pval, 3), "< 0.001" ), ".")})
    output$plot <- renderPlot({
      visualise(data = rbind(results, c("ES", minimeta$pval, "n", "var", "Meta")))
    })
  })
  observeEvent(input$remove, {
    studylist <- unlist(strsplit(input$hackstudy, " "))
    if (sum(studylist %in% results$Study) == length(studylist)) {
      results <<- filter(results, !(Study %in% studylist))
      output$results <- renderTable({
        results %>%
          select(Study, P, n) %>%
          mutate(P = ifelse(P > 0.001,
                            ifelse(P < 0.05, paste0(round(P, 3), "*"), round(P, 3)),
                            "< 0.001*" ),
                 n = as.character(n*2),
                 Study = as.character(Study)) %>%
          rename("P-Value" = P, "N" = n)}, digits = 3)
      output$plot <- renderPlot({
        visualise(data = results)
      })
    } else {}
  })
}

# Define server logic
server <- function(input, output) {
  #pressing the "Start Fresh" button
  observeEvent({input$start | input$d}, {
    isolate(sim_func(input$n, input$d))
    output$results <- renderTable({
      isolate(sim_func(input$n, input$d))
      results <<- tibble(ES= study.es, P = study.p, n = study.n, var = study.var)  %>% 
        mutate(Study = 1:nrow(.))
      results %>%
        select(Study, P, n) %>%
        mutate(P = ifelse(P > 0.001,
                          ifelse(P < 0.05, paste0(round(P, 3), "*"), round(P, 3)),
                          "< 0.001*" ),
               n = as.character(n*2),
               Study = as.character(Study)) %>%
        rename("P-Value" = P, "N" = n)
    }, digits = 3)
    output$plot <- renderPlot({
      visualise(data = results)
    })
    output$metadescribe <- renderText({paste("")})
  })
  #pressing the "Add a new study" button
  observeEvent(input$newstudy, {
    isolate(sim_func(input$n, input$d))
    output$results <- renderTable({
      resultdat()}, digits = 3)
    output$plot <- renderPlot({
      visualise(data = results)
    })
    output$metadescribe <- renderText({paste("")})
  })
  #Pressing the "Meta-analyse!" button
  observeEvent(input$meta, {
    minimeta <- rma(results$ES, results$var, method = "HE")
    output$metadescribe <- renderText({paste0("
                       You included ", nrow(results), " studies in your internal meta-analysis, which gave your report an overall p-value of ", ifelse(minimeta$pval > 0.001, round(minimeta$pval, 3), "< 0.001" ), ".")})
    output$plot <- renderPlot({
      visualise(data = rbind(results, c("ES", minimeta$pval, "n", "var", "Meta")))
    })
  })
  observeEvent(input$remove, {
    studylist <- unlist(strsplit(input$hackstudy, " "))
    if (sum(studylist %in% results$Study) == length(studylist)) {
      results <<- filter(results, !(Study %in% studylist))
      output$results <- renderTable({t(
        results %>%
          select(Study, P, n) %>%
          mutate(P = ifelse(P > 0.001,
                            ifelse(P < 0.05, paste0(round(P, 3), "*"), round(P, 3)),
                            "< 0.001*" ),
                 n = as.character(n*2),
                 Study = as.character(Study)) %>%
          rename("P-Value" = P, "N" = n))},
        digits = 3)
      output$plot <- renderPlot({
        visualise(data = results)
      })
    } 
  })
}


# Run the application
shinyApp(ui = ui, server = server)