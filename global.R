library(shiny)
library(tidyverse)
library(metafor)
library(shinythemes)



## Defining functions


#Function for plot
results <- tibble(Study = 0)
visualise <- function(data) {
  if (ncol(results) == 1 | nrow(results) < 1) {stop("Nothing to see here! press 'Start Fresh'")} else {
    ggplot(data, aes(x = as.factor(Study), y = as.numeric(P))) +
      geom_point(size = 5, color = "#303d4e", shape = 16) +
      geom_hline(yintercept = 0.05, color = "#5589c4", linetype = "dashed", size = 1.5) +
      labs(x = "Study", y = "p-value", title = "Visualisation") +
      theme_bw() +
      theme(axis.title = element_text(size = 20, color = "#303d4e"),
            title = element_text(size = 25, face = "bold", color = "#303d4e"),
            axis.text.x = element_text(size = 15, color = "#98a4a5"),
            axis.text.y = element_text(size = 15, color = "#98a4a5"),
            axis.line = element_line(color = "#303d4e", size = 1))
    
  }
}


#makes a table that adds a study to the results
resultdat <- function() {
  if (ncol(results) == 1 | nrow(results) < 1) {} else {
    tempresults <- tibble(ES = study.es, 
                          P = study.p, 
                          n = study.n, 
                          var = study.var, 
                          Study = study.number)
    results <<- results %>% bind_rows(tempresults)
    #prints results in study and p-value columns
    results %>%
      select(Study, P, n) %>%
      mutate(P = ifelse(P > 0.001,
                        ifelse(P < 0.05, paste0(round(P, 3), "*"),
                               round(P, 3)),
                        "< 0.001*" ),
             n = as.character(n*2),
             Study = as.character(Study)) %>%
      rename("P-Value" = P, "N" = n)
  }
}



#cohen's d formula (from Lakens 2013)
cohens_d <- function(t, n1, n2 = n1){
  t*sqrt(1/n1 + 1/n2)
}



#simulating a single study (2 samples analysed using independent t-test)
sim_func <- function(n, d = 0) {
  dat <- tibble(
    grp = rep(LETTERS[1:2], each = n),
    score = c(rnorm(n, d, 1), rnorm(n, 0, 1)
    ))
  myt <- t.test(score ~ grp, dat)
  p <- myt$p.value
  es <- cohens_d(myt$statistic[[1]], n, n)
  study.p <<- p
  study.es <<- es
  study.n <<- n
  study.var <<- var_d(study.es, study.n)
  study.number <<- max(results$Study + 1)
}


#variance formula from Vosgerau et al. (2019)
var_d <- function(d,n){
  df <- (2*n-2)
  (2/n+(d^2)/(2*df)) * ((2*n)/(df))
}
