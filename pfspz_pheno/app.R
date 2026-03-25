#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(here)
file_path <- here("data", "U01_VTEU0042_pheno_ANON.xlsx")
df <- read_excel(file_path)
head(df)
unique(df$Cohort)
df <- df %>%
  mutate(
    Sex = as.factor(Sex),
    Cohort = as.factor(Cohort),
    Outcome = as.factor(Outcome),
    `CSP Ab response` = as.factor(`CSP Ab response`)
  )
df <- df %>%
  mutate(
    Ab_Group = case_when(
      grepl("high", `CSP Ab response`, ignore.case = TRUE) ~ "High",
      grepl("low", `CSP Ab response`, ignore.case = TRUE) ~ "Low",
      TRUE ~ NA_character_
    )
  )
head(df)
ui <- fluidPage(
  
  titlePanel("PfSPZ Trial Cohort Analysis"),
  
  tabsetPanel(
    
    tabPanel("Cohort_1",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 radioButtons(
                   "analysis_c1",
                   "Choose analysis:",
                   choices = c(
                     "Male vs Female" = "sex",
                     "Protected vs Non Protected" = "outcome",
                     "High vs Low Antibody Response" = "antibody"
                   )
                 )
                 
               ),
               
               mainPanel(
                 plotOutput("plot_c1")
               )
               
             )
    ),
    
    tabPanel("Cohort_3",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 radioButtons(
                   "analysis_c3",
                   "Choose analysis:",
                   choices = c(
                     "Male vs Female" = "sex",
                     "Protected vs Non Protected" = "outcome",
                     "High vs Low Antibody Response" = "antibody"
                   )
                 )
                 
               ),
               
               mainPanel(
                 plotOutput("plot_c3")
               )
               
             )
    )
    
  )
)
