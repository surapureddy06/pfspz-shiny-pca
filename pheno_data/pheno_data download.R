library(shiny)
library(dplyr)
library(DT)
library(here)
library(Biobase)

# ---------- Load anonymized data ----------
file_path <- here("data", "phenodata_clean.rds")
df <- readRDS(file_path)

# ---------- If RDS contains ExpressionSet ----------
if (inherits(df, "ExpressionSet") || inherits(df, "SeqExpressionSet")) {
  df <- pData(df)
}

# ---------- Safety checks ----------
stopifnot(all(c(
  "Cohort.Number",
  "Sex",
  "Outcome",
  "antibody_response",
  "timepoint_wrtVAX"
) %in% names(df)))

# ---------- Keep only required timepoints ----------
allowed_timepoints <- c(
  "VAX3+ 2 days",
  "VAX3+ 7 days",
  "VAX1+2 days",
  "VAX2+ 2 days",
  "VAX1",
  "CHMI+2 days"
)

# ---------- Clean ----------
df <- df %>%
  dplyr::mutate(
    Cohort.Number = as.character(Cohort.Number),
    Sex = as.character(Sex),
    Outcome = as.character(Outcome),
    antibody_response = as.character(antibody_response),
    timepoint_wrtVAX = as.character(timepoint_wrtVAX)
  ) %>%
  dplyr::mutate(
    Sex_clean = dplyr::case_when(
      Sex %in% c("M", "Male", "male") ~ "Male",
      Sex %in% c("F", "Female", "female") ~ "Female",
      TRUE ~ Sex
    ),
    
    Outcome_clean = dplyr::case_when(
      grepl("not|non|infected", Outcome, ignore.case = TRUE) ~ "Non-Protected",
      grepl("protect|never", Outcome, ignore.case = TRUE) ~ "Protected",
      TRUE ~ Outcome
    ),
    
    Ab_Group = dplyr::case_when(
      grepl("high", antibody_response, ignore.case = TRUE) ~ "High",
      grepl("low", antibody_response, ignore.case = TRUE) ~ "Low",
      TRUE ~ NA_character_
    ),
    
    Timepoint_clean = dplyr::case_when(
      timepoint_wrtVAX %in% allowed_timepoints ~ timepoint_wrtVAX,
      TRUE ~ NA_character_
    )
  )

# ---------- UI ----------
ui <- fluidPage(
  tags$style(HTML("
    body { background-color: #f5f5f5; }
    .well { background-color: #ffffff; border-radius: 10px; }
    .dataTables_wrapper { font-size: 14px; }
  ")),
  
  titlePanel("Human PfSPZ CVac Trial - Pheno Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Apply Filters"),
      
      checkboxGroupInput(
        "cohort_filter",
        "Choose Cohort:",
        choices = sort(unique(df$Cohort.Number)),
        selected = sort(unique(df$Cohort.Number))
      ),
      
      checkboxGroupInput(
        "sex_filter",
        "Choose Sex:",
        choices = c("Male", "Female"),
        selected = character(0)
      ),
      
      checkboxGroupInput(
        "outcome_filter",
        "Choose Outcome:",
        choices = c("Protected", "Non-Protected"),
        selected = character(0)
      ),
      
      checkboxGroupInput(
        "ab_filter",
        "Choose Antibody Response:",
        choices = c("High", "Low"),
        selected = character(0)
      ),
      
      checkboxGroupInput(
        "timepoint_filter",
        "Choose Timepoint:",
        choices = allowed_timepoints,
        selected = character(0)
      ),
      
      radioButtons(
        "file_type",
        "File type:",
        choices = c("csv", "tsv"),
        selected = "csv"
      ),
      
      downloadButton("download_data", "Download filtered data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Pheno Data",
          br(),
          verbatimTextOutput("nrows"),
          DTOutput("pheno_table")
        )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    dat <- df
    
    if (length(input$cohort_filter) > 0) {
      dat <- dat %>% dplyr::filter(Cohort.Number %in% input$cohort_filter)
    }
    
    if (length(input$sex_filter) > 0) {
      dat <- dat %>% dplyr::filter(Sex_clean %in% input$sex_filter)
    }
    
    if (length(input$outcome_filter) > 0) {
      dat <- dat %>% dplyr::filter(Outcome_clean %in% input$outcome_filter)
    }
    
    if (length(input$ab_filter) > 0) {
      dat <- dat %>% dplyr::filter(Ab_Group %in% input$ab_filter)
    }
    
    if (length(input$timepoint_filter) > 0) {
      dat <- dat %>% dplyr::filter(Timepoint_clean %in% input$timepoint_filter)
    }
    
    dat <- dat %>% dplyr::select(-dplyr::any_of("filenames"))
    
    dat
  })
  
  output$nrows <- renderText({
    paste("Rows after filtering:", nrow(filtered_data()))
  })
  
  output$pheno_table <- renderDT({
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("filtered_pheno_data.", input$file_type)
    },
    content = function(file) {
      dat <- filtered_data()
      
      if (input$file_type == "csv") {
        write.csv(dat, file, row.names = FALSE)
      } else {
        write.table(dat, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    }
  )
}

shinyApp(ui = ui, server = server)