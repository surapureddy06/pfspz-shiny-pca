library(shiny)
library(dplyr)
library(ggplot2)
library(here)
library(Biobase)

# ---------- Load anonymized data from RDS ----------
file_path <- here("data", "phenodata_clean.rds")
df <- readRDS(file_path)

# ---------- If RDS is an ExpressionSet, extract pData ----------
if (inherits(df, "ExpressionSet") || inherits(df, "SeqExpressionSet")) {
  df <- pData(df)
}

# ---------- Safety check ----------
stopifnot(all(c("Subject.ID", "Cohort.Number", "Sex", "Outcome", "antibody_response") %in% names(df)))

# ---------- Clean data ----------
df <- df %>%
  dplyr::mutate(
    Subject.ID = as.character(Subject.ID),
    Cohort.Number = as.character(Cohort.Number),
    Sex = as.factor(Sex),
    Outcome = as.factor(Outcome),
    antibody_response = as.factor(antibody_response),
    Ab_Group = dplyr::case_when(
      grepl("high", as.character(antibody_response), ignore.case = TRUE) ~ "High",
      grepl("low",  as.character(antibody_response), ignore.case = TRUE) ~ "Low",
      TRUE ~ NA_character_
    ),
    Ab_Group = factor(Ab_Group, levels = c("High", "Low"))
  )

# ---------- Donut function using UNIQUE subjects ----------
make_donut <- function(data, var, title_text) {
  
  data_unique <- data %>%
    dplyr::distinct(Subject.ID, .keep_all = TRUE)
  
  total_n <- dplyr::n_distinct(data_unique$Subject.ID)
  
  pdat <- data_unique %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    dplyr::count(.data[[var]], name = "n") %>%
    dplyr::mutate(
      pct = n / sum(n),
      label = paste0(.data[[var]], "\n", n),
      ymax = cumsum(pct),
      ymin = dplyr::lag(ymax, default = 0),
      label_pos = (ymax + ymin) / 2
    )
  
  ggplot(pdat) +
    geom_rect(
      aes(
        ymin = ymin,
        ymax = ymax,
        xmin = 2,
        xmax = 3,
        fill = .data[[var]]
      ),
      color = "white",
      linewidth = 0.9
    ) +
    coord_polar(theta = "y") +
    xlim(0, 3.35) +
    geom_text(
      aes(x = 3.15, y = label_pos, label = label),
      size = 5
    ) +
    labs(
      title = paste0(title_text, " (n = ", total_n, ")"),
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", size = 18),
      legend.position = "none"
    )
}

# ---------- UI ----------
ui <- fluidPage(
  tags$style(HTML("
    body { background-color: #ffffff; }
    .well { background-color: #ffffff; border-radius: 12px; }
  ")),
  
  titlePanel("PfSPZ Trial Cohort Analysis"),
  
  tabsetPanel(
    
    tabPanel(
      "Cohort_1",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "analysis_c1",
            "Choose analysis:",
            choices = c(
              "Male vs Female" = "sex",
              "Protected vs Non Protected" = "outcome",
              "High vs Low Antibody Response" = "antibody"
            ),
            selected = "sex"
          )
        ),
        mainPanel(
          plotOutput("plot_c1", height = "520px")
        )
      )
    ),
    
    tabPanel(
      "Cohort_3",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "analysis_c3",
            "Choose analysis:",
            choices = c(
              "Male vs Female" = "sex",
              "Protected vs Non Protected" = "outcome",
              "High vs Low Antibody Response" = "antibody"
            ),
            selected = "sex"
          )
        ),
        mainPanel(
          plotOutput("plot_c3", height = "520px")
        )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  output$plot_c1 <- renderPlot({
    
    d <- df %>%
      dplyr::filter(Cohort.Number == "Cohort_1")
    
    if (input$analysis_c1 == "sex") {
      make_donut(d, "Sex", "Cohort_1: Male vs Female")
    } else if (input$analysis_c1 == "outcome") {
      make_donut(d, "Outcome", "Cohort_1: Protected vs Non Protected")
    } else if (input$analysis_c1 == "antibody") {
      make_donut(d, "Ab_Group", "Cohort_1: High vs Low Antibody Response")
    }
  })
  
  output$plot_c3 <- renderPlot({
    
    d <- df %>%
      dplyr::filter(Cohort.Number == "Cohort_3")
    
    if (input$analysis_c3 == "sex") {
      make_donut(d, "Sex", "Cohort_3: Male vs Female")
    } else if (input$analysis_c3 == "outcome") {
      make_donut(d, "Outcome", "Cohort_3: Protected vs Non Protected")
    } else if (input$analysis_c3 == "antibody") {
      make_donut(d, "Ab_Group", "Cohort_3: High vs Low Antibody Response")
    }
  })
}

shinyApp(ui = ui, server = server)
