library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(here)
library(viridis)

# -----------------------------
# Run updated R scripts
# These should create:
# pca_baseline_df
# pca_post_df
# percent_var_baseline (or percent_var)
# percent_var_post
# -----------------------------
source(here("data", "Baseline.R"))
source(here("data", "PostVax.R"))

# -----------------------------
# Standardize baseline columns
# -----------------------------
if (!"Antibody" %in% colnames(pca_baseline_df) && "antibody_response" %in% colnames(pca_baseline_df)) {
  pca_baseline_df$Antibody <- pca_baseline_df$antibody_response
}

if (!"SubjectID" %in% colnames(pca_baseline_df) && "Subject.ID" %in% colnames(pca_baseline_df)) {
  pca_baseline_df$SubjectID <- pca_baseline_df$Subject.ID
}

if (!"Timepoint" %in% colnames(pca_baseline_df)) {
  pca_baseline_df$Timepoint <- "VAX1"
}

if (!"ParasitemiaDays" %in% colnames(pca_baseline_df) && "tte.parasitemia.days" %in% colnames(pca_baseline_df)) {
  pca_baseline_df$ParasitemiaDays <- pca_baseline_df$tte.parasitemia.days
}

if ("ParasitemiaDays" %in% colnames(pca_baseline_df)) {
  pca_baseline_df$ParasitemiaDays <- as.numeric(pca_baseline_df$ParasitemiaDays)
  pca_baseline_df$ParasitemiaDays[is.na(pca_baseline_df$ParasitemiaDays)] <- 0
}

# -----------------------------
# Standardize postvax columns
# -----------------------------
if (!"Antibody" %in% colnames(pca_post_df) && "antibody_response" %in% colnames(pca_post_df)) {
  pca_post_df$Antibody <- pca_post_df$antibody_response
}

if (!"SubjectID" %in% colnames(pca_post_df) && "Subject.ID" %in% colnames(pca_post_df)) {
  pca_post_df$SubjectID <- pca_post_df$Subject.ID
}

if (!"Timepoint" %in% colnames(pca_post_df) && "timepoint_wrtVAX" %in% colnames(pca_post_df)) {
  pca_post_df$Timepoint <- pca_post_df$timepoint_wrtVAX
}

if (!"ParasitemiaDays" %in% colnames(pca_post_df) && "tte.parasitemia.days" %in% colnames(pca_post_df)) {
  pca_post_df$ParasitemiaDays <- pca_post_df$tte.parasitemia.days
}

if ("ParasitemiaDays" %in% colnames(pca_post_df)) {
  pca_post_df$ParasitemiaDays <- as.numeric(pca_post_df$ParasitemiaDays)
  pca_post_df$ParasitemiaDays[is.na(pca_post_df$ParasitemiaDays)] <- 0
}

# -----------------------------
# Clean labels to match colors
# -----------------------------
clean_pca_df <- function(df) {
  df %>%
    mutate(
      Sex = as.character(Sex),
      Outcome = as.character(Outcome),
      Antibody = as.character(Antibody),
      SubjectID = as.character(SubjectID),
      Timepoint = as.character(Timepoint)
    ) %>%
    mutate(
      Sex = case_when(
        Sex %in% c("M", "Male", "male") ~ "Male",
        Sex %in% c("F", "Female", "female") ~ "Female",
        TRUE ~ Sex
      ),
      Outcome = case_when(
        grepl("not|non|infect", Outcome, ignore.case = TRUE) ~ "Non-Protected",
        grepl("protect", Outcome, ignore.case = TRUE) ~ "Protected",
        TRUE ~ Outcome
      ),
      Antibody = case_when(
        grepl("high", Antibody, ignore.case = TRUE) ~ "High",
        grepl("low", Antibody, ignore.case = TRUE) ~ "Low",
        TRUE ~ Antibody
      )
    )
}

pca_baseline_df <- clean_pca_df(pca_baseline_df)
pca_post_df <- clean_pca_df(pca_post_df)

# -----------------------------
# Percent variance fallbacks
# -----------------------------
if (!exists("percent_var_baseline")) {
  if (exists("percent_var")) {
    percent_var_baseline <- percent_var
  } else {
    percent_var_baseline <- c(NA, NA)
  }
}

if (!exists("percent_var_post")) {
  percent_var_post <- c(NA, NA)
}

# -----------------------------
# Keep useful columns
# -----------------------------
needed_cols <- c(
  "PC1", "PC2",
  "Sex", "Outcome", "Antibody",
  "SubjectID", "Timepoint", "ParasitemiaDays"
)

pca_baseline_df <- pca_baseline_df[, intersect(colnames(pca_baseline_df), needed_cols), drop = FALSE]
pca_post_df     <- pca_post_df[, intersect(colnames(pca_post_df), needed_cols), drop = FALSE]

# -----------------------------
# Store datasets and variance
# -----------------------------
pca_list <- list(
  baseline = pca_baseline_df,
  postvax  = pca_post_df
)

percent_var_list <- list(
  baseline = percent_var_baseline,
  postvax  = percent_var_post
)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("PCA Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "analysis_type",
        label = "Select analysis type:",
        choices = c("baseline", "postvax"),
        selected = "baseline"
      ),
      
      selectInput(
        inputId = "color_var",
        label = "Select variable:",
        choices = c("Sex", "Outcome", "Antibody", "SubjectID", "ParasitemiaDays", "Timepoint"),
        selected = "Sex"
      ),
      
      br(),
      strong("Hovered / clicked point details"),
      verbatimTextOutput("clicked_info")
    ),
    
    mainPanel(
      plotlyOutput("pca_plot", height = "700px")
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  selected_data <- reactive({
    req(input$analysis_type)
    pca_list[[input$analysis_type]]
  })
  
  selected_percent_var <- reactive({
    req(input$analysis_type)
    percent_var_list[[input$analysis_type]]
  })
  
  output$pca_plot <- renderPlotly({
    df <- selected_data()
    percent_var <- selected_percent_var()
    
    validate(
      need("PC1" %in% colnames(df), "PC1 column is missing."),
      need("PC2" %in% colnames(df), "PC2 column is missing."),
      need(input$color_var %in% colnames(df), paste(input$color_var, "column is missing."))
    )
    
    df <- df %>%
      mutate(
        hover_text = paste0(
          "SubjectID: ", SubjectID,
          "<br>Timepoint: ", Timepoint,
          "<br>Sex: ", Sex,
          "<br>PC1: ", round(PC1, 2),
          "<br>PC2: ", round(PC2, 2)
        )
      )
    
    x_lab <- if (!is.na(percent_var[1])) {
      paste0("PC1 (", round(percent_var[1], 2), "%)")
    } else {
      "PC1"
    }
    
    y_lab <- if (!is.na(percent_var[2])) {
      paste0("PC2 (", round(percent_var[2], 2), "%)")
    } else {
      "PC2"
    }
    
    p <- ggplot(
      df,
      aes(
        x = PC1,
        y = PC2,
        color = .data[[input$color_var]],
        text = hover_text,
        key = SubjectID
      )
    ) +
      geom_point(size = 5, alpha = 0.9) +
      labs(
        title = paste("PCA Plot -", tools::toTitleCase(input$analysis_type)),
        subtitle = paste("Colored by", input$color_var),
        x = x_lab,
        y = y_lab,
        color = input$color_var
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    if (input$color_var == "Sex") {
      p <- p + scale_color_manual(values = c(
        "Male" = "blue",
        "Female" = "hotpink"
      ))
    }
    
    if (input$color_var == "Outcome") {
      p <- p + scale_color_manual(values = c(
        "Protected" = "green",
        "Non-Protected" = "red"
      ))
    }
    
    if (input$color_var == "Antibody") {
      p <- p + scale_color_manual(values = c(
        "High" = "blue",
        "Low" = "orange"
      ))
    }
    
    if (input$color_var == "SubjectID") {
      p <- p + scale_color_viridis_d(option = "turbo")
    }
    
    if (input$color_var == "ParasitemiaDays") {
      p <- p + scale_color_viridis_c(option = "B", name = "Parasitemia Days")
    }
    
    ggplotly(p, tooltip = "text", source = "pca_src")
  })
  
  output$clicked_info <- renderText({
    click_data <- event_data("plotly_click", source = "pca_src")
    hover_data <- event_data("plotly_hover", source = "pca_src")
    df <- selected_data()
    
    info <- click_data
    if (is.null(info)) info <- hover_data
    if (is.null(info)) return("Hover or click a point to see SubjectID, Timepoint, and Sex.")
    
    # nearest row match by x/y
    idx <- which.min((df$PC1 - info$x)^2 + (df$PC2 - info$y)^2)
    
    paste0(
      "SubjectID: ", df$SubjectID[idx], "\n",
      "Timepoint: ", df$Timepoint[idx], "\n",
      "Sex: ", df$Sex[idx]
    )
  })
}

shinyApp(ui = ui, server = server)