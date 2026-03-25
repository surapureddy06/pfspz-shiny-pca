#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

# ---- 1) Define labels ----
defs <- list(
  VAX1 = "Vaccine dose 1",
  VAX2 = "Vaccine dose 2",
  VAX3 = "Vaccine dose 3",
  CQ1  = "Chloroquine dose 1",
  CQ2  = "Chloroquine dose 2",
  CQ3  = "Chloroquine dose 3",
  CQ4  = "Chloroquine dose 4",
  CHMI = "Controlled human malaria infection",
  PfCSP_Ab = "Antibodies to PfCSP"
)

# ---- 2) Hotspot helper ----
hotspot <- function(id, left, top, width, height, tooltip) {
  actionLink(
    inputId = id,
    label = "",
    class = "hotspot",
    style = sprintf("left:%s; top:%s; width:%s; height:%s;", left, top, width, height),
    `data-bs-toggle` = "tooltip",
    `data-bs-placement` = "top",
    title = tooltip
  )
}

ui <- fluidPage(
  theme = bs_theme(version = 5),
  
  tags$head(
    
    tags$style(HTML("
      .img-wrap{
        position: relative;
        width: 100%;
        max-width: 1200px;
        margin: auto;
      }
      .img-wrap img{
        width: 100%;
        height: auto;
        border: 1px solid #999;
        display:block;
      }

      .hotspot{
        position: absolute;
        display: block;
        background: rgba(255,0,0,0.0);
        border: 0px solid rgba(255,0,0,0.0);
        z-index: 10;
      }

      /* For alignment debugging (optional)
      .hotspot{
        background: rgba(255,0,0,0.2);
        border: 2px solid red;
      }
      */

    ")),
    
    tags$script(HTML("
      function initTooltips(){
        var els = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        els.forEach(function(el){
          if (!el._tooltip) { el._tooltip = new bootstrap.Tooltip(el); }
        });
      }
      document.addEventListener('DOMContentLoaded', function(){ initTooltips(); });
      document.addEventListener('shiny:connected', function(){ initTooltips(); });
    "))
  ),
  
  titlePanel("Human PfSPZ CVac (VTEU-0042) — Study Design"),
  
  fluidRow(
    column(
      12,
      
      div(
        class="img-wrap",
        
        tags$img(src="study_design.png"),
        
        # --------------------
        # COHORT 1
        # --------------------
        
        hotspot("CQ1", "12%", "6%", "6%", "11%", defs$CQ1),
        hotspot("CQ2", "30%", "6%", "6%", "11%", defs$CQ2),
        hotspot("CQ3", "48%", "6%", "6%", "11%", defs$CQ3),
        hotspot("CQ4", "66%", "6%", "6%", "11%", defs$CQ4),
        
        hotspot("VAX1", "15%", "14%", "6%", "12%", defs$VAX1),
        hotspot("VAX2", "35%", "14%", "6%", "12%", defs$VAX2),
        hotspot("VAX3", "55%", "14%", "6%", "12%", defs$VAX3),
        
        hotspot("CHMI", "76%", "10%", "10%", "16%", defs$CHMI),
        
        hotspot("PfCSP_Ab", "6%", "43%", "16%", "12%", defs$PfCSP_Ab),
        
        # --------------------
        # COHORT 3
        # --------------------
        
        hotspot("C3_CQ1", "14%", "56%", "6%", "11%", defs$CQ1),
        hotspot("C3_CQ2", "30%", "56%", "6%", "11%", defs$CQ2),
        hotspot("C3_CQ3", "46%", "56%", "6%", "11%", defs$CQ3),
        hotspot("C3_CQ4", "62%", "56%", "6%", "11%", defs$CQ4),
        
        hotspot("C3_VAX1", "12%", "62%", "7%", "13%", defs$VAX1),
        hotspot("C3_VAX2", "30%", "62%", "7%", "13%", defs$VAX2),
        hotspot("C3_VAX3", "48%", "62%", "7%", "13%", defs$VAX3),
        
        hotspot("C3_CHMI", "76%", "62%", "10%", "16%", defs$CHMI),
        
        hotspot("C3_PfCSP_Ab", "6%", "84%", "16%", "12%", defs$PfCSP_Ab)
        
      )
    )
  )
)

server <- function(input, output, session){
  
  # Cohort 1
  lapply(names(defs), function(id){
    observeEvent(input[[id]],{
      showModal(modalDialog(
        title = defs[[id]],
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)
  })
  
  # Cohort 3 (same labels)
  observeEvent(input$C3_VAX1,{showModal(modalDialog(title=defs$VAX1, easyClose=TRUE, footer=modalButton("Close")))})
  observeEvent(input$C3_VAX2,{showModal(modalDialog(title=defs$VAX2, easyClose=TRUE, footer=modalButton("Close")))})
  observeEvent(input$C3_VAX3,{showModal(modalDialog(title=defs$VAX3, easyClose=TRUE, footer=modalButton("Close")))})
  
  observeEvent(input$C3_CQ1,{showModal(modalDialog(title=defs$CQ1, easyClose=TRUE, footer=modalButton("Close")))})
  observeEvent(input$C3_CQ2,{showModal(modalDialog(title=defs$CQ2, easyClose=TRUE, footer=modalButton("Close")))})
  observeEvent(input$C3_CQ3,{showModal(modalDialog(title=defs$CQ3, easyClose=TRUE, footer=modalButton("Close")))})
  observeEvent(input$C3_CQ4,{showModal(modalDialog(title=defs$CQ4, easyClose=TRUE, footer=modalButton("Close")))})
  
  observeEvent(input$C3_CHMI,{showModal(modalDialog(title=defs$CHMI, easyClose=TRUE, footer=modalButton("Close")))})
  
  observeEvent(input$C3_PfCSP_Ab,{showModal(modalDialog(title=defs$PfCSP_Ab, easyClose=TRUE, footer=modalButton("Close")))})
  
}

shinyApp(ui,server)