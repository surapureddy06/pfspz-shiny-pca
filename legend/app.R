#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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

# ---- 1) Define what each label should mean ----
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

# ---- 2) Hotspot helper: creates an invisible clickable box ----
hotspot <- function(id, left, top, width, height, tooltip) {
  actionLink(
    inputId = id,
    label   = "",
    class   = "hotspot",
    style   = sprintf("left:%s; top:%s; width:%s; height:%s;", left, top, width, height),
    ⁠ data-bs-toggle ⁠ = "tooltip",
    ⁠ data-bs-placement ⁠ = "top",
    title = tooltip
  )
}

ui <- fluidPage(
  theme = bs_theme(version = 5),
  
  tags$head(
    # --- CSS for overlay positioning ---
    tags$style(HTML("
      .img-wrap{
        position: relative;
        width: 100%;
        max-width: 1200px;
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
        background: rgba(255, 0, 0, 0.0); /* set to 0.15 while aligning */
        border: 0px solid rgba(255,0,0,0.4); /* set to 1px while aligning */
        z-index: 10;
      }
    ")),
    
    # --- JS to activate Bootstrap tooltips ---
    tags$script(HTML("
      function initTooltips(){
        var els = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        els.forEach(function(el){
          if (!el._tooltip) { el._tooltip = new bootstrap.Tooltip(el); }
        });
      }
      document.addEventListener('DOMContentLoaded', function(){ initTooltips(); });
      document.addEventListener('shiny:connected', function(){ initTooltips(); });
      document.addEventListener('shiny:value', function(){ initTooltips(); });
    "))
  ),
  
  titlePanel("Human PfSPZ CVac (VTEU-0042) — Study Design"),
  
  fluidRow(
    column(
      width = 12,
      
      div(
        class = "img-wrap",
        
        # ---- 3) Your PNG ----
        tags$img(src = "study_design.png"),
        
        # ---- 4) Hotspots ON TOP of the PNG ----
        # IMPORTANT: These numbers are EXAMPLES. You must adjust them to match your image.
        # Use percentages so it works even when window resizes.
        
        hotspot("VAX1", "12%", "20%", "6%", "8%", defs$VAX1),
        hotspot("VAX2", "32%", "20%", "6%", "8%", defs$VAX2),
        hotspot("VAX3", "52%", "20%", "6%", "8%", defs$VAX3),
        
        hotspot("CQ1",  "08%", "08%", "6%", "8%", defs$CQ1),
        hotspot("CQ2",  "25%", "08%", "6%", "8%", defs$CQ2),
        hotspot("CQ3",  "45%", "08%", "6%", "8%", defs$CQ3),
        hotspot("CQ4",  "62%", "08%", "6%", "8%", defs$CQ4),
        
        hotspot("CHMI", "78%", "18%", "7%", "10%", defs$CHMI),
        
        hotspot("PfCSP_Ab", "06%", "48%", "12%", "10%", defs$PfCSP_Ab)
      )
    )
  )
)

server <- function(input, output, session) {
  
  # When user clicks any hotspot -> show a bigger modal message
  lapply(names(defs), function(id) {
    observeEvent(input[[id]], {
      showModal(modalDialog(
        title = id,
        p(defs[[id]]),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)
  })
}

shinyApp(ui, server)