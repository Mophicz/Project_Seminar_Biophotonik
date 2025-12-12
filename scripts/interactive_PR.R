library(shiny)
library(PRROC) # Switched to PRROC
library(dplyr)
library(readr)
library(here)

# ==========================================
# 1. Data Loading / Simulation
# ==========================================

# NOTE: Replace this block with your file loading:
df <- read_table(here("data", "df_sim_4.txt"))

# ==========================================
# 2. Pre-calculate Metrics (Global)
# ==========================================
conc_levels <- unique(df$conc)

# Initialize vectors
df$z_mean <- 0; df$z_median <- 0; df$z_iqr <- 0
df$z_trim <- 0; df$z_wins <- 0

for(val in conc_levels) {
  idx <- df$conc == val
  dat <- df$signal_out[idx]
  
  # A. Mean + SD
  m_val <- mean(dat)
  s_val <- sd(dat)
  df$z_mean[idx] <- (dat - m_val) / s_val
  
  # B. Median + MAD
  med_val <- median(dat)
  mad_val <- mad(dat)
  df$z_median[idx] <- (dat - med_val) / mad_val
  
  # C. Median + IQR
  iqr_val <- IQR(dat)
  df$z_iqr[idx] <- (dat - med_val) / (iqr_val / 1.349)
  
  # D. Trimmed (10%)
  trim_m <- mean(dat, trim = 0.1)
  q_lim <- quantile(dat, probs = c(0.1, 0.9))
  trim_sd <- sd(dat[dat >= q_lim[1] & dat <= q_lim[2]])
  df$z_trim[idx] <- (dat - trim_m) / trim_sd
  
  # E. Winsorized (10%)
  wins_dat <- dat
  wins_dat[wins_dat < q_lim[1]] <- q_lim[1]
  wins_dat[wins_dat > q_lim[2]] <- q_lim[2]
  wins_m <- mean(wins_dat)
  wins_s <- sd(wins_dat)
  df$z_wins[idx] <- (dat - wins_m) / wins_s
}

# ==========================================
# 3. Shiny UI
# ==========================================
ui <- fluidPage(
  
  titlePanel("Interactive Threshold Tuner (PR Curve)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("method", "Z-Score Method:", 
                  choices = c("Mean + SD" = "z_mean",
                              "Median + MAD" = "z_median",
                              "Median + IQR" = "z_iqr",
                              "Trimmed Mean" = "z_trim",
                              "Winsorized" = "z_wins")),
      
      # --- Threshold Inputs (Synced) ---
      wellPanel(
        h5(strong("Set Threshold")),
        # Slider for coarse adjustment
        sliderInput("thresh", NULL, 
                    min = -5, max = 5, value = 3, step = 0.01),
        
        # Numeric input for fine/manual adjustment
        numericInput("num_thresh", "Exact Value:", 
                     value = 3, step = 0.01)
      ),
      
      hr(),
      h4("Confusion Matrix"),
      uiOutput("matrixUI"),
      
      br(),
      h4("Metric Derivation"),
      uiOutput("derivationsUI"),
      
      p(style="color:grey; font-size:0.8em; margin-top:10px;", 
        "Note: 'Positive' class = Outlier.")
    ),
    
    mainPanel(
      fluidRow(column(12, plotOutput("scorePlot", height = "300px"))),
      fluidRow(column(12, plotOutput("prPlot", height = "400px")))
    )
  )
)

# ==========================================
# 4. Shiny Server
# ==========================================
server <- function(input, output, session) {
  
  # Reactive: Get the selected z-score vector
  selected_z <- reactive({
    df[[input$method]]
  })
  
  # --- OBSERVERS: Sync Slider and Text Box ---
  observe({
    scores <- selected_z()
    min_val <- min(scores, na.rm = TRUE)
    max_val <- max(scores, na.rm = TRUE)
    
    current_val <- input$num_thresh
    if (is.na(current_val)) current_val <- 3
    if (current_val > max_val) current_val <- max_val
    if (current_val < min_val) current_val <- min_val
    
    updateSliderInput(session, "thresh",
                      min = floor(min_val * 10) / 10, 
                      max = ceiling(max_val * 10) / 10, 
                      value = current_val,
                      step = 0.01)
    
    updateNumericInput(session, "num_thresh", 
                       value = current_val,
                       min = floor(min_val * 10) / 10,
                       max = ceiling(max_val * 10) / 10)
  })
  
  observeEvent(input$thresh, {
    if (is.numeric(input$num_thresh) && input$thresh != input$num_thresh) {
      updateNumericInput(session, "num_thresh", value = input$thresh)
    }
  })
  
  observeEvent(input$num_thresh, {
    if (is.numeric(input$num_thresh) && input$num_thresh != input$thresh) {
      updateSliderInput(session, "thresh", value = input$num_thresh)
    }
  })
  
  # Reactive: Calculate PR Curve Object
  pr_obj <- reactive({
    scores <- selected_z()
    # PRROC requires separate vectors for Positives (class0) and Negatives (class1)
    # class0 = Outliers, class1 = Normal
    scores_pos <- scores[df$is_outlier == TRUE]
    scores_neg <- scores[df$is_outlier == FALSE]
    
    pr.curve(scores.class0 = scores_pos, 
             scores.class1 = scores_neg, 
             curve = TRUE)
  })
  
  # Reactive: Calculate Counts & Metrics (TP, Precision, Recall)
  conf_stats <- reactive({
    scores <- selected_z()
    thresh <- if(is.numeric(input$num_thresh)) input$num_thresh else 3
    truth  <- df$is_outlier
    
    pred_pos <- scores > thresh
    pred_neg <- scores <= thresh
    
    TP <- sum(pred_pos & truth)
    TN <- sum(pred_neg & !truth)
    FP <- sum(pred_pos & !truth)
    FN <- sum(pred_neg & truth)
    
    # Precision = TP / (TP + FP)
    Prec <- if((TP+FP) == 0) 1 else TP/(TP+FP) # Handle div by zero (usually 1 if no positives predicted)
    
    # Recall (Sensitivity) = TP / (TP + FN)
    Rec  <- if((TP+FN) == 0) 0 else TP/(TP+FN)
    
    list(TP=TP, TN=TN, FP=FP, FN=FN, Prec=Prec, Rec=Rec)
  })
  
  # --- UI Output: Visual Confusion Matrix ---
  output$matrixUI <- renderUI({
    s <- conf_stats()
    
    table_style <- "width: 100%; border-collapse: collapse; table-layout: fixed;"
    box_style <- "border: 1px solid #ddd; padding: 8px; text-align: center; vertical-align: middle;"
    head_style <- paste(box_style, "font-weight: bold; background-color: #f9f9f9; font-size: 0.85em;")
    
    w_label <- "width: 30%;"
    w_data  <- "width: 35%;"
    
    tags$table(style = table_style,
               tags$tr(
                 tags$td(style = w_label, ""), 
                 tags$td(style = paste(head_style, w_data), "Actual Outlier"), 
                 tags$td(style = paste(head_style, w_data), "Actual Normal")
               ),
               tags$tr(
                 tags$td(style = paste(head_style, w_label), "Pred. Outlier"),
                 tags$td(style = paste(box_style, "background-color: #dff0d8; color: #3c763d; font-weight:bold;"), paste("TP:", s$TP)), 
                 tags$td(style = paste(box_style, "background-color: #f2dede; color: #a94442;"), paste("FP:", s$FP))
               ),
               tags$tr(
                 tags$td(style = paste(head_style, w_label), "Pred. Normal"),
                 tags$td(style = paste(box_style, "background-color: #fcf8e3; color: #8a6d3b;"), paste("FN:", s$FN)), 
                 tags$td(style = paste(box_style, "background-color: #dff0d8; color: #3c763d; font-weight:bold;"), paste("TN:", s$TN))
               )
    )
  })
  
  # --- UI Output: Visual Math Derivation (Precision/Recall) ---
  output$derivationsUI <- renderUI({
    s <- conf_stats()
    fmt <- function(x) format(round(x, 3), nsmall=3)
    
    div(style = "font-size: 0.9em;",
        # Precision Formula
        div(style="margin-bottom: 8px;",
            strong("Precision"), span("(PPV)"), br(),
            # Formula: TP / (TP + FP)
            code(paste0(s$TP, " / (", s$TP, " + ", s$FP, ")")),
            strong(paste(" = ", fmt(s$Prec)))
        ),
        # Recall Formula
        div(
          strong("Recall"), span("(Sensitivity)"), br(),
          # Formula: TP / (TP + FN)
          code(paste0(s$TP, " / (", s$TP, " + ", s$FN, ")")),
          strong(paste(" = ", fmt(s$Rec)))
        )
    )
  })
  
  # Plot 1: Z-Scores (Standard)
  output$scorePlot <- renderPlot({
    scores <- selected_z()
    thresh <- if(is.numeric(input$num_thresh)) input$num_thresh else 3
    
    pt_col <- ifelse(scores > thresh, "red", "black")
    pt_pch <- ifelse(df$is_outlier, 17, 19)
    
    par(mar = c(4, 4, 2, 1))
    plot(scores, pch = pt_pch, col = pt_col,
         main = paste("Z-Scores:", input$method),
         xlab = "Index", ylab = "Z-Score")
    
    abline(h = thresh, col = "blue", lwd = 2, lty = 2)
    grid()
    legend("topleft", legend=c("Normal", "Outlier (Actual)"), pch=c(19, 17), bty="n")
  })
  
  # Plot 2: PR Curve (Precision vs Recall)
  output$prPlot <- renderPlot({
    pr <- pr_obj()
    s  <- conf_stats()
    thresh <- if(is.numeric(input$num_thresh)) input$num_thresh else 3
    
    par(mar = c(4, 4, 2, 1))
    
    # Plot the curve using PRROC's native plot function
    # auc.main=FALSE to hide default title
    plot(pr, main = "Precision-Recall Curve", 
         auc.main = FALSE, color = "#003366", lwd = 4)
    
    # Add AUPRC to the legend/plot area
    legend("bottomleft", 
           legend = paste("AUPRC =", round(pr$auc.integral, 3)), 
           bty = "n", cex=1.2)
    
    grid()
    
    # Plot the specific point corresponding to the current slider threshold
    # X = Recall, Y = Precision
    points(s$Rec, s$Prec, pch = 19, col = "red", cex = 3)
    
    # Add crosshairs
    abline(v = s$Rec, col = "red", lty = 3, lwd = 2)
    abline(h = s$Prec, col = "red", lty = 3, lwd = 2)
    
    # Shift text down by 0.08 to avoid overlap
    text(s$Rec, s$Prec - 0.08, 
         labels = paste0("  Thresh: ", thresh), 
         pos = 4, col = "red", font = 2, cex = 1.5)
  })
}

shinyApp(ui = ui, server = server)