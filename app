library(shiny)
library(GGally)
library(ggplot2)
library(readxl)
library(dplyr)
library(shinyjs)
library(svglite)
library(colourpicker)

get_numeric_vars <- function(df) names(df)[sapply(df, is.numeric)]
get_factor_vars <- function(df) names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]

MAXVARS <- 10  # Maximum number of variables for plotting

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Interactive ggpairs Explorer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV/Excel", accept = c(".csv", ".xls", ".xlsx")),
      uiOutput("sheet_ui"),
      checkboxInput("transpose", "Transpose Data (Rows↔Cols)", FALSE),
      tags$hr(),
      
      uiOutput("var_select_ui"),
      uiOutput("factor_select_ui"),
      uiOutput("factor_order_ui"),
      # Tooltip explaining factor variable exclusion
      fluidRow(
        column(10, uiOutput("exclude_select_ui")),
        column(2, 
               tags$span(
                 style="display:inline-block; margin-left:5px;",
                 title = "Excluding the factor variable removes it from the plot matrix, but it will still be used for coloring the points if selected as grouping variable.",
                 icon("info-circle")
               )
        )
      ),
      uiOutput("color_picker_ui"),
      tags$hr(),
      
      selectInput("cor_method", "Correlation Method", c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
      selectInput("upper_cont", "Upper Panel (continuous)", 
                  choices = list("Blank" = "blank", "Density" = "density", "Points" = "points", "Correlation" = "cor", "Box Plot" = "box_no_facet"), 
                  selected = "cor"),
      selectInput("lower_cont", "Lower Panel (continuous)", 
                  choices = list("Blank" = "blank", "Density" = "density", "Points" = "points", "Correlation" = "cor", "Dot Plot" = "dot_no_facet"), 
                  selected = "points"),
      tags$hr(),
      
      checkboxInput("show_smoothing", "Smoothing Options", FALSE),
      conditionalPanel(
        "input.show_smoothing == true",
        checkboxInput("loess", "Add LOESS (red)", TRUE),
        checkboxInput("lm", "Add LM (blue)", FALSE)
      ),
      tags$hr(),
      
      checkboxInput("show_subset", "Subset Plot", FALSE),
      conditionalPanel(
        "input.show_subset == true",
        numericInput("row_plot", "Row", 1, min = 1),
        numericInput("col_plot", "Column", 2, min = 1),
        actionButton("subset_btn", "Show Subset")
      ),
      tags$hr(),
      
      checkboxInput("show_download", "Download Plot", FALSE),
      conditionalPanel(
        "input.show_download == true",
        radioButtons("download_which", "Download Which Plot?", 
                     choices = c("Main ggpairs plot" = "main", "Custom subset plot" = "subset"), selected = "main"),
        numericInput("plot_width", "Width (in)", 10, min = 1),
        numericInput("plot_height", "Height (in)", 7, min = 1),
        numericInput("plot_dpi", "DPI", 600, min = 72),
        selectInput("plot_bg", "Background color", c("white", "transparent"), selected = "white"),
        selectInput("format", "Format", c("png", "svg", "pdf", "tiff")),
        downloadButton("downloadPlot", "Download")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", tableOutput("table")),
        tabPanel("ggpairs Plot",
                 fluidRow(
                   column(10, plotOutput("ggpairs_plot", height = "600px")),
                   column(2,
                          actionButton("update_plot_btn", "Update Plot", width = "100%"),
                          uiOutput("plot_up_to_date"),
                          uiOutput("var_count_warning")
                   )
                 )
        ),
        tabPanel("Correlation Matrix",
                 fluidRow(
                   column(8, tableOutput("correlation_matrix")),
                   column(4, 
                          downloadButton("downloadMatrix", "Download Matrix as CSV")
                   )
                 )
        ),
        tabPanel("Subset Plot", plotOutput("subset_plot", height = "500px"))
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      sheet <- input$sheet %||% excel_sheets(input$file$datapath)[1]
      df <- readxl::read_excel(input$file$datapath, sheet = sheet)
      df <- as.data.frame(df)
    } else {
      showNotification("Unsupported file type.", type = "error")
      return(NULL)
    }
    if (input$transpose) {
      df <- as.data.frame(t(df), stringsAsFactors = FALSE)
      colnames(df) <- df[1, ]
      df <- df[-1, ]
      df[] <- lapply(df, type.convert, as.is = TRUE)
    }
    df
  })
  
  output$sheet_ui <- renderUI({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext %in% c("xls", "xlsx")) {
      sheets <- readxl::excel_sheets(input$file$datapath)
      selectInput("sheet", "Excel Sheet", sheets)
    }
  })
  
  output$var_select_ui <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    choices <- names(df)
    selectInput("vars", "Variables for ggpairs", choices, selected = get_numeric_vars(df), multiple = TRUE)
  })
  output$factor_select_ui <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    choices <- get_factor_vars(df)
    selectInput("factor", "Factor (color)", choices, selected = choices[1])
  })
  
  output$factor_order_ui <- renderUI({
    df <- raw_data()
    factor_var <- input$factor
    if (is.null(df) || is.null(factor_var) || factor_var == "") return(NULL)
    lvls <- unique(as.character(df[[factor_var]]))
    selectInput("factor_order", "Reorder Factor Levels (for legend):", choices = lvls, selected = lvls, multiple = TRUE, selectize = TRUE)
  })
  
  output$exclude_select_ui <- renderUI({
    df <- raw_data()
    factor_var <- input$factor
    choices <- names(df)  # Now allow excluding factor variable
    selectInput("exclude", "Exclude Variables", choices, multiple = TRUE, selected = NULL)
  })
  
  output$color_picker_ui <- renderUI({
    df <- raw_data()
    factor_var <- input$factor
    if (is.null(df) || is.null(factor_var) || factor_var == "") return(NULL)
    lvls <- unique(as.character(df[[factor_var]]))
    lapply(seq_along(lvls), function(i) {
      colourpicker::colourInput(paste0("col_", i), paste("Color for", lvls[i]), value = scales::hue_pal()(length(lvls))[i])
    })
  })
  
  output$table <- renderTable({
    head(raw_data(), 10)
  })
  
  correlation_matrix_long <- reactive({
    df <- raw_data()
    req(df, input$vars)
    numeric_vars <- intersect(input$vars, get_numeric_vars(df))
    validate(need(length(numeric_vars) >= 2, "Select at least two numeric variables"))
    dat <- df[numeric_vars]
    n <- ncol(dat)
    cor_mat <- cor(dat, use = "pairwise.complete.obs", method = input$cor_method)
    p_mat <- matrix(NA, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i != j) {
          test <- cor.test(dat[[i]], dat[[j]], method = input$cor_method)
          p_mat[i, j] <- test$p.value
        } else {
          p_mat[i, j] <- NA
        }
      }
    }
    cor_long <- data.frame(
      Variable1 = rep(colnames(cor_mat), each = n),
      Variable2 = rep(colnames(cor_mat), times = n),
      Correlation = sprintf("%.3f", as.vector(cor_mat)),
      P_Value = sprintf("%.3f", as.vector(p_mat)),
      stringsAsFactors = FALSE
    )
    cor_long <- cor_long[cor_long$Variable1 != cor_long$Variable2, ]
    cor_long
  })
  
  output$correlation_matrix <- renderTable({
    correlation_matrix_long()
  })
  
  output$downloadMatrix <- downloadHandler(
    filename = function() sprintf("correlation_matrix_%s.csv", Sys.Date()),
    content = function(file) {
      write.csv(correlation_matrix_long(), file, row.names = FALSE)
    }
  )
  
  my_fn <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
      geom_point() +
      {if (isTRUE(input$loess)) geom_smooth(method = "loess", color = "red", fill = "red", ...) } +
      {if (isTRUE(input$lm))    geom_smooth(method = "lm", color = "blue", fill = "blue", ...) }
  }
  
  # ---- Two-step plotting logic ----
  plot_state <- reactiveValues(
    plot_vars = NULL,
    plot_factor = NULL,
    plot_exclude = NULL,
    plot_factor_order = NULL,
    plot_data = NULL,
    plot_obj = NULL,
    up_to_date = FALSE
  )
  
  get_plot_vars <- reactive({
    df <- raw_data()
    req(df, input$vars, input$factor)
    factor_var <- input$factor
    exclude_vars <- input$exclude  # Now, can include factor var
    use_vars <- setdiff(input$vars, exclude_vars)
    unique(use_vars)
  })
  
  observe({
    n_vars <- length(get_plot_vars())
    disable <- n_vars > MAXVARS
    shinyjs::toggleState("update_plot_btn", !disable)
  })
  
  output$var_count_warning <- renderUI({
    n_vars <- length(get_plot_vars())
    if (n_vars > MAXVARS)
      tags$div(
        style = "color:red; font-weight:bold; margin-top:10px;",
        paste("Too many variables selected (", n_vars, "). Maximum allowed is ", MAXVARS, ".", sep = "")
      )
  })
  
  observe({
    isolate({
      if (!is.null(plot_state$plot_vars)) {
        input_vars <- get_plot_vars()
        same_vars <- identical(sort(input_vars), sort(plot_state$plot_vars))
        same_factor <- identical(input$factor, plot_state$plot_factor)
        same_exclude <- identical(sort(input$exclude), sort(plot_state$plot_exclude))
        same_order <- identical(input$factor_order, plot_state$plot_factor_order)
        if (!all(same_vars, same_factor, same_exclude, same_order)) {
          plot_state$up_to_date <- FALSE
        }
      }
    })
  })
  
  output$plot_up_to_date <- renderUI({
    if (!isTRUE(plot_state$up_to_date)) {
      tags$div(
        style = "color: orange; font-weight: bold; margin-top:10px;",
        "Plot not up-to-date, please click 'Update Plot'."
      )
    } else {
      NULL
    }
  })
  
  observeEvent(input$update_plot_btn, {
    req(raw_data())
    n_vars <- length(get_plot_vars())
    if (n_vars > MAXVARS) return(NULL)
    df <- raw_data()
    plot_vars <- get_plot_vars()
    factor_var <- input$factor
    factor_order <- input$factor_order
    
    # For coloring, always add the factor variable back for mapping, if needed
    plot_df <- df[, unique(c(plot_vars, factor_var)), drop = FALSE]
    if (!is.null(factor_order) && length(factor_order) > 1) {
      plot_df[[factor_var]] <- factor(plot_df[[factor_var]], levels = factor_order)
    } else {
      plot_df[[factor_var]] <- factor(plot_df[[factor_var]])
    }
    lvls <- levels(plot_df[[factor_var]])
    colors <- sapply(seq_along(lvls), function(i) input[[paste0("col_", i)]] %||% scales::hue_pal()(length(lvls))[i])
    
    withProgress(message = "Generating ggpairs plot...", value = 0, {
      incProgress(0.3)
      # Only plot non-factor variables if excluded, but pass factor var for mapping
      if (factor_var %in% plot_vars) {
        columns_to_plot <- seq_along(plot_vars)
      } else {
        # Exclude from plot panels, but pass for mapping
        columns_to_plot <- which(names(plot_df) %in% plot_vars)
      }
      p <- ggpairs(
        plot_df,
        columns = columns_to_plot,
        mapping = aes(color = .data[[factor_var]]),
        upper = list(continuous = wrap(input$upper_cont, method = input$cor_method)),
        lower = list(continuous = if (input$show_smoothing) my_fn else input$lower_cont),
        legend = 1
      ) +
        scale_color_manual(values = colors) +
        theme(text = element_text(size = 12))
      incProgress(0.7)
      plot_state$plot_vars <- plot_vars
      plot_state$plot_factor <- factor_var
      plot_state$plot_exclude <- input$exclude
      plot_state$plot_factor_order <- factor_order
      plot_state$plot_data <- plot_df
      plot_state$plot_obj <- p
      plot_state$up_to_date <- TRUE
    })
  }, ignoreNULL = TRUE)
  
  output$ggpairs_plot <- renderPlot({
    req(plot_state$plot_obj)
    print(plot_state$plot_obj)
  })
  
  subset_plot_obj <- reactiveVal(NULL)
  observeEvent(input$subset_btn, {
    req(plot_state$plot_obj, plot_state$plot_data)
    n <- length(plot_state$plot_vars)
    row <- input$row_plot %||% 1
    col <- input$col_plot %||% 2
    if (row > n || col > n) {
      showNotification("Row or column out of range.", type = "error")
      return()
    }
    withProgress(message = "Generating subset plot...", value = 0, {
      incProgress(0.5)
      p <- tryCatch(plot_state$plot_obj[row, col], error = function(e) NULL)
      subset_plot_obj(p)
      incProgress(0.5)
      output$subset_plot <- renderPlot({ print(p) })
    })
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() sprintf("plot_%s.%s", Sys.Date(), input$format),
    content = function(file) {
      p <- switch(
        input$download_which,
        "main"   = plot_state$plot_obj,
        "subset" = subset_plot_obj() %||% plot_state$plot_obj
      )
      withProgress(message = "Saving plot...", value = 0, {
        incProgress(0.5)
        if (input$format == "svg") {
          svglite::svglite(file, width = input$plot_width, height = input$plot_height, bg = input$plot_bg)
          print(p)
          dev.off()
        } else {
          ggsave(
            file, plot = p, width = input$plot_width, height = input$plot_height,
            dpi = input$plot_dpi, bg = input$plot_bg, device = input$format
          )
        }
        incProgress(0.5)
      })
    }
  )
}

shinyApp(ui, server)
