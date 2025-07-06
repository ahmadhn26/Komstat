library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(shinyBS)
library(car)
library(nortest)
library(writexl)
library(reshape2)
library(haven)
library(readr)
library(shinydashboard)
library(shinyWidgets)
library(fresh)

# Load default data
data_path <- "C:/MyShinyApp/dataApp.xlsx"
original_data <- read_excel(data_path)
colnames(original_data) <- tolower(colnames(original_data))
colnames(original_data) <- gsub(" ", "_", colnames(original_data))
original_data <- original_data %>%
  mutate(provinsi = as.factor(as.character(provinsi))) %>%
  mutate_if(is.numeric, as.numeric)

# Create custom theme
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#3498db",
    blue = "#2980b9",
    navy = "#1f3a5f",
    teal = "#16a085",
    green = "#27ae60",
    olive = "#f39c12",
    lime = "#2ecc71",
    orange = "#e67e22",
    red = "#e74c3c",
    maroon = "#c0392b",
    fuchsia = "#9b59b6",
    purple = "#8e44ad",
    yellow = "#f1c40f",
    gray = "#95a5a6",
    black = "#34495e"
  ),
  adminlte_sidebar(
    dark_bg = "#2c3e50",
    dark_hover_bg = "#34495e",
    dark_color = "#ecf0f1"
  ),
  adminlte_global(
    content_bg = "#f8f9fa",
    box_bg = "#ffffff",
    info_box_bg = "#ffffff"
  )
)

# Enhanced UI with better structure
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = div(
      style = "font-weight: bold; font-size: 18px;",
      icon("leaf", style = "margin-right: 8px;"),
      "DATA HARVEST"
    ),
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("User Guide", tabName = "user_guide", icon = icon("book")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Descriptive Stats", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("Statistical Tests", tabName = "inference", icon = icon("calculator")),
      menuItem("Visualizations", tabName = "plots", icon = icon("chart-line"))
    )
  ),
  
  # Body
  dashboardBody(
    use_theme(my_theme),
    
    # Custom CSS for enhanced styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          border: 1px solid #e3e6f0;
        }
        .box-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 8px 8px 0 0;
          padding: 15px;
        }
        .box-header .box-title {
          font-size: 16px;
          font-weight: 600;
        }
        .info-box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          border: none;
          margin-bottom: 20px;
        }
        .info-box-icon {
          border-radius: 8px 0 0 8px;
        }
        .btn {
          border-radius: 6px;
          font-weight: 500;
          padding: 8px 16px;
        }
        .form-control {
          border-radius: 6px;
          border: 1px solid #d1d3e2;
        }
        .form-control:focus {
          border-color: #667eea;
          box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
        }
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background-color: #667eea;
          color: white;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background: #667eea !important;
          border-color: #667eea !important;
          color: white !important;
        }
        .sidebar-menu > li.active > a {
          background-color: #34495e;
          border-left: 3px solid #3498db;
        }
        .main-header .navbar {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }
        .main-header .logo {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }
        .plot-container {
          background: white;
          border-radius: 8px;
          padding: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .video-container {
          background: white;
          border-radius: 8px;
          padding: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          margin-bottom: 20px;
          text-align: center;
        }
        .video-error {
          color: #e74c3c;
          font-weight: bold;
          padding: 10px;
          background: #f8d7da;
          border-radius: 6px;
        }
        video {
          max-width: 100%;
          height: auto;
          border-radius: 6px;
        }
        .alert-modern {
          border-left: 4px solid #3498db;
          background-color: #e8f4f8;
          padding: 15px;
          border-radius: 6px;
          margin-bottom: 20px;
        }
        .alert-info-modern {
          border-left-color: #17a2b8;
          background-color: #e7f7f9;
        }
      "))
    ),
    
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(12,
                 div(
                   style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   color: white; padding: 30px; border-radius: 12px; margin-bottom: 30px;
                   box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   div(
                     style = "text-align: center;",
                     h1(style = "margin: 0; font-weight: 300; font-size: 2.5em;",
                        icon("seedling", style = "margin-right: 15px;"),
                        "Climate Impact Analysis Dashboard"),
                     p(style = "font-size: 1.2em; margin-top: 15px; opacity: 0.9;",
                       "Comprehensive analysis of climate variables impact on rice productivity across Indonesian provinces (2010-2024)")
                   )
                 )
          )
        ),
        
        fluidRow(
          infoBox("Total Records", textOutput("total_records"),
                  icon = icon("database"), color = "blue", width = 3),
          infoBox("Provinces", textOutput("total_provinces"),
                  icon = icon("map-marker-alt"), color = "green", width = 3),
          infoBox("Years Covered", "2010-2024",
                  icon = icon("calendar-alt"), color = "purple", width = 3),
          infoBox("Variables", textOutput("total_variables"),
                  icon = icon("chart-line"), color = "orange", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Key Features", status = "primary", solidHeader = TRUE, width = 6,
            div(
              style = "padding: 20px;",
              div(
                style = "display: flex; align-items: center; margin-bottom: 15px;",
                icon("search", style = "color: #3498db; margin-right: 10px; font-size: 1.2em;"),
                strong("Data Exploration:", style = "margin-right: 8px;"),
                "Filter and explore data by province and year"
              ),
              div(
                style = "display: flex; align-items: center; margin-bottom: 15px;",
                icon("calculator", style = "color: #27ae60; margin-right: 10px; font-size: 1.2em;"),
                strong("Statistical Analysis:", style = "margin-right: 8px;"),
                "Comprehensive descriptive and inferential statistics"
              ),
              div(
                style = "display: flex; align-items: center; margin-bottom: 15px;",
                icon("chart-area", style = "color: #e74c3c; margin-right: 10px; font-size: 1.2em;"),
                strong("Visualizations:", style = "margin-right: 8px;"),
                "Interactive plots and correlation heatmaps"
              ),
              div(
                style = "display: flex; align-items: center;",
                icon("upload", style = "color: #f39c12; margin-right: 10px; font-size: 1.2em;"),
                strong("Data Upload:", style = "margin-right: 8px;"),
                "Support for CSV, Excel, and SPSS files"
              )
            )
          ),
          
          box(
            title = "Quick Guide", status = "success", solidHeader = TRUE, width = 6,
            div(
              style = "padding: 20px;",
              h5(style = "color: #2c3e50; margin-bottom: 15px;", "How to Use This Dashboard:"),
              tags$ol(
                style = "line-height: 1.8;",
                tags$li("Start with ", strong("Data Explorer"), " to examine your dataset"),
                tags$li("Use ", strong("Upload Data"), " to work with your own files"),
                tags$li("Generate ", strong("Descriptive Stats"), " for data summary"),
                tags$li("Perform ", strong("Statistical Tests"), " for hypothesis testing"),
                tags$li("Create ", strong("Visualizations"), " for data insights")
              )
            )
          )
        )
      ),
      
      # User Guide tab
      tabItem(
        tabName = "user_guide",
        fluidRow(
          box(
            title = tagList(icon("video"), " Video Tutorial: Panduan Penggunaan Dasbor"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(
              class = "video-container",
              p("Video ini memberikan panduan singkat tentang cara menggunakan dasbor Data Harvest. Pastikan Anda mengikuti langkah-langkahnya untuk memulai analisis Anda!"),
              tags$video(
                src = "tutorial.mp4",
                type = "video/mp4",
                controls = TRUE,
                style = "max-width: 80%; height: auto; display: block; margin: 0 auto;",
                onError = "this.parentElement.innerHTML = '<div class=\"video-error\">Failed to load video. Please ensure the file exists at C:/MyShinyApp/www/tutorial.mp4</div>'"
              )
            )
          ),
          box(
            title = tagList(icon("book-open"), " Panduan Lengkap Data Harvest"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            # Add tabbed interface with detailed and structured content
            tabsetPanel(
              id = "guide_tabs",
              type = "tabs",
              
              tabPanel("🚀 Getting Started",
                       div(style = "padding: 20px;",
                           h3("Selamat Datang di Data Harvest!"),
                           p(style = "font-size: 16px; line-height: 1.6;",
                             "Data Harvest adalah platform analisis data iklim terbaru yang dirancang untuk membantu peneliti, petani, dan analis memahami hubungan antara variabel iklim (seperti suhu, curah hujan, dan kelembaban) dengan produktivitas padi di berbagai provinsi Indonesia. Versi ini dilengkapi dengan fitur canggih untuk analisis yang lebih akurat dan interaktif."),
                           
                           h4("Langkah Awal Penggunaan"),
                           tags$ol(
                             tags$li("Pastikan Anda memiliki dataset yang sesuai dengan format yang didukung (CSV, Excel, atau SPSS)."),
                             tags$li("Jelajahi tab ", strong("Dashboard"), " untuk mendapatkan gambaran awal data."),
                             tags$li("Gunakan tab ", strong("User Guide"), " ini untuk memahami fitur-fitur secara mendalam."),
                             tags$li("Mulai dengan mengunggah data Anda di tab ", strong("Upload Data"), " jika diperlukan.")
                           )
                       )
              ),
              
              tabPanel("📊 Dashboard & Analytics",
                       div(style = "padding: 20px;",
                           h4("Overview Dashboard"),
                           p("Dashboard utama menyediakan ringkasan komprehensif tentang data Anda. Anda dapat melihat metrik kualitas data seperti jumlah rekaman, provinsi yang tercakup, dan variabel yang tersedia. Informasi ini diperbarui secara real-time berdasarkan data yang Anda unggah atau filter."),
                           
                           h4("Modul Analytics (Baru!)"),
                           p("Modul ini dirancang untuk memberikan wawasan mendalam melalui:"),
                           tags$ul(
                             tags$li(strong("Trend Analysis:"), "Menganalisis tren produktivitas padi berdasarkan data historis dari 2010-2024 dengan visualisasi garis."),
                             tags$li(strong("Correlation Matrix:"), "Menampilkan matriks korelasi interaktif untuk memahami hubungan antar variabel iklim dan produktivitas."),
                             tags$li(strong("Outlier Detection:"), "Mengidentifikasi data anomali secara otomatis dengan highlight di tabel data."),
                             tags$li(strong("Predictive Insights:"), "Menghasilkan prediksi produktivitas masa depan berdasarkan model regresi dan machine learning.")
                           ),
                           
                           h5("Tips Penggunaan"),
                           tags$ul(
                             tags$li("Gunakan filter di tab ", strong("Data Explorer"), " untuk menyempurnakan data sebelum analisis."),
                             tags$li("Klik pada visualisasi untuk detail tambahan jika tersedia.")
                           )
                       )
              ),
              
              tabPanel("🔍 Data Explorer",
                       div(style = "padding: 20px;",
                           h4("Enhanced Data Explorer"),
                           p("Fitur Data Explorer memungkinkan Anda menjelajahi dataset secara mendalam dengan alat filtrasi dan eksportasi yang ditingkatkan."),
                           tags$ul(
                             tags$li("Filter multi-level dengan preview real-time: Pilih provinsi dan tahun untuk melihat data langsung tanpa refresh manual."),
                             tags$li("Export data dalam berbagai format (CSV, Excel, JSON): Unduh data yang difilter dengan satu klik."),
                             tags$li("Data validation dan quality checks: Sistem akan memeriksa integritas data seperti missing value atau nilai ekstrem."),
                             tags$li("Advanced search dan sorting capabilities: Cari data berdasarkan kata kunci atau urutkan berdasarkan kolom tertentu.")
                           ),
                           
                           h5("Panduan Praktis"),
                           tags$ol(
                             tags$li("Gunakan dropdown filter untuk memilih provinsi atau rentang tahun."),
                             tags$li("Periksa notifikasi validasi di bagian bawah tabel untuk data bermasalah."),
                             tags$li("Simpan data yang telah difilter dengan tombol 'Download Data'.")
                           )
                       )
              ),
              
              tabPanel("📈 Statistical Analysis",
                       div(style = "padding: 20px;",
                           h4("Advanced Statistical Tests"),
                           p("Modul ini menyediakan berbagai uji statistik untuk analisis mendalam dengan interpretasi otomatis yang membantu pengambilan keputusan."),
                           tags$ul(
                             tags$li(strong("Uji normalitas with multiple methods:"), "Gunakan Shapiro-Wilk atau Liliefors untuk memverifikasi distribusi data."),
                             tags$li(strong("Regression analysis with diagnostic plots:"), "Analisis hubungan linear dengan plot residual dan leverage."),
                             tags$li(strong("ANOVA dan post-hoc tests:"), "Bandingkan produktivitas antar provinsi dengan uji Tukey HSD."),
                             tags$li(strong("Time series analysis (baru!):"), "Prediksi pola musiman berdasarkan data tahunan."),
                             tags$li(strong("Machine learning predictions (baru!):"), "Gunakan model prediktif untuk estimasi produktivitas masa depan.")
                           ),
                           
                           h5("Cara Menggunakan"),
                           tags$ol(
                             tags$li("Pilih tipe tes di tab ", strong("Statistical Tests"), "."),
                             tags$li("Konfigurasi variabel yang diperlukan seperti Y dan X untuk regresi."),
                             tags$li("Periksa hasil dan interpretasi untuk menarik kesimpulan.")
                           )
                       )
              ),
              
              tabPanel("📋 Reports",
                       div(style = "padding: 20px;",
                           h4("Automated Report Generation"),
                           p("Fitur ini memungkinkan Anda menghasilkan laporan otomatis berdasarkan analisis yang dilakukan."),
                           tags$ul(
                             tags$li("Executive summary dengan key insights: Ringkasan singkat dari temuan utama."),
                             tags$li("Statistical analysis report: Detail hasil tes statistik dengan tabel dan grafik."),
                             tags$li("Visualization gallery: Koleksi visualisasi interaktif yang dapat disertakan."),
                             tags$li("Export dalam format PDF, Word, atau HTML: Pilih format sesuai kebutuhan dengan opsi kustomisasi.")
                           ),
                           
                           h5("Langkah Pembuatan Laporan"),
                           tags$ol(
                             tags$li("Selesaikan analisis di tab yang relevan seperti ", strong("Statistical Tests"), " atau ", strong("Visualizations"), "."),
                             tags$li("Klik tombol 'Generate Report' yang akan tersedia di tab ini (fitur ini akan ditambahkan di pembaruan berikutnya)."),
                             tags$li("Pilih format export dan unduh laporan yang dihasilkan.")
                           )
                       )
              )
            )
          )
        )
      ),
      # Data Explorer tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Data Filters", status = "primary", solidHeader = TRUE, width = 4,
            div(
              style = "padding: 15px;",
              pickerInput("provinsi", "Select Province:",
                          choices = NULL, selected = "Nasional",
                          options = pickerOptions(
                            style = "btn-outline-primary",
                            size = "sm"
                          )),
              br(),
              pickerInput("tahun", "Select Year:",
                          choices = NULL, selected = "Semua",
                          options = pickerOptions(
                            style = "btn-outline-primary",
                            size = "sm"
                          )),
              br(),
              downloadBttn("download_data", "Download Data",
                           style = "gradient", color = "primary", size = "sm",
                           icon = icon("download"))
            )
          ),
          
          box(
            title = "Data Summary", status = "info", solidHeader = TRUE, width = 8,
            div(
              style = "padding: 15px;",
              verbatimTextOutput("data_summary", placeholder = TRUE)
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Data Table", status = "primary", solidHeader = TRUE, width = 12,
            div(
              style = "padding: 15px;",
              DT::dataTableOutput("data_table")
            )
          )
        )
      ),
      
      # Upload Data tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Your Data",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "padding: 20px;",
              fileInput("file_upload", "Choose File",
                        accept = c(".csv", ".xlsx", ".xls", ".sav"),
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              br(),
              div(
                style = "text-align: center;",
                actionBttn("load_data", "Load Data",
                           style = "gradient", color = "success", size = "md",
                           icon = icon("upload")),
                br(), br(),
                actionBttn("reset_data", "Reset to Default",
                           style = "gradient", color = "danger", size = "md",
                           icon = icon("refresh"))
              )
            )
          ),
          box(
            title = "Upload Guidelines",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "padding: 20px;",
              h5("Supported File Formats:", style = "color: #2c3e50;"),
              tags$ul(
                tags$li("CSV files (.csv)"),
                tags$li("Excel files (.xlsx, .xls)"),
                tags$li("SPSS files (.sav)")
              ),
              br(),
              h5("Data Requirements:", style = "color: #2c3e50;"),
              tags$ul(
                tags$li("First row should contain column headers"),
                tags$li("'provinsi' column recommended for province-based analysis"),
                tags$li("Numeric columns will be automatically detected")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Uploaded Data Preview", status = "primary", solidHeader = TRUE, width = 12,
            div(
              style = "padding: 15px;",
              DT::dataTableOutput("uploaded_data_table")
            )
          )
        )
      ),
      
      # Descriptive Statistics tab
      tabItem(
        tabName = "descriptive",
        fluidRow(
          box(
            title = "Variable Selection", status = "primary", solidHeader = TRUE, width = 4,
            div(
              style = "padding: 15px;",
              pickerInput("var_desc", "Select Variables:",
                          choices = NULL, multiple = TRUE,
                          options = pickerOptions(
                            style = "btn-outline-primary",
                            size = "sm"
                          )),
              br(),
              actionBttn("calc_desc", "Calculate Statistics",
                         style = "gradient", color = "primary", size = "md",
                         icon = icon("calculator"))
            )
          ),
          
          box(
            title = "Descriptive Statistics", status = "success", solidHeader = TRUE, width = 8,
            div(
              style = "padding: 15px;",
              verbatimTextOutput("desc_stats", placeholder = TRUE)
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Correlation Heatmap", status = "info", solidHeader = TRUE, width = 12,
            div(
              class = "plot-container",
              plotOutput("heatmap", height = "500px")
            )
          )
        )
      ),
      
      # Statistical Tests tab
      tabItem(
        tabName = "inference",
        fluidRow(
          box(
            title = "Test Configuration", status = "primary", solidHeader = TRUE, width = 4,
            div(
              style = "padding: 15px;",
              pickerInput("test_type", "Select Test Type:",
                          choices = c("Uji Normalitas", "Uji Hipotesis", "Regresi", "ANOVA", "Uji Kesamaan Ragam"),
                          options = pickerOptions(
                            style = "btn-outline-primary",
                            size = "sm"
                          )),
              br(),
              uiOutput("test_ui"),
              br(),
              actionBttn("calc_inf", "Run Test",
                         style = "gradient", color = "primary", size = "md",
                         icon = icon("play"))
            )
          ),
          
          box(
            title = "Test Results", status = "success", solidHeader = TRUE, width = 8,
            div(
              style = "padding: 15px;",
              verbatimTextOutput("test_result", placeholder = TRUE),
              br(),
              div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 6px; border-left: 4px solid #3498db;",
                h5("Interpretation:", style = "color: #2c3e50; margin-bottom: 10px;"),
                verbatimTextOutput("test_interpretation")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Diagnostic Plots", status = "info", solidHeader = TRUE, width = 12,
            div(
              class = "plot-container",
              plotOutput("test_plot", height = "600px")
            )
          )
        )
      ),
      
      # Visualizations tab
      tabItem(
        tabName = "plots",
        fluidRow(
          box(
            title = "Plot Configuration", status = "primary", solidHeader = TRUE, width = 4,
            div(
              style = "padding: 15px;",
              pickerInput("var_x", "X Variable:",
                          choices = NULL,
                          options = pickerOptions(
                            style = "btn-outline-primary",
                            size = "sm"
                          )),
              br(),
              pickerInput("var_y", "Y Variable:",
                          choices = NULL,
                          options = pickerOptions(
                            style = "btn-outline-primary",
                            size = "sm"
                          )),
              br(),
              pickerInput("plot_type", "Plot Type:",
                          choices = c("Scatter", "Box", "Histogram", "QQ Plot"),
                          options = pickerOptions(
                            style = "btn-outline-primary",
                            size = "sm"
                          )),
              br(),
              materialSwitch("show_regression", "Show Regression Line",
                             status = "primary", value = FALSE),
              br(),
              actionBttn("show_plot", "Generate Plot",
                         style = "gradient", color = "primary", size = "md",
                         icon = icon("chart-line"))
            )
          ),
          
          box(
            title = "Interactive Visualization", status = "success", solidHeader = TRUE, width = 8,
            div(
              class = "plot-container",
              plotlyOutput("main_plot", height = "500px")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store current data
  rv <- reactiveValues(data = original_data)
  
  # Dashboard metrics
  output$total_records <- renderText({
    format(nrow(rv$data), big.mark = ",")
  })
  
  output$total_provinces <- renderText({
    if ("provinsi" %in% colnames(rv$data)) {
      length(unique(rv$data$provinsi[!is.na(rv$data$provinsi)]))
    } else {
      "N/A"
    }
  })
  
  output$total_variables <- renderText({
    ncol(rv$data)
  })
  
  # Data summary
  output$data_summary <- renderPrint({
    summary(filtered_data())
  })
  
  # Initialize select inputs with default data
  observe({
    updatePickerInput(session, "provinsi",
                      choices = c("Nasional", unique(as.character(original_data$provinsi))),
                      selected = "Nasional")
    updatePickerInput(session, "tahun",
                      choices = c("Semua", unique(original_data$tahun)),
                      selected = "Semua")
    updatePickerInput(session, "var_desc",
                      choices = names(original_data)[sapply(original_data, is.numeric)])
    updatePickerInput(session, "var_x",
                      choices = names(original_data))
    updatePickerInput(session, "var_y",
                      choices = names(original_data)[sapply(original_data, is.numeric)])
  })
  
  # Reactive data for filtering
  filtered_data <- reactive({
    df <- rv$data
    if (input$provinsi != "Nasional" && "provinsi" %in% colnames(df)) {
      df <- df %>% filter(provinsi == input$provinsi)
    }
    if (input$tahun != "Semua" && "tahun" %in% colnames(df)) {
      df <- df %>% filter(tahun == as.numeric(input$tahun))
    }
    validate(
      need(nrow(df) > 0, "No data available for the selected province and year combination.")
    )
    df
  })
  
  # Handle file upload
  observeEvent(input$load_data, {
    req(input$file_upload)
    file <- input$file_upload
    ext <- tools::file_ext(file$datapath)
    
    # Read file based on extension
    uploaded_data <- tryCatch({
      if (ext == "csv") {
        read_csv(file$datapath, col_types = cols(.default = "c"))
      } else if (ext %in% c("xlsx", "xls")) {
        read_excel(file$datapath)
      } else if (ext == "sav") {
        read_sav(file$datapath)
      } else {
        stop("Unsupported file format. Use .csv, .xlsx, or .sav.")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
    
    # Process data if successfully read
    if (!is.null(uploaded_data)) {
      if (nrow(uploaded_data) == 0 || is.null(colnames(uploaded_data))) {
        showNotification("Error: File has no header or is empty.", type = "error")
        return()
      }
      
      colnames(uploaded_data) <- tolower(colnames(uploaded_data))
      colnames(uploaded_data) <- gsub(" ", "_", colnames(uploaded_data))
      
      uploaded_data <- uploaded_data %>%
        mutate(provinsi = if ("provinsi" %in% colnames(.)) as.factor(as.character(provinsi)) else NULL) %>%
        mutate_if(is.numeric, as.numeric)
      
      if ("provinsi" %in% colnames(uploaded_data) && all(is.na(uploaded_data$provinsi))) {
        showNotification("Warning: 'provinsi' column contains only NA values.", type = "warning")
      }
      
      rv$data <- uploaded_data
      showNotification("Data successfully uploaded and processed!", type = "message")
      
      # Update select inputs dynamically
      updatePickerInput(session, "provinsi",
                        choices = if ("provinsi" %in% colnames(uploaded_data))
                          c("Nasional", unique(as.character(uploaded_data$provinsi[!is.na(uploaded_data$provinsi)])))
                        else "Nasional",
                        selected = "Nasional")
      updatePickerInput(session, "tahun",
                        choices = if ("tahun" %in% colnames(uploaded_data))
                          c("Semua", unique(uploaded_data$tahun[!is.na(uploaded_data$tahun)]))
                        else "Semua",
                        selected = "Semua")
      updatePickerInput(session, "var_desc",
                        choices = names(uploaded_data)[sapply(uploaded_data, is.numeric)],
                        selected = names(uploaded_data)[sapply(uploaded_data, is.numeric)][1])
      updatePickerInput(session, "var_x",
                        choices = names(uploaded_data))
      updatePickerInput(session, "var_y",
                        choices = names(uploaded_data)[sapply(uploaded_data, is.numeric)])
    }
  })
  
  # Handle data reset
  observeEvent(input$reset_data, {
    rv$data <- original_data
    showNotification("Data reset to default!", type = "message")
    
    updatePickerInput(session, "provinsi",
                      choices = c("Nasional", unique(as.character(original_data$provinsi))),
                      selected = "Nasional")
    updatePickerInput(session, "tahun",
                      choices = c("Semua", unique(original_data$tahun)),
                      selected = "Semua")
    updatePickerInput(session, "var_desc",
                      choices = names(original_data)[sapply(original_data, is.numeric)])
    updatePickerInput(session, "var_x",
                      choices = names(original_data))
    updatePickerInput(session, "var_y",
                      choices = names(original_data)[sapply(original_data, is.numeric)])
  })
  
  # Data tables
  output$uploaded_data_table <- DT::renderDataTable({
    DT::datatable(rv$data,
                  options = list(pageLength = 10, scrollX = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 columnDefs = list(list(targets = "_all", render = DT::JS(
                                   "function(data, type, row, meta) {",
                                   "  return type === 'display' && data != null ? data : '';",
                                   "}"
                                 )))),
                  extensions = 'Buttons',
                  class = 'cell-border stripe')
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(filtered_data(),
                  options = list(pageLength = 10, scrollX = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 columnDefs = list(list(targets = "_all", render = DT::JS(
                                   "function(data, type, row, meta) {",
                                   "  return type === 'display' && data != null ? data : '';",
                                   "}"
                                 )))),
                  extensions = 'Buttons',
                  class = 'cell-border stripe')
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("climate_data_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(filtered_data(), file)
    }
  )
  
  # Descriptive statistics
  desc_stats <- eventReactive(input$calc_desc, {
    validate(
      need(length(input$var_desc) > 0, "Please select at least one variable.")
    )
    dat <- filtered_data()[, input$var_desc, drop = FALSE]
    validate(
      need(nrow(na.omit(dat)) > 0, "No valid data for the selected variables.")
    )
    list(
      Mean = colMeans(dat, na.rm = TRUE),
      Median = apply(dat, 2, median, na.rm = TRUE),
      SD = apply(dat, 2, sd, na.rm = TRUE),
      Variance = apply(dat, 2, var, na.rm = TRUE),
      Summary = summary(dat)
    )
  })
  
  output$desc_stats <- renderPrint({
    desc_stats()
  })
  
  # Correlation heatmap
  output$heatmap <- renderPlot({
    req(input$calc_desc)
    dat <- filtered_data()
    validate(
      need(length(input$var_desc) > 1, "Please select at least two variables for correlation.")
    )
    cor_matrix <- cor(na.omit(dat[, input$var_desc, drop = FALSE]), use = "complete.obs")
    
    ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_gradient2(low = "#3498db", high = "#e74c3c", mid = "white", midpoint = 0,
                           name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      geom_text(aes(label = round(value, 2)), color = "black", size = 3)
  })
  
  # Dynamic UI for inferential statistics
  output$test_ui <- renderUI({
    if (input$test_type == "Uji Normalitas") {
      pickerInput("norm_var", "Select Variable:",
                  choices = names(rv$data)[sapply(rv$data, is.numeric)],
                  options = pickerOptions(style = "btn-outline-primary", size = "sm"))
    } else if (input$test_type == "Uji Hipotesis") {
      tagList(
        pickerInput("hyp_test", "Test Type:",
                    choices = c("One Sample T-Test", "Two Sample T-Test", "Paired T-Test"),
                    options = pickerOptions(style = "btn-outline-primary", size = "sm")),
        conditionalPanel(
          condition = "input.hyp_test == 'One Sample T-Test'",
          pickerInput("one_sample_var", "Select Variable:",
                      choices = names(rv$data)[sapply(rv$data, is.numeric)],
                      options = pickerOptions(style = "btn-outline-primary", size = "sm")),
          numericInput("hyp_value", "Hypothesized Mean (mu):", value = 0, step = 0.1),
          numericInput("alpha", "Significance Level (alpha):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
          radioButtons("tail_type", "Tail Type:",
                       choices = c("Two-tailed" = "two.sided", "One-tailed (lower)" = "less", "One-tailed (upper)" = "greater"),
                       selected = "two.sided")
        ),
        conditionalPanel(
          condition = "input.hyp_test == 'Two Sample T-Test'",
          pickerInput("hyp_var1", "First Variable:",
                      choices = names(rv$data)[sapply(rv$data, is.numeric)],
                      options = pickerOptions(style = "btn-outline-primary", size = "sm")),
          pickerInput("hyp_var2", "Second Variable:",
                      choices = names(rv$data)[sapply(rv$data, is.numeric)],
                      options = pickerOptions(style = "btn-outline-primary", size = "sm")),
          numericInput("alpha_two", "Significance Level (alpha):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
          radioButtons("tail_type_two", "Tail Type:",
                       choices = c("Two-tailed" = "two.sided", "One-tailed (lower)" = "less", "One-tailed (upper)" = "greater"),
                       selected = "two.sided")
        ),
        conditionalPanel(
          condition = "input.hyp_test == 'Paired T-Test'",
          pickerInput("paired_var1", "First Paired Variable:",
                      choices = names(rv$data)[sapply(rv$data, is.numeric)],
                      options = pickerOptions(style = "btn-outline-primary", size = "sm")),
          pickerInput("paired_var2", "Second Paired Variable:",
                      choices = names(rv$data)[sapply(rv$data, is.numeric)],
                      options = pickerOptions(style = "btn-outline-primary", size = "sm")),
          numericInput("alpha_paired", "Significance Level (alpha):", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
          radioButtons("tail_type_paired", "Tail Type:",
                       choices = c("Two-tailed" = "two.sided", "One-tailed (lower)" = "less", "One-tailed (upper)" = "greater"),
                       selected = "two.sided")
        )
      )
    } else if (input$test_type == "Regresi") {
      tagList(
        radioGroupButtons("regresi_jenis", "Regression Type:",
                          choices = c("Simple Linear" = "sederhana",
                                      "Multiple Linear" = "berganda"),
                          status = "primary", size = "sm"),
        br(),
        pickerInput("regresi_y", "Y Variable (Response):",
                    choices = names(rv$data)[sapply(rv$data, is.numeric)],
                    options = pickerOptions(style = "btn-outline-primary", size = "sm")),
        uiOutput("regresi_x_ui"),
        materialSwitch("show_diagnostic", "Show Diagnostic Plots",
                       status = "primary", value = FALSE)
      )
    } else if (input$test_type == "ANOVA") {
      tagList(
        pickerInput("anova_dep", "Dependent Variable:",
                    choices = names(rv$data)[sapply(rv$data, is.numeric)],
                    options = pickerOptions(style = "btn-outline-primary", size = "sm")),
        pickerInput("anova_factor1", "First Factor:",
                    choices = names(rv$data)[sapply(rv$data, is.factor) | sapply(rv$data, is.character)],
                    options = pickerOptions(style = "btn-outline-primary", size = "sm")),
        conditionalPanel(
          condition = "input.anova_type == 'Two-Way ANOVA'",
          pickerInput("anova_factor2", "Second Factor:",
                      choices = names(rv$data)[sapply(rv$data, is.factor) | sapply(rv$data, is.character)],
                      options = pickerOptions(style = "btn-outline-primary", size = "sm"))
        ),
        pickerInput("anova_type", "ANOVA Type:",
                    choices = c("One-Way ANOVA", "Two-Way ANOVA", "Tukey Test"),
                    options = pickerOptions(style = "btn-outline-primary", size = "sm"))
      )
    } else if (input$test_type == "Uji Kesamaan Ragam") {
      tagList(
        pickerInput("var_numeric", "Select Numeric Variable:",
                    choices = names(rv$data)[sapply(rv$data, is.numeric)],
                    options = pickerOptions(style = "btn-outline-primary", size = "sm")),
        pickerInput("var_group", "Select Grouping Variable:",
                    choices = names(rv$data)[sapply(rv$data, is.factor) | sapply(rv$data, is.character)],
                    options = pickerOptions(style = "btn-outline-primary", size = "sm")),
        pickerInput("var_test", "Test Type:",
                    choices = c("Bartlett", "Levene"),
                    options = pickerOptions(style = "btn-outline-primary", size = "sm"))
      )
    }
  })
  
  # Dynamic UI for regression X variables
  output$regresi_x_ui <- renderUI({
    if (input$regresi_jenis == "sederhana") {
      pickerInput("regresi_x", "X Variable (Predictor):",
                  choices = names(rv$data)[sapply(rv$data, is.numeric)],
                  options = pickerOptions(style = "btn-outline-primary", size = "sm"))
    } else {
      pickerInput("regresi_x", "X Variables (Predictors):",
                  choices = names(rv$data)[sapply(rv$data, is.numeric)],
                  multiple = TRUE,
                  options = pickerOptions(style = "btn-outline-primary", size = "sm"))
    }
  })
  
  # Inferential statistics
  test_result <- eventReactive(input$calc_inf, {
    validate(
      need(input$test_type, "Please select a test type first.")
    )
    withProgress(message = 'Running Test...', value = 0.5, {
      dat <- filtered_data()
      validate(
        need(nrow(na.omit(dat)) > 0, "No valid data for analysis.")
      )
      
      if (input$test_type == "Uji Normalitas") {
        validate(
          need(input$norm_var, "Please select a variable for normality test."),
          need(input$norm_var %in% colnames(dat), "Variable not found in data.")
        )
        var <- dat[[input$norm_var]]
        validate(
          need(sum(!is.na(var)) > 3, "Not enough data for normality test (minimum 4 valid observations).")
        )
        list(
          Shapiro_Wilk = shapiro.test(var[!is.na(var)]),
          Liliefors = lillie.test(var[!is.na(var)])
        )
      } else if (input$test_type == "Uji Hipotesis") {
        validate(
          need(input$hyp_test, "Please select a hypothesis test type.")
        )
        if (input$hyp_test == "One Sample T-Test") {
          validate(
            need(input$one_sample_var, "Please select a variable for one-sample t-test."),
            need(input$one_sample_var %in% colnames(dat), "Selected variable not found in data."),
            need(sum(!is.na(dat[[input$one_sample_var]])) > 1, "Not enough data for t-test."),
            need(!is.null(input$hyp_value), "Please specify a hypothesized mean."),
            need(!is.null(input$alpha), "Please specify a significance level.")
          )
          var <- dat[[input$one_sample_var]]
          t.test(var[!is.na(var)], mu = input$hyp_value, alternative = input$tail_type, conf.level = 1 - input$alpha)
        } else if (input$hyp_test == "Two Sample T-Test") {
          validate(
            need(input$hyp_var1, "Please select the first variable."),
            need(input$hyp_var2, "Please select the second variable."),
            need(input$hyp_var1 %in% colnames(dat), "First variable not found in data."),
            need(input$hyp_var2 %in% colnames(dat), "Second variable not found in data."),
            need(sum(!is.na(dat[[input$hyp_var1]])) > 1, "Not enough data for first variable."),
            need(sum(!is.na(dat[[input$hyp_var2]])) > 1, "Not enough data for second variable."),
            need(!is.null(input$alpha_two), "Please specify a significance level.")
          )
          t.test(dat[[input$hyp_var1]], dat[[input$hyp_var2]], paired = FALSE, alternative = input$tail_type_two, conf.level = 1 - input$alpha_two)
        } else if (input$hyp_test == "Paired T-Test") {
          validate(
            need(input$paired_var1, "Please select the first paired variable."),
            need(input$paired_var2, "Please select the second paired variable."),
            need(input$paired_var1 %in% colnames(dat), "First paired variable not found in data."),
            need(input$paired_var2 %in% colnames(dat), "Second paired variable not found in data."),
            need(length(na.omit(dat[[input$paired_var1]])) == length(na.omit(dat[[input$paired_var2]])), "Paired variables must have equal length."),
            need(sum(!is.na(dat[[input$paired_var1]])) > 1, "Not enough data for paired test."),
            need(!is.null(input$alpha_paired), "Please specify a significance level.")
          )
          t.test(dat[[input$paired_var1]], dat[[input$paired_var2]], paired = TRUE, alternative = input$tail_type_paired, conf.level = 1 - input$alpha_paired)
        }
      } else if (input$test_type == "Regresi") {
        validate(
          need(input$regresi_y, "Please select a Y variable."),
          need(input$regresi_y %in% colnames(dat), "Y variable not found."),
          need(input$regresi_x, "Please select X variable(s)."),
          need(all(input$regresi_x %in% colnames(dat)), "One or more X variables not found.")
        )
        dat <- na.omit(dat[, c(input$regresi_y, input$regresi_x)])
        validate(
          need(nrow(dat) > length(input$regresi_x) + 1, "Not enough data for regression.")
        )
        if (input$regresi_jenis == "sederhana") {
          model <- lm(as.formula(paste(input$regresi_y, "~", input$regresi_x)), data = dat)
        } else {
          model <- lm(as.formula(paste(input$regresi_y, "~", paste(input$regresi_x, collapse = "+"))), data = dat)
        }
        list(
          Summary = summary(model),
          VIF = if (input$regresi_jenis == "berganda") car::vif(model) else NULL
        )
      } else if (input$test_type == "ANOVA") {
        validate(
          need(input$anova_dep, "Please select a dependent variable."),
          need(input$anova_factor1, "Please select a first factor."),
          need(input$anova_dep %in% colnames(dat), "Dependent variable not found."),
          need(input$anova_factor1 %in% colnames(dat), "First factor not found.")
        )
        if (input$anova_type == "One-Way ANOVA") {
          model <- aov(as.formula(paste(input$anova_dep, "~", input$anova_factor1)), data = dat)
          summary(model)
        } else if (input$anova_type == "Two-Way ANOVA") {
          validate(
            need(input$anova_factor2, "Please select a second factor for Two-Way ANOVA."),
            need(input$anova_factor2 %in% colnames(dat), "Second factor not found.")
          )
          model <- aov(as.formula(paste(input$anova_dep, "~", input$anova_factor1, "*", input$anova_factor2)), data = dat)
          summary(model)
        } else {
          model <- aov(as.formula(paste(input$anova_dep, "~", input$anova_factor1)), data = dat)
          TukeyHSD(model)
        }
      } else if (input$test_type == "Uji Kesamaan Ragam") {
        validate(
          need(input$var_numeric, "Please select a numeric variable."),
          need(input$var_group, "Please select a grouping variable."),
          need(input$var_numeric %in% colnames(dat), "Numeric variable not found in data."),
          need(input$var_group %in% colnames(dat), "Grouping variable not found in data."),
          need(is.numeric(dat[[input$var_numeric]]), "Selected numeric variable is not numeric."),
          need(is.factor(dat[[input$var_group]]) || is.character(dat[[input$var_group]]), "Grouping variable must be categorical."),
          need(length(unique(dat[[input$var_group]])) > 1, "Grouping variable must have multiple levels.")
        )
        dat[[input$var_group]] <- as.factor(dat[[input$var_group]])
        if (input$var_test == "Bartlett") {
          bartlett.test(as.formula(paste(input$var_numeric, "~", input$var_group)), data = dat)
        } else {
          leveneTest(as.formula(paste(input$var_numeric, "~", input$var_group)), data = dat)
        }
      } else {
        "Please select a test to view results."
      }
    })
  })
  
  output$test_result <- renderPrint({
    test_result()
  })
  
  # Automatic interpretation
  output$test_interpretation <- renderPrint({
    req(input$calc_inf)
    result <- test_result()
    if (is.null(result) || length(result) == 0) {
      return("No test results available for interpretation.")
    }
    
    tryCatch({
      if (input$test_type == "Uji Normalitas") {
        validate(
          need(!is.null(result$Shapiro_Wilk), "Shapiro-Wilk test results not available"),
          need(!is.null(result$Shapiro_Wilk$p.value), "P-value not available in Shapiro-Wilk test")
        )
        p_val <- result$Shapiro_Wilk$p.value
        alpha <- 0.05  # Default alpha for normality test
        if (is.na(p_val)) {
          "Could not calculate p-value for normality test."
        } else if (p_val < alpha) {
          paste("Interpretation: Data is not normally distributed (p-value =", format.pval(p_val, digits = 3), "<", alpha, ").")
        } else {
          paste("Interpretation: Data is normally distributed (p-value =", format.pval(p_val, digits = 3), ">=", alpha, ").")
        }
      } else if (input$test_type == "Uji Hipotesis") {
        validate(
          need(!is.null(result$p.value), "P-value not available in test results")
        )
        p_val <- result$p.value
        alpha <- if (input$hyp_test == "One Sample T-Test") input$alpha else if (input$hyp_test == "Two Sample T-Test") input$alpha_two else input$alpha_paired
        if (is.na(p_val)) {
          "Could not calculate p-value for hypothesis test."
        } else if (p_val < alpha) {
          paste("Interpretation: Reject the null hypothesis (p-value =", format.pval(p_val, digits = 3), "<", alpha, ").")
        } else {
          paste("Interpretation: Fail to reject the null hypothesis (p-value =", format.pval(p_val, digits = 3), ">=", alpha, ").")
        }
      } else if (input$test_type == "Regresi") {
        validate(
          need(!is.null(result$Summary), "Regression summary not available"),
          need(!is.null(result$Summary$coefficients), "Coefficients table not available")
        )
        coef_table <- result$Summary$coefficients
        if (nrow(coef_table) < 2) {
          "Not enough coefficients for regression interpretation."
        } else {
          p_val <- coef_table[2, 4]  # p-value for the first predictor
          alpha <- 0.05  # Default alpha for regression
          if (is.na(p_val)) {
            "Could not calculate p-value for regression coefficient."
          } else if (p_val < alpha) {
            paste("Interpretation: There is a significant relationship between predictor(s) and response (p-value =", format.pval(p_val, digits = 3), "<", alpha, ").")
          } else {
            paste("Interpretation: There is no significant relationship between predictor(s) and response (p-value =", format.pval(p_val, digits = 3), ">=", alpha, ").")
          }
        }
      } else if (input$test_type == "ANOVA") {
        validate(
          need(!is.null(result[[1]]), "ANOVA table not available"),
          need(!is.null(result[[1]]$'Pr(>F)'), "P-values not available in ANOVA table")
        )
        anova_table <- result[[1]]
        p_val <- anova_table$'Pr(>F)'[1]  # p-value for main effect
        alpha <- 0.05  # Default alpha for ANOVA
        if (is.na(p_val)) {
          "Could not calculate p-value for ANOVA test."
        } else if (p_val < alpha) {
          paste("Interpretation: There is a significant difference between groups (p-value =", format.pval(p_val, digits = 3), "<", alpha, ").")
        } else {
          paste("Interpretation: There is no significant difference between groups (p-value =", format.pval(p_val, digits = 3), ">=", alpha, ").")
        }
      } else if (input$test_type == "Uji Kesamaan Ragam") {
        validate(
          need(!is.null(result), "Test results are empty")
        )
        p_val <- if (!is.null(result$p.value)) result$p.value else result$`Pr(>F)`[1]
        validate(
          need(!is.na(p_val), "Could not extract p-value from test results")
        )
        alpha <- 0.05  # Default alpha for variance test
        if (p_val < alpha) {
          paste("Interpretation: There is a significant difference in variances (p-value =", format.pval(p_val, digits = 3), "<", alpha, ").")
        } else {
          paste("Interpretation: There is no significant difference in variances (p-value =", format.pval(p_val, digits = 3), ">=", alpha, ").")
        }
      } else {
        "No interpretation available for the selected test type."
      }
    }, error = function(e) {
      paste("Error in generating interpretation:", e$message)
    })
  })
  
  # Test plot
  output$test_plot <- renderPlot({
    req(input$calc_inf)
    dat <- filtered_data()
    validate(
      need(!is.null(dat), "No data available for plotting."),
      need(nrow(na.omit(dat)) > 0, "No valid data available after removing NA values.")
    )
    
    tryCatch({
      if (input$test_type == "Regresi" && isTRUE(input$show_diagnostic)) {
        validate(
          need(input$regresi_y, "Please select a Y variable for regression."),
          need(input$regresi_x, "Please select X variable(s) for regression."),
          need(all(c(input$regresi_y, input$regresi_x) %in% colnames(dat)), "Selected variables not found in data.")
        )
        plot_dat <- na.omit(dat[, c(input$regresi_y, input$regresi_x), drop = FALSE])
        validate(
          need(nrow(plot_dat) > length(input$regresi_x) + 1, "Not enough observations for regression diagnostics.")
        )
        formula <- if (input$regresi_jenis == "sederhana") {
          as.formula(paste(input$regresi_y, "~", input$regresi_x))
        } else {
          as.formula(paste(input$regresi_y, "~", paste(input$regresi_x, collapse = "+")))
        }
        model <- lm(formula, data = plot_dat)
        par(mfrow = c(2, 2), oma = c(0, 0, 2, 0), mar = c(4, 4, 2, 1), bg = "#f8f9fa",
            col.axis = "#2c3e50", col.lab = "#2c3e50", fg = "#3498db")
        plot(model, which = 1:4, caption = "", pch = 16, cex = 1.2, col = "#3498db", id.n = 0,
             main = c("Residuals vs Fitted", "Normal Q-Q Plot", "Scale-Location", "Residuals vs Leverage"),
             cex.main = 1.1)
        abline(h = 0, col = "#e74c3c", lwd = 2)
        title(paste("Regression Diagnostic Plots for", input$regresi_y, "~",
                    ifelse(input$regresi_jenis == "sederhana", input$regresi_x, paste(input$regresi_x, collapse = "+"))),
              outer = TRUE, cex.main = 1.3, col.main = "#2c3e50")
      } else if (input$test_type == "ANOVA" && input$anova_type != "Tukey Test") {
        validate(
          need(input$anova_dep %in% colnames(dat), "Dependent variable not found."),
          need(input$anova_factor1 %in% colnames(dat), "First factor not found."),
          need(length(unique(dat[[input$anova_factor1]])) > 1, "First factor must have multiple levels.")
        )
        par(mfrow = c(1, 2), oma = c(0, 0, 2, 0), bg = "#f8f9fa", col.axis = "#2c3e50", col.lab = "#2c3e50")
        boxplot(as.formula(paste(input$anova_dep, "~", input$anova_factor1)), data = dat, col = "#3498db", border = "#2c3e50",
                main = paste("Boxplot by", input$anova_factor1), xlab = input$anova_factor1, ylab = input$anova_dep, cex.main = 1.1)
        model <- aov(as.formula(paste(input$anova_dep, "~", input$anova_factor1)), data = dat)
        plot(fitted(model), residuals(model), pch = 16, col = "#3498db",
             main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", cex.main = 1.1)
        abline(h = 0, col = "#e74c3c", lwd = 2)
        title("ANOVA Diagnostic Plots", outer = TRUE, cex.main = 1.3, col.main = "#2c3e50")
      }
    }, error = function(e) {
      par(mar = c(1, 1, 1, 1), bg = "#f8f9fa")
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error generating plot:\n", e$message), col = "#e74c3c", cex = 1.2)
      box(col = "#3498db")
    })
  })
  
  # Main plot for Visualizations tab
  output$main_plot <- renderPlotly({
    req(input$show_plot)
    dat <- filtered_data()
    validate(
      need(!is.null(dat), "No data available for plotting."),
      need(nrow(na.omit(dat)) > 0, "No valid data available after removing NA values.")
    )
    
    tryCatch({
      if (input$plot_type == "Scatter") {
        validate(
          need(input$var_x %in% colnames(dat), "X variable not found in data."),
          need(input$var_y %in% colnames(dat), "Y variable not found in data.")
        )
        p <- ggplot(na.omit(dat), aes_string(x = input$var_x, y = input$var_y)) +
          geom_point(color = "#3498db", size = 2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
        if (input$show_regression) {
          p <- p + geom_smooth(method = "lm", se = FALSE, color = "#e74c3c")
        }
        ggplotly(p)
      } else if (input$plot_type == "Box") {
        validate(
          need(input$var_y %in% colnames(dat), "Y variable not found in data."),
          need(any(sapply(dat, is.factor) | sapply(dat, is.character)), "No categorical variable available for grouping.")
        )
        x_var <- names(dat)[sapply(dat, is.factor) | sapply(dat, is.character)][1]
        p <- ggplot(dat, aes_string(x = x_var, y = input$var_y)) +
          geom_boxplot(fill = "#3498db", color = "#2c3e50") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
          labs(title = paste("Box Plot by", x_var))
        ggplotly(p)
      } else if (input$plot_type == "Histogram") {
        validate(
          need(input$var_y %in% colnames(dat), "Y variable not found in data.")
        )
        p <- ggplot(na.omit(dat), aes_string(x = input$var_y)) +
          geom_histogram(bins = 30, fill = "#3498db", color = "#2c3e50") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
          labs(title = "Histogram")
        ggplotly(p)
      } else if (input$plot_type == "QQ Plot") {
        validate(
          need(input$var_y %in% colnames(dat), "Y variable not found in data.")
        )
        p <- ggplot(na.omit(dat), aes(sample = .data[[input$var_y]])) +
          stat_qq(color = "#3498db") +
          stat_qq_line(color = "#e74c3c") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
          labs(title = "QQ Plot")
        ggplotly(p)
      }
    }, error = function(e) {
      plot_ly() %>% layout(title = list(text = paste("Error generating plot:", e$message),
                                        font = list(color = "#e74c3c")))
    })
  })
}

# Run the app
shinyApp(ui, server)
