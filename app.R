library(shiny)
library(shiny.router)
library(tidyverse)
library(DT)

options(shiny.maxRequestSize = 1000 * 1024^2)

home_page <- div(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
  DT::DTOutput("files"),
  tableOutput("selected_file_table")
)

data_analysis_page <- div(
  titlePanel("Data analysis"),
  "Shiny is available on CRAN, so you can install it in the usual way from your R console:",
    selectInput(
      "var",
      label = "Choose an option",
      choices = 
        list(
          "Branch wise transactions", 
          "Type wise transactions", 
          "Caetgory wise transactions",
          "Monthly transactions",
          "Frequency of non zero fines",
          "Monthly Transactions for branch"
        ),
      selected = "Percent White"
    ),
  uiOutput("SecondSelect"),
  actionButton("save_btn","Save"),
  div(
    class = "card",
    div(
      class = "card-header",
      "Introducing Shiny"
    ),
    div(
      class = "card-body",
      plotOutput("plot")
    ),
    div(
      class = "card-footer",
      "Shiny is a product of Posit."
    )
  )
)

router <- make_router(
  route("/", home_page),
  route("contact", data_analysis_page)
)

ui <- fluidPage(
  tags$ul(
    tags$li(a(href = route_link("/"), "Dashboard")),
    tags$li(a(href = route_link("contact"), "Data Analysis"))
  ),
  router$ui
)

source("C:/Users/sarth/OneDrive/Documents/SY-SEM2/DS/DS_dashboard/circ_preprocessing.R")
source("C:/Users/sarth/OneDrive/Documents/SY-SEM2/DS/DS_dashboard/borr_preprocessing.R")
source("C:/Users/sarth/OneDrive/Documents/SY-SEM2/DS/DS_dashboard/data_visualization.R")

server <- function(input, output, session) {
  router$server(input, output, session)
  
  current_route <- reactive({
    session$clientData$url_hash
  })
  
  output$files <- DT::renderDT({
    DT::datatable(input$upload, selection = c("single"))
  })
  
  all_files <- reactive({
    req(input$upload)
    purrr::map(input$upload$datapath, read_csv) %>%
      purrr::set_names(input$upload$name)
  })
  
  circ_df_processed <- reactiveVal(NULL)
  borr_df_processed <- reactiveVal(NULL)
  is_done <- reactiveVal(FALSE)

  output$selected_file_table <- renderTable({
    req(input$files_rows_selected)
    
    selected_file_name <- input$upload$name[input$files_rows_selected]
    
    if (selected_file_name == "Circ.csv") {
      circ_df_processed() %>% head()
    } else if (selected_file_name == "BorrowersTransactions.csv") {
      borr_df_processed() %>% head()
    }
  })
  
  observe({
    route <- current_route()
    print(paste("Current Route:", route))
    #print("In preprocessing")
    if (route == "#!/" && !is_done()) {
      print("In preprocessing")
      files_list <- all_files()
      
      circ_df <- files_list[["Circ.csv"]]
      borr_df <- files_list[["BorrowersTransactions.csv"]]
      
      circ_df <- circ_preprocessing(circ_df)
      borr_df <- borr_preprocessing(borr_df, circ_df)
      
      circ_df_processed(circ_df)
      borr_df_processed(borr_df)
      
      is_done(TRUE)
    }
    
    if (route == "#!/contact") {
      borr_df_processed(filter(borr_df_processed()))
      output$SecondSelect <- renderUI({
        if (input$var == "Monthly Transactions for branch") {
          selectInput(
            "secondVar",
            label = "Choose a branch",
            choices = list("COMP",
                           "MECH",
                           "CHEM",
                           "DESH",
                           "IC",
                           "ELEX",
                           "IT",
                           "DOME",
                           "CSAIML",
                           "AIDS"),
            selected = "COMP"
          )
        } else {
          NULL
        }
      })
      
      observeEvent(input$save_btn, {
        print("inside button")
        selected_var <- input$var
        plot <- NULL
        if (!is.null(input$secondVar)){
          selected_var2 <- input$secondVar
          print(selected_var2)
          plot <- branch_month(borr_df_processed(),selected_var2)
        }
        else{
          if(selected_var == "Branch wise transactions") {
            plot <- tran_branch(borr_df_processed())
          }
          else if(selected_var == "Type wise transactions") {
            plot <- tran_type(borr_df_processed())
          }
          else if(selected_var == "Caetgory wise transactions") {
            plot <- tran_category(borr_df_processed())
          }
          else if(selected_var == "Monthly transactions") {
            plot <- freq_month(borr_df_processed())
          }
          else if(selected_var == "Frequency of non zero fines") {
            plot <- zero_fine_branch(borr_df_processed())
          }
        }
        
        output$plot <- renderPlot({
          plot
        })
        
        
        output$myImage <- renderImage({
          outfile <- tempfile(fileext = '.png')

          png(outfile, 
              width = 200*8, 
              height = 200*8,
              res = 100*8)
          print(plot)
          dev.off()

          list(src = outfile,
               contentType = 'image/png',
               width = 200,
               height = 200,
               alt = "This is alternate text")
        }, deleteFile = TRUE)
        
      })
    }
  })
  
}

shinyApp(ui, server)