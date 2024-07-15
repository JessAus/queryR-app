# Load required packages -------------------------------------------------------
pacman::p_load(tidyverse, DBI, shiny, shinydashboard, shinythemes, shinyWidgets,
               DT, pool, fontawesome, bslib, echarts4r, RPostgres, lubridate,
               stringr, fresh, shinyalert)

# Create theme for app ---------------------------------------------------------
myTheme = create_theme(
    theme = "sandstone",
    bs_vars_global(
      body_bg = "rgb(255, 248, 232)",
      text_color = "black"
    ),
    bs_vars_button(
      default_bg = "rgba(105, 137, 150, 0.9)",
      default_color = "rgb(255, 248, 232)"
    )
  )



  # Define UI for application ----------------------------------------------------
  ui <- function () {

    dashboardPage(
      ## Create title and make header look like a dashboard layout -----------------
      title = "Database QueryR",

      header = dashboardHeader(
        tags$li(
          class = "dropdown",
          tags$style(
            ".main-header {max-height: 100px;
                        font-size:24px;
                        font-weight:bold;
                        line-height:24px;}"),
          tags$style(
            ".main-header .logo {height: 100px;
                               font-size:24px;
                               font-weight:bold;
                               line-height:24px;}"
          )
        ),
        title = HTML(
          "<div style = 'background-color: #407076; vertical-align:middle'>
          <i class='fa-solid fa-database' style='color: #ffffff;'></i>  QueryR
         </div>"),
        titleWidth = "95%"
      ),

      ## Define elements in Sidebar ------------------------------------------------
      dashboardSidebar(
        width = "300px",
        sidebarMenu(id = "sidebarmenu",
                    tags$style(
                      ".main-sidebar {float:top; margin-top:40px; padding-left:15px; padding-right:15px}"
                    ),

                    br(),

                    ### Tab 1 - Patient Record Lookup ------------------------------
                    menuItem("Patient Record Lookup", tabName = "personLookup", icon = icon("person", library = "fa")),
                    ### Tab 2 - Disease Reports/Case Counts ------------------------
                    menuItem("Disease Report Metrics", tabName = "diseaseLookup", icon = icon("viruses", library = "fa")),
                    ### Tab 3 - Investigator Metrics -------------------------------
                    menuItem("Investigator Metrics", tabName = "invMetrics", icon = icon("briefcase", library = "fa")),
                    ### Tab 4 - Full Data Lookup -----------------------------------
                    menuItem("Full Case Data Lookup", tabName = "fullLook", icon = icon("magnifying-glass", library = "fa"))

        )
      ),

      ## Define appearance for body of the dashboard and apply theme ---------------
      dashboardBody(

        use_theme(myTheme),

        tags$head(
          tags$style(
            HTML(
              ".skin-blue .main-header .navbar {background-color: #407076}
           .skin-blue .main-header .navbar .sidebar-toggle:hover {background-color: #1C2541; padding-bottom: 61px; vertical-align: middle;}
           .skin-blue .main-header .logo {background-color: #407076;border-bottom: 0 solid transparent;}
           .content-wrapper {float:top;}
           .content-wrapper .content {padding-left:40px; padding-right:40px;}
           .wrapper {overflow-y: hidden;}
.main-header {
  max-height: 200px !important;
}
.sidebar-toggle {
  float: right !important;
}

.main-header .logo {
  line-height: 100px !important;
  padding: 0px 0px;
}

hr {border-top: 1px solid #23513C;}

.content-wrapper, .right-side {
background-color: #FFF8E8;
}"
            )
          )
        ),

        ### Define contents of each of the tabs/subtabs created in UI portion ------
        tabItems(
          #### Tab 1 Contents - Patient Record Lookup ------------------------------
          tabItem(
            tabName = "personLookup",
            fluidRow(
              tabsetPanel(
                ##### Tabset panel to select lookup style --------------------------
                tabPanel("Select Lookup Method:",
                         br(),
                         radioButtons(inputId = "LookupMethod",
                                      label = "Lookup Method:",
                                      choices = c("Individual Lookup (Name+DOB)",
                                                  "Multi-person Lookup (Person ID)",
                                                  "Query All"))
                ),
                ##### Tabset panel for inputs - conditional on selected method -----
                tabPanel("User Inputs:",
                         ###### Individual Lookup method ---------------------------
                         conditionalPanel(
                           condition = "input.LookupMethod == 'Individual Lookup (Name+DOB)'",
                           br(),
                           br(),
                           strong("User Instructions:"),
                           em("Please either enter a value for ALL provided inputs before pressing the 'Load/Refresh Query' button on this tab or navigate back to the previous tab to select a new lookup method. Please note that every time you enter details for a new individual within the provide input boxes, you will need to press the button again for the output table to reflect those changes."),
                           br(),
                           br(),
                           hr(),
                           column(3,
                                  textInput("patient_fn", "Patient Full Name:")
                           ),
                           column(6,
                                  dateRangeInput("dob", "Date of Birth (Range):"),
                                  tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                                  br(),
                                  actionButton("query_go_people1", "Load Query/Refresh", icon = icon("arrows-rotate", library = "fa"))
                           )
                         ),
                         ###### Multi-person Lookup method -------------------------
                         conditionalPanel(
                           condition = "input.LookupMethod == 'Multi-person Lookup (Person ID)'",
                           br(),
                           br(),
                           strong("User Instructions:"),
                           em("Please provide a list of one or more patient IDs by selecting them from the drop-down and/or using the search bar within the provided drop-down window to add a new selection. Multiple or single person IDs can be selected at a time using this method. When you are ready, load or refresh the data table by pressing the 'Load/Refresh Query' button. Please note that every time you make changes to your person ID selection(s), you will need to press the button again for the output table to reflect those changes."),
                           br(),
                           br(),
                           hr(),
                           column(3,
                                  uiOutput('ids_list')),
                           column(6,
                                  br(),
                                  actionButton("query_go_people2", "Load Query/Refresh", icon = icon("arrows-rotate", library = "fa"))
                           )
                         ),
                         ###### Query All method -----------------------------------
                         conditionalPanel(
                           condition = "input.LookupMethod == 'Query All'",
                           br(),
                           br(),
                           strong("User Instructions:"),
                           em("To query all patient data, please press the 'Load Query/Refresh' button and narrow down your data from there as needed by using the filter boxes at the top of each column within the output table. Data can be downloaded using the 'Download Formats' drop-down. Simply click the drop-down and select the output format you desire. Please note that any filtering applied to the table will be reflected in the downloaded data."),
                           br(),
                           br(),
                           hr(),
                           actionButton("query_go_people3", "Load Query/Refresh", icon = icon("arrows-rotate", library = "fa"))
                         )
                )
              )
            ),

            ##### Display results of conditional query for Tab 1 -------------------
            fluidRow(
              hr(),
              h2("Query Results:"),
              DTOutput("personLookup_table")
            )
          ),

          #### Tab 2 Contents - Disease Reports/Case Counts ------------------------
          tabItem(
            tabName = "diseaseLookup",
            fluidRow(
              tabsetPanel(
                tabPanel("Select Output:",
                         br(),
                         radioButtons(inputId = "diseaseOutput",
                                      label = "Output As:",
                                      choices = c("Table Summary",
                                                  "Case Counts",
                                                  "Demographics by Disease"))

                ),
                ##### Tabset panel for inputs - conditional on selected output -----
                tabPanel("User Inputs:",
                         ###### Bar Chart -------------------------------------
                         conditionalPanel(condition = "input.diseaseOutput == 'Case Counts'",
                                          br(),
                                          br(),
                                          strong("User Instructions:"),
                                          em("To query disease data, please select a disease from the provided drop-down below, then press the 'Load Query/Refresh' button. Only one disease may be selected at a time. If you would like to view case counts for another disease, please make those changes in the provided drop-down and press the button again to refresh the visual."),
                                          br(),
                                          br(),
                                          hr(),
                                          column(3,
                                                 uiOutput('diseases_list')),
                                          column(6,
                                                 br(),
                                                 actionButton("query_go_disease1", "Load Query/Refresh", icon = icon("arrows-rotate", library = "fa")))
                         ),
                         conditionalPanel(condition = "input.diseaseOutput == 'Table Summary'",
                                          br(),
                                          br(),
                                          strong("User Instructions:"),
                                          em("To query disease data, please press the 'Load Query/Refresh' button. By default, all diseases are included within the table summary data. However, filters at the top of each column can be used to narrow down the table data before downloading, as needed. Data can be downloaded using the 'Download Formats' drop-down. Simply click the drop-down and select the output format you desire. Please note that any filtering applied to the table will be reflected in the downloaded data."),
                                          br(),
                                          br(),
                                          hr(),
                                          actionButton("query_go_disease2", "Load Query/Refresh", icon = icon("arrows-rotate", library = "fa"))
                         ),
                         conditionalPanel(condition = "input.diseaseOutput == 'Demographics by Disease'",
                                          br(),
                                          br(),
                                          strong("User Instructions:"),
                                          em("To query disease data, please select a disease from the provided drop-down below, then press the 'Load Query/Refresh' button. Only one disease may be selected at a time. If you would like to view demographic details for another disease, please make those changes in the provided drop-down and press the button again to refresh the visuals."),
                                          br(),
                                          br(),
                                          hr(),
                                          column(3,
                                                 uiOutput('diseases_list2')),
                                          column(6,
                                                 br(),
                                                 actionButton("query_go_disease3", "Load Query/Refresh", icon = icon("arrows-rotate", library = "fa")))
                         )
                )
              )
            ),

            ##### Display results of conditional query for Tab 2 -------------------
            fluidRow(
              hr(),
              h2("Query Results:"),
              conditionalPanel(condition = "input.diseaseOutput == 'Table Summary'",
                               DTOutput("diseaseLookup_table")),
              conditionalPanel(condition = "input.diseaseOutput == 'Case Counts'",
                               echarts4rOutput("barChart_disease")),
              conditionalPanel(condition = "input.diseaseOutput == 'Demographics by Disease'",
                               br(),
                               column(6,
                                      echarts4rOutput("pieChart1"),
                                      echarts4rOutput("pieChart2")),
                               column(6,
                                      echarts4rOutput("pieChart3"),
                                      echarts4rOutput("pieChart4"))
              )
            )
          ),

          #### Tab 3 Contents - Investigator Metrics -------------------------------
          tabItem(
            tabName = "invMetrics",
            fluidRow(
              tabsetPanel(
                tabPanel("Select Output:",
                         br(),
                         radioButtons(inputId = "invOutput",
                                      label = "Output As:",
                                      choices = c("Table Summary",
                                                  "Investigations per Month",
                                                  "Investigations per Type"))

                ),
                tabPanel("User Inputs:",
                         br(),
                         br(),
                         strong('User Instructions:'),
                         em("Select an investigator the drop-down provided below (or use the 'Select All' option), then press the 'Load Query/Refresh' button to retrieve data."),
                         br(),
                         br(),
                         strong('For the Table Summary Output:'),
                         em("Data can be downloaded using the 'Download Formats' drop-down. Simply click the drop-down and select the output format you desire. Please note that any filtering applied to the table will be reflected in the downloaded data."),
                         br(),
                         br(),
                         hr(),
                         column(3,
                                uiOutput('investigators_list')
                         ),
                         column(6,
                                br(),
                                actionButton("query_go_inv", "Load Query/Refresh", icon = icon("arrows-rotate", library = "fa")))
                )
              )
            ),

            ##### Display results of conditional query for Tab 3 -------------------
            fluidRow(
              hr(),
              h2("Query Results:"),
              conditionalPanel(condition = "input.invOutput == 'Investigations per Month'",
                               echarts4rOutput('inv_per_mo')
              ),
              conditionalPanel(condition = "input.invOutput == 'Investigations per Type'",
                               echarts4rOutput('inv_per_type')
              ),
              conditionalPanel(condition = "input.invOutput == 'Table Summary'",
                               DTOutput("invMetrics_table")
              )
            )

          ),

          #### Tab 4 Contents - Full Data Lookup -----------------------------------
          tabItem(
            tabName = "fullLook",
            fluidRow(
              tabsetPanel(
                tabPanel("Query All Data:",
                         br(),
                         br(),
                         strong("Please Note:"),
                         em("The data displayed in this table is full CASE data. Not all person records are shown here since not all person records have associated case record(s) on file. Data can be downloaded using the 'Download Formats' drop-down. Simply click the drop-down and select the output format you desire. Please note that any filtering applied to the table will be reflected in the downloaded data."),
                         br(),
                         br(),
                         hr(),
                         actionButton("query_go_full", "Load Query/Refresh", icon = icon("arrows-rotate", library = "fa"))
                )
              )
            ),

            ##### Display results of conditional query for Tab 4 -------------------
            fluidRow(
              hr(),
              h2("Query Results:"),
              DTOutput("fullLook_table")
            )
          )
        )
      ))
  }

# Define server contents for application ---------------------------------------
server <- function(input, output, session) {

  ## Create login modal and warning for false login on startup -----------------

  ### Reactively gather login credentials to pass to DB ------------------------
  inputs <- reactiveValues(username = NULL, password = NULL, conn = NULL)

  #### Modal dialog box to gather user credentials -----------------------------
  login_modal <- modalDialog(
    title = "Please Enter Login Credentials",
    textInput('user', "User:", placeholder = "Username"),
    passwordInput('pass', "Password:", placeholder = "Password"),
    easyClose = FALSE,
    fade = FALSE,
    footer = tagList(
      actionButton("login", "Login")
    )
  )

  #### Show modal dialogue box for login pop-up at application startup ---------
  showModal(login_modal)

  #### Establish a connection with pool so it connects only as needed ----------
  observeEvent(input$login, {
    inputs$username <- input$user
    inputs$password <- input$pass
    inputs$conn <-  try(pool::dbPool(RPostgres::Postgres(),
                                     dbname = 'git_project',
                                     host = 'localhost',
                                     port = 5432,
                                     user = inputs$username,
                                     password = inputs$password))
    if("try-error" %in% class(inputs$conn)){
      showModal(modalDialog(paste(inputs$conn), footer = actionButton("warning_login","Close")))
    } else{
      removeModal()
      shinyalert("Important Note", "You have successfully logged in! While navigating the application, please note that all data within this application are completely fake and most of it has been generated via the charlatan package.", type = "success")
      
    }

  })

  #### Display warning if login is invalid and bring box back for retry --------
  observeEvent(input$warning_login,{
    showModal(login_modal)
  })

  ## Tab 1 Outputs -------------------------------------------------------------

  ### Query results for "Individual Lookup" option -----------------------------
  people_query1 <- eventReactive(input$query_go_people1, {
    #### Convert date inputs to date format to use in query --------------------
    dob1 <- as.Date(input$dob[1])

    dob2 <- as.Date(input$dob[2])

    #### Create the list of available inputs -----------------------------------
    input_fn <- input$patient_fn

    #### Get the data from db --------------------------------------------------
    dbGetQuery(inputs$conn, paste0("SELECT * FROM person_data WHERE dob >= '", dob1,
                                   "' AND dob <= '", dob2,
                                   "' AND name = '", input_fn,
                                   "' ;"))
  })

  ### Retrieve list of available person ids and create picker input ------------
  output$ids_list <- renderUI({
    #### Retrieve list of person ids from db -----------------------------------
    list_of_ids <- dbGetQuery(inputs$conn, "SELECT DISTINCT ON (person_id) person_id FROM person_data ;")
    #### Create picker drop-down -----------------------------------------------
    pickerInput('selectId',
                label = "Select Person ID(s):",
                choices = list_of_ids,
                options = list(`actions-box` = TRUE,
                               `live-search` = TRUE),
                multiple = TRUE)
  })

  ### Query results for "Multi-person Lookup" option ---------------------------
  people_query2 <- eventReactive(input$query_go_people2, {
    #### Create the list of available inputs -----------------------------------
    input_ids <- input$selectId

    #### Get the data from db --------------------------------------------------
    dbGetQuery(inputs$conn, paste0("SELECT * FROM person_data WHERE person_id IN ('", paste(input_ids, collapse = "','"),
                                   "') ;"))
  })

  ### Query results for "Query All" option -------------------------------------
  people_query3 <- eventReactive(input$query_go_people3, {

    #### Get the data from db --------------------------------------------------
    dbGetQuery(inputs$conn, paste0("SELECT * FROM person_data ;"))
  })

  ### DT Table Output ----------------------------------------------------------
  output$personLookup_table <- renderDT(server = FALSE, {
    datatable(if (input$LookupMethod == "Individual Lookup (Name+DOB)" & !is.null(input$query_go_people1)) {
      people_query1()
    } else if (input$LookupMethod == "Multi-person Lookup (Person ID)" & !is.null(input$query_go_people2)) {
      people_query2()
    } else if (input$LookupMethod == "Query All" & !is.null(input$query_go_people3)) {
      people_query3()
    },
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    options = list(paging = TRUE,
                   scrollX=TRUE,
                   searching = TRUE,
                   ordering = TRUE,
                   dom = 'Bfrtip',
                   buttons =
                     list(list(
                       extend = "collection",
                       buttons = list(
                         list(extend = "csv", title = paste0("person_lookup_results_", Sys.Date())),
                         list(extend = "excel", title = paste0("person_lookup_results_", Sys.Date())),
                         list(extend = "pdf", title = paste0("person_lookup_results_", Sys.Date()))),
                       text = "Download Formats",
                       exportOptions = list(
                         modifier = list(
                           search = "applied"
                         )
                       )
                     )),
                   pageLength=10 ))
  })

  ## Tab 2 Outputs -------------------------------------------------------------

  ### Retrieve list of available diseases and create picker input --------------
  output$diseases_list <- renderUI({
    #### Retrieve list of diseases from db -------------------------------------
    list_of_dis <- dbGetQuery(inputs$conn, "SELECT DISTINCT ON (disease) disease FROM disease_data ;")
    #### Create picker drop-down -----------------------------------------------
    pickerInput('selectDisease',
                label = "Select Disease:",
                choices = list_of_dis,
                options = list(`actions-box` = TRUE,
                               `live-search` = TRUE),
                multiple = FALSE)
  })

  ### Retrieve list of available diseases and create picker input --------------
  output$diseases_list2 <- renderUI({
    #### Retrieve list of diseases from db -------------------------------------
    list_of_dis <- dbGetQuery(inputs$conn, "SELECT DISTINCT ON (disease) disease FROM disease_data ;")
    #### Create picker drop-down -----------------------------------------------
    pickerInput('selectDisease2',
                label = "Select Disease:",
                choices = list_of_dis,
                options = list(`actions-box` = TRUE,
                               `live-search` = TRUE),
                multiple = FALSE)
  })

  ### Query results for Disease query (bar chart) ------------------------------
  disease_query <- eventReactive(input$query_go_disease1, {
    #### Create the list of available inputs -----------------------------------
    diseaseInput <- input$selectDisease

    #### Get the data from db --------------------------------------------------
    dbGetQuery(inputs$conn, paste0("SELECT * FROM disease_data WHERE disease = '", diseaseInput, "';"))
  })

  ### Query results for Disease query (table) ----------------------------------
  disease_query_table <- eventReactive(input$query_go_disease2, {

    #### Get the data from db --------------------------------------------------
    dbGetQuery(inputs$conn, paste0("SELECT * FROM disease_data ;"))
  })

  ### Query results for Disease query (pie charts) -----------------------------
  disease_query_demo <- eventReactive(input$query_go_disease3, {
    #### Create the list of available inputs -----------------------------------
    diseaseInput2 <- input$selectDisease2

    #### Get the data from db --------------------------------------------------
    dbGetQuery(inputs$conn, paste0("SELECT * FROM disease_data
                                   NATURAL JOIN person_data
                                   WHERE disease = '", diseaseInput2, "';"))
  })

  ### DT Table Output ----------------------------------------------------------
  output$diseaseLookup_table <- renderDT(server = FALSE, {
    datatable( if (!is.null(input$query_go_disease2)) {
      disease_query_table()
    },
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    options = list(paging = TRUE,
                   scrollX=TRUE,
                   searching = TRUE,
                   ordering = TRUE,
                   dom = 'Bfrtip',
                   buttons =
                     list(list(
                       extend = "collection",
                       buttons = list(
                         list(extend = "csv", title = paste0("disease_lookup_results_", Sys.Date())),
                         list(extend = "excel", title = paste0("disease_lookup_results_", Sys.Date())),
                         list(extend = "pdf", title = paste0("disease_lookup_results_", Sys.Date()))),
                       text = "Download Formats",
                       exportOptions = list(
                         modifier = list(
                           search = "applied"
                         )
                       )
                     )),
                   pageLength=10 ))
  })

  ### Echarts Bar Chart Output -------------------------------------------------
  output$barChart_disease <- renderEcharts4r ({
    #### Create the list of available inputs -----------------------------------
    diseaseBarInput <- input$selectDisease

    #### Transform data as needed ----------------------------------------------
    graph_data <- disease_query()

    if (graph_data$disease[[1]] == diseaseBarInput) {
      graph_data <- graph_data %>%
        filter(disease %in% diseaseBarInput) %>%
        mutate(month = month(report_date, label = TRUE)) %>%
        mutate(year = year(report_date)) %>%
        mutate(month_year = paste0(month, "_", year)) %>%
        group_by(disease, month_year) %>%
        mutate(count = n()) %>%
        select(c(disease, month_year, count)) %>%
        distinct() %>%
        mutate(disease = str_trim(disease)) %>%
        ungroup()

      #### Define the order of the x-axis (time) ---------------------------------
      graph_data$month_year <- factor(graph_data$month_year,
                                      c("Jan_2019", "Feb_2019", "Mar_2019", "Apr_2019","May_2019","Jun_2019", "Jul_2019",
                                        "Aug_2019", "Sep_2019", "Oct_2019", "Nov_2019", "Dec_2019",
                                        "Jan_2020", "Feb_2020", "Mar_2020", "Apr_2020","May_2020","Jun_2020", "Jul_2020",
                                        "Aug_2020", "Sep_2020", "Oct_2020", "Nov_2020", "Dec_2020",
                                        "Jan_2021", "Feb_2021", "Mar_2021", "Apr_2021","May_2021","Jun_2021", "Jul_2021",
                                        "Aug_2021", "Sep_2021", "Oct_2021", "Nov_2021", "Dec_2021",
                                        "Jan_2022", "Feb_2022", "Mar_2022", "Apr_2022","May_2022","Jun_2022", "Jul_2022",
                                        "Aug_2022", "Sep_2022", "Oct_2022", "Nov_2022", "Dec_2022",
                                        "Jan_2023", "Feb_2023", "Mar_2023", "Apr_2023","May_2023","Jun_2023", "Jul_2023",
                                        "Aug_2023", "Sep_2023", "Oct_2023", "Nov_2023", "Dec_2023",
                                        "Jan_2024", "Feb_2024", "Mar_2024", "Apr_2024","May_2024","Jun_2024", "Jul_2024",
                                        "Aug_2024", "Sep_2024", "Oct_2024", "Nov_2024", "Dec_2024"), ordered = TRUE)

      #### Define start and end month/year based on user inputs ------------------
      max_month = substr(max(graph_data$month_year), start = 1, stop = 3)
      max_year = substr(max(graph_data$month_year), start = 5, stop = 8)

      min_month = substr(min(graph_data$month_year), start = 1, stop = 3)
      min_year = substr(min(graph_data$month_year), start = 5, stop = 8)

      #### Create color palette --------------------------------------------------
      bar_color = c("#330040")

      #### Create the bar chart --------------------------------------------------
      graph_data %>%
        group_by(disease) %>%
        arrange(month_year) %>%
        e_charts(month_year, reorder = FALSE) %>%
        e_bar(count, stack = 'grp') %>%
        e_tooltip(trigger = 'axis') %>%
        e_title(text = paste0("Case Counts of ", str_trim(diseaseBarInput), " by Date Reported"), subtext = paste0(min_month, " ", min_year ," - ", max_month, " ", max_year), left = 'center') %>%
        e_x_axis(name = 'Month', nameLocation = "middle", nameGap = 30) %>%
        e_y_axis(name = 'Case Count', nameLocation = "middle", nameGap = 60) %>%
        e_legend(show = FALSE) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_color(bar_color)
    } else if (graph_data$disease[[1]] != diseaseBarInput) {
      shinyalert("Oops!", "It looks like you changed the disease input without pressing the 'Load/Refresh Query' button. Please press this button again to view the refreshed visual. Thank you!", type = "info")
    }

  })

  ### Echarts Pie Chart 1 Output -----------------------------------------------
  output$pieChart1 <- renderEcharts4r ({
    #### Create the list of available inputs -----------------------------------
    diseasePieInput <- input$selectDisease2

    #### Transform data as needed ----------------------------------------------
    graph_data_pie1 <- disease_query_demo()

    if (graph_data_pie1$disease[[1]] == diseasePieInput) {
      graph_data_pie1 <- graph_data_pie1 %>%
        filter(disease %in% diseasePieInput) %>%
        mutate(disease = str_trim(disease)) %>%
        mutate(race = str_trim(race)) %>%
        group_by(race) %>%
        mutate(Count = n()) %>%
        select(race, Count) %>%
        distinct() %>%
        ungroup()

      #### Create color palette --------------------------------------------------
      colors1 = c("#330040", "#407076", "#AFE1E7", "#6B6581", "#ADA9BB")

      #### Create the pie chart -------------------------------------------------
      graph_data_pie1 %>%
        e_charts(race) %>%
        e_pie(Count, radius = c("50%", "70%")) %>%
        e_tooltip() %>%
        e_title(text = paste0(str_trim(diseasePieInput), " Case Counts by Race"), left = 'left') %>%
        e_legend(show = FALSE) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_color(colors1)
    } else if (graph_data_pie1$disease[[1]] != diseasePieInput) {

    }

  })

  ### Echarts Pie Chart 2 Output -----------------------------------------------
  output$pieChart2 <- renderEcharts4r ({
    #### Create the list of available inputs -----------------------------------
    diseasePieInput2 <- input$selectDisease2

    #### Transform data as needed ----------------------------------------------
    graph_data_pie2 <- disease_query_demo()

    if (graph_data_pie2$disease[[1]] == diseasePieInput2) {
      graph_data_pie2 <- graph_data_pie2 %>%
        filter(disease %in% diseasePieInput2) %>%
        mutate(disease = str_trim(disease)) %>%
        mutate(gender = str_trim(gender)) %>%
        group_by(gender) %>%
        mutate(Count = n()) %>%
        select(gender, Count) %>%
        distinct() %>%
        ungroup()

      #### Create color palette --------------------------------------------------
      colors2 = c("#330040", "#B7BBDD")

      #### Create the pie chart -------------------------------------------------
      graph_data_pie2 %>%
        e_charts(gender) %>%
        e_pie(Count, radius = c("50%", "70%")) %>%
        e_tooltip() %>%
        e_title(text = paste0(str_trim(diseasePieInput2), " Case Counts by Gender"), left = 'left') %>%
        e_legend(show = FALSE) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_color(colors2)
    } else if (graph_data_pie2$disease[[1]] != diseasePieInput2) {

    }

  })

  ### Echarts Pie Chart 3 Output -----------------------------------------------
  output$pieChart3 <- renderEcharts4r ({
    #### Create the list of available inputs -----------------------------------
    diseasePieInput3 <- input$selectDisease2

    #### Transform data as needed ----------------------------------------------
    graph_data_pie3 <- disease_query_demo()

    if (graph_data_pie3$disease[[1]] == diseasePieInput3) {
      graph_data_pie3 <- graph_data_pie3 %>%
        filter(disease %in% diseasePieInput3) %>%
        mutate(disease = str_trim(disease)) %>%
        mutate(gender = str_trim(ethnicity)) %>%
        group_by(ethnicity) %>%
        mutate(Count = n()) %>%
        select(ethnicity, Count) %>%
        distinct() %>%
        ungroup()

      #### Create color palette --------------------------------------------------
      colors3 = c("#330040", "#B7BBDD")

      #### Create the pie chart -------------------------------------------------
      graph_data_pie3 %>%
        e_charts(ethnicity) %>%
        e_pie(Count, radius = c("50%", "70%")) %>%
        e_tooltip() %>%
        e_title(text = paste0(str_trim(diseasePieInput3), " Case Counts by Ethnicity"), left = 'left') %>%
        e_legend(show = FALSE) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_color(colors3)
    } else if (graph_data_pie3$disease[[1]] != diseasePieInput3) {

    }

  })

  ### Echarts Pie Chart 4 Output -----------------------------------------------
  output$pieChart4 <- renderEcharts4r ({
    #### Create the list of available inputs -----------------------------------
    diseasePieInput4 <- input$selectDisease2

    #### Transform data as needed ----------------------------------------------
    graph_data_pie4 <- disease_query_demo()

    if (graph_data_pie4$disease[[1]] == diseasePieInput4) {
      graph_data_pie4 <- graph_data_pie4 %>%
        filter(disease %in% diseasePieInput4) %>%
        mutate(disease = str_trim(disease)) %>%
        mutate(county_resident = as.character(county_resident)) %>%
        mutate(county_resident = case_when(
          county_resident == "1" ~ "County Resident",
          county_resident == "0" ~ "Out Of County (Non-Resident)",
          .default = "Unknown"
        )) %>%
        group_by(county_resident) %>%
        mutate(Count = n()) %>%
        select(county_resident, Count) %>%
        distinct() %>%
        ungroup()

      #### Create color palette --------------------------------------------------
      colors4 = c("#330040", "#B7BBDD")

      #### Create the pie chart -------------------------------------------------
      graph_data_pie4 %>%
        e_charts(county_resident) %>%
        e_pie(Count, radius = c("50%", "70%")) %>%
        e_tooltip() %>%
        e_title(text = paste0(str_trim(diseasePieInput4), " Case Counts by County Residency"), left = 'left') %>%
        e_legend(show = FALSE) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_color(colors4)
    } else if (graph_data_pie4$disease[[1]] != diseasePieInput4) {
      shinyalert("Oops!", "It looks like you changed the disease input without pressing the 'Load/Refresh Query' button. Please press this button again to view the refreshed visuals. Thank you!", type = "info")
    }

  })

  ## Tab 3 Outputs -------------------------------------------------------------

  ### Retrieve list of available investigators and create picker input ---------
  output$investigators_list <- renderUI({
    #### Retrieve list of investigators from db --------------------------------
    list_of_inv <- dbGetQuery(inputs$conn, "SELECT DISTINCT ON (investigator_assigned) investigator_assigned FROM investigation_data ;")
    #### Create picker drop-down -----------------------------------------------
    pickerInput('selectInvestigator',
                label = "Select Investigator:",
                choices = list_of_inv,
                options = list(`actions-box` = TRUE,
                               `live-search` = TRUE),
                multiple = TRUE)
  })


  ### Query results for user inputs provided -----------------------------------
  inv_query <- eventReactive(input$query_go_inv, {

    #### Create the list of available inputs -----------------------------------
    input_investigator <- input$selectInvestigator

    #### Get the data from db --------------------------------------------------
    dbGetQuery(inputs$conn, paste0("SELECT * FROM disease_data
                                  NATURAL JOIN investigation_data
                                  WHERE investigator_assigned IN ('", paste(input_investigator, collapse = "','"),
                                   "') ;"))
  })

  ### Create summary table output for Tab 3 ------------------------------------
  output$invMetrics_table <- renderDT(server = FALSE, {
    datatable(if (!is.null(input$query_go_inv)) {
      table_data <- inv_query()

      #### Define start and end month/year based on user inputs ------------------
      max_month_table = month(max(table_data$investigation_start_date), label = TRUE)
      max_year_table = year(max(table_data$investigation_start_date))

      min_month_table = month(min(table_data$investigation_start_date), label = TRUE)
      min_year_table = year(min(table_data$investigation_start_date))

      #### Transform data as needed --------------------------------------------
      table_data <- table_data %>%
        mutate(month = month(investigation_start_date, label = TRUE)) %>%
        mutate(year = year(investigation_start_date)) %>%
        mutate(month_year = paste0(month, "_", year)) %>%
        mutate(time = difftime(investigation_start_date, report_date, days)) %>%
        group_by(investigator_assigned, type) %>%
        mutate(avg = round(mean(time), digits = 2)) %>%
        select(investigator_assigned, type, avg) %>%
        distinct() %>%
        mutate(avg = paste0(avg, " days"))

      #### Pivot wider to get the columns by investigation type ----------------
      table_data <- table_data %>%
        pivot_wider(names_from = type, values_from = avg) %>%
        rename(Investigator = investigator_assigned)

      #### Reference final df for datatable() function -------------------------
      table_data
    },
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color: black; padding-bottom: 30px;  font-size:150% ;',paste0('Avg. Time from Report to Investigation Start by Disease Category (', min_month_table, ' ', min_year_table, ' - ', max_month_table, ' ', max_year_table, ')')),
    rownames = FALSE,
    extensions = "Buttons",
    options = list(paging = FALSE,
                   scrollX=TRUE,
                   searching = FALSE,
                   ordering = FALSE,
                   dom = 'B',
                   buttons =
                     list(list(
                       extend = "collection",
                       buttons = list(
                         list(extend = "csv", title = paste0("disease_lookup_results_", Sys.Date())),
                         list(extend = "excel", title = paste0("disease_lookup_results_", Sys.Date())),
                         list(extend = "pdf", title = paste0("disease_lookup_results_", Sys.Date()))),
                       text = "Download Formats"
                     )
                     )))
  })

  ### Create visual for inv per month for Tab 3 --------------------------------
  output$inv_per_mo <- renderEcharts4r ({

    #### Transform data as needed ----------------------------------------------
    per_mo_data <- inv_query()

    per_mo_data <- per_mo_data %>%
      mutate(investigator_assigned = str_trim(investigator_assigned)) %>%
      mutate(month = month(investigation_start_date, label = TRUE)) %>%
      mutate(year = year(investigation_start_date)) %>%
      mutate(month_year = paste0(month, "_", year)) %>%
      group_by(investigator_assigned, month_year) %>%
      mutate(count = n()) %>%
      select(investigator_assigned, month_year, count) %>%
      distinct() %>%
      ungroup()

    #### Define the order of the x-axis (time) ---------------------------------
    per_mo_data$month_year <- factor(per_mo_data$month_year,
                                     c("Jan_2019", "Feb_2019", "Mar_2019", "Apr_2019","May_2019","Jun_2019", "Jul_2019",
                                       "Aug_2019", "Sep_2019", "Oct_2019", "Nov_2019", "Dec_2019",
                                       "Jan_2020", "Feb_2020", "Mar_2020", "Apr_2020","May_2020","Jun_2020", "Jul_2020",
                                       "Aug_2020", "Sep_2020", "Oct_2020", "Nov_2020", "Dec_2020",
                                       "Jan_2021", "Feb_2021", "Mar_2021", "Apr_2021","May_2021","Jun_2021", "Jul_2021",
                                       "Aug_2021", "Sep_2021", "Oct_2021", "Nov_2021", "Dec_2021",
                                       "Jan_2022", "Feb_2022", "Mar_2022", "Apr_2022","May_2022","Jun_2022", "Jul_2022",
                                       "Aug_2022", "Sep_2022", "Oct_2022", "Nov_2022", "Dec_2022",
                                       "Jan_2023", "Feb_2023", "Mar_2023", "Apr_2023","May_2023","Jun_2023", "Jul_2023",
                                       "Aug_2023", "Sep_2023", "Oct_2023", "Nov_2023", "Dec_2023",
                                       "Jan_2024", "Feb_2024", "Mar_2024", "Apr_2024","May_2024","Jun_2024", "Jul_2024",
                                       "Aug_2024", "Sep_2024", "Oct_2024", "Nov_2024", "Dec_2024"), ordered = TRUE)

    #### Define start and end month/year based on user inputs ------------------
    max_month_inv = substr(max(per_mo_data$month_year), start = 1, stop = 3)
    max_year_inv = substr(max(per_mo_data$month_year), start = 5, stop = 8)

    min_month_inv = substr(min(per_mo_data$month_year), start = 1, stop = 3)
    min_year_inv = substr(min(per_mo_data$month_year), start = 5, stop = 8)

    #### Create color palette --------------------------------------------------
    colors_mo = c("#330040", "#407076", "#5DCBAD", "#6B6581", "#ADA9BB", "#79114D")

    #### Create the bar chart --------------------------------------------------
    per_mo_data %>%
      group_by(investigator_assigned) %>%
      arrange(month_year) %>%
      e_charts(month_year, reorder = FALSE) %>%
      e_bar(count, stack = 'grp') %>%
      e_tooltip(trigger = 'axis') %>%
      e_title(text = "Case Counts by Investigation Start Date", subtext = paste0(min_month_inv, " ", min_year_inv ," - ", max_month_inv, " ", max_year_inv), left = 'center') %>%
      e_x_axis(name = 'Month', nameLocation = "middle", nameGap = 30) %>%
      e_y_axis(name = 'Case Count', nameLocation = "middle", nameGap = 60) %>%
      e_legend(top = 'bottom', align = 'left', orient = 'horizontal', padding = c(30, 0, 0, 0)) %>%
      e_toolbox_feature(feature = "saveAsImage") %>%
      e_color(colors_mo)

  })

  ### Create visual for inv per type for Tab 3 ---------------------------------
  output$inv_per_type <- renderEcharts4r ({

    type_data <- inv_query()

    #### Define start and end month/year based on user inputs ------------------
    max_month_type = month(max(type_data$investigation_start_date), label = TRUE)
    max_year_type = year(max(type_data$investigation_start_date))

    min_month_type = month(min(type_data$investigation_start_date), label = TRUE)
    min_year_type = year(min(type_data$investigation_start_date))

    #### Transform data as needed ----------------------------------------------
    type_data <- type_data %>%
      mutate(investigator_assigned = str_trim(investigator_assigned),
             type = str_trim(type)) %>%
      group_by(investigator_assigned, type) %>%
      mutate(count = n()) %>%
      select(investigator_assigned, type, count) %>%
      distinct() %>%
      ungroup()

    #### Create color palette --------------------------------------------------
    colors_type = c("#330040", "#407076", "#5DCBAD", "#6B6581", "#ADA9BB", "#79114D")

    #### Create the bar chart --------------------------------------------------
    type_data %>%
      group_by(investigator_assigned) %>%
      e_charts(type) %>%
      e_bar(count, stack = 'grp') %>%
      e_tooltip(trigger = 'axis') %>%
      e_title(text = "Case Counts by Type of Investigation", subtext = paste0(min_month_type, " ", min_year_type, " - ", max_month_type, " ", max_year_type), left = 'center') %>%
      e_x_axis(name = 'Type of Investigation', nameLocation = "middle", nameGap = 30) %>%
      e_y_axis(name = 'Case Count', nameLocation = "middle", nameGap = 60) %>%
      e_legend(top = 'bottom', align = 'left', orient = 'horizontal', padding = c(30, 0, 0, 0)) %>%
      e_toolbox_feature(feature = "saveAsImage") %>%
      e_color(colors_type)

  })

  ## Tab 4 Outputs -------------------------------------------------------------

  ### Query results  -----------------------------------------------------------
  full_query <- eventReactive(input$query_go_full, {

    #### Get the data from db --------------------------------------------------
    dbGetQuery(inputs$conn, paste0("SELECT * FROM disease_data
                                  NATURAL JOIN investigation_data
                                  NATURAL JOIN person_data;"))
  })

  ### Create DT table for Tab 4 ------------------------------------------------
  output$fullLook_table <- renderDT(server = FALSE, {
    datatable( if (!is.null(input$query_go_full)) {
      full_query()
    },
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    options = list(paging = TRUE,
                   scrollX=TRUE,
                   searching = TRUE,
                   ordering = TRUE,
                   dom = 'Bfrtip',
                   buttons =
                     list(list(
                       extend = "collection",
                       buttons = list(
                         list(extend = "csv", title = paste0("disease_lookup_results_", Sys.Date())),
                         list(extend = "excel", title = paste0("disease_lookup_results_", Sys.Date())),
                         list(extend = "pdf", title = paste0("disease_lookup_results_", Sys.Date()))),
                       text = "Download Formats",
                       exportOptions = list(
                         modifier = list(
                           search = "applied"
                         )
                       )
                     )),
                   pageLength=10 ))
  })

  ## Ensure that connection to db is closed when app is closed -----------------
  onStop(function() {
    pool::poolClose(inputs$conn)
  })

}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)


