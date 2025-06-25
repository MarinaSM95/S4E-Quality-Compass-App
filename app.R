
##Delete unused libraries !!!

## LIBRARIES required

library(shiny)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
#library(shinysurveys) NOT NEEDED ANYMORE
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(readr)
library(bootstrap)
library(shinyjs)
library(shinyBS)
library(bsplus)
library(DT)




#survey_full<- read_csv2(".//www/survey_data.csv", col_names = TRUE)
#shiny::addResourcePath ("survey, ".//www/checklist/checklist.html")

# Load and preprocess the survey data (Global environment)
survey <- read_csv2("www/test.csv") %>%
  mutate(
    help = ifelse(is.na(notes) | notes == "", NA, notes),
    choices = ifelse(!is.na(options), strsplit(options, ";"), NA),
    required = as.logical(required)
  )
# Define extended input type textSlider
# CHECK DIFFERENCE



ui <- dashboardPage(
 
 
  ## HEADER
  header=dashboardHeader(
    #TITLE in Header: title + logo
    title = tags$div(
      style = "display: flex; align-items: center; height: 100%; overflow: visible;",
      tags$a(
        href = "https://www.skills4eosc.eu/",
        tags$img(
          src = "logo_S4E_neg_comp.png",  
          title = "Skills4EOSC website", 
          height = "70px",
          style = "margin-left: 11px; margin-right: 5px; margin-top: 0px;"
        )
      ),
      tags$div(
        "S4E Quality Compass",
        style = "font-size: 25px; font-weight: bold; line-height: 1.2;"
      )
    )
    ,
    # Title width of header
    titleWidth="250px" # same as width in Sidebar
   ),
  
  
 
  ## SIDEBAR (built dinamically in the server section)
  dashboardSidebar(
    width = 250,
    uiOutput("dynamic_sidebar")
  )
  
  ,
  
  ## BODY
  dashboardBody(
    useShinyjs(),
    tags$style(HTML("
  /* Make header taller */
  .main-header {
    height: 100px !important;
  }

  /* Push sidebar content down to avoid overlap */
  .main-sidebar {
    padding-top: 110px !important;
  }

  /* Style and extend the title area */
  .main-header .logo {
    background-color: #1E282C !important; /* Or match your color */
    height: 110px !important;
    line-height: 20px !important;
    padding: 5px;
    overflow: visible;
  }

  /* Adjust the navbar to align properly */
  .main-header .navbar {
    margin-left: 250px;
    height: 100px !important;
    
  }
 
 
  #intro_about .box-header{
  color: white;
  
  }

  #intro_about {
    background-color:#d26f2d;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
    border-radius: 12px;
    padding: 2em;
   
  }
  #checklist_about {
    background-color: #3278B1;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
    border-radius: 12px;
    padding: 2em;
  }
  #checklist_about .box-header{
    color: white;
  }
  #compass_about {
    background-color: #3278B1;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2); 
    padding: 2em;
    border-radius: 12px;
  }
  #compass_about .box-header{
    color: white;
  }
  #introassessment{
    background-color: #d26f2d;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2); 
    padding: 2em;
    border-radius: 12px;
  }
  #introassessment .box-header{
     
    color: white;
  }
  #outputs_about {
    background-color: #3278B1;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);  
    border-radius: 12px;
    padding: 2em;
  }
  #outputs_about .box-header{
    color: white;
  }
    
    
     .section-box {
  border: 2px solid transparent;
  transition: border 0.3s, background-color 0.3s, box-shadow 0.3s;
  border-radius: 12px;
  text-align: center;
  display: flex;
  align-items: center;
  justify-content: center;
  height: 100px;
  min-width: 180px;
  flex: 1;
  font-size: 14px;
}
.section-box.active {
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
}

  
#sections_select {
  display: flex;
  justify-content: center;
  gap: 10px;
  flex-wrap: wrap; /* <-- allows wrapping */
  margin-bottom: 20px;
  margin: 20px;
}
#select_section0.active {
  border-color: #7D7D7D !important;
  background-color: #B8B8B8 !important;
  font-weight: bold;
  box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
}
#select_section1.active {
  border-color: #F49200 !important;
  background-color: #FFC165 !important;
  font-weight: bold;
  box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
}
#select_section2.active {
  border-color: #95C11F !important;
  background-color: #C2E561 !important;
  font-weight: bold;
  box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
}
#select_section3.active {
  border-color: #3278B1 !important;
  background-color: #69A4D5 !important;
  font-weight: bold;
  box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
}
#select_section4.active {
  border-color: #E6007E !important;
  background-color: #FF6DBD !important;
  font-weight: bold;
  box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
}
#progress_container {
  margin-top: 10px;
}
.progress {
  height: 25px;
}
.progress-bar {
  font-weight: bold;
  background-color: #337ab7;
}
#self-assessment {
  border-radius: 12px !important;
  overflow: hidden;
}
.flag-button {
  background: none;
  border: none;
  padding: 0;
  cursor: pointer;
}"
))
    ,
    
    tabItems(
      tabItem(tabName = "about",
              fluidRow( width="100%",
                box (id= "intro_about",
                     title = tags$b("Welcome!"),
                     solidHeader = TRUE,
                     top = 0,
                     left = 0,
                     right = 0,
                     bottom = 0,
                     width = 12,
                     height = "20%",
                     draggable = FALSE,
                     fixed = TRUE,
                     tags$p("Welcome to the Skills4EOSC Quality Compass, the self-assessment app that
                     helps you in making your courses compliant with the Skills4EOSC Quality
                     Assurance Framework. In our mission of ensuring quality in the full life-cycle
                     of training, we have produced two main outputs that will guide you in taking your
                     learning resources to the next level."),
                     tags$li("S4E Quality Compass"),
                     tags$li("Skills4EOSC Checklist and Guide"),
                     tags$br(),
                     tags$p("By following our guidelines, you will ensure the integration of the FAIR-by-design 
                     methodology, the Minimum Viable Skillsets, key Ethical and Legal aspects and other 
                     e-learning quality criteria in your course.")
                     )
              ),
              tags$div(tags$h3 (tags$b("At which stage of designing your course are you?")),
                       style= "color:grey; text-align: center;"),
              tags$br(),
              fluidRow (box (id= "checklist_about", width=6,
                             title= tags$b("First stage of designing your course"), 
                             solidHeader = TRUE,
                             
                             tags$div(style="text-align: certer; color:#b8cce0;",
                                       tags$h4("The S4E Quality Checklist")),
                             tags$br(),
                             htmlOutput("checklist"),
                             tags$div(
                               tags$br(),
                               
                               tags$p("The QA Checklist is an interactive infography that
                               covers the main aspects and indicators of the QA Framework.
                               It aims to help you in making your learning resource compliant
                               during its first stages of design and planification. It also
                               serves as a more to complement the deliverable (you can find
                               the deliverable below in 'Other outputs').")
                               )
                             )
                        ,
                        box (id= "compass_about",
                             width=6,
                             title= tags$b("Last stage of designing your course"), 
                             solidHeader = TRUE,
                             tags$div( style="text-align: certer; color:#b8cce0;",
                               tags$h4("The S4E Quality Compass")),
                               tags$br(),
                               tags$img(src = "clip_compass.gif",  
                                      style = "max-width: 100%; height: auto;"),
                             
                             tags$div(
                               tags$br(),
                               
                               tags$p("The Skills4EOSC Quality Compass is a self-assessment
                               test that covers all indicators from the Skills4EOSC Quality
                               Assurance Framework. After answering some those questions, you
                               will get a report on your course compliance with the framework. 
                               It provides scores by section and recommendations on how to 
                               improve your learning materials. You can find the tool in the 
                               sidebar menu")
                             )
                        )
                        ),
              fluidRow(
                        box (id= "outputs_about",
                             width=12,
                             title= tags$b("Other Outputs related"),
                             solidHeader= TRUE,
                             "If you want to know more about how the Skills4EOSC Quality Assurance
                             was built, here are some Zenodo's publications regarding our work:",
                             tags$li(a(href="https://zenodo.org/records/8305482", "D2.3. Community-endorsed quality assurance and certification framework for professional training and qualifications - first iteration")),
                             tags$li(a(href="https://zenodo.org/records/12604767", "FAIR-by-Design Learning Materials Methodology Training of Trainers")),
                             tags$li(a(href="https://zenodo.org/records/8101903", "D2.1 Catalogue of Open Science Career Profiles - Minimum Viable Skillsets"))
                             
                             ))
              
       ),
      
      tabItem(tabName = "assessment",
              # Panel shown by default, hidden survey until button "clickforsurvey" is clicked
              conditionalPanel("input.clickforsurvey == 0", 
                               
              fluidRow (
                box(
                  id = "introassessment",
                  title = tags$b("S4E Quality Compass"),
                  tags$p("The Skills4EOSC Quality Compass is a self-assessment
                  test that covers all indicators from the Skills4EOSC Quality
                  Assurance Framework. You will navigatethrough 5 sections: background
                  information, Content and Structure, Implementation, Evaluation and
                  Compliance, Licensing and Ethics. In addition to answering the question, 
                  we encourage you to provide any comments or doubts regarding the questions.
                  Just click on the flag next to each question, write your comments and send them."),
                  tags$p("After answering some those questions, you
                  will get a report on your course compliance with the framework. 
                  This report provides your scores by section and recommendations on
                  how to improve your learning materials."),
                  tags$p("Regarding personal data collection, you don't need to provide
                  any personal information you don't wish to be stored. You will still
                  receive your report and results regardless of providing personal 
                  information. Any information you decide to provide and your answers and
                  feedback will be store for research purposes and to keep improving
                  the tool"), 
                  
                  top = 0,
                  left = 0,
                  right = 0,
                  bottom = 0,
                  width = 12,
                  height = "10%",
                  draggable = FALSE,
                  fixed = TRUE,
                  div(tags$br(),actionButton("clickforsurvey", label ="Start self-assessment test"))
                    #actionButton(inputId = "m", label = "Proceed", icon = NULL)   ),  ## Conditional Panel for Approval   
                  ))),
              # survey shown when button is clicked
              conditionalPanel("input.clickforsurvey == 1",
              tags$h2("S4E Quality Compass"),                 
              fluidRow (
                
                  div(id="sections_select", style = "display: flex; justify-content: center; gap: 10px; height: 100%;",
                  div(id="select_section0", class="section-box", "Personal Data", width=NULL),
                  div(id="select_section1", class="section-box","Content & Structure", width=NULL),
                  div(id="select_section2", class="section-box","Implementation", width=NULL),
                  div(id="select_section3", class="section-box","Evaluation", width=NULL),
                  div(id="select_section4", class="section-box","Compliance, Licensing & Ethics", width=NULL)
                )),
              uiOutput("progress_bar_ui"),
                fluidRow(
                  box(id="self-assessment",width=12, uiOutput("page_ui"))
                 
                )
              )),
      tabItem(tabName = "results",
              conditionalPanel(
                condition = "output.show_results",
                fluidRow(
                  box(
                    title = "Your Answers",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput("results_table")
                  )
                ),
                fluidRow(
                  tags$h3("Your Score"),
                  valueBoxOutput("score_total"),
                  valueBoxOutput("score_minimal"),
                  valueBoxOutput("score_detailed")
                ),
                
                fluidRow(
                  tags$h3("Score by section"),
                  box(solidHeader = TRUE,
                      background = NULL,
                      width = 6,
                      # centered valueBoxes
                      style = "border-color: #F49200; border-radius: 12px;",
                      
                      title= "Content & Structure",
                      valueBoxOutput("score_content_minimal"),
                      valueBoxOutput("score_content_detailed")
                      ),
                  box(title= "Implementation",
                      width = 6,
                      solidHeader = TRUE,
                      background = NULL,
                      style = "border-color: #95C11F; border-radius: 12px; padding:5px;",
                      valueBoxOutput("score_implementation_minimal"),
                      valueBoxOutput("score_implementation_detailed")
                      )),
                fluidRow(
                  box(title="Evaluation", 
                      width = 6,
                      solidHeader = TRUE,
                      background = NULL,
                      style = "border-color: #3278B1; border-radius: 12px;",
                      valueBoxOutput("score_evaluation_minimal"),
                      valueBoxOutput("score_evaluation_detailed")                     
                      ),
                  box(title = "Compliance, Licensing & Ethics",
                      solidHeader = TRUE,
                      width = 6,
                      background = NULL,
                      style = "border-color: #E6007E; border-radius: 12px;",
                      valueBoxOutput("score_compliance_minimal"),
                      valueBoxOutput("score_compliance_detailed")
                      )
                  ),

                fluidRow(
                  box(title = "How Can You Improve the Quality of Your Course?",
                      width = 12,
                      tags$h4("Best Practices"),
                      DT::dataTableOutput("Best_practices")
                  )
                )
              ))
      )),
    
    
    

  footer= dashboardFooter(
    left= tags$div(style= "font-size: 10px; padding:5px; padding-top:10px;position:relative;", 
                   tags$p("Except where otherwise noted, content on this site is licensed 
                            under a Creative Commons Attribution 4.0 
                            International License.  Sanchez-Moreno, Marina.
                            (2025). S4E Quality Compass app (1.0.0).")),

    right= tags$div(style="padding:8px; margin-top: 0px; position:relative;",
                    img(src="logo_S4E_pos_ext.png",  
                        title = "Skills4EOSC project", 
                        height = "30px"),
                    img(src="logo_eosc_ext.jpeg",  
                        title = "Skills4EOSC website", 
                        height = "30px"),
                    img(src="logo_eu_trans.png",  
                        title = "Co-funded by the European Union", 
                        height = "30px"),
                    img(src="logo_uc3m_pos_ext.png",  
                        title = "Carlos III University of Madrid", 
                        height = "30px"),
                    )
    
  )
    
)

## SERVER

server <- function(input, output, session) {
  pages <- unique(survey$pages)
  current_page <- reactiveVal(pages[1])
  
  # Store user feedback
  feedback_store <- reactiveValues()
  
  # Track if submission is done
  submission_complete <- reactiveVal(FALSE)
  
  #Track user responses
  user_results <- reactiveVal(NULL)
  
  # Dinamic sidebar (hidden results tab until self-assessment is complete)
  output$dynamic_sidebar <- renderMenu({
    sidebarMenu(id = "sidebarMenuid", selected="about",
                menuItem("About", tabName = "about", icon = icon("home")),
                menuItem("S4E Quality Compass", icon = icon("list-check"),
                         tabName = "assessment",
                         menuSubItem("Self-assessment", tabName = "assessment"),
                         if (submission_complete()) {
                           menuSubItem("Results", tabName = "results")
                         }
                ),
                menuItem("Feedback", tabName = "feedback", icon = icon("comments"))
    )
  })
  
  check_required_inputs <- function(page_data, input) {
    missing <- list()
    for (i in 1:nrow(page_data)) {
      row <- page_data[i, ]
      input_id <- paste0("q_", row$input_id)
      
      # Skip if hidden by dependency
      if (!is.na(row$dependence) && nzchar(row$dependence)) {
        parent_input <- input[[paste0("q_", row$dependence)]]
        if (is.null(parent_input) || parent_input != row$dependence_value) {
          next
        }
      }
      
      if (isTRUE(row$required)) {
        val <- input[[input_id]]
        if (is.null(val) || val == "") {
          missing[[row$input_id]] <- row$question
        }
      }
    }
    return(missing)
  }
  
  output$page_ui <- renderUI({
    page_data <- survey %>% filter(pages == current_page())
    visible_index <- 0
    
    questions_ui <- lapply(1:nrow(page_data), function(i) {
      row <- page_data[i, ]
      qid <- row$input_id
      inputId <- paste0("q_", qid)
      
      # Handle dependency logic
      show_question <- TRUE
      if (!is.na(row$dependence) && nzchar(row$dependence)) {
        parent_val <- input[[paste0("q_", row$dependence)]]
        if (is.null(parent_val) || parent_val != row$dependence_value) {
          show_question <- FALSE
        }
      }
      if (!show_question) return(NULL)
      
      visible_index <<- visible_index + 1
      
      asterisk <- if (isTRUE(row$required)) {
        tags$span("*", style = "color:red; margin-left:5px;")
      } else NULL
      
      label_question <- tags$b(
        tagList(paste0(visible_index, ". ", row$question), asterisk)
      )
      
      help_text <- if (!is.na(row$notes) && nzchar(row$notes)) {
        tags$p(style = "font-size: 90%; color: #555; margin-top: 0.25em; margin-bottom: 0.75em;", row$notes)
      } else NULL
      
      choices <- if (row$input_type %in% c("mc", "select", "textSlider") && !is.na(row$options)) {
        trimmed <- trimws(unlist(strsplit(row$options, ";")))
        if (length(trimmed) == 0) NULL else trimmed
      } else NULL
      
      # Feedback icon logic
      has_feedback <- !is.null(feedback_store[[qid]])
      flag_icon <- if (has_feedback) {
        icon("flag", class = "fa-solid", style = "color:red;")
      } else {
        icon("flag", class = "fa-regular", style = "color:red;")
      }
      tooltip_title <- if (has_feedback) "Feedback submitted" else "Flag this question"
      feedback_button <- actionButton(
        inputId = paste0("flag_", qid),
        label = NULL,
        icon = flag_icon,
        style = "background: none; border: none;",
        class = "pull-left",
        title = tooltip_title
      )
      
      # Input UI
      input_ui <- switch(
        row$input_type,
        "mc" = radioButtons(inputId = inputId, label = NULL, choices = choices,
                            selected = isolate(input[[inputId]]) %||% character(0)),
        "text" = textInput(inputId = inputId, label = NULL, value = isolate(input[[inputId]]) %||% ""),
        "textSlider" = shinyWidgets::sliderTextInput(inputId = inputId, label = NULL,
                                                     choices = choices, force_edges = TRUE),
        "select" = selectInput(inputId = inputId, label = NULL, choices = choices,
                               selected = isolate(input[[inputId]]) %||% character(0)),
        div(style = "color:red;", paste("Unsupported input_type:", row$input_type))
      )
      
      # Combine
      div(style = "margin-bottom: 1.5em;",
          fluidRow(
            column(width = 1, feedback_button),
            column(width = 11,
                   label_question,
                   help_text,
                   div(style = "margin-top: 0.5em;", input_ui)
            )
          )
      )
    })
    
    nav_buttons <- tagList(
      textOutput("error_message"),
      tags$style("#error_message { color: red; font-weight: bold; margin-bottom: 1em; }"),
      if (which(pages == current_page()) > 1)
        actionButton("prev_page", "Previous"),
      if (which(pages == current_page()) < length(pages))
        actionButton("next_page", "Next"),
      if (which(pages == current_page()) == length(pages))
        actionButton("submit", "Submit")
    )
    
    box(
      title = current_page(),
      width = 12,
      style = "border-radius: 12px;",  # round corners
      do.call(tagList, questions_ui),
      nav_buttons
    )
  })
  
  # Navigation logic
  observeEvent(input$next_page, {
    page_data <- survey %>% filter(pages == current_page())
    missing <- check_required_inputs(page_data, input)
    
    if (length(missing) > 0) {
      output$error_message <- renderText("Please answer all required questions before continuing.")
    } else {
      output$error_message <- renderText("")
      idpage <- which(pages == current_page())
      if (idpage < length(pages)) {
        current_page(pages[idpage + 1])
        runjs("window.scrollTo(0, 0);")
      }
    }
  })
  
  observeEvent(input$prev_page, {
    idpage <- which(pages == current_page())
    if (idpage > 1) {
      current_page(pages[idpage - 1])
      runjs("window.scrollTo(0, 0);")
    }
  })
  
  # Progress bar
  output$progress_bar_ui <- renderUI({
    if (current_page() != pages[1]) {
      tagList(
        div(id = "progress_container",
            div(class = "progress",
                div(id = "progress_bar", class = "progress-bar",
                    role = "progressbar", style = "width: 0%;",
                    "0% Completed"
                )
            )
        )
      )
    } else {
      NULL
    }
  })
  
  get_completion_percent <- reactive({
    filtered <- survey %>% filter(pages != pages[1])
    total <- nrow(filtered)
    answered <- sum(sapply(filtered$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && val != ""
    }))
    percent <- round((answered / total) * 100)
    percent
  })
  
  observe({
    updateTextInput(session, "current_page_internal", value = current_page())
  })
  
  observe({
    percent <- get_completion_percent()
    runjs(sprintf("
      $('#progress_bar').css('width', '%s%%');
      $('#progress_bar').attr('aria-valuenow', %s);
      $('#progress_bar').text('%s%% Completed');
    ", percent, percent, percent))
  })
  
  # Feedback modals for all questions
  observe({
    lapply(survey$input_id, function(qid) {
      observeEvent(input[[paste0("flag_", qid)]], {
        showModal(modalDialog(
          title = paste("Feedback for question", qid),
          textAreaInput(
            inputId = paste0("temp_feedback_", qid),
            label = "Provide your comment below:",
            value = feedback_store[[qid]] %||% "",
            width = "100%",
            height = "120px"
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(paste0("send_feedback_", qid), "Send Feedback")
          ),
          easyClose = TRUE
        ))
      })
      
      observeEvent(input[[paste0("send_feedback_", qid)]], {
        val <- input[[paste0("temp_feedback_", qid)]]
        if (!is.null(val) && nzchar(val)) {
          feedback_store[[qid]] <- val
        }
        removeModal()
      })
    })
  })
  
  # Section highlighting
  section_map <- setNames(
    c("select_section0", "select_section1", "select_section2", "select_section3", "select_section4"),
    pages
  )
  
  observe({
    current <- section_map[[current_page()]]
    all_sections <- unname(section_map)
    
    lapply(all_sections, function(sec) {
      shinyjs::removeClass(id = sec, class = "active")
    })
    shinyjs::addClass(id = current, class = "active")
    shinyjs::runjs(sprintf("$('#%s').fadeOut(200).fadeIn(200);", current))
  })
  # SUBMIT button behaviour (it includes dependency and required, although this is not implemented)
  observeEvent(input$submit, {
    cat("Submit button clicked!\n")  # For debugging
    page_data <- survey %>% filter(pages == current_page())
    missing <- check_required_inputs(page_data, input)
    
    if (length(missing) > 0) {
      output$error_message <- renderText("Please, answer all required questions before submitting.")
    } else {
      output$error_message <- renderText("")
      
      # Store results
      answers <- lapply(seq_len(nrow(survey)), function(i) {
        row <- survey[i, ]
        input_id <- paste0("q_", row$input_id)
        
        # Check visibility
        parent_input <- input[[paste0("q_", row$dependence)]]
        
        show_question <- is.na(row$dependence) ||
          (!is.null(parent_input) && parent_input == row$dependence_value)
        
        if (!show_question) return(NULL)
        
        list(
          Number = i,
          Page = row$pages,
          Question = row$question,
          Answer = if (!is.null(input[[input_id]]) && nzchar(input[[input_id]])) input[[input_id]] else "No answer",
          Level = if (!is.null(row$minimal_detailed) && !is.na(row$minimal_detailed)) row$minimal_detailed else "Personal question",
          Feedback = feedback_store[[row$input_id]] %||% ""
          
        )
      })
      
      # Filter non-null results
      results_df <- do.call(rbind, lapply(answers, function(ans) {
        if (is.null(ans)) return(NULL)
        as.data.frame(ans, stringsAsFactors = FALSE)
        
      }))
      
      user_results(results_df)
      
      output$results_table <- DT::renderDataTable({
        DT::datatable(results_df, 
                      extensions = 'Buttons',
                      options = list(
                        dom = 'Bfrtip',
                        buttons = c('csv', 'pdf'),
                        pageLength = 10,
                        rowCallback = JS(
                          "function(row, data, index) {",
                          "  var answer = data[3];", 
                          "  var level = data[4];",   
                          "  if (answer === 'No answer') {",
                          "    $(row).css({'background-color': '#f0f0f0', 'color': '#555'});",
                          "  } else if (answer === 'Yes' && (level === 'minimal' || level === 'detailed')) {",
                          "    $(row).css('background-color', '#d4edda');",
                          "  } else if (answer === 'No' && level === 'minimal') {",
                          "    $(row).css('background-color', '#f8d7da');",
                          "  } else if (answer === 'No' && level === 'detailed') {",
                          "    $(row).css('background-color', '#fff3cd');",
                          "  }",
                          "}"
                        )
                        
                      ),
                      rownames = FALSE
        )
      })
      
      
      
      submission_complete(TRUE)
      
      updateTabItems(session, "sidebarMenuid", selected = "results")
      
    }
  })
  

  
  # Show/Hide condition for results
  output$show_results <- reactive({
    submission_complete()
  })
  
  outputOptions(output, "show_results", suspendWhenHidden = FALSE)
  
  observe({
    updateTabItems(session, "sidebarMenuid", selected = "about")
  })
  
  #RESULTS. SCORES
  # Total score
  output$score_total <- renderValueBox({
    total_questions <- survey %>% filter(minimal_detailed %in% c("minimal", "detailed"))
    
    not_applicable <- sum(vapply(total_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && identical(val, "Not applicable")
    }, logical(1)))
    
    yes_answers <- sum(vapply(total_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && identical(val, "Yes")
    }, logical(1)))
    
    total <- nrow(total_questions) - not_applicable
    
    valueBox(
      paste0(yes_answers, " / ", total),
      subtitle = "Total Yes Answers",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  
  
  # Minimal total score
  output$score_minimal <- renderValueBox({
    minimal_questions <- survey %>% filter(minimal_detailed == "minimal")
    
    not_applicable <- sum(vapply(minimal_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && identical(val, "Not applicable")
    }, logical(1)))
    
    yes_answers <- sum(vapply(minimal_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && identical(val, "Yes")
    }, logical(1)))
    
    total <- nrow(minimal_questions) - not_applicable
    
    valueBox(
      paste0(yes_answers, " / ", total),
      subtitle = "Minimal Level - Yes Answers",
      icon = icon("flag-checkered"),
      color = "yellow"
    )
  })
  
  
  
  output$score_detailed <- renderValueBox({
    detailed_questions <- survey %>% filter(minimal_detailed == "detailed")
    
    not_applicable <- sum(vapply(detailed_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && identical(val, "Not applicable")
    }, logical(1)))
    
    yes_answers <- sum(vapply(detailed_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && identical(val, "Yes")
    }, logical(1)))
    
    total <- nrow(detailed_questions) - not_applicable
    
    valueBox(
      paste0(yes_answers, " / ", total),
      subtitle = "Detailed Level - Yes Answers",
      icon = icon("clipboard-check"),
      color = "blue"
    )
  })
  # SCORE BY SECTION
  # function for all sections
  score_by_section <- function(section, level) {
    df <- user_results()
    if (is.null(df)) return(list(yes = 0, total = 0))
    
    section_df <- df %>%
      filter(Page == section, Level == level)
    
    cat("Debug — Section:", section, "Level:", level, "→", nrow(section_df), "rows\n")
    
    not_applicable <- sum(section_df$Answer == "Not applicable")
    yes_answers <- sum(section_df$Answer == "Yes")
    total <- nrow(section_df) - not_applicable
    
    list(yes = yes_answers, total = total)
  }
  
  
  
  #SCORE BY SECTION
  # Rendering each valuebox
  # Section: Content & Structure
  output$score_content_minimal <- renderValueBox({
    req(user_results())
    res <- score_by_section("1. Content & Structure", "minimal")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon("flag"), color = "yellow")
  })
  
  output$score_content_detailed <- renderValueBox({
    req(user_results())
    res <- score_by_section("1. Content & Structure", "detailed")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon("clipboard"), color = "blue")
  })
  
  # Section: Implementation
  output$score_implementation_minimal <- renderValueBox({
    req(user_results())
    res <- score_by_section("2. Implementation", "minimal")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon("flag"), color = "yellow")
  })
  
  output$score_implementation_detailed <- renderValueBox({
    req(user_results())
    res <- score_by_section("2. Implementation", "detailed")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon("clipboard"), color = "blue")
  })
  
  # Section: Evaluation
  output$score_evaluation_minimal <- renderValueBox({
    req(user_results())
    res <- score_by_section("3. Evaluation", "minimal")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon("flag"), color = "yellow")
  })
  
  output$score_evaluation_detailed <- renderValueBox({
    req(user_results())
    res <- score_by_section("3. Evaluation", "detailed")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon("clipboard"), color = "blue")
  })
  
  # Section: Compliance, Licensing & Ethics
  output$score_compliance_minimal <- renderValueBox({
    req(user_results())
    res <- score_by_section("4. Compliance, Licensing & Ethics", "minimal")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon("flag"), color = "yellow")
  })
  
  output$score_compliance_detailed <- renderValueBox({
    req(user_results())
    res <- score_by_section("4. Compliance, Licensing & Ethics", "detailed")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon("clipboard"), color = "blue")
  })

  ## RESULTS: Best practices
  output$Best_practices <- DT::renderDataTable({
    req(user_results())
    
    # Merge user results with survey to get best practices and section
    merged <- user_results() %>%
      left_join(
        survey %>% select(question, input_id, bestpractices, pages),
        by = c("Question" = "question")
      )
    
    # Filter to keep only relevant rows
    filtered <- merged %>%
      filter(
        pages != "0. Personal Data",
        Answer %in% c("No", "No answer"),
        !is.na(bestpractices) & bestpractices != ""
      ) %>%
      select(Question, Level, Answer, `Best Practice` = bestpractices)
    
    # Create interactive datatable with export options
    DT::datatable(
      filtered,
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
  
  # checklist iframe (about page)
  output$checklist <- renderUI({
    tags$iframe(
      title = "Checklist",
      src = "https://view.genially.com/682d8981d26d435becb916c6",
      type = "text/html",
      width = "100%",
      style = "max-width: 100%; height: auto;"
    )
  })
  
  
  
}






# Run the application 
shinyApp(ui = ui, server = server)
