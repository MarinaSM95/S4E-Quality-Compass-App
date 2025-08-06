
##Delete unused libraries !!!

## LIBRARIES required

library(shiny)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
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






# Load and preprocess the survey data (Global environment)
survey <- read_csv2("www/self-assessment_clean.csv") %>%
  mutate(
    help = ifelse(is.na(notes) | notes == "", NA, notes),
    choices = ifelse(!is.na(options), strsplit(options, ";"), NA),
    required = as.logical(required)
  )

# Load and preprocess the general feedback data
general_feedback <- read_csv2("www/general_feedback_clean.csv") %>%
  mutate(
    choices = ifelse(!is.na(options), strsplit(options, ";"), NA),
    required = as.logical(required)
  )



ui <- dashboardPage(
 
 
  ## HEADER
  header=dashboardHeader(
    #TITLE in Header: logo
    title = tags$div(
      style = "display: flex; align-items: center; height: 70px;",
      tags$a(
        href = "https://www.skills4eosc.eu/",
        
        # Horizontal logo (default)
        tags$img(
          src = "logo_S4E_neg_horizontal.png",
          title = "Skills4EOSC website",
          height = "55px",
          class = "logo-expanded"
          
        ),
        
        # Vertical logo (collapsed)
        tags$img(
          src = "logo_S4E_neg_vertical.png",
          title = "Skills4EOSC website",
          height = "30px",
          class = "logo-collapsed",
          style = "display: none;"
        )
      )
    )
    ,
    # Title width of header
    titleWidth="250px", # same as width in Sidebar
    tags$li(
      class = "dropdown",
      
      tags$div(
        style = "
      display: flex;
      align-items: center;
      height: 70px;
      color: white;
      font-size: 20px;
      font-weight: bold;
      justify-content: flex-start;
    ",
        "S4E Quality Compass"
      )
    )
   ),
  
  
 
  ## SIDEBAR (built dinamically in the server section)
  dashboardSidebar(
    
    width = 250,
    sidebarMenu( uiOutput("dynamic_sidebar"))
  )
  
  ,
  
  ## BODY
  dashboardBody(
    useShinyjs(),
    tags$head(tags$script(HTML("
  $(document).on('change', 'input[type=checkbox]', function(e) {
    var name = $(this).attr('name');
    var checkboxes = $('input[name=' + name + ']');
    
    if (checkboxes.filter(':checked').length > 1) {
      // Uncheck other boxes
      checkboxes.not(this).prop('checked', false);
      // Trigger input event so Shiny picks up the change
      checkboxes.not(this).trigger('change');
    }
  });
  
"))
    ),
    tags$style(HTML("
    
  /* Make header taller */
  .main-header {
    height: 70px !important;
    background-color: #3C8DBC !important;
  }


body, .wrapper, .content-wrapper, .right-side {
  background-color: white !important;
}


  /* Push sidebar content down to avoid overlap */
  .main-sidebar {
    padding-top: 90px !important;
  }

  /* Style and extend the title area */
  .main-header .logo {
    height: 70px !important;
  width: 250px;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 0 !important;
  transition: width 0.3s ease;
  }
  .logo-expanded {
  max-height: 50px;
  height: auto;
  display: block;
}

.logo-collapsed {
  display: none;
  max-height: 40px;
}

.sidebar-collapse .logo-expanded {
  display: none !important;
}
.sidebar-collapse .logo-collapsed {
  display: inline-block !important;
}


/* Collapsed logo fits */
.sidebar-collapse .main-header .logo {
  width: 50px !important;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
  padding: 0 !important;
}

  /* Adjust the navbar to align */
  .main-header .navbar {
    margin-left: 0px;
    float: left !important;
    
    
  }
 .navbar-custom-menu > .navbar-nav > .dropdown {
  width: 100%; /* control the container width */
}

 /* About page */
 
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
  
  /* Self-assessment and feedback survey intro pages */
  #introassessment, #introsurvey {
    background-color: #d26f2d;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2); 
    padding: 2em;
    border-radius: 12px;
  }
  #introassessment .box-header, #introsurvey .box-header{
     
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
  #outputs_about a {
  color: #b0dbff !important; /* example: gold */
  text-decoration: underline; /* optional */
}

#outputs_about a:hover {
  color: #FFFFFF !important; /* example: white on hover */
  text-decoration: none; /* optional */
}
  
#clickforassessment, #clickforsurvey {
  position: center;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 12px;
  padding: 14px 28px;
  background: linear-gradient(145deg, #3278B1, #3278B1);
  border: 2px solid rgba(255, 255, 255, 0.2);
  border-radius: 100px;
  color: #fff;
  font-size: 16px;
  font-weight: 600;
  letter-spacing: 0.5px;
  cursor: pointer;
  overflow: hidden;
  transition: all 0.4s ease-in-out;
  box-shadow: 0 0 20px rgba(0, 255, 255, 0.1);
  backdrop-filter: blur(8px);
  z-index: 1;
}

#clickforassessment::before, #clickforsurvey::before {
  content: '';
  position: absolute;
  top: -50%;
  left: -50%;
  width: 200%;
  height: 200%;
  background: conic-gradient(from 0deg, #E6007E, #3278B1, #95C11F, #E6007E);
  animation: rotate 4s linear infinite;
  z-index: -2;
}

#clickforassessment::after, #clickforsurvey::after {
  content: '';
  position: absolute;
  inset: 2px;
  background: #3278B1;
  border-radius: inherit;
  z-index: -1;
}

#clickforassessment:hover, #clickforsurvey:hover {
  transform: scale(1.05);
  box-shadow: 0 0 40px rgba(0, 0, 0, 0.3));
}

#clickforassessment:hover .arrow, #clickforsurvey:hover .arrow {
  transform: translateX(6px);
}

.arrow {
  width: 22px;
  height: 22px;
  transition: transform 0.3s ease-in-out;
  color: white;
}

@keyframes rotate {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}

#sections_select {
  display: flex;
  justify-content: center;
  gap: 10px;
  flex-wrap: wrap;
  margin: 30px;
}

/* Base style for all section boxes */
.section-box {
  position: relative;
  flex: 1;
  height: 70px;
  min-width: 140px;
  font-size: 15px;
  text-align: center;
  font-weight: bold;
  color: white;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 0 15px;
  margin-left: -20px;
  z-index: 1;
  background-color: #f4f4f4; /* light gray for inactive */
  box-shadow: 0 2px 6px rgba(0, 0, 0, 0.08);
  transition: transform 0.3s, box-shadow 0.3s, background-color 0.3s;
  clip-path: polygon(0 0, 90% 0, 100% 50%, 90% 100%, 0 100%, 10% 50%);
}

/* First box: no overlap + rounded left */
.section-box:first-child {
  margin-left: 0;
  clip-path: polygon(0 0, 90% 0, 100% 50%, 90% 100%, 0 100%);
  border-top-left-radius: 35px;
  border-bottom-left-radius: 35px;
}

/* Last box: sharp arrowhead style */
.section-box:last-child {
  clip-path: polygon(0 0, 90% 0, 100% 50%, 90% 100%, 0 100%, 10% 50%);
  flex-grow: 1.2;
  z-index: 0;
}

/* Active section box: enlarged, strong shadow, colored background */
.section-box.active {
  transform: scale(1.03);
  z-index: 2;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
}

/* Colored backgrounds for active sections */
#select_section0.active {
  background-color: #333 !important; /* gray */
}
#select_section1.active {
  background-color: #F49200 !important; /* orange */
}
#select_section2.active {
  background-color: #95C11F !important; /* green */
}
#select_section3.active {
  background-color: #3278B1 !important; /* blue */
}
#select_section4.active {
  background-color: #E6007E !important; /* pink */
}


/* Inactive sections text color darker for readability */
.section-box:not(.active) {
  color: #525252;
}

#progress_container {
  margin-top: 10px;
  
}
.progress {
  height: 25px;
   border-radius: 10px !important; 
}
.progress-bar {
  font-weight: bold;
  background-color: #337ab7;
  border-radius: 10px !important; 
}




#self-assessment .box-header, #general-feedback .box-header {
  display: none !important;
}

.flag-button {
  background: none;
  border: none;
  padding: 0;
  cursor: pointer;
}

.popover-title {
  background-color: #3278B1 !important;
  color: white !important;
  font-weight: bold;
}

.popover-content {
  background-color: #f9f9f9;
  color: #333;
}

.modal-header {
  background-color: #3278B1 !important;
  color: white !important;
  font-weight: bold;
  border-bottom: none;
}

.modal-title {
  color: white !important;
} 

#self-assessment.box, #general-feedback,
#general-feedck-container{
  background-color: transparent !important;
  box-shadow: none !important;
  border: none !important;
}

#next_page, #prev_page {
  background-color: #3278B1;
  margin-right: 10px;
  margin-left: 10px;
  justify: center !important;
  color: white;
  border: none;
  border-radius: 30px;
  padding: 10px 24px;
  font-weight: bold;
  font-size: 16px;
  transition: all 0.3s ease;
  box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);

}
#next_page:hover,
#prev_page:hover {
  background-color: #255d89;
  transform: scale(1.05);
}

#submit, #submit_general_feedback{
  background-color: #95C11F;
  color: white;
  border: none;
  border-radius: 30px;
  padding: 10px 24px;
  font-weight: bold;
  font-size: 16px;
  transition: all 0.3s ease;
  box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);

}

#submit:hover, #submit_general_feedback:hover{
    background-color: #739518;
    transform: scale(1.05);
}


"

)),
    
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
                     
                     draggable = FALSE,
                     
                     tags$p("Welcome to the Skills4EOSC Quality Compass, the app that
                     helps you in making your courses compliant with the Skills4EOSC Quality
                     Assurance Framework. In our mission of ensuring quality in the full life-cycle
                     of training in Open Science, we have produced two main outputs that will 
                     guide you in taking your learning resources to the next level: the QA 
                     Checklist & Guide and the Skills4EOSC Quality Self-assessment Test."),
                     
                     tags$p("By following our guidelines, you will ensure the integration of
                     the FAIR-by-design methodology, the Minimum Viable Skillsets model, 
                     key Ethical and Legal aspects and other e-learning quality criteria
                            in your Open Science course.")
                     )
              ),
              tags$div(tags$h3 (tags$b("At which stage of designing your course are you?")),
                       style= "color:grey; text-align: center;"),
              tags$br(),
              fluidRow (box (id= "checklist_about", width=6,
                             title= tags$b("First stage of designing your course"), 
                             solidHeader = TRUE,
                             
                             tags$div(style="text-align: certer; color:#b8cce0;",
                                       tags$h4("The S4E Quality Checklist & Guide")),
                             tags$br(),
                             tags$div(style = "max-width: 100%; height: auto; overflow: auto;", 
                                      htmlOutput("checklist")),
                             
                             tags$div(
                               tags$br(),
                               tags$p("The Skills4EOSC QA Checklist & Guide is an interactive infography that
                               covers the main aspects and indicators of our QA Framework.
                               It aims to help you in making your learning resource compliant
                               during its first stages of design and planification, while 
                               introducing the framework in a visual and user-friendly way. It also
                               serves as a more easy-to-read complement to the deliverable (you can find
                               the deliverable below in 'Other outputs').")
                               )
                             )
                        ,
                        box (id= "compass_about",
                             width=6,
                             title= tags$b("Last stage of designing your course"), 
                             solidHeader = TRUE,
                             tags$div( style="text-align: certer; color:#b8cce0;",
                               tags$h4("The S4E Quality Self-assessment Test")),
                             tags$br(),
                             tags$img(src = "gif_compass.gif",  
                                      style = "max-width: 100%; height: auto;"),
                             
                             tags$div(
                               tags$br(),
                               
                               tags$p("The Skills4EOSC Quality Self-assessment Test
                               covers all indicators from the Skills4EOSC Quality
                               Assurance Framework. By answering its questions, you
                               will get a report on your course compliance with the framework. 
                               This report provides scores by section and by subframework and recommendations on how to 
                               improve your learning materials. You can find the test in the 
                               sidebar menu")
                             )
                        )
                        ),
              fluidRow(
                        box (id= "outputs_about",
                             width=12,
                             title= tags$b("Other Outputs related"),
                             solidHeader= TRUE,
                             "If you want to know more about how the Skills4EOSC Quality Assurance Framework was developed
                             was built, here are some resources regarding our work and other related project's outputs:",
                             tags$li(a(href="https://doi.org/10.5281/zenodo.16748616", "Our app manual booklet")),
                             tags$li(a(href="https://zenodo.org/records/15731878", "D2.7 Community-endorsed quality assurance 
                             and certification framework for professional training and qualifications - final version")),
                             
                             tags$li(a(href="https://zenodo.org/records/15731870", "D2.6 Catalogue of OS career profiles and MVS - update")),
                             tags$li(a(href="https://zenodo.org/records/12604767", "FAIR-by-Design Learning Materials Methodology Training of Trainers"))
                             
                             
                             ))
              
       ),
      
      tabItem(tabName = "assessment",
              value="assessment",
              # Panel shown by default, hidden survey until button "clickforassessment" is clicked
              conditionalPanel("input.clickforassessment == 0", 
                               
              fluidRow (
                box(
                  id = "introassessment",
                  title = tags$b("Self-assessment test"),
                  tags$p("The Skills4EOSC Quality self-assessment
                  test covers all indicators from the Skills4EOSC Quality
                  Assurance Framework. You will navigate through 5 sections: Background
                  Information, Content and Structure, Implementation, Evaluation and
                  Licensing and Ethics. In addition to answering the questions, 
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
                  the tool."), 
                  
                  top = 0,
                  left = 0,
                  right = 0,
                  bottom = 0,
                  width = 12,
                  
                  draggable = FALSE
                  
                    #actionButton(inputId = "m", label = "Proceed", icon = NULL)   ),  ## Conditional Panel for Approval   
                  )),
              div(style = "width: 100%; text-align: center;",
                  actionButton("clickforassessment", label = "Start Self-assessment Test")
              )
              
              ),
              # Assessment shown when button is clicked
              conditionalPanel("input.clickforassessment == 1",
              tags$h2(style= "text-align: center; font-weight: bold; color: #3C8DBC;", "S4E Quality Self-assessment"),                 
              fluidRow (
                
                  div(id="sections_select", style = "display: flex; justify-content: center; gap: 10px; height: 100%;",
                  div(id="select_section0", class="section-box", "Background Information", width=NULL),
                  div(id="select_section1", class="section-box","Content & Structure", width=NULL),
                  div(id="select_section2", class="section-box","Implementation", width=NULL),
                  div(id="select_section3", class="section-box","Evaluation", width=NULL),
                  div(id="select_section4", class="section-box","Licensing & Ethics", width=NULL)
                )),
              div(id = "section_theme_container",
                  uiOutput("progress_bar_ui"),
                  fluidRow(
                    box(
                      id = "self-assessment",
                      width = 12,
                      style = "background-color: transparent !important; box-shadow: none; border: none; padding: 0;",
                      uiOutput("page_ui")
                    )
                    
                  )
              )
              )),
      tabItem(tabName = "results",
              tags$h2(style= "text-align: center; font-weight: bold; color: #3C8DBC;",
                      "Your Course Quality Report"),
              conditionalPanel(
                condition = "output.show_results",
                fluidRow(
                  box(title = "Total Score",
                      width = 12,
                      status = "primary",
                      solidHeader = TRUE,
                      valueBoxOutput("score_total"),
                      valueBoxOutput("score_minimal"),
                      valueBoxOutput("score_detailed")),
                ),
                
                fluidRow(
                  tabBox(title= "Your score in detail",
                         id="score_detail",
                         side = "right",
                         width = 12,
                         tabPanel(id="score1", "Score by section",
                                  fluidRow(box(solidHeader = TRUE,
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
                                  box(title = "Licensing & Ethics",
                                      solidHeader = TRUE,
                                      width = 6,
                                      background = NULL,
                                      style = "border-color: #E6007E; border-radius: 12px;",
                                      valueBoxOutput("score_ethics_minimal"),
                                      valueBoxOutput("score_ethics_detailed")
                                  ))
                                  ),
                         tabPanel (id="score2", "Score by sub-framework"),
                         tabPanel (id="score3", "Visualizing your score",
                                   plotOutput("score_plot"))
                      )),

                fluidRow(
                  box(title = "How Can You Improve the Quality of Your Course?",
                      width = 12,
                      status = "primary",
                      solidHeader = TRUE,
                      DT::dataTableOutput("Best_practices")
                  )
                ),
                fluidRow(
                  box(
                    title = "Your Answers",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput("results_table")
                  )
                )
              )),
      tabItem(tabName = "generalfeedback",
              # Panel shown by default, hidden survey until button "clickforsurvey" is clicked
              conditionalPanel("input.clickforsurvey == 0", 
                               
                               fluidRow (
                                 box(
                                   id = "introsurvey",
                                   title = tags$b("General Feedback survey"),
                                   tags$p("One of the core goals of the Skills4EOSC project
                                   is to support the development of high-quality, FAIR, and
                                   community-aligned learning materials for Open Science.
                                   To do this, we created the Skills4EOSC Quality Assurance
                                   Framework (QAF)—a practical reference tool designed to
                                   guide trainers, course creators, and instructional designers
                                   through essential quality indicators for building and evaluating
                                   Open Science learning materials."),
                                   tags$p("To make the framework more usable and adaptable,
                                   we’ve built two main tools:"),
                                   tags$ul(tags$li("the Checklist & Guide for planning during
                                   the course design phase,"), tags$li("and the S4E Quality 
                                   Compass app, a self-assessment tool for reviewing completed 
                                   materials.")),
                                   tags$p("As part of our commitment to continuous improvement,
                                   we are collecting feedback from real users like you. Your
                                   experience, opinions, and ideas are key to making the QAF 
                                   even more effective, practical, and user-friendly."),
                                   tags$p("This short survey will help us better understand
                                   how the QAF is being used, how helpful it has been in 
                                   supporting your work, and what can be done to improve it.
                                   Your input will contribute directly to future updates of 
                                   both the framework and the related tools. You do not need
                                   to provide any personal information."), 
                                   tags$p("Thank you for helping us strengthen Open Science
                                   training through better tools and shared practices!"),
                                   
                                   top = 0,
                                   left = 0,
                                   right = 0,
                                   bottom = 0,
                                   width = 12,
                                   
                                   draggable = FALSE
                                   )),
                               div(style = "width: 100%; text-align: center;",
                                   actionButton("clickforsurvey",
                                                label ="Start General Feedback Survey"
                                   ))
                                 ),
              conditionalPanel("input.clickforsurvey == 1",
                               tags$h3(style= "text-align: center; font-weight: bold; color: #3C8DBC;", "General Feedback survey" ),                 
                               
                               uiOutput("progress_bar2_ui"),
                               fluidRow(id= "general-feedback-container",
                                 box(
                                   id = "general-feedback",
                                   width = 12,
                                   style = "background-color: transparent !important;
                                   box-shadow: none; border: none; padding: 0;",
                                   uiOutput("feedback_ui")
                                 ))
                               )
      ),
      
      tabItem(tabName = "terms",
              fluidRow(
                box(title = "Terms of Service", width = 12,
                    htmlOutput("terms_content"))
              )
      ),
      
      tabItem(tabName = "privacy",
              fluidRow(
                box(title = "Privacy Policy", width = 12,
                    htmlOutput("privacy_content"))
              )
      )
      
      
      
      )),
    
    
    

  footer= dashboardFooter(
    left= tags$div(style= "font-size: 10px; padding:5px; padding-top:10px;position:relative;", 
                   p("Except where otherwise noted, content on this site is licensed 
                            under a", style= "display:inline;"),
                   a(href= "https://creativecommons.org/licenses/by/4.0/", "Creative Commons Attribution 4.0 
                            International License."),
                   a(href= "https://creativecommons.org/licenses/by/4.0/", 
                     tags$img(src="cc-by.png", title = "CC-by 4.0 License",
                              height = "15px")),
                   p(" Sanchez-Moreno, M. ",style= "display:inline;"),
                   a(href = "https://orcid.org/0000-0003-2148-2494",
                   tags$img(src = "orcid_logo.png",
                            title = "Orcid profile",
                            height = "15px"
                            )),
                  p(" (2025). S4E Quality Compass app (1.0.0).", style= "display:inline;")),

    right= tags$div(style="padding:8px; margin-top: 0px; position:relative; z-index:10; pointer-events:auto;",
                    # Important! Set pointer-events:auto; otherwise images links don't work
                    a(href = "https://www.skills4eosc.eu/",
                      tags$img(src="logo_S4E_pos_ext.png",  
                               title = "Skills4EOSC project", 
                               height = "30px")
                      ),
                    a(href = "https://eosc.eu/",
                      tags$img(src="logo_eosc_ext.jpeg",  
                               title = "EOSC Association", 
                               height = "30px")
                    ),
                    a(href = "https://ec.europa.eu/regional_policy/home_en",
                      tags$img(src="logo_eu_trans.png",  
                               title = "Co-funded by the European Union", 
                               height = "30px")
                    ),
                    a(href = "https://www.uc3m.es/home",
                      tags$img(src="logo_uc3m_pos_ext.png",  
                               title = "Carlos III University of Madrid", 
                               height = "30px")
                    )
    
  ))
    
)

## SERVER

server <- function(input, output, session) {
  
 ## TERMS OF SERVICE AND PROVICY POLICY 
  # HTML docs rendering
  
  output$terms_content <- renderUI({
    tags$iframe(
      src = "./ToS.html",  
      width = "100%",
      height = "600px",
      style = "border:none;"
    )
  })
  
  output$privacy_content <- renderUI({
    tags$iframe(
      src = "./Privacy-Policy.html",  
      width = "100%",
      height = "600px",
      style = "border:none;"
    )
  })
  
  
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
    sidebarMenu(id = "sidebarMenuid", selected = "about",
                menuItem("About", tabName = "about", icon = icon("home")),
                menuItem("Quality Self-assessment Test", tabName = "assessment", icon = icon("list-check")),
                if (submission_complete()) {
                  menuItem("Results", tabName = "results", icon = icon("chart-bar"))
                },
                menuItem("General Feedback", tabName = "generalfeedback", icon = icon("comments")),
                
                ## --- Spacer and legal section ---
                tags$hr(style = "border-top: 1px solid #999; margin: 20px 0;"),
                
                menuItem("Terms of Service", tabName = "terms", icon = icon("file-contract")),
                menuItem("Privacy Policy", tabName = "privacy", icon = icon("user-shield"))
                
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
        if (is.null(val) || length(val) == 0) {
          missing[[row$input_id]] <- row$question
        }
      }
      
    }
    return(missing)
  }
  
  output$page_ui <- renderUI({
    page_data <- survey %>% filter(pages == current_page())
    visible_index <- 0
    categories <- unique(page_data$category)
    section_colors <- c(
      "0. Background information" = "#333333",
      "1. Content & Structure" = "#F49200",
      "2. Implementation" = "#95C11F",
      "3. Evaluation" = "#3278B1",
      "4. Licensing & Ethics" = "#E6007E"
    )
    
    
    # Page Title box (remove the index at the beginning)
    title_div <- tags$h3(
      style = "margin-bottom: 1em; text-align: center; font-weight:bold;",
      sub("^\\d+\\.\\s*", "", current_page())
    )
    
    
    # Loop over categories
    category_boxes <- lapply(categories, function(cat) {
      cat_data <- page_data %>% filter(category == cat)
      
      # Rebuild the header row for each category box
      header_row <- div(
        style = "display: flex; flex-wrap: wrap; font-weight: bold; 
              margin-bottom: 1em; gap: 1.2em; padding-left: 2px;",
        div(style = "flex: 0 0 15%; min-width: 70px; text-align: center;", "Feedback"),
        div(style = "flex: 0 0 15%; min-width: 70px; text-align: center;", "Help notes"),
        div(style = "flex: 1; min-width: 200px;", "Question")
      )
      
      # Questions within the category
      question_ui <- lapply(seq_len(nrow(cat_data)), function(i) {
        row <- cat_data[i, ]
        qid <- row$input_id
        inputId <- paste0("q_", qid)
        
        
        show_question <- TRUE
        if (!is.na(row$dependence) && nzchar(row$dependence)) {
          parent_val <- input[[paste0("q_", row$dependence)]]
          if (is.null(parent_val) || parent_val != row$dependence_value) {
            show_question <- FALSE
          }
        }
        if (!show_question) return(NULL)
        
        visible_index <- visible_index + 1
        
        asterisk <- if (isTRUE(row$required)) {
          tags$span("*", style = "color:red; margin-left:5px;")
        } else NULL
        
        label_question <- tags$b(
          tagList(paste0(visible_index, ". ", row$question), asterisk)
        )
        
        help_icon <- if (!is.na(row$notes) && nzchar(row$notes)) {
          shinyBS::bsButton(
            inputId = paste0("help_icon_", qid),
            label = NULL,
            icon = icon("circle-info"),
            style = "info",
            size = "extra-small"
          )
        } else NULL
        
        choices <- if (row$input_type %in% c("mc", "select", "textSlider") && !is.na(row$options)) {
          trimmed <- trimws(unlist(strsplit(row$options, ";")))
          if (length(trimmed) == 0) NULL else trimmed
        } else NULL
        
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
        
        input_ui <- switch(
          row$input_type,
          "mc" = checkboxGroupInput(
            inputId = inputId,
            label = NULL,
            choices = choices,
            selected = isolate(input[[inputId]]) %||% character(0)
          ),
          
          "text" = textInput(inputId = inputId, label = NULL, value = isolate(input[[inputId]]) %||% ""),
          "textSlider" = shinyWidgets::sliderTextInput(inputId = inputId, label = NULL,
                                                       choices = choices, force_edges = TRUE),
          "select" = selectInput(inputId = inputId, label = NULL, choices = choices,
                                 selected = isolate(input[[inputId]]) %||% character(0)),
          div(style = "color:red;", paste("Unsupported input_type:", row$input_type))
        )
        
        # Question row layout
        div(
          style = "margin-bottom: 2em; display: flex; flex-wrap: wrap; align-items: flex-start; gap: 1.2em;",
          div(style = "flex: 0 0 15%; min-width: 70px; display: flex; justify-content: center;", feedback_button),
          div(style = "flex: 0 0 15%; min-width: 70px; display: flex; align-items: center; justify-content: center;",
              if (!is.null(help_icon)) help_icon else NULL),
          div(style = "flex: 1; min-width: 200px;",
              label_question,
              div(style = "margin-top: 0.5em;", input_ui))
        )
      })
      if (current_page() %in% names(section_colors)) {
        section_color <- section_colors[[current_page()]]
      } else {
        section_color <- "#3278B1"  # Fallback color (blue)
      }
      
      
      tagList(
        
        box(
          width = 12,
          title = NULL,        # No header 
          solidHeader = FALSE, # No default box header s
          collapsible = FALSE,
          style = "background-color: transparent; border: none; box-shadow: none; padding: 0;",
          
          # Custom category header outside the box
          div(
            style = paste0(
              "background-color:", section_color, ";",
              "color: white; font-weight: bold; font-size: 18px;",
              "padding: 10px 15px; border-radius: 6px; margin-bottom: 0.5em;"
            ),
            cat
          ),
          
          header_row,
          do.call(tagList, question_ui)
        )
      )
      
      
      
      
    })
    
    # Navigation buttons
    nav_buttons <- tagList(
      div(
      textOutput("error_message"),
      tags$style("#error_message { color: red; font-weight: bold; margin-bottom: 1em; }"),
      if (which(pages == current_page()) > 1)
        actionButton("prev_page", "Previous"),
      if (which(pages == current_page()) < length(pages))
        actionButton("next_page", "Next"),
      if (which(pages == current_page()) == length(pages))
        actionButton("submit", "Submit")
    ))
    
    runjs("Shiny.setInputValue('page_ui_ready', new Date().getTime());")
    
    
    tagList(
      title_div,
      category_boxes,
      nav_buttons
    )
  })
  
  
  observeEvent(input$page_ui_ready, {
    page_data <- survey %>% filter(pages == current_page())
    for (i in seq_len(nrow(page_data))) {
      row <- page_data[i, ]
      qid <- row$input_id
      if (!is.na(row$notes) && nzchar(row$notes)) {
        shinyBS::addPopover(
          session,
          id = paste0("help_icon_", qid),
          title = "Note",
          content = row$notes,
          placement = "center",
          trigger = "click"
        )
      }
    }
  })
  
  
  # Navigation logic
  observeEvent(input$next_page, {
    page_data <- survey %>% filter(pages == current_page())
    missing <- check_required_inputs(page_data, input)
    
    if (length(missing) > 0) {
      output$error_message <- renderText("Please answer all required questions before continuing")
    } else {
      output$error_message <- renderText("")
      idpage <- which(pages == current_page())
      if (idpage < length(pages)) {
        current_page(pages[idpage + 1])
        # Move to top after clicking on next (slow scroll)
        runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
        
      }
    }
  })
  # Navigation previous button
  observeEvent(input$prev_page, {
    idpage <- which(pages == current_page())
    if (idpage > 1) {
      current_page(pages[idpage - 1])
      # Move to top after clicking on previous (slow scroll)
      runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
      
    }
  })
  
  
  ## PROGRESS BAR
  # Show only when it its not in page 1 (Background info)
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
      div(style = "height: 25px;")
    }
  })
  # Function: Progress bar completion, calculate percentages by each answered question
  get_completion_percent <- reactive({
    filtered <- survey %>% filter(pages != pages[1])
    total <- nrow(filtered)
    answered <- sum(sapply(filtered$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && length(val) > 0
    }))
    
    percent <- round((answered / total) * 100)
    percent
  })
  
  observe({
    updateTextInput(session, "current_page_internal", value = current_page())
  })
  
  # Progress bar update visuals
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
      question_text <- survey$question[survey$input_id == qid]
      observeEvent(input[[paste0("flag_", qid)]], {
        showModal(modalDialog(
          title = div(
            "Feedback to question:",
            tags$div(style = "font-weight: normal; font-size: 90%; margin-top: 4px;", 
                     tags$i(question_text))
          ),
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
  # SUBMIT button behaviour (it includes dependency and required, although there are no required questions)
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
                        dom = "Bfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print"),
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
      !is.null(val) && length(val) == 1 && val[[1]] == "Not applicable"
    }, logical(1)))
    
    yes_answers <- sum(vapply(total_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && length(val) == 1 && val[[1]] == "Yes"

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
      !is.null(val) && length(val) == 1 && val[[1]] == "Not applicable"
    }, logical(1)))
    
    yes_answers <- sum(vapply(minimal_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && length(val) == 1 && val[[1]] == "Yes"

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
      !is.null(val) && length(val) == 1 && val[[1]] == "Not applicable"
    }, logical(1)))
    
    yes_answers <- sum(vapply(detailed_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && length(val) == 1 && val[[1]] == "Yes"

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
  
  # Section: Licensing & Ethics
  output$score_ethics_minimal <- renderValueBox({
    req(user_results())
    res <- score_by_section("4. Licensing & Ethics", "minimal")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon("flag"), color = "yellow")
  })
  
  output$score_ethics_detailed <- renderValueBox({
    req(user_results())
    res <- score_by_section("4. Licensing & Ethics", "detailed")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon("clipboard"), color = "blue")
  })

  ## RESULTS: Best practices
  output$Best_practices <- DT::renderDataTable({
    req(user_results())
    
    # Merge user results with survey to get best practices and section
    merged <- user_results() %>%
      left_join(
        survey %>% select(question, input_id, bestpractices, pages),
        by = c("Question" = "question"),
        # At least two rows are duplicated (Background Info) by question (different id)
        relationship = "many-to-many"
      )
    
    # Filter to keep only relevant rows
    filtered <- merged %>%
      filter(
        pages != "0. Background information",
        Answer %in% c("No", "No answer"),
        !is.na(bestpractices) & bestpractices != ""
      ) %>%
      select(Question, Level, Answer, `Best Practice` = bestpractices)
    
    # Create interactive datatable with export options
    DT::datatable(
      filtered,
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 10
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
  
  
  ## SCORE PLOT
  output$score_plot <- renderPlot({
    req(user_results())
    
    df <- user_results() %>%
      filter(Page != "0. Background information") %>%
      left_join(survey %>% select(question, subframework, pages), by = c("Question" = "question")) %>%
      mutate(
        Framework = toupper(subframework),
        SECTION = stringr::str_wrap(pages, 20),
        LevelT = if_else(Level == "detailed", "b", "a"),
        Answer = as.character(ifelse(is.na(Answer), "-1", Answer))  # ← force to character
      ) %>%
      bind_rows(
        tibble(Framework = "et", Level = "zzz", SECTION = "D", Answer = "20"),
        tibble(Framework = "et", Level = "zzz", SECTION = "D", Answer = "20"),
        tibble(Framework = "em", Level = "kkk", SECTION = "Comz", Answer = "20"),
        tibble(Framework = "em", Level = "kkk", SECTION = "Comz", Answer = "20"),
        tibble(Framework = "fb", Level = "aaa", SECTION = "F", Answer = "20"),
        tibble(Framework = "fb", Level = "aaa", SECTION = "F", Answer = "20"),
        tibble(Framework = "et", Level = "zzz", SECTION = "D", Answer = "20"),
        tibble(Framework = "et", Level = "zzz", SECTION = "D", Answer = "20"),
        tibble(Framework = "em", Level = "kkk", SECTION = "Comz", Answer = "20"),
        tibble(Framework = "em", Level = "kkk", SECTION = "Comz", Answer = "20"),
        tibble(Framework = "fb", Level = "aaa", SECTION = "F", Answer = "20"),
        tibble(Framework = "fb", Level = "aaa", SECTION = "F", Answer = "20")
      ) %>%
      mutate(
        color = if_else(Level %in% c("aaa", "kkk", "zzz"), "#ffffff00", "black"),
        alpha = if_else(Level %in% c("aaa", "kkk", "zzz"), 0, 0.7)
      )
    
    library(ggalluvial)
    ggplot(df) + theme_void() + scale_color_identity() + scale_alpha_identity() +
      
      scale_fill_manual(
        values = c(
          "Yes" = "darkgreen",
          "No" = "darkred",
          "Not applicable" = "lightblue",
          "-1" = "grey",      # for NAs or missing
          "20" = "#ffffff00"       # for your spacer rows
        )
      )+
    
      aes(
        axis1 = interaction(Answer, Level),
        axis2 = interaction(Answer, Level, Framework),
        axis3 = interaction(Answer, Level, SECTION)
      ) +
      geom_flow(aes(fill = factor(Answer)), show.legend = FALSE, alpha = 0.25) +
      geom_lode(aes(fill = factor(Answer), alpha = alpha), show.legend = FALSE) +
      geom_text(aes(label = after_stat(n), color = color), stat = "stratum") +
      geom_stratum(
        aes(axis1 = interaction(Level),
            axis2 = interaction(Level, Framework),
            axis3 = interaction(Level, SECTION),
            color = color, linetype = LevelT),
        fill = NA, show.legend = FALSE
      ) +
      geom_text(
        aes(axis1 = Level, axis2 = Framework, axis3 = SECTION, color = color,
            label = after_stat(stratum), y = after_stat(2 * y - ymin) + 1.5
),
        stat = "stratum", vjust = 0, lineheight = .8  ) +
      annotate("text", x = 1:3, y = 72,
               label = c("Total", "By Framework", "By Section"),
               size = 5, fontface = "bold")
    
  })
  
  
  
  
  
  ## GENERAL FEEDBACK SURVEY
  output$feedback_ui <- renderUI({
    categories <- unique(general_feedback$category)
    
    question_ui <- lapply(categories, function(cat) {
      cat_questions <- general_feedback %>% filter(category == cat)
      
      questions <- lapply(seq_len(nrow(cat_questions)), function(i) {
        row <- cat_questions[i, ]
        input_id <- paste0("gf_", row$input_id)
        parent_id <- if (!is.na(row$dependence)) paste0("gf_", row$dependence) else NULL
        choices <- if (!is.na(row$options)) strsplit(row$options, ";")[[1]] else NULL
        
        label <- tags$b(paste0(i, ". ", row$question))
        
        input_ui <- switch(
          row$input_type,
          "mc" = radioButtons(input_id, NULL, choices = choices, inline = FALSE),
          "select" = selectInput(input_id, NULL, choices = choices),
          "textSlider" = shinyWidgets::sliderTextInput(input_id, NULL, choices = choices, force_edges = TRUE),
          "text" = textAreaInput(input_id, NULL, placeholder = "Type your answer here...", height = "100px"),
          NULL
        )
        
        # ConditionalPanel for dependent questions
        if (!is.null(parent_id) && !is.na(row$dependence_value)) {
          condition <- sprintf("input['%s'] == '%s'", parent_id, row$dependence_value)
          conditionalPanel(condition = condition,
                           div(style = "margin-bottom: 1.5em;", label, input_ui))
        } else {
          div(style = "margin-bottom: 1.5em;", label, input_ui)
        }
      })
      
      tagList(
        
        box(
          title = NULL,
          solidHeader = FALSE,
          width = 12,
          style = "background-color: transparent !important; border: none; box-shadow: none;
          ; border-radius: 6px;",
          div(
            style = paste0(
              "background-color:#3278B1; color: white; font-weight: bold;
              font-size: 18px; padding: 10px 15px; border-radius: 6px; margin-bottom: 0.5em; margin-top: 1em;"
            ),
            cat
          ),
          div(style= "background-color: transparent !important; margin-bottom: 2em; display: flex; flex-wrap: wrap; align-items: flex-start; gap: 1.2em;", questions)
        )
      )
    })
    
    tagList(
      question_ui,
      tags$br(),
      div(style = "text-align: center;",
          actionButton("submit_general_feedback", "Submit Feedback")
      )
    )
  })
  
  
  
  #Store responses
  observeEvent(input$submit_general_feedback, {
    responses <- lapply(general_feedback$input_id, function(id) {
      value <- input[[paste0("gf_", id)]]
      if (is.null(value)) value <- NA
      value
    })
    
    result_df <- data.frame(
      input_id = general_feedback$input_id,
      question = general_feedback$question,
      answer = unlist(responses),
      stringsAsFactors = FALSE
    )
    
    print(result_df)  # Or store/save it
    
    showModal(modalDialog(
      title = "Thank you!",
      "Your feedback has been submitted successfully.",
      easyClose = TRUE
    ))
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
