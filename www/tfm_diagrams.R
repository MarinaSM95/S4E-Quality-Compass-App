library(DiagrammeR)
library(DiagrammeRsvg)
library(dplyr)
library(stringr)
library(rsvg)
library(readr)



survey <- read_csv2("./self-assessment_clean.csv")

## APP GENERAL NAVIGATION (tabs + conditional “Results”)

wf_app <-grViz('
digraph app_nav {
  graph [rankdir=LR, labelloc=t, fontsize=16]
  node  [shape=box, style="filled,rounded", fontname=Helvetica, fillcolor=white]

  subgraph cluster_tabs {
    label="Sidebar Tabs"; color="#cccccc";

    app            [label="Open the app", shape=oval, fillcolor="#f4f4f4"]
    about          [label="About/Main default", fillcolor="#f4f4f4"]
    assess_info    [label="Assessment\\n(Intro)",           fillcolor="#FCE5D0"]
    assess_run     [label="Assessment\\n(Questionnaire)",   fillcolor="#D6E8F7"]
    results        [label="Results report\\n(visible after submit)", fillcolor="#DFF3F0"]
    

    gfeedback_info [label="General Feedback\\n(Intro)", fillcolor="#FCE5D0"]
    gfeedback_run  [label="General Feedback Survey",   fillcolor="#D6E8F7"]
    gfresults      [label="Thank you message\\n(visible after submit)", fillcolor="#DFF3F0"]
    terms          [label="Terms of Service", fillcolor="#f4f4f4"]
    privacy        [label="Privacy Policy",    fillcolor="#f4f4f4"]
  }

  // Edges
  edge [fontname=Helvetica, color="#555555"]

  app -> about
  app -> assess_info
  assess_info -> assess_run [label="Start Self-assessment Test", color="#C97422"]
  assess_run   -> results   [label="Submit ✓",               color="#226399"]

  app -> gfeedback_info
  gfeedback_info -> gfeedback_run [label="Start General Feedback Survey", color="#C97422"]
  gfeedback_run  -> gfresults     [label="Submit ✓",                       color="#226399"]
  app -> terms
  app -> privacy
  
  app -> results
  
}
  
')
# Check visualisation
#wf_app

# Download svg:
svg_txt <- export_svg(wf_app)
writeLines(svg_txt, "app_navigation.svg")
rsvg_png(charToRaw(svg_txt), "app_navigation.png", width = 1400, height = 900)



## APP SELF-ASSESSMENT TEST NAVIGATION
# Overview of user workflow in self-assessment

wf_assessment <-grViz(
  ' digraph self_assessment_flow { 
  graph [rankdir=LR, labelloc=t, fontsize=80, fontname=Arial, nodesep=0.7, ranksep=0.0] node [shape=box, style="filled,rounded",
  fontname=Arial, fillcolor=white, fontsize=80, pad=0] edge [fontname=Arial, color="#555555"]
  subgraph cluster_store { label="Storage"; color="#cccccc"; 
  auth [label="Auth (gargle + SA)", fillcolor="#eef7ff"]; 
  gs_sa [label="Google Sheets:\nself-assessment", fillcolor="#eef7ff"]; 
  } 
  subgraph cluster_user { label="User"; color="#dddddd";
  u_select [label="Start Self-assessment Test",shape=oval, fillcolor="#f4f4f4"];
  u_answer [label="Answer items", fillcolor="#f4f4f4"];
  u_flag [label="Flag & comment (opt.)", fillcolor="#f4f4f4"];
  u_nav [label="Next / Previous page", fillcolor="#f4f4f4"];
  u_submit [label="Submit assessment", shape=oval, fillcolor="#f4f4f4"];
  u_read [label="Read/download results", fillcolor="#f4f4f4"];
  }
  subgraph cluster_ui { label="UI (Browser)"; color="#cccccc"; 
  ui_intro [label="Intro screen", fillcolor="#FCE5D0"];
  ui_pages [label="Questionnaire\n(dynamic visibility)", fillcolor="#D6E8F7"]; 
  ui_prog [label="Progress bar\n(raw answers)", fillcolor="#D6E8F7"]; 
  ui_results [label="Results dashboard", fillcolor="#DFF3F0"]; 
  } 
  
  subgraph cluster_server { label="Server (Shiny)"; color="#cccccc"; 
 
  s_state [label="Reactive state\n(raw inputs, feedback)", fillcolor="#fff7e6"]; 
  s_vis [label="Visibility rules\n(dependencies)", shape=diamond, fillcolor="#fff7e6"]; 
  s_live [label="Live scoring\n(raw)", fillcolor="#fff7e6"]; 
  s_validate [label="Validate required\nquestions", shape=diamond, fillcolor="#fff7e6"]; 
  s_norm [label="Normalize answers\n+ Feedback", fillcolor="#fff7e6"]; 
  s_score [label="Final scoring", fillcolor="#fff7e6"]; 
  s_maturity [label="Maturity level", fillcolor="#fff7e6"];
  s_bestp [label="Best practices", fillcolor="#fff7e6"]; 
  s_answers [label="Answers & Feedback", fillcolor="#fff7e6"]
  } 
  
  
  // Storage
  auth -> gs_sa s_norm -> gs_sa [style=dashed]
  
  // User and UI
  ui_intro -> u_select 
  u_select -> ui_pages
  u_answer -> ui_pages [style=dashed, label="re-render", fontsize=60] 
  u_flag -> ui_pages [style=dashed] 
  u_nav -> ui_pages 
  
  // UI and Server 
  ui_pages -> s_state [label="input/answers"]
  s_state -> s_vis s_vis -> ui_pages [label="hide/show",fontsize=60, color="#888888"]
  s_state -> s_live s_live -> ui_prog [label="% complete",fontsize=60, color="#888888"]
  
  // Submit path
  u_submit -> s_validate s_validate -> s_norm [label="OK", fontsize=60] 
  s_norm -> s_score 
  s_norm -> s_maturity
  s_norm-> s_bestp 
  s_norm -> s_answers
  s_maturity-> ui_results 
  s_bestp -> ui_results 
  s_score -> ui_results 
  s_answers ->ui_results
  
  ui_results-> u_read 
  
   } '
  )

#wf_assessment 

# Download svg:
svg_txt <- export_svg(wf_assessment)
writeLines(svg_txt, "app_assessment.svg")
rsvg_png(charToRaw(svg_txt), "app_assessment.png", width = 1200, height = 900)



# Detail on user workflow on self-assessment question

wf_question <- grViz('
digraph item_cycle {
  graph [rankdir=TB, labelloc=t, fontsize=16]
  node  [shape=box, style="filled,rounded", fontname=Helvetica, fillcolor=white]
  edge  [fontname=Helvetica, color="#555555"]

  s_dep   [label="Dependency met?\n(If any)", shape=diamond, fillcolor="#fff7e6"]
  q_show  [label="Render item UI", fillcolor="#D6E8F7"]
  q_ans   [label="User selects:\nYes / No \n OR: no answer", fillcolor="#f4f4f4"]
  p_upd [label="Progress bar\nupdate", fillcolor="#D6E8F7"]
  q_flag  [label="Optional:\nflag + comment"]
  q_note [label="Optional:\nnotes displayed"]
  upd     [label="Update reactive state\nraw input", fillcolor="#fff7e6"]
  nextq   [label="Next visible question", fillcolor="#D6E8F7"]
   

  s_dep -> q_show   [label="Yes or\nno dependency"]
  s_dep -> nextq    [label="No", color="#888888"]
  q_show -> q_ans
  q_ans  -> q_flag  [style=dashed]
  q_ans  -> upd
  q_ans  -> p_upd
  q_flag -> upd    [style=dashed]
  q_show -> q_flag  [style=dashed]
  q_show -> q_note  [style=dashed]

  q_ans  -> q_note  [style=dashed]
  upd -> q_ans [label="Changing\ntheir answer", style="dashed"]
  upd    -> nextq
  upd -> q_flag [label="Changing\ntheir feedback", style="dashed"]
}
')

wf_question

# Download svg:
svg_txt <- export_svg(wf_question)
writeLines(svg_txt, "app_question.svg")
rsvg_png(charToRaw(svg_txt), "app_question.png", width = 1400, height = 900)

