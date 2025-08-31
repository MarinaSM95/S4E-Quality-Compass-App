
##Delete unused libraries !!!

## LIBRARIES required

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(readr)
library(bootstrap)
library(shinyBS)
library(DT)
library(googledrive)
library(googlesheets4)
library(htmltools)
library(ggalluvial)
library(uuid)
library(ggalluvial)
library(ggplot2)
library(stringr)
library(tibble)


# Only read project .Renviron if env vars are missing (nice for local dev)
need_env <- !nzchar(Sys.getenv("GCP_SERVICE_ACCOUNT_JSON")) &&
  !nzchar(Sys.getenv("GCP_SA_JSON_CONTENT"))
if (need_env && file.exists(".Renviron")) readRenviron(".Renviron")


safe_gs4_auth <- function() {
  sa_path   <- Sys.getenv("GCP_SERVICE_ACCOUNT_JSON")
  sa_inline <- Sys.getenv("GCP_SA_JSON_CONTENT")  # JSON *content* in an env var
  
  if (nzchar(sa_path) && file.exists(sa_path)) {
    googledrive::drive_auth(path = sa_path)
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    return(invisible(TRUE))
  }
  
  if (!nzchar(sa_path) && nzchar(sa_inline)) {
    tmp <- tempfile(fileext = ".json")
    writeLines(sa_inline, tmp, useBytes = TRUE)
    try(Sys.chmod(tmp, "600"), silent = TRUE)               # restrict perms (Windows)
    on.exit(try(unlink(tmp), silent = TRUE), add = TRUE)    # clean up
    
    googledrive::drive_auth(path = tmp)
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    return(invisible(TRUE))
  }
  
  # Local dev fallback (interactive OAuth)
  googlesheets4::gs4_auth()
  googledrive::drive_auth()
  invisible(TRUE)
}
safe_gs4_auth()




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


# Decorative icons (accessible)
icon_dec <- function(name, ...) {
  # Accept: a string icon name, an <i> tag, or NULL/other (fallback to 'flag')
  if (inherits(name, "shiny.tag")) {
    x <- name
  } else if (is.character(name) && length(name) == 1 && nzchar(name)) {
    x <- icon(name, ...)
  } else {
    x <- icon("flag", ...)  # safe fallback
  }
  x$attribs$`aria-hidden` <- "true"
  x$attribs$role          <- "presentation"
  x$attribs$`aria-label`  <- NULL
  x$attribs$title         <- NULL
  x
}



# Helper (ARIA)
liveBoxOutput <- function(outputId, width = 4, label = NULL) {
  tags$div(
    role = "status", `aria-live` = "polite",
    `aria-label` = label,
    valueBoxOutput(outputId, width = width)
  )
}




ui <- dashboardPage(
  
  title="S4E Quality Compass",
 
 
  ## HEADER
  header=dashboardHeader(
    #TITLE in Header: logo
    title = tags$div(
      style = "display: flex; align-items: center; height: 80px;",
      # Accessibility: skip to main content link
      tags$a(
        href = "#main-content",        
        class = "skip-link",
        "Skip to main content"
      ),
      tags$a(
        href   = "https://www.skills4eosc.eu/",
        `aria-label` = "Go to Skills4EOSC website",
        target = "_blank", rel = "noopener noreferrer",
        
        # Horizontal logo (default)
        tags$img(
          src = "logo_S4E_neg_horizontal.png",
          alt = "",
          title = "Skills4EOSC project",
          height = "55px",
          class = "logo-expanded"
        ),
        
        # Vertical logo (collapsed)
        tags$img(
          src = "logo_S4E_neg_vertical.png",
          alt = "",
          title = "Skills4EOSC project",
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
    sidebarMenuOutput("dynamic_sidebar")
  ),
  
  ## BODY
  dashboardBody(
    useShinyjs(),
    
    
    tags$head(
      
      tags$script(HTML("document.documentElement.setAttribute('lang','en');")),
      tags$script(HTML("
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
  
")),
      
      tags$script(HTML("
  (function () {
    /* =========================
       Helpers
       ========================= */
    function isVisible(el) {
      if (!el || el.nodeType !== 1) return false;
      if (el.closest('[hidden], [aria-hidden=\"true\"]')) return false;
      var style = window.getComputedStyle(el);
      if (style.display === 'none' || style.visibility === 'hidden') return false;
      var rects = el.getClientRects();
      return rects && rects.length > 0;
    }

    function hasVisibleListItems(el) {
      var items = el.querySelectorAll('li, [role=\"listitem\"]');
      for (var i = 0; i < items.length; i++) {
        var it = items[i];
        if (!isVisible(it)) continue;
        var hasText = (it.textContent || '').trim().length > 0;
        var hasInteractive = it.querySelector('a, button, input, select, textarea, [tabindex]') !== null;
        if (hasText || hasInteractive) return true;
      }
      return false;
    }

    /* =========================
       Tabs & ARIA cleanup
       ========================= */
    function wireBootstrapTabs() {
      document.querySelectorAll('.nav-tabs, .nav-pills').forEach(function(ul){
        ul.setAttribute('role','tablist');
      });

      document.querySelectorAll('.nav-tabs li > a[data-toggle=\"tab\"], .nav-pills li > a[data-toggle=\"tab\"]').forEach(function(a){
        a.setAttribute('role','tab');
        if (!a.id) a.id = 'tab-' + Math.random().toString(36).slice(2);
        var target = a.getAttribute('href');
        if (target && target.charAt(0) === '#') {
          a.setAttribute('aria-controls', target.slice(1));
          var panel = document.querySelector(target);
          if (panel) {
            panel.setAttribute('role','tabpanel');
            panel.setAttribute('aria-labelledby', a.id);
          }
        }
      });
    }

    function scrubNonTabs() {
      document.querySelectorAll('.sidebar-menu li > a[data-toggle=\"tab\"]').forEach(function(a){
        a.removeAttribute('role');
        a.removeAttribute('aria-selected');
        a.removeAttribute('aria-controls');
      });

      document.querySelectorAll('a[role=\"tab\"]').forEach(function(a){
        if (!a.closest('.nav-tabs, .nav-pills')) {
          a.removeAttribute('role');
          a.removeAttribute('aria-selected');
          a.removeAttribute('aria-controls');
        }
      });

      document.querySelectorAll('[role=\"tabpanel\"]').forEach(function(p){
        var labelledby = p.getAttribute('aria-labelledby');
        var labelEl = labelledby && document.getElementById(labelledby);
        if (!labelEl || !labelEl.closest('.nav-tabs, .nav-pills')) {
          p.removeAttribute('role');
          p.removeAttribute('aria-labelledby');
        }
      });

      document.querySelectorAll('a[aria-selected]:not([data-toggle=\"tab\"])').forEach(function(a){
        a.removeAttribute('aria-selected');
      });
    }

    function setAriaCurrentOnActive() {
      document.querySelectorAll('a[aria-current]').forEach(function(a){
        a.removeAttribute('aria-current');
      });
      document.querySelectorAll('.sidebar-menu li.active > a').forEach(function(a){
        a.setAttribute('aria-current','page');
      });
    }

    /* =========================
       Lists: fix role=list issues
       ========================= */
    function scrubLists() {
      document.querySelectorAll('[role=\"list\"]').forEach(function(el){
        var tag = el.tagName;
        // 1) Native lists: drop role
        if (tag === 'UL' || tag === 'OL' || tag === 'MENU') {
          el.removeAttribute('role');
          return;
        }

        // 2) Empty/pointless lists: drop role
        if (!hasVisibleListItems(el)) {
          el.removeAttribute('role');
          return;
        }

        // 3) For non-native list containers, ensure children act as items
        Array.prototype.forEach.call(el.children, function(child){
          if (!isVisible(child)) return;
          if (child.matches('li, [role=\"listitem\"]')) return;
          var hasText = (child.textContent || '').trim().length > 0;
          var hasInteractive = child.querySelector('a, button, input, select, textarea, [tabindex]') !== null;
          if (hasText || hasInteractive) {
            child.setAttribute('role','listitem');
          }
        });
      });

      // 4) Strip listitem role from empty/hidden nodes
      document.querySelectorAll('[role=\"listitem\"]').forEach(function(it){
        if (!isVisible(it)) { it.removeAttribute('role'); return; }
        var text = (it.textContent || '').trim();
        var hasInteractive = it.querySelector('a, button, input, select, textarea, [tabindex]') !== null;
        if (!text && !hasInteractive) {
          it.removeAttribute('role');
        }
      });
    }
    
    
        function scrubEmptyNavbars() {
      // Only touch header navbars; do NOT touch the sidebar menu
      document.querySelectorAll('.navbar-custom-menu > ul.nav.navbar-nav').forEach(function(ul){
        var hasItems = ul.querySelector('li') !== null;

        if (!hasItems) {
          // Neutralize semantics and hide while empty
          ul.setAttribute('role', 'none');        // removes list semantics
          ul.setAttribute('aria-hidden', 'true'); // hide from AT
          ul.style.display = 'none';              // optional: hide visually
        } else {
          // Restore when items appear later
          ul.removeAttribute('role');
          ul.removeAttribute('aria-hidden');
          ul.style.display = '';
        }
      });
        }

    

    /* ==========================================
       Auto-size same-origin iframes (ToS/Privacy)
       ========================================== */
    function resizeAutosizeIframes() {
      document.querySelectorAll('iframe[data-autosize=\"true\"]').forEach(function(iframe){
        try {
          var doc = iframe.contentDocument || iframe.contentWindow.document;
          if (!doc) return;
          var h = Math.max(
            doc.body.scrollHeight, doc.documentElement.scrollHeight,
            doc.body.offsetHeight, doc.documentElement.offsetHeight
          );
          iframe.style.height = (h + 20) + 'px';
        } catch (e) { /* cross-origin guard */ }
      });
    }

        function applyAll() {
      wireBootstrapTabs();
      scrubNonTabs();
      setAriaCurrentOnActive();
      scrubLists();               // fixes generic role=list issues
      scrubEmptyNavbars();        // <-- add this line
      resizeAutosizeIframes();
    }


    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', applyAll);
    } else {
      applyAll();
    }
    window.addEventListener('load', function(){
      setTimeout(applyAll, 0);
      setTimeout(applyAll, 500);
    });
    $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"], .sidebar-menu a', applyAll);
    $(document).on('shiny:inputchanged', function(){ setTimeout(applyAll, 0); });

    new MutationObserver(function(){ applyAll(); })
      .observe(document.body, {
        subtree: true,
        childList: true,
        attributes: true,
        attributeFilter: ['class','aria-selected','role','hidden','style']
      });

    window.addEventListener('resize', function(){ setTimeout(resizeAutosizeIframes, 50); });
    setInterval(resizeAutosizeIframes, 1200);
  })();
  
  /* Scoped ARIA for help-note popovers (only .qa-help-trigger) */
(function(){
  function getTip($trigger){
    var pop = $trigger.data('bs.popover') || $trigger.data('Popover');
    if (!pop) return null;
    return (typeof pop.tip === 'function') ? $(pop.tip()) :
           (pop.tip ? $(pop.tip) : (pop.$tip ? pop.$tip : null));
  }

  function enhance(e){
    var $trg = $(e.target);
    if (!$trg.is('.qa-help-trigger')) return;       // <-- scope

    var $tip = getTip($trg);
    if (!$tip || !$tip.length) return;

    // give the popover a stable id
    var pid = $tip.attr('id') || ('pop-' + ($trg.attr('id') || Math.random().toString(36).slice(2)));
    $tip.attr('id', pid);

    // Use a lightweight landmark so we don't fight other roles
    $tip.attr({'role':'region', 'aria-label':'Help note'});

    // If there is a visible header, prefer it as the label
    var $hdr = $tip.find('.popover-title, .popover-header').first();
    if ($hdr.length){
      if (!$hdr.attr('id')) $hdr.attr('id', pid + '-label');
      $hdr.attr({'role':'heading','aria-level':'3'});
      $tip.attr({'aria-labelledby': $hdr.attr('id')}).removeAttr('aria-label');
    }

    // Mark the body as document-ish content (optional)
    $tip.find('.popover-content, .popover-body').attr({'role':'document'});

    // Connect trigger ↔ popover while open
    $trg.attr({'aria-controls': pid, 'aria-expanded': 'true'});
  }

  function teardown(e){
    var $trg = $(e.target);
    if (!$trg.is('.qa-help-trigger')) return;       // <-- scope
    $trg.removeAttr('aria-expanded aria-controls');
  }

  // Bootstrap events, scoped to our triggers only
  $(document)
    .on('inserted.bs.popover shown.bs.popover', '.qa-help-trigger', enhance)
    .on('hide.bs.popover hidden.bs.popover',     '.qa-help-trigger', teardown);
})();

"))
   ),
    tags$style(HTML("
    
  /* Make header taller */
  .main-header {
    height: 70px !important;
    background-color: #3C8DBC !important;
  }
a{color: #0645AD; text-decoration: underline; }
a:hover{color: #1d5380 ; text-decoration: none; }

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
  max-height: 44px;
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
    background-color:#b35b04;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
    border-radius: 12px;
    padding: 2em;
   
  }
  #checklist_about, #compass_about {
    background-color: #1d598a;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);
    border-radius: 12px;
    padding: 2em;
  }
  #checklist_about .box-header, #compass_about .box-header{
    color: white;
  }
  
  #outputs_about {
    background-color: #1d598a;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2);  
    border-radius: 12px;
    padding: 2em;
  }
  #outputs_about .box-header{
    color: white;
  }
  #outputs_about a {
  color: #FFFFFF !important; /* example: gold */
  text-decoration: underline; 
}

#outputs_about a:hover {
  color: #FFFFFF !important; /* example: white on hover */
  text-decoration: none; 
}

/* Style for H2 and h3 inside box headers (removes extra default spacing) */
/* new: let text wrap naturally and fill the header */
.box .box-header h2, .box-header h3 {
  margin: 0;
  padding: 0;
  font-weight: bold;
  line-height: 1.3;
  color: inherit;
  display: block;               /* wrap properly */
  white-space: normal;
  overflow-wrap: anywhere;
}


  
  
  /* Self-assessment and feedback survey intro pages */
  #introassessment, #introsurvey {
    background-color: #b35b04;
    color: white;
    box-shadow: 0 8px 18px rgba(0, 0, 0, 0.2); 
    padding: 2em;
    border-radius: 12px;
  }
  #introassessment .box-header, #introsurvey .box-header{
     
    color: white;
  }
/* Big CTA buttons: accessible, zoom-safe, motion-aware */
#clickforassessment,
#clickforsurvey {
  position: relative;                /* was: invalid 'position: center' */
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 0.75rem;

  /* Zoom / text scaling friendliness */
  padding: 0.9em 1.6em;              /* scales with font size */
  line-height: 1.3;                  /* avoid clipping */
  white-space: normal;               /* allow wrapping */
  flex-wrap: wrap;                   /* allow wrapping */
  height: auto;                      /* grow with content */
  max-width: min(90vw, 36rem);       /* keep a sensible max width */
  text-align: center;

  /* Visuals */
  background: linear-gradient(145deg, #1d598a, #1d598a);
  border: 2px solid rgba(255, 255, 255, 0.25);
  border-radius: 999px;              /* pill shape, still wraps fine */
  color: #fff;
  font-size: 1rem;                   /* was 16px; use relative units */
  font-weight: 600;
  letter-spacing: 0.5px;
  cursor: pointer;

  /* No clipping! */
  overflow: visible;                 /* was: hidden */

  /* Subtle motion; reduce if user prefers less */
  transition: transform 0.2s ease, box-shadow 0.2s ease;
  box-shadow: 0 0 20px rgba(0,0,0, 0.12);
  backdrop-filter: blur(8px);
  z-index: 1;
}



/* Inner fill so the text sits on solid color with good contrast */
#clickforassessment::after,
#clickforsurvey::after {
  content: '';
  position: absolute;
  inset: 2px;
  background: #1d598a;
  border-radius: inherit;
  z-index: -1;
  pointer-events: none;
}

/* Hover/focus interactions */
#clickforassessment:hover,
#clickforsurvey:hover {
  transform: scale(1.02);            /* gentler than 1.05 to avoid jitter */
  box-shadow: 0 0 28px rgba(0,0,0, 0.18);
}

/* Strong keyboard focus (pairs well with your global :focus-visible rules) */
#clickforassessment:focus-visible,
#clickforsurvey:focus-visible {
  outline: 3px solid #000;           /* high contrast */
  outline-offset: 3px;
}

/* Disabled look (in case you use it later) */
#clickforassessment:disabled,
#clickforsurvey:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

/* Icon inside the button */
.arrow {
  width: 22px;
  height: 22px;
  transition: transform 0.2s ease;   /* keep it snappy but short */
  color: #fff;
}
#clickforassessment:hover .arrow,
#clickforsurvey:hover .arrow {
  transform: translateX(6px);
}

/* Respect reduced motion preferences */
@media (prefers-reduced-motion: reduce) {
  #clickforassessment,
  #clickforsurvey {
    transition: none;
    transform: none;
  }
  #clickforassessment::before,
  #clickforsurvey::before,
  .arrow {
    animation: none !important;
    transition: none !important;
  }
}

/* Bigger, still accessible */
#clickforassessment,
#clickforsurvey {
  font-size: clamp(1.125rem, 1.2vw + 0.75rem, 1.5rem); /* ~18–24px responsive */
  line-height: 1.35;                   /* avoids text clipping */
  padding: 1em 1.8em;                  /* scales with font size */
  min-inline-size: 14ch;               /* at least ~14 characters wide */
  min-height: 3.4rem;                  /* ≥ 54px touch target */
}

/* Bigger icon that scales with text */
#clickforassessment .arrow,
#clickforsurvey .arrow {
  width: 1.35em;
  height: 1.35em;
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
  background-color: #b35b04 !important; /* orange */
}
#select_section2.active {
  background-color: #5D7E02 !important; /* green */
}
#select_section3.active {
  background-color: #1d598a !important; /* navy */
}
#select_section4.active {
  background-color: #BD0068 !important; /* pink */
}


/* Inactive sections text color darker for readability */
.section-box:not(.active) {
  color: #525252;
}

#progress_container {
  margin-top: 10px;
  
}
.progress {
  height: 30px;
   border-radius: 10px !important; 
}
.progress-bar {
  font-weight: bold;
  background-color: #337ab7;
  border-radius: 10px !important; 
}

/* Progress bar: readable at 200% zoom, no clipping */
.progress {
  position: relative;
  min-height: 32px;               /* give text room to breathe */
  overflow: visible !important;   /* allow overlay text to show even if bar is tiny */
  border-radius: 10px !important;
}

.progress-bar {
  display: block;
  height: 100%;
  padding: 0;                      /* text will live in ::after, not inside the bar */
  line-height: 1.2;
  white-space: normal !important;  /* allow wrapping if needed */
  overflow: visible !important;
}

/* Centered overlay label driven by a data attribute */
#progress_bar::after {
  content: attr(data-label);
  position: absolute;
  left: 50%;
  top: 50%;
  transform: translate(-50%, -50%);
  text-align: center;
  font-weight: 700;
  font-size: clamp(12px, 2.2vw, 18px);  /* scales nicely up to 200% */
  color: #000;                           /* good contrast on light track */
  pointer-events: none;                  /* don’t block clicks */
  padding: 0 6px;                        /* avoid feeling cramped */
}




#self-assessment .box-header, #general-feedback .box-header {
  display: none !important;
}
#self-assessment, #general-feedback, .tab-content, .content-wrapper {
  overflow: visible !important;
}

.flag-button {
  background: none;
  border: none;
  padding: 0;
  cursor: pointer;
}

.popover-title {
  background-color: #1d598a !important;
  color: white !important;
  font-weight: bold;
}

.popover-content {
  background-color: #f9f9f9;
  color: #333;
}

.modal-content {
  border-bottom: none;
  border-radius: 10px;
}


.modal-h2 {
 font-weight: bold;
  color: #1d598a !important;
} 


#self-assessment.box, #general-feedback,
#general-feedck-container{
  background-color: transparent !important;
  box-shadow: none !important;
  border: none !important;
}

#next_page, #prev_page, .modal-footer .btn,
.modal-footer .btn-default,
.modal-footer .action-button {
min-width:44px;
min-height:44px;
  background-color: #1d598a;
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
#prev_page:hover, .modal-footer .btn:hover,
.modal-footer .btn-default,
.modal-footer .action-button {
  background-color: #255d89;
  transform: scale(1.05);
}

#submit, #submit_general_feedback{
  background-color: #567308;
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

/* ===== Accessibility helpers & focus styles (final) ===== */

/* SR-only utility */
.sr-only {
  position: absolute !important;
  width: 1px; height: 1px;
  padding: 0; margin: -1px;
  overflow: hidden; clip: rect(0,0,0,0);
  white-space: nowrap; border: 0;
}

/* Show focus only for keyboard navigation */
:focus { 
  outline: none;                 /* don't show on mouse click */
}

/* Reveal outlines only when keyboard users tab */
:focus-visible {
  outline: 3px solid #000 !important;
  outline-offset: 2px;
  box-shadow: none !important;
}

/* Bootstrap tends to add shadows; kill them on plain :focus
   (focus outline still shows via :focus-visible) */
a:focus,
button:focus,
.btn:focus,
.action-button:focus,
.form-control:focus,
input:focus,
select:focus,
textarea:focus,
.pagination > li > a:focus,
.pagination > li > span:focus {
  box-shadow: none !important;
  outline: none;                 /* rely on :focus-visible instead */
}

/* Keep touch-target sizing AND strong outline for keyboard focus on key buttons */
#next_page:focus-visible,
#prev_page:focus-visible,
#submit:focus-visible,
#submit_general_feedback:focus-visible {
  outline: 3px solid #000 !important;
  outline-offset: 2px;
}

/* --- Skip link: hidden by default; shown only for keyboard users --- */
.skip-link {
  position: absolute;
  left: 8px; top: 8px;
  transform: translateY(-200%);     /* hide off-canvas without layout jank */
  transition: transform .15s ease;
  z-index: 2000;
  background: #fff;
  color: #000;
  padding: 10px 14px;
  border: 2px solid #000;
  border-radius: 6px;
  font-weight: 700;
}

/* Do NOT reveal on generic :focus (mouse click) */
.skip-link:focus {
  transform: translateY(-200%);
  outline: none;
}

/* Reveal only for keyboard users */
.skip-link:focus-visible {
  transform: translateY(0);
  outline: none;                   /* border already provides contrast */
}

/* --- Footer links: WCAG contrast and clear focus state --- */
.main-footer a {
  color: #0645AD !important;       /* higher-contrast blue */
  text-decoration: underline;
  outline: none;
}

.main-footer a:hover,
.main-footer a:focus-visible {
  color: #000000 !important;        /* max contrast on hover/keyboard focus */
  text-decoration: none;
}

/* Accessible color for slider bars (self-assessment + general feedback) */
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-to,
.irs-from {
  background-color: #1d598a !important;   /* main bar + handle */
  border-color: #1d598a !important;
  color: #fff !important;                 /* text labels on handles */
}

.irs-handle > i:first-child {
  background-color: #1d598a !important;   /* round handle dot */
  
}

/* Optional: make the inactive track lighter for contrast */
.irs-line,
.irs-line-left,
.irs-line-mid,
.irs-line-right {
  background-color: #e0e0e0 !important;   /* light gray background */
}

/* Only these report boxes get dark blue headers with white text */
#results-main #maturitylvl > .box-header,
#results-main #score1    > .box-header,
#results-main #score2    > .box-header,
#results-main #score3    > .box-header,
#results-main #Best_practices > .box-header,
#results-main #results_table  > .box-header {
  background-color: #1d598a !important;
  color: #ffffff !important;
  border-bottom: none !important;
}

#results-main #maturitylvl > .box-header .box-title,
#results-main #score1    > .box-header .box-title,
#results-main #score2    > .box-header .box-title,
#results-main #score3    > .box-header .box-title,
#results-main #Best_practices > .box-header .box-title,
#results-main #results_table  > .box-header .box-title {
  color: #ffffff !important;
}

/* Tab pills in the Results 'Your score' tabBox */
#score_detail .nav-tabs > li > a {
  background-color: #ffffff;          /* default white */
  color: #1d598a;                     /* dark blue text */
  border: 1px solid #1d598a;          /* dark blue border */
  font-weight: bold;
  border-radius: 20px;                /* pill shape */
  margin-right: 6px;
  padding: 6px 14px;
  transition: background-color 0.2s, color 0.2s;
}

#score_detail .nav-tabs > li.active > a,
#score_detail .nav-tabs > li.active > a:focus,
#score_detail .nav-tabs > li.active > a:hover {
  background-color: #1d598a !important;  /* dark blue active */
  color: #ffffff !important;             /* white text */
  border: 1px solid #1d598a !important;
}

#score_detail .nav-tabs > li > a:hover {
  background-color: #eaf2f8;           /* subtle blue hover */
  color: #1d598a;
}

/* Results section: force all inner box headers to dark blue */
#results-main .box > .box-header {
  background-color: #1d598a !important;
  color: #ffffff !important;
  border-bottom: none !important;
  border-radius: 2px 2px 0 0;  /* smooth rounded top */
  padding: 8px 12px;
}

/* Ensure header title text/icons are white */
#results-main .box > .box-header .box-title,
#results-main .box > .box-header .fa,
#results-main .box > .box-header .glyphicon,
#results-main .box > .box-header .ion {
  color: #ffffff !important;
}

/* But EXCLUDE the 'Your score' tabBox header from this styling */
#results-main #score_detail > .nav-tabs-custom > .box-header {
  background-color: #ffffff !important; /* keep white */
  color: #1d598a !important;           /* dark blue text */
  border-bottom: 2px solid #1d598a !important;
}
#results-main #score_detail > .nav-tabs-custom > .box-header .box-title {
  color: #1d598a !important;
}

/* === Brand palette for shinydashboard ValueBoxes === */
.small-box[class*='bg-'] {
  color: #ffffff !important;                 /* force white text */
}
.small-box .icon > i,
.small-box .icon > i.fa,
.small-box .icon > i.glyphicon,
.small-box .icon > i.ion {
  color: #ffffff !important;                 /* white icons */
  opacity: 0.95;
}

/* Brand blue family */
.bg-blue, .bg-aqua, .bg-light-blue {
  background-color: #1d598a !important;
}
.bg-navy {
  background-color: #154a7a !important;
}

/* Warm (orange) */
.bg-yellow {
  background-color: #b35b04 !important;
}

/* Greens */
.bg-green, .bg-olive {
  background-color: #5D7E02 !important;
}

/* Magentas/Reds */
.bg-red, .bg-fuchsia, .bg-maroon {
  background-color: #BD0068 !important;
}

/* Supporting violet if you ever use it */
.bg-purple {
  background-color: #6a1b9a !important;
}

/* Make sure inner text stays white and readable */
.small-box .inner h3,
.small-box .inner p,
.small-box .inner .small-box-footer {
  color: #ffffff !important;
}



"

)),
tags$main(
  id = "main-content",
  tabindex = "-1",  # so it can receive focus when skipped to
  role = "main",
    
    tabItems(
      tabItem(tabName = "about", 
              tags$h1(id= "about-h1", "About page", class = "sr-only"), # Title only for SR
              div(role = "region", `aria-labelledby` = "about-h1", id = "about-main",# Focus
                  tags$h2("Welcome section", class="sr-only"),
              fluidRow( width="100%",
                box (id= "intro_about",
                     title ="Welcome",
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
              tags$div(tags$h2 ("At which stage of designing your course are you?"),
                       style= "color:grey; text-align: center; font-weight:bold"),
              tags$br(),
              fluidRow (box(
                id = "checklist_about",
                width = 6,
                title = "First stage of designing your course",
                
                solidHeader = TRUE,
                tags$div(style="text-align: certer; color:#FFFFFF;",
                         tags$b("The S4E Quality Checklist & Guide")),
                tags$br(),
                tags$div(style = "max-width: 100%; height: auto; overflow: auto;", 
                htmlOutput("checklist")),
                tags$div(tags$br(),
                tags$p("The Skills4EOSC QA Checklist & Guide is an interactive infography that
                       covers the main aspects and indicators of our QA Framework.
                       It aims to help you in making your learning resource compliant
                       during its first stages of design and planification, while 
                       introducing the framework in a visual and user-friendly way. It also
                       serves as a more easy-to-read complement to the deliverable (you can find
                       the deliverable below in 'Other outputs').")
                       )
                             ),
                 box (id= "compass_about",
                     width=6,
                     title= "Last stage of designing your course", 
                     solidHeader = TRUE,
                     tags$div( style="text-align: certer; color:#FFFFFF;",
                       tags$h4("The S4E Quality Self-assessment Test")),
                     tags$br(),
                     tags$img(src = "gif_compass.gif",
                              alt="Animated preview of the S4E Quality Self‑assessment flow",
                              title = "Self-assessment test gif",
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
                     ))),
              fluidRow(
                        box (id= "outputs_about",
                             width=12,
                             title= "Other Outputs related",
                             solidHeader= TRUE,
                             tags$p("If you want to know more about how the Skills4EOSC Quality Assurance Framework was developed
                             was built, here are some resources regarding our work and other related project's outputs:"),
                             
                             tags$ul(
                               tags$li(a(href="https://doi.org/10.5281/zenodo.16748616", "Skills4EOSC Quality Compass App: Improving your Open Science training course (booklet)")),
                               tags$li(a(href="https://zenodo.org/records/15731878", "D2.7 Community-endorsed quality assurance 
                             and certification framework for professional training and qualifications - final version")),
                               tags$li(a(href="https://zenodo.org/records/15731870", "D2.6 Catalogue of OS career profiles and MVS - update")),
                               tags$li(a(href="https://zenodo.org/records/12604767", "FAIR-by-Design Learning Materials Methodology Training of Trainers"))
                            )))
              )
              
       ),
      
      tabItem(tabName = "assessment",
              value="assessment",
              # Panel shown by default, hidden survey until button "clickforassessment" is clicked
              conditionalPanel("input.clickforassessment == 0", 
                               tags$h1(id = "assess-h1-intro","Self-assessment test intro", class = "sr-only"),# Title only for SR  
                               div(role = "region", `aria-labelledby` = "assess-h1-intro", id = "assessment-intro",
                                   tags$h2("Self-assessment test intro section", class="sr-only"),
                                                
                                  fluidRow (
                                    box(
                                      id = "introassessment",
                                      title = "Self-assessment test",
                                      tags$p("The Skills4EOSC Quality self-assessment
                                      test covers all indicators from the Skills4EOSC Quality
                                      Assurance Framework. During the test, you will navigate through 5 sections: Background
                                      Information, Content and Structure, Implementation, Evaluation and
                                      Licensing and Ethics. In addition to answering the questions, 
                                      we encourage you to provide any comments or doubts you may have about a question.
                                      Just click on the flag next to each question, write your comments and send them."),
                                      tags$p("After submitting your answers, you
                                      will get a report on your course compliance with our quality framework. 
                                      This report provides your scores by section and by subframework and recommendations on
                                      how to improve your learning materials."),
                                      tags$p("Regarding personal data collection, you don't need to provide
                                      any personal information you don't wish to be stored. You will still
                                      receive your report and results regardless of providing personal 
                                      information. Any information you decide to provide together with and your answers and
                                      feedback will be store for research purposes and to keep improving
                                      the tool. For more information about this, visit our Privacy Policy page 
                                             in the sidebar menu"), 
                                      
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
                                  ))
              
              ),
              # Assessment shown when button is clicked
              conditionalPanel("input.clickforassessment == 1",
              tags$h1(id = "assess-h1-test", style= "text-align: center; font-weight: bold; color: #3C8DBC;", "S4E Quality Self-assessment"),                 
              div(role = "region", `aria-labelledby` = "assess-h1-test", id = "assessment-test",
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
                    ))),
      tabItem(
        tabName = "results",
        tags$h1(
          id    = "results-h1",
          style = "text-align: center; font-weight: bold; color: #3C8DBC;",
          "Your Course Quality Report"
        ),
        tags$p(class = "sr-only", "This panel shows your overall maturity level as a value box."),
        
        div(role = "region", `aria-labelledby` = "results-h1", id = "results-main",
        tags$h2("Self-assessment results page", class="sr-only"),
          
          tags$br(),
          
          conditionalPanel(
            condition = "output.show_results",
            
            # Maturity level
            fluidRow(
              box(
                id = "maturitylvl",
                title = "Your course maturity level",
                width = 12, status = "primary", solidHeader = TRUE,
                div(style = "text-align:center;", 
                    tags$p(class = "sr-only", "Your course maturity level"),
                    liveBoxOutput("maturity_level",
                                  label="Quality maturity level of your course", width = 12))
              )
            ),
            
            # Scores
            fluidRow(
              tabBox(
                header = tags$span("Your score",
                                   style = "background-color: #ffffff; color: #1d598a; font-size: 20px; font-weight: bold;"
                ),
                id = "score_detail",
                side = "right",
                width = 12,
                type = "pills",
                
                tabPanel(
                  id = "scoretotal", "Total score", 
                  tags$p(class = "sr-only", "This tab shows your total, minimal, and detailed scores."),
                  
                  fluidRow(
                    tags$br(),
                  box(
                    title= "Your overall score",
                    width = 12,
                    solidHeader = FALSE,
                    tags$p(class = "sr-only", "The following value boxes display total counts of Yes answers."),
                    liveBoxOutput("score_total", label = "Total score. Displays number of Yes answers out of total questions."),
                    liveBoxOutput("score_minimal", label = "Minimal Level score. Displays number of Yes answers out of total minimal questions."),
                    liveBoxOutput("score_detailed", label = "Detailed Level score. Displays number of Yes answers out of total detailed questions.")
                  )
                )),
                
                tabPanel(
                  id = "score1", "Score by section",
                  tags$p(class = "sr-only", "This tab shows minimal and detailed scores for each section."),
                  
                  fluidRow(
                    tags$br(),
                    box(
                      title = "Content & Structure", solidHeader = TRUE, background = NULL,
                      width = 6, status = "primary",
                      style = "border-color: #95C11F; border-radius: 12px;",
                      tags$p(class = "sr-only", "Content & Structure: minimal and detailed value boxes."),
                      
                      liveBoxOutput("score_content_minimal",  width = 6,
                                    label = "Minimal Level score in section 1. Displays number of Yes answers out of total minimal questions in Content & Structure."),
                      liveBoxOutput("score_content_detailed", width = 6,
                                    label = "Detailed Level score in section 1. Displays number of Yes answers out of total detailed questions in Content & Structure.")
                    ),
                    box(
                      title = "Implementation", solidHeader = TRUE, background = NULL,
                      width = 6, status = "primary",
                      style = "border-color: #95C11F; border-radius: 12px;",
                      tags$p(class = "sr-only", "Implementation: minimal and detailed value boxes."),
                      liveBoxOutput("score_implementation_minimal",  width = 6,
                                    label = "Minimal Level score in section 2. Displays number of Yes answers out of total minimal questions in Implementation."),
                      liveBoxOutput("score_implementation_detailed", width = 6,
                                    label = "Detailed Level score in section 2. Displays number of Yes answers out of total detailed questions in Implementation.")
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Evaluation", solidHeader = TRUE, background = NULL,
                      width = 6, status = "primary",
                      style = "border-color: #1d598a; border-radius: 12px;",
                      tags$p(class = "sr-only", "Evaluation: minimal and detailed value boxes."),
                      liveBoxOutput("score_evaluation_minimal",  width = 6,
                                    label = "Minimal Level score in section 3. Displays number of Yes answers out of total minimal questions in Evaluation."),
                      liveBoxOutput("score_evaluation_detailed", width = 6,
                                    label = "Detailed Level score in section 3. Displays number of Yes answers out of total detailed questions in Evaluation.")
                    ),
                    box(
                      title = "Licensing & Ethics", solidHeader = TRUE, background = NULL,
                      width = 6, status = "primary",
                      style = "border-color: #E6007E; border-radius: 12px;",
                      tags$p(class = "sr-only", "Licensing and Ethics: minimal and detailed value boxes."),
                      
                      liveBoxOutput("score_ethics_minimal",  width = 6,
                                    label = "Minimal Level score in section 4. Displays number of Yes answers out of total minimal questions in Licensing & Ethics."),
                      liveBoxOutput("score_ethics_detailed", width = 6,
                                    label = "Detailed Level score in section 4. Displays number of Yes answers out of total detailed questions in Licensing & Ethics.")
                    )
                  )
                ),
                
                tabPanel(
                  id = "score2", "Score by sub-framework",
                  fluidRow(
                    tags$br(),
                    box(
                      title = "ESSENTIAL", solidHeader = TRUE, background = NULL,
                      width = 6, status = "primary",
                      style = "border-color: #F49200; border-radius: 12px;",
                      tags$p(class = "sr-only", "Essential sub-framework: minimal and detailed value boxes."),
                      liveBoxOutput("score_essential_minimal",  width = 6,
                                    label = "Minimal Level score in the Essential Subframework. Displays number of Yes answers out of total minimal questions in Essential."),
                      liveBoxOutput("score_essential_detailed", width = 6,
                                    label= "Detailed Level score in the Essential Subframework. Displays number of Yes answers out of total detailed questions in Essential.")
                    ),
                    box(
                      title = "FAIR", solidHeader = TRUE, background = NULL,
                      width = 6, status = "primary",
                      style = "border-color: #95C11F; border-radius: 12px;",
                      tags$p(class = "sr-only", "FAIR sub-framework: minimal and detailed value boxes."),
                      
                      liveBoxOutput("score_fair_minimal",  width = 6,
                                    label= "Minimal Level score in the FAIR Subframework. Displays number of Yes answers out of total minimal questions in FAIR"),
                      liveBoxOutput("score_fair_detailed", width = 6,
                                    "Detailed Level score in the FAIR subframework. Displays number of Yes answers out of total detailed questions in FAIR.")
                    )
                  ),
                  fluidRow(
                    box(
                      title = "MVS", solidHeader = TRUE, background = NULL,
                      width = 6, status = "primary",
                      style = "border-color: #1d598a; border-radius: 12px;",
                      tags$p(class = "sr-only", "MVS sub-framework: minimal and detailed value boxes."),
                      liveBoxOutput("score_mvs_minimal",  width = 6,
                                    label= "Minimal Level score in the MVS Subframework. Displays number of Yes answers out of total minimal questions in MVS."),
                      liveBoxOutput("score_mvs_detailed", width = 6,
                                    label= "Detailed Level score in the MVS Subframework. Displays number of Yes answers out of total detailed questions in MVS.")
                    ),
                    box(
                      title = "ELSI", solidHeader = TRUE, background = NULL,
                      width = 6, status = "primary",
                      style = "border-color: #E6007E; border-radius: 12px;",
                      tags$p(class = "sr-only", "ELSI sub-framework: minimal and detailed value boxes."),
                      liveBoxOutput("score_elsi_minimal",  width = 6,
                                    label= "Minimal Level score in the ELSI Subframework. Displays number of Yes answers out of total minimal questions in ELSI."),
                      liveBoxOutput("score_elsi_detailed", width = 6,
                                    label= "Detailed Level score in the ELSI Subframework. Displays number of Yes answers out of total detailed questions in ELSI")
                    )
                  )
                )
              )
            ),
            
            # Plot
            fluidRow(
              box(
                id = "score3", title = "Visualizing your score",
                width = 12, status = "primary", solidHeader = TRUE,
                plotOutput("score_plot", height = "600px")
              )
            ),
            
            # Best practices
            fluidRow(
              box(
                title = "How can you improve the quality of your course?",
                width = 12, status = "primary", solidHeader = TRUE,
                DT::dataTableOutput("Best_practices")
              )
            ),
            
            # Answers
            fluidRow(
              box(
                title = "Your Answers",
                width = 12, status = "primary", solidHeader = TRUE,
                DT::dataTableOutput("results_table")
              )
            )
          ) # conditionalPanel
        )   # main
      )
      ,
      tabItem(tabName = "generalfeedback",
              # Panel shown by default, hidden survey until button "clickforsurvey" is clicked
              conditionalPanel("input.clickforsurvey == 0", 
                               tags$h1(id="gf-h1", class="sr-only", "Introduction to General Feedback Survey"),
                               
                               div(role="region", `aria-labelledby`="gf-h1", id="gf-main",
                                   tags$h2("General feedback survey intro section", class="sr-only"),
                               
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
                                                label ="Start General Feedback Survey"))
                                 )),
              conditionalPanel("input.clickforsurvey == 1",
                               tags$h1(id="gf-h1-active",
                                       style="text-align:center; font-weight:bold; color:#3C8DBC;",
                                       "General Feedback survey"),
                               div(role = "region", `aria-labelledby` = "gf-h1-active", id = "gf-survey",
                               uiOutput("progress_bar2_ui"),
                               fluidRow(id= "general-feedback-container",
                                 box(
                                   id = "general-feedback",
                                   width = 12,
                                   style = "background-color: transparent !important;
                                   box-shadow: none; border: none; padding: 0;",
                                   uiOutput("feedback_ui"))
                                 )
                               ))
              
      ),
      
      tabItem(tabName = "terms",
              tags$h1(id="terms-h1", class="sr-only", "Terms of Service"),
              div(role="region", `aria-labelledby`="terms-h1", id="terms-main",
                  tags$h2(class="sr-only", "ToS section"),
              fluidRow(
                box(title = tags$h2("Terms of Service"), status = "primary",
                    style="overflow:auto;",
                    solidHeader = TRUE, width = 12, htmlOutput("terms_content"))
              ))
      ),
      
      tabItem(tabName = "privacy",
              tags$h1(id="privacy-h1", class="sr-only", "Privacy Policy"),
              div(role="region", `aria-labelledby`="privacy-h1", id="privacy-main",
                  tags$h2(class="sr-only", "Privacy Policy section"),
              fluidRow(
                box(title = "Privacy Policy", status = "primary",
                    solidHeader = TRUE, width = 12, htmlOutput("privacy_content"))
              ))
      )
      
      
      
      ))),
    
    
    

  footer= dashboardFooter(
    
    left= tags$div(style= "font-size: 12px; padding:5px; padding-top:10px;position:relative;", 
                   p("Except where otherwise noted, content on this site is licensed 
                            under a", style= "display:inline;"),
                   a(href= "https://creativecommons.org/licenses/by/4.0/", "Creative Commons Attribution 4.0 
                            International License."),
                   a(href= "https://creativecommons.org/licenses/by/4.0/", 
                     tags$img(src="cc-by.png", alt = "Image of CC-by 4.0 License", title ="CC-by 4.0",
                              height = "15px")),
                   p(" Sanchez-Moreno, M. ",style= "display:inline;"),
                   a(
                     href = "https://orcid.org/0000-0003-2148-2494",
                     `aria-label` = "ORCID profile of Sanchez-Moreno, M.",
                     rel  = "noopener noreferrer",
                     tags$img(
                       src = "orcid_logo.png",
                       alt = "",
                       title = "ORCID profile",
                       height = "15px"
                     )),
                  p(" (2025). S4E Quality Compass app (1.0.0).", style= "display:inline;")),

    right = tags$div(
      id = "footer-right",
      style = "padding:8px; margin-top:0; position:relative; z-index:10; pointer-events:auto;",
      
      a(
        href = "https://www.skills4eosc.eu/",
        `aria-label` = "Skills4EOSC website",
        target = "_blank", rel = "noopener noreferrer",
        tags$img(
          src = "logo_S4E_pos_ext.png",
          alt = "",
          title = "Skills4EOSC project",
          height = "44px"
        )
      ),
      
      a(
        href = "https://eosc.eu/",
        `aria-label` = "EOSC Association website",
        target = "_blank", rel = "noopener noreferrer",
        tags$img(
          src = "logo_eosc_ext.jpeg",
          alt = "",
          title = "EOSC Association",
          height = "44px"
        )
      ),
      
      a(
        href = "https://ec.europa.eu/regional_policy/home_en",
        `aria-label` = "European Union Regional Policy website",
        target = "_blank", rel = "noopener noreferrer",
        tags$img(
          src = "logo_eu_trans.png",
          alt = "",
          title = "Co-funded by the European Union",
          height = "44px"
        )
      ),
      
      a(
        href = "https://www.uc3m.es/home",
        `aria-label` = "Carlos III University of Madrid website",
        target = "_blank", rel = "noopener noreferrer",
        tags$img(
          src = "logo_uc3m_pos_ext.png",
          alt = "",
          title = "Carlos III University of Madrid",
          height = "44px"
        )
      )
    )
    
    
  )
    
)

# Set language attribute


## SERVER

server <- function(input, output, session) {
  
  # Score_total_level FUNCTION
  # Help function to calculate scores (report from self-assessment test)
  score_total_level <- function(levels = c("minimal", "detailed"),
                                section = NULL,
                                subfw = NULL) {
    all_relevant <- survey %>%
      filter(minimal_detailed %in% levels) %>%
      { if (!is.null(section)) filter(., pages == section) else . } %>%
      { if (!is.null(subfw))  filter(., subframework == subfw) else . }
    
    # Denominator: total questions in the group minus "Not applicable"
    not_applicable <- sum(vapply(all_relevant$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && val == "Not applicable"
    }, logical(1)))
    total <- nrow(all_relevant) - not_applicable
    
    # Numerator: count "Yes" only for questions that are currently visible
    visible_questions <- all_relevant %>%
      rowwise() %>%
      filter(is.na(dependence) || input[[paste0("q_", dependence)]] == dependence_value) %>%
      ungroup()
    
    yes_answers <- sum(vapply(visible_questions$input_id, function(id) {
      val <- input[[paste0("q_", id)]]
      !is.null(val) && val == "Yes"
    }, logical(1)))
    
    list(yes = yes_answers, total = total)
  }
  
  # Helper: compute maturity level from current scores
  maturity_level_info <- reactive({
    # Reuse your score_total_level() helper
    total   <- score_total_level(c("minimal", "detailed"))
    minimal <- score_total_level("minimal")
    detailed<- score_total_level("detailed")
    
    pct_total <- if (total$total > 0) total$yes / total$total else 0
    
    # Adjustable cutoff for "Defined"
    cutoff_defined <- 0.50  # 50% — tweak as you like
    
    # Decide level
    if (minimal$total > 0 && minimal$yes == minimal$total &&
        detailed$total > 0 && detailed$yes == detailed$total) {
      list(
        level = 4,
        title = "Level 4 – Optimized",
        desc  = "Full detailed & minimal compliance; feedback loops; continuous improvement.",
        color = "purple",
        icon  = icon_dec("ranking-star")
      )
    } else if (minimal$total > 0 && minimal$yes == minimal$total) {
      list(
        level = 3,
        title = "Level 3 – Managed",
        desc  = "Full minimal compliance + learning resource reviews.",
        color = "maroon",
        icon  = icon_dec("hand-fist")
      )
    } else if (pct_total >= cutoff_defined) {
      list(
        level = 2,
        title = "Level 2 – Defined",
        desc  = "Partial use of QAF; some minimal indicators implemented.",
        color = "olive",
        icon  = icon_dec("thumbs-up")
      )
    } else {
      list(
        level = 1,
        title = "Level 1 – Initial",
        desc  = "No QA in place; ad hoc training materials.",
        color = "red",
        icon  = icon_dec("face-grin-stars")
      )
    }
  })
  
  
  
  
  

  
 ## TERMS OF SERVICE AND PROVICY POLICY 
  # HTML docs rendering
  
  output$terms_content <- renderUI({
    tags$iframe(
      src   = "./ToS.html",
      title = "Terms of Service",
      width = "100%",
      style = "border:none; width:100%; height:1px; min-height:60vh; overflow:auto;!important",
      `data-autosize` = "true"
    )
  })
  
  output$privacy_content <- renderUI({
    tags$iframe(
      src   = "./Privacy-Policy_accessible.html",
      title = "Privacy Policy",
      width = "100%",
      style = "border:none; width:100%; height:1px; min-height:60vh;",
      `data-autosize` = "true"
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
    sidebarMenu(
      id = "sidebarMenuid",
      selected = "about",
      menuItem("About", tabName = "about", icon = icon_dec("home")),
      menuItem("Quality Self-assessment", tabName = "assessment", icon = icon_dec("list-check")),
      if (submission_complete()) menuItem("Results", tabName = "results", icon = icon_dec("chart-bar")),
      menuItem("General Feedback", tabName = "generalfeedback", icon = icon_dec("comments")),
      tags$hr(style = "border-top: 1px solid #999; margin: 20px 0;"),
      menuItem("Terms of Service", tabName = "terms", icon = icon_dec("file-contract")),
      menuItem("Privacy Policy", tabName = "privacy", icon = icon_dec("user-shield"))
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
      "1. Content & Structure"    = "#b35b04",
      "2. Implementation"         = "#5D7E02",
      "3. Evaluation"             = "#1d598a",
      "4. Licensing & Ethics"     = "#BD0068"
    )
    
    # Page title (visual)
    title_div <- tagList(
      tags$h2(
        id = "section-title",
        `aria-describedby` = "section-title-desc",
        style = "margin-bottom: 1em; text-align: center; font-weight:bold;",
        sub("^\\d+\\.\\s*", "", current_page())
      ),
      tags$p(
        id = "section-title-desc",
        class = "sr-only",
        "This section lists the questions for the selected part of the self‑assessment."
      )
    )
    
    # Loop over categories
    category_boxes <- lapply(categories, function(cat) {
      cat_data <- page_data %>% filter(category == cat)
      
      # Header row for columns
      header_row <- div(
        style = "display: flex; flex-wrap: wrap; font-weight: bold; 
               margin-bottom: 1em; gap: 1.2em; padding-left: 2px;",
        div(style = "flex: 0 0 15%; min-width: 70px; text-align: center;", "Feedback"),
        div(style = "flex: 0 0 15%; min-width: 70px; text-align: center;", "Help notes"),
        div(style = "flex: 1; min-width: 200px;", "Question")
      )
      
      # Questions within the category
      question_ui <- lapply(seq_len(nrow(cat_data)), function(i) {
        row   <- cat_data[i, ]
        qid   <- row$input_id
        inputId <- paste0("q_", qid)
        
        
        # Dependency visibility
        show_question <- TRUE
        if (!is.na(row$dependence) && nzchar(row$dependence)) {
          parent_val <- input[[paste0("q_", row$dependence)]]
          if (is.null(parent_val) || parent_val != row$dependence_value) show_question <- FALSE
        }
        if (!show_question) return(NULL)
        
        # Increment visual index (for numbering in label text)
        visible_index <<- visible_index + 1
        
        # Required marker (visual asterisk if you ever show labels visually)
        asterisk <- if (isTRUE(row$required)) tags$span("*", style = "color:red; margin-left:5px;") else NULL
        
        # Build *text* for the label that SR will read
        label_text <- HTML(paste0(visible_index, ". ", row$question))
        
        # Unique ids for accessible associations
        label_id <- paste0(inputId, "-label")
        help_id  <- paste0("help_note_", qid)
        
        # visible label id
        vis_label_id <- paste0(inputId, "-vlabel")
        
        # Help button (popover trigger)
        help_btn <- if (!is.na(row$notes) && nzchar(row$notes)) {
          shinyBS::bsButton(
            inputId = paste0("help_icon_", qid),
            label   = NULL,
            icon    = icon_dec("circle-info"),
            style   = "info",
            size    = "small"
          ) |> htmltools::tagAppendAttributes(
            class = "qa-help-trigger", 
            `aria-label`   = "Show help note for this question",
            `aria-describedby` = help_id
          )
        } else NULL
        
        # Feedback flag (decorative icon-only button, already ARIA-labelled)
        has_feedback <- !is.null(feedback_store[[qid]])
        flag_icon <- if (has_feedback) {
          icon_dec("flag", class = "fa-solid",  style = "color:red;")
        } else {
          icon_dec("flag", class = "fa-regular", style = "color:red;")
        }
        flag_id     <- paste0("flag_", qid)
        flag_label  <- if (has_feedback) "Feedback submitted. Edit feedback" else "Flag this question and add feedback"
        flag_pressed <- if (has_feedback) "true" else "false"
        feedback_button <- actionButton(
          inputId = flag_id, label = NULL, icon = flag_icon,
          style   = "background: none; border: none;", class = "pull-left"
        ) |> htmltools::tagAppendAttributes(
          `aria-label`   = flag_label,
          `aria-pressed` = flag_pressed,
          title          = flag_label
        )
        
        # Options for controls
        choices <- if (row$input_type %in% c("mc", "select", "textSlider") && !is.na(row$options)) {
          trimmed <- trimws(unlist(strsplit(row$options, ";")))
          if (length(trimmed) == 0) NULL else trimmed
        } else NULL
        
        # Build the input with SR-only labeling per type
        input_ui <- switch(
          row$input_type,
          
          # Multiple choice (checkbox group) -> use fieldset + legend (SR-only)
          "mc" = tags$fieldset(
            `aria-describedby` = if (!is.null(help_btn)) help_id else NULL,
            tags$legend(tags$span(id = label_id, class = "sr-only", tagList(label_text, asterisk))),
            checkboxGroupInput(
              inputId  = inputId,
              label    = NULL,  # legend serves as label
              choices  = choices,
              selected = isolate(input[[inputId]]) %||% character(0)
            )
          ),
          
          # Free text -> use SR-only label via label= param
          "text" = textInput(
            inputId = inputId,
            label   = tags$span(id = label_id, class = "sr-only", tagList(label_text, asterisk)),
            value   = isolate(input[[inputId]]) %||% ""
          ),
          
          # Slider with text ticks -> SR-only label
          "textSlider" = shinyWidgets::sliderTextInput(
            inputId = inputId,
            label   = tags$span(id = label_id, class = "sr-only", tagList(label_text, asterisk)),
            choices = choices,
            force_edges = TRUE
          ),
          
          # Select -> SR-only label
          "select" = selectInput(
            inputId  = inputId,
            label    = tags$span(id = label_id, class = "sr-only", tagList(label_text, asterisk)),
            choices  = choices,
            selected = isolate(input[[inputId]]) %||% character(0),
            selectize = FALSE
          ),
          
          # Fallback (debug)
          div(style = "color:red;", paste("Unsupported input_type:", row$input_type))
        )
        
        # If there *is* a help note, output an SR-only description element to reference
        help_desc <- if (!is.na(row$notes) && nzchar(row$notes)) {
          tags$span(id = help_id, class = "sr-only", HTML(row$notes))
        } else NULL
        
        # Row layout
        # REPLACE your current "Row layout" div(...) with this:
        div(
          role = "region",
          `aria-labelledby` = vis_label_id,
          style = "margin-bottom: 2em; display: flex; flex-wrap: wrap; align-items: flex-start; gap: 1.2em;",
          
          div(style = "flex: 0 0 15%; min-width: 70px; display: flex; justify-content: center;", feedback_button),
          
          div(style = "flex: 0 0 15%; min-width: 70px; display: flex; align-items: center; justify-content: center;",
              if (!is.null(help_btn)) help_btn else NULL),
          
          div(
            style = "flex: 1; min-width: 200px;",
            # Give the VISIBLE label an id and keep it visible
            tags$b(id = vis_label_id, tagList(label_text, asterisk)),
            
            help_desc,                                # SR-only extra description if any
            div(style = "margin-top: 0.5em;", input_ui)
          )
        )
        
      })
      
      section_color <- if (current_page() %in% names(section_colors)) section_colors[[current_page()]] else "#1d598a"
      
      tagList(
        box(
          width = 12, title = NULL, solidHeader = FALSE, collapsible = FALSE,
          style = "background-color: transparent; border: none; box-shadow: none; padding: 0;",
          # Category header
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
        if (which(pages == current_page()) > 1) actionButton("prev_page", "Previous"),
        if (which(pages == current_page()) < length(pages)) actionButton("next_page", "Next"),
        if (which(pages == current_page()) == length(pages)) actionButton("submit", "Submit")
      )
    )
    
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
        # stable ids for labelling the landmark
        pop_id   <- paste0("pop-", qid)
        label_id <- paste0(pop_id, "-label")
        
        shinyBS::addPopover(
          session,
          id = paste0("help_icon_", qid),
          title   = "Note",
          content = HTML(row$notes),
          placement = "center",
          trigger   = "hover",
          options = list(
            container = "body",
            html = TRUE,
            # Bootstrap 3 popover template with ARIA landmark + labelled heading
            template = sprintf(
              '<div id="%s" class="popover qa-popover" role="region" aria-labelledby="%s">
               <div class="arrow"></div>
               <h3 id="%s" class="popover-title" role="heading" aria-level="3"></h3>
               <div class="popover-content" role="document"></div>
             </div>',
              pop_id, label_id, label_id
            )
          )
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
  # Show only when it's not in page 1 (Background info)
  output$progress_bar_ui <- renderUI({
     
    if (current_page() != pages[1]) {
      
      tagList(
        div(id = "progress_container",
            # SR-only label (description)
            tags$span(id = "progress_label", class = "sr-only",
                      "Assessment progress"),
            div(class = "progress",
                div(
                  id = "progress_bar",
                  class = "progress-bar",
                  role = "progressbar",
                  `aria-labelledby` = "progress_label",
                  `aria-valuemin` = "0",
                  `aria-valuemax` = "100",
                  `aria-valuenow`  = "0",
                  # This is the live text SR will get
                  `aria-live` = "polite",
                  style = "width: 0%;",
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
    var bar = $('#progress_bar');
    bar.css('width', '%1$s%%');
    bar.attr('aria-valuenow', %1$s);
    // Friendly text for SR, e.g., '72 percent completed'
    bar.attr('aria-valuetext', '%1$s percent completed');
    // Visible text for sighted users
    bar.text('%1$s%% Completed');
  ", percent))
  })
  
  
  # Feedback modals for all questions
  # Feedback modals for all questions
  observe({
    lapply(survey$input_id, function(qid) {
      question_text <- survey$question[survey$input_id == qid]
      
      # Open modal on click of the flag button
      # Open modal on click of the flag button (REPLACE THIS BLOCK)
      observeEvent(input[[paste0("flag_", qid)]], {
        # Stable IDs for ARIA relationships
        modal_title_id <- paste0("fb-title-", qid)
        question_id    <- paste0("fb-q-", qid)
        form_id        <- paste0("fb-form-", qid)
        
        showModal(
          modalDialog(
            # Landmark region wrapping perceivable text
            tags$div(
              role = "region",
              `aria-labelledby` = modal_title_id,
              class = "fb-modal-region",
              
              # Real structured heading for the modal title
              tags$h2(id = modal_title_id, class = "modal-h2",
                      "Feedback on this question"),
              
              # Question text (perceivable content, referenced by aria-describedby)
              tags$p(id = question_id, class = "modal-question",
                     tags$em(question_text)),
              
              # Form landmark for the input controls
              tags$form(
                id = form_id, role = "form",
                `aria-labelledby` = modal_title_id,
                textAreaInput(
                  inputId = paste0("temp_feedback_", qid),
                  label   = "Provide your comment below:",
                  value   = feedback_store[[qid]] %||% "",
                  width   = "100%",
                  height  = "120px"
                )
              )
            ),
            
            # Footer actions (Bootstrap will render this inside .modal-footer)
            footer = tagList(
              # We'll wrap these with a landmark via JS right after showModal
              modalButton("Cancel"),
              actionButton(paste0("send_feedback_", qid), "Send Feedback")
            ),
            
            easyClose = TRUE
          )
        )
        
        # --- Patch the rendered modal for perfect landmarks & no duplication ---
        # --- Patch the rendered modal AFTER it is fully shown (prevents duplicates) ---
        shinyjs::runjs(sprintf("
  (function(){
    var modal = $('#shiny-modal');
    if (!modal.length) return;

    // Run once per open, after Bootstrap has finished showing it
    modal.one('shown.bs.modal', function(){
      var $m = $(this);
      var tid = %1$s;   // title id
      var qid = %2$s;   // question id
      var sendBtnId = %3$s;

      var $dialog  = $m.find('.modal-dialog').first();
      var $content = $m.find('.modal-content').first();
      var $body    = $m.find('.modal-body').first();
      var $footer  = $m.find('.modal-footer').first();

      // Dialog relationships / landmarks
      $dialog.attr({
        'role': 'dialog',
        'aria-modal': 'true',
        'aria-labelledby': tid,
        'aria-describedby': qid
      });
      $content.attr('role','document');
      $body.attr({'role':'region','aria-labelledby': tid});

      // 1) Remove previously injected landmarks (idempotent)
      $footer.find('.fb-actions-landmark').children().unwrap();

      // 2) Remove any stray duplicates that sometimes get inserted
      //    after .modal-footer by other handlers
      $content
        .children(':not(.modal-header):not(.modal-body):not(.modal-footer)')
        .filter(function(){
          // anything that looks like a duplicate button group or a lone Cancel
          var isGroup = this.getAttribute('aria-label') === 'Feedback actions';
          var hasBtns = $(this).find('button').length >= 1;
          var isLoneCancel = $(this).is('button.btn[data-dismiss], button.btn[data-bs-dismiss]');
          return isGroup || hasBtns || isLoneCancel;
        }).remove();

      // 3) Wrap exactly the two footer buttons in a single landmark (once)
      if (!$footer.children('.fb-actions-landmark').length) {
        $footer.wrapInner('<div class=\"fb-actions-landmark\" role=\"region\" aria-label=\"Feedback actions\"></div>');
      }

      // Ensure each visible button has an aria-label that matches its text
      $footer.find('button').attr('aria-label', function(_, val){
        var t = (this.textContent || '').trim();
        return t || val || null;
      });

      // Focus the textarea for quicker input
      var $ta = $m.find('textarea[id^=\"temp_feedback_\"]').first();
      if ($ta.length) $ta.trigger('focus');
    });
  })();
",
                               jsonlite::toJSON(modal_title_id, auto_unbox = TRUE),
                               jsonlite::toJSON(question_id,    auto_unbox = TRUE),
                               jsonlite::toJSON(paste0('send_feedback_', qid), auto_unbox = TRUE)
        ))
        
      })
      
      
      
      
      
      # Save feedback, close modal, and update the icon + accessibility attributes
      observeEvent(input[[paste0("send_feedback_", qid)]], {
        val <- input[[paste0("temp_feedback_", qid)]]
        has <- !is.null(val) && nzchar(val)
        
        if (has) {
          feedback_store[[qid]] <- val
        } else {
          feedback_store[[qid]] <- NULL
        }
        removeModal()
        
        # Rebuild the button id locally (this fixes the scope error)
        flag_id    <- paste0("flag_", qid)
        aria_label <- if (has) "Feedback submitted. Edit feedback"
        else      "Flag this question and add feedback"
        
        # Swap icon style (regular ↔ solid) + update ARIA state/label + title
        # We rely on the <i> inside the actionButton to toggle Font Awesome class.
        shinyjs::runjs(sprintf("
        (function(){
          var $btn = $('#%1$s');
          var $icon = $btn.find('i.fa-flag');

          if (%2$s) {
            $icon.removeClass('fa-regular').addClass('fa-solid');
          } else {
            $icon.removeClass('fa-solid').addClass('fa-regular');
          }

          $btn.attr({
            'aria-pressed': %3$s,
            'aria-label'  : %4$s,
            'title'       : %4$s
          });
        })();
      ",
                               flag_id,
                               if (has) "true" else "false",             # %2$s toggle to solid?
                               if (has) "'true'" else "'false'",         # %3$s aria-pressed
                               jsonlite::toJSON(aria_label, auto_unbox = TRUE)  # %4$s safe string
        ))
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
          InputID  = row$input_id, 
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
      
      # Append responses to Google Sheet
      library(uuid)
      
      submission_id <- uuid::UUIDgenerate(use.time = FALSE)
      submitted_at  <- format(Sys.time(), tz = "Europe/Madrid", usetz = TRUE)
      user_id       <- session$user %||% session$token %||% ""
      
      sa_df <- survey %>%
        dplyr::select(input_id, question, pages, subframework, minimal_detailed) %>%
        dplyr::left_join(
          user_results() %>% dplyr::select(InputID, Answer, Level, Feedback),
          by = c("input_id" = "InputID")
        ) %>%
        dplyr::mutate(
          answer_raw  = as.character(Answer),
          answer_norm = dplyr::case_when(
            is.na(answer_raw) ~ "No answer",
            trimws(answer_raw) == "" ~ "No answer",
            grepl("^\\s*Select", answer_raw, ignore.case = TRUE) ~ "No answer",
            TRUE ~ answer_raw),
          level       = dplyr::coalesce(Level, minimal_detailed),
          feedback    = dplyr::coalesce(Feedback, ""),
          question_order = match(input_id, survey$input_id),
          is_background  = pages == "0. Background information"
        ) %>%
        dplyr::transmute(
          submission_id, submitted_at, user_id,
          question_order,
          input_id,
          page = pages,
          subframework,
          level,
          question,
          answer_raw, answer_norm,
          feedback,
          is_background
        )
      
      # Write to Google Sheets
      safe_gs4_auth()
      sa_id <- Sys.getenv("SA_SHEET_ID")
      if (nzchar(sa_id)) {
        # Create tab if missing (no-op if it already exists)
        try({
          if (!"responses" %in% googlesheets4::sheet_names(sa_id)) {
            googlesheets4::sheet_add(sa_id, sheet = "responses")
          }
        }, silent = TRUE)
        
        tryCatch({
          googlesheets4::sheet_append(ss = sa_id, data = sa_df, sheet = "responses")
        }, error = function(e) {
          showNotification(paste("Could not save self-assessment:", e$message),
                           type = "error", duration = 8)
        })
      }
      
      
      output$results_table <- DT::renderDataTable({
        req(user_results())
        
        # Build results_df including hidden questions
        results_df <- survey %>%
          dplyr::select(question, input_id, pages, subframework, minimal_detailed) %>%
          dplyr::left_join(
            user_results() %>% dplyr::select(Question, Answer, Level, Feedback),
            by = c("question" = "Question"),
            relationship = "many-to-many"   # allow expected duplicates
          ) %>%
          dplyr::mutate(
            # blank, NA, or "Select..." answers as No answer
            Answer = dplyr::case_when(
              is.na(Answer) | trimws(Answer) == "" ~ "No answer",
              grepl("^\\s*Select", as.character(Answer), ignore.case = TRUE) ~ "No answer",
              TRUE ~ as.character(Answer)
            ),
            # Fill Level from survey if missing
            Level = dplyr::coalesce(Level, minimal_detailed),
            Framework = toupper(subframework),
            Section   = pages,
            Question = question
          ) %>%
          # Keep Feedback and Answer columns in final table
          dplyr::select(
            Question, Framework, Section, Answer, Level, Feedback
          ) %>%
          dplyr::arrange(Section)
        
        # Render HTML links in Question column
        results_df$Question <- lapply(results_df$Question, HTML)
        
        DT::datatable(
          results_df,
          extensions = 'Buttons',
          escape = FALSE,
          caption = htmltools::tags$caption(
            style = "caption-side: top; text-align: left; color:black;",
            "Your answers by section and subframework."),
          options = list(
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            pageLength = 10,
            rowCallback = JS(
              "function(row, data, index) {",
              "  var answer = data[3]; // 0-based: 0 Q, 1 Framework, 2 Section, 3 Answer, 4 Level, 5 Feedback",
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
  
  
  
  ## MATURITY LEVEL
  # Render tmaturity level box
  output$maturity_level <- renderValueBox({
    info <- maturity_level_info()
    valueBox(
      value    = paste0("Level ", info$level),
      subtitle = paste0(info$title, " — ", info$desc),
      icon     = icon_dec(info$icon),
      color    = info$color
    )
  })
  
  
  
  ## RESULTS. SCORES
  # Rendering each valuebox (using score_total_level function )
  
  # Total score
  output$score_total <- renderValueBox({
    res <- score_total_level(c("minimal", "detailed"))
    valueBox(
      paste0(res$yes, " / ", res$total),
      subtitle = "Total Yes Answers",
      icon = icon_dec("check-circle"),
      color = "green"
    )
  })
  
  # Minimal total score
  output$score_minimal <- renderValueBox({
    res <- score_total_level("minimal")
    valueBox(
      paste0(res$yes, " / ", res$total),
      subtitle = "Minimal Level - Yes Answers",
      icon = icon_dec("flag-checkered"),
      color = "yellow"
    )
  })
  
  #Detailed total score
  
  output$score_detailed <- renderValueBox({
    res <- score_total_level("detailed")
    valueBox(
      paste0(res$yes, " / ", res$total),
      subtitle = "Detailed Level - Yes Answers",
      icon = icon_dec("clipboard-check"),
      color = "navy"
    )
  })
  

  # SCORE BY SECTION

  # Rendering each valuebox (using score_total_level function )
  # Section: Content & Structure
  output$score_content_minimal <- renderValueBox({
    res <- score_total_level("minimal", section = "1. Content & Structure")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon_dec("flag"), color = "yellow")
  })
  
  output$score_content_detailed <- renderValueBox({
    res <- score_total_level("detailed", section = "1. Content & Structure")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon_dec("clipboard"), color = "navy")
  })
  
  # Section: Implementation
  output$score_implementation_minimal <- renderValueBox({
    res <- score_total_level("minimal", section = "2. Implementation")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon_dec("flag"), color = "yellow")
  })
  
  output$score_implementation_detailed <- renderValueBox({
    res <- score_total_level("detailed", section = "2. Implementation")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon_dec("clipboard"), color = "navy")
  })
  
  
  # Section: Evaluation
  output$score_evaluation_minimal <- renderValueBox({
    res <- score_total_level("minimal", section = "3. Evaluation")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon_dec("flag"), color = "yellow")
  })
  
  output$score_evaluation_detailed <- renderValueBox({
    res <- score_total_level("detailed", section = "3. Evaluation")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon_dec("clipboard"), color = "navy")
  })
  
  # Section: Licensing & Ethics
  output$score_ethics_minimal <- renderValueBox({
    res <- score_total_level("minimal", section = "4. Licensing & Ethics")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon_dec("flag"), color = "yellow")
  })
  
  output$score_ethics_detailed <- renderValueBox({
    res <- score_total_level("detailed", section = "4. Licensing & Ethics")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon_dec("clipboard"), color = "navy")
  })

  ## SCORE BY SUBFRAMEWORK
  
  #SF: Essential
  output$score_essential_minimal <- renderValueBox({
    res <- score_total_level("minimal", subfw = "essential")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon_dec("flag"), color = "yellow")
  })
  
  output$score_essential_detailed <- renderValueBox({
    res <- score_total_level("detailed", subfw = "essential")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon_dec("clipboard"), color = "navy")
  })
  
  # SF: FAIR
  output$score_fair_minimal <- renderValueBox({
    res <- score_total_level("minimal", subfw = "fair")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon_dec("flag"), color = "yellow")
  })
  
  output$score_fair_detailed <- renderValueBox({
    res <- score_total_level("detailed", subfw = "fair")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon_dec("clipboard"), color = "navy")
  })
  
  # SF: MVS
  output$score_mvs_minimal <- renderValueBox({
    res <- score_total_level("minimal", subfw = "mvs")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon_dec("flag"), color = "yellow")
  })
  
  output$score_mvs_detailed <- renderValueBox({
    res <- score_total_level("detailed", subfw = "mvs")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon_dec("clipboard"), color = "navy")
  })
  
  # SF: ELSI
  output$score_elsi_minimal <- renderValueBox({
    res <- score_total_level("minimal", subfw = "elsi")
    valueBox(paste0(res$yes, " / ", res$total), "Minimal Level", icon_dec("flag"), color = "yellow")
  })
  
  output$score_elsi_detailed <- renderValueBox({
    res <- score_total_level("detailed", subfw = "elsi")
    valueBox(paste0(res$yes, " / ", res$total), "Detailed Level", icon_dec("clipboard"), color = "navy")
  })
  
  
  
  ## RESULTS: Best practices
  
  
  output$Best_practices <- DT::renderDataTable({
    req(user_results())
    
    merged <- survey %>%
      select(question, input_id, bestpractices, pages, subframework, minimal_detailed) %>%
      # Keep all survey questions (including hidden), join answers if they exist
      left_join(user_results(), by = c("question" = "Question"), relationship = "many-to-many")
    
    filtered <- merged %>%
      # Remove only background info page
      filter(pages != "0. Background information") %>%
      # Keep "No", "No answer" and hidden (NA)
      filter(is.na(Answer) | Answer %in% c("No", "No answer")) %>%
      # Keep only where we have best practices text
      filter(!is.na(bestpractices) & bestpractices != "") %>%
      mutate(`Best Practice` = lapply(bestpractices, HTML)) %>%
      select(Question = question, Level = minimal_detailed, Answer,
             `Best Practice`, subframework, pages) %>%
      mutate(subframework = toupper(subframework))
    
    DT::datatable(
      filtered,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color:black;",
        "Suggested best practices for questions answered No or left unanswered."),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 5
      ),
      extensions = "Buttons",
      rownames = FALSE
    )
  })
  
  
  
  
  
  
  ## SCORE PLOT
  output$score_plot <- renderPlot({
    req(user_results())
    
    # Join all survey questions with user results 
    # Include hidden as "No answer"
    df <- survey %>%
      dplyr::filter(pages != "0. Background information",
                    minimal_detailed %in% c("minimal", "detailed")) %>%
      dplyr::left_join(user_results(), by = c("question" = "Question")) %>%
      dplyr::mutate(
        Answer = ifelse(is.na(Answer) | Answer == "", "No answer", Answer),
        Level = ifelse(is.na(Level), minimal_detailed, Level),
        Page = ifelse(is.na(Page), pages, Page),
        Feedback = ifelse(is.na(Feedback), "", Feedback),
        Framework = toupper(subframework),
        SECTION = stringr::str_wrap(Page, 40)
      ) %>%
      dplyr::select(Framework, Level, SECTION, Answer)
    
    # Add dummy spacer rows for visual balance
    df <- df %>%
      dplyr::bind_rows(
        tibble::tibble(Framework = "ET", Level = "zzz", SECTION = "D",    Answer = "20"),
        tibble::tibble(Framework = "ET", Level = "zzz", SECTION = "D",    Answer = "20"),
        tibble::tibble(Framework = "EM", Level = "kkk", SECTION = "Comz", Answer = "20"),
        tibble::tibble(Framework = "EM", Level = "kkk", SECTION = "Comz", Answer = "20"),
        tibble::tibble(Framework = "FB", Level = "aaa", SECTION = "F",    Answer = "20"),
        tibble::tibble(Framework = "FB", Level = "aaa", SECTION = "F",    Answer = "20"),
        tibble::tibble(Framework = "ET", Level = "zzz", SECTION = "D",    Answer = "20"),
        tibble::tibble(Framework = "EM", Level = "kkk", SECTION = "Comz", Answer = "20"),
        tibble::tibble(Framework = "FB", Level = "aaa", SECTION = "F",    Answer = "20")
      ) %>%
      dplyr::mutate(
        color = dplyr::if_else(Level %in% c("aaa", "kkk", "zzz"), "#ffffff00", "black"),
        alpha = dplyr::if_else(Level %in% c("aaa", "kkk", "zzz"), 0, 0.7)
      )
    
    # Order Framework for axis 2
    df$Framework <- factor(
      df$Framework,
      levels = c("ESSENTIAL","ET", "MVS", "EM","FAIR","FB", "ELSI") 
    )
    # Order Section for axis 3
    df$Section <- factor(
      df$SECTION,
      levels = c("1. Content & Structure", "D", "2. Implementation", "Comz", "3. Evaluation","F", "4. Licensing & Ethics") 
    )
    
    
    ggplot(df) +
      theme_void() +
      # Top legends: horizontal, Answer + Level
      theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
        plot.margin = margin(10, 20, 10, 10)
      ) +
      coord_cartesian(clip = "off") +
      
      # Suppress any color legend (we only want linetype legend for Level)
      scale_color_identity(guide = "none") +
      scale_alpha_identity() +
      
      # Color legend for Answer
      scale_fill_manual(
        name = "Answer:",
        values = c(
          "Yes" = "darkgreen",
          "No" = "darkred",
          "Not applicable" = "lightnavy",
          "No answer" = "grey",
          "20" = "#ffffff00"
        ),
        breaks = c("Yes", "No", "Not applicable", "No answer")
      ) +
      
      # Line-type legend for Level
      scale_linetype_manual(
        name = "Level:",
        values = c("minimal" = "solid", "detailed" = "dashed"),
        breaks = c("minimal", "detailed")
      ) +
      
      aes(
        axis1 = interaction(Answer, Level),
        axis2 = interaction(Answer, Level, Framework),
        axis3 = interaction(Answer, Level, SECTION)
      ) +
      
      # Flows (color legend from fill = Answer)
      geom_flow(aes(fill = factor(Answer)), show.legend = TRUE, alpha = 0.25) +
      geom_lode(aes(fill = factor(Answer), alpha = alpha), show.legend = FALSE) +
      
      # Counts in strata
      geom_text(aes(label = after_stat(n), color = color), stat = "stratum") +
      
      # Borders (linetype legend from Level)
      geom_stratum(
        aes(
          axis1 = interaction(Level),
          axis2 = interaction(Level, Framework),
          axis3 = interaction(Level, SECTION),
          linetype = Level
        ),
        color = "black",
        fill = NA,
        show.legend = TRUE
      ) +
      
      # Axis labels
      geom_text(
        aes(
          axis1 = Level,
          axis2 = Framework,
          axis3 = SECTION,
          color = color,
          label = after_stat(stratum),
          y = after_stat(2 * y - ymin) + 0.5
        ),
        stat = "stratum",
        vjust = 0,
        lineheight = .8
      ) +
      
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
          "mc" = radioButtons(input_id, label="Select your answer", choices = choices, inline = FALSE),
          "select" = selectInput(input_id, label=tags$span("Select your experience", class="sr-only"),
                                 choices = choices, selectize=FALSE),
          "textSlider" = shinyWidgets::sliderTextInput(input_id, 
                                                       label=tags$span("Select your knowledge or opinion",
                                                                       class="sr-only"),
                                                       choices = choices, force_edges = TRUE),
          "text" = textAreaInput(input_id, label=tags$span("Your answer", class="sr-only"), placeholder = "Type your answer here...", height = "100px"),
          NULL
        )
        
        # ConditionalPanel for dependent questions
        if (!is.null(parent_id) && !is.na(row$dependence_value)) {
          condition <- sprintf("input['%s'] == '%s'", parent_id, row$dependence_value)
          conditionalPanel(condition = condition,
                           div(style = "margin-bottom: 1.5em;", label, input_ui))
        } else {
          div(style = "margin-bottom: 1.5em;", div(style = "margin-bottom: 1.5em; padding-left: 14px;",
            div(style = "margin-bottom: 0.5em;", label),
            input_ui
          )
          )
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
              "background-color:#1d598a; color: white; font-weight: bold;
              font-size: 18px; padding: 10px 15px; border-radius: 6px; margin-bottom: 0.5em; margin-top: 1em;"
            ),
            cat
          ),
          div(style= "background-color: transparent !important; margin-bottom: 2em; display: flex; flex-wrap: wrap; align-items: flex-start; gap: 1.2em;", 
              questions)
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
  
  
  # SUBMIT General feedback: Modal dialog and add responses to Google Sheet
  observeEvent(input$submit_general_feedback, {
    # 1) Gather + normalize answers
    answers <- setNames(
      lapply(general_feedback$input_id, function(id) {
        val <- input[[paste0("gf_", id)]]
        val <- if (is.null(val)) NA_character_ else as.character(val)
        if (is.na(val) || trimws(val) == "" || grepl("^\\s*Select", val, ignore.case = TRUE)) {
          "No answer"
        } else {
          val
        }
      }),
      general_feedback$question     # or use general_feedback$input_id for shorter column names
    )
    
    # 2) Metadata FIRST (so they appear as the first columns)
    ts  <- format(Sys.time(), tz = "Europe/Madrid", usetz = TRUE)
    uid <- session$user %||% session$token %||% ""   # fallback if session$user is NULL
    
    # 3) One-row, wide format (metadata + answers)
    result_df <- as.data.frame(
      c(list(timestamp = ts, user_id = uid), answers),
      stringsAsFactors = FALSE,
      check.names = TRUE   # makes very long question texts valid column names
    )
    
    # 4) Append to Google Sheet
    safe_gs4_auth()
    googlesheets4::sheet_append(
      ss   = Sys.getenv("GF_SHEET_ID"),
      data = result_df
      # , sheet = "responses"   # uncomment if you’re using a specific tab name
    )
    
    # 5) Confirmation
    # Confirmation
    showModal(
      modalDialog(
        easyClose = TRUE,
        footer = NULL,   # no footer buttons here
        tags$div(
          role = "region",
          `aria-labelledby` = "gf-thankyou-title",
          class = "gf-modal-region",
          
          # Structured heading for modal title
          tags$h2(
            id = "gf-thankyou-title",
            class = "modal-h2",
            "Thank you!"
          ),
          
          # Body content wrapped as document
          tags$div(
            role = "document",
            class = "gf-modal-body",
            tags$p("Your feedback has been submitted successfully."),
            tags$p("We appreciate your contribution to improving the Skills4EOSC Quality Assurance Framework."),
            tags$p("You may now close this window or continue using the app.")
          )
        )
      )
    )
    
    # After rendering, ARIA dialog roles (like with feedback modals)
    shinyjs::runjs("
  (function(){
    var $m = $('#shiny-modal').last();
    if (!$m.length) return;

    var $dialog  = $m.find('.modal-dialog').first();
    var $content = $m.find('.modal-content').first();
    var $body    = $m.find('.modal-body').first();

    $dialog.attr({
      'role': 'dialog',
      'aria-modal': 'true',
      'aria-labelledby': 'gf-thankyou-title'
    });
    $content.attr('role','document');
    $body.attr({'role':'region','aria-labelledby':'gf-thankyou-title'});
  })();
")
  })
  

  
  
  
  # checklist iframe (about page)
  output$checklist <- renderUI({
    tags$iframe(
      title = "S4E Checklist & Guide",
      src = "https://view.genially.com/682d8981d26d435becb916c6",
      type = "text/html",
      width = "100%",
      style = "max-width: 100%; height: auto;",
      tabindex = "0" # Focusable
    )
  })

  
}






# Run the application 
shinyApp(ui = ui, server = server)
