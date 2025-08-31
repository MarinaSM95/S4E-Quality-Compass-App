# S4E-Quality-Compass-App
This is the repository for the [S4E Quality Compass](https://csslab.uc3m.es/shiny/S4E-Quality-Compass-App/), an app created for the [Skills4EOSC Project](https://www.uc3m.es) to provide quality assurance self-assessment on Open Science training materials for course designers and trainers, in compliance with the Skills4EOSC Quality Assurance Framework. 

The main aim of this app is to provide a **self-assessment test** to evaluate the compliance of an Open Science course to the S4E Quality Framework. Besides collecting some user background information, this test addresses 4 key areas or sub-frameworks: 

    - ESSENTIAL (baseline practices)
    - MVS (Minimum Viable Standards)
    - FAIR (Findable, Accessible, Interoperable, Reusable)
    - ELSI (Ethical, Legal, and Social Issues)


## Features 
The main features of this app are:

    - Dynamic multipage questionnaire with dependent questions and feedback modals by question
    - Automated results dashboard with visualization of scores, maturity level, graph of performance, best practices recommendations for questions that user didn't pass, and user answer records
    - User data storage in Google sheets 
    - A simpler questionaire on General feedback

## Files
In this repo you will find these main files (among others):

    - clean_data.Rmd <- a narrative .rmd explaining how to clean and structure data for the app 
    - app.R <- since the work is still in progress, this files contains the UI, server, JavaScript and the CSS of the app. In later stages, the code will be organised separately.
    - www/ <- folder with all the images, datasets (raw and clean), raw and clean datasets and .Rmd files:
        - Privacy.rmd and Privacy_accessibility.rmd : both produce and html file that is embedded in the app inside the Privacy Policy tab. The only difference is that the latter is a newer and more accessible version
        - Privacy.rmd and Privacy_accessibility.rmd : both produce and html file that is embedded in the app inside the Privacy Policy tab. The only difference is that the latter is a newer and more accessible version
    - tfm_diagrams.R <- this file produces some of the diagrams I used in my Master Thesis about this app. It mainly uses DiagrammeR, DiagrammeRsvg and rsvg. These diagrams illustrate the app logic and user navigation.

## Accessibility

Accessibility support (WCAG 2.1 AA level, WAI-ARIA best practices applied). However, more work is needed in this regard.


## Licenses

Except where otherwise noted, content on this site is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). Sanchez-Moreno, M. (2025). S4E Quality Compass app (1.0.0).

## References

[App User Manual](https://www.uc3m.es): Introduction and walthrough the app

[The S4E Checklist & Guide](https://view.genially.com/682d8981d26d435becb916c6): very useful to get familiar with the framework

[D2.7 Community-endorsed quality assurance and certification framework for professional training and qualifications - final version](https://zenodo.org/records/15731878)


