# mobile UI to collect data
# last update: 2017-03-01

appNewData <- function(){
        tabPanel('Datenerfassung',
                 helpText('Erfasse hier einen neuen Eintrag im Allergie-Tagebuch.'),
                 dateInput('dateInput',
                           label = 'Datum',
                           language = 'de'),
                 numericInput('conditionInput',
                              label = 'Befinden (1 - sehr gut, 6 - sehr schlecht)',
                              value = 3,
                              min = 1, max = 6, step = 1),
                 checkboxInput('medInput',
                               label = strong('Medikament eingenommen'),
                               value = FALSE),
                 tags$label('Anmerkung:'),
                 br(),
                 tags$textarea(id='diaryInput',
                               rows=2, cols=50,
                               ''),
                 br(),br(),
                 actionButton('saveAllergyInput', 'Speichern',
                              icon('save')),
                 br(), br(), uiOutput('allergyInputStatus')
        )
}