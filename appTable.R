# mobile UI to show data in table
# last update: 2017-03-09

source('appTableSelect.R')

appTable <- function(){
        tabPanel('Tabelle',
                 appTableSelect(),
                 DT::dataTableOutput('tableList')
        )
}